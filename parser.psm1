using module .\cmdb_handle.psm1

# Parses a regex pattern
# Extracts the named capture group 'val'.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#   [Regex]$regx - The regex pattern to match.
#
# Returns:
#   A PSCustomObject with `val` and `consumed`
#   - `val` holds the extracted value from 'val' capture group.
#   - `consumed` hodls the entire matched expression.
#   `$null` on failure.
function ParsePattern {
    param ([Ref]$expr, [Regex]$regx)
    if ($expr.Value -match $regx) {
        $expr.Value = $expr.Value -replace $regx, ""
        return [PSCustomObject]@{
            val = $Matches["val"]
            consumed = $Matches[0]
        }
    } else { return $null }
}

# Parses a string value
# Supports plain, single-quoted, and double-quoted strings.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A PSCustomObject with `val` and `consumed`.
#   - `val` holds the extracted string value, unescaped.
#   - `consumed` holds the entire matched expression.
#   `$null` on failure.
function ParseString {
    param ([Ref]$expr)
    [String]$plain = "(?<val>(\\.|\s*[^\s`"'!=~<>&|()])+)"
    [String]$single = "'(?<val>(\\.|[^'])*)'"
    [String]$double = "`"(?<val>(\\.|[^`"])*)`""
    [PSCustomObject]$str = ParsePattern $expr "^\s*($plain|$single|$double)"
    if (-not $str) { return $null }
    $str.val = $str.val -replace "\\(.)", '$1'
    return $str
}

# Parses a comparison operator
# Valid operators are:
# ~  =~  (IsMatch)
# !~     (IsNotMatch)
# =  ==  (IsEqual)
# != <>  (IsNotEqual)
# <      (LessThan)
# <=     (LessThanOrEqual)
# >      (GreaterThan)
# >=     (GreaterThanOrEqual)
# 
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A PSCustomObject with `val` and `consumed`.
#   - `val` holds the extracted operator.
#   - `consumed` holds the entire matched expression.
#   `$null` if no operator is found.
function ParseOperator {
    param ([Ref]$expr)
    return ParsePattern $expr "^\s*(?<val>(=?~|!~|==?|!=|<>|<=?|>=?))"
}

# Parses an AND combinator (&&, & or "AND").
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A PSCustomObject with `val` and `consumed`.
#   - `val` holds the extracted AND combinator.
#   - `consumed` holds the entire matched expression.
#   `$null` if no AND combinator is found.
function ParseAnd {
    param ([Ref]$expr)
    return ParsePattern $expr "^\s*(?<val>(&{1,2}|[Aa][Nn][Dd]))"
}

# Parses an OR combinator (||, | or "OR").
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A PSCustomObject with `val` and `consumed`.
#   - `val` holds the extracted OR combinator.
#   - `consumed` holds the entire matched expression.
#   `$null` if no OR combinator is found.
function ParseOr {
    param ([Ref]$expr)
    return ParsePattern $expr "^\s*(?<val>(\|{1,2}|[Oo][Rr]))"
}

# Maps a parsed operator string to a [CmdbOperator] enum value.
# Valid operators are:
# ~  =~  (IsMatch)
# !~     (IsNotMatch)
# =  ==  (IsEqual)
# != <>  (IsNotEqual)
# <      (LessThan)
# <=     (LessThanOrEqual)
# >      (GreaterThan)
# >=     (GreaterThanOrEqual)
# 
# Arguments:
#   [String]$op_str - The operator string to map.
#
# Returns:
#   The corresponding [CmdbOperator] enum value.
#
# Throws on failure.
function MapOperator {
    param ([String]$op_str)
    switch ($op_str) {
        ("~")   { return [CmdbOperator]::IsMatch }
        ("=~")  { return [CmdbOperator]::IsMatch }
        ("!~")  { return [CmdbOperator]::IsNotMatch }
        ("=")   { return [CmdbOperator]::IsEqual }
        ("==")  { return [CmdbOperator]::IsEqual }
        ("!=")  { return [CmdbOperator]::IsNotEqual }
        ("<>")  { return [CmdbOperator]::IsNotEqual }
        ("<")   { return [CmdbOperator]::LessThan }
        ("<=")  { return [CmdbOperator]::LessThanOrEqual }
        (">")   { return [CmdbOperator]::GreaterThan }
        (">=")  { return [CmdbOperator]::GreaterThanOrEqual }
        default { throw "'$op_str' is not a valid operator" }
    }
}

# Parses a single comparison (e.g., `age >= 30`).
# The left and right side are parsed as any valid string value.
# 
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A PSCustomObject with `field`, `value`, and `operator`.
#   `$null` on failure.
#
# Throws on an empty `field` string
function ParseComparison {
    param ([Ref]$expr)
    [PSCustomObject]$field = ParseString $expr; if (-not $field) { return $null }
    [PSCustomObject]$oper = ParseOperator $expr; if (-not $oper) { return $null }
    [PSCustomObject]$value = ParseString $expr; if (-not $value) { return $null }
    if (-not $field.val) { throw "Found Comparison with an empty field" }
    return [PSCustomObject]@{
        field = $field.val
        value = $value.val
        operator = (MapOperator $oper.val)
    }
}

enum Term {
    Comparsion
    Combination
    Inversion
}

# Parses a term, which can be a comparison, an inversion,
# or a grouped subexpression in parentheses.
# 
# Arguments:
#   [Ref]$expr - The expression being parsed.
# 
# Returns:
#   A PSCustomObject with `term_type` and `value`.
#   $null on failure.
#
# Throws on inversion without following parentheses.
# Throws on any unmatched opening parentheses.
function ParseTerm {
    param ([Ref]$expr)
    if ($expr.Value -match "^\s*(!|[Nn][Oo][Tt]\s*\()") {
        [String]$inv = (ParsePattern $expr "^\s*(?<val>!|[Nn][Oo][Tt])").val
        if (-not ($expr.Value -match "^\s*\(")) { throw "Expected Parentheses after '$inv'"}
        return [PSCustomObject]@{ term_type = [Term]::Inversion; value = ParseTerm $expr }
    }
    if (ParsePattern $expr "^\s*\(") {
        [PSCustomObject]$inner = ParseTermChain $expr
        if (-not $inner) { throw "Expected Expression inside Parentheses" }
        if (-not (ParsePattern $expr "^\s*\)")) { throw "Found unmatched '('" }
        return $inner
    }
    [PSCustomObject]$val = ParseComparison $expr
    if (-not $val) { return $null }
    return [PSCustomObject]@{ term_type = [Term]::Comparison; value = $val }
}

# Parses a chain of AND-connected terms.
# 
# Arguments:
#   [Ref]$expr - The expression being parsed.
# 
# Returns:
#   A PSCustomObject with `term_type` and `value`.
#   $null on failure.
#
# Throws on any dangling operator.
function ParseAndChain {
    param ([Ref]$expr)
    [PSCustomObject]$left = ParseTerm $expr
    if (-not $left) { return $null }
    [PSCustomObject]$combinator = ParseAnd $expr
    if (-not $combinator) { return $left }
    [PSCustomObject]$right = ParseAndChain $expr
    if (-not $right) { throw "Expected Expression after '$($combinator.val)'"}
    return [PSCustomObject]@{
        term_type = [Term]::Combination
        value = [PSCustomObject]@{
            left = $left; right = $right
            combine = [CmdbCombine]::And
        }
    }
}

# Parses a chain of OR-connected terms.
# Uses `ParseAndChain` to handle precedence.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
# 
# Returns:
#   A PSCustomObject with `term_type` and `value`.
#   $null on failure.
#
# Throws on any dangling operator.
function ParseTermChain {
    param ([Ref]$expr)
    [PSCustomObject]$left = ParseAndChain $expr
    if (-not $left) { return $null }
    [PSCustomObject]$combinator = ParseOr $expr
    if (-not $combinator) { return $left }
    [PSCustomObject]$right = ParseTermChain $expr
    if (-not $right) { throw "Expected Expression after '$($combinator.val)'" }
    return [PSCustomObject]@{
        term_type = [Term]::Combination
        value = [PSCustomObject]@{
            left = $left; right = $right
            combine = [CmdbCombine]::Or
        }
    }
}

# Parses an entire boolean expression into a structured AST.
#
# Examples:
# ```
# ParseExpression("location ~ RZ%")
# ParseExpression("age >= 30 AND (name = 'Alice' OR NOT (city = 'Paris'))")
# ````
#
# Arguments:
#   [String]$string - The input expression to parse.
#
# Returns:
#   A structured PSCustomObject representing the parsed expression.
#
# Throws on any syntax errors.
function ParseExpression {
    param ([String]$string)
    [PSCustomObject]$evaluated = ParseTermChain ([Ref]$string)
    if (-not $evaluated)  { throw "Expected Expression" }
    [PSCustomObject]$trail = ParsePattern ([Ref]$string) "^\s*(?<val>\S+)"
    if ($trail) { throw "Unexpected '$($trail.val)' after Expression" }
    return $evaluated
}

function RenderAst {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Inversion)   { return "NOT ($(RenderAst $node.value))" }
        ([Term]::Comparison)  {
            return "$($node.value.field | ConvertTo-Json) $(switch ($node.value.operator) {
                ([CmdbOperator]::IsMatch)            { "=~" }
                ([CmdbOperator]::IsNotMatch)         { "!~" }
                ([CmdbOperator]::IsEqual)            { "==" }
                ([CmdbOperator]::IsNotEqual)         { "!=" }
                ([CmdbOperator]::LessThan)           { "<"  }
                ([CmdbOperator]::LessThanOrEqual)    { "<=" }
                ([CmdbOperator]::GreaterThan)        { ">"  }
                ([CmdbOperator]::GreaterThanOrEqual) { ">=" }
            }) $($node.value.value | ConvertTo-Json)"
        }
        ([Term]::Combination) {
            return "($(RenderAst $node.value.left)) $(switch ($node.value.combine) {
                ([CmdbCombine]::And) { "AND" }
                ([CmdbCombine]::Or)  { "OR" }
            }) ($(RenderAst $node.value.right))"
        }
    }
}

function InvertTerm {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Comparison)  {
            [CmdbOperator]$inv_oper = switch ($node.value.operator) {
                ([CmdbOperator]::IsMatch)            { [CmdbOperator]::IsNotMatch }
                ([CmdbOperator]::IsNotMatch)         { [CmdbOperator]::IsMatch }
                ([CmdbOperator]::IsEqual)            { [CmdbOperator]::IsNotEqual }
                ([CmdbOperator]::IsNotEqual)         { [CmdbOperator]::IsEqual }
                ([CmdbOperator]::LessThan)           { [CmdbOperator]::GreaterThanOrEqual }
                ([CmdbOperator]::LessThanOrEqual)    { [CmdbOperator]::GreaterThan }
                ([CmdbOperator]::GreaterThan)        { [CmdbOperator]::LessThanOrEqual }
                ([CmdbOperator]::GreaterThanOrEqual) { [CmdbOperator]::LessThan }
            }
            return [PSCustomObject]@{
                term_type = [Term]::Comparison
                value = [PSCustomObject]@{
                    field = $node.value.field
                    value = $node.value.value
                    operator = $inv_oper
                }
            }
        }
        ([Term]::Combination) {
            [CmdbCombine]$inv_comb = switch ($node.value.combine) {
                ([CmdbCombine]::And) { [CmdbCombine]::Or }
                ([CmdbCombine]::Or) { [CmdbCombine]::And }
            }
            [PSCustomObject]$inv_val = [PSCustomObject]@{
                left = InvertTerm $node.value.left
                right = InvertTerm $node.value.right
                combine = $inv_comb
            }
            return [PSCustomObject]@{
                term_type = [Term]::Combination
                value = NormalizeAst $inv_val
            }
        }
        ([Term]::Inversion) { return NormalizeAst $node.value }
    }
}

function NormalizeAst {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Combination) {
            return [PSCustomObject]@{
                term_type = [Term]::Combination
                value = [PSCustomObject]@{
                    left = NormalizeAst $node.value.left
                    right = NormalizeAst $node.value.right
                    combine = $node.value.combine
                }
            }
        }
        ([Term]::Inversion)   { return InvertTerm $node.value }
        ([Term]::Comparison)  { return $node }
    }
}
