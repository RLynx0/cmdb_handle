using module .\cmdb_handle.psm1

###################
# SECTION PARSING #
###################

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
    FlatCombination
    Inversion
    Boolean
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


######################
# SECTION EVALUATION #
######################

function BoolTerm {
    param([Bool]$value)
    return [PSCustomObject]@{
        term_type = [Term]::Boolean
        value = $value
    }
}

function CompareTerms {
    param ([PSCustomObject]$term_a, [PSCustomObject]$term_b)
    if ($term_a.term_type -ne $term_b.term_type) { return $false }
    switch ($term_a.term_type) {
        ([Term]::FlatCombination) {
            if ($term_a.value.combine -ne $term_b.value.combine) { return $false }
            if ($term_a.value.terms.Count -ne $term_b.value.terms.Count) { return $false }
            for ($i = 0; $i -lt $term_a.value.terms.Count; $i++) {
                if (CompareTerms $term_a.value.terms[$i] $term_b.value.terms[$i]) { continue }
                return $false
            }
            return $true
        }
        ([Term]::Combination) {
            return $term_a.value.combine -eq $term_b.value.combine `
                -and (CompareTerms $term_a.value.left $term_b.value.left) `
                -and (CompareTerms $term_a.value.right $term_b.value.right)
        }
        ([Term]::Comparison) {
            return $term_a.value.operator -eq $term_b.value.operator `
                -and $term_a.value.field -eq $term_b.value.field `
                -and $term_a.value.value -eq $term_b.value.value
        }
        ([Term]::Inversion) { return CompareTerms $term_a.value $term_b.value }
        ([Term]::Boolean) { return $term_a.value -eq $term_b.value }
    }
}

function InvertOperator {
    param([CmdbOperator]$operator)
    switch ($operator) {
        ([CmdbOperator]::IsMatch)            { return [CmdbOperator]::IsNotMatch }
        ([CmdbOperator]::IsNotMatch)         { return [CmdbOperator]::IsMatch }
        ([CmdbOperator]::IsEqual)            { return [CmdbOperator]::IsNotEqual }
        ([CmdbOperator]::IsNotEqual)         { return [CmdbOperator]::IsEqual }
        ([CmdbOperator]::LessThan)           { return [CmdbOperator]::GreaterThanOrEqual }
        ([CmdbOperator]::LessThanOrEqual)    { return [CmdbOperator]::GreaterThan }
        ([CmdbOperator]::GreaterThan)        { return [CmdbOperator]::LessThanOrEqual }
        ([CmdbOperator]::GreaterThanOrEqual) { return [CmdbOperator]::LessThan }
    }
}
function InvertCombine {
    param([CmdbCombine]$combine)
    switch ($combine) {
        ([CmdbCombine]::And) { return [CmdbCombine]::Or }
        ([CmdbCombine]::Or) { return [CmdbCombine]::And }
    }
}

function InvertTerm {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Comparison)  {
            return [PSCustomObject]@{
                term_type = [Term]::Comparison
                value = [PSCustomObject]@{
                    field = $node.value.field
                    value = $node.value.value
                    operator = InvertOperator $node.value.operator
                }
            }
        }
        ([Term]::Combination) {
            return NormalizeAst ([PSCustomObject]@{
                term_type = [Term]::Combination
                value = [PSCustomObject]@{
                    left = InvertTerm $node.value.left
                    right = InvertTerm $node.value.right
                    combine = InvertCombine $node.value.combine
                }
            })
        }
        ([Term]::FlatCombination) {
            return FlattenCombination `
            -terms @($node.value.terms | ForEach-Object { InvertTerm $_ }) `
            -combine (InvertCombine $node.value.combine)
        }
        ([Term]::Boolean) { return BoolTerm (-not $node.value) }
        ([Term]::Inversion) { return NormalizeAst $node.value }
    }
}

function FlattenCombination {
    param([PSCustomObject[]]$terms, [CmdbCombine]$combine)
    [PSCustomObject]$short_circuit = BoolTerm ($combine -eq [CmdbCombine]::Or)
    [PSCustomObject]$neutral = InvertTerm $short_circuit
    [PSCustomObject[]]$unique = @()
    $terms = $terms | ForEach-Object {
        if ($_.term_type -eq [Term]::FlatCombination -and $_.value.combine -eq $combine) `
        { $_.value.terms } else { $_ }
    }
    foreach ($term in $terms) {
        [Bool]$add = $true
        [PSCustomObject]$inverse = InvertTerm $term
        if (CompareTerms $term $short_circuit) { return $term }
        if (CompareTerms $term $neutral) { continue }
        foreach ($seen in $unique) {
            if (CompareTerms $inverse $seen) { return $short_circuit }
            if (CompareTerms $term $seen) { $add = $false; break }
        }
        if ($add) { $unique += $term }
    }
    if ($unique.Count -eq 0) { return $neutral }
    if ($unique.Count -eq 1) { return $unique[0] }
    return [PSCustomObject[]]@{
        term_type = [Term]::FlatCombination
        value = [PSCustomObject]@{
            combine = $combine
            terms = $unique
        }
    }
}

function DistributeTerm {
    param ([PSCustomObject]$term, [PSCustomObject]$into, [CmdbCombine]$combine)
    return FlattenCombination -terms ($into.value.terms | ForEach-Object {
        NormalizeAst ([PSCustomObject]@{
            term_type = [Term]::Combination
            value = [PSCustomObject]@{
                left = $_; right = $term
                combine = $combine
            }
        })
    }) -combine $into.value.combine
}

function NormalizeAst {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Combination) {
            [PSCustomObject]$left = NormalizeAst $node.value.left
            [PSCustomObject]$right = NormalizeAst $node.value.right
            [CmdbCombine]$combine = $node.value.combine
            [Bool]$distribute = $combine -eq [CmdbCombine]::And
            [Bool]$left_comb = $left.term_type -eq [Term]::FlatCombination
            [Bool]$right_comb = $right.term_type -eq [Term]::FlatCombination
            [Bool]$distribute_left = $distribute -and $right_comb -and $right.value.combine -ne $combine
            [Bool]$distribute_right = $distribute -and $left_comb -and $left.value.combine -ne $combine
            if ($distribute_right) { return DistributeTerm -term $right -into $left -combine $combine }
            if ($distribute_left)  { return DistributeTerm -term $left -into $right -combine $combine }
            return FlattenCombination -terms @($left, $right) -combine $combine
        }
        ([Term]::Inversion)       { return InvertTerm $node.value }
        ([Term]::FlatCombination) { return $node } # Already Normalized
        ([Term]::Comparison)      { return $node }
        ([Term]::Boolean)         { return $node }
    }
}


#####################
# SECTION INTERFACE #
#####################

function RenderAst {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Boolean) { return $node.value | ConvertTo-Json }
        ([Term]::Inversion) { return "NOT ($(RenderAst $node.value))" }
        ([Term]::Comparison) {
            return "$($node.value.field) $(switch ($node.value.operator) {
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
        ([Term]::FlatCombination) {
            [String]$combine = switch ($node.value.combine) {
                ([CmdbCombine]::And) { " AND " }
                ([CmdbCombine]::Or)  { " OR " }
            }
            return ($node.value.terms | ForEach-Object {
                "($(RenderAst $_))"
            }) -join $combine
        }
    }
}

function EvaluateExpression {
    param([String]$expr)
    return NormalizeAst (ParseExpression $expr)
}
