using module .\cmdb_handle.psm1

###################
# SECTION PARSING #
###################

# Represents a successfully parsed object.
#
# Fields:
#   [Object]$val - The extracted and/or mapped value.
#   [String]$rem - Substring without the parsed part.
class ParseResult {
    [Object]$val
    [String]$rem
    ParseResult([Object]$val, [String]$rem) {
        $this.val = $val; $this.rem = $rem
    }
}

# Parses a regex pattern.
# Extracts the named capture group 'val'.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#   [Regex]$regx - The regex pattern to match.
#
# Returns:
#   A ParseResult with the extracted value of the `val` capture group.
#   `$null` on failure.
function ParsePattern {
    param ([String]$expr, [Regex]$regx)
    if ($expr -match $regx) {
        return [ParseResult]::New($Matches["val"], $expr.Substring($Matches[0].Length))
    } else { return $null }
}

# Parses a string value.
# Supports plain, single-quoted, and double-quoted strings.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A ParseResult with the extracted string value, unescaped.
#   `$null` on failure.
function ParseString {
    param ([String]$expr)
    [String]$plain = "(?<val>(\\.|\s*[^\s`"'!=~<>&|()])+)"
    [String]$single = "'(?<val>(\\.|[^'])*)'"
    [String]$double = "`"(?<val>(\\.|[^`"])*)`""
    [ParseResult]$str = ParsePattern $expr "^\s*($plain|$single|$double)"
    if ($null -eq $str) { return $null }
    $str.val = $str.val -replace "\\(.)", '$1'
    return $str
}

# Parses a comparison operator.
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
#   A ParseResult with the extracted operator.
#   `$null` if no operator is found.
function ParseOperator {
    param ([String]$expr)
    return ParsePattern $expr "^\s*(?<val>(=?~|!~|==?|!=|<>|<=?|>=?))"
}

# Parses an AND combinator (&&, & or "AND").
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A ParseResult with the extracted AND combinator.
#   `$null` if no AND combinator is found.
function ParseAnd {
    param ([String]$expr)
    return ParsePattern $expr "(?i)^\s*(?<val>(&{1,2}|and))"
}

# Parses an OR combinator (||, | or "OR").
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A ParseResult with the extracted OR combinator.
#   `$null` if no OR combinator is found.
function ParseOr {
    param ([String]$expr)
    return ParsePattern $expr "(?i)^\s*(?<val>(\|{1,2}|or))"
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
#   The corresponding `CmdbOperator` enum value.
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

enum Term {
    Comparison
    Combination
    FlatCombination
    Inversion
    Boolean
    Symbol
}

# Parses a single comparison (e.g., `age >= 30`).
# The left and right side are parsed as any valid string value.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   - `term_type` is always.
#   `$null` on failure.
#
# Throws on an empty `field` string.
# Throws on an operator without following value.
function ParseComparison {
    param ([String]$expr)
    [ParseResult]$field = ParseString $expr; if ($field) { $expr = $field.rem } else { return $null }
    [ParseResult]$oper = ParseOperator $expr; if ($oper) { $expr = $oper.rem } else { return $null }
    [ParseResult]$value = ParseString $expr
    if (-not $value) { throw "Expected Value after '$($oper.val)'" }
    if (-not $field.val) { throw "Found Comparison with an empty field" }
    return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Comparison
        value = [PSCustomObject]@{
            field = $field.val
            value = $value.val
            operator = MapOperator $oper.val
        }
    }, $value.rem)
}

# Converts a boolean value to a Term.
#
# Arguments:
#   [Bool]$value - The value of the boolean.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   - `value` holds the boolean.
function BoolTerm {
    param ([Bool]$value)
    return [PSCustomObject]@{
        term_type = [Term]::Boolean
        value = $value
    }
}

function ParseBool {
    param ([String]$expr)
    [ParseResult]$bool = ParsePattern $expr "(?i)^\s*(?<val>true|false)"
    if (-not $bool) { return $null }
    [Bool]$val = $bool.val.ToLower() -eq "true"
    return [ParseResult]::New((BoolTerm $val), $bool.rem)
}

function ParseSymbol {
    param ([String]$expr)
    [ParseResult]$sym = ParsePattern $expr "(?i)^\s*(?<val>[a-z][0-9a-z_-]*)"; if (-not $sym) { return $null }
    return [ParseResult]::New([PSCustomObject]@{ term_type = [Term]::Symbol; value = $sym.val }, $sym.rem)
}

function ParseGroupedTerm {
    param ([String]$expr)
    [ParseResult]$lparen = ParsePattern $expr "^\s*\("
    if ($lparen) { $expr = $lparen.rem } else { return $null }
    [ParseResult]$inner = ParseTermChain $expr
    if ($inner) { $expr = $inner.rem } else { throw "Expected Expression inside Parentheses" }
    [ParseResult]$rparen = ParsePattern $expr "^\s*\)"
    if (-not $rparen) { throw "Found unmatched '('" }
    return [ParseResult]::New($inner.val, $rparen.rem)
}

function ParseInverseTerm {
    param ([String]$expr)
    [ParseResult]$inv = ParsePattern $expr "(?i)^\s*(?<val>!|not)"
    if ($inv) { $expr = $inv.rem } else { return $null }
    [ParseResult]$comp = ParseComparison $expr
    if ($comp) { throw "Expected Parentheses or Symbol after '$($inv.val)'" }
    [ParseResult]$inv = ParseInverseTerm $expr
    if ($inv) { return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Inversion; value = $inv.val
    }, $inv.rem) }
    [ParseResult]$group = ParseGroupedTerm $expr
    if ($group) { return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Inversion; value = $group.val
    }, $group.rem) }
    [ParseResult]$bool = ParseBool $expr
    if ($bool) { return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Inversion; value = $bool.val
    }, $bool.rem) }
    [ParseResult]$symbol = ParseSymbol $expr
    if ($symbol) { return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Inversion; value = $symbol.val
    }, $symbol.rem) }
    return $null
}

# Parses a term, which can be a comparison, an inversion,
# a grouped subexpression in parentheses, a boolean value,
# or a symbol.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   $null on failure.
#
# Throws on inversion without following parentheses.
# Throws on any unmatched opening parentheses.
function ParseTerm {
    param ([String]$expr)
    [ParseResult]$comp = ParseComparison $expr; if ($comp) { return $comp }
    [ParseResult]$group = ParseGroupedTerm $expr; if ($group) { return $group }
    [ParseResult]$inv = ParseInverseTerm $expr; if ($inv) { return $inv }
    [ParseResult]$bool = ParseBool $expr; if ($bool) { return $bool }
    return ParseSymbol $expr
}

# Parses a chain of AND-connected terms.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   $null on failure.
#
# Throws on any dangling operator.
function ParseAndChain {
    param ([String]$expr)
    [ParseResult]$left = ParseTerm $expr; if ($left) { $expr = $left.rem } else { return $null }
    [ParseResult]$comb = ParseAnd $expr; if ($comb) { $expr = $comb.rem } else { return $left }
    [ParseResult]$right = ParseAndChain $expr; if (-not $right) { throw "Expected Expression after '$($comb.val)'" }
    return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Combination
        value = [PSCustomObject]@{
            left = $left.val; right = $right.val
            combine = [CmdbCombine]::And
        }
    }, $right.rem)
}

# Parses a chain of OR-connected terms.
# Uses `ParseAndChain` to handle precedence.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   $null on failure.
#
# Throws on any dangling operator.
function ParseTermChain {
    param ([String]$expr)
    [ParseResult]$left = ParseAndChain $expr; if ($left) { $expr = $left.rem } else { return $null }
    [ParseResult]$comb = ParseOr $expr; if ($comb) { $expr = $comb.rem } else { return $left }
    [ParseResult]$right = ParseTermChain $expr; if (-not $right) { throw "Expected Expression after '$($comb.val)'" }
    return [ParseResult]::New([PSCustomObject]@{
        term_type = [Term]::Combination
        value = [PSCustomObject]@{
            left = $left.val; right = $right.val
            combine = [CmdbCombine]::Or
        }
    }, $right.rem)
}

# Parses an entire logic expression into a structured AST.
#
# Examples:
# ```.
# ParseExpression("location ~ RZ%").
# ParseExpression("age >= 30 AND (name = 'Alice' OR NOT (city = 'Paris'))").
# ````.
#
# Arguments:
#   [String]$string - The input expression to parse.
#
# Returns:
#   A structured `PSCustomObject` representing the parsed expression.
#
# Throws on any syntax errors.
function ParseExpression {
    param ([String]$expr)
    [ParseResult]$evaluated = ParseTermChain $expr
    if (-not $evaluated) { throw "Expected Expression" }
    [ParseResult]$trail = ParsePattern $evaluated.rem "^\s*(?<val>\S+)"
    if ($trail) { throw "Unexpected '$($trail.val)' after Expression" }
    return $evaluated.val
}


######################
# SECTION EVALUATION #
######################

# Checks if two term-nodes are equal.
#
# Arguments:
#   [`PSCustomObject`]$term_a - The first term.
#   [`PSCustomObject`]$term_b - The second term.
#
# Returns:
#   $true if the terms are equal or both $null.
#   $false otherwise.
function CompareTerms {
    param ([PSCustomObject]$term_a, [PSCustomObject]$term_b)
    if ($null -eq $term_a -and $null -eq $term_b) { return $true }
    if ($null -eq $term_a -or $null -eq $term_b) { return $false }
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
        ([Term]::Symbol) { return $term_a.value -eq $term_b.value }
    }
}

# Returns the logical inverse of a CmdbOperator.
#
# These are all operators and their inverses:
#   IsMatch      <>  IsNotMatch.
#   IsEqual      <>  IsNotEqual.
#   LessThan     <>  GreaterThanOrEqual.
#   GreaterThan  <>  LessThanOrEqual.
#
# Arguments:
#   [CmdbOperator]$operator - The input operator.
#
# Returns:
#   A CmdbOperator that is inverse to the input.
function InvertOperator {
    param ([CmdbOperator]$operator)
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

# Returns the logical inverse of a CmdbCombine.
# Input `And` returns `Or` and vice-versa.
#
# Arguments:
#   [CmdbCombine]$combine - The input combinator.
#
# Returns:
#   A CmdbCombine that is inverse to the input.
function InvertCombine {
    param ([CmdbCombine]$combine)
    switch ($combine) {
        ([CmdbCombine]::And) { return [CmdbCombine]::Or }
        ([CmdbCombine]::Or) { return [CmdbCombine]::And }
    }
}

# Returns the logical inverse of a term.
# Uses `InvertCombine` and `InvertOperator` as helper functions.
# Uses `NormalizeAST` to return a simplified value.
#
# Arguments:
#   [`PSCustomObject`]$node - The input term.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   The term is returned in normalized form.
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
        ([Term]::Symbol) {
            return [PSCustomObject]@{ term_type = [Term]::Inversion; value = $node }
        }
        ([Term]::Boolean) { return BoolTerm (-not $node.value) }
        ([Term]::Inversion) { return NormalizeAst $node.value }
    }
}

# Converts a Combination term into a FlatCombination.
# If any of the child terms are compatible FlatCombinations,
# they will also get desolved into the parent term.
#
# Arguments:
#   [`PSCustomObject`[]]$terms - The children terms.
#   [CmdbCombine]$combine - The combinator of the combination.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   This term is always a FlatCombination.
function FlattenCombination {
    param ([PSCustomObject[]]$terms, [CmdbCombine]$combine)
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

# Distributes a term into a flat combination-term.
# Also normalizes and flattens the terms.
#
# Arguments:
#   [`PSCustomObject`]$term - The term that will get distributed.
#   [`PSCustomObject`]$into - The combination to distribute the term into.
#   [CmdbCombine]$combine - The combinator to join terms with.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   - This term is always a FlatCombination.
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

# Normalizes an entire logic-AST.
# Resolves all Inversions by applying DeMorgan's Law.
# Distributes And-combinations into child-Or-combinations.
# Flattens the tree into a single flat Or-Combination,
# that contains only flat And-Combinations (DNF).
# Eliminates redundant comparisons.
# Evaluates tautologies and oximora.
#
# Arguments:
#   [`PSCustomObject`]$node - The AST root term.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   - The term is returned in disjunctive normal form.
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
        ([Term]::FlatCombination) { return $node } # Already Normalized.
        ([Term]::Comparison)      { return $node }
        ([Term]::Boolean)         { return $node }
        ([Term]::Symbol)          { return $node }
    }
}


#####################
# SECTION INTERFACE #
#####################

# Converts a logical term into a readable string.
#
# Arguments:
#   [`PSCustomObject`]$node - The input term.
#
# Returns:
#   A String representing the term.
function RenderAst {
    param ([PSCustomObject]$node)
    switch ($node.term_type) {
        ([Term]::Boolean) { return "$(if ($node.value) { "True" } else { "False" })" }
        ([Term]::Inversion) { return "!$(RenderAst $node.value)" }
        ([Term]::Symbol) { return $node.value }
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
                ([CmdbCombine]::And) { " & " }
                ([CmdbCombine]::Or)  { " | " }
            }
            return ($node.value.terms | ForEach-Object {
                RenderAst $_
            }) -join $combine
        }
    }
}

# Parses and normalizes a logic expression.
#
# Arguments:
#   [String]$expr - The input expression to parse.
#
# Returns:
#   A `PSCustomObject` with `term_type` and `value`.
#   - The term is returned in disjunctive normal form.
function EvaluateExpression {
    param ([String]$expr)
    return NormalizeAst (ParseExpression $expr)
}
