using module .\cmdb_handle.psm1

# Parses a regex pattern
# Extracts the named capture group 'val'.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#   [Regex]$regx - The regex pattern to match.
#
# Returns:
#   The extracted value from 'val' capture group.
#   The whole matched expression if 'val' is not found.
#   `$null` on failure.
function ParsePattern {
    param ([Ref]$expr, [Regex]$regx)
    if ($expr.Value -match $regx) {
        $expr.Value = $expr.Value -replace $regx, ""
        if ($Matches["val"]) { return $Matches["val"] }
        else { return $Matches[0] }
    } else { return $null }
}

# Parses a string value
# Supports plain, single-quoted, and double-quoted strings.
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   The extracted string value, unescaped.
#   `$null` on failure.
function ParseString {
    param ([Ref]$expr)
    [String]$plain = "(?<val>(\\.|\s*[^\s`"'!=~<>&|()])+)"
    [String]$single = "'(?<val>(\\.|[^'])+)'"
    [String]$double = "`"(?<val>(\\.|[^`"])+)`""
    return (ParsePattern $expr "^\s*($plain|$single|$double)") -replace "\\(.)", '$1'
}

# Parses a comparison operator
# Valid operators are:
# ~      (Match)
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
#   The extracted operator or `$null` if none is found.
function ParseOperator {
    param ([Ref]$expr)
    return ParsePattern $expr "^\s*(?<val>(~|==?|!=|<>|<=?|>=?))"
}

# Parses an AND combinator (&&, & or "AND").
#
# Arguments:
#   [Ref]$expr - The expression being parsed.
#
# Returns:
#   The extracted AND combinator or `$null` if none is found.
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
#   The extracted OR combinator or `$null` if none is found.
function ParseOr {
    param ([Ref]$expr)
    return ParsePattern $expr "^\s*(?<val>(\|{1,2}|[Oo][Rr]))"
}

# Maps a parsed operator string to a [CmdbOperator] enum value.
# Valid operators are:
# ~      (Match)
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
        ("~")   { return [CmdbOperator]::Match }
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
function ParseComparison {
    param ([Ref]$expr)
    [String]$field = ParseString $expr; if (-not $field) { return $null }
    [String]$oper = ParseOperator $expr; if (-not $oper) { return $null }
    [String]$value = ParseString $expr; if (-not $value) { return $null }
    return [PSCustomObject]@{
        field = $field
        value = $value
        operator = (MapOperator $oper)
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
# Throws on any unmatched opening parentheses.
function ParseTerm {
    param ([Ref]$expr)
    if ($expr.Value -match "^\s*(!|[Nn][Oo][Tt])\s*\(") {
        ParsePattern $expr "^\s*(!|[Nn][Oo][Tt])" | Out-Null
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
    [String]$combinator = ParseAnd $expr
    if (-not $combinator) { return $left }
    [PSCustomObject]$right = ParseAndChain $expr
    if (-not $right) { throw "Expected Expression after '$combinator'"}
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
    [String]$combinator = ParseOr $expr
    if (-not $combinator) { return $left }
    [PSCustomObject]$right = ParseTermChain $expr
    if (-not $right) { throw "Expected Expression after '$combinator'" }
    return [PSCustomObject]@{
        term_type = [Term]::Combination
        value = [PSCustomObject]@{
            left = $left; right = $right
            combine = [CmdbCombine]::Or
        }
    }
}

function ParseExpression {
    param ([String]$string)
    [PSCustomObject]$evaluated = ParseTermChain ([Ref]$string)
    if (-not $evaluated)  { throw "Expected Expression" }
    [String]$trail = ParsePattern ([Ref]$string) "^\s*(?<val>\S+)"
    if ($trail) { throw "Unexpected '$trail' after Expression" }
    return $evaluated
}
