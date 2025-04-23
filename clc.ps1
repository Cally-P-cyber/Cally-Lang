<# 
    Cally Lang Interpreter in PowerShell
    This script tokenizes, parses, and evaluates a .clp file.

    Parameters:
      - Switch flags:
            -ast : Outputs the AST (in JSON format).
            -ts  : Outputs the token stream.
	  - args:
			- additional aguments you would want to add on start up
    Usage:
      .\clc.ps1 <Path_To_Your_Program.clp> <args> [-ast] [-ts] <br/>
#>

param(
    [Parameter(Mandatory = $true)]
    [string]$FilePath,
    [Parameter(ValueFromRemainingArguments = $true)]
    $LangArgs,
    [switch]$ast,
    [switch]$ts
)

if (!(Test-Path $FilePath)) {
    Write-Error "File not found: $FilePath"
    exit 1
}

$sourceLines = Get-Content $FilePath
if (-not $sourceLines) {
    Write-Error "File is empty or cannot be read: $FilePath"
    exit 1
}
$source = $sourceLines -join "`r`n"

$processedLines = foreach ($line in $sourceLines) {
    $line -replace '//.*', ''
}
$source = $processedLines -join "`r`n"

$global:UserFuncs = @{}
$global:Env = @{}

function Tokenize($source) {
    $tokens = @()
    $lines = $source -split "`r?`n"
    $lineCount = $lines.Count
    $symbols = @("(", ")", "{", "}", "[", "]", ";", ",", ".", "+", "-", "*", "/", "%", "=", "<", ">", "!", ":", "$")
    $multiCharOps = @("==", "!=", "<=", ">=", "&&", "||", "++", "--")
    $keywords = @("Main", "if", "else", "while", "for", "print", "input", "parseInt", "len", "func", "return" )
    
    for ($i = 0; $i -lt $lineCount; $i++) {
        $lineText = $lines[$i]
        $pos = 0
        $len = $lineText.Length
        while ($pos -lt $len) {
            $char = $lineText[$pos]
            if ([char]::IsWhiteSpace($char)) {
                $pos++
                continue
            }
            if ($char -eq '"') {
                $pos++
                $startPos = $pos
                $str = ""
                while ($pos -lt $len -and $lineText[$pos] -ne '"') {
                    $str += $lineText[$pos]
                    $pos++
                }
                if ($pos -lt $len -and $lineText[$pos] -eq '"') { $pos++ }
                $tokens += [PSCustomObject]@{
                    type        = "String"
                    value       = $str
                    line        = $i + 1
                    columnStart = $startPos
                }
                continue
            }
            if ($char -match '[0-9]') {
                $startPos = $pos
                $num = ""
                while ($pos -lt $len -and $lineText[$pos] -match '[0-9]') {
                    $num += $lineText[$pos]
                    $pos++
                }
                $tokens += [PSCustomObject]@{
                    type        = "Number"
                    value       = [int]$num
                    line        = $i + 1
                    columnStart = $startPos + 1
                }
                continue
            }
            if ($char -match '[A-Za-z_]') {
                $startPos = $pos
                $ident = ""
                while ($pos -lt $len -and $lineText[$pos] -match '[A-Za-z0-9_]') {
                    $ident += $lineText[$pos]
                    $pos++
                }
                if ($keywords -contains $ident) {
                    $tokenType = "Keyword"
                }
                else {
                    $tokenType = "Identifier"
                }
                $tokens += [PSCustomObject]@{
                    type        = $tokenType
                    value       = $ident
                    line        = $i + 1
                    columnStart = $startPos + 1
                }
                continue
            }
            if ($pos + 1 -lt $len) {
                $pair = $lineText.Substring($pos, 2)
                if ($multiCharOps -contains $pair) {
                    $tokens += [PSCustomObject]@{
                        type        = "Operator"
                        value       = $pair
                        line        = $i + 1
                        columnStart = $pos + 1
                    }
                    $pos += 2
                    continue
                }
            }
            if ($symbols -contains $char) {
                $tokens += [PSCustomObject]@{
                    type        = "Symbol"
                    value       = $char
                    line        = $i + 1
                    columnStart = $pos + 1
                }
                $pos++
                continue
            }
            Write-Warning "Skipping unknown char '$char' at line $($i + 1), col $($pos + 1)"
            $pos++
        }
    }
    return $tokens
}

$tokens = Tokenize $source

if ($ts) {
    Write-Host "Token Stream:"
    $tokens | ForEach-Object { "[Line $($_.line):Col $($_.columnStart)] $($_.type): $($_.value)" }
}

$global:tokenIndex = 0

function PeekToken() {
    if ($global:tokenIndex -lt $tokens.Count) { return $tokens[$global:tokenIndex] }
    return $null
}

function NextToken() {
    $token = PeekToken
    $global:tokenIndex++
    return $token
}

function ExpectToken($expectedValue) {
    $token = NextToken
    if (-not $token) { throw "Parse Error: Expected '$expectedValue' but reached end of input." }
    if ($token.value -ne $expectedValue) {
        throw ("Parse Error [line {0}, col {1}]: Expected '{2}' but got '{3}'." -f $token.line, $token.columnStart, $expectedValue, $token.value)
    }
    return $token
}

function New-ASTNode($type, $props) {
    $node = @{ type = $type; line = $null; column = $null }
    if ($props.ContainsKey("line")) { $node.line = $props["line"] }
    if ($props.ContainsKey("column")) { $node.column = $props["column"] }
    foreach ($key in $props.Keys) {
        if ($key -notin @("type", "line", "column")) { $node[$key] = $props[$key] }
    }
    return [PSCustomObject]$node
}

function ParseArrayLiteral() {
    $openBracket = NextToken  # Consume "["
    $elements = @()
    while ($true) {
        $peek = PeekToken
        if (-not $peek) { throw "Parse Error: Missing closing ']' for array literal." }
        if ($peek.value -eq "]") { NextToken | Out-Null; break }
        $element = ParseExpression
        $elements += $element
        $peek = PeekToken
        if ($peek -and $peek.value -eq ",") { NextToken | Out-Null; continue }
    }
    return New-ASTNode "ArrayLiteral" @{
        elements = $elements
        line     = $openBracket.line
        column   = $openBracket.columnStart
    }
}

function ParseObjectLiteral() {
    $openBrace = NextToken  # Consume "{"
    $properties = @{}
    while ($true) {
        $peek = PeekToken
        if (-not $peek) { throw "Parse Error: Missing closing '}' for object literal." }
        if ($peek.value -eq "}") { NextToken | Out-Null; break }
        
        # Expect a key (identifier or string)
        if ($peek.type -eq "Identifier" -or $peek.type -eq "String") {
            $keyToken = NextToken
            $key = $keyToken.value
        }
        else {
            throw ("Parse Error [line {0}, col {1}]: Object keys must be identifiers or strings." `
                    -f $peek.line, $peek.columnStart)
        }
        
        $dummy = ExpectToken ":"  # consume colon
        $value = ParseExpression
        $properties[$key] = $value
        
        $peek = PeekToken
        if ($peek -and $peek.value -eq ",") { NextToken | Out-Null; continue }
    }
    return New-ASTNode "ObjectLiteral" @{
        properties = $properties
        line       = $openBrace.line
        column     = $openBrace.columnStart
    }
}

function ParseExpression() {
    $node = ParsePostfix
    while ($true) {
        $token = PeekToken
        if (-not $token) { break }
        if ($token.type -eq "Operator" -or ($token.type -eq "Symbol" -and "+-*/%<>".Contains($token.value))) {
            $op = NextToken
            $right = ParsePostfix 
            $node = New-ASTNode "BinaryOp" @{
                left   = $node
                op     = $op.value
                right  = $right
                line   = $op.line
                column = $op.columnStart
            }
        }
        else { break }
    }
    return $node
}

function ParsePostfix() {
    $node = ParsePrimary
    while ($true) {
        $peek = PeekToken
        if (-not $peek) { break }
        if ($peek.value -eq "[") {
            NextToken | Out-Null
            $indexExpr = ParseExpression
            $dummy = ExpectToken "]"
            $node = New-ASTNode "ArrayAccess" @{ base = $node; index = $indexExpr; line = $peek.line; column = $peek.columnStart }
            continue
        }
        if ($peek.value -eq "(") {
            $openPar = NextToken
            $args = @()
            while ($true) {
                $next = PeekToken
                if (-not $next) { throw "Parse Error: Missing closing ')' after function call" }
                if ($next.value -eq ")") { NextToken | Out-Null; break }
                $args += ParseExpression
                if ((PeekToken).value -eq ",") { NextToken | Out-Null }
            }
            $node = New-ASTNode "FuncCall" @{ name = ($node.name); args = $args; line = $openPar.line; column = $openPar.columnStart }
            continue
        }
        if ($peek.value -eq ".") {
            NextToken | Out-Null
            $propToken = PeekToken
            if (-not $propToken -or ($propToken.type -ne "Identifier" -and $propToken.type -ne "String")) {
                throw ("Parse Error [line {0}, col {1}]: Expected identifier after '.'" -f $propToken.line, $propToken.columnStart)
            }
            $prop = NextToken
            $node = New-ASTNode "PropertyAccess" @{ base = $node; property = $prop.value; line = $prop.line; column = $prop.columnStart }
            continue
        }
        break
    }
    return $node
}


function ParseReturnStatement() {
    $retTok = NextToken
    $expr = $null
    if ((PeekToken) -and (PeekToken).value -ne ";") {
        $expr = ParseExpression
    }
    $dummy = ExpectToken ";"
    return New-ASTNode "Return" @{
        value  = $expr
        line   = $retTok.line
        column = $retTok.columnStart
    }
}

function ParsePrimary() {
    $token = PeekToken
    if (-not $token) { throw "Parse Error: Unexpected end of input in expression." }
    
    if ($token.type -eq "Identifier" -and $token.value -eq "p") {
        # Peek ahead: if the next token is "{" then we parse the PS block.
        $next = PeekToken
        # Note: since "p" is an identifier, it’s already returned by PeekToken.
        # We need to consume the "p" token and then ensure that the next token is "{"
        $dummy = NextToken  # consume "p"
        $next = PeekToken
        if ($next -and $next.value -eq "{") {
            return ParsePSBlock
        }
        else {
            # If "p" is not followed by a block, treat it as a normal variable.
            return New-ASTNode "Variable" @{
                name   = "p"
                line   = $token.line
                column = $token.columnStart
            }
        }
    }
    
    switch ($token.type) {
        "Number" {
            NextToken | Out-Null
            return New-ASTNode "Number" @{
                value  = $token.value
                line   = $token.line
                column = $token.columnStart
            }
        }
        "String" {
            NextToken | Out-Null
            return New-ASTNode "String" @{
                value  = $token.value
                line   = $token.line
                column = $token.columnStart
            }
        }
        "Identifier" {
            $identToken = NextToken
            $varNode = New-ASTNode "Variable" @{
                name   = $identToken.value
                line   = $identToken.line
                column = $identToken.columnStart
            }
            $next = PeekToken
            if ($next -and $next.type -eq "Operator" -and ($next.value -in @("++", "--"))) {
                $opToken = NextToken
                return New-ASTNode "UnaryOp" @{
                    op     = $opToken.value
                    value  = $varNode
                    line   = $opToken.line
                    column = $opToken.columnStart
                }
            }
            return $varNode
        }
        "Keyword" {
            $kwToken = NextToken
            if ($kwToken.value -in @("if", "while", "for")) {
                throw ("Parse Error [line {0}, col {1}]: Unexpected keyword '{2}' in expression." `
                        -f $kwToken.line, $kwToken.columnStart, $kwToken.value)
            }
            $next = PeekToken
            if ($next -and $next.value -eq "(") {
                NextToken | Out-Null
                $args = @()
                while ($true) {
                    $next = PeekToken
                    if (-not $next) { throw "Parse Error: Missing closing ')' after $($kwToken.value)(...)" }
                    if ($next.value -eq ")") { NextToken | Out-Null; break }
                    $arg = ParseExpression
                    $args += $arg
                    $next = PeekToken
                    if ($next -and $next.value -eq ",") { NextToken | Out-Null; continue }
                }
                return New-ASTNode "FuncCall" @{
                    name   = $kwToken.value
                    args   = $args
                    line   = $kwToken.line
                    column = $kwToken.columnStart
                }
            }
            else {
                return New-ASTNode "Variable" @{
                    name   = $kwToken.value
                    line   = $kwToken.line
                    column = $kwToken.columnStart
                }
            }
        }
        default {
            if ($token.value -eq "(") {
                NextToken | Out-Null
                $expr = ParseExpression
                $dummy = ExpectToken ")"
                return $expr
            }
            elseif ($token.value -eq "[") {
                return ParseArrayLiteral
            }
            elseif ($token.value -eq "{") {
                return ParseObjectLiteral
            }
            else {
                throw ("Parse Error [line {0}, col {1}]: Unexpected token '{2}' ({3})." `
                        -f $token.line, $token.columnStart, $token.value, $token.type)
            }
        }
    }
}

function ParseStatement {
    # skip leading semicolons
    while ($true) {
        $temp = PeekToken
        if (-not $temp) { break }
        if ($temp.value -eq ";") { NextToken | Out-Null }
        else { break }
    }

    $token = PeekToken
    if (-not $token) { return $null }

    if ($token.type -eq "Keyword") {
        switch ($token.value) {
            "if" { return ParseIfStatement }
            "while" { return ParseWhileStatement }
            "for" { return ParseForStatement }
            "return" { return ParseReturnStatement }
        }
    }

    # normal expression (may later be part of an assignment)
    $expr = ParseExpression

    # a lone PowerShell block needs no trailing semicolon
    if ($expr.type -eq "PSBlock") {
        return New-ASTNode "ExprStmt" @{ expression = $expr; line = $expr.line; column = $expr.column }
    }

    # assignment?
    if ((PeekToken) -and (PeekToken).value -eq "=") {
        NextToken | Out-Null
        $rhs = ParseExpression
        ExpectToken ";" | Out-Null
        return New-ASTNode "Assignment" @{
            variable = $expr; value = $rhs; line = $expr.line; column = $expr.column 
        }
    }

    # plain expression statement → must end with ;
    ExpectToken ";" | Out-Null
    return New-ASTNode "ExprStmt" @{ expression = $expr; line = $expr.line; column = $expr.column }
}
function EvalBlock ($stmts) {
    foreach ($s in $stmts) {
        if ($null -eq $s) { continue }
        $r = EvalStatement $s
        if ($r -and $r.PSObject.Properties.Name -contains '_rt') {
            return $r          # bubble the return object up the stack
        }
    }
    return $null
}

function ParseFuncDefinition {
    $dummy = ExpectToken "func"
    $nameToken = PeekToken
    if (-not $nameToken -or $nameToken.type -ne "Identifier") { throw ("Parse Error [line {0}, col {1}]: Function name expected after 'func'." -f $nameToken.line, $nameToken.columnStart) }
    $funcName = (NextToken).value
    $params = @()
    if ((PeekToken).value -eq "(") {
        NextToken | Out-Null
        while ($true) {
            $peek = PeekToken
            if ($peek.value -eq ")") { NextToken | Out-Null; break }
            if ($peek.type -ne "Identifier") { throw ("Parse Error [line {0}, col {1}]: Param name expected" -f $peek.line, $peek.columnStart) }
            $params += (NextToken).value
            if ((PeekToken).value -eq ",") { NextToken | Out-Null }
        }
    }
    $body = ParseBlock
    return New-ASTNode "FuncDef" @{
        name   = $funcName
        params = $params
        body   = $body
        line   = $nameToken.line
        column = $nameToken.columnStart
    }
}

function ParsePSBlock() {
    $openBrace = NextToken  # consume '{'
    $rawCode = ""
    $braceCount = 1
    # Define symbols that should be concatenated with no extra space.
    $noSpaceSymbols = @("(", ")", "{", "}", "[", "]", "$", ".", ",", ";", ":", "+", "-", "*", "/", "%", "=", "<", ">", "!", "++", "--")
    while ($braceCount -gt 0) {
        $token = NextToken
        if (-not $token) { throw "Parse Error: Missing closing '}' for PS block." }
        if ($token.value -eq "{") {
            $braceCount++
        }
        elseif ($token.value -eq "}") {
            $braceCount--
            if ($braceCount -eq 0) { break }
        }
        # Append token.value without extra space if token is in the no-space list; otherwise add a space.
        if ($noSpaceSymbols -contains $token.value) {
            # make sure any trailing space we just added is removed
            $rawCode = $rawCode.TrimEnd() + $token.value
        }
        else {
            $rawCode += $token.value + " "
        }
    }
    $rawCode = $rawCode.Trim()
    return New-ASTNode "PSBlock" @{
        code   = $rawCode
        line   = $openBrace.line
        column = $openBrace.columnStart
    }
}

function ParseBlock() {
    $openBrace = PeekToken
    if (-not $openBrace -or $openBrace.value -ne "{") {
        throw ("Parse Error [line {0}, col {1}]: Expected '{{' but got '{2}'." -f $openBrace.line, $openBrace.columnStart, $openBrace.value)
    }
    NextToken | Out-Null
    $stmts = @()
    while ($true) {
        $token = PeekToken
        if (-not $token) { throw "Parse Error: Missing closing '}' for block." }
        if ($token.value -eq "}") {
            NextToken | Out-Null
            break
        }
        $stmt = ParseStatement
        if ($stmt) { $stmts += $stmt }
    }
    return $stmts
}

function ParseIfStatement() {
    $ifToken = NextToken
    $dummy = ExpectToken "("
    $condition = ParseExpression
    $dummy = ExpectToken ")"
    $thenBlock = ParseBlock
    $elseBlock = @()
    $next = PeekToken
    if ($next -and $next.type -eq "Keyword" -and $next.value -eq "else") {
        NextToken | Out-Null
        $elseBlock = ParseBlock
    }
    return New-ASTNode "If" @{
        condition = $condition
        then      = $thenBlock
        else      = $elseBlock
        line      = $ifToken.line
        column    = $ifToken.columnStart
    }
}

function ParseWhileStatement() {
    $whileToken = NextToken
    $dummy = ExpectToken "("
    $condition = ParseExpression
    $dummy = ExpectToken ")"
    $body = ParseBlock
    return New-ASTNode "While" @{
        condition = $condition
        body      = $body
        line      = $whileToken.line
        column    = $whileToken.columnStart
    }
}

function ParseForStatement() {
    $forToken = NextToken
    $dummy = ExpectToken "("
    $init = $null
    $peek = PeekToken
    if ($peek -and $peek.value -ne ";") {
        $lhs = ParseExpression
        $maybeEq = PeekToken
        if ($maybeEq -and $maybeEq.value -eq "=") {
            NextToken | Out-Null
            $rhs = ParseExpression
            $init = New-ASTNode "Assignment" @{
                variable = $lhs
                value    = $rhs
                line     = $lhs.line
                column   = $lhs.column
            }
        }
        else {
            # Wrap the raw expression in an ExprStmt so it is recognized later.
            $init = New-ASTNode "ExprStmt" @{
                expression = $lhs
                line       = $lhs.line
                column     = $lhs.column
            }
        }
    }
    $dummy = ExpectToken ";"
    $condition = $null
    $peek = PeekToken
    if ($peek -and $peek.value -ne ";") {
        $condition = ParseExpression
    }
    $dummy = ExpectToken ";"
    $update = $null
    $peek = PeekToken
    if ($peek -and $peek.value -ne ")") {
        $update = ParseExpression
    }
    $dummy = ExpectToken ")"
    $body = ParseBlock
    return New-ASTNode "For" @{
        init      = $init
        condition = $condition
        update    = $update
        body      = $body
        line      = $forToken.line
        column    = $forToken.columnStart
    }
}

function ParseProgram() {
    $token = NextToken
    if (-not $token -or $token.value -ne "Main") {
        $line = if ($token) { $token.line } else { "-" }
        $col = if ($token) { $token.columnStart } else { "-" }
        throw ("Program must start with 'Main'. Found '{0}' at line {1}, col {2}." -f ($token.value), $line, $col)
    }

    # Check if the next token is "(" to parse parameters.
    $next = PeekToken
    if ($next -and $next.value -eq "(") {
        $dummy = ExpectToken "("
        $paramToken = NextToken
        $global:Env[$paramToken.value] = $LangArgs  
        if (-not $paramToken) { throw "Parse Error: Missing parameter in Main(...)" }
        $dummy = ExpectToken ")"
    }
    
    $body = ParseBlock

    $funcDefs = @()
    while ($true) {
        $peek = PeekToken
        if (-not $peek) { break }
        if ($peek.type -eq "Keyword" -and $peek.value -eq "func") {
            $funcDefs += ParseFuncDefinition
        }
        else {
            throw "Parse Error: extra tokens after program"
        }
    }

    return New-ASTNode "Program" @{
        body   = $body
        funcs  = $funcDefs
        line   = $token.line
        column = $token.columnStart
    }
}

$programAST = ParseProgram

foreach ($f in $programAST.funcs) {
    $global:UserFuncs[$f.name] = $f
}

if ($ast) {  
    Write-Host "AST:"
    $programAST | ConvertTo-Json -Depth 10 | Write-Host
}

function Builtin_print {
    param(
        $printv, 
        $type
    )
    $output = ""
    foreach ($p in $printv) {
        # Check if the value is an array or a hashtable (object)
        if ($p -is [array] -or $p -is [hashtable]) {
            # Convert to a JSON-like string
            $processed = $p | ConvertTo-Json -Compress
        }
        # Replace literal "\n" with an actual newline if needed
        $processed = $p -replace '\\n', "`n"
        $output += $processed
    }
    Write-Host -NoNewline $output
    return $null
}


function Read-Host($prompt) {
    return & Microsoft.PowerShell.Utility\Read-Host
}
function Builtin_input ($prompt = '') {
    if ($prompt) {
        foreach ($p in $prompt) {
            # Check if the value is an array or a hashtable (object)
            if ($p -is [array] -or $p -is [hashtable]) {
                # Convert to a JSON-like string
                $processed = $p | ConvertTo-Json -Compress
            }
            # Replace literal "\n" with an actual newline if needed
            $processed = $p -replace '\\n', "`n"
            $output += $processed
        }
        Write-Host -NoNewline $output
    }
    $raw = Read-Host
    $raw = $raw.Trim()
    if ($raw -match '^-?\d+$') { return [int]   $raw }   # whole number
    elseif ($raw -match '^-?\d+\.\d+$') { return [double]$raw }   # decimal
    elseif ($raw -match '^(true|false)$') { return [bool]  $raw }   # Boolean literal
    else { return $raw }          # plain string
}


function Builtin_len {
    param(
        $len, 
        $type
    )
    if ($len.Count -ge 1) {
        if ($type -eq [string]) { 
            $arg = [string]$len
            return $arg.Length 
        }
        elseif ($type -is [object]) { return $len.Count }
    }
    return 0
}

$global:BuiltinFuncs = @{
    "print"    = { param($value, $type) Builtin_print $value $type }
    "input"    = { param($value, $ignored = $null) Builtin_input $value }
    "len"      = { param($value, $type) Builtin_len $value $type }
    "parseInt" = { param($v, $ignored = $null) [int]::Parse($v) }
}

function EvalExpression($node) {
    if (-not $node) { return $null }
    switch ($node.type) {
        "Number" { return $node.value }
        "String" { return $node.value }
        "Variable" {
            $n = $node.name
            if ($global:Env.ContainsKey($n)) { return $global:Env[$n] }
            else {
                throw ("Runtime Error [line {0}, col {1}]: Variable '{2}' not defined." -f $node.line, $node.column, $n)
            }
        }
        "PSBlock" {
            $code = $node.code.Trim()
			
            # For each variable in the global environment, replace all occurrences of
            # the pattern ${{ key }} with its value (converted to a string).
            foreach ($key in $global:Env.Keys) {
                $pattern = "\$\{\{\s*$key\s*\}\}"
                $replacement = $global:Env[$key].ToString()
                $code = $code -replace $pattern, $replacement
            }

            # If no explicit "return" is found anywhere in the code, append one.
            if ($code -notmatch '\breturn\b') {
                $code = $code + "; return $null"
            }
			
            # (Optional) For debugging, output the final code:
            # Write-Host "PSBlock code: $code"
			
            $sb = [scriptblock]::Create($code)
            $result = & $sb
            return $result
        }
        "ArrayAccess" {
            $baseValue = EvalExpression $node.base
            $indexValue = EvalExpression $node.index
            if($indexValue -lt 0) { throw "Error [line {0}, col {1}]: Index out of bounds" -f $node.line, $node.column  }
            # If the base value is a string, convert the indexed char to a string.
            if ($baseValue -is [string]) {
                return [string]$baseValue[$indexValue]
            }
            else {
                return $baseValue[$indexValue]
            }
        }
        "PropertyAccess" {
            $baseValue = EvalExpression $node.base
            $propName = $node.property
            return $baseValue[$propName]
        }
        "BinaryOp" {
            $l = EvalExpression $node.left
            $r = EvalExpression $node.right
            switch ($node.op) {
                "+" { return $l + $r }
                "-" { return $l - $r }		"ArrayAccess" {
                    $baseValue = EvalExpression $node.base
                    $indexValue = EvalExpression $node.index
                    # If the base value is a string, convert the indexed char to a string.
                    if ($baseValue -is [string]) {
                        return [string]$baseValue[$indexValue]
                    }
                    else {
                        return $baseValue[$indexValue]
                    }
                }
                "*" { return $l * $r }
                "/" {
                    if ($r -eq 0) {
                        throw ("Runtime Error [line {0}, col {1}]: Division by zero." -f $node.line, $node.column)
                    }
                    return [int]($l / $r)
                }
                "%" { return $l % $r }
                "==" { return ($l -eq $r) }
                "!=" { return ($l -ne $r) }
                "<" { return ($l -lt $r) }
                "<=" { return ($l -le $r) }
                ">" { return ($l -gt $r) }
                ">=" { return ($l -ge $r) }
                default {
                    throw ("Runtime Error [line {0}, col {1}]: Unsupported operator '{2}'" -f $node.line, $node.column, $node.op)
                }
            }
        }
        "UnaryOp" {
            $val = EvalExpression $node.value
            switch ($node.op) {
                "++" { 
                    $global:Env[$node.value.name] = [int]$val + 1
                    return $val 
                }
                "--" { 
                    $global:Env[$node.value.name] = [int]$val - 1
                    return $val 
                }
                default {
                    throw ("Runtime Error [line {0}, col {1}]: Unsupported operator '{2}'" -f $node.line, $node.column, $node.op)
                }
            }
        }
        "FuncCall" {
            $fname = $node.name
            $vals = @(); foreach ($a in $node.args) { $vals += ,(EvalExpression $a) }

            if ($global:BuiltinFuncs.ContainsKey($fname)) {
                if($fname -eq "len"){
                    $typeName = $global:Env[$node.args[0].name].GetType().Name
                    return & $global:BuiltinFuncs[$fname] $vals $typeName
                }
                $typeName = $vals.GetType().Name
                return & $global:BuiltinFuncs[$fname] $vals $typeName
            }
            elseif ($global:UserFuncs.ContainsKey($fname)) {
                $f = $global:UserFuncs[$fname]
                if ($vals.Count -ne $f.params.Count) { throw "Arity mismatch" }

                # local scope
                $saved = $global:Env.Clone()
                for ($i = 0; $i -lt $f.params.Count; $i++) { $global:Env[$f.params[$i]] = $vals[$i] }

                $ret = $null
                foreach ($s in $f.body) { 
                    $tmp = EvalStatement $s
                    if ($tmp -and $tmp.PSObject.Properties.Name -contains '_rt') {
                        $ret = $tmp._rt
                        break
                    }
                }
                $global:Env = $saved
                return $ret
            }
            else { throw "Unknown function $fname" }
        }
        "ArrayLiteral" {
            $result = @()
            foreach ($element in $node.elements) {
                $result += EvalExpression $element
            }
            return $result
        }
        "ObjectLiteral" {
            $result = @{}
            foreach ($key in $node.properties.Keys) {
                $result[$key] = EvalExpression $node.properties[$key]
            }
            return $result
        }
        default {
            throw ("Runtime Error [line {0}, col {1}]: Unknown expression type: {2}" -f ($node.line -or "-"), ($node.column -or "-"), $node.type)
        }
    }
}

function _Return($value) { [PSCustomObject]@{ _rt = $value } }

function EvalStatement($node) {
    if (-not $node -or -not $node.type) { return $null }
    switch ($node.type) {
        "Assignment" {

            $varNode = $node.variable

            if ($varNode.type -eq "ArrayAccess") {
                $baseArr = EvalExpression $varNode.base    # evaluate the array expression
                $index = EvalExpression $varNode.index   # evaluate the index
                $value = EvalExpression $node.value      # evaluate the RHS
                $baseArr[$index] = $value                   # mutate the array copy
                return $null
            }

            # existing variable-only check
            if ($varNode.type -ne "Variable") {
                throw ("Runtime Error [line {0}, col {1}]: Invalid left-hand side …" `
                        -f $node.line, $node.column)
            }

            # normal scalar assignment
            $vName = $varNode.name
            $value = EvalExpression $node.value
            $global:Env[$vName] = $value
            return $null
        }
        "ExprStmt" {
            EvalExpression $node.expression | Out-Null
            return $null
        }
        "If" {
            if (EvalExpression $node.condition) { EvalBlock $node.then }
            else { EvalBlock $node.else }
            return $null
        }
        "While" {
            while (EvalExpression $node.condition) {
                EvalBlock $node.body
            }
            return $null
        }
        "For" {
            EvalStatement $node.init | Out-Null
            while (EvalExpression $node.condition) {
                EvalBlock $node.body
                EvalExpression $node.update | Out-Null
            }
            return $null
        }
        "Return" {
            $val = if ($node.value) { EvalExpression $node.value } else { $null }
            return _Return $val
        }
        default {
            throw ("Runtime Error [line {0}, col {1}]: Unknown statement type: {2}" -f $node.line, $node.column, $node.type)
        }
    }
}

function EvalBlock($stmts) {
    foreach ($s in $stmts) {
        if ($null -eq $s) { continue }
        EvalStatement $s
    }
}

try {
    EvalBlock $programAST.body
}
catch {
    Write-Error $_
}