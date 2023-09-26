%namespace Compiler.Imperative
%scannertype ImperativeScanner
%visibility internal
%tokentype Token

%option minimize, verbose

// Words
Bool            boolean
Int             integer
Real            real
Type            type
Is              is
End             end
Return          return
Var             var
Routine         routine
For             for
While           while
Loop            loop
In              in
Reverse         reverse
If              if
Then            then
Else            else
Array           array
Record          record
False           false
True            true
And             and
Or              or
Xor             xor

// Symbols
RoundOpen       \(
RoundClose      \)
CurlyOpen       \{
CurlyClose      \}
SquareOpen      \[
SquareClose     \]
Semicolon       ;
Colon           :
Comma           ,
Assign          :=
Dot             \.
Minus           -
Plus            \+
Multiply        \*
Divide          /
Percent         %
Range           \.\.
Leq             <=
Geq             >=
Less            <
Greater         >
Equal           =
NotEqual        /=

// Complex
IntLiteral     [0-9]+
RealLiteral    [0-9]+"."[0-9]+
Identifier     [a-zA-Z_][a-zA-Z0-9_]*



%{

%}

%%

/* Scanner body */

{Is}            { return GetTokenData(Token.IS); }
{Bool}          { return GetTokenData(Token.BOOL); }
{Int}           { return GetTokenData(Token.INT); }
{Real}          { return GetTokenData(Token.REAL); }
{Type}          { return GetTokenData(Token.TYPE); }
{End}           { return GetTokenData(Token.END); }
{Return}        { return GetTokenData(Token.RETURN); }
{Var}           { return GetTokenData(Token.VAR); }
{Routine}       { return GetTokenData(Token.ROUTINE); }
{For}           { return GetTokenData(Token.FOR); }
{While}         { return GetTokenData(Token.WHILE); }
{Loop}          { return GetTokenData(Token.LOOP); }
{In}            { return GetTokenData(Token.IN); }
{Reverse}       { return GetTokenData(Token.REVERSE); }
{If}            { return GetTokenData(Token.IF); }
{Then}          { return GetTokenData(Token.THEN); }
{Else}          { return GetTokenData(Token.ELSE); }
{Array}         { return GetTokenData(Token.ARRAY); }
{Record}        { return GetTokenData(Token.RECORD); }
{False}         { return GetTokenData(Token.FALSE); }
{True}          { return GetTokenData(Token.TRUE); }

{RoundOpen}     { return GetTokenData(Token.ROUND_OPEN); }
{RoundClose}    { return GetTokenData(Token.ROUND_CLOSE); }
{CurlyOpen}     { return GetTokenData(Token.CURLY_OPEN); }
{CurlyClose}    { return GetTokenData(Token.CURLY_CLOSE); }
{SquareOpen}    { return GetTokenData(Token.SQUARE_OPEN); }
{SquareClose}   { return GetTokenData(Token.SQUARE_CLOSE); }
{Semicolon}     { return GetTokenData(Token.SEMICOLON); }
{Colon}         { return GetTokenData(Token.COLON); }
{Comma}         { return GetTokenData(Token.COMMA); }
{Assign}        { return GetTokenData(Token.ASSIGN); }
{Dot}           { return GetTokenData(Token.DOT); }
{Minus}         { return GetTokenData(Token.MINUS); }
{Plus}          { return GetTokenData(Token.PLUS); }
{Multiply}      { return GetTokenData(Token.MULTIPLY); }
{Divide}        { return GetTokenData(Token.DIVIDE); }
{Percent}       { return GetTokenData(Token.PERCENT); }
{And}           { return GetTokenData(Token.AND); }
{Or}            { return GetTokenData(Token.OR); }
{Xor}           { return GetTokenData(Token.XOR); }
{Range}         { return GetTokenData(Token.RANGE); }
{Leq}           { return GetTokenData(Token.LEQ); }
{Geq}           { return GetTokenData(Token.GEQ); }
{Less}          { return GetTokenData(Token.LESS); }
{Greater}       { return GetTokenData(Token.GREATER); }
{Equal}         { return GetTokenData(Token.EQUAL); }
{NotEqual}      { return GetTokenData(Token.NOT_EQUAL); }

{IntLiteral}    { return GetTokenData(Token.INT_LITERAL); }
{RealLiteral}   { return GetTokenData(Token.REAL_LITERAL); }
{Identifier}    { return GetTokenData(Token.IDENTIFIER); }



%%