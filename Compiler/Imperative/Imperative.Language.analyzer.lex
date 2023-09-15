%namespace Compiler.Imperative
%scannertype ImperativeScanner
%visibility internal
%tokentype Token

%option stack, minimize, parser, verbose, persistbuffer, noembedbuffers

// Words
Bool            bool
Int             int
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
And             and
Or              or
Xor             xor
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

{Bool}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.BOOL; }
{Int}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.INT; }
{Real}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.REAL; }
{Type}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.TYPE; }
{Is}            { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.IS; }
{End}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.END; }
{Return}        { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.RETURN; }
{Var}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.VAR; }
{Routine}       { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.ROUTINE; }
{For}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.FOR; }
{While}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.WHILE; }
{Loop}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.LOOP; }
{In}            { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.IN; }
{Reverse}       { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.REVERSE; }
{If}            { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.IF; }
{Then}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.THEN; }
{Else}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.ELSE; }
{Array}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.ARRAY; }
{Record}        { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.RECORD; }
{False}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.FALSE; }
{True}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.TRUE; }

{RoundOpen}     { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.ROUND_OPEN; }
{RoundClose}    { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.ROUND_CLOSE; }
{CurlyOpen}     { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.CURLY_OPEN; }
{CurlyClose}    { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.CURLY_CLOSE; }
{SquareOpen}    { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.SQUARE_OPEN; }
{SquareClose}   { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.SQUARE_CLOSE; }
{Semicolon}     { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.SEMICOLON; }
{Colon}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.COLON; }
{Comma}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.COMMA; }
{Assign}        { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.ASSIGN; }
{Dot}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.DOT; }
{Minus}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.MINUS; }
{Plus}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.PLUS; }
{Multiply}      { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.MULTIPLY; }
{Divide}        { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.DIVIDE; }
{Percent}       { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.PERCENT; }
{And}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.AND; }
{Or}            { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.OR; }
{Xor}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.XOR; }
{Range}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.RANGE; }
{Leq}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.LEQ; }
{Geq}           { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.GEQ; }
{Less}          { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.LESS; }
{Greater}       { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.GREATER; }
{Equal}         { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.EQUAL; }
{NotEqual}      { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.NOT_EQUAL; }

{IntLiteral}    { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.INT_LITERAL; }
{RealLiteral}   { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.REAL_LITERAL; }
{Identifier}    { Console.WriteLine("token: {0} {1}", yytext, yylloc.StartColumn); GetTokenData(); return (int)Token.IDENTIFIER; }


%%