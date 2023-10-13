%namespace Compiler.Imperative
%partial
%YYLTYPE CustomLexLocation
%parsertype ImperativeParser
%visibility internal
%tokentype Token

%YYSTYPE INode

%start main

%token IDENTIFIER  

%token BOOL
%token INT
%token REAL

%token TYPE
%token IS
%token END
%token RETURN
%token VAR
%token ROUTINE
%token FOR
%token WHILE
%token LOOP
%token IN
%token REVERSE
%token IF
%token THEN
%token ELSE
%token ARRAY
%token RECORD

%token ROUND_OPEN
%token ROUND_CLOSE
%token CURLY_OPEN
%token CURLY_CLOSE
%token SQUARE_OPEN
%token SQUARE_CLOSE
%token SEMICOLON
%token COLON
%token COMMA

%token ASSIGN // :=
%token DOT
%token MINUS
%token PLUS
%token MULTIPLY
%token DIVIDE
%token PERCENT
%token AND
%token OR
%token XOR
%token RANGE   // ..

%token LEQ
%token GEQ
%token LESS
%token GREATER
%token EQUAL
%token NOT_EQUAL 

%token INT_LITERAL
%token REAL_LITERAL
%token FALSE
%token TRUE

%%

main   :  ROUTINE IDENTIFIER ROUND_OPEN ROUND_CLOSE COLON  //{$$ = new Routine(@1.underlyingString);}//routine IDENTIFIER arguments COLON REAL IS RETURN identifier PLUS identifier SEMICOLON END            {}
      ;

routine : ROUTINE                          // {$$ = new Routine(@1);}
       ;

%%