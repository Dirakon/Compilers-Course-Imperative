%namespace Compiler.Imperative
%partial
%YYLTYPE CustomLexLocation
%parsertype ImperativeParser
%visibility internal
%tokentype Token

%YYSTYPE INode

%start Program

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

Program
: /* empty */ { var node = new Program(new EmptyNodeList<IDeclaration>()); $$ = node; RootNode = node; }
| DeclarationList { var node = new Program((INodeList<IDeclaration>) $1); $$ = node; RootNode = node; }
;

DeclarationList
: /* empty */ { $$ = new EmptyNodeList<IDeclaration>(); }
| Declaration DeclarationList {$$ = new NonEmptyNodeList<IDeclaration>(
        (IDeclaration)$1, 
        (INodeList<IDeclaration>)$2, 
        ((IDeclaration)$1).LexLocation.Merge(((INodeList<IDeclaration>)$2).LexLocation));}
;

Declaration
: ROUTINE IDENTIFIER ROUND_OPEN ParametersList ROUND_CLOSE{ $$ = new EmptyNodeList<IDeclaration>(); }
| Declaration DeclarationList {$$ = new NonEmptyNodeList<IDeclaration>(
        (IDeclaration)$1, 
        (INodeList<IDeclaration>)$2, 
        ((IDeclaration)$1).LexLocation.Merge(((INodeList<IDeclaration>)$2).LexLocation));}
;

Declaration
: IS
;

%%