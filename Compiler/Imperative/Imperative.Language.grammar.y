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
: /* empty */ { $$ = new EmptyNodeList<IDeclaration>(); @$ = $$.LexLocation; }
| Declaration DeclarationList {$$ = new NonEmptyNodeList<IDeclaration>(
        (IDeclaration)$1,
        (INodeList<IDeclaration>)$2, 
        @1.Merge(@2)); @$ = $$.LexLocation;}
;

Declaration
: ROUTINE IDENTIFIER ROUND_OPEN ParametersList ROUND_CLOSE COLON Type IS Body END { $$ = new RoutineDeclaration(
      @2.UnderlyingString,
      (INodeList<Parameter>)$4,
      (IType) $7,
      (INodeList<IBodyElement>)$9,
      @1.Merge(@10)
    );  @$ = $$.LexLocation;}
| SimpleDeclaration {$$ = $1; @$ = @1;}
;

SimpleDeclaration
: VariableDeclaration {$$ = $1; @$ = @1;}
| TypeDeclaration {$$ = $1; @$ = @1;}
;

Body
: IS // TODO 
;

Statement
: IS // TODO 
;

TypeDeclaration
: IS //  TODO 
;

ParametersList
: /* empty */ { $$ = new EmptyNodeList<Parameter>(); }
| Parameter COMMA ParametersList {$$ = new NonEmptyNodeList<Parameter>(
        (Parameter)$1,
        (INodeList<Parameter>)$3, 
        @1.Merge(@3)); @$ = $$.LexLocation;}
| Parameter {$$ = new NonEmptyNodeList<Parameter>(
        (Parameter)$1,
        new EmptyNodeList<Parameter>(), 
        @1); @$ = $$.LexLocation;}
;

Parameter
: IDENTIFIER COLON Type {$$ = new Parameter(
    @1.UnderlyingString,
    (IType)$3,
    @1.Merge(@3)
); @$ = $$.LexLocation;}
;


Type
: INT {$$ = new IntType(@1); @$ = $$.LexLocation;}
| REAL {$$ = new RealType(@1); @$ = $$.LexLocation;}
| BOOL {$$ = new BoolType(@1); @$ = $$.LexLocation;}
| IDENTIFIER {$$ = new UserDefinedType(@1.UnderlyingString, @1); @$ = $$.LexLocation;}
| ARRAY SQUARE_OPEN Expression SQUARE_CLOSE Type {$$ = new ArrayType((Expression)$3, (IType) $5, @1.Merge(@5)); @$ = $$.LexLocation;}
| ARRAY Type {$$ = new ArrayType(null, (IType) $2, @1.Merge(@2)); @$ = $$.LexLocation;}
| RECORD CURLY_OPEN VariableDeclarationList CURLY_CLOSE END {$$ = new RecordType(
    (INodeList<VariableDeclaration>)$3, 
    @1.Merge(@5)); @$ = $$.LexLocation;}
;

VariableDeclarationList
: /* empty */ { $$ = new EmptyNodeList<VariableDeclaration>(); }
| VariableDeclaration SEMICOLON VariableDeclarationList {$$ = new NonEmptyNodeList<VariableDeclaration>(
        (VariableDeclaration)$1,
        (INodeList<VariableDeclaration>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
;

VariableDeclaration
: VAR IDENTIFIER COLON Type IS Expression { $$ = new VariableDeclaration(
        @2.UnderlyingString,
        (IType)$4, 
        (Expression)$6, 
        @1.Merge(@6)); @$ = $$.LexLocation;}
| VAR IDENTIFIER IS Expression { $$ = new VariableDeclaration(
        @2.UnderlyingString,
        null, 
        (Expression)$4, 
        @1.Merge(@4)); @$ = $$.LexLocation;}
| VAR IDENTIFIER COLON Type { $$ = new VariableDeclaration(
        @2.UnderlyingString,
        (IType)$4, 
        null, 
        @1.Merge(@4)); @$ = $$.LexLocation;}
;

Expression
: Relation {$$ = new Expression((Relation)$1, null, @1); @$ = $$.LexLocation;}
| Relation AND Relation {$$ = new Expression((Relation)$1, (RelationOperation.And, (Relation)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Relation OR Relation {$$ = new Expression((Relation)$1, (RelationOperation.Or, (Relation)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Relation XOR Relation {$$ = new Expression((Relation)$1, (RelationOperation.Xor, (Relation)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
;

Relation
: Simple {$$ = new Relation((Simple)$1, null, @1); @$ = $$.LexLocation;}
| Simple LESS Simple {$$ = new Relation((Simple)$1, (SimpleOperation.Less, (Simple)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Simple LEQ Simple {$$ = new Relation((Simple)$1, (SimpleOperation.LessOrEqual, (Simple)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Simple GREATER Simple {$$ = new Relation((Simple)$1, (SimpleOperation.Greater, (Simple)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Simple GEQ Simple {$$ = new Relation((Simple)$1, (SimpleOperation.GreaterOrEqual, (Simple)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Simple EQUAL Simple {$$ = new Relation((Simple)$1, (SimpleOperation.Equal, (Simple)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Simple NOT_EQUAL Simple {$$ = new Relation((Simple)$1, (SimpleOperation.NotEqual, (Simple)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
;

Simple
: Summand {$$ = new Simple((Summand)$1, null, @1); @$ = $$.LexLocation;}
| Summand PLUS Summand {$$ = new Simple((Summand)$1, (SummandOperation.Plus, (Summand)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Summand MINUS Summand {$$ = new Simple((Summand)$1, (SummandOperation.Minus, (Summand)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
;

Summand
: Factor {$$ = new Summand((IFactor)$1, null, @1); @$ = $$.LexLocation;}
| Factor MULTIPLY Factor {$$ = new Summand((IFactor)$1, (FactorOperation.Multiplication, (IFactor)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Factor DIVIDE Factor {$$ = new Summand((IFactor)$1, (FactorOperation.Division, (IFactor)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
| Factor PERCENT Factor {$$ = new Summand((IFactor)$1, (FactorOperation.ModularDivision, (IFactor)$3), @1.Merge(@3)); @$ = $$.LexLocation;}
;

Factor
: Primary {$$ = $1; @$ = @1;}
| ROUND_OPEN Expression ROUND_CLOSE { $$ = $2; @$ = @1.Merge(@3);}
;

Primary
: TRUE {$$ = new BoolPrimary(true, @1); @$ = $$.LexLocation;}
| FALSE {$$ = new BoolPrimary(false, @1); @$ = $$.LexLocation;}
| INT_LITERAL {$$ = new IntegerPrimary(int.Parse(@1.UnderlyingString), @1); @$ = $$.LexLocation;}
| PLUS INT_LITERAL {$$ = new IntegerPrimary(int.Parse(@1.UnderlyingString), @1); @$ = $$.LexLocation;}
| MINUS INT_LITERAL {$$ = new IntegerPrimary(-int.Parse(@1.UnderlyingString), @1); @$ = $$.LexLocation;}
| REAL_LITERAL {$$ = new RealPrimary(double.Parse(@1.UnderlyingString), @1); @$ = $$.LexLocation;}
| PLUS REAL_LITERAL {$$ = new RealPrimary(double.Parse(@2.UnderlyingString), @1.Merge(@2)); @$ = $$.LexLocation;}
| MINUS REAL_LITERAL {$$ = new RealPrimary(-double.Parse(@2.UnderlyingString), @1.Merge(@2)); @$ = $$.LexLocation;}
| ModifiablePrimary {$$ = $1; @$ = @1;}
;

ModifiablePrimary
: IDENTIFIER ModifiablePrimaryOperationList { $$ = new ModifiablePrimary(
    @1.UnderlyingString, (INodeList<IModifiablePrimaryOperation>) $2, @1.Merge(@2));}
;

ModifiablePrimaryOperationList
: /* empty */ { $$ = new EmptyNodeList<IModifiablePrimaryOperation>(); }
| DOT IDENTIFIER ModifiablePrimaryOperationList {$$ = new NonEmptyNodeList<IModifiablePrimaryOperation>(
        new MemberCall(@2.UnderlyingString, @1.Merge(@2)),
        (INodeList<IModifiablePrimaryOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
| SQUARE_OPEN Expression SQUARE_CLOSE ModifiablePrimaryOperationList {$$ = new NonEmptyNodeList<IModifiablePrimaryOperation>(
        new ArrayCall((Expression)$2, @1.Merge(@3)),
        (INodeList<IModifiablePrimaryOperation>)$4, 
        @1.Merge(@3).Merge(@4)); @$ = $$.LexLocation;}
;


%%