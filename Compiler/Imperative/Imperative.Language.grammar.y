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
| Declaration SEMICOLON DeclarationList {$$ = new NonEmptyNodeList<IDeclaration>(
        (IDeclaration)$1,
        (INodeList<IDeclaration>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
;

Declaration
: ROUTINE IDENTIFIER ROUND_OPEN ParametersList ROUND_CLOSE COLON Type IS Body END { $$ = new RoutineDeclaration(
      @2.UnderlyingString,
      (INodeList<Parameter>)$4,
      (IType) $7,
      (INodeList<IBodyElement>)$9,
      @1.Merge(@10)
    );  @$ = $$.LexLocation;}
| ROUTINE IDENTIFIER ROUND_OPEN ParametersList ROUND_CLOSE IS Body END { $$ = new RoutineDeclaration(
      @2.UnderlyingString,
      (INodeList<Parameter>)$4,
      null,
      (INodeList<IBodyElement>)$7,
      @1.Merge(@8)
    );  @$ = $$.LexLocation;}
| SimpleDeclaration {$$ = $1; @$ = @1;}
;

SimpleDeclaration
: VariableDeclaration {$$ = $1; @$ = @1;}
| TypeDeclaration {$$ = $1; @$ = @1;}
;

Body
: /* empty */ { $$ = new EmptyNodeList<IBodyElement>(); @$ = $$.LexLocation; }
| SimpleDeclaration SEMICOLON Body {$$ = new NonEmptyNodeList<IBodyElement>(
        (ISimpleDeclaration)$1,
        (INodeList<IBodyElement>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
| Statement SEMICOLON Body {$$ = new NonEmptyNodeList<IBodyElement>(
        (IStatement)$1,
        (INodeList<IBodyElement>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
;

Statement
: RETURN {$$ = new Return(
                  null,
                  @1); @$ = $$.LexLocation;}
| RETURN Expression {$$ = new Return(
                  (Expression)$2,
                  @1.Merge(@2)); @$ = $$.LexLocation;}
| ModifiablePrimary ASSIGN Expression {$$ = new Assignment(
                  (ModifiablePrimary)$1,
                  (Expression)$3,
                  @1.Merge(@3)); @$ = $$.LexLocation;}
| RoutineCall {$$ = $1; @$ = @1;}
| WHILE Expression LOOP Body END {$$ = new WhileLoop(
                  (Expression)$2,
                  (INodeList<IBodyElement>)$4,
                  @1.Merge(@5)); @$ = $$.LexLocation;}
| FOR IDENTIFIER Range LOOP Body END {$$ = new ForLoop(
                  @2.UnderlyingString,
                  (Range)$3,
                  (INodeList<IBodyElement>)$5,
                  @1.Merge(@6)); @$ = $$.LexLocation;}
| IF Expression THEN Body ELSE Body END {$$ = new IfStatement(
                  (Expression)$2,
                  (INodeList<IBodyElement>)$4,
                  (INodeList<IBodyElement>)$6,
                  @1.Merge(@7)); @$ = $$.LexLocation;}
| IF Expression THEN Body END {$$ = new IfStatement(
                  (Expression)$2,
                  (INodeList<IBodyElement>)$4,
                  null,
                  @1.Merge(@5)); @$ = $$.LexLocation;}
;

Range
: IN REVERSE Expression RANGE Expression {$$ = new Range(
                  true,
                  (Expression)$3,
                  (Expression)$5,
                  @1.Merge(@5)); @$ = $$.LexLocation;}
| IN Expression RANGE Expression {$$ = new Range(
                  false,
                  (Expression)$2,
                  (Expression)$4,
                  @1.Merge(@4)); @$ = $$.LexLocation;}
;

TypeDeclaration
: TYPE IDENTIFIER IS Type { $$ = new TypeDeclaration(
                  @2.UnderlyingString,
                  (IType)$4,
                  @1.Merge(@4)); @$ = $$.LexLocation;}
;

ParametersList
: /* empty */ { $$ = new EmptyNodeList<Parameter>(); @$ = $$.LexLocation;}
| Parameter COMMA ParametersList {$$ = new NonEmptyNodeList<Parameter>(
        (Parameter)$1,
        (INodeList<Parameter>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
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
| ARRAY SQUARE_OPEN SQUARE_CLOSE Type {$$ = new ArrayType(null, (IType) $4, @1.Merge(@4)); @$ = $$.LexLocation;}
| RECORD CURLY_OPEN VariableDeclarationList CURLY_CLOSE END {$$ = new RecordType(
    (INodeList<VariableDeclaration>)$3, 
    @1.Merge(@5)); @$ = $$.LexLocation;}
;

VariableDeclarationList
: /* empty */ { $$ = new EmptyNodeList<VariableDeclaration>(); @$ = $$.LexLocation;}
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
: Relation RelationOperationsList {$$ = new Expression((Relation)$1, (INodeList<RelationOperation>)$2, @1.Merge(@2)); 
    @$ = $$.LexLocation;}
;

RelationOperationsList
: /* empty */ { $$ = new EmptyNodeList<RelationOperation>(); @$ = $$.LexLocation;}
| AND Relation RelationOperationsList {$$ = new NonEmptyNodeList<RelationOperation>(
        new RelationOperation(
            RelationOperationType.And,
            (Relation)$2,
            @1.Merge(@2)
        ),
        (INodeList<RelationOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
| OR Relation RelationOperationsList {$$ = new NonEmptyNodeList<RelationOperation>(
        new RelationOperation(
            RelationOperationType.Or,
            (Relation)$2,
            @1.Merge(@2)
        ),
        (INodeList<RelationOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
| XOR Relation RelationOperationsList {$$ = new NonEmptyNodeList<RelationOperation>(
        new RelationOperation(
            RelationOperationType.Xor,
            (Relation)$2,
            @1.Merge(@2)
        ),
        (INodeList<RelationOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
;

Relation
: Simple SimpleOperationList {$$ = new Relation((Simple)$1, (INodeList<SimpleOperation>)$2, @1.Merge(@2)); 
    @$ = $$.LexLocation; }
;

SimpleOperationList
: /* empty */ { $$ = new EmptyNodeList<SimpleOperation>(); @$ = $$.LexLocation; }
| LESS Simple SimpleOperationList {$$ = new NonEmptyNodeList<SimpleOperation>(
        new SimpleOperation(
            SimpleOperationType.Less,
            (Simple)$2,
            @1.Merge(@2)
        ),
        (INodeList<SimpleOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| LEQ Simple SimpleOperationList {$$ = new NonEmptyNodeList<SimpleOperation>(
        new SimpleOperation(
            SimpleOperationType.LessOrEqual,
            (Simple)$2,
            @1.Merge(@2)
        ),
        (INodeList<SimpleOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| GREATER Simple SimpleOperationList {$$ = new NonEmptyNodeList<SimpleOperation>(
        new SimpleOperation(
            SimpleOperationType.Greater,
            (Simple)$2,
            @1.Merge(@2)
        ),
        (INodeList<SimpleOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| GEQ Simple SimpleOperationList {$$ = new NonEmptyNodeList<SimpleOperation>(
        new SimpleOperation(
            SimpleOperationType.GreaterOrEqual,
            (Simple)$2,
            @1.Merge(@2)
        ),
        (INodeList<SimpleOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| EQUAL Simple SimpleOperationList {$$ = new NonEmptyNodeList<SimpleOperation>(
        new SimpleOperation(
            SimpleOperationType.Equal,
            (Simple)$2,
            @1.Merge(@2)
        ),
        (INodeList<SimpleOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| NOT_EQUAL Simple SimpleOperationList {$$ = new NonEmptyNodeList<SimpleOperation>(
        new SimpleOperation(
            SimpleOperationType.NotEqual,
            (Simple)$2,
            @1.Merge(@2)
        ),
        (INodeList<SimpleOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
;

Simple
: Summand SummandOperationsList {$$ = new Simple((Summand)$1, (INodeList<SummandOperation>)$2, @1.Merge(@2)); 
    @$ = $$.LexLocation;}
;

SummandOperationsList
: /* empty */ { $$ = new EmptyNodeList<SummandOperation>(); @$ = $$.LexLocation; }
| PLUS Summand SummandOperationsList {$$ = new NonEmptyNodeList<SummandOperation>(
        new SummandOperation(
            SummandOperationType.Plus,
            (Summand)$2,
            @1.Merge(@2)
        ),
        (INodeList<SummandOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| MINUS Summand SummandOperationsList {$$ = new NonEmptyNodeList<SummandOperation>(
        new SummandOperation(
            SummandOperationType.Minus,
            (Summand)$2,
            @1.Merge(@2)
        ),
        (INodeList<SummandOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
;

Summand
: Factor FactorOperationsList {$$ = new Summand((IFactor)$1, (INodeList<FactorOperation>)$2, @1.Merge(@2)); 
    @$ = $$.LexLocation;}
;

FactorOperationsList
: /* empty */ { $$ = new EmptyNodeList<FactorOperation>(); @$ = $$.LexLocation; }
| MULTIPLY Factor FactorOperationsList {$$ = new NonEmptyNodeList<FactorOperation>(
        new FactorOperation(
            FactorOperationType.Multiplication,
            (IFactor)$2,
            @1.Merge(@2)
        ),
        (INodeList<FactorOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| DIVIDE Factor FactorOperationsList {$$ = new NonEmptyNodeList<FactorOperation>(
        new FactorOperation(
            FactorOperationType.Division,
            (IFactor)$2,
            @1.Merge(@2)
        ),
        (INodeList<FactorOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
| PERCENT Factor FactorOperationsList {$$ = new NonEmptyNodeList<FactorOperation>(
        new FactorOperation(
            FactorOperationType.ModularDivision,
            (IFactor)$2,
            @1.Merge(@2)
        ),
        (INodeList<FactorOperation>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation; }
;


Factor
: Primary {$$ = $1; @$ = @1;}
| ROUND_OPEN Expression ROUND_CLOSE { $$ = new ExpressionFactor((Expression)$2, @1.Merge(@3)); @$ = @1.Merge(@3);}
;

Primary
: TRUE {$$ = new BoolPrimary(true, @1); @$ = $$.LexLocation;}
| FALSE {$$ = new BoolPrimary(false, @1); @$ = $$.LexLocation;}
| INT_LITERAL {$$ = new IntegerPrimary(int.Parse(@1.UnderlyingString), @1); @$ = $$.LexLocation;}
| PLUS INT_LITERAL {$$ = new IntegerPrimary(int.Parse(@2.UnderlyingString), @1.Merge(@2)); @$ = $$.LexLocation;}
| MINUS INT_LITERAL {$$ = new IntegerPrimary(-int.Parse(@2.UnderlyingString), @1.Merge(@2)); @$ = $$.LexLocation;}
| REAL_LITERAL {$$ = new RealPrimary(double.Parse(@1.UnderlyingString), @1); @$ = $$.LexLocation;}
| PLUS REAL_LITERAL {$$ = new RealPrimary(double.Parse(@2.UnderlyingString), @1.Merge(@2)); @$ = $$.LexLocation;}
| MINUS REAL_LITERAL {$$ = new RealPrimary(-double.Parse(@2.UnderlyingString), @1.Merge(@2)); @$ = $$.LexLocation;}
| RoutineCall {$$ = $1; @$ = @1;}
| ModifiablePrimary {$$ = $1; @$ = @1;}
;

RoutineCall
:  IDENTIFIER ROUND_OPEN ArgumentsList ROUND_CLOSE {$$ = new RoutineCall(
    @1.UnderlyingString, 
    (INodeList<Expression>)$3, 
    @1.Merge(@4)); @$ = $$.LexLocation;}
;

ArgumentsList
: /* empty */ { $$ = new EmptyNodeList<Expression>(); @$ = $$.LexLocation;}
| Expression COMMA ArgumentsList {$$ = new NonEmptyNodeList<Expression>(
        (Expression)$1,
        (INodeList<Expression>)$3, 
        @1.Merge(@2).Merge(@3)); @$ = $$.LexLocation;}
| Expression {$$ = new NonEmptyNodeList<Expression>(
        (Expression)$1,
        new EmptyNodeList<Expression>(), 
        @1); @$ = $$.LexLocation;}
;

ModifiablePrimary
: IDENTIFIER ModifiablePrimaryOperationList { $$ = new ModifiablePrimary(
    @1.UnderlyingString, (INodeList<IModifiablePrimaryOperation>) $2, @1.Merge(@2)); @$ = $$.LexLocation;}
;

ModifiablePrimaryOperationList
: /* empty */ { $$ = new EmptyNodeList<IModifiablePrimaryOperation>(); @$ = $$.LexLocation; }
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