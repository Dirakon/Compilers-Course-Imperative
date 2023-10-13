namespace Compiler;

public interface INode
{
    public CustomLexLocation LexLocation { get; }
}

public interface INodeList<T> : INode where T : INode
{
}

public record NonEmptyNodeList<T>(T ThisNode, INodeList<T>? OtherNode, CustomLexLocation LexLocation) : INodeList<T>
    where T : INode;

public record EmptyNodeList<T>(CustomLexLocation LexLocation) : INodeList<T> where T : INode;

public record Program(INodeList<IDeclaration> Declarations, CustomLexLocation LexLocation) : INode;

public interface IDeclaration : INode
{
}

public record RoutineDeclaration(string RoutineName, INodeList<Parameter> Parameters, IType? ReturnType,
    INodeList<IBodyElement> Body,
    CustomLexLocation LexLocation) : IDeclaration;

public record Parameter(string Name, IType Type, CustomLexLocation LexLocation) : INode;

public interface IBodyElement : INode
{
}

public interface ISimpleDeclaration : IDeclaration, IBodyElement
{
}

public record VariableDeclaration(string Name, IType? Type, Expression? Expresion, CustomLexLocation LexLocation)
    : ISimpleDeclaration;

public record TypeDeclaration(string Name, IType Type, CustomLexLocation LexLocation) : ISimpleDeclaration;

public interface IType : INode
{
}

public record IntType(CustomLexLocation LexLocation) : IType;

public record RealType(CustomLexLocation LexLocation) : IType;

public record BoolType(CustomLexLocation LexLocation) : IType;

/// <summary>
///     type myType = int;
///     var myVar:myType = ...
/// </summary>
public record UserDefinedType(string TypeName, CustomLexLocation LexLocation) : IType;

public record ArrayType(Expression? SizeExpression, IType UnderlyingType, CustomLexLocation LexLocation) : IType;

public record RecordType(INodeList<VariableDeclaration> Variables, CustomLexLocation LexLocation) : IType;

public interface IStatement : IBodyElement
{
}

public record Assignment(ModifiablePrimary Target, Expression Expression, CustomLexLocation LexLocation) : IStatement;

public record RoutineCall
    (string RoutineName, INodeList<Expression> Arguments, CustomLexLocation LexLocation) : IStatement, IPrimary;

public record WhileLoop
    (Expression Condition, INodeList<IBodyElement> Body, CustomLexLocation LexLocation) : IStatement;

public record ForLoop(string IteratorName, Range Range, CustomLexLocation LexLocation) : IStatement;

public record Range(bool IsReversed, Expression Start, Expression End, CustomLexLocation LexLocation) : IStatement;

public record IfStatement(Expression Condition, INodeList<IBodyElement> ThenBody, INodeList<IBodyElement>? ElseBody,
    CustomLexLocation LexLocation) : IStatement;


public record Expression(Relation First, (RelationOperation,  Relation)? Second, CustomLexLocation LexLocation) : INode;

public enum RelationOperation
{
    And,
    Or,
    Xor
}

public enum SimpleOperation
{
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual
}

public record Relation(Simple First, (SimpleOperation,  Simple)? Second, CustomLexLocation LexLocation) : INode;

public record Simple(Factor First, (FactorOperation,  Factor)? Second, CustomLexLocation LexLocation) : INode;

public enum FactorOperation
{
    Multiplication,
    Division,
    ModularDivision
}

public record Factor(ISummand First, (SummandOperation, ISummand)? Second, CustomLexLocation LexLocation) : INode;

public enum SummandOperation
{
    Plus,
    Minus
}

public interface ISummand : INode
{
    
}
public record ExpressionSummand(Expression Expression, CustomLexLocation LexLocation) : ISummand;

public interface IPrimary : ISummand
{
    
}

public record ModifiablePrimary(string Identifier, INodeList<IModifiablePrimaryOperation> Operations, CustomLexLocation LexLocation) : IPrimary;

public interface IModifiablePrimaryOperation : INode
{
}

public record MemberCall(string MemberName, CustomLexLocation LexLocation) : IModifiablePrimaryOperation;
public record ArrayCall(Expression IndexExpression, CustomLexLocation LexLocation) : IModifiablePrimaryOperation;

/// <summary>
/// not 1
/// </summary>
public record InvertedIntegerPrimary(int Value, CustomLexLocation LexLocation) : IPrimary;

public record IntegerPrimary(UnarySign? Sign, int Literal, CustomLexLocation LexLocation) : IPrimary;

public record RealPrimary(UnarySign? Sign, double Literal, CustomLexLocation LexLocation) : IPrimary;

public record BoolLiteral(bool Value, CustomLexLocation LexLocation) : IPrimary;

public enum UnarySign
{
    Plus,
    Minus
}
