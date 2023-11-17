using System.Collections;

namespace Compiler;

public interface INode
{
    public CustomLexLocation LexLocation { get; }
}

public interface INodeList<T> : IEnumerable<T>, INode where T : INode
{
}

public record NonEmptyNodeList<T>(T ThisNode, INodeList<T> OtherNodes, CustomLexLocation LexLocation) : INodeList<T>
    where T : INode
{
    public IEnumerator<T> GetEnumerator()
    {
        yield return ThisNode;
        foreach (var otherNode in OtherNodes)
        {
            yield return otherNode;
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}

public record EmptyNodeList<T> : INodeList<T> where T : INode
{
    public CustomLexLocation LexLocation { get; } = CustomLexLocation.Empty;

    public IEnumerator<T> GetEnumerator()
    {
        yield break;
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}

public record Program(INodeList<IDeclaration> Declarations) : INode
{
    public CustomLexLocation LexLocation { get; } = Declarations.LexLocation;
}

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

public record VariableDeclaration(string Name, IType? Type, Expression? Expression, CustomLexLocation LexLocation)
    : ISimpleDeclaration;

public record TypeDeclaration(string Name, IType Type, CustomLexLocation LexLocation) : ISimpleDeclaration;

public interface IType : INode
{
    public string GetTypeName();
}

public record IntType(CustomLexLocation LexLocation) : IType
{
    public string GetTypeName()
    {
        return "integer";
    }
}

public record RealType(CustomLexLocation LexLocation) : IType
{
    public string GetTypeName()
    {
        return "real";
    }
}

public record BoolType(CustomLexLocation LexLocation) : IType
{
    public string GetTypeName()
    {
        return "bool";
    }
}

/// <summary>
///     type myType = int;
///     var myVar:myType = ...
/// </summary>
public record UserDefinedType(string TypeName, CustomLexLocation LexLocation) : IType
{
    public string GetTypeName()
    {
        return TypeName;
    }
}

public record ArrayType(Expression? SizeExpression, IType UnderlyingType, CustomLexLocation LexLocation) : IType
{
    public string GetTypeName()
    {
        return "array";
    }
}

public record RecordType(INodeList<VariableDeclaration> Variables, CustomLexLocation LexLocation) : IType
{
    public string GetTypeName()
    {
        return "record";
    }
}

public interface IStatement : IBodyElement
{
}

public record Assignment(ModifiablePrimary Target, Expression Expression, CustomLexLocation LexLocation) : IStatement;

public record RoutineCall
    (string RoutineName, INodeList<Expression> Arguments, CustomLexLocation LexLocation) : IStatement, IPrimary;

public record WhileLoop
    (Expression Condition, INodeList<IBodyElement> Body, CustomLexLocation LexLocation) : IStatement;

public record ForLoop(string IteratorName, Range Range, INodeList<IBodyElement> Body,
    CustomLexLocation LexLocation) : IStatement;

public record Return(Expression? ReturnValue, CustomLexLocation LexLocation) : IStatement;

public record Range(bool IsReversed, Expression Start, Expression End, CustomLexLocation LexLocation) : INode;

public record IfStatement(Expression Condition, INodeList<IBodyElement> ThenBody, INodeList<IBodyElement>? ElseBody,
    CustomLexLocation LexLocation) : IStatement;

public enum RelationOperationType
{
    And,
    Or,
    Xor
}

public record RelationOperation(RelationOperationType Type, Relation Relation, CustomLexLocation LexLocation)
    : INode
{
    public new string GetType()
    {
        return Type.ToString();
    }
}

public record Expression(Relation First, INodeList<RelationOperation> Operations, CustomLexLocation LexLocation)
    : INode;

public enum SimpleOperationType
{
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual
}

public record SimpleOperation(SimpleOperationType Type, Simple Simple, CustomLexLocation LexLocation)
    : INode
{
    public new string GetType()
    {
        return Type.ToString();
    }
}

public record Relation(Simple First, SimpleOperation? Operation, CustomLexLocation LexLocation) : INode;

public enum SummandOperationType
{
    Plus,
    Minus
}

public record SummandOperation(SummandOperationType Type, Summand Summand, CustomLexLocation LexLocation)
    : INode
{
    public new string GetType()
    {
        return Type.ToString();
    }
}

public record Simple(Summand First, INodeList<SummandOperation> Operations, CustomLexLocation LexLocation) : INode;

public enum FactorOperationType
{
    Multiplication,
    Division,
    ModularDivision
}

public record FactorOperation(FactorOperationType Type, IFactor Factor, CustomLexLocation LexLocation)
    : INode
{
    public new string GetType()
    {
        return Type.ToString();
    }
}

public record Summand(IFactor First, INodeList<FactorOperation> Operations, CustomLexLocation LexLocation) : INode;

public interface IFactor : INode
{
}

public record ExpressionFactor(Expression Expression, CustomLexLocation LexLocation) : IFactor;

public interface IPrimary : IFactor
{
}

public record ModifiablePrimary(string Identifier, INodeList<IModifiablePrimaryOperation> Operations,
        CustomLexLocation LexLocation)
    : IPrimary;

public interface IModifiablePrimaryOperation : INode
{
}

public record MemberCall(string MemberName, CustomLexLocation LexLocation) : IModifiablePrimaryOperation;

public record ArrayIndexing(Expression IndexExpression, CustomLexLocation LexLocation) : IModifiablePrimaryOperation;

public record IntegerPrimary(int Literal, CustomLexLocation LexLocation) : IPrimary;

public record RealPrimary(double Literal, CustomLexLocation LexLocation) : IPrimary;

public record BoolPrimary(bool Value, CustomLexLocation LexLocation) : IPrimary;

public static class AstExtensions
{
    public static INodeList<T> WithNodesRemoved<T>(this INodeList<T> source, IEnumerable<T> nodesToRemove)
        where T : INode
    {
        var toRemove = nodesToRemove as T[] ?? nodesToRemove.ToArray();
        return source switch
        {
            EmptyNodeList<T> emptyNodeList => emptyNodeList,
            NonEmptyNodeList<T> nonEmptyNodeList => toRemove.Contains(nonEmptyNodeList.ThisNode)
                ? nonEmptyNodeList.OtherNodes.WithNodesRemoved(toRemove)
                : nonEmptyNodeList with { OtherNodes = nonEmptyNodeList.OtherNodes.WithNodesRemoved(toRemove) },
            _ => throw new ArgumentOutOfRangeException(nameof(source))
        };
    }

    public static INodeList<T> WithNodesTransformed<T>(this INodeList<T> source, Func<T, T?> transformerFunction)
        where T : INode
    {
        return source switch
        {
            EmptyNodeList<T> emptyNodeList => emptyNodeList,
            NonEmptyNodeList<T> nonEmptyNodeList => transformerFunction(nonEmptyNodeList.ThisNode) switch
            {
                null => nonEmptyNodeList.OtherNodes.WithNodesTransformed(transformerFunction),
                { } someNewNode => nonEmptyNodeList with
                {
                    ThisNode = someNewNode,
                    OtherNodes = nonEmptyNodeList.OtherNodes.WithNodesTransformed(transformerFunction)
                }
            },
            _ => throw new ArgumentOutOfRangeException(nameof(source))
        };
    }
}