using Compiler.Imperative;

namespace Compiler;

public interface ILexem
{
    public CustomLexLocation LexLocation { get; }
}

public record Program(IDeclaration[] Declarations, CustomLexLocation LexLocation) : ILexem
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public interface IDeclaration : ILexem
{
}

public record RoutineDeclaration(string RoutineName, Parameter[] Parameters, IType ReturnType, IStatement[] Body,
    CustomLexLocation LexLocation) : IDeclaration
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public record Parameter(string Name, IType Type, CustomLexLocation LexLocation) : IDeclaration
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public interface IType : ILexem
{
}

public record IntType(CustomLexLocation LexLocation) : IType
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public record RealType(CustomLexLocation LexLocation) : IType
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public record BoolType(CustomLexLocation LexLocation) : IType
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public interface IStatement : ILexem
{
}

public record ReturnStatement(IExpression ReturnExpression, CustomLexLocation LexLocation) : IDeclaration
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

public interface IExpression : ILexem
{
}

public record BinaryExpression
    (string Literal1, string Operation, string Literal2, CustomLexLocation LexLocation) : IExpression
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}


// public record TypeDeclaration(Identifier TypeName, ITypeNode Type, CustomLexLocation LexLocation) : IDeclaration
// {
//     public CustomLexLocation LexLocation { get; } = LexLocation;
// }

// public record ArrayType(ITypeNode publicType, ExpressionNode? LengthExpression, CustomLexLocation LexLocation) : ITypeNode
// {
//     public CustomLexLocation LexLocation { get; } = LexLocation;
// }
//
// public record RecordType(VariableDeclaration[] types, CustomLexLocation LexLocation) : ITypeNode
// {
//     public CustomLexLocation LexLocation { get; } = LexLocation;
// }