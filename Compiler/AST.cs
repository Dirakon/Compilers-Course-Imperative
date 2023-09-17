using Compiler.Imperative;

namespace Compiler;

internal interface ILexem
{
    public CustomLexLocation LexLocation { get;  }
}

internal record Program(IDeclaration[] Declarations, CustomLexLocation LexLocation) : ILexem
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}



internal interface IDeclaration : ILexem
{
}

internal record RoutineDeclaration(string RoutineName, Parameter[] Parameters,  IType ReturnType, IStatement[] Body , CustomLexLocation LexLocation) : IDeclaration
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}
internal record Parameter(string Name, IType Type, CustomLexLocation LexLocation) : IDeclaration
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}


internal interface IType : ILexem
{
}

internal record IntType(CustomLexLocation LexLocation) : IType
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

internal record RealType(CustomLexLocation LexLocation) : IType
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

internal record BoolType(CustomLexLocation LexLocation) : IType
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}

internal interface IStatement : ILexem
{
}

internal record ReturnStatement(IExpression ReturnExpression, CustomLexLocation LexLocation) : IDeclaration
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}



internal interface IExpression : ILexem
{
}

internal record BinaryExpression(string Literal1, string Operation, string Literal2, CustomLexLocation LexLocation) : IExpression
{
    public CustomLexLocation LexLocation { get; } = LexLocation;
}


// internal record TypeDeclaration(Identifier TypeName, ITypeNode Type, CustomLexLocation LexLocation) : IDeclaration
// {
//     public CustomLexLocation LexLocation { get; } = LexLocation;
// }

// internal record ArrayType(ITypeNode InternalType, ExpressionNode? LengthExpression, CustomLexLocation LexLocation) : ITypeNode
// {
//     public CustomLexLocation LexLocation { get; } = LexLocation;
// }
//
// internal record RecordType(VariableDeclaration[] types, CustomLexLocation LexLocation) : ITypeNode
// {
//     public CustomLexLocation LexLocation { get; } = LexLocation;
// }
