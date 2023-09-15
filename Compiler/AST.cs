using QUT.Gppg;

namespace Compiler;

internal interface INode
{
    public LexLocation LexLocation { get;  }
}

internal record Program(Routine[] Routines, TypeDeclaration[] TypeDeclarations, LexLocation LexLocation) : INode
{
    public LexLocation LexLocation { get; } = LexLocation;
}

internal record Routine(LexLocation LexLocation) : INode
{
    public LexLocation LexLocation { get; } = LexLocation;
}

internal record TypeDeclaration(LexLocation LexLocation) : INode
{
    public LexLocation LexLocation { get; } = LexLocation;
}

