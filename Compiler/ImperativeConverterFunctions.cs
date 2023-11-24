using Compiler.Imperative;

namespace Compiler;

internal static class ImperativeConverterFunctions
{
    public static IDeclaration[] GetConverterFunctionsDeclarations()
    {
        using var scanner = new ImperativeScanner("Assets/BuiltinFunctions.txt", null);
        var parser = new ImperativeParser(scanner);
        parser.Parse();
        return parser.RootNode
            .Declarations
            .WithNodesTransformed(
                declaration => declaration switch
                {
                    // Obfuscate lex locations not to confuse the user with error messages that use these tokens
                    // (only needed for global declarations:
                    //      only they can emit duplicate error messages when adding user code)
                    TypeDeclaration typeDeclaration =>
                        typeDeclaration with { LexLocation = CustomLexLocation.Empty },
                    VariableDeclaration variableDeclaration =>
                        variableDeclaration with { LexLocation = CustomLexLocation.Empty },
                    RoutineDeclaration routineDeclaration =>
                        routineDeclaration with { LexLocation = CustomLexLocation.Empty },
                    _ => throw new ArgumentOutOfRangeException(nameof(declaration))
                }
            )
            .ToArray();
    }
}