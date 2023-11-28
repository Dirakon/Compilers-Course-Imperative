using CommandLine;
using Compiler;
using Compiler.CodeGeneration;
using Compiler.Imperative;
using Compiler.TypeChecking;
using Compiler.Visualizers;
using LLVMSharp;

Parser.Default.ParseArguments<CommandLineOptions>(args)
    .WithParsed(o =>
    {
        var builtInDeclarations = ImperativeConverterFunctions.GetConverterFunctionsDeclarations();
        using (var scanner = new ImperativeScanner(o.InputFile, o.LogsOutputFile))
        {
            var parser = new ImperativeParser(scanner);
            // TODO: figure out how to work with output file in a better way
            File.Delete(o.LogsOutputFile);
            try
            {
                parser.Parse();
                var program = parser.RootNode
                // TODO: uncomment
                // with
                // {
                //     Declarations =
                //     parser.RootNode.Declarations.WithNodesAdded(DummyNodeList.From(builtInDeclarations))
                // }
                ;
                var (globalScope, typeCheckingErrors) = program.TypeCheckAndGetGlobalScope();
                if (typeCheckingErrors is not OperationFailure(var someErrors))
                {
                    Console.WriteLine("No typechecking errors found!");
                    AstVisualizer.VisualizeAst(program, o.BeforeAstOutputFile);
                    program = new Compiler.Program(
                        program.Declarations
                            //.WithNodesTransformed(AstOptimization.ExpressionSimplifier)
                            );
                
                    if (globalScope.DeclaredEntities.GetValueOrDefault("EntryPoint") is DeclaredRoutine declaredRoutine)
                    {
                        GenerateBitcode.StartExecution(o.BitCodeFile, declaredRoutine.ReturnType, globalScope, program);
                    }
                    else Console.WriteLine("Entry point is not detected");
                    AstVisualizer.VisualizeAst(program, o.AfterAstOutputFile);
                }
                else
                {
                    var somewhatOrderedErrors = someErrors
                        .OrderBy(error => error
                            .Locations
                            .DefaultIfEmpty()
                            .MinBy(loc => loc?.StartLine)
                            ?.StartLine);
                    foreach (var error in somewhatOrderedErrors)
                    {
                        var locationString = string.Join(", ", error.Locations.Select(location => $"[{location}]"));
                        Console.WriteLine($"At {locationString}:");
                        Console.WriteLine($"\t{error.Message}");
                        Console.WriteLine();
                    }
                }
                
                
               
            }
            catch (SyntaxErrorException er)
            {
                Console.WriteLine(er.Message);
            }
        }

        TokenVisualiser.VisualiseTokensIntoSourceCode(
            ImperativeScanner.GetAllTokens(File.ReadAllText(o.InputFile)),
            o.TokenVisualizationOutputFile);
    });

public static class DummyNodeList
{
    /// <summary>
    /// Create node list with no lex locations
    /// </summary>
    public static INodeList<T> From<T>(IEnumerable<T> nodes) where T : class, INode
    {
        using var enumerator = nodes.GetEnumerator();
        return From(enumerator);
    }

    private static INodeList<T> From<T>(IEnumerator<T> enumerator) where T : class, INode
    {
        if (!enumerator.MoveNext())
        {
            return new EmptyNodeList<T>();
        }

        return new NonEmptyNodeList<T>(
            enumerator.Current,
            From(enumerator),
            CustomLexLocation.Empty);
    }
}

namespace Compiler
{
    public class CommandLineOptions
    {
        [Option('i', "input", Required = true, HelpText = "Source file to compile.")]
        public required string InputFile { get; init; }

        [Option('t', "visualizeTokens", Required = false, HelpText = "Path to file where to output visualized tokens.")]
        public string TokenVisualizationOutputFile { get; init; } = "Logs/visualizedTokens.txt";

        [Option('l', "logs", Required = false, HelpText = "Path to file where to output logs.")]
        public string LogsOutputFile { get; init; } = "Logs/logs.txt";

        [Option('b', "ast-before", Required = false, HelpText = "Path to file where to output AST.")]
        public string BeforeAstOutputFile { get; init; } = "Logs/before-ast.dot";

        [Option('a', "ast-after", Required = false, HelpText = "Path to file where to output AST.")]
        public string AfterAstOutputFile { get; init; } = "Logs/after-ast.dot";
        
        [Option('c', "bit-code", Required = false, HelpText = "Path to file where to output bitcode.")]
        public string BitCodeFile { get; init; } = "Logs/final.bc";
    }
}