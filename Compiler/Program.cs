using CommandLine;
using Compiler;
using Compiler.Imperative;
using Compiler.TypeChecking;

Parser.Default.ParseArguments<CommandLineOptions>(args)
    .WithParsed(o =>
    {
        using (var scanner = new ImperativeScanner(o.InputFile, o.LogsOutputFile))
        {
            var parser = new ImperativeParser(scanner, o.LogsOutputFile);
            // TODO: figure out how to work with output file in a better way
            File.Delete(o.LogsOutputFile);
            try
            {
                parser.Parse();
                var rootNode = parser.RootNode;
                var typeCheckingErrors = rootNode.TypeCheck();
                if (typeCheckingErrors is not OperationFailure(var someErrors))
                {
                    Console.WriteLine("No typechecking errors found!");
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
                
                var program = parser.RootNode;
                AstVisualizer.VisualizeAst(program, o.BeforeAstOutputFile);
                program = new Compiler.Program(program.Declarations.WithNodesTransformed(AstOptimization.ExpressionSimplifier));
                AstVisualizer.VisualizeAst(program, o.AfterAstOutputFile);
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

public class CommandLineOptions
{
    [Option('i', "input", Required = true, HelpText = "Source file to compile.")]
    public required string InputFile { get; init; }

    [Option('t', "visualizeTokens", Required = false, HelpText = "Path to file where to output visualized tokens.")]
    public string TokenVisualizationOutputFile { get; init; } = "visualizedTokens.txt";

    [Option('l', "logs", Required = false, HelpText = "Path to file where to output logs.")]
    public string LogsOutputFile { get; init; } = "logs.txt";

    [Option('b', "ast-before", Required = false, HelpText = "Path to file where to output AST.")]
    public string BeforeAstOutputFile { get; init; } = "before-ast.dot";
    
    [Option('a', "ast-after", Required = false, HelpText = "Path to file where to output AST.")]
    public string AfterAstOutputFile { get; init; } = "after-ast.dot";
}
