using CommandLine;
using Compiler;
using Compiler.Imperative;


Parser.Default.ParseArguments<CommandLineOptions>(args)
    .WithParsed(o =>
    {
        var scanner = new ImperativeScanner(o.InputFile, o.LogsOutputFile);
        var parser = new ImperativeParser(scanner, o.LogsOutputFile);

        // TODO: figure out how to work with output file in a better way
        File.Delete(o.LogsOutputFile);
        try
        {
            parser.Parse();
        }
        catch (SyntaxErrorException er)
        {
            Console.WriteLine(er.Message);
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
}