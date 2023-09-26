using QUT.Gppg;

namespace Compiler.Imperative;

internal partial class ImperativeScanner
{
    private int GetTokenData(Token tokenType)
    {
        yylloc = new(tokLin, tokCol, tokELin, tokECol, yytext);

        var outputLines = new[]
        {
            $"Token of type {tokenType} encountered.",
            $"{yylloc.StartLine}:{yylloc.StartColumn} - {yylloc.EndLine}:{yylloc.EndColumn}",
            $"Underlying string: {yytext}",
            ""
        };

        foreach (var outputLine in outputLines)
        {
            Console.WriteLine(outputLine);
        }

        File.AppendAllLines(@"output.txt", outputLines);

        return (int)tokenType;
    }

    public override void yyerror(string format, params object[] args)
    {
        base.yyerror(format, args);
        Console.WriteLine(format, args);
        Console.WriteLine();
    }
}