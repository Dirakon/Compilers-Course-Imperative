using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeScanner
{
    private readonly string? _logsFilePath;

    public ImperativeScanner(string inputFilePath, string logsFilePath) 
        : this(new FileStream(inputFilePath, FileMode.Open))
    {
        _logsFilePath = logsFilePath;
    }
    
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

        if (_logsFilePath != null)
        {
            File.AppendAllLines(_logsFilePath, outputLines);
        }

        return (int)tokenType;
    }

    public override void yyerror(string format, params object[] args)
    {
        base.yyerror(format, args);
        Console.WriteLine(format, args);
        Console.WriteLine();
    }
    
    public static (Token token, CustomLexLocation lexLocation)[] GetAllTokens(string inputText)
    {
        var inputBuffer = Encoding.Default.GetBytes(inputText);
        var stream = new MemoryStream(inputBuffer);
        var scanner = new ImperativeScanner(stream);

        var tokensData = new List<(Token token, CustomLexLocation lexLocation)>();
        Token currentToken;
        do
        {
            currentToken = (Token)scanner.yylex();
            tokensData.Add((currentToken, scanner.yylloc));
        } while (currentToken != Token.EOF);

        return tokensData.ToArray();
    }
}