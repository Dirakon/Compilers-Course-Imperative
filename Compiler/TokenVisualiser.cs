using Compiler.Imperative;

namespace Compiler;

internal static class TokenVisualiser
{
    public static void VisualiseTokensIntoSourceCode(
        IEnumerable<(Token token, CustomLexLocation lexLocation)> tokensData,
        string to)
    {
        File.Delete(to);
        var line = 1;
        foreach (var (token, lexLocation) in tokensData)
        {
            var tokenType = token.ToString();
            var lineNumber = lexLocation.StartLine;
            var column = lexLocation.StartColumn;
            var uStr = lexLocation.UnderlyingString;
            string textToWrite;
            if (line == lineNumber)
            {
                textToWrite = tokenType;
            }
            else
            {
                textToWrite = new string('\n', lineNumber - line) + new string(' ', column) + tokenType;
                line = lineNumber;
            }

            textToWrite += tokenType == "IDENTIFIER" || tokenType == "INT_LITERAL" || tokenType == "REAL_LITERAL"
                ? $"({uStr}) "
                : " ";
            File.AppendAllText(to, textToWrite);
        }
    }
}