using System.Text.RegularExpressions;

namespace Compiler;

public static class TokenVisualiser
{
    public static void VisualiseTokensIntoSourceCode(string from, string to)
    {
        File.Delete(to);
        var tokens = File.ReadAllText(from).Split(Environment.NewLine + Environment.NewLine,
            StringSplitOptions.RemoveEmptyEntries);
        var line = 1;
        foreach (var tk in tokens)
        {
            var match = Regex.Match(tk,
                @"Token of type ([A-Z_]+) encountered\.\r?\n(\d+):(\d+) - \d+:\d+\r?\nUnderlying string: (.+)");
            var tokenType = match.Groups[1].Value;
            var lineNumber = int.Parse(match.Groups[2].Value);
            var column = int.Parse(match.Groups[3].Value);
            var uStr = match.Groups[4].Value;
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