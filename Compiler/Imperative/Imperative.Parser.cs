using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeParser
{
    public ImperativeParser() : base(null)
    {
    }

    public void Parse(string s)
    {
        var inputBuffer = Encoding.Default.GetBytes(s);
        var stream = new MemoryStream(inputBuffer);
        Scanner = new ImperativeScanner(stream);
            
        // TODO: separate concerns GPPG from GPLEX?
        while (NextToken != (int)Token.EOF)
        {
            NextToken = Scanner.yylex();
        }


        // Parse();
        // var topElement = ValueStack.TopElement();
    }
}