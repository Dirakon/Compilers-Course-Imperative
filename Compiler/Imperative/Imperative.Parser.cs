using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeParser
{
    public ImperativeParser() : base(null)
    {
        
    }

    public Program ProgramNode { set; get; }

    public void Parse(string s)
    {
        var inputBuffer = Encoding.Default.GetBytes(s);
        var stream = new MemoryStream(inputBuffer);
        Scanner = new ImperativeScanner(stream);

       // TODO: separate concerns GPPG from GPLEX?
        // while (NextToken != (int) Token.EOF)
        // {
        //     NextToken = Scanner.yylex();
        //     Console.WriteLine(Scanner.yylloc);
        //     
        // }

        
        Parse();
        var topElement = ValueStack.TopElement();
        
    }
}