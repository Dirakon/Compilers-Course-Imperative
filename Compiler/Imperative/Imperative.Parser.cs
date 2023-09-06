using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeParser
{
    public ImperativeParser() : base(null)
    {
    }

    public int SomeValue { set; get; }

    public void Parse(string s)
    {
        var inputBuffer = Encoding.Default.GetBytes(s);
        var stream = new MemoryStream(inputBuffer);
        Scanner = new ImperativeScanner(stream);
        Parse();
    }
}