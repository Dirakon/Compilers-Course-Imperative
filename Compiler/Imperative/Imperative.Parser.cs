using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeParser
{
    private readonly string _logsOutputPath;

    public ImperativeParser(ImperativeScanner scanner, string logsOutputPath) : base(scanner)
    {
        _logsOutputPath = logsOutputPath;
    }
}