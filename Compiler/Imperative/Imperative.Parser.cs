using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeParser
{
    private readonly string _logsOutputPath;

    public Program RootNode = null!;

    public ImperativeParser(ImperativeScanner scanner, string logsOutputPath) : base(scanner)
    {
        _logsOutputPath = logsOutputPath;
    }
}