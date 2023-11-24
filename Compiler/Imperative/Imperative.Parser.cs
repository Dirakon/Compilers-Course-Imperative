using System.Text;

namespace Compiler.Imperative;

internal partial class ImperativeParser
{
    public Program RootNode = null!;

    public ImperativeParser(ImperativeScanner scanner) : base(scanner)
    {
    }
}