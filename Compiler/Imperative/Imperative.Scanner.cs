using QUT.Gppg;

namespace Compiler.Imperative;

internal partial class ImperativeScanner
{
    private void GetTokenData()
    {
        yylloc = new (tokLin, tokCol, tokELin, tokECol, yytext);
        Console.WriteLine($"{yylloc.StartLine}:{yylloc.StartColumn} - {yylloc.EndLine}:{yylloc.EndColumn}");
        Console.WriteLine(yytext);
    }

    public override void yyerror(string format, params object[] args)
    {
        base.yyerror(format, args);
        Console.WriteLine(format, args);
        Console.WriteLine();
    }
}