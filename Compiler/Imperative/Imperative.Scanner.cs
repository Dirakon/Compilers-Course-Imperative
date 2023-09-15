using QUT.Gppg;

namespace Compiler.Imperative;

internal partial class ImperativeScanner
{
    private void GetTokenData()
    {
        yylval.position = new (tokLin, tokCol, tokELin, tokECol);
        yylval.underlyingString = yytext;
        Console.WriteLine($"{yylval.position.StartLine}:{yylval.position.StartColumn} - {yylval.position.EndLine}:{yylval.position.EndColumn}");
        Console.WriteLine(yylval.underlyingString);
    }

    public override void yyerror(string format, params object[] args)
    {
        base.yyerror(format, args);
        Console.WriteLine(format, args);
        Console.WriteLine();
    }
}