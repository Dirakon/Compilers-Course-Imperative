namespace Compiler.Imperative;

internal partial class ImperativeScanner
{
    private void GetNumber()
    {
        yylval.s = yytext;
        yylval.n = int.Parse(yytext);
    }

    public override void yyerror(string format, params object[] args)
    {
        base.yyerror(format, args);
        Console.WriteLine(format, args);
        Console.WriteLine();
    }
}