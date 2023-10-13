// This code was generated by the Gardens Point Parser Generator
// Copyright (c) Wayne Kelly, John Gough, QUT 2005-2014
// (see accompanying GPPGcopyright.rtf)

// GPPG version 1.5.2
// DateTime: 10/14/2023 12:38:00 AM
// Input file <Imperative/Imperative.Language.grammar.y - 10/14/2023 12:37:53 AM>

// options: no-lines gplex

using System;
using System.Collections.Generic;
using System.CodeDom.Compiler;
using System.Globalization;
using System.Text;
using QUT.Gppg;

namespace Compiler.Imperative
{
internal enum Token {error=2,EOF=3,IDENTIFIER=4,BOOL=5,INT=6,
    REAL=7,TYPE=8,IS=9,END=10,RETURN=11,VAR=12,
    ROUTINE=13,FOR=14,WHILE=15,LOOP=16,IN=17,REVERSE=18,
    IF=19,THEN=20,ELSE=21,ARRAY=22,RECORD=23,ROUND_OPEN=24,
    ROUND_CLOSE=25,CURLY_OPEN=26,CURLY_CLOSE=27,SQUARE_OPEN=28,SQUARE_CLOSE=29,SEMICOLON=30,
    COLON=31,COMMA=32,ASSIGN=33,DOT=34,MINUS=35,PLUS=36,
    MULTIPLY=37,DIVIDE=38,PERCENT=39,AND=40,OR=41,XOR=42,
    RANGE=43,LEQ=44,GEQ=45,LESS=46,GREATER=47,EQUAL=48,
    NOT_EQUAL=49,INT_LITERAL=50,REAL_LITERAL=51,FALSE=52,TRUE=53};

// Abstract base class for GPLEX scanners
[GeneratedCodeAttribute( "Gardens Point Parser Generator", "1.5.2")]
internal abstract class ScanBase : AbstractScanner<INode,CustomLexLocation> {
  private CustomLexLocation __yylloc = new CustomLexLocation();
  public override CustomLexLocation yylloc { get { return __yylloc; } set { __yylloc = value; } }
  protected virtual bool yywrap() { return true; }
}

// Utility class for encapsulating token information
[GeneratedCodeAttribute( "Gardens Point Parser Generator", "1.5.2")]
internal class ScanObj {
  public int token;
  public INode yylval;
  public CustomLexLocation yylloc;
  public ScanObj( int t, INode val, CustomLexLocation loc ) {
    this.token = t; this.yylval = val; this.yylloc = loc;
  }
}

[GeneratedCodeAttribute( "Gardens Point Parser Generator", "1.5.2")]
internal partial class ImperativeParser: ShiftReduceParser<INode, CustomLexLocation>
{
#pragma warning disable 649
  private static Dictionary<int, string> aliases;
#pragma warning restore 649
  private static Rule[] rules = new Rule[4];
  private static State[] states = new State[8];
  private static string[] nonTerms = new string[] {
      "main", "$accept", "routine", };

  static ImperativeParser() {
    states[0] = new State(new int[]{13,3},new int[]{-1,1});
    states[1] = new State(new int[]{3,2});
    states[2] = new State(-1);
    states[3] = new State(new int[]{4,4});
    states[4] = new State(new int[]{24,5});
    states[5] = new State(new int[]{25,6});
    states[6] = new State(new int[]{31,7});
    states[7] = new State(-2);

    for (int sNo = 0; sNo < states.Length; sNo++) states[sNo].number = sNo;

    rules[1] = new Rule(-2, new int[]{-1,3});
    rules[2] = new Rule(-1, new int[]{13,4,24,25,31});
    rules[3] = new Rule(-3, new int[]{13});
  }

  protected override void Initialize() {
    this.InitSpecialTokens((int)Token.error, (int)Token.EOF);
    this.InitStates(states);
    this.InitRules(rules);
    this.InitNonTerminals(nonTerms);
  }

  protected override void DoAction(int action)
  {
#pragma warning disable 162, 1522
    switch (action)
    {
    }
#pragma warning restore 162, 1522
  }

  protected override string TerminalToString(int terminal)
  {
    if (aliases != null && aliases.ContainsKey(terminal))
        return aliases[terminal];
    else if (((Token)terminal).ToString() != terminal.ToString(CultureInfo.InvariantCulture))
        return ((Token)terminal).ToString();
    else
        return CharToString((char)terminal);
  }

}
}
