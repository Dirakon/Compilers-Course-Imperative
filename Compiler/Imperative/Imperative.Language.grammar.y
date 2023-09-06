%namespace Compiler.Imperative
%partial
%parsertype ImperativeParser
%visibility internal
%tokentype Token

%union { 
			public int n; 
			public string s; 
	   }

%start main

%token NUMBER

%%

main   : number
       ;

number : 
       | NUMBER							{ Console.WriteLine("Rule -> number: {0}", $1.n); SomeValue = $1.n;}
       ;

%%