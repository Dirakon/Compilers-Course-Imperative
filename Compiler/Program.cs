﻿// See https://aka.ms/new-console-template for more information

using Compiler.Imperative;

var parser = new ImperativeParser();
parser.Parse(@"routine EntryPoint():");
Console.WriteLine($"WE PARSED SOMETHING. WE GOT: {parser.SomeValue}");

// TODO: take CLI arguments, compile code, run with args, output result