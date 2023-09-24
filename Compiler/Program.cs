// See https://aka.ms/new-console-template for more information

using Compiler.Imperative;

var parser = new ImperativeParser();

var filename = args[0];

// TODO: figure out how to work with output file in a better way
File.Delete($"output.txt");
parser.Parse(File.ReadAllText($"Assets/{filename}.txt"));
// Console.WriteLine($"WE PARSED SOMETHING. WE GOT: {parser.SomeValue}");

// TODO: take CLI arguments, compile code, run with args, output result