using Xunit.Abstractions;

namespace Tests;

public class CompilingTests
{
    private readonly ITestOutputHelper _testOutputHelper;

    public CompilingTests(ITestOutputHelper testOutputHelper)
    {
        _testOutputHelper = testOutputHelper;
    }

    // TODO: specify arguments and expected output
    [Theory]
    [InlineData("Sum", new object[] {1, 2}, 3)]
    [InlineData("Arrays", new object[] {}, true)]
    [InlineData("Records", new object[] {}, true)]
    [InlineData("EntryPoint", new object[] {}, true)]
    [InlineData("ScopeShadowing", new object[] {}, true)]
    [InlineData("Routines", new object[] {}, true)]
    [InlineData("Circle", new object[] {1, 1, 0, 0, 2}, true)]
    [InlineData("ReferenceClone", new object[] {}, true)]
    [InlineData("Logic101", new object[] {true, false}, true)]
    [InlineData("Palindrome", new object[] {12321}, true)]
    [InlineData("Sort", new object[] {10}, true)]
    [InlineData("ImpConv", new object[] {1}, 2)]
    [InlineData("ForLoopRev", new object[] {}, 9)]
    [InlineData("WhileLoop", new object[] {17}, 7)]
    [InlineData("Divide", new object[] {4,2}, 2)]
    public void RunProgram_ValidCode_ShouldGiveExpectedOutput(
        string fileName,
        object[] arguments,
        object expectedResult)
    {
        _testOutputHelper.WriteLine(GetCode(fileName));

        // TODO do something
        Compiler.Compiler.Compile(GetCode(fileName));
    }

    // TODO: compile and execute compiled program, inserting arguments and extracting output
    private string GetCode(string fileName)
    {
        var code = File.ReadAllText($"../../../TestableCode/{fileName}.txt");

        return code;
    }
}