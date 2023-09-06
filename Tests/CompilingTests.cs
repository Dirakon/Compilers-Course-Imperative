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