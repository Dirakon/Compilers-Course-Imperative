using QUT.Gppg;

namespace Compiler;

public class CustomLexLocation : IMerge<CustomLexLocation>
{
    public static readonly CustomLexLocation Empty = new CustomLexLocation(-1,-1,1,-1, "");

    /// <summary>
    ///     Default no-arg constructor.
    /// </summary>
    public CustomLexLocation()
    {
        UnderlyingString = "";
    }

    /// <summary>
    ///     Constructor for text-span with given start and end.
    /// </summary>
    /// <param name="sl">start line</param>
    /// <param name="sc">start column</param>
    /// <param name="el">end line </param>
    /// <param name="ec">end column</param>
    /// <param name="us">underlying string</param>
    public CustomLexLocation(int sl, int sc, int el, int ec, string us)
    {
        UnderlyingString = us;
        UnderlyingString = us;
        StartLine = sl;
        StartColumn = sc;
        EndLine = el;
        EndColumn = ec;
    }

    /// <summary>
    ///     The line at which the text span starts.
    /// </summary>
    public string UnderlyingString { get; }

    /// <summary>
    ///     The line at which the text span starts.
    /// </summary>
    public int StartLine { get; }

    /// <summary>
    ///     The column at which the text span starts.
    /// </summary>
    public int StartColumn { get; }

    /// <summary>
    ///     The line on which the text span ends.
    /// </summary>
    public int EndLine { get; }

    /// <summary>
    ///     The column of the first character
    ///     beyond the end of the text span.
    /// </summary>
    public int EndColumn { get; }

    /// <summary>
    ///     Create a text location which spans from the
    ///     start of "this" to the end of the argument "last"
    /// </summary>
    /// <param name="last">The last location in the result span</param>
    /// <returns>The merged span</returns>
    public CustomLexLocation Merge(CustomLexLocation last)
    {
        if (this == Empty)
            return last;
        if (last == Empty)
            return this;
        // TODO: add explanation why underlying string is empty (because we dont need it)
        return new CustomLexLocation(StartLine, StartColumn, last.EndLine, last.EndColumn, "");
    }

    public override string ToString()
    {
        if (this == Empty)
        {
            return "-";
        }
        return StartLine == EndLine
            ? $"line {StartLine}, columns {StartColumn}-{EndColumn}"
            : $"lines {StartLine}-{EndLine}";
    }
}