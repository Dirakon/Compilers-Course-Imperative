using QUT.Gppg;

namespace Compiler;

internal class CustomLexLocation : IMerge<CustomLexLocation>
{
    /// <summary>
    ///     The line at which the text span starts.
    /// </summary>
    public string UnderlyingString { get;  }
    
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
    ///     Default no-arg constructor.
    /// </summary>
    public CustomLexLocation()
    {
    }

    /// <summary>
    ///     Constructor for text-span with given start and end.
    /// </summary>
    /// <param name="sl">start line</param>
    /// <param name="sc">start column</param>
    /// <param name="el">end line </param>
    /// <param name="ec">end column</param>
    public CustomLexLocation(int sl, int sc, int el, int ec, string us)
    {
        UnderlyingString = us;
        StartLine = sl;
        StartColumn = sc;
        EndLine = el;
        EndColumn = ec;
    }

    /// <summary>
    ///     Create a text location which spans from the
    ///     start of "this" to the end of the argument "last"
    /// </summary>
    /// <param name="last">The last location in the result span</param>
    /// <returns>The merged span</returns>
    public CustomLexLocation Merge(CustomLexLocation last)
    {
        // TODO: add explanation why underlying string is empty (because we dont need it)
        return new CustomLexLocation(StartLine, StartColumn, last.EndLine, last.EndColumn, "");
    }
}