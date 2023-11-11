namespace Compiler;

public static class EnumerableExtensions
{
    public static IEnumerable<T> NotNull<T>(this IEnumerable<T?> enumerable) where T : class
    {
        return enumerable.Where(e => e != null).Select(e => e!);
    }
    
    public static IEnumerable<T> NotNull<T>(this IEnumerable<T?> enumerable) where T : struct
    {
        return enumerable.Where(e => e != null).Select(e => e!.Value);
    }

    public static IEnumerable<TSub> OfSubType<TMain, TSub>(this IEnumerable<TMain> enumerable) where TMain : class where TSub: class, TMain
    {
        return enumerable.Select(value => value switch
        {
            TSub subTypeValue => subTypeValue,
            _ => null
        }).NotNull();
    }
}