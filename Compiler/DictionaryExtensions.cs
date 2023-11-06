namespace Compiler;

public static class DictionaryExtensions
{
    public static Dictionary<TKey, TElement> ToDictionaryIgnoringDuplicateKeys<TSource, TKey, TElement>(
        this IEnumerable<TSource> source, 
        Func<TSource, TKey> keySelector, 
        Func<TSource, TElement> elementSelector, 
        IEqualityComparer<TKey>? comparer = null) where TKey : notnull
    {
        var dictionary = new Dictionary<TKey, TElement>(comparer);
        
        foreach (var element in source)
        {
            dictionary[keySelector(element)] = elementSelector(element);
        }

        return dictionary; 
    }
}