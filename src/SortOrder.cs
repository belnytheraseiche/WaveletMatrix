
namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Specifies the sort order for the results of a Locate operation.
/// </summary>
public enum SortOrder
{
    /// <summary>
    /// The positions are returned in an unsorted, arbitrary order. This is the most performant option.
    /// </summary>
    Unordered,
    /// <summary>
    /// The positions are returned in ascending numerical order. This requires an internal sort and is less performant than Unordered.
    /// </summary>
    Ascending,
    /// <summary>
    /// The positions are returned in descending numerical order. This requires an internal sort and is less performant than Unordered.
    /// </summary>
    Descending,
}
