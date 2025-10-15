
using BelNytheraSeiche.WaveletMatrix;

class Program
{
    // Samples.
    static void Main(string[] args)
    {
        Console.WriteLine("## WaveletMatrixGeneric<T>");
        SampleWaveletMatrix();
        Console.WriteLine();

        Console.WriteLine("## SuffixArray, LcpIndex");
        SampleSuffixArrayAndLcpIndex();
        Console.WriteLine();

        Console.WriteLine("## FMIndex");
        SampleFMIndex();
        Console.WriteLine();
    }

    /// <summary>
    /// A sample demonstrating the main features of WaveletMatrixGeneric<T>.
    /// </summary>
    static void SampleWaveletMatrix()
    {
        var data = new int[] { 3, 1, 4, 1, 5, 9, 2, 5, 3, 5, 8, 9 };
        Console.WriteLine($"Source Data: [{string.Join(", ", data)}]");

        // 1. Construction
        var wm = WaveletMatrixGeneric<int>.Create(data);

        // 2. Basic Queries
        Console.WriteLine($"Access(4): {wm.Access(4)}"); // Gets the element at index 4 -> 5
        Console.WriteLine($"Rank(10, 5): {wm.Rank(10, 5)}"); // Counts occurrences of '5' in the range [0, 10) -> 3
        Console.WriteLine($"Select(3, 5): {wm.Select(3, 5)}"); // Finds the position of the 3rd occurrence of '5' -> 9

        // 3. Analytical Queries
        var rangeMode = wm.RangeMode(3, 10);
        Console.WriteLine($"RangeMode(3, 10): Value={rangeMode.Value}, Freq={rangeMode.Frequency}"); // Finds the mode in range [1,5,9,2,5,3,5] -> 5 (3 times)

        var quantile = wm.Quantile(3, 10, 0); // Finds the 0-th smallest value (minimum) in range [1,5,9,2,5,3,5] -> 1
        Console.WriteLine($"Quantile(3, 10, 0): {quantile}");

        // 4. Serialization and Deserialization
        var wmSerialized = WaveletMatrixGeneric<int>.Serialize(wm);
        var wmDeserialized = WaveletMatrixGeneric<int>.Deserialize(wmSerialized);
        Console.WriteLine($"Serialization & Deserialization successful: {wmDeserialized.Rank(10, 5) == 3}");
    }

    /// <summary>
    /// A sample demonstrating the main features of SuffixArray and LcpIndex.
    /// </summary>
    static void SampleSuffixArrayAndLcpIndex()
    {
        var text = "GATTACATACAGATTACA";
        Console.WriteLine($"Source Text: {text}");

        // 1. Construction
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);

        // 2. Basic search with SuffixArray
        var pattern = "TACA";
        var positions = sa.Search(pattern);
        Console.WriteLine($"SuffixArray.Search('{pattern}'): [{string.Join(", ", positions)}]"); // Finds occurrences of "TACA" -> [3, 7, 14]

        // 3. Advanced queries with LcpIndex
        var uniqueCount = lcpIndex.CountUniqueSubstrings();
        Console.WriteLine($"CountUniqueSubstrings(): {uniqueCount}"); // Counts all unique substrings

        // Calculates the LCP length between the suffix starting at index 3 ("TACATACA...") and the one at index 14 ("TACA")
        var lcp = lcpIndex.GetLcp(3, 14);
        Console.WriteLine($"GetLcp(3, 14): {lcp}"); // "TACA" matches, so the length is 4

        // Finds the longest repeated substring
        var longestRepeat = sa.SearchLongestRepeated().First();
        Console.WriteLine($"SearchLongestRepeated(): '{longestRepeat.Text}' at [{string.Join(", ", longestRepeat.Positions)}]"); // "GATTACA"

        // 4. Serialization and Deserialization
        var saSerialized = SuffixArray.Serialize(sa);
        var saDeserialized = SuffixArray.Deserialize(saSerialized);
        Console.WriteLine($"SuffixArray, Serialization & Deserialization successful: {saDeserialized.Search(pattern).ToArray() is [3, 7, 14]}");

        var lcpIndexSerialized = LcpIndex.Serialize(lcpIndex);
        var lcpIndexDeserialized = LcpIndex.Deserialize(lcpIndexSerialized);
        Console.WriteLine($"LcpIndex, Serialization & Deserialization successful: {lcpIndexDeserialized.GetLcp(3, 14) == 4}");
    }

    /// <summary>
    /// A sample demonstrating full-text search using the FMIndex.
    /// </summary>
    static void SampleFMIndex()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        Console.WriteLine($"Source Text: {text}");

        // 1. Construction (BWT, WaveletMatrix, etc., are created automatically)
        var fm = FMIndex.Create(text);

        // 2. Count (Counts the number of occurrences)
        var pattern = "fox";
        var count = fm.Count(pattern);
        Console.WriteLine($"FMIndex.Count('{pattern}'): {count}"); // Number of occurrences of "fox" -> 2

        // 3. Locate (Finds the starting positions of all occurrences)
        var positions = fm.Locate(pattern);
        Console.WriteLine($"FMIndex.Locate('{pattern}'): [{string.Join(", ", positions)}]"); // Positions of "fox" -> [16, 85]

        // 4. GetSnippet (Displays the surrounding text for each match)
        Console.WriteLine("--- Snippets ---");
        foreach (var pos in positions)
        {
            // Get a snippet of 30 characters total, centered around the keyword "fox" (length 3)
            var snippet = fm.GetSnippet(pos, pattern.Length, 30);
            Console.WriteLine($"...{snippet.Text}...");
            // Simple highlighting of the keyword within the snippet
            Console.WriteLine(new string(' ', 3 + snippet.KeyPosition) + new string('^', pattern.Length));
        }

        // 5. ReconstructText (Restores the original text from the index)
        var reconstructed = fm.RestoreSourceText();
        Console.WriteLine($"\nRestoreSourceText successful: {reconstructed == text}");

        // 6. Serialization and Deserialization
        var fmSerialized = FMIndex.Serialize(fm);
        var fmDeserialized = FMIndex.Deserialize(fmSerialized);
        Console.WriteLine($"Serialization & Deserialization successful: {fmDeserialized.Locate(pattern).ToArray() is [16, 85]}");
    }
}
