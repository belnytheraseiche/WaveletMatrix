// MIT License
// 
// Copyright (c) 2025 belnytheraseiche
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides a static method for performing the Burrows-Wheeler Transform (BWT).
/// </summary>
/// <remarks>
/// The BWT rearranges a string into runs of similar characters, which is useful for compression and indexing.
/// This implementation uses a <see cref="SuffixArray"/> to efficiently compute the transform.
/// </remarks>
public static class BurrowsWheelerTransform
{
    /// <summary>
    /// Performs the Burrows-Wheeler Transform on the given text.
    /// </summary>
    /// <param name="text">The input text to transform. For a correct and reversible transform, it is highly recommended that the text ends with a unique, lexicographically smallest character (terminator), such as '\0'.</param>
    /// <returns>A <see cref="BwtResult"/> record containing the generated Suffix Array, the BWT string (L-column), and the zero-based index of the original string in the sorted rotation matrix.</returns>
    public static BwtResult Transform(ReadOnlyMemory<char> text)
    {
        var sa = SuffixArray.Create(text);
        var bwtChars = new char[text.Length];
        var original = -1;
        var saSpan = sa.SA.Span;
        var textSpan = text.Span;
        for (var i = 0; i < text.Length; i++)
        {
            var start = saSpan[i];
            bwtChars[i] = start == 0 ? textSpan[^1] : textSpan[start - 1];
            if (start == 0)
                original = i;
        }

        return new(sa, new(bwtChars), original);
    }

    // 
    // 

    /// <summary>
    /// Encapsulates the result of a Burrows-Wheeler Transform operation.
    /// </summary>
    /// <param name="SA">The Suffix Array generated during the transform.</param>
    /// <param name="BwtString">The resulting BWT string (often called the L-column).</param>
    /// <param name="OriginalIndex">The zero-based index of the original string in the conceptual sorted matrix of rotations. This is required for the inverse transform.</param>
    public record BwtResult(SuffixArray SA, string BwtString, int OriginalIndex);
}
