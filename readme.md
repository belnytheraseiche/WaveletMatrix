# BelNytheraSeiche.WaveletMatrix

<!-- [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) -->

BelNytheraSeiche.WaveletMatrix is a high-performance .NET library for advanced sequence and text analysis. It provides a suite of powerful, low-allocation data structures designed for complex queries and full-text search.

This library is built with performance and modern .NET idioms in mind, leveraging Span<T>, ReadOnlyMemory<T>, and BinaryPrimitives for efficient, low-allocation memory management.

---

## Features

- **Generic Wavelet Matrix (`WaveletMatrixGeneric<T>`)**:
  - A compressed data structure for any `IComparable<T>` sequence.
  - Fast `Access`, `Rank`, and `Select` queries.
  - Advanced analytical queries: `Quantile`, `RangeCount`, `RangeFreq`, `RangeMode`, `TopK`, `SmallerValue`(predecessor), `LargerValue`(successor).
  - Built-in coordinate compression for high memory efficiency.

<div></div>

- **Suffix Array (`SuffixArray`)**:
  - Built using the high-performance **SA-IS** algorithm (`O(N)`).
  - Includes a full **LCP Array** built with Kasai's algorithm (`O(N)`).
  - Advanced search capabilities: `Search`, `SearchRepeated`, `SearchLongestRepeated`, `SearchCommon`, `SearchLongestCommon`, `SearchWildcard`.

<div></div>

- **LCP Index (`LcpIndex`)**:
  - Builds on `SuffixArray` to provide **`O(1)` LCP queries** between any two suffixes using a Sparse Table (RMQ).
  - Enables complex string analysis: `CountUniqueSubstrings`, `FindRepeats`, `FindTandemRepeats`, `CalculateZivLempelComplexity`(LZ78).

<div></div>

- **FM-Index (`FMIndex`)**:
  - A complete, fast full-text search index.
  - `O(P)` pattern counting, where `P` is the pattern length.
  - `O(P + k)` pattern locating, where `k` is the number of occurrences (when using the full Suffix Array).
  - Includes `GetSnippet` for displaying context and `RestoreSourceText` for BWT inversion.

<div></div>

- **Robust Serialization**:
  - All major data structures support serialization.
  - Features compression and checksums for data integrity.

---

## Recent Enhancements (This Fork)

This fork introduces several high-performance features aimed at scalable memory management and LLM orchestration:

- **Zero-Allocation Memory Mapping**:
  - Added `FlatMemoryMappedSerializer` for creating succint indices suitable for direct memory mapping.
  - New `WaveletMatrixGeneric<T>.Map(string filePath)` allows instant initialization of massive indices (multi-GB) without reading them into managed memory.
- **LLM Token Optimization**:
  - `BypassCoordinateCompression` flag in `WaveletMatrixOptions` allows bypassing mapping when the input is already a dense integer sequence (e.g., tokens in an LLM context).
- **Core Performance Refinements**:
  - Optimized the internal `WaveletMatrixCore` for better throughput in `Quantile`, `Rank`, and `Select`.
  - Added `.NET 8/9/10` specific hardware intrinsics support.
- **Expanded Targeting**:
  - Added support for `netstandard2.1`, `net8.0`, `net9.0`, and `net10.0`.

---

## Quick Start

Here are some examples of how to use the core components of the library.

[**➡️ View `Program.cs` for More Examples**](https://github.com/belnytheraseiche/WaveletMatrix/blob/main/src/Program.cs)

### `WaveletMatrixGeneric<T>`

Use the Wavelet Matrix for fast queries on generic sequences.

```csharp
using BelNytheraSeiche.WaveletMatrix;

var data = (int[])[3, 1, 4, 1, 5, 9, 2, 5, 3, 5];
var wm = WaveletMatrixGeneric<int>.Create(data);

// Get the value at index 4
var value = wm.Access(4); // -> 5

// Count occurrences of '1' in the prefix [0, 5)
var rank = wm.Rank(5, 1); // -> 2

// Find the position of the 3rd '5'
var pos = wm.Select(3, 5); // -> 9

// Find the most frequent value in the range [3, 10)
var mode = wm.RangeMode(3, 10);
Console.WriteLine($"Mode: {mode.Value}, Freq: {mode.Frequency}"); // -> Mode: 5, Freq: 3
```

### `SuffixArray` and `LcpIndex`

Use `SuffixArray` for powerful string searches and `LcpIndex` for advanced analysis.

```csharp
using BelNytheraSeiche.WaveletMatrix;

var text = "GATTACATACAGATTACA";
var sa = SuffixArray.Create(text);
var lcpIndex = LcpIndex.Create(sa);

// Find all occurrences of "TACA"
var positions = sa.Search("TACA"); // -> [3, 7, 14]

// Find the longest repeated substring
var longestRepeat = sa.SearchLongestRepeated().First();
Console.WriteLine($"Longest Repeat: '{longestRepeat.Text}'"); // -> 'GATTACA'

// Get the LCP length between the suffix at index 0 and 11 in O(1)
int lcp = lcpIndex.GetLcp(0, 11); // -> 7 (for "GATTACA")
```

### `FMIndex`

Use the `FMIndex` for efficient full-text search.

```csharp
using BelNytheraSeiche.WaveletMatrix;

var longText = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
var fmIndex = FMIndex.Create(longText);

// Count occurrences of a pattern
int count = fmIndex.Count("fox"); // -> 2

// Locate all occurrences and display snippets
var pattern = "lazy";
foreach (int pos in fmIndex.Locate(pattern))
{
    var snippet = fmIndex.GetSnippet(pos, pattern.Length, 30);
    Console.WriteLine($"Found at {pos}: ...{snippet.Text}...");
}
// Found at 35: ...mps over the lazy dog. The qui...
// Found at 80: ...n dog jumps over the lazy fox....

### High-Performance Features (This Fork)

#### Memory Mapping for Massive Indices
Use memory mapping for instant loading of multi-GB indices with zero managed allocations.

```csharp
using BelNytheraSeiche.WaveletMatrix;

// MAP an existing index from disk (Zero-Allocation)
var wm = WaveletMatrixGeneric<int>.Map("huge_index.wmxg");

// Use standard queries as usual
var rank = wm.Rank(1000000, 42); 
```

#### LLM Token Optimization
Bypass coordinate compression for sequences that are already dense integers (like LLM tokens).

```csharp
using BelNytheraSeiche.WaveletMatrix;

int[] tokens = GetHugeTokenSequence();
var options = new WaveletMatrixOptions { BypassCoordinateCompression = true };

// Create index without creating a distinct element map
var wm = WaveletMatrixGeneric<int>.Create(tokens, options);
```

---

## License

This project is licensed under the MIT License. See the  **[LICENSE](https://github.com/belnytheraseiche/WaveletMatrix/blob/main/LICENSE)** file for details.

---

## Project Website

[Main Page - https://github.com/belnytheraseiche/WaveletMatrix/](https://github.com/belnytheraseiche/WaveletMatrix/)

[Reference - https://belnytheraseiche.github.io/WaveletMatrix/](https://belnytheraseiche.github.io/WaveletMatrix/)
