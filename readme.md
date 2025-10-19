# BelNytheraSeiche.WaveletMatrix

[![NuGet version](https://img.shields.io/nuget/v/BelNytheraSeiche.WaveletMatrix.svg)](https://www.nuget.org/packages/BelNytheraSeiche.WaveletMatrix/)
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

## Installation

You can install the library via the .NET CLI:

```sh
dotnet add package BelNytheraSeiche.WaveletMatrix
```

Or via the NuGet Package Manager Console:

```powershell
Install-Package BelNytheraSeiche.WaveletMatrix
```

[**➡️ View on nuget package page**](https://www.nuget.org/packages/BelNytheraSeiche.WaveletMatrix/)

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
```

---

## Performance

The following benchmarks were run on [Snapdragon X Plus - X1P42100] with .NET 9.0.
Benchmarks were run on a typical machine using `BenchmarkDotNet` to measure the performance of key operations on two standard corpora from the [Pizza & Chili Corpus](http://pizzachili.dcc.uchile.cl/).

-   **`english.50MB`**: A 100MB file of English text, representing a **large alphabet**.
-   **`dna.50MB`**: A 100MB file of DNA sequences, representing a **small alphabet**.

The patterns used for searching were `{"government", "internationalization", "the"}` for English and `{"ACGTACGT", "ACGTACGTACGTACGT", "GATTACA", "T"}` for DNA.

### Benchmark Results

This benchmark meatures the time taken to perform `SuffixArray.Search`.

- english.50MB

| Method | pattern              | Mean            | Error         | StdDev        | Gen0   | Allocated  |
|------- |--------------------- |----------------:|--------------:|--------------:|-------:|-----------:|
| Search | government           |     16,536.8 ns |     302.53 ns |     268.18 ns | 4.9438 |    20680 B |
| Search | internationalization |        145.5 ns |       0.61 ns |       0.58 ns | 0.0305 |      128 B |
| Search | the                  | 83,279,781.1 ns | 380,001.20 ns | 355,453.36 ns |      - | 21189896 B |

This benchmark meatures the time taken to perform `FMIndex.Count`.

- english.50MB

| Method                   | pattern              | Mean       | Error    | StdDev   | Allocated |
|------------------------- |--------------------- |-----------:|---------:|---------:|----------:|
| Count (no sampling)      | government           | 1,358.2 ns |  2.92 ns |  2.73 ns |         - |
| Count (sampling rate 32) | government           | 1,343.4 ns |  2.19 ns |  2.05 ns |         - |
| Count (no sampling)      | internationalization | 1,347.0 ns | 17.27 ns | 16.15 ns |         - |
| Count (sampling rate 32) | internationalization | 1,282.2 ns |  3.03 ns |  2.69 ns |         - |
| Count (no sampling)      | the                  |   321.5 ns |  0.62 ns |  0.55 ns |         - |
| Count (sampling rate 32) | the                  |   283.3 ns |  0.78 ns |  0.73 ns |         - |

- dna.50MB

| Method                   | pattern          | Mean      | Error    | StdDev    | Allocated |
|------------------------- |----------------- |----------:|---------:|----------:|----------:|
| Count (no sampling)      | ACGTACGT         | 554.24 ns | 1.333 ns |  1.182 ns |         - |
| Count (sampling rate 32) | ACGTACGT         | 559.41 ns | 4.838 ns |  4.525 ns |         - |
| Count (no sampling)      | ACGTACGTACGTACGT | 728.51 ns | 1.815 ns |  1.417 ns |         - |
| Count (sampling rate 32) | ACGTACGTACGTACGT | 727.01 ns | 2.037 ns |  1.905 ns |         - |
| Count (no sampling)      | GATTACA          | 493.43 ns | 1.561 ns |  1.304 ns |         - |
| Count (sampling rate 32) | GATTACA          | 498.76 ns | 9.615 ns | 10.288 ns |         - |
| Count (no sampling)      | T                |  27.54 ns | 0.247 ns |  0.231 ns |         - |
| Count (sampling rate 32) | T                |  27.35 ns | 0.055 ns |  0.051 ns |         - |


This benchmark meatures the time taken to perform `LcpIndex.GetLcp`.

- english.50MB

| Method | Mean     | Error    | StdDev   | Allocated |
|------- |---------:|---------:|---------:|----------:|
| GetLcp | 21.71 ns | 0.027 ns | 0.026 ns |         - |

- dna.50MB

| Method | Mean     | Error    | StdDev   | Allocated |
|------- |---------:|---------:|---------:|----------:|
| GetLcp | 26.94 ns | 0.381 ns | 0.356 ns |         - |

This benchmark meatures the time taken to perform `WaveletMatrixGeneric<char>.Rank`.

- dna.50MB

| Method | Mean     | Error    | StdDev   | Allocated |
|------- |---------:|---------:|---------:|----------:|
| Rank   | 17.79 ns | 0.046 ns | 0.043 ns |         - |

This benchmark measures the final file size after serializing.

- english.50MB

| Method                     | Serialized |
|--------------------------- |-----------:|
| SuffixArray                |  455.33 MB |
| FMIndex (no sampling)      |  467.38 MB |
| FMIndex (sampling rate 32) |   17.71 MB |
| LcpIndex                   |  556.21 MB |
| WaveletMatrix              |   27.44 MB |

- dna.50MB

| Method                     | Serialized |
|--------------------------- |-----------:|
| SuffixArray                |  417.51 MB |
| FMIndex (no sampling)      |  429.10 MB |
| FMIndex (sampling rate 32) |   17.25 MB |
| LcpIndex                   |  501.07 MB |
| WaveletMatrix              |   12.32 MB |

---

## License

This project is licensed under the MIT License. See the  **[LICENSE](https://github.com/belnytheraseiche/WaveletMatrix/blob/main/LICENSE)** file for details.

---

## Project Website

[Main Page - https://github.com/belnytheraseiche/WaveletMatrix/](https://github.com/belnytheraseiche/WaveletMatrix/)

[Reference - https://belnytheraseiche.github.io/WaveletMatrix/](https://belnytheraseiche.github.io/WaveletMatrix/)
