# API Reference

Welcome to the API documentation for the **BelNytheraSeiche.WaveletMatrix** library. This library is organized into several key components that work together to provide powerful sequence and text analysis capabilities.

Below is an overview of the main classes and their roles.

---

## Core Data Structures

These are the fundamental building blocks of the library.

- ### [WaveletMatrixGeneric<T>](WaveletMatrix.WaveletMatrixGeneric-1.yml)
  The main generic class for creating a Wavelet Matrix from any `IComparable<T>` sequence. It handles coordinate compression and provides a rich set of query APIs.

- ### [WaveletMatrixCore](WaveletMatrix.WaveletMatrixCore.yml)
  The high-performance, non-generic engine that powers the `WaveletMatrixGeneric<T>`. It operates directly on integer sequences.

- ### [SparseTable<T>](WaveletMatrix.SparseTable-1.yml) and [AggregateSparseTable<T>](WaveletMatrix.AggregateSparseTable-1.yml)
  Helper data structures used for answering Range Minimum/Maximum Queries (RMQ) in O(1) or O(log N) time, respectively. They are used internally by `LcpIndex`.

---

## Text Analysis Components

These classes are specialized for advanced stringology and full-text search.

- ### [SuffixArray](WaveletMatrix.SuffixArray.yml)
  The foundational class for most text analysis. It builds a Suffix Array and LCP Array from a given text, enabling fast substring searches.

- ### [LcpIndex](WaveletMatrix.LcpIndex.yml)
  An index built on top of a `SuffixArray` that provides advanced O(1) LCP queries. This is the key to complex analyses like finding tandem repeats or calculating string complexity.

- ### [FMIndex](WaveletMatrix.FMIndex.yml)
  The high-level, all-in-one full-text search index. It combines the power of the `WaveletMatrix`, `SuffixArray`, and Burrows-Wheeler Transform to offer extremely fast pattern counting (`Count`) and locating (`Locate`).

- ### [BurrowsWheelerTransform](WaveletMatrix.BurrowsWheelerTransform.yml)
  A static utility class that performs the Burrows-Wheeler Transform, a key step in building the `FMIndex`.
