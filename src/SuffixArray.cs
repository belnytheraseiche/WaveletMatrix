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
//
// ---
//
// This file contains a C# implementation of the SA-IS algorithm.
// The logic is a port of the original C implementation found in sais-lite by Yuta Mori,
// but has been significantly adapted to utilize modern C# features and idioms
// rather than being a direct line-by-line translation.
//
// Original C source: https://github.com/davehughes/sais/blob/master/sais-lite/sais.c
//
// The original MIT license for the C source is preserved below and also applies
// to this C# derivative work.
//
// --- Original License ---
//
// Copyright (c) 2008-2010 Yuta Mori All Rights Reserved.
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

using System.Text;
using System.Collections;
using System.IO.Compression;
using System.IO.Hashing;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Buffers.Binary;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides a powerful string searching data structure using a Suffix Array and LCP Array.
/// </summary>
/// <remarks>
/// This class builds an index from a given string to perform various advanced searches with high performance.
/// Construction is based on the SA-IS algorithm for the suffix array and Kasai's algorithm for the LCP array.
/// While the initial construction can be resource-intensive, subsequent search operations are extremely fast.
/// </remarks>
public sealed class SuffixArray
{
    readonly ReadOnlyMemory<char> text_;
    readonly ReadOnlyMemory<int> sa_;
    readonly int[] lcp_;
    readonly int[] rank_;

    /// <summary>
    /// Gets the original text used to build the suffix array.
    /// </summary>
    public ReadOnlyMemory<char> Text => text_;

    /// <summary>
    /// Gets the suffix array. This is a sorted array of all suffixes of the text.
    /// </summary>
    /// <remarks>
    /// The 0-th element of the internal array is skipped as it corresponds to the sentinel character used by the SA-IS algorithm.
    /// </remarks>
    public ReadOnlyMemory<int> SA => sa_[1..];

    /// <summary>
    /// Gets the LCP (Longest Common Prefix) array.
    /// `Lcp[i]` stores the length of the longest common prefix between the suffixes starting at `SA[i-1]` and `SA[i]`.
    /// </summary>
    public ReadOnlyMemory<int> Lcp => lcp_.AsMemory();

    /// <summary>
    /// Gets the Rank array (also known as the Inverse Suffix Array).
    /// `Rank[i]` gives the rank (the index in the suffix array) of the suffix starting at position `i` of the original text.
    /// </summary>
    public ReadOnlyMemory<int> Rank => rank_.AsMemory();

    // 
    // 

    // Private constructor for deserialization.
    SuffixArray(Init init)
    {
        (text_, sa_, lcp_, rank_) = (init.Text, init.SA, init.Lcp, init.Rank);
    }

    /// <summary>
    /// Creates a new instance of the <see cref="SuffixArray"/> class by building the suffix and LCP arrays for the specified text.
    /// </summary>
    /// <param name="text">The input string to be indexed. The string will be held in memory.</param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text"/> is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="text"/> length exceeds the maximum supported length of 100,000,000 characters.</exception>
    public static SuffixArray Create(string text)
    => Create(text?.AsMemory() ?? throw new ArgumentNullException(nameof(text)));

    /// <summary>
    /// Creates a new instance of the <see cref="SuffixArray"/> class by building the suffix and LCP arrays for the specified text.
    /// </summary>
    /// <param name="text">The input string to be indexed. The string will be held in memory.</param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text"/> is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="text"/> length exceeds the maximum supported length of 100,000,000 characters.</exception>
    public static SuffixArray Create(ReadOnlyMemory<char> text)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfGreaterThan(text.Length, 100_000_000, nameof(text));
#else
        if (text.Length > 100_000_000)
            throw new ArgumentOutOfRangeException(nameof(text), $"{nameof(text)}.Length exceeds the maximum supported length of 100,000,000 characters.");
#endif

        var sa = CreateSA(text);
        var (lcp, rank) = CreateLCP(sa.Span[1..], text.Span);
        return new(new(text, sa, lcp, rank));
    }

    /// <summary>
    /// Serializes the <see cref="SuffixArray"/> instance into a byte array.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> is null.</exception>
    /// <returns>A byte array containing the serialized data.</returns>
    public static byte[] Serialize(SuffixArray obj, SerializationOptions? options = null)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(obj);
#else
        if (obj == null)
            throw new ArgumentNullException(nameof(obj));
#endif

        using var memoryStream = new MemoryStream();
        Serialize(obj, memoryStream, options);
        return memoryStream.ToArray();
    }

    /// <summary>
    /// Serializes the <see cref="SuffixArray"/> instance to the specified file.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="file">The path of the file to write to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="file"/> is null.</exception>
    public static void Serialize(SuffixArray obj, string file, SerializationOptions? options = null)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(obj);
        ArgumentNullException.ThrowIfNull(file);
#else
        if (obj == null)
            throw new ArgumentNullException(nameof(obj));
        if (file == null)
            throw new ArgumentNullException(nameof(file));
#endif

        using var fileStream = new FileStream(file, FileMode.Create, FileAccess.Write, FileShare.None);
        Serialize(obj, fileStream, options);
    }

    /// <summary>
    /// Serializes the <see cref="SuffixArray"/> instance to a stream.
    /// The data is compressed using Brotli and includes a checksum for integrity verification.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="stream">The stream to write the serialized data to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="stream"/> is null.</exception>
    public static void Serialize(SuffixArray obj, Stream stream, SerializationOptions? options = null)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(obj);
        ArgumentNullException.ThrowIfNull(stream);
#else
        if (obj == null)
            throw new ArgumentNullException(nameof(obj));
        if (stream == null)
            throw new ArgumentNullException(nameof(stream));
#endif

        options ??= SerializationOptions.Default;

        var firstPosition = stream.Position;
        var xxh = new XxHash32();
        Span<byte> buffer0 = stackalloc byte[64];
        stream.Write(buffer0);

        // text block
        var length1 = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                var buffer1 = new byte[4 + Encoding.UTF8.GetByteCount(obj.text_.Span)];
                Encoding.UTF8.GetBytes(obj.text_.Span, buffer1.AsSpan(4));
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                // count of elements
                BinaryPrimitives.WriteInt32LittleEndian(buffer1, buffer1.Length - 4);
                // body
                compressStream.Write(buffer1);
            }
            var array = memoryStream.ToArray();
            length1 = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }
        // sa block
        var length2 = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                // count of elements
                Span<byte> bytes = stackalloc byte[4];
                BinaryPrimitives.WriteInt32LittleEndian(bytes, obj.sa_.Length);
                compressStream.Write(bytes);
                // sa elements
                __WriteStreamFromInt32Memory(compressStream, obj.sa_);
            }
            var array = memoryStream.ToArray();
            length2 = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }
        // lcp block
        var length3 = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                // count of elements
                Span<byte> bytes = stackalloc byte[4];
                BinaryPrimitives.WriteInt32LittleEndian(bytes, obj.lcp_.Length);
                compressStream.Write(bytes);
                // lcp elements
                __WriteStreamFromInt32Memory(compressStream, obj.lcp_);
            }
            var array = memoryStream.ToArray();
            length3 = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }
        // rank block
        var length4 = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                // count of elements
                Span<byte> bytes = stackalloc byte[4];
                BinaryPrimitives.WriteInt32LittleEndian(bytes, obj.rank_.Length);
                compressStream.Write(bytes);
                // rank elements
                __WriteStreamFromInt32Memory(compressStream, obj.rank_);
            }
            var array = memoryStream.ToArray();
            length4 = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }

        var lastPosition = stream.Position;

        //  0: byte * 4, SFXA
        "SFXA"u8.CopyTo(buffer0);
        //  4: uint * 1, xxh
        BinaryPrimitives.WriteUInt32LittleEndian(buffer0[4..], xxh.GetCurrentHashAsUInt32());
        //  8: int * 1, size of text block
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[8..], length1);
        // 12: int * 1, size of sa block
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[12..], length2);
        // 16: int * 1, size of lcp block
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[16..], length3);
        // 20: int * 1, size of rank block
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[20..], length4);
        // 24- empty
        stream.Seek(firstPosition, SeekOrigin.Begin);
        stream.Write(buffer0);

        stream.Seek(lastPosition, SeekOrigin.Begin);

        #region @@
        static void __WriteStreamFromInt32Memory(Stream stream, ReadOnlyMemory<int> memory)
        {
            if (BitConverter.IsLittleEndian)
                stream.Write(MemoryMarshal.AsBytes(memory.Span));
            else
            {
                var buffer = new byte[4194304];
                var offset = 0;
                while (offset < memory.Length)
                {
                    var length = Math.Min(1048576, memory.Length - offset);
                    var span = memory.Slice(offset, length).Span;
                    for (var i = 0; i < span.Length; i++)
                        BinaryPrimitives.WriteInt32LittleEndian(buffer.AsSpan(i * 4, 4), span[i]);
                    stream.Write(buffer.AsSpan(0, 4 * length));
                    offset += length;
                }
            }
        }
        #endregion
    }

    /// <summary>
    /// Deserializes a byte array into a <see cref="SuffixArray"/> instance.
    /// </summary>
    /// <param name="data">The byte array containing the serialized data.</param>
    /// <returns>A new, deserialized instance of <see cref="SuffixArray"/>.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="data"/> is null.</exception>
    public static SuffixArray Deserialize(byte[] data)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(data);
#else
        if (data == null)
            throw new ArgumentNullException(nameof(data));
#endif

        using var memoryStream = new MemoryStream(data);
        return Deserialize(memoryStream);
    }

    /// <summary>
    /// Deserializes a <see cref="SuffixArray"/> instance from the specified file.
    /// </summary>
    /// <param name="file">The path of the file to read from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="file"/> is null.</exception>
    public static SuffixArray Deserialize(string file)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(file);
#else
        if (file == null)
            throw new ArgumentNullException(nameof(file));
#endif

        using var fileStream = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.Read);
        return Deserialize(fileStream);
    }

    /// <summary>
    /// Deserializes a <see cref="SuffixArray"/> instance from a stream.
    /// It verifies the file format, type identifier, and checksum.
    /// </summary>
    /// <param name="stream">The stream to read the serialized data from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="stream"/> is null.</exception>
    /// <exception cref="InvalidDataException">Thrown if the data format is unsupported, the type is incompatible, or the data is corrupt.</exception>
    public static SuffixArray Deserialize(Stream stream)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(stream);
#else
        if (stream == null)
            throw new ArgumentNullException(nameof(stream));
#endif

        var xxh = new XxHash32();

        Span<byte> buffer0 = stackalloc byte[64];
        stream.ReadExactly(buffer0);
        if (!buffer0[0..4].SequenceEqual("SFXA"u8))
            throw new InvalidDataException("Unsupported format.");

        var (text, sa, lcp, rank) = ("", (int[])[], (int[])[], (int[])[]);

        // text block
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[8..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> bytes = stackalloc byte[4];
            decompressStream.ReadExactly(bytes);
            var body = new byte[BinaryPrimitives.ReadInt32LittleEndian(bytes)];
            decompressStream.ReadExactly(body);
            text = Encoding.UTF8.GetString(body);
        }
        // sa block
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[12..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> bytes = stackalloc byte[4];
            decompressStream.ReadExactly(bytes);
            sa = new int[BinaryPrimitives.ReadInt32LittleEndian(bytes)];
            __ReadInt32ArrayFromStream(decompressStream, sa);
        }
        // lcp block
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[16..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> bytes = stackalloc byte[4];
            decompressStream.ReadExactly(bytes);
            lcp = new int[BinaryPrimitives.ReadInt32LittleEndian(bytes)];
            __ReadInt32ArrayFromStream(decompressStream, lcp);
        }
        // rank block
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[20..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> bytes = stackalloc byte[4];
            decompressStream.ReadExactly(bytes);
            rank = new int[BinaryPrimitives.ReadInt32LittleEndian(bytes)];
            __ReadInt32ArrayFromStream(decompressStream, rank);
        }

        if (xxh.GetCurrentHashAsUInt32() != BinaryPrimitives.ReadUInt32LittleEndian(buffer0[4..]))
            throw new InvalidDataException("Broken.");

        return new(new(text.AsMemory(), sa, lcp, rank));

        #region @@
        static void __ReadInt32ArrayFromStream(Stream stream, int[] buffer)
        {
            stream.ReadExactly(MemoryMarshal.AsBytes(buffer.AsSpan()));
            if (!BitConverter.IsLittleEndian)
                for (var i = 0; i < buffer.Length; i++)
                    buffer[i] = BinaryPrimitives.ReadInt32LittleEndian(MemoryMarshal.AsBytes(buffer.AsSpan(i)));
        }
        #endregion
    }

    /// <summary>
    /// Finds all starting positions of a specified substring. The search is case-sensitive.
    /// </summary>
    /// <param name="text">The substring to search for.</param>
    /// <returns>An enumerable collection of 0-based starting positions where the substring is found, sorted in ascending order.</returns>
    public IEnumerable<int> Search(ReadOnlySpan<char> text)
    => InternalSearch(text.ToString());

    IEnumerable<int> InternalSearch(string text)
    {
        if (text.Length != 0)
        {
            var lb = __Lower(text);
            if (lb != -1)
            {
                var result = new List<int>();
                {
                    var ub = __Upper(text);
                    var sa = sa_.Span[1..];
                    for (var i = lb; i <= ub; i++)
                        result.Add(sa[i]);
                }
                foreach (var m in result.OrderBy(n => n))
                    yield return m;
            }
        }

        #region @@
        int __Lower(ReadOnlySpan<char> sequence)
        {
            var span = text_.Span;
            var sa = sa_.Span[1..];
            var (left, right) = (0, sa.Length - 1);
            var found = -1;
            while (left <= right)
            {
                var mid = (left + right) / 2;
                var index = sa[mid];
                var cmp = __Compare(sequence, span, index);
                if (cmp == 0 && span.Length - index < sequence.Length)
                    cmp = 1;
                if (cmp == 0)
                    (found, right) = (mid, mid - 1);
                else if (cmp < 0)
                    right = mid - 1;
                else
                    left = mid + 1;
            }
            return found;
        }
        int __Upper(ReadOnlySpan<char> sequence)
        {
            var span = text_.Span;
            var sa = sa_.Span[1..];
            var (left, right) = (0, sa.Length - 1);
            var found = -1;
            while (left <= right)
            {
                var mid = (left + right) / 2;
                var index = sa[mid];
                var cmp = __Compare(sequence, span, index);
                if (cmp == 0 && span.Length - index < sequence.Length)
                    cmp = 1;
                if (cmp == 0)
                    (found, left) = (mid, mid + 1);
                else if (cmp < 0)
                    right = mid - 1;
                else
                    left = mid + 1;
            }
            return found;
        }
        static int __Compare(ReadOnlySpan<char> sequence, ReadOnlySpan<char> text, int index)
        {
            var length = Math.Min(sequence.Length, text.Length - index);
            for (var i = 0; i < length; i++)
                if (sequence[i] != text[i + index])
                    return sequence[i] - text[i + index];
            return 0;
        }
        #endregion
    }

    /// <summary>
    /// Finds the longest substring(s) that appear more than once in the text.
    /// </summary>
    /// <returns>An enumerable collection of <see cref="MatchRepeated"/> records, one for each longest repeated substring found.</returns>
    public IEnumerable<MatchRepeated> SearchLongestRepeated()
    {
        if (lcp_.Length == 0)
            yield break;

        var keys = new HashSet<string>(32);
        {
            var span = text_.Span;
            var sa = sa_.Span[1..];
            var lcpMax = lcp_.Max();
            for (var i = 1; i < lcp_.Length; i++)
            {
                if (lcp_[i] == lcpMax)
                {
                    var index = sa[i];
                    keys.Add(span[index..(index + lcpMax)].ToString());
                }
            }
        }
        foreach (var key in keys)
            yield return new MatchRepeated(key, Search(key).ToArray());
    }

    /// <summary>
    /// Finds all unique substrings that are repeated in the text and meet a minimum length requirement.
    /// </summary>
    /// <param name="minLength">The minimum length of the repeated substrings to find. Must be 2 or greater.</param>
    /// <returns>An enumerable collection of <see cref="MatchRepeated"/> records for each unique repeated substring found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="minLength"/> is less than 2.</exception>
    public IEnumerable<MatchRepeated> SearchRepeated(int minLength = 2)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfLessThan(minLength, 2);
#else
        if (minLength < 2)
            throw new ArgumentOutOfRangeException(nameof(minLength), $"{nameof(minLength)} must be greater than or equal 2.");
#endif

        var keys = new HashSet<string>(32);
        {
            var span = text_.Span;
            var sa = sa_.Span[1..];
            for (var i = 1; i < lcp_.Length; i++)
            {
                var commonLength = lcp_[i];
                if (commonLength >= minLength)
                {
                    for (var length = minLength; length <= commonLength; length++)
                    {
                        var index = sa[i];
                        keys.Add(span[index..(index + length)].ToString());
                    }
                }
            }
        }
        foreach (var key in keys)
            yield return new MatchRepeated(key, Search(key).ToArray());
    }

    /// <summary>
    /// Finds the longest common substring(s) between the text of this instance and another specified text.
    /// </summary>
    /// <param name="text">The other text to compare against.</param>
    /// <returns>An enumerable collection of <see cref="MatchGroup"/> records for each longest common substring found.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text"/> is null.</exception>
    public IEnumerable<MatchGroup> SearchLongestCommon(string text)
    => SearchLongestCommon(text?.AsMemory() ?? throw new ArgumentNullException(nameof(text)));

    public IEnumerable<MatchGroup> SearchLongestCommon(ReadOnlyMemory<char> text)
    {
        if (text.Length == 0 || text_.Length == 0)
            yield break;

        foreach (var m in InternalSearchLongestCommon(this, Create(text)))
            yield return m;
    }

    /// <summary>
    /// Finds the longest common substring(s) between two specified texts.
    /// </summary>
    /// <param name="text1">The first text.</param>
    /// <param name="text2">The second text.</param>
    /// <returns>An enumerable collection of <see cref="MatchGroup"/> records for each longest common substring found.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text1"/> or <paramref name="text2"/> is null.</exception>
    public static IEnumerable<MatchGroup> SearchLongestCommon(string text1, string text2)
    => SearchLongestCommon(text1?.AsMemory() ?? throw new ArgumentNullException(nameof(text1)), text2?.AsMemory() ?? throw new ArgumentNullException(nameof(text2)));

    public static IEnumerable<MatchGroup> SearchLongestCommon(ReadOnlyMemory<char> text1, ReadOnlyMemory<char> text2)
    {
        if (text1.Length == 0 || text2.Length == 0)
            yield break;

        foreach (var m in InternalSearchLongestCommon(Create(text1), Create(text2)))
            yield return m;
    }

    static IEnumerable<MatchGroup> InternalSearchLongestCommon(SuffixArray sa1, SuffixArray sa2)
    {
        char[] text0 = new char[sa1.text_.Length + sa2.text_.Length + 1];
        sa1.text_.CopyTo(text0.AsMemory());
        text0[sa1.text_.Length] = '\uFFFF';
        sa2.text_.CopyTo(text0.AsMemory(sa1.text_.Length + 1));
        var sa0 = Create(text0);

        var keys = new HashSet<string>(32);
        {
            var sa = sa0.sa_.Span[1..];
            var lcpMax = 0;
            for (var i = 1; i < sa0.lcp_.Length; i++)
            {
                var (isPrevious, isCurrent) = (sa[i - 1] < sa1.text_.Length, sa[i] < sa1.text_.Length);
                if (isPrevious != isCurrent)
                {
                    var lcpCurrent = sa0.lcp_[i];
                    if (lcpCurrent > lcpMax)
                    {
                        lcpMax = lcpCurrent;
                        keys.Clear();
                    }
                    if (lcpCurrent == lcpMax && lcpMax > 0)
                    {
                        var index = sa[i];
                        keys.Add(text0.AsSpan(index, lcpMax).ToString());
                    }
                }
            }
        }
        foreach (var key in keys)
            yield return new(key, sa1.Search(key).ToArray(), sa2.Search(key).ToArray());
    }

    /// <summary>
    /// Finds all common substrings between the text of this instance and another specified text that meet a minimum length requirement.
    /// </summary>
    /// <param name="text">The other text to compare against.</param>
    /// <param name="minLength">The minimum length of the common substrings to find. Must be 2 or greater.</param>
    /// <returns>An enumerable collection of <see cref="MatchGroup"/> records for each unique common substring found.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text"/> is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="minLength"/> is less than 2.</exception>
    public IEnumerable<MatchGroup> SearchCommon(string text, int minLength = 2)
    => SearchCommon(text?.AsMemory() ?? throw new ArgumentNullException(nameof(text)), minLength);

    public IEnumerable<MatchGroup> SearchCommon(ReadOnlyMemory<char> text, int minLength = 2)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfLessThan(minLength, 2);
#else
        if (minLength < 2)
            throw new ArgumentOutOfRangeException(nameof(minLength), $"{minLength} must be greater than or equal 2.");
#endif

        if (text.Length == 0 || text_.Length == 0)
            yield break;

        foreach (var m in InternalSearchCommon(this, Create(text), minLength))
            yield return m;
    }

    /// <summary>
    /// Finds all common substrings between two specified texts that meet a minimum length requirement.
    /// </summary>
    /// <param name="text1">The first text.</param>
    /// <param name="text2">The second text.</param>
    /// <param name="minLength">The minimum length of the common substrings to find. Must be 2 or greater.</param>
    /// <returns>An enumerable collection of <see cref="MatchGroup"/> records for each unique common substring found.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text1"/> or <paramref name="text2"/> is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="minLength"/> is less than 2.</exception>
    public static IEnumerable<MatchGroup> SearchCommon(string text1, string text2, int minLength = 2)
    => SearchCommon(text1?.AsMemory() ?? throw new ArgumentNullException(nameof(text1)), text2?.AsMemory() ?? throw new ArgumentNullException(nameof(text2)), minLength);

    public static IEnumerable<MatchGroup> SearchCommon(ReadOnlyMemory<char> text1, ReadOnlyMemory<char> text2, int minLength = 2)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfLessThan(minLength, 2);
#else
        if (minLength < 2)
            throw new ArgumentOutOfRangeException(nameof(minLength), $"{minLength} must be greater than or equal 2.");
#endif

        if (text1.Length == 0 || text2.Length == 0)
            yield break;

        foreach (var m in InternalSearchCommon(Create(text1), Create(text2), minLength))
            yield return m;
    }

    static IEnumerable<MatchGroup> InternalSearchCommon(SuffixArray sa1, SuffixArray sa2, int minLength)
    {
        var text0 = new char[sa1.text_.Length + sa2.text_.Length + 1];
        sa1.text_.CopyTo(text0.AsMemory());
        text0[sa1.text_.Length] = '\uFFFF';
        sa2.text_.CopyTo(text0.AsMemory(sa1.text_.Length + 1));
        var sa0 = Create(text0);

        var keys = new HashSet<string>(32);
        {
            var sa = sa0.sa_.Span[1..];
            for (var i = 1; i < sa0.lcp_.Length; i++)
            {
                var (isPrevious, isCurrent) = (sa[i - 1] < sa1.text_.Length, sa[i] < sa1.text_.Length);
                if (isPrevious != isCurrent)
                {
                    var commonLength = sa0.lcp_[i];
                    if (commonLength >= minLength)
                    {
                        for (var length = minLength; length <= commonLength; length++)
                        {
                            var index = sa[i];
                            keys.Add(text0.AsSpan(index, length).ToString()!);
                        }
                    }
                }
            }
        }
        foreach (var key in keys)
            yield return new(key, sa1.Search(key).ToArray(), sa2.Search(key).ToArray());
    }

    /// <summary>
    /// Finds all substrings that match a given pattern containing wildcards.
    /// </summary>
    /// <param name="expr">The search pattern. Use '*' for a variable-length wildcard and '?' for a single-character wildcard.</param>
    /// <param name="options">
    /// The options that customize the wildcard search behavior. If null, the default options specified by <see cref="WildcardOptions.Default"/> will be used.
    /// </param>
    /// <returns>An enumerable collection of <see cref="Match"/> records for all found occurrences.</returns>
    /// <remarks>
    /// The search is anchored by the non-wildcard parts of the pattern. Therefore, leading and trailing asterisks are trimmed as they do not provide additional constraints for the search and have no effect on the result.
    /// </remarks>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="expr"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <see name="WildcardOptions.Asterisk"/> and <see name="WildcardOptions.Question"/> are the same character.</exception>
    /// <exception cref="ArgumentException">Thrown if <see name="WildcardOptions.Asterisk"/> or <see name="WildcardOptions.Question"/> are found in <see name="WildcardOptions.StopCharacters"/>.</exception>
    /// <exception cref="ArgumentException">Thrown if <see name="WildcardOptions.AsteriskMinLength"/> is greater than <see name="WildcardOptions.AsteriskMaxLength"/>.</exception>
    public IEnumerable<Match> SearchWildcard(ReadOnlySpan<char> expr, WildcardOptions? options = null)
    {
        options ??= WildcardOptions.Default;
        if (options.Asterisk == options.Question)
            throw new ArgumentException($"{nameof(options.Asterisk)} must be different {nameof(options.Question)}.");
        if (options.AsteriskMinLength > options.AsteriskMaxLength)
            throw new ArgumentException($"{nameof(options.AsteriskMinLength)} must be less than or equal {nameof(options.AsteriskMaxLength)}.");
        if (options.StopCharacters?.Any(n => n == options.Asterisk || n == options.Question) ?? false)
            throw new ArgumentException($"{nameof(options.StopCharacters)} does not allowed to contain {nameof(options.Asterisk)} and {nameof(options.Question)}.");

        // note: trim *
        var trim = expr.ToString().Trim(options.Asterisk);
        return trim.Length != 0 ? InternalSearchWildcard(trim, options) : [];
    }

    IEnumerable<Match> InternalSearchWildcard(string expr, WildcardOptions options)
    {
        var stops = options.StopCharacters ?? [];

        var exprParts = expr.Split(options.Asterisk).Where(n => n is not "").ToArray();
        var initialPositions = __FindFirst(exprParts[0], stops);
        var result = new HashSet<(int, int)>();
        foreach (var initialPosition in initialPositions)
        {
            var (startPosition, currentPosition) = (initialPosition, initialPosition + exprParts[0].Length);
            var isMatch = true;
            for (var i = 1; i < exprParts.Length; i++)
            {
                var found = __FindPart(exprParts[i], currentPosition, stops);
                if (found == -1)
                {
                    isMatch = false;
                    break;
                }
                currentPosition = found + exprParts[i].Length;
            }
            if (isMatch)
                result.Add((startPosition, currentPosition - startPosition));
        }
        foreach (var r in result.OrderBy(n => n))
            yield return new(r.Item1, r.Item2);

        #region @@
        int[] __FindFirst(string expr, char[] stops)
        {
            var exprParts = expr.Split(options.Question);
            if (exprParts.Length == 1)
                return Search(expr).ToArray();

            var span = text_.Span;
            var anchorPart = exprParts.FirstOrDefault(n => n is not "");
            if (anchorPart == null)
                return Enumerable.Range(0, text_.Length - expr.Length + 1).ToArray();

            var indexSub = expr.IndexOf(anchorPart);
            var result = new HashSet<int>(32);
            foreach (var initialPosition in Search(anchorPart))
            {
                var potentialStart = initialPosition - indexSub;
                if (potentialStart < 0 || potentialStart + expr.Length > text_.Length)
                    continue;

                var isMatch = true;
                for (var i = 0; i < expr.Length; i++)
                {
                    if ((expr[i] != options.Question && expr[i] != span[potentialStart + i]) || stops.Contains(span[potentialStart + i]))
                    {
                        isMatch = false;
                        break;
                    }
                }
                if (isMatch)
                    result.Add(potentialStart);
            }
            return result.OrderBy(n => n).ToArray();
        }
        int __FindPart(string expr, int startPosition, char[] stops)
        {
            var span = text_.Span;
            var stopPosition = (int)Math.Min(text_.Length - expr.Length, startPosition + (long)options.AsteriskMaxLength);
            for (var i = startPosition; i <= stopPosition; i++)
            {
                if (stops.Contains(span[i]))
                    break;
                if (i - startPosition < options.AsteriskMinLength)
                    continue;

                var isMatch = true;
                for (var j = 0; j < expr.Length; j++)
                {
                    if (i + j  >= span.Length || stops.Contains(span[i + j]) || (expr[j] != options.Question && expr[j] != span[i + j]))
                    {
                        isMatch = false;
                        break;
                    }
                }
                if (isMatch)
                    return i;
            }
            return -1;
        }
        #endregion
    }

    static (int[], int[]) CreateLCP(ReadOnlySpan<int> sa, ReadOnlySpan<char> text)
    {
        var lcp = new int[text.Length];
        var rank = new int[text.Length];

        for (var i = 0; i < text.Length; i++)
            rank[sa[i]] = i;

        var d = 0;
        for (var i = 0; i < text.Length; i++)
        {
            if (rank[i] > 0)
            {
                var j = sa[rank[i] - 1];
                if (d > 0)
                    d--;
                while (i + d < text.Length && j + d < text.Length && text[i + d] == text[j + d])
                    d++;
                lcp[rank[i]] = d;
            }
        }

        return (lcp, rank);
    }

    static Memory<int> CreateSA(ReadOnlyMemory<char> text)
    {
        if (text.Length == 0)
            return new([0]);

        var span = text.Span;
        var codes = new int[text.Length];
        for (var i = 0; i < text.Length; i++)
            codes[i] = span[i];

        return __SAIS(codes, codes.Max() + 1);

        #region @@
        static Memory<int> __SAIS(int[] codes, int codeRange)
        {
            var sentinels = new int[codes.Length + 1];
            for (var i = 0; i < codes.Length; i++)
                sentinels[i] = codes[i] + 1;
            codeRange++;

            var typeS = new BitArray(sentinels.Length);
            typeS[^1] = true;
            for (var i = typeS.Length - 2; i >= 0; i--)
                typeS[i] = (sentinels[i] < sentinels[i + 1]) || (sentinels[i] == sentinels[i + 1] && typeS[i + 1]);

            var bucketEnds = new int[codeRange];
            foreach (var sentinel in sentinels)
                bucketEnds[sentinel]++;
            for (var i = 1; i < codeRange; i++)
                bucketEnds[i] += bucketEnds[i - 1];

            var sa = new int[sentinels.Length];
            Array.Fill(sa, -1);

            var lmsIndices = new List<int>(sentinels.Length / 2);
            {
                var bucketTmp = (int[])[.. bucketEnds];
                for (var i = 1; i < sentinels.Length; i++)
                {
                    if (__IsLMS(typeS, i))
                    {
                        lmsIndices.Add(i);
                        sa[--bucketTmp[sentinels[i]]] = i;
                    }
                }
            }

            var bucketStarts = new int[codeRange];
            for (var i = 1; i < codeRange; i++)
                bucketStarts[i] = bucketEnds[i - 1];

            {
                var bucketTmp = (int[])[.. bucketStarts];
                for (var i = 0; i < sentinels.Length; i++)
                    if (sa[i] > 0 && !typeS[sa[i] - 1])
                        sa[bucketTmp[sentinels[sa[i] - 1]]++] = sa[i] - 1;
            }
            {
                var bucketTmp = (int[])[.. bucketEnds];
                for (var i = sentinels.Length - 1; i >= 0; i--)
                    if (sa[i] > 0 && typeS[sa[i] - 1])
                        sa[--bucketTmp[sentinels[sa[i] - 1]]] = sa[i] - 1;
            }

            var lmsSorted = __Sorted(typeS, sa, lmsIndices.Count); //sa.Where(n => n != -1 && __IsLMS(typeS, n)).ToArray();
            var summaryRanks = new int[sentinels.Length];
            Array.Fill(summaryRanks, -1);
            summaryRanks[lmsSorted[0]] = 0;
            var rank = 0;
            for (var i = 1; i < lmsSorted.Length; i++)
            {
                var lmsPrevious = lmsSorted[i - 1];
                var lmsCurrent = lmsSorted[i];
                var different = false;
                for (var d = 0; d < sentinels.Length; d++)
                {
                    if (sentinels[lmsPrevious + d] != sentinels[lmsCurrent + d] || typeS[lmsPrevious + d] != typeS[lmsCurrent + d])
                    {
                        different = true;
                        break;
                    }
                    if (d > 0 && (__IsLMS(typeS, lmsPrevious + d) || __IsLMS(typeS, lmsCurrent + d)))
                        break;
                }
                if (different)
                    rank++;
                summaryRanks[lmsCurrent] = rank;
            }

            // var summary = lmsIndices.Select(n => summaryRanks[n]).ToArray();
            var summary = new int[lmsIndices.Count];
            for (var i = 0; i < summary.Length; i++)
                summary[i] = summaryRanks[lmsIndices[i]];
            Memory<int> memorySA = rank + 1 < lmsIndices.Count ? __SAIS(summary, rank + 1) : new int[1 + lmsIndices.Count];
            var summarySA = memorySA.Span[1..];
            if (rank + 1 >= lmsIndices.Count)
                for (var i = 0; i < summary.Length; i++)
                    summarySA[summary[i]] = i;

            var lmsFinal = new int[summarySA.Length];
            for (var i = 0; i < lmsFinal.Length; i++)
                lmsFinal[i] = lmsIndices[summarySA[i]];

            Array.Fill(sa, -1);
            {
                var bucketTmp = (int[])[.. bucketEnds];
                for (var i = lmsFinal.Length - 1; i >= 0; i--)
                    sa[--bucketTmp[sentinels[lmsFinal[i]]]] = lmsFinal[i];
            }
            {
                var bucketTmp = (int[])[.. bucketStarts];
                for (var i = 0; i < sentinels.Length; i++)
                    if (sa[i] > 0 && !typeS[sa[i] - 1])
                        sa[bucketTmp[sentinels[sa[i] - 1]]++] = sa[i] - 1;
            }
            {
                var bucketTmp = (int[])[.. bucketEnds];
                for (var i = sentinels.Length - 1; i >= 0; i--)
                    if (sa[i] > 0 && typeS[sa[i] - 1])
                        sa[--bucketTmp[sentinels[sa[i] - 1]]] = sa[i] - 1;
            }

            return sa;

            #region @@
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            static bool __IsLMS(BitArray bits, int index)
            => index > 0 && bits[index] && !bits[index - 1];
            static int[] __Sorted(BitArray bits, int[] sa, int capacity)
            {
                var result = new List<int>(capacity);
                foreach (var n in sa)
                    if (n != -1 && __IsLMS(bits, n))
                        result.Add(n);
                return [.. result];
            }
            #endregion
        }
        #endregion
    }

    // 
    // 

    /// <summary>
    /// Provides a single match found in a text.
    /// </summary>
    /// <param name="Position">The 0-based starting position of the match.</param>
    /// <param name="Length">The length of the match.</param>
    public record struct Match(int Position, int Length);

    /// <summary>
    /// Provides a repeated substring and all its occurrences.
    /// </summary>
    /// <param name="Text">The repeated substring.</param>
    /// <param name="Positions">An array of all 0-based starting positions where the substring occurs.</param>
    public record struct MatchRepeated(string Text, int[] Positions);

    /// <summary>
    /// Provides a common substring found between two texts.
    /// </summary>
    /// <param name="Text">The common substring.</param>
    /// <param name="Positions1">An array of all 0-based starting positions in the first text.</param>
    /// <param name="Positions2">An array of all 0-based starting positions in the second text.</param>
    public record struct MatchGroup(string Text, int[] Positions1, int[] Positions2);

    /// <summary>
    /// Provides options to customize the behavior of a wildcard search.
    /// </summary>
    public sealed class WildcardOptions
    {

        /// <summary>
        /// The character to be treated as a variable-length wildcard (*).
        /// </summary>
        public char Asterisk { get; set; } = '*';

        /// <summary>
        /// The character to be treated as a single-character wildcard (?).
        /// </summary>
        public char Question { get; set; } = '?';

        /// <summary>
        /// Gets or sets the minimum number of characters the wildcard can match. Defaults to 0.
        /// </summary>
        public int AsteriskMinLength { get; set; } = 0;

        /// <summary>
        /// Gets or sets the maximum number of characters the wildcard can match. Defaults to Int32.MaxValue.
        /// </summary>
        public int AsteriskMaxLength { get; set; } = Int32.MaxValue;

        /// <summary>
        /// Gets or sets a set of characters that will stop the match.
        /// If the match encounters any of these characters, it will end.
        /// </summary>
        public char[]? StopCharacters { get; set; }

        /// <summary>
        /// Gets a singleton instance of <see cref="WildcardOptions"/> with default values.
        /// </summary>
        public static WildcardOptions Default { get; } = new();
    }

    record Init(ReadOnlyMemory<char> Text, ReadOnlyMemory<int> SA, int[] Lcp, int[] Rank);
}
