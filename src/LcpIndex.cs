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

using System.IO.Hashing;
using System.Buffers.Binary;
using System.IO.Compression;
using System.Runtime.InteropServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides an index structure over a Suffix Array's LCP (Longest Common Prefix) array
/// to answer advanced stringology queries efficiently.
/// </summary>
/// <remarks>
/// This class builds a <see cref="SparseTable{T}"/> on the LCP array to enable O(1) LCP queries
/// between any two suffixes. It is a powerful tool for complex string analysis, such as finding
/// repeated substrings or calculating string complexity.
/// </remarks>
public sealed class LcpIndex
{
    readonly SuffixArray sa_;
    readonly SparseTable<int> lcpRmq_;

    /// <summary>
    /// Gets the original source text in Suffix Array.
    /// </summary>
    public ReadOnlyMemory<char> Text => sa_.Text;

    /// <exclude />
    public Init InnerData => new(sa_, lcpRmq_);

    // 
    // 

    /// <exclude />
    public LcpIndex(Init init)
    {
        (sa_, lcpRmq_) = init;
    }

    /// <summary>
    /// Creates a new instance of the <see cref="LcpIndex"/> for a given <see cref="SuffixArray"/>.
    /// </summary>
    /// <param name="sa">The Suffix Array to build the LCP index upon.</param>
    /// <returns>A new, fully initialized <see cref="LcpIndex"/> instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="sa"/> is null.</exception>
    public static LcpIndex Create(SuffixArray sa)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(sa);
#else
        if (sa == null)
            throw new ArgumentNullException(nameof(sa));
#endif

        return new(new(sa, new(sa.Lcp.Span, (a, b) => a <= b)));
    }

    /// <summary>
    /// Serializes the <see cref="LcpIndex"/> instance into a byte array.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> is null.</exception>
    /// <returns>A byte array containing the serialized data.</returns>
    public static byte[] Serialize(LcpIndex obj, SerializationOptions? options = null)
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
    /// Serializes the <see cref="LcpIndex"/> instance to the specified file.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="file">The path of the file to write to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="file"/> is null.</exception>
    public static void Serialize(LcpIndex obj, string file, SerializationOptions? options = null)
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
    /// Serializes the <see cref="LcpIndex"/> instance to a stream.
    /// This method saves the pre-computed Sparse Table and the associated Suffix Array.
    /// The data is compressed using Brotli and includes a checksum for integrity verification.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="stream">The stream to write the serialized data to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="stream"/> is null.</exception>
    public static void Serialize(LcpIndex obj, Stream stream, SerializationOptions? options = null)
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

        // lcpRmp
        var lcpRmpSize = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                var (table, n) = obj.lcpRmq_.InnerData;
                Span<byte> buffer1 = stackalloc byte[8];
                // n
                BinaryPrimitives.WriteInt32LittleEndian(buffer1, n);
                // length of table
                BinaryPrimitives.WriteInt32LittleEndian(buffer1[4..], table.Length);
                compressStream.Write(buffer1);
                // table
                __WriteTable(compressStream, table);
            }
            var array = memoryStream.ToArray();
            lcpRmpSize = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }

        var lastPosition = stream.Position;

        //  0: byte * 4, "LINS"
        "LINS"u8.CopyTo(buffer0);
        //  4: uint * 1, xxh
        BinaryPrimitives.WriteUInt32LittleEndian(buffer0[4..], xxh.GetCurrentHashAsUInt32());
        //  8: int * 1, size of lcpRmq
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[8..], lcpRmpSize);
        //  12- empty
        stream.Seek(firstPosition, SeekOrigin.Begin);
        stream.Write(buffer0);

        stream.Seek(lastPosition, SeekOrigin.Begin);

        // sa
        SuffixArray.Serialize(obj.sa_, stream, options);

        #region @@
        static void __WriteTable(Stream stream, SparseTable<int>.ValueWithIndex[] table)
        {
            foreach (var buffer in __EnumeratePacked(table, new ulong[1048576]))
                __WriteStreamFromUInt64Memory(stream, buffer);

            #region @@
            static IEnumerable<ulong[]> __EnumeratePacked(SparseTable<int>.ValueWithIndex[] table, ulong[] buffer)
            {
                var count = 0;
                foreach (var entry in table)
                {
                    buffer[count++] = ((ulong)entry.Index << 32) | (uint)entry.Value;
                    if (count == buffer.Length)
                    {
                        yield return buffer;
                        count = 0;
                    }
                }

                if (count != 0)
                    yield return count == buffer.Length ? buffer : buffer[..count];
            }
            #endregion
        }
        static void __WriteStreamFromUInt64Memory(Stream stream, ReadOnlyMemory<ulong> memory)
        {
            if (BitConverter.IsLittleEndian)
                stream.Write(MemoryMarshal.AsBytes(memory.Span));
            else
            {
                var buffer = new byte[8 * memory.Length];
                var span = memory.Span;
                for (var i = 0; i < span.Length; i++)
                    BinaryPrimitives.WriteUInt64LittleEndian(buffer.AsSpan(i * 8, 8), span[i]);
                stream.Write(buffer);
            }
        }
        #endregion
    }

    /// <summary>
    /// Deserializes a byte array into a <see cref="LcpIndex"/> instance.
    /// </summary>
    /// <param name="data">The byte array containing the serialized data.</param>
    /// <returns>A new, deserialized instance of <see cref="LcpIndex"/>.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="data"/> is null.</exception>
    public static LcpIndex Deserialize(byte[] data)
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
    /// Deserializes a <see cref="LcpIndex"/> instance from the specified file.
    /// </summary>
    /// <param name="file">The path of the file to read from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="file"/> is null.</exception>
    public static LcpIndex Deserialize(string file)
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
    /// Deserializes a <see cref="LcpIndex"/> instance from a stream.
    /// It verifies the file format and checksum, and reconstructs the internal Sparse Table and Suffix Array.
    /// </summary>
    /// <param name="stream">The stream to read the serialized data from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="stream"/> is null.</exception>
    /// <exception cref="InvalidDataException">Thrown if the data format is unsupported, the type is incompatible, or the data is corrupt.</exception>
    public static LcpIndex Deserialize(Stream stream)
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
        if (!buffer0[0..4].SequenceEqual("LINS"u8))
            throw new InvalidDataException("Unsupported format.");

        // lcpRmp
        SparseTable<int> lcpRmp = null!;
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[8..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> buffer2 = stackalloc byte[8];
            decompressStream.ReadExactly(buffer2);
            var (n, length) = (BinaryPrimitives.ReadInt32LittleEndian(buffer2), BinaryPrimitives.ReadInt32LittleEndian(buffer2[4..]));
            var table = new SparseTable<int>.ValueWithIndex[length];
            __ReadTableFromStream(decompressStream, table);
            lcpRmp = new SparseTable<int>(new SparseTable<int>.Init(table, n), (a, b) => a <= b);
        }

        if (xxh.GetCurrentHashAsUInt32() != BinaryPrimitives.ReadUInt32LittleEndian(buffer0[4..]))
            throw new InvalidDataException("Broken.");

        // sa
        var sa = SuffixArray.Deserialize(stream);
        return new(new(sa, lcpRmp));

        #region @@
        static void __ReadTableFromStream(Stream stream, SparseTable<int>.ValueWithIndex[] table)
        {
            var buffer = new ulong[1048576];
            var offset = 0;
            while (offset < table.Length)
            {
                var length = Math.Min(table.Length - offset, buffer.Length);
                stream.ReadExactly(MemoryMarshal.AsBytes(buffer.AsSpan(0, length)));
                if (!BitConverter.IsLittleEndian)
                    for (var i = 0; i < length; i++)
                        buffer[i] = BinaryPrimitives.ReadUInt64LittleEndian(MemoryMarshal.AsBytes(buffer.AsSpan(i)));

                for (var i = 0; i < length; i++)
                    table[offset + i] = new((int)(uint)buffer[i], (int)(buffer[i] >> 32));

                offset += length;
            }
        }
        #endregion
    }

    /// <summary>
    /// Gets the length of the Longest Common Prefix (LCP) between the suffixes starting at <paramref name="index1"/> and <paramref name="index2"/>.
    /// This operation is performed in O(1) time.
    /// </summary>
    /// <param name="index1">The zero-based starting position of the first suffix in the original text.</param>
    /// <param name="index2">The zero-based starting position of the second suffix in the original text.</param>
    /// <returns>The length of the longest common prefix.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="index1"/> or <paramref name="index2"/> are out of the valid range.</exception>
    public int GetLcp(int index1, int index2)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(index1);
        ArgumentOutOfRangeException.ThrowIfNegative(index2);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(index1, sa_.Rank.Length);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(index2, sa_.Rank.Length);
#else
        if (index1 < 0)
            throw new ArgumentOutOfRangeException(nameof(index1), $"{nameof(index1)} must be greater than or equal 0.");
        if (index2 < 0)
            throw new ArgumentOutOfRangeException(nameof(index1), $"{nameof(index2)} must be greater than or equal 0.");
        if (index1 >= sa_.Rank.Length)
            throw new ArgumentOutOfRangeException(nameof(index1), $"{nameof(index1)} must be less than {sa_.Rank.Length}.");
        if (index2 >= sa_.Rank.Length)
            throw new ArgumentOutOfRangeException(nameof(index2), $"{nameof(index2)} must be less than {sa_.Rank.Length}.");
#endif

        if (index1 == index2)
            return sa_.Text.Length - index1;

        var rankSpan = sa_.Rank.Span;
        var (rank1, rank2) = (rankSpan[index1], rankSpan[index2]);
        if (rank1 > rank2)
            (rank1, rank2) = (rank2, rank1);
        return lcpRmq_.Query(rank1 + 1, rank2 + 1).Value;
    }

    /// <summary>
    /// Counts the total number of unique substrings within the text.
    /// This is calculated in O(N) time using the suffix and LCP arrays.
    /// </summary>
    /// <returns>The total count of unique substrings.</returns>
    public long CountUniqueSubstrings()
    {
        var n = sa_.Text.Length;
        var total = n * (n + 1L) / 2L;
        var lcpSum = 0L;
        var lcpSpan = sa_.Lcp.Span;
        for (var i = 0; i < lcpSpan.Length; i++)
            lcpSum += lcpSpan[i];
        return total - lcpSum;
    }

    /// <summary>
    /// Finds all maximal tandem repeats ( substrings of the form `s`s`s`... ) within the text.
    /// </summary>
    /// <returns>An enumerable collection of <see cref="TandemRepeat"/> records, ordered by position and length.</returns>
    public IEnumerable<TandemRepeat> FindTandemRepeats()
    {
        var length = sa_.Text.Length;
        var found = new HashSet<TandemRepeat>();
        for (var d = 1; d <= length / 2; d++)
        {
            var i = 0;
            while (i <= length - 2 * d)
            {
                if (GetLcp(i, i + d) >= d)
                {
                    var count = 2;
                    while (i + (count * d) < length && GetLcp(i, i + (count * d)) >= d)
                        count++;
                    found.Add(new(i, d, count));
                    i += (count - 1) * d;
                }
                i++;
            }
        }

        return found.GroupBy(n => n.Position).Select(g => g.OrderByDescending(n => n.Length * n.Count).First()).OrderBy(n => n.Position).ThenBy(n => n.Length);
    }

    /// <summary>
    /// Finds all unique substrings that are repeated anywhere in the text (adjacent or non-adjacent)
    /// and meet a minimum length requirement.
    /// </summary>
    /// <param name="minLength">The minimum length of the repeated substrings to find.</param>
    /// <returns>An enumerable collection of <see cref="Repeat"/> records.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="minLength"/> is negative or 0.</exception>
    public IEnumerable<Repeat> FindRepeats(int minLength = 2)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(minLength);
#else
        if (minLength <= 0)
            throw new ArgumentOutOfRangeException(nameof(minLength));
#endif

        var saSpan = sa_.SA.Span;
        var lcpSpan = sa_.Lcp.Span;
        var textSpan = sa_.Text.Span;
        var results = new Dictionary<string, List<int>>();

        for (var i = 1; i < textSpan.Length; i++)
        {
            var lcpLength = lcpSpan[i];
            if (lcpLength >= minLength)
            {
                for (var j = minLength; j <= lcpLength; j++)
                {
                    var key = textSpan.Slice(saSpan[i], j).ToString();
                    if (!results.TryGetValue(key, out var positions))
                        results.Add(key, positions = new());
                    positions.AddRange([saSpan[i - 1], saSpan[i]]);
                }
            }
        }

        foreach (var entry in results)
            yield return new(entry.Key, entry.Value.Distinct().Order().ToArray());
    }

    /// <summary>
    /// Calculates the Ziv-Lempel 78 (LZ78) complexity of the text.
    /// This value represents the number of phrases in the LZ78 parsing of the string, indicating its compressibility.
    /// </summary>
    /// <returns>The LZ78 complexity of the text.</returns>
    public int CalculateZivLempelComplexity()
    {
        if (sa_.Text.Length == 0)
            return 0;

        var found = new HashSet<ReadOnlyMemory<char>>(CharMemoryComparer.Instance);
        var textMemory = sa_.Text;
        var complexity = 0;
        var index = 0;
        while (index < textMemory.Length)
        {
            var length = 1;
            while (index + length <= textMemory.Length)
            {
                var key = textMemory.Slice(index, length);
                if (found.Add(key))
                {
                    complexity++;
                    break;
                }
                length++;
            }
            index += length;
        }

        return complexity;
    }

    // 
    // 

    class CharMemoryComparer : IEqualityComparer<ReadOnlyMemory<char>>
    {
        public static CharMemoryComparer Instance { get; } = new();

        public bool Equals(ReadOnlyMemory<char> x, ReadOnlyMemory<char> y)
        => x.Span.SequenceEqual(y.Span);

        public int GetHashCode(ReadOnlyMemory<char> obj)
        {
            var code = 17;
            foreach (var n in obj.Span)
                code = unchecked(code * 31 + n.GetHashCode());
            return code;
        }
    }

    /// <summary>
    /// Represents a tandem repeat found in the text.
    /// </summary>
    /// <param name="Position">The 0-based starting position of the entire repeat block.</param>
    /// <param name="Length">The length of the repeating unit string.</param>
    /// <param name="Count">The number of times the unit repeats (will be 2 or more).</param>
    public record TandemRepeat(int Position, int Length, int Count);

    /// <summary>
    /// Represents a epeated substring.
    /// </summary>
    /// <param name="Text">The repeating unit string.</param>
    /// <param name="Positions">An array of all 0-based starting positions where the substring occurs.</param>
    public record Repeat(string Text, int[] Positions);

    /// <exclude />
    public record Init(SuffixArray SA, SparseTable<int> LcpRmp);
}
