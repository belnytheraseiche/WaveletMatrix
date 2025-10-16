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
using System.Buffers;
using System.Buffers.Binary;
using System.IO.Compression;
using System.Runtime.InteropServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides an index structure over a Suffix Array's LCP (Longest Common Prefix) array
/// to answer advanced stringology queries efficiently.
/// </summary>
/// <remarks>
/// This class builds a <see cref="FischerHeunSparseTable{T}"/> on the LCP array to enable O(1) LCP queries
/// between any two suffixes after an O(N) preprocessing step. It is a powerful tool for complex string analysis, 
/// such as finding repeated substrings or calculating string complexity.
/// </remarks>
public sealed class LcpIndex
{
    static readonly ArrayPool<int> intPool_ = ArrayPool<int>.Shared;
    static readonly ArrayPool<byte> bytePool_ = ArrayPool<byte>.Shared;

    readonly SuffixArray sa_;
    readonly FischerHeunSparseTable<int> lcpRmq_;

    /// <summary>
    /// Gets the original source Suffix Array.
    /// </summary>
    public SuffixArray SA => sa_;

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

        return new(new(sa, new(sa.Lcp, (a, b) => a <= b)));
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
                var init = obj.lcpRmq_.InnerData;
                var (table, n) = init.StInit;
                Span<byte> buffer1 = stackalloc byte[20];
                // n
                BinaryPrimitives.WriteInt32LittleEndian(buffer1, n);
                // length of table
                BinaryPrimitives.WriteInt32LittleEndian(buffer1[4..], table.Length);
                compressStream.Write(buffer1[..8]);
                // table
                __WriteTable(compressStream, table);
                // blockSize
                BinaryPrimitives.WriteInt32LittleEndian(buffer1, init.BlockSize);
                // blockCount
                BinaryPrimitives.WriteInt32LittleEndian(buffer1[4..], init.BlockCount);
                // length of blockMins
                BinaryPrimitives.WriteInt32LittleEndian(buffer1[8..], init.BlockMins.Length);
                // length of blockTypes
                BinaryPrimitives.WriteInt32LittleEndian(buffer1[12..], init.BlockTypes.Length);
                // length of patternRmq
                BinaryPrimitives.WriteInt32LittleEndian(buffer1[16..], init.PatternRmq.Count);
                compressStream.Write(buffer1);
                // blockMins
                __WriteStreamFromInt32Memory(compressStream, init.BlockMins);
                // blockTypes
                __WriteStreamFromUInt64Memory(compressStream, init.BlockTypes);
                // PatternRmq
                __WritePatternRmq(compressStream, init.PatternRmq);
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
        static void __WritePatternRmq(Stream stream, Dictionary<ulong, int[,]> table)
        {
            Span<byte> buffer1 = stackalloc byte[8];
            foreach (var (key, value) in table)
            {
                var dimensionLength = (int)(key >> 32);
                BinaryPrimitives.WriteUInt64LittleEndian(buffer1, key);
                stream.Write(buffer1);
                if (dimensionLength != 0)
                {
                    var rent = intPool_.Rent(dimensionLength * dimensionLength);
                    try
                    {
                        var memory = rent.AsMemory(0, dimensionLength * dimensionLength);
                        MemoryMarshal.CreateReadOnlySpan<int>(ref value[0, 0], value.Length).CopyTo(memory.Span);
                        __WriteStreamFromInt32Memory(stream, memory);
                    }
                    finally
                    {
                        intPool_.Return(rent);
                    }
                }
            }
        }
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
        static void __WriteStreamFromUInt64Memory(Stream stream, ReadOnlyMemory<ulong> memory)
        {
            if (BitConverter.IsLittleEndian)
                stream.Write(MemoryMarshal.AsBytes(memory.Span));
            else
            {
                var buffer = new byte[8388608];
                var offset = 0;
                while (offset < memory.Length)
                {
                    var length = Math.Min(1048576, memory.Length - offset);
                    var span = memory.Slice(offset, length).Span;
                    for (var i = 0; i < span.Length; i++)
                        BinaryPrimitives.WriteUInt64LittleEndian(buffer.AsSpan(i * 8, 8), span[i]);
                    stream.Write(buffer.AsSpan(0, 8 * length));
                    offset += length;
                }
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
        FischerHeunSparseTable<int>.Init lcpRmqInit = null!;
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[8..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> buffer2 = stackalloc byte[20];
            decompressStream.ReadExactly(buffer2[..8]);
            var (n, length) = (BinaryPrimitives.ReadInt32LittleEndian(buffer2), BinaryPrimitives.ReadInt32LittleEndian(buffer2[4..]));
            var table = new SparseTable<int>.ValueWithIndex[length];
            __ReadTableFromStream(decompressStream, table);
            decompressStream.ReadExactly(buffer2);
            var (blockSize, blockCount, blockMinsLength, blockTypesLength, patternRmqCount) = (BinaryPrimitives.ReadInt32LittleEndian(buffer2), BinaryPrimitives.ReadInt32LittleEndian(buffer2[4..]), BinaryPrimitives.ReadInt32LittleEndian(buffer2[8..]), BinaryPrimitives.ReadInt32LittleEndian(buffer2[12..]), BinaryPrimitives.ReadInt32LittleEndian(buffer2[16..]));
            lcpRmqInit = new(new(table, n), null!, blockSize, blockCount, new int[blockMinsLength], new ulong[blockTypesLength], new(patternRmqCount));
            __ReadInt32ArrayFromStream(decompressStream, lcpRmqInit.BlockMins);
            __ReadUInt64ArrayFromStream(decompressStream, lcpRmqInit.BlockTypes);
            __ReadPatternRmq(decompressStream, lcpRmqInit.PatternRmq, patternRmqCount);
        }

        if (xxh.GetCurrentHashAsUInt32() != BinaryPrimitives.ReadUInt32LittleEndian(buffer0[4..]))
            throw new InvalidDataException("Broken.");

        // sa
        var sa = SuffixArray.Deserialize(stream);
        // lcpRmp
        var lcpRmp = new FischerHeunSparseTable<int>(lcpRmqInit with { Memory = sa.Lcp }, (a, b) => a <= b);

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
        static void __ReadPatternRmq(Stream stream, Dictionary<ulong, int[,]> table, int count)
        {
            Span<byte> buffer1 = stackalloc byte[8];
            while (count-- > 0)
            {
                stream.ReadExactly(buffer1);
                var key = BinaryPrimitives.ReadUInt64LittleEndian(buffer1);
                var dimensionLength = (int)(key >> 32);
                if (dimensionLength == 0)
                    table.Add(key, new int[0, 0]);
                else
                {
                    var rent = bytePool_.Rent(4 * dimensionLength * dimensionLength);
                    try
                    {
                        var memory = rent.AsMemory(0, 4 * dimensionLength * dimensionLength);
                        stream.ReadExactly(memory.Span);
                        var value = new int[dimensionLength, dimensionLength];
                        memory.Span.CopyTo(MemoryMarshal.AsBytes(MemoryMarshal.CreateSpan<int>(ref value[0, 0], value.Length)));
                        if (!BitConverter.IsLittleEndian)
                            for (var i = 0; i < dimensionLength; i++)
                                for (var j = 0; j < dimensionLength; j++)
                                    value[i, j] = BinaryPrimitives.ReadInt32LittleEndian(memory.Span[(4 * i * dimensionLength + (4 * j))..]);
                        table.Add(key, value);
                    }
                    finally
                    {
                        bytePool_.Return(rent);
                    }
                }
            }
        }
        static void __ReadInt32ArrayFromStream(Stream stream, int[] buffer)
        {
            stream.ReadExactly(MemoryMarshal.AsBytes(buffer.AsSpan()));
            if (!BitConverter.IsLittleEndian)
                for (var i = 0; i < buffer.Length; i++)
                    buffer[i] = BinaryPrimitives.ReadInt32LittleEndian(MemoryMarshal.AsBytes(buffer.AsSpan(i)));
        }
        static void __ReadUInt64ArrayFromStream(Stream stream, ulong[] buffer)
        {
            stream.ReadExactly(MemoryMarshal.AsBytes(buffer.AsSpan()));
            if (!BitConverter.IsLittleEndian)
                for (var i = 0; i < buffer.Length; i++)
                    buffer[i] = BinaryPrimitives.ReadUInt64LittleEndian(MemoryMarshal.AsBytes(buffer.AsSpan(i)));
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
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="minLength"/> is less than or equal 0.</exception>
    public IEnumerable<Repeat> FindRepeats(int minLength = 2)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(minLength);
#else
        if (minLength <= 0)
            throw new ArgumentOutOfRangeException(nameof(minLength));
#endif

        var results = new Dictionary<ReadOnlyMemory<char>, List<int>>(CharMemoryComparer.Instance);
        {
            var saSpan = sa_.SA.Span;
            var lcpSpan = sa_.Lcp.Span;
            var textMemory = sa_.Text;

            for (var i = 1; i < textMemory.Length; i++)
            {
                var lcpLength = lcpSpan[i];
                if (lcpLength >= minLength)
                {
                    for (var j = minLength; j <= lcpLength; j++)
                    {
                        var key = textMemory.Slice(saSpan[i], j);
                        if (!results.TryGetValue(key, out var positions))
                            results.Add(key, positions = []);
                        positions.AddRange([saSpan[i - 1], saSpan[i]]);
                    }
                }
            }
        }

        foreach (var entry in results)
            yield return new(entry.Key.Span.ToString(), entry.Value.Distinct().Order().ToArray());
    }

    /// <summary>
    /// Finds the longest substring that appears at least twice in the text.
    /// This is achieved by finding the maximum value in the LCP array.
    /// If no substring is repeated, returns null.
    /// </summary>
    /// <returns>A <see cref="Repeat"/> record representing the longest repeated substring and its positions, or null if no repeats exist.</returns>
    public Repeat? FindLongestRepeats()
    {
        var lcpSpan = sa_.Lcp.Span;
        if (lcpSpan.Length == 0)
            return null;

        var (lcpMax, lcpMaxPosition) = (0, -1);
        for (var i = 1; i < lcpSpan.Length; i++)
            if (lcpSpan[i] > lcpMax)
                lcpMax = lcpSpan[lcpMaxPosition = i];

        if (lcpMax == 0)
            return null;

        var text = this.Text.Slice(sa_.SA.Span[lcpMaxPosition], lcpMax).Span.ToString();
        // return new(text, FindRepeats(lcpMax).First(n => n.Text == text).Positions);

        var saSpan = sa_.SA.Span;
        var positions = new List<int>(4) { saSpan[lcpMaxPosition - 1], saSpan[lcpMaxPosition] };
        for (var i = lcpMaxPosition - 1; i > 0; i--)
        {
            if (lcpSpan[i] == lcpMax)
                positions.Add(saSpan[i - 1]);
            else
                break;
        }
        for (var i = lcpMaxPosition + 1; i < lcpSpan.Length; i++)
        {
            if (lcpSpan[i] == lcpMax)
                positions.Add(saSpan[i]);
            else
                break;
        }
        return new(text, positions.Order().ToArray());
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

    /// <summary>
    /// Creates a new <see cref="SimilarityMatcher"/> instance to find common substrings between two texts.
    /// This is achieved by concatenating the two texts with a unique separator and building a single LCP index on the result.
    /// </summary>
    /// <param name="text1">The first text to compare.</param>
    /// <param name="text2">The second text to compare.</param>
    /// <returns>A new <see cref="SimilarityMatcher"/> instance ready for finding matches.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text1"/> or <paramref name="text2"/> is null.</exception>
    public static SimilarityMatcher CreateSimilarityMatcher(string text1, string text2)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(text1);
        ArgumentNullException.ThrowIfNull(text2);
#else
        if (text1 == null)
            throw new ArgumentNullException(nameof(text1));
        if (text2 == null)
            throw new ArgumentNullException(nameof(text2));
#endif

        var buffer = new char[text1.Length + text2.Length + 1];
        text1.CopyTo(0, buffer, 0, text1.Length);
        text2.CopyTo(0, buffer, text1.Length + 1, text2.Length);
        return new(LcpIndex.Create(SuffixArray.Create(buffer)), text1, text2);
    }

    /// <summary>
    /// Creates a new <see cref="SimilarityMatcher"/> instance specifically configured to find palindromic substrings within a single text.
    /// This is achieved by concatenating the text with its reverse, separated by a unique character,
    /// and building an LCP index on the combined result.
    /// </summary>
    /// <param name="text">The text in which to find palindromes.</param>
    /// <returns>A new <see cref="SimilarityMatcher"/> instance ready for palindrome detection.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text"/> is null.</exception>
    /// <remarks>
    /// The returned matcher will have its <c>Text1</c> property set to the original <paramref name="text"/> and its <c>Text2</c>
    /// property set to the reversed version of the text.
    /// </remarks>
    public static SimilarityMatcher CreateSimilarityMatcherForPalindrome(string text)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(text);
#else
        if (text == null)
            throw new ArgumentNullException(nameof(text));
#endif

        var buffer = new char[text.Length * 2 + 1];
        text.CopyTo(0, buffer, 0, text.Length);
        text.CopyTo(0, buffer, text.Length + 1, text.Length);
        Array.Reverse(buffer, text.Length + 1, text.Length);
        return new(LcpIndex.Create(SuffixArray.Create(buffer)), text, new(buffer[(text.Length + 1)..]));
    }

    /// <summary>
    /// Efficiently counts the occurrences of a specified pattern within the text.
    /// It leverages binary search over the suffix array to quickly find the range, ensuring high performance even with large texts.
    /// </summary>
    /// <param name="pattern">The string pattern to search for.</param>
    /// <returns>The number of times the pattern appears in the text.</returns>
    public int CountOccurrences(ReadOnlySpan<char> pattern)
    {
        var (start, end) = FindSuffixRange(pattern);
        return start != -1 ? end - start + 1 : 0;
    }

    /// <summary>
    /// Finds all starting positions where the specified pattern occurs in the text.
    /// </summary>
    /// <param name="pattern">The string pattern to search for.</param>
    /// <param name="sortOrder">
    /// Specifies the sort order for the resulting positions. The default is <see cref="SortOrder.Ascending"/>.
    /// </param>
    /// <returns>
    /// An <see cref="IEnumerable{Int32}"/> that enumerates the starting positions (0-based indices) of the pattern's occurrences.
    /// Returns an empty sequence if the pattern is not found.
    /// </returns>
    /// <remarks>
    /// Specifying <see cref="SortOrder.Unordered"/> provides the fastest result by skipping any sorting.
    /// This option is recommended when the order of occurrences is not important.
    /// </remarks>
    /// <exception cref="ArgumentException">Thrown if an invalid <paramref name="sortOrder"/> is provided.</exception>
    public IEnumerable<int> Locate(ReadOnlySpan<char> pattern, SortOrder sortOrder = SortOrder.Ascending)
    {
        if (pattern.IsEmpty)
            return [];

        var (start, end) = FindSuffixRange(pattern);
        return __Enumerate(sa_.SA, start, end, sortOrder);

        #region @@
        static IEnumerable<int> __Enumerate(ReadOnlyMemory<int> saMemory, int start, int end, SortOrder sortOrder)
        {
            switch (sortOrder)
            {
                case SortOrder.Unordered:
                    {
                        for (var i = start; i <= end; i++)
                            yield return saMemory.Span[i];
                    }
                    break;
                case SortOrder.Ascending:
                case SortOrder.Descending:
                    {
                        var result = new List<int>(end - start);
                        for (var i = start; i <= end; i++)
                            result.Add(saMemory.Span[i]);
                        result.Sort();
                        if (sortOrder == SortOrder.Descending)
                            result.Reverse();
                        foreach (var n in result)
                            yield return n;
                    }
                    break;
                default:
                    throw new ArgumentException($"Invalid sort order.", nameof(sortOrder));
            }
        }
        #endregion
    }

    /// <summary>
    /// Finds the shortest substring that appears only once in the text.
    /// This is determined by analyzing the LCP (Longest Common Prefix) values between adjacent suffixes in the suffix array.
    /// </summary>
    /// <returns>
    /// A <see cref="TextWithPosition"/> record containing the shortest unique substring and its starting position.
    /// Returns null if no unique substring exists (e.g., if the text is empty or consists of a single repeating character).
    /// </returns>
    public TextWithPosition? FindShortestUniqueSubstring()
    {
        var saSpan = sa_.SA.Span;
        var lcpSpan = sa_.Lcp.Span;
        var textSpan = this.Text.Span;

        if (saSpan.Length == 0)
            return null;
        else if (saSpan.Length == 1)
            return new(textSpan.ToString(), 0);

        var (minLength, minPosition) = (lcpSpan[1] + 1, saSpan[0]);
        var tmpLength = lcpSpan[1] + 1;

        for (var i = 1; i < saSpan.Length - 1; i++)
        {
            tmpLength = Math.Max(lcpSpan[i], lcpSpan[i + 1]) + 1;
            if (tmpLength < minLength)
                (minLength, minPosition) = (tmpLength, saSpan[i]);
        }

        tmpLength = lcpSpan[saSpan.Length - 1] + 1;
        if (tmpLength < minLength)
            (minLength, minPosition) = (tmpLength, saSpan[^1]);

        if (minPosition == -1 || minLength > saSpan.Length - minPosition)
            return null;

        return new(textSpan[minPosition..(minPosition + minLength)].ToString(), minPosition);
    }

    /// <summary>
    /// Finds all shortest substrings that appear only once in the text.
    /// There can be multiple unique substrings of the same shortest length.
    /// </summary>
    /// <returns>
    /// An enumerable collection of <see cref="TextWithPosition"/> records, each representing
    /// one of the shortest unique substrings found. Returns an empty collection if no unique substring exists.
    /// </returns>
    public IEnumerable<TextWithPosition> FindAllShortestUniqueSubstrings()
    {
        var saSpan = sa_.SA.Span;

        if (saSpan.Length == 0)
            return [];
        else if (saSpan.Length == 1)
            return [new(this.Text.Span.ToString(), 0)];

        var lcpSpan = sa_.Lcp.Span;
        var minLength = Int32.MaxValue;
        var results = new List<(int, int)>();
        for (var i = 0; i < saSpan.Length; i++)
        {
            var currentPosition = saSpan[i];
            var currentLength = i switch
            {
                0 => lcpSpan[1] + 1,
                var n when n == lcpSpan.Length - 1 => lcpSpan[^1] + 1,
                _ => Math.Max(lcpSpan[i], lcpSpan[i + 1]) + 1,
            };
            if (currentPosition + currentLength > saSpan.Length)
                continue;

            if (currentLength == minLength)
                results.Add((currentPosition, minLength));
            else if (currentLength < minLength)
            {
                minLength = currentLength;
                results.Clear();
                results.Add((currentPosition, minLength));
            }
        }

        return results.Order().Select(n => new TextWithPosition(this.Text.Span[n.Item1..(n.Item1 + n.Item2)].ToString(), n.Item1));
    }

    (int, int) FindSuffixRange(ReadOnlySpan<char> pattern)
    {
        if (pattern.Length == 0)
            return (-1, -1);

        var textSpan = this.Text.Span;
        var saSpan = sa_.SA.Span;

        var start = -1;
        {
            var (left, right) = (0, saSpan.Length - 1);
            while (left <= right)
            {
                var mid = left + (right - left) / 2;
                var suffixPosition = saSpan[mid];
                var suffix = textSpan[suffixPosition..];
                var comparison = suffix.StartsWith(pattern) ? 0 : suffix.CompareTo(pattern, StringComparison.Ordinal);
                if (comparison == 0)
                    (start, right) = (mid, mid - 1);
                else if (comparison < 0)
                    left = mid + 1;
                else
                    right = mid - 1;
            }
            if (start == -1)
                return (-1, -1);
        }

        var end = start;
        {
            var (left, right) = (start, saSpan.Length - 1);
            while (left <= right)
            {
                var mid = left + (right - left) / 2;
                var suffixPosition = saSpan[mid];
                var suffix = textSpan[suffixPosition..];
                if (suffix.StartsWith(pattern))
                    (end, left) = (mid, mid + 1);
                else
                    right = mid - 1;
            }
        }

        return (start, end);
    }

    // 
    // 

    /// <summary>
    /// Provides functionality to find all common substrings between two texts using a combined <see cref="LcpIndex"/>.
    /// An instance of this class is created via the <see cref="LcpIndex.CreateSimilarityMatcher"/> factory method.
    /// </summary>
    /// <param name="lcpIndex">The LcpIndex built on the combined text.</param>
    /// <param name="text1">The first original text.</param>
    /// <param name="text2">The second original text.</param>
    public sealed class SimilarityMatcher(LcpIndex lcpIndex, string text1, string text2)
    {
        /// <summary>
        /// Gets the underlying <see cref="LcpIndex"/> built on the combined text.
        /// </summary>
        public LcpIndex LcpIndex => lcpIndex;

        /// <summary>
        /// Gets the first original text used for the comparison.
        /// </summary>
        public string Text1 => text1;

        /// <summary>
        /// Gets the second original text used for the comparison.
        /// </summary>
        public string Text2 => text2;

        // 
        // 

        /// <summary>
        /// Finds all common substrings between the two texts that are at least a specified minimum length.
        /// This method works by scanning the LCP array of the combined text and identifying adjacent suffixes
        /// that originate from different source texts.
        /// </summary>
        /// <param name="minLength">The minimum length for a substring to be reported as a match. Defaults to 2.</param>
        /// <returns>An enumerable collection of <see cref="Match"/> records, each representing a common substring.</returns>
        /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="minLength"/> is less than 0.</exception>
        public IEnumerable<Match> Matches(int minLength = 2)
        {
#if NET8_0_OR_GREATER
            ArgumentOutOfRangeException.ThrowIfNegative(minLength);
#else
            if (minLength < 0)
                throw new ArgumentOutOfRangeException(nameof(minLength), $"{nameof(minLength)} must be greater than or equal 0.");
#endif

            var boundaryPosition = this.Text1.Length;
            var saMemory = this.LcpIndex.SA.SA;
            var lcpMemory = this.LcpIndex.SA.Lcp;
            for (var i = 1; i < lcpMemory.Length; i++)
            {
                var (tmpPosition1, tmpPosition2) = (saMemory.Span[i - 1], saMemory.Span[i]);
                if ((tmpPosition1 < boundaryPosition && tmpPosition2 > boundaryPosition) || (tmpPosition2 < boundaryPosition && tmpPosition1 > boundaryPosition))
                {
                    var lcpLength = lcpMemory.Span[i];
                    if (lcpLength >= minLength)
                    {
                        var resultPosition1 = tmpPosition1 < boundaryPosition ? tmpPosition1 : tmpPosition2;
                        var resultPosition2 = tmpPosition1 > boundaryPosition ? tmpPosition1 - boundaryPosition - 1 : tmpPosition2 - boundaryPosition - 1;
                        yield return new(resultPosition1, resultPosition2, lcpLength);
                    }
                }
            }
        }

        /// <summary>
        /// Finds the longest common substring between the two texts.
        /// </summary>
        /// <returns>A <see cref="Match"/> record representing the longest common substring, or null if no common part exists.</returns>
        public Match? LongestMatch()
        {
            var boundaryPosition = this.Text1.Length;
            var saSpan = this.LcpIndex.SA.SA.Span;
            var lcpSpan = this.LcpIndex.SA.Lcp.Span;

            var (lcpMax, lcpMaxPosition1, lcpMaxPosition2) = (0, -1, -1);
            for (var i = 1; i < lcpSpan.Length; i++)
            {
                var lcpLength = lcpSpan[i];
                if (lcpLength > lcpMax)
                {
                    var (tmpPosition1, tmpPosition2) = (saSpan[i - 1], saSpan[i]);
                    if ((tmpPosition1 < boundaryPosition && tmpPosition2 > boundaryPosition) || (tmpPosition2 < boundaryPosition && tmpPosition1 > boundaryPosition))
                    {
                        lcpMax = lcpLength;
                        lcpMaxPosition1 = tmpPosition1 < boundaryPosition ? tmpPosition1 : tmpPosition2;
                        lcpMaxPosition2 = tmpPosition1 > boundaryPosition ? tmpPosition1 - boundaryPosition - 1 : tmpPosition2 - boundaryPosition - 1;
                    }
                }
            }

            return lcpMax == 0 ? null : new(lcpMaxPosition1, lcpMaxPosition2, lcpMax);
        }

        /// <summary>
        /// Finds the longest palindromic substring within the original text.
        /// This method requires the <see cref="SimilarityMatcher"/> to be created via <see cref="LcpIndex.CreateSimilarityMatcherForPalindrome"/>.
        /// It operates by finding the longest common substring between the text and its reverse.
        /// </summary>
        /// <returns>A <see cref="Palindrome"/> record for the longest palindrome, or null if no palindrome of length 2 or more exists.</returns>
        public Palindrome? FindPalindrome()
        {
            if (this.Text1.Length <= 1)
                return null;

            var boundaryPosition = this.Text1.Length;
            var saSpan = this.LcpIndex.SA.SA.Span;
            var lcpSpan = this.LcpIndex.SA.Lcp.Span;

            var (lcpMax, startPosition) = (0, -1);
            for (var i = 1; i < lcpSpan.Length; i++)
            {
                var (tmpPosition1, tmpPosition2) = (saSpan[i - 1], saSpan[i]);
                if ((tmpPosition1 < boundaryPosition && tmpPosition2 > boundaryPosition) || (tmpPosition2 < boundaryPosition && tmpPosition1 > boundaryPosition))
                {
                    var lcpLength = lcpSpan[i];
                    if (lcpLength > lcpMax)
                    {
                        lcpMax = lcpLength;
                        startPosition = tmpPosition1 < boundaryPosition ? tmpPosition1 : tmpPosition2;
                    }
                }
            }

            return lcpMax == 0 ? null : new(startPosition, lcpMax);
        }

        // 
        // 

        /// <summary>
        /// Represents a common substring found between two texts.
        /// </summary>
        /// <param name="Position1">The zero-based starting position of the match in the first text.</param>
        /// <param name="Position2">The zero-based starting position of the match in the second text.</param>
        /// <param name="Length">The length of the common substring.</param>
        public record Match(int Position1, int Position2, int Length);

        /// <summary>
        /// Represents a palindromic substring found in the text.
        /// </summary>
        /// <param name="Position">The 0-based starting position of the palindrome in the original text.</param>
        /// <param name="Length">The length of the palindrome.</param>
        public record Palindrome(int Position, int Length);
    }

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
    /// Represents a repeated substring and all its occurrences.
    /// </summary>
    /// <param name="Text">The repeating unit string.</param>
    /// <param name="Positions">An array of all 0-based starting positions where the substring occurs.</param>
    public record Repeat(string Text, int[] Positions);

    /// <summary>
    /// Represents a substring with its starting position in the original text.
    /// </summary>
    /// <param name="Text">The content of the substring.</param>
    /// <param name="Position">The 0-based starting position of the substring in the original text.</param>
    public record TextWithPosition(string Text, int Position);

    /// <exclude />
    public record Init(SuffixArray SA, FischerHeunSparseTable<int> LcpRmp);
}
