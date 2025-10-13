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
using System.IO.Compression;
using System.Buffers.Binary;
using System.Runtime.InteropServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides a high-performance full-text search index based on the FM-Index algorithm.
/// </summary>
/// <remarks>
/// The FM-Index is a compressed full-text index that allows for fast counting (<see cref="Count"/>) and locating (<see cref="Locate"/>)
/// of patterns within a large text. It is constructed using the Burrows-Wheeler Transform (BWT), a Wavelet Matrix, and a Suffix Array.
/// </remarks>
public sealed class FMIndex
{
    readonly WaveletMatrixGeneric<char> wm_;
    readonly SuffixArray sa_;
    readonly int original_;
    readonly int length_;
    readonly Dictionary<char, int> ctable_;

    /// <summary>
    /// Gets the original source text used to build the index, excluding the internal terminator character.
    /// </summary>
    public ReadOnlyMemory<char> Text => sa_.Text[..^1];

    /// <summary>
    /// Gets the underlying Wavelet Matrix instance used by this index.
    /// </summary>
    public WaveletMatrixGeneric<char> WM => wm_;

    /// <summary>
    /// Gets the underlying Suffix Array instance used by this index.
    /// </summary>
    public SuffixArray SA => sa_;

    // 
    // 

    FMIndex(Init init)
    {
        (wm_, sa_, original_, length_, ctable_) = init;
    }

    /// <summary>
    /// Creates a new instance of the <see cref="FMIndex"/> from a string.
    /// </summary>
    /// <param name="text">The text to be indexed.</param>
    /// <returns>A new, fully initialized <see cref="FMIndex"/> instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="text"/> is null.</exception>
    public static FMIndex Create(string text)
    => Create(text?.AsMemory() ?? throw new ArgumentNullException(nameof(text)));

    /// <summary>
    /// Creates a new instance of the <see cref="FMIndex"/> from a read-only memory segment of characters.
    /// An internal terminator character ('\0') is appended to the text for correctness.
    /// </summary>
    /// <param name="text">The text to be indexed.</param>
    /// <returns>A new, fully initialized <see cref="FMIndex"/> instance.</returns>
    public static FMIndex Create(ReadOnlyMemory<char> text)
    {
        // append terminator
        var textWithTerminator = new char[text.Length + 1];
        text.CopyTo(textWithTerminator);
        // textWithTerminator[^1] = '\u0000';

        var bwtResult = BurrowsWheelerTransform.Transform(textWithTerminator);
        var wm = WaveletMatrixGeneric<char>.Create(bwtResult.BwtString);

        // [type 1]
        var index = 0;
        var ctable = Enumerable.Aggregate(textWithTerminator.Order(), new Dictionary<char, int>(), (acc, current) =>
        {
            acc.TryAdd(current, index++);
            return acc;
        });
        // [type 2]
        // var sorted = textWithTerminator.Order().ToArray();
        // var ctable = new Dictionary<char, int>();
        // for (var i = 0; i < sorted.Length; i++)
        //     ctable.TryAdd(sorted[i], i);
        // [type 3]
        // var freq = new Dictionary<char, int>();
        // foreach (var ch in textWithTerminator)
        //     if (!freq.TryAdd(ch, 1))
        //         freq[ch]++;
        // var ctable = new Dictionary<char, int>();
        // var sum = 0;
        // foreach (var kv in freq.OrderBy(n => n.Key))
        // {
        //     ctable[kv.Key] = sum;
        //     sum += kv.Value;
        // }

        return new(new(wm, bwtResult.SA, bwtResult.OriginalIndex, bwtResult.BwtString.Length, ctable));
    }

    /// <summary>
    /// Serializes the <see cref="FMIndex"/> instance into a byte array.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> is null.</exception>
    /// <returns>A byte array containing the serialized data.</returns>
    public static byte[] Serialize(FMIndex obj, SerializationOptions? options = null)
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
    /// Serializes the <see cref="FMIndex"/> instance to the specified file.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="file">The path of the file to write to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="file"/> is null.</exception>
    public static void Serialize(FMIndex obj, string file, SerializationOptions? options = null)
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
    /// Serializes the <see cref="FMIndex"/> instance to a stream.
    /// The data is compressed using Brotli and includes a checksum for integrity verification.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="stream">The stream to write the serialized data to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="stream"/> is null.</exception>
    public static void Serialize(FMIndex obj, Stream stream, SerializationOptions? options = null)
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

        // ctable
        var size1 = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                Span<byte> buffer1 = stackalloc byte[8];
                // count of entries
                BinaryPrimitives.WriteInt32LittleEndian(buffer1, obj.ctable_.Count);
                compressStream.Write(buffer1);
                __WriteStreamCTable(compressStream, obj.ctable_);
            }
            var array = memoryStream.ToArray();
            size1 = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }

        var lastPosition = stream.Position;

        //  0: byte * 4, "FMIL"
        "FMIL"u8.CopyTo(buffer0);
        //  4: uint * 1, xxh
        BinaryPrimitives.WriteUInt32LittleEndian(buffer0[4..], xxh.GetCurrentHashAsUInt32());
        //  8: int * 1, original
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[8..], obj.original_);
        // 12: int * 1, length
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[12..], obj.length_);
        // 16: int * 1, size of ctable
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[16..], size1);
        // 20- empty
        stream.Seek(firstPosition, SeekOrigin.Begin);
        stream.Write(buffer0);

        stream.Seek(lastPosition, SeekOrigin.Begin);

        // wm
        WaveletMatrixGeneric<char>.Serialize(obj.wm_, stream, WaveletMatrixGeneric<char>.CharSerializer.Instance, options);
        // sa
        SuffixArray.Serialize(obj.sa_, stream, options);

        #region @@
        static void __WriteStreamCTable(Stream stream, Dictionary<char, int> ctable)
        {
            foreach (var buffer in __EnumeratePacked(ctable, new ulong[4096]))
                __WriteStreamFromUInt64Memory(stream, buffer);

            #region @@
            static IEnumerable<ulong[]> __EnumeratePacked(Dictionary<char, int> ctable, ulong[] buffer)
            {
                var count = 0;
                foreach (var entry in ctable)
                {
                    buffer[count++] = ((ulong)entry.Key << 48) | (uint)entry.Value;
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
    /// Deserializes a byte array into a <see cref="FMIndex"/> instance.
    /// </summary>
    /// <param name="data">The byte array containing the serialized data.</param>
    /// <returns>A new, deserialized instance of <see cref="FMIndex"/>.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="data"/> is null.</exception>
    public static FMIndex Deserialize(byte[] data)
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
    /// Deserializes a <see cref="FMIndex"/> instance from the specified file.
    /// </summary>
    /// <param name="file">The path of the file to read from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="file"/> is null.</exception>
    public static FMIndex Deserialize(string file)
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
    /// Deserializes a <see cref="FMIndex"/> instance from a stream.
    /// It verifies the file format, type identifier, and checksum.
    /// </summary>
    /// <param name="stream">The stream to read the serialized data from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="stream"/> is null.</exception>
    /// <exception cref="InvalidDataException">Thrown if the data format is unsupported, the type is incompatible, or the data is corrupt.</exception>
    public static FMIndex Deserialize(Stream stream)
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
        if (!buffer0[0..4].SequenceEqual("FMIL"u8))
            throw new InvalidDataException("Unsupported format.");

        // ctable
        Dictionary<char, int> ctable = null!;
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[16..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> buffer2 = stackalloc byte[8];
            decompressStream.ReadExactly(buffer2);
            var count = BinaryPrimitives.ReadInt32LittleEndian(buffer2);
            ctable = new(count);
            foreach (var entry in __ReadCTableFromStream(decompressStream, count))
                ctable.Add(entry.Item1, entry.Item2);
        }

        if (xxh.GetCurrentHashAsUInt32() != BinaryPrimitives.ReadUInt32LittleEndian(buffer0[4..]))
            throw new InvalidDataException("Broken.");

        var wm = WaveletMatrixGeneric<char>.Deserialize(stream, WaveletMatrixGeneric<char>.CharSerializer.Instance);
        var sa = SuffixArray.Deserialize(stream);
        var original = BinaryPrimitives.ReadInt32LittleEndian(buffer0[8..]);
        var length = BinaryPrimitives.ReadInt32LittleEndian(buffer0[12..]);

        return new(new(wm, sa, original, length, ctable));

        #region @@
        static IEnumerable<(char, int)> __ReadCTableFromStream(Stream stream, int count)
        {
            var buffer = new ulong[1048576];
            var offset = 0;
            while (offset < count)
            {
                var length = Math.Min(count - offset, buffer.Length);
                stream.ReadExactly(MemoryMarshal.AsBytes(buffer.AsSpan(0, length)));
                if (!BitConverter.IsLittleEndian)
                    for (var i = 0; i < length; i++)
                        buffer[i] = BinaryPrimitives.ReadUInt64LittleEndian(MemoryMarshal.AsBytes(buffer.AsSpan(i)));

                for (var i = 0; i < length; i++)
                    yield return ((char)(ushort)(buffer[i] >> 48), (int)(uint)buffer[i]);

                offset += length;
            }
        }
        #endregion
    }

    /// <summary>
    /// Counts the number of occurrences of a pattern within the text.
    /// This operation is extremely fast, typically proportional to the length of the pattern, not the text.
    /// </summary>
    /// <param name="pattern">The pattern to search for.</param>
    /// <returns>The total number of non-overlapping occurrences of the pattern.</returns>
    public int Count(ReadOnlySpan<char> pattern)
    {
        var (start, end) = FindRange(pattern);
        return end - start;
    }

    /// <summary>
    /// Finds all starting positions of a pattern within the text.
    /// </summary>
    /// <param name="pattern">The pattern to search for.</param>
    /// <param name="sortOrder">Specifies the order of the returned positions. Defaults to <see cref="SortOrder.Ascending"/>.</param>
    /// <returns>An enumerable collection of the zero-based starting positions of all occurrences.</returns>
    /// <remarks>
    /// Specifying <see cref="SortOrder.Ascending"/> or <see cref="SortOrder.Descending"/> requires collecting all results
    /// and sorting them, which incurs a performance cost proportional to the number of matches (k log k).
    /// For the highest performance where order is not important, use <see cref="SortOrder.Unordered"/>.
    /// </remarks>
    public IEnumerable<int> Locate(ReadOnlySpan<char> pattern, SortOrder sortOrder = SortOrder.Ascending)
    {
        var (start, end) = FindRange(pattern);
        if (start < end)
            return __Enumerate(start, end, sortOrder, sa_.SA);
        else
            return [];

        #region @@
        static IEnumerable<int> __Enumerate(int start, int end, SortOrder sortOrder, ReadOnlyMemory<int> saMemory)
        {
            switch (sortOrder)
            {
                case SortOrder.Unordered:
                    {
                        for (var i = start; i < end; i++)
                            yield return saMemory.Span[i];
                    }
                    break;
                case SortOrder.Ascending:
                case SortOrder.Descending:
                    {
                        var result = new List<int>(end - start);
                        for (var i = start; i < end; i++)
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
    /// Reconstructs the original source text from the compressed index.
    /// This method demonstrates the reversibility of the Burrows-Wheeler Transform.
    /// </summary>
    /// <returns>The original text that was used to create the index.</returns>
    public string RestoreSourceText()
    {
        var buffer = new char[length_];
        var current = original_;
        for (var i = 0; i < buffer.Length; i++)
        {
            var c = wm_.Access(current);
            buffer[buffer.Length - i - 1] = c;
            current = ctable_[c] + wm_.Rank(current, c);
        }

        return new string(buffer, 0, length_ - 1);
    }

    /// <summary>
    /// Extracts a snippet of text surrounding a specified position, with a configurable context ratio.
    /// </summary>
    /// <param name="position">The zero-based starting position of the keyword in the original text.</param>
    /// <param name="keyLength">The length of the keyword.</param>
    /// <param name="totalLength">The desired total length of the snippet.</param>
    /// <param name="leadingRatio">
    /// The desired proportion (0.0 to 1.0) of the context text to appear before the keyword. Defaults to 0.5 for centering the keyword.
    /// </param>
    /// <returns>A tuple containing the generated snippet string and the zero-based starting position of the keyword within the snippet.</returns>
    /// <exception cref="ArgumentOutOfRangeException">
    /// Thrown if <paramref name="position"/>, <paramref name="keyLength"/>, or <paramref name="totalLength"/> are invalid, or if <paramref name="leadingRatio"/> is not between 0.0 and 1.0.
    /// </exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="position"/> + <paramref name="keyLength"/> is greater than the length of text.</exception>
    /// <remarks>
    /// The specified <paramref name="leadingRatio"/> is a guideline. The method prioritizes returning a snippet of approximately
    /// <paramref name="totalLength"/>. If the keyword is near the start or end of the text, the actual ratio of leading/trailing context will be adjusted to fill the requested length.
    /// </remarks>
    public (string, int) GetSnippet(int position, int keyLength, int totalLength, double leadingRatio = 0.5)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(position);
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(keyLength);
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(totalLength);
#else
        if (position < 0)
            throw new ArgumentOutOfRangeException(nameof(position), $"{nameof(position)} must be greater than or equal 0.");
        if (keyLength <= 0)
            throw new ArgumentOutOfRangeException(nameof(position), $"{nameof(keyLength)} must be greater than 0.");
        if (totalLength <= 0)
            throw new ArgumentOutOfRangeException(nameof(totalLength), $"{nameof(totalLength)} must be greater than or equal 0.");
#endif
        if (leadingRatio < 0 | 1 < leadingRatio)
            throw new ArgumentOutOfRangeException(nameof(leadingRatio), $"{nameof(leadingRatio)} must be between 0 and 1.");

        var text = this.Text;

        if (position + keyLength > text.Length)
            throw new ArgumentException($"{nameof(position)}+{nameof(keyLength)} must be less than or equal the length of text.");

        if (keyLength >= totalLength)
            return (text[position..(position + totalLength)].ToString(), 0);

        var contextLength = totalLength - keyLength;
        var leadingLength = (int)Math.Round(contextLength * leadingRatio);
        var trailingLength = contextLength - leadingLength;

        var start = position - leadingLength;
        if (start < 0)
        {
            trailingLength += -start;
            start = 0;
        }

        var end = position + keyLength + trailingLength;
        if (end > text.Length)
        {
            var overflow = end - text.Length;
            start = Math.Max(0, start - overflow);
            end = text.Length;
        }

        return (text.Span[start..end].ToString(), position - start);
    }

    (int, int) FindRange(ReadOnlySpan<char> pattern)
    {
        if (pattern.Length == 0)
            return (0, 0);

        var lastChar = pattern[^1];
        if (!ctable_.TryGetValue(lastChar, out var start))
            return (0, 0);
        else
        {
            var end = start + wm_.Rank(length_, lastChar);
            for (var i = pattern.Length - 2; i >= 0; i--)
            {
                var currentChar = pattern[i];
                if (!ctable_.TryGetValue(currentChar, out var tmp))
                    return (0, 0);
                else
                {
                    (start, end) = (tmp + wm_.Rank(start, currentChar), tmp + wm_.Rank(end, currentChar));
                    if (start >= end)
                        return (0, 0);
                }
            }

            return (start, end);
        }
    }

    // 
    // 

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

    record Init(WaveletMatrixGeneric<char> Wm, SuffixArray Sa, int OriginalIndex, int Length, Dictionary<char, int> Ct);
}
