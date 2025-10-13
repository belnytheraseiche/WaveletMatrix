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
/// Provides the core, high-performance implementation of a Wavelet Matrix for integer sequences.
/// This class is not generic and operates directly on coordinate-compressed integer arrays.
/// </summary>
/// <remarks>
/// This class forms the engine for the generic <see cref="WaveletMatrixGeneric{T}"/>. It encapsulates
/// the bit-level data structures and algorithms, such as Rank, Select, and Quantile.
/// </remarks>
public sealed class WaveletMatrixCore
{
    readonly int size_;
    readonly int[] zeros_;
    readonly RankSelectBitSet[] matrix_;

    /// <summary>
    /// Gets the total number of elements in the sequence.
    /// </summary>
    public int Size => size_;

    // 
    // 

    WaveletMatrixCore(Init init)
    {
        (size_, zeros_, matrix_) = init;
    }

    /// <summary>
    /// Creates a new instance of the <see cref="WaveletMatrixCore"/> from a sequence of non-negative integers.
    /// </summary>
    /// <param name="sequence">The sequence of integers to build the Wavelet Matrix from. All values must be non-negative.</param>
    /// <returns>A new, fully initialized <see cref="WaveletMatrixCore"/> instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="sequence"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="sequence"/> contains negative values.</exception>
    public static WaveletMatrixCore Create(IEnumerable<int> sequence)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(sequence);
#else
        if (sequence == null)
            throw new ArgumentNullException(nameof(sequence));
#endif

        var current = sequence.ToList();
        var size = current.Count;
        if (size == 0)
            return new(new(0, [], []));

        if (current.Any(n => n < 0))
            throw new ArgumentException($"{nameof(sequence)} must be greater than or equal 0.", nameof(sequence));

        var maxValue = current.Max();
        var depth = maxValue != 0 ? (int)Math.Floor(Math.Log(maxValue, 2.0)) + 1 : 1;
        var zeros = new int[depth];
        var matrix = new RankSelectBitSet[depth];
        for (var i = depth - 1; i >= 0; i--)
        {
            var bitsTmp = new BitSet();
            var zerosTmp = new ValueBuffer<int>(64, true);
            var onesTmp = new ValueBuffer<int>(64, true);
            foreach (var value in current)
            {
                if (((value >> i) & 0x01) != 0)
                {
                    bitsTmp.Add(true);
                    onesTmp.Add(value);
                }
                else
                {
                    bitsTmp.Add(false);
                    zerosTmp.Add(value);
                }
            }

            var index = depth - i - 1;
            matrix[index] = bitsTmp.ToRankSelect();
            zeros[index] = zerosTmp.Count;
            current.Clear();
            current.AddRange(zerosTmp);
            current.AddRange(onesTmp);
        }

        return new(new(size, zeros, matrix));
    }

    /// <summary>
    /// Serializes the <see cref="WaveletMatrixCore"/> instance into a byte array.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> is null.</exception>
    /// <returns>A byte array containing the serialized data.</returns>
    public static byte[] Serialize(WaveletMatrixCore obj, SerializationOptions? options = null)
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
    /// Serializes the <see cref="WaveletMatrixCore"/> instance to the specified file.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="file">The path of the file to write to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="file"/> is null.</exception>
    public static void Serialize(WaveletMatrixCore obj, string file, SerializationOptions? options = null)
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
    /// Serializes the <see cref="WaveletMatrixCore"/> instance to a stream.
    /// The data is compressed using Brotli and includes a checksum for integrity verification.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="stream">The stream to write the serialized data to.</param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="stream"/> is null.</exception>
    public static void Serialize(WaveletMatrixCore obj, Stream stream, SerializationOptions? options = null)
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

        // zeros_
        var sizeZeros = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                __WriteStreamFromInt32Memory(compressStream, obj.zeros_);
            }
            var array = memoryStream.ToArray();
            sizeZeros = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }
        // matrix_
        var sizeMatrix = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                Span<byte> buffer1 = stackalloc byte[8];
                foreach (var n in obj.matrix_)
                {
                    BinaryPrimitives.WriteInt32LittleEndian(buffer1, n.Buffer.Length);
                    BinaryPrimitives.WriteInt32LittleEndian(buffer1[4..], n.Count);
                    compressStream.Write(buffer1);
                    __WriteStreamFromUInt64Memory(compressStream, n.Buffer);
                }
            }
            var array = memoryStream.ToArray();
            sizeMatrix = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }

        var lastPosition = stream.Position;

        //  0: byte * 3, WMCR
        "WMCR"u8.CopyTo(buffer0);
        //  4: uint * 1, xxhash
        BinaryPrimitives.WriteUInt32LittleEndian(buffer0[4..], xxh.GetCurrentHashAsUInt32());
        //  8: int * 1, size_
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[8..], obj.size_);
        // 12: int * 1, length of zeros_
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[12..], obj.zeros_.Length);
        // 16: int * 1, size of zeros_ buffer
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[16..], sizeZeros);
        // 20: int * 1, depth of matrix_
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[20..], obj.matrix_.Length);
        // 24: int * 1, size of matrix_ buffer
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[24..], sizeMatrix);
        // 28- empty
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
                var buffer = new byte[256];
                var offset = 0;
                while (offset < memory.Length)
                {
                    var length = Math.Min(64, memory.Length - offset);
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
    /// Deserializes a byte array into a <see cref="WaveletMatrixCore"/> instance.
    /// </summary>
    /// <param name="data">The byte array containing the serialized data.</param>
    /// <returns>A new, deserialized instance of <see cref="WaveletMatrixCore"/>.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="data"/> is null.</exception>
    public static WaveletMatrixCore Deserialize(byte[] data)
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
    /// Deserializes a <see cref="WaveletMatrixCore"/> instance from the specified file.
    /// </summary>
    /// <param name="file">The path of the file to read from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="file"/> is null.</exception>
    public static WaveletMatrixCore Deserialize(string file)
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
    /// Deserializes a <see cref="WaveletMatrixCore"/> instance from a stream.
    /// It verifies the file format, type identifier, and checksum.
    /// </summary>
    /// <param name="stream">The stream to read the serialized data from.</param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="stream"/> is null.</exception>
    /// <exception cref="InvalidDataException">Thrown if the data format is unsupported, the type is incompatible, or the data is corrupt.</exception>
    public static WaveletMatrixCore Deserialize(Stream stream)
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
        if (!buffer0[..4].SequenceEqual("WMCR"u8))
            throw new InvalidDataException("Unsupported format.");

        var size = BinaryPrimitives.ReadInt32LittleEndian(buffer0[8..]);

        // zeros
        var zeros = new int[BinaryPrimitives.ReadInt32LittleEndian(buffer0[12..])];
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[16..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            __ReadInt32ArrayFromStream(decompressStream, zeros);
        }
        // matrix
        var matrix = new RankSelectBitSet[BinaryPrimitives.ReadInt32LittleEndian(buffer0[20..])];
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[24..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            Span<byte> buffer2 = stackalloc byte[8];
            for (var i = 0; i < matrix.Length; i++)
            {
                decompressStream.ReadExactly(buffer2);
                var length = BinaryPrimitives.ReadInt32LittleEndian(buffer2);
                var count = BinaryPrimitives.ReadInt32LittleEndian(buffer2[4..]);
                var buffer3 = new ulong[length];
                __ReadUInt64ArrayFromStream(decompressStream, buffer3);
                matrix[i] = new ImmutableBitSet(buffer3, count).ToRankSelect();
            }
        }

        if (xxh.GetCurrentHashAsUInt32() != BinaryPrimitives.ReadUInt32LittleEndian(buffer0[4..]))
            throw new InvalidDataException("Broken.");

        return new(new(size, zeros, matrix));

        #region @@
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
    /// Gets the value of the element at the specified zero-based position.
    /// </summary>
    /// <param name="k">The zero-based index of the element to retrieve.</param>
    /// <returns>The integer value at the specified position.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    public int Access(int k)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(k);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(k, size_);
#else
        if (k < 0)
            throw new ArgumentOutOfRangeException(nameof(k));
        if (k >= size_)
            throw new ArgumentOutOfRangeException(nameof(k));
#endif

        var depth = matrix_.Length;
        var value = 0;
        var current = k;
        for (var i = 0; i < depth; i++)
        {
            var bit = matrix_[i].At(current);
            value = (value << 1) | (bit ? 1 : 0);
            current = bit ? matrix_[i].Rank1(current) : matrix_[i].Rank0(current);
            if (bit)
                current += zeros_[i];
        }

        return value;
    }

    /// <summary>
    /// Counts the number of occurrences of a specified value in the prefix of the sequence [0, k).
    /// </summary>
    /// <param name="k">The exclusive end of the range [0, k).</param>
    /// <param name="value">The value to count.</param>
    /// <returns>The number of times the value appears in the specified prefix.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    public int Rank(int k, int value)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(k);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(k, size_);
#else
        if (k < 0)
            throw new ArgumentOutOfRangeException(nameof(k));
        if (k > size_)
            throw new ArgumentOutOfRangeException(nameof(k), $"{nameof(k)} must be less than or equal the size of sequence ({size_}).");
#endif

        var depth = matrix_.Length;
        var (start, end) = (0, k);
        for (var i = 0; i < depth; i++)
        {
            var bit = 0 != ((value >> (depth - i - 1)) & 1);
            var (startZeros, endZeros) = (matrix_[i].Rank0(start), matrix_[i].Rank0(end));
            if (bit)
                (start, end) = (start - startZeros + zeros_[i], end - endZeros + zeros_[i]);
            else
                (start, end) = (startZeros, endZeros);
        }

        return end - start;
    }

    /// <summary>
    /// Finds the zero-based position of the k-th occurrence of a specified value.
    /// </summary>
    /// <param name="k">The one-based rank of the occurrence to find (e.g., k=1 for the first occurrence).</param>
    /// <param name="value">The value to search for.</param>
    /// <returns>The zero-based index of the k-th occurrence of the value, or -1 if not found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    public int Select(int k, int value)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(k);
        // ArgumentOutOfRangeException.ThrowIfGreaterThan(k, size_);
#else
        if (k <= 0)
            throw new ArgumentOutOfRangeException(nameof(k));
        // if (k > size_)
        //     throw new ArgumentOutOfRangeException(nameof(k), $"{nameof(k)} must be less than or equal the size of sequence ({size_}).");
#endif

        var total = Rank(size_, value);
        if (k > total)
            return -1;

        var depth = matrix_.Length;
        var (start, end) = (0, size_);
        for (var i = 0; i < depth; i++)
        {
            var bit = 0 != ((value >> (depth - i - 1)) & 1);
            var (startZeros, endZeros) = (matrix_[i].Rank0(start), matrix_[i].Rank0(end));
            if (!bit)
                (start, end) = (startZeros, endZeros);
            else
                (start, end) = (zeros_[i] + start - startZeros, zeros_[i] + end - endZeros);
        }

        if (k > end - start)
            return -1;

        var current = start + k - 1;
        for (var i = depth - 1; i >= 0; i--)
        {
            var bit = 0 != ((value >> (depth - i - 1)) & 1);
            if (!bit)
                current = matrix_[i].Select0(current + 1);
            else
            {
                var ones = current - zeros_[i] + 1;
                if (ones <= 0)
                    current = -1;
                else
                    current = matrix_[i].Select1(ones);
            }

            if (current == -1)
                break;
        }

        return current;
    }

    // ok, binary search
    // public int Select(int k, int value)
    // {
    //     var total = Rank(size_, value);
    //     if (k <= 0 || k > total)
    //         return -1;
    //     var (lo, hi, result) = (0, size_ - 1, -1);
    //     while (lo <= hi)
    //     {
    //         var mid = lo + ((hi - lo) >> 1);
    //         if (Rank(mid + 1, value) >= k)
    //             hi = (result = mid) - 1;
    //         else
    //             lo = mid + 1;
    //     }
    //     return result;
    // }

    /// <summary>
    /// Counts the number of occurrences of a specified value.
    /// </summary>
    /// <param name="value">The value to count.</param>
    /// <returns>The number of occurrences of the value.</returns>
    /// <remarks>This method is a shorthand for calling RangeCount(0, <see cref="Size"/>, <paramref name="value"/>).</remarks>
    public int RangeCount(int value)
    => RangeCount(0, size_, value);

    /// <summary>
    /// Counts the number of occurrences of a specified value within the range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The value to count.</param>
    /// <returns>The number of occurrences of the value in the range.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public int RangeCount(int start, int end, int value)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}.");

        return Rank(end, value) - Rank(start, value);
    }

    /// <summary>
    /// Finds the k-th smallest value.
    /// </summary>
    /// <param name="k">The zero-based rank of the value to find (e.g., k=0 for the smallest value).</param>
    /// <returns>The k-th smallest value.</returns>
    /// <exception cref="ArgumentException">Thrown if <paramref name="k"/> is out of the valid range.</exception>
    /// <remarks>This method is a shorthand for calling Quantile(0, <see cref="Size"/>, <paramref name="k"/>).</remarks>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of the valid range [0, Size).</exception>
    public int Quantile(int k)
    => Quantile(0, size_, k);

    /// <summary>
    /// Finds the k-th smallest value within the specified range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="k">The zero-based rank of the value to find (e.g., k=0 for the smallest value).</param>
    /// <returns>The k-th smallest value in the range.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/>, <paramref name="end"/> or <paramref name="k"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="k"/> is out of the valid range [0, end-start).</exception>
    public int Quantile(int start, int end, int k)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
        ArgumentOutOfRangeException.ThrowIfNegative(k);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
        if (k < 0)
            throw new ArgumentOutOfRangeException(nameof(k));
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}");
        if (k >= end - start)
            throw new ArgumentException($"{nameof(k)} must be less than {nameof(end)} - {nameof(start)}");

        var depth = matrix_.Length;
        var value = 0;
        for (var i = 0; i < depth; i++)
        {
            var countZeros = matrix_[i].Rank0(end) - matrix_[i].Rank0(start);
            if (k < countZeros)
            {
                value <<= 1;
                (start, end) = (matrix_[i].Rank0(start), matrix_[i].Rank0(end));
            }
            else
            {
                value = (value << 1) | 1;
                k -= countZeros;
                (start, end) = (matrix_[i].Rank1(start) + zeros_[i], matrix_[i].Rank1(end) + zeros_[i]);
            }
        }

        return value;
    }

    /// <summary>
    /// Counts the number of elements whose values are within the value range [minValue, maxValue).
    /// </summary>
    /// <param name="minValue">The inclusive start of the value range.</param>
    /// <param name="maxValue">The exclusive end of the value range.</param>
    /// <returns>The count of elements matching the criteria.</returns>
    /// <exception cref="ArgumentException">Thrown if <paramref name="minValue"/> is greater than or equal <paramref name="maxValue"/>.</exception>
    /// <remarks>This method is a shorthand for calling RangeFreq(0, <see cref="Size"/>, <paramref name="minValue"/>, <paramref name="maxValue"/>).</remarks>
    public int RangeFreq(int minValue, int maxValue)
    => RangeFreq(0, size_, minValue, maxValue);

    /// <summary>
    /// Counts the number of elements in a position range [start, end) whose values are within the value range [minValue, maxValue).
    /// </summary>
    /// <param name="start">The inclusive start of the position range.</param>
    /// <param name="end">The exclusive end of the position range.</param>
    /// <param name="minValue">The inclusive start of the value range.</param>
    /// <param name="maxValue">The exclusive end of the value range.</param>
    /// <returns>The count of elements matching the criteria.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="minValue"/> is greater than or equal <paramref name="maxValue"/>.</exception>
    public int RangeFreq(int start, int end, int minValue, int maxValue)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}");
        if (minValue > maxValue)
            throw new ArgumentException($"{nameof(minValue)} must be less than {nameof(maxValue)}.");

        var countMin = CountLessThan(end, minValue) - CountLessThan(start, minValue);
        var countMax = CountLessThan(end, maxValue) - CountLessThan(start, maxValue);
        return countMax - countMin;
    }

    /// <summary>
    /// Finds the most frequent value (the mode) and its frequency.
    /// </summary>
    /// <returns>A <see cref="ValueWithFrequency"/> record containing the mode and its count.</returns>
    /// <remarks>This method is a shorthand for calling RangeMode(0, <see cref="Size"/>).</remarks>
    public ValueWithFrequency RangeMode()
    => RangeMode(0, size_);

    /// <summary>
    /// Finds the most frequent value (the mode) and its frequency within the specified range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <returns>A <see cref="ValueWithFrequency"/> record containing the mode and its count.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public ValueWithFrequency RangeMode(int start, int end)
    // => TopK(start, end, 1).FirstOrDefault() ?? throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}.");
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}");

        var depth = matrix_.Length;
        var (topValue, topFreq) = (-1, -1);
        var stack = new Stack<KFindFrame>();
        stack.Push(new(0, 0, start, end, end - start));

        while (stack.TryPop(out var frame))
        {
            if (frame.Frequency <= topFreq)
                continue;

            if (frame.Depth == depth)
            {
                if (frame.Frequency > topFreq)
                    (topValue, topFreq) = (frame.Value, frame.Frequency);
                continue;
            }

            var startZeros = matrix_[frame.Depth].Rank0(frame.Start);
            var endZeros = matrix_[frame.Depth].Rank0(frame.End);
            var countZeros = endZeros - startZeros;
            if (countZeros > 0)
                stack.Push(new(frame.Value << 1, frame.Depth + 1, startZeros, endZeros, countZeros));

            var countOnes = frame.End - frame.Start - countZeros;
            if (countOnes > 0)
            {
                var startOnes = zeros_[frame.Depth] + frame.Start - startZeros;
                var endOnes = zeros_[frame.Depth] + frame.End - endZeros;
                stack.Push(new((frame.Value << 1) | 1, frame.Depth + 1, startOnes, endOnes, countOnes));
            }
        }

        return new(topValue, topFreq);
    }

    /// <summary>
    /// Finds the top K most frequent values and their frequencies.
    /// </summary>
    /// <param name="k">The number of top elements to retrieve.</param>
    /// <returns>An enumerable collection of <see cref="ValueWithFrequency"/> records, ordered by frequency in descending order.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    /// <remarks>This method is a shorthand for calling TopK(0, <see cref="Size"/>, <paramref name="k"/>).</remarks>
    public IEnumerable<ValueWithFrequency> TopK(int k)
    => TopK(0, size_, k);

    /// <summary>
    /// Finds the top K most frequent values and their frequencies within the specified range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="k">The number of top elements to retrieve.</param>
    /// <returns>An enumerable collection of <see cref="ValueWithFrequency"/> records, ordered by frequency in descending order.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/>, <paramref name="end"/> or <paramref name="k"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public IEnumerable<ValueWithFrequency> TopK(int start, int end, int k)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
        ArgumentOutOfRangeException.ThrowIfNegative(k);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}");

        if (k == 0)
            return [];

        if (k > size_)
            k = size_;

        var depth = matrix_.Length;
        var result = new List<ValueWithFrequency>();
#if NET6_0_OR_GREATER
        var queue = new PriorityQueue<KFindFrame, int>([(new(0, 0, start, end, end - start), -(end - start))]);
#else
        var queue = new SortedList<int, KFindFrame> { { -(end - start), new(0, 0, start, end, end - start) } };
#endif
        while (result.Count < k && __TryDequeue(queue, out var frame))
        {
            if (frame.Depth == depth)
            {
                result.Add(new(frame.Value, frame.Frequency));
                continue;
            }

            var startZeros = matrix_[frame.Depth].Rank0(frame.Start);
            var endZeros = matrix_[frame.Depth].Rank0(frame.End);
            var countZeros = endZeros - startZeros;
            if (countZeros > 0)
                __Enqueue(queue, new(frame.Value << 1, frame.Depth + 1, startZeros, endZeros, countZeros), -countZeros);

            var countOnes = (frame.End - frame.Start) - countZeros;
            if (countOnes > 0)
            {
                var startOnes = zeros_[frame.Depth] + frame.Start - startZeros;
                var endOnes = zeros_[frame.Depth] + frame.End - endZeros;
                __Enqueue(queue, new((frame.Value << 1) | 1, frame.Depth + 1, startOnes, endOnes, countOnes), -countOnes);
            }
        }

        return result;

        #region @@
#if NET6_0_OR_GREATER
        static bool __TryDequeue(PriorityQueue<KFindFrame, int> queue, out KFindFrame frame)
        => queue.TryDequeue(out frame!, out var _);
        static void __Enqueue(PriorityQueue<KFindFrame, int> queue, KFindFrame frame, int priority)
        => queue.Enqueue(frame, priority);
#else
        static bool __TryDequeue(SortedList<int, KFindFrame> queue, out KFindFrame frame)
        {
            frame = null!;
            if (queue.Count == 0)
                return false;
            frame = queue.Values[0];
            queue.RemoveAt(0);
            return true;
        }
        static void __Enqueue(SortedList<int, KFindFrame> queue, KFindFrame frame, int priority)
        => queue.Add(priority, frame);
#endif
        #endregion
    }

    /// <summary>
    /// Finds the predecessor of a given value.
    /// The predecessor is the largest value that is strictly smaller than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="value">The reference value.</param>
    /// <returns>The predecessor value, or null if no such value is found.</returns>
    /// <remarks>This method is a shorthand for calling SmallerValue(0, <see cref="Size"/>, <paramref name="value"/>).</remarks>
    public int? SmallerValue(int value)
    => SmallerValue(0, size_, value);

    /// <summary>
    /// Finds the predecessor of a given value in the specified range [start, end).
    /// The predecessor is the largest value in the range that is strictly smaller than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The reference value.</param>
    /// <returns>The predecessor value, or null if no such value is found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public int? SmallerValue(int start, int end, int value)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}");

        return FindNeighbor(start, end, value, false);
    }

    /// <summary>
    /// Finds the successor of a given value.
    /// The successor is the smallest value that is strictly larger than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="value">The reference value.</param>
    /// <returns>The successor value, or null if no such value is found.</returns>
    /// <remarks>This method is a shorthand for calling LargerValue(0, <see cref="Size"/>, <paramref name="value"/>).</remarks>
    public int? LargerValue(int value)
    => LargerValue(0, size_, value);

    /// <summary>
    /// Finds the successor of a given value in the specified range [start, end).
    /// The successor is the smallest value in the range that is strictly larger than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The reference value.</param>
    /// <returns>The successor value, or null if no such value is found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public int? LargerValue(int start, int end, int value)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, size_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > size_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({size_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}");

        return FindNeighbor(start, end, value, true);
    }

    int? FindNeighbor(int start, int end, int value, bool larger)
    {
        var depth = matrix_.Length;
        int? candidate = null;

        for (var i = 0; i < depth; i++)
        {
            var startZeros = matrix_[i].Rank0(start);
            var endZeros = matrix_[i].Rank0(end);
            var countZeros = endZeros - startZeros;
            var countOnes = end - start - countZeros;
            var bit = 1 == ((value >> (depth - i - 1)) & 1);
            if (!larger && bit && countZeros > 0)
            {
                var tmpStart = startZeros;
                var tmpEnd = endZeros;
                var tmpValue = value & ~((1 << (depth - i)) - 1);
                candidate = __FindExtreme(tmpStart, tmpEnd, i + 1, tmpValue, false);
            }
            else if (larger && !bit && countOnes > 0)
            {
                var tmpStart = zeros_[i] + start - startZeros;
                var tmpEnd = zeros_[i] + end - endZeros;
                var tmpValue = (value & ~((1 << (depth - i)) - 1)) | (1 << (depth - i - 1));
                candidate = __FindExtreme(tmpStart, tmpEnd, i + 1, tmpValue, true);
            }

            if (bit)
                (start, end) = (zeros_[i] + start - startZeros, zeros_[i] + end - endZeros);
            else
                (start, end) = (startZeros, endZeros);
            if (start == end)
                break;
        }

        return candidate;

        #region @@
        int __FindExtreme(int start, int end, int depth, int value, bool largest)
        {
            for (var i = depth; i < matrix_.Length; i++)
            {
                var startZeros = matrix_[i].Rank0(start);
                var endZeros = matrix_[i].Rank0(end);
                var countZeros = endZeros - startZeros;
                var countOnes = end - start - countZeros;
                var shift = matrix_.Length - i - 1;

                if (largest)
                {
                    if (countOnes > 0)
                    {
                        value |= 1 << shift;
                        (start, end) = (zeros_[i] + start - startZeros, zeros_[i] + end - endZeros);
                    }
                    else
                    {
                        value &= ~(1 << shift);
                        (start, end) = (startZeros, endZeros);
                    }
                }
                else
                {
                    if (countOnes > 0)
                    {
                        value |= 1 << shift;
                        (start, end) = (zeros_[i] + start - startZeros, zeros_[i] + end - endZeros);
                    }
                    else
                    {
                        value &= ~(1 << shift);
                        (start, end) = (startZeros, endZeros);
                    }
                }
            }

            return value;
        }
        #endregion
    }

    int CountLessThan(int k, int value)
    {
        if (value <= 0)
            return 0;

        var depth = matrix_.Length;
        if (value >= (1 << depth))
            return k;

        var count = 0;
        var current = k;
        for (var i = 0; i < depth; i++)
        {
            var bit = 0 != ((value >> (depth - i - 1)) & 1);
            var countZeros = matrix_[i].Rank0(current);
            if (!bit)
                current = countZeros;
            else
            {
                count += countZeros;
                current = zeros_[i] + matrix_[i].Rank1(current);
            }
        }

        return count;
    }

    // 
    // 

    /// <summary>
    /// Represents a value and its frequency of occurrence.
    /// </summary>
    /// <param name="Value">The element's value.</param>
    /// <param name="Frequency">The number of times the value occurred.</param>
    public record ValueWithFrequency(int Value, int Frequency);

    record KFindFrame(int Value, int Depth, int Start, int End, int Frequency);

    record Init(int Size, int[] Zeros, RankSelectBitSet[] Matrix);
}
