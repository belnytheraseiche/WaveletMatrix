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
/// A generic, compressed data structure for sequences of any comparable type.
/// It provides fast Rank, Select, Quantile, and other advanced queries.
/// This class uses coordinate compression internally, making it highly memory-efficient for data with a small alphabet size.
/// </summary>
/// <typeparam name="T">The type of elements in the sequence. Must implement <see cref="IComparable{T}"/>.</typeparam>
/// <example>
/// <code>
/// var data = new[] { "apple", "banana", "apple", "orange" };
/// var wm = WaveletMatrixGeneric&lt;string&gt;.Create(data);
/// int rank = wm.Rank(3, "apple"); // Counts "apple" in the range [0, 3). Result: 2
/// string value = wm.Access(1);    // Gets the element at index 1. Result: "banana"
/// </code>
/// </example>
public sealed class WaveletMatrixGeneric<T> where T : IComparable<T>
{
    readonly WaveletMatrixCore core_;
    readonly Dictionary<T, int> assignMap_;
    readonly T[] resolveMap_;

    /// <summary>
    /// Gets the total number of elements in the sequence.
    /// </summary>
    public int Size => core_.Size;

    // 
    // 

    WaveletMatrixGeneric(WaveletMatrixCore core, Init<T> init)
    {
        core_ = core;
        (assignMap_, resolveMap_) = init;
    }

    /// <summary>
    /// Creates a new instance of the <see cref="WaveletMatrixGeneric{T}"/> from a sequence of elements.
    /// This factory method performs coordinate compression on the input sequence before building the core data structure.
    /// </summary>
    /// <param name="sequence">The input sequence of data to be indexed.</param>
    /// <returns>A new, fully initialized <see cref="WaveletMatrixGeneric{T}"/> instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="sequence"/> is null.</exception>
    public static WaveletMatrixGeneric<T> Create(IEnumerable<T> sequence)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(sequence);
#else
        if (sequence == null)
            throw new ArgumentNullException(nameof(sequence));
#endif

        var array1 = sequence.ToArray();
        var array2 = new int[array1.Length];
        var elements = array1.Distinct().Order().ToArray();
        var assignMap = new Dictionary<T, int>(elements.Length);
        for (var i = 0; i < elements.Length; i++)
            assignMap.Add(elements[i], i);
        for (var i = 0; i < array1.Length; i++)
            array2[i] = assignMap[array1[i]];

        var core = WaveletMatrixCore.Create(array2);
        return new(core, new(assignMap, elements));
    }

    /// <summary>
    /// Serializes the <see cref="WaveletMatrixGeneric{T}"/> instance into a byte array.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="genericSerializer">
    /// An optional serializer for handling the generic type <typeparamref name="T"/>. 
    /// If null, a default serializer for <see cref="byte"/>, <see cref="int"/>, or <see cref="char"/> will be used if applicable.
    /// If a default serializer is not available for <typeparamref name="T"/>, an exception will be thrown.
    /// </param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="genericSerializer"/> is null and a default serializer for type <typeparamref name="T"/> cannot be found.</exception>
    /// <returns>A byte array containing the serialized data.</returns>
    public static byte[] Serialize(WaveletMatrixGeneric<T> obj, IGenericSerializer<T>? genericSerializer = null, SerializationOptions? options = null)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(obj);
#else
        if (obj == null)
            throw new ArgumentNullException(nameof(obj));
#endif

        using var memoryStream = new MemoryStream();
        Serialize(obj, memoryStream, genericSerializer, options);
        return memoryStream.ToArray();
    }

    /// <summary>
    /// Serializes the <see cref="WaveletMatrixGeneric{T}"/> instance to the specified file.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="file">The path of the file to write to.</param>
    /// <param name="genericSerializer">
    /// An optional serializer for handling the generic type <typeparamref name="T"/>. 
    /// If null, a default serializer for <see cref="byte"/>, <see cref="int"/>, or <see cref="char"/> will be used if applicable.
    /// If a default serializer is not available for <typeparamref name="T"/>, an exception will be thrown.
    /// </param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="file"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="genericSerializer"/> is null and a default serializer for type <typeparamref name="T"/> cannot be found.</exception>
    public static void Serialize(WaveletMatrixGeneric<T> obj, string file, IGenericSerializer<T>? genericSerializer = null, SerializationOptions? options = null)
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
        Serialize(obj, fileStream, genericSerializer, options);
    }

    /// <summary>
    /// Serializes the <see cref="WaveletMatrixGeneric{T}"/> instance to a stream.
    /// The data is compressed using Brotli and includes a checksum for integrity verification.
    /// </summary>
    /// <param name="obj">The instance to serialize.</param>
    /// <param name="stream">The stream to write the serialized data to.</param>
    /// <param name="genericSerializer">
    /// An optional serializer for handling the generic type <typeparamref name="T"/>. 
    /// If null, a default serializer for <see cref="byte"/>, <see cref="int"/>, or <see cref="char"/> will be used if applicable.
    /// If a default serializer is not available for <typeparamref name="T"/>, an exception will be thrown.
    /// </param>
    /// <param name="options">
    /// Serialization options, such as the compression level. 
    /// If null, <see cref="SerializationOptions.Default"/> will be used.
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="obj"/> or <paramref name="stream"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="genericSerializer"/> is null and a default serializer for type <typeparamref name="T"/> cannot be found.</exception>
    public static void Serialize(WaveletMatrixGeneric<T> obj, Stream stream, IGenericSerializer<T>? genericSerializer = null, SerializationOptions? options = null)
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

        if (genericSerializer == null)
        {
            var t = typeof(T);
            if (t == typeof(byte))
                genericSerializer = (IGenericSerializer<T>)ByteSerializer.Instance;
            else if (t == typeof(int))
                genericSerializer = (IGenericSerializer<T>)Int32Serializer.Instance;
            else if (t == typeof(char))
                genericSerializer = (IGenericSerializer<T>)CharSerializer.Instance;
            else
                throw new ArgumentException($"{nameof(genericSerializer)} of {t.Name} was not supplied.", nameof(genericSerializer));
        }
        if (genericSerializer.TypeIdentifier?.Length != 4)
            throw new ArgumentException($"{nameof(genericSerializer)}.TypeIdentifier must be 4 bytes.", nameof(genericSerializer));

        options ??= SerializationOptions.Default;

        var firstPosition = stream.Position;
        var xxh = new XxHash32();
        Span<byte> buffer0 = stackalloc byte[64];
        stream.Write(buffer0);

        // resolveMap
        var size1 = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, options.CompressionLevel);
                genericSerializer.WriteResolveMap(compressStream, obj.resolveMap_);
            }
            var array = memoryStream.ToArray();
            size1 = array.Length;
            xxh.Append(array);
            stream.Write(array);
        }

        var lastPosition = stream.Position;

        //  0: byte * 4, WMGR
        "WMGR"u8.CopyTo(buffer0);
        //  4: uint * 1, xxh
        BinaryPrimitives.WriteUInt32LittleEndian(buffer0[4..], xxh.GetCurrentHashAsUInt32());
        //  8: byte * 4, type identifier
        genericSerializer.TypeIdentifier.CopyTo(buffer0[8..]);
        // 12: int * 1, size of resolveMap
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[12..], size1);
        // 16- empty
        stream.Seek(firstPosition, SeekOrigin.Begin);
        stream.Write(buffer0);

        stream.Seek(lastPosition, SeekOrigin.Begin);

        // core
        WaveletMatrixCore.Serialize(obj.core_, stream, options);
    }

    /// <summary>
    /// Deserializes a byte array into a <see cref="WaveletMatrixGeneric{T}"/> instance.
    /// </summary>
    /// <param name="data">The byte array containing the serialized data.</param>
    /// <param name="genericSerializer">
    /// An optional serializer for handling the generic type <typeparamref name="T"/>. 
    /// If null, a default serializer for <see cref="byte"/>, <see cref="int"/>, or <see cref="char"/> will be used if applicable.
    /// If a default serializer is not available for <typeparamref name="T"/>, an exception will be thrown.
    /// </param>
    /// <returns>A new, deserialized instance of <see cref="WaveletMatrixGeneric{T}"/>.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="data"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="genericSerializer"/> is null and a default serializer for type <typeparamref name="T"/> cannot be found.</exception>
    public static WaveletMatrixGeneric<T> Deserialize(byte[] data, IGenericSerializer<T>? genericSerializer = null)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(data);
#else
        if (data == null)
            throw new ArgumentNullException(nameof(data));
#endif

        using var memoryStream = new MemoryStream(data);
        return Deserialize(memoryStream, genericSerializer);
    }

    /// <summary>
    /// Deserializes a <see cref="WaveletMatrixGeneric{T}"/> instance from the specified file.
    /// </summary>
    /// <param name="file">The path of the file to read from.</param>
    /// <param name="genericSerializer">
    /// An optional serializer for handling the generic type <typeparamref name="T"/>. 
    /// If null, a default serializer for <see cref="byte"/>, <see cref="int"/>, or <see cref="char"/> will be used if applicable.
    /// If a default serializer is not available for <typeparamref name="T"/>, an exception will be thrown.
    /// </param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="file"/> is null.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="genericSerializer"/> is null and a default serializer for type <typeparamref name="T"/> cannot be found.</exception>
    public static WaveletMatrixGeneric<T> Deserialize(string file, IGenericSerializer<T>? genericSerializer = null)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(file);
#else
        if (file == null)
            throw new ArgumentNullException(nameof(file));
#endif

        using var fileStream = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.Read);
        return Deserialize(fileStream, genericSerializer);
    }

    /// <summary>
    /// Deserializes a <see cref="WaveletMatrixGeneric{T}"/> instance from a stream.
    /// It verifies the file format, type identifier, and checksum.
    /// </summary>
    /// <param name="stream">The stream to read the serialized data from.</param>
    /// <param name="genericSerializer">
    /// An optional serializer for handling the generic type <typeparamref name="T"/>. 
    /// If null, a default serializer for <see cref="byte"/>, <see cref="int"/>, or <see cref="char"/> will be used if applicable.
    /// If a default serializer is not available for <typeparamref name="T"/>, an exception will be thrown.
    /// </param>
    /// <returns>A new, deserialized instance.</returns>
    /// <exception cref="ArgumentException">Thrown if <paramref name="genericSerializer"/> is null and a default serializer for type <typeparamref name="T"/> cannot be found.</exception>
    /// <exception cref="InvalidDataException">Thrown if the data format is unsupported, the type is incompatible, or the data is corrupt.</exception>
    public static WaveletMatrixGeneric<T> Deserialize(Stream stream, IGenericSerializer<T>? genericSerializer = null)
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
        if (!buffer0[..4].SequenceEqual("WMGR"u8))
            throw new InvalidDataException("Unsupported format.");

        if (genericSerializer == null)
        {
            var t = typeof(T);
            if (t == typeof(byte))
                genericSerializer = (IGenericSerializer<T>)ByteSerializer.Instance;
            else if (t == typeof(int))
                genericSerializer = (IGenericSerializer<T>)Int32Serializer.Instance;
            else if (t == typeof(char))
                genericSerializer = (IGenericSerializer<T>)CharSerializer.Instance;
            else
                throw new ArgumentException($"{nameof(genericSerializer)} of {t.Name} was not supplied.", nameof(genericSerializer));
        }
        if (genericSerializer.TypeIdentifier?.Length != 4)
            throw new ArgumentException($"{nameof(genericSerializer)}.TypeIdentifier must be 4 bytes.", nameof(genericSerializer));

        if (!buffer0[8..12].SequenceEqual(genericSerializer.TypeIdentifier))
            throw new InvalidDataException($"Type incompatible.");

        // resolveMap
        T[] resolveMap = [];
        Dictionary<T, int> assignMap = [];
        {
            var buffer1 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer0[12..])];
            stream.ReadExactly(buffer1);
            xxh.Append(buffer1);
            using var memoryStream = new MemoryStream(buffer1);
            using var decompressStream = new BrotliStream(memoryStream, CompressionMode.Decompress);
            resolveMap = genericSerializer.ReadResolveMap(decompressStream);
            for (var i = 0; i < resolveMap.Length; i++)
                assignMap.Add(resolveMap[i], i);
        }

        if (xxh.GetCurrentHashAsUInt32() != BinaryPrimitives.ReadUInt32LittleEndian(buffer0[4..]))
            throw new InvalidDataException("Broken.");

        var core = WaveletMatrixCore.Deserialize(stream);
        return new(core, new(assignMap, resolveMap));
    }

    /// <summary>
    /// Gets the value of the element at the specified zero-based position.
    /// </summary>
    /// <param name="k">The zero-based index of the element to retrieve.</param>
    /// <returns>The integer value at the specified position.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    public T Access(int k)
    => GetResolved(core_.Access(k));

    /// <summary>
    /// Counts the number of occurrences of a specified value in the prefix of the sequence [0, k).
    /// </summary>
    /// <param name="k">The exclusive end of the range [0, k).</param>
    /// <param name="value">The value to count.</param>
    /// <returns>The number of times the value appears in the specified prefix.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    public int Rank(int k, T value)
    => TryGetAssigned(value, out var assigned) ? core_.Rank(k, assigned) : 0;

    /// <summary>
    /// Finds the zero-based position of the k-th occurrence of a specified value.
    /// </summary>
    /// <param name="k">The one-based rank of the occurrence to find (e.g., k=1 for the first occurrence).</param>
    /// <param name="value">The value to search for.</param>
    /// <returns>The zero-based index of the k-th occurrence of the value, or -1 if not found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    public int Select(int k, T value)
    => TryGetAssigned(value, out var assigned) ? core_.Select(k, assigned) : -1;

    /// <summary>
    /// Counts the number of occurrences of a specified value.
    /// </summary>
    /// <param name="value">The value to count.</param>
    /// <returns>The number of occurrences of the value.</returns>
    /// <remarks>This method is a shorthand for calling RangeCount(0, <see cref="Size"/>, <paramref name="value"/>).</remarks>
    public int RangeCount(T value)
    => RangeCount(0, this.Size, value);

    /// <summary>
    /// Counts the number of occurrences of a specified value within the range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The value to count.</param>
    /// <returns>The number of occurrences of the value in the range.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public int RangeCount(int start, int end, T value)
    => TryGetAssigned(value, out var assigned) ? core_.RangeCount(start, end, assigned) : 0;

    /// <summary>
    /// Finds the k-th smallest value.
    /// </summary>
    /// <param name="k">The zero-based rank of the value to find (e.g., k=0 for the smallest value).</param>
    /// <returns>The k-th smallest value.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of the valid range [0, Size).</exception>
    /// <remarks>This method is a shorthand for calling Quantile(0, <see cref="Size"/>, <paramref name="k"/>).</remarks>
    public T Quantile(int k)
    => Quantile(0, this.Size, k);

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
    public T Quantile(int start, int end, int k)
    => GetResolved(core_.Quantile(start, end, k));

    /// <summary>
    /// Counts the number of elements whose values are within the value range [minValue, maxValue).
    /// </summary>
    /// <param name="minValue">The inclusive start of the value range.</param>
    /// <param name="maxValue">The exclusive end of the value range.</param>
    /// <returns>The count of elements matching the criteria.</returns>
    /// <exception cref="ArgumentException">Thrown if <paramref name="minValue"/> is greater than or equal <paramref name="maxValue"/>.</exception>
    /// <remarks>This method is a shorthand for calling RangeFreq(0, <see cref="Size"/>, <paramref name="minValue"/>, <paramref name="maxValue"/>).</remarks>
    public int RangeFreq(T minValue, T maxValue)
    => RangeFreq(0, this.Size, minValue, maxValue);

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
    public int RangeFreq(int start, int end, T minValue, T maxValue)
    => TryGetAssignedBounds(minValue, maxValue, out var lb, out var ub) ? core_.RangeFreq(start, end, lb, ub) : 0;

    /// <summary>
    /// Finds the most frequent value (the mode) and its frequency.
    /// </summary>
    /// <returns>A <see cref="ValueWithFrequency{T}"/> record containing the mode and its count.</returns>
    /// <remarks>This method is a shorthand for calling RangeMode(0, <see cref="Size"/>).</remarks>
    public ValueWithFrequency<T> RangeMode()
    => RangeMode(0, this.Size);

    /// <summary>
    /// Finds the most frequent value (the mode) and its frequency within the specified range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <returns>A <see cref="ValueWithFrequency{T}"/> record containing the mode and its count.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public ValueWithFrequency<T> RangeMode(int start, int end)
    {
        var result = core_.RangeMode(start, end);
        return new(GetResolved(result.Value), result.Frequency);
    }

    /// <summary>
    /// Finds the top K most frequent values and their frequencies.
    /// </summary>
    /// <param name="k">The number of top elements to retrieve.</param>
    /// <returns>An enumerable collection of <see cref="ValueWithFrequency{T}"/> records, ordered by frequency in descending order.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="k"/> is out of range.</exception>
    /// <remarks>This method is a shorthand for calling TopK(0, <see cref="Size"/>, <paramref name="k"/>).</remarks>
    public IEnumerable<ValueWithFrequency<T>> TopK(int k)
    => TopK(0, this.Size, k);

    /// <summary>
    /// Finds the top K most frequent values and their frequencies within the specified range [start, end).
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="k">The number of top elements to retrieve.</param>
    /// <returns>An enumerable collection of <see cref="ValueWithFrequency{T}"/> records, ordered by frequency in descending order.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/>, <paramref name="end"/> or <paramref name="k"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public IEnumerable<ValueWithFrequency<T>> TopK(int start, int end, int k)
    {
        foreach (var n in core_.TopK(start, end, k))
            yield return new(GetResolved(n.Value), n.Frequency);
    }

    /// <summary>
    /// Finds the predecessor of a given value.
    /// The predecessor is the largest value in the range that is strictly smaller than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="value">The reference value.</param>
    /// <param name="defaultValue">The value to return if no predecessor is found.</param>
    /// <returns>The predecessor value, or <paramref name="defaultValue"/> if no such value is found.</returns>
    /// <remarks>This method is a shorthand for calling SmallerValue(0, <see cref="Size"/>, <paramref name="value"/>, <paramref name="defaultValue"/>).</remarks>
    public T SmallerValue(T value, T defaultValue = default!)
    => SmallerValue(0, this.Size, value, defaultValue);

    /// <summary>
    /// Finds the predecessor of a given value in the specified range [start, end).
    /// The predecessor is the largest value in the range that is strictly smaller than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The reference value.</param>
    /// <param name="defaultValue">The value to return if no predecessor is found.</param>
    /// <returns>The predecessor value, or <paramref name="defaultValue"/> if no such value is found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public T SmallerValue(int start, int end, T value, T defaultValue = default!)
    => TrySmallerValue(start, end, value, out var result) ? result : defaultValue;

    /// <summary>
    /// Finds the predecessor of a given value.
    /// The predecessor is the largest value in the range that is strictly smaller than the given <paramref name="value"/>.
    /// This is the robust version that returns a boolean indicating success.
    /// </summary>
    /// <param name="value">The reference value.</param>
    /// <param name="result">When this method returns, contains the successor value if found; otherwise, the default value for the type.</param>
    /// <returns><c>true</c> if a predecessor value was found; otherwise, <c>false</c>.</returns>
    /// <remarks>This method is a shorthand for calling TrySmallerValue(0, <see cref="Size"/>, <paramref name="value"/>, out <paramref name="result"/>).</remarks>
    public bool TrySmallerValue(T value, out T result)
    => TrySmallerValue(0, this.Size, value, out result);

    /// <summary>
    /// Finds the predecessor of a given value in the specified range [start, end).
    /// The predecessor is the largest value in the range that is strictly smaller than the given <paramref name="value"/>.
    /// This is the robust version that returns a boolean indicating success.
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The reference value.</param>
    /// <param name="result">When this method returns, contains the successor value if found; otherwise, the default value for the type.</param>
    /// <returns><c>true</c> if a predecessor value was found; otherwise, <c>false</c>.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public bool TrySmallerValue(int start, int end, T value, out T result)
    {
        if (TryGetAssigned(value, out var assigned))
        {
            var found = core_.SmallerValue(start, end, assigned);
            if (found.HasValue)
            {
                result = GetResolved(found.Value);
                return true;
            }
        }

        result = default!;
        return false;
    }

    /// <summary>
    /// Finds the successor of a given value.
    /// The successor is the smallest value in the range that is strictly larger than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="value">The reference value.</param>
    /// <param name="defaultValue">The value to return if no successor is found.</param>
    /// <returns>The successor value, or <paramref name="defaultValue"/> if no such value is found.</returns>
    /// <remarks>This method is a shorthand for calling LargerValue(0, <see cref="Size"/>, <paramref name="value"/>, <paramref name="defaultValue"/>).</remarks>
    public T LargerValue(T value, T defaultValue = default!)
    => LargerValue(0, this.Size, value, defaultValue);

    /// <summary>
    /// Finds the successor of a given value in the specified range [start, end).
    /// The successor is the smallest value in the range that is strictly larger than the given <paramref name="value"/>.
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The reference value.</param>
    /// <param name="defaultValue">The value to return if no successor is found.</param>
    /// <returns>The successor value, or <paramref name="defaultValue"/> if no such value is found.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public T LargerValue(int start, int end, T value, T defaultValue = default!)
    => TryLargerValue(start, end, value, out var result) ? result : defaultValue;

    /// <summary>
    /// Finds the successor of a given value.
    /// The successor is the smallest value in the range that is strictly larger than the given <paramref name="value"/>.
    /// This is the robust version that returns a boolean indicating success.
    /// </summary>
    /// <param name="value">The reference value.</param>
    /// <param name="result">When this method returns, contains the successor value if found; otherwise, the default value for the type.</param>
    /// <returns><c>true</c> if a predecessor value was found; otherwise, <c>false</c>.</returns>
    /// <remarks>This method is a shorthand for calling TryLargerValue(0, <see cref="Size"/>, <paramref name="value"/>, out <paramref name="result"/>).</remarks>
    public bool TryLargerValue(T value, out T result)
    => TryLargerValue(0, this.Size, value, out result);

    /// <summary>
    /// Finds the successor of a given value in the specified range [start, end).
    /// The successor is the smallest value in the range that is strictly larger than the given <paramref name="value"/>.
    /// This is the robust version that returns a boolean indicating success.
    /// </summary>
    /// <param name="start">The inclusive start of the range.</param>
    /// <param name="end">The exclusive end of the range.</param>
    /// <param name="value">The reference value.</param>
    /// <param name="result">When this method returns, contains the successor value if found; otherwise, the default value for the type.</param>
    /// <returns><c>true</c> if a predecessor value was found; otherwise, <c>false</c>.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public bool TryLargerValue(int start, int end, T value, out T result)
    {
        if (TryGetAssigned(value, out var assigned))
        {
            var found = core_.LargerValue(start, end, assigned);
            if (found.HasValue)
            {
                result = GetResolved(found.Value);
                return true;
            }
        }

        result = default!;
        return false;
    }

    T GetResolved(int value)
    => resolveMap_[value];

    bool TryGetAssigned(T value, out int assigned)
    => assignMap_.TryGetValue(value, out assigned);

    bool TryGetAssignedBounds(T minValue, T maxValue, out int lower, out int upper)
    {
        (lower, upper) = (default, default);
        if (minValue.CompareTo(maxValue) >= 0)
            return false;

        (lower, upper) = (__BinarySearch(resolveMap_, minValue), __BinarySearch(resolveMap_, maxValue));
        if (lower > upper || lower >= resolveMap_.Length)
            return false;

        return true;

        #region @@
        static int __BinarySearch(T[] array, T value)
        {
            var (l, r) = (0, array.Length);
            while (l < r)
            {
                var m = l + (r - l) / 2;
                if (array[m].CompareTo(value) < 0)
                    l = m + 1;
                else
                    r = m;
            }
            return l;
        }
        #endregion
    }

    // 
    // 

    /// <summary>
    /// Defines the contract for serializing and deserializing elements of type <typeparamref name="Tx"/>.
    /// </summary>
    /// <typeparam name="Tx">The type to be serialized.</typeparam>
    public interface IGenericSerializer<Tx> where Tx : IComparable<Tx>
    {
        /// <summary>
        /// Gets a unique 4-byte identifier for the type <typeparamref name="Tx"/>.
        /// This identifier is written to the file header to ensure type safety during deserialization.
        /// </summary>
        /// <example><code>public byte[] TypeIdentifier => "INT_";</code></example>
        byte[] TypeIdentifier { get; }

        /// <summary>
        /// Writes the resolve map (the dictionary of unique values) to the stream.
        /// </summary>
        /// <param name="stream">The stream to write to. Typically a <see cref="BrotliStream"/> for compression.</param>
        /// <param name="resolveMap">The array of unique values to serialize.</param>
        void WriteResolveMap(Stream stream, Tx[] resolveMap);

        /// <summary>
        /// Reads the resolve map from the stream.
        /// </summary>
        /// <param name="stream">The stream to read from. Typically a <see cref="BrotliStream"/> for decompression.</param>
        /// <returns>An array containing the deserialized unique values.</returns>
        Tx[] ReadResolveMap(Stream stream);
    }

    /// <summary>
    /// Provides a default serializer for the <see cref="byte"/> type.
    /// </summary>
    public sealed class ByteSerializer : IGenericSerializer<byte>
    {
        /// <summary>
        /// Gets the singleton instance of the <see cref="ByteSerializer"/>.
        /// </summary>
        public static IGenericSerializer<byte> Instance { get; } = new ByteSerializer();

        /// <inheritdoc/>
        public byte[] TypeIdentifier { get; } = "BYTE"u8.ToArray();

        /// <inheritdoc/>
        public void WriteResolveMap(Stream stream, byte[] resolveMap)
        {
            Span<byte> buffer = stackalloc byte[4];
            BinaryPrimitives.WriteInt32LittleEndian(buffer, resolveMap.Length);
            stream.Write(buffer);
            stream.Write(resolveMap);
        }

        /// <inheritdoc/>
        public byte[] ReadResolveMap(Stream stream)
        {
            Span<byte> buffer1 = stackalloc byte[4];
            stream.ReadExactly(buffer1);
            var buffer2 = new byte[BinaryPrimitives.ReadInt32LittleEndian(buffer1)];
            stream.ReadExactly(buffer2);
            return buffer2;
        }
    }

    /// <summary>
    /// Provides a default serializer for the <see cref="char"/> type.
    /// </summary>
    public sealed class CharSerializer : IGenericSerializer<char>
    {
        /// <summary>
        /// Gets the singleton instance of the <see cref="CharSerializer"/>.
        /// </summary>
        public static IGenericSerializer<char> Instance { get; } = new CharSerializer();

        /// <inheritdoc/>
        public byte[] TypeIdentifier { get; } = "CHAR"u8.ToArray();

        /// <inheritdoc/>
        public void WriteResolveMap(Stream stream, char[] resolveMap)
        {
            Span<byte> buffer1 = stackalloc byte[4];
            BinaryPrimitives.WriteInt32LittleEndian(buffer1, resolveMap.Length);
            stream.Write(buffer1);
            if (BitConverter.IsLittleEndian)
                stream.Write(MemoryMarshal.AsBytes(resolveMap.AsSpan()));
            else
            {
                var buffer2 = new byte[32768];
                var offset = 0;
                while (offset < resolveMap.Length)
                {
                    var length = Math.Min(16384, resolveMap.Length - offset);
                    var span = resolveMap.AsSpan(offset, length);
                    for (var i = 0; i < span.Length; i++)
                        BinaryPrimitives.WriteUInt16LittleEndian(buffer2.AsSpan(i * 2, 2), (ushort)span[i]);
                    stream.Write(buffer2.AsSpan(0, 2 * length));
                    offset += length;
                }
            }
        }

        /// <inheritdoc/>
        public char[] ReadResolveMap(Stream stream)
        {
            Span<byte> buffer1 = stackalloc byte[4];
            stream.ReadExactly(buffer1);
            var length = BinaryPrimitives.ReadInt32LittleEndian(buffer1);
            var buffer2 = new char[length];
            var bytes = MemoryMarshal.AsBytes(buffer2.AsSpan());
            stream.ReadExactly(bytes);
            if (!BitConverter.IsLittleEndian)
                for (var i = 0; i < length; i++)
                    buffer2[i] = (char)BinaryPrimitives.ReadUInt16LittleEndian(bytes[(i * 2)..]);
            return buffer2;
        }
    }

    /// <summary>
    /// Provides a default serializer for the <see cref="int"/> type.
    /// </summary>
    public sealed class Int32Serializer : IGenericSerializer<int>
    {
        /// <summary>
        /// Gets the singleton instance of the <see cref="Int32Serializer"/>.
        /// </summary>
        public static IGenericSerializer<int> Instance { get; } = new Int32Serializer();

        /// <inheritdoc/>
        public byte[] TypeIdentifier { get; } = "I32_"u8.ToArray();

        /// <inheritdoc/>
        public void WriteResolveMap(Stream stream, int[] resolveMap)
        {
            Span<byte> buffer1 = stackalloc byte[4];
            BinaryPrimitives.WriteInt32LittleEndian(buffer1, resolveMap.Length);
            stream.Write(buffer1);
            if (BitConverter.IsLittleEndian)
                stream.Write(MemoryMarshal.AsBytes(resolveMap.AsSpan()));
            else
            {
                var buffer2 = new byte[65536];
                var offset = 0;
                while (offset < resolveMap.Length)
                {
                    var length = Math.Min(16384, resolveMap.Length - offset);
                    var span = resolveMap.AsSpan(offset, length);
                    for (var i = 0; i < span.Length; i++)
                        BinaryPrimitives.WriteInt32LittleEndian(buffer2.AsSpan(i * 4, 4), span[i]);
                    stream.Write(buffer2.AsSpan(0, 4 * length));
                    offset += length;
                }
            }
        }

        /// <inheritdoc/>
        public int[] ReadResolveMap(Stream stream)
        {
            Span<byte> buffer1 = stackalloc byte[4];
            stream.ReadExactly(buffer1);
            var length = BinaryPrimitives.ReadInt32LittleEndian(buffer1);
            var buffer2 = new int[length];
            var bytes = MemoryMarshal.AsBytes(buffer2.AsSpan());
            stream.ReadExactly(bytes);
            if (!BitConverter.IsLittleEndian)
                for (var i = 0; i < length; i++)
                    buffer2[i] = BinaryPrimitives.ReadInt32LittleEndian(bytes[(i * 4)..]);
            return buffer2;
        }
    }

    /// <summary>
    /// Represents a value and its frequency of occurrence.
    /// </summary>
    /// <param name="Value">The element's value.</param>
    /// <param name="Frequency">The number of times the value occurred.</param>
    public record ValueWithFrequency<Tx>(Tx Value, int Frequency);

    record Init<Tx>(Dictionary<Tx, int> AssignMap, Tx[] ResolveMap) where Tx : IComparable<T>;
}
