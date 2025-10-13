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

using System.Collections;
using System.Numerics;
using System.Runtime.CompilerServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Represents a buffer for value types, implemented using a list of array chunks.
/// </summary>
/// <typeparam name="T">The type of element, which must be a struct.</typeparam>
/// <remarks>
/// This class is designed to manage large collections of value types without allocating on the Large Object Heap (LOH).
/// It achieves this by storing elements in a series of smaller array "chunks". The size of these chunks, specified by <see cref="ElementsPerChunk"/>, must be a power of 2.
/// </remarks>
public sealed class ValueBuffer<T> : ICollection<T> where T : struct
{
    /// <summary>
    /// Gets the size of each internal array chunk.
    /// </summary>
    public int ElementsPerChunk { get; }
    /// <summary>
    /// Gets the number of internal array chunks that are currently allocated.
    /// </summary>
    public int NumberOfChunks => chunks_.Count;
    /// <summary>
    /// Gets the number of elements that are considered to be in use within the buffer.
    /// This value is updated when an element is written to an index greater than the current used count.
    /// </summary>
    public int Used => used_;
    /// <summary>
    /// Gets the total number of elements that can be stored across all currently allocated chunks.
    /// </summary>
    public int Capacity => capacity_;
    /// <summary>
    /// Gets a value indicating whether the buffer will automatically allocate new chunks when an index outside the current capacity is accessed.
    /// </summary>
    public bool ExtendAutomatically { get; }
    /// <summary>
    /// Gets the number of elements contained in the <see cref="ValueBuffer{T}"/>.
    /// This property is an implementation for the <see cref="ICollection{T}"/> interface and is an alias for the <see cref="Used"/> property.
    /// </summary>
    public int Count => used_;
    /// <summary>
    /// Gets a value indicating whether the <see cref="ValueBuffer{T}"/> is read-only.
    /// </summary>
    public bool IsReadOnly => false;

    /// <summary>
    /// Gets or sets the element at the specified index.
    /// </summary>
    /// <param name="index">The zero-based index of the element to get or set.</param>
    /// <returns>A reference to the element at the specified index.</returns>
    /// <remarks>
    /// Accessing an index greater than or equal to the current <see cref="Used"/> count will update the count to <c>index + 1</c>.
    /// If <see cref="ExtendAutomatically"/> is true, accessing an index beyond the current <see cref="Capacity"/> will allocate new chunks.
    /// </remarks>
    /// <exception cref="ArgumentOutOfRangeException">
    /// Thrown if <paramref name="index"/> is negative, or if <paramref name="index"/> is greater than or equal to <see cref="Capacity"/> and <see cref="ExtendAutomatically"/> is false.
    /// </exception>
    public ref T this[int index]
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
#if NET8_0_OR_GREATER
            ArgumentOutOfRangeException.ThrowIfNegative(index);
#else
            if (index < 0)
                throw new ArgumentOutOfRangeException(nameof(index));
#endif

            if (index >= capacity_)
            {
                if (ExtendAutomatically)
                    ExtendCapacity(index + 1);
                else
                    throw new ArgumentOutOfRangeException(nameof(index), $"{nameof(index)} must be less than {capacity_}.");
            }

            if (index >= used_)
                used_ = index + 1;
            // return ref buffers_[index / ArrayAlignment][index % ArrayAlignment];
            return ref chunks_[index >> shift_][index & (ElementsPerChunk - 1)];
        }
    }

    readonly List<T[]> chunks_ = [];
    int used_;
    int capacity_;
    readonly int shift_;

    //
    // 

    /// <summary>
    /// Initializes a new instance of the <see cref="ValueBuffer{T}"/> class.
    /// </summary>
    /// <param name="elementsPerChunk">The size of each internal array chunk. This value must be a power of 2.</param>
    /// <param name="extendAutomatically">A value indicating whether to automatically extend the buffer's capacity when an out-of-bounds index is accessed.</param>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="elementsPerChunk"/> is not positive.</exception>
    /// <exception cref="ArgumentException"><paramref name="elementsPerChunk"/> is not a power of 2.</exception>
    public ValueBuffer(int elementsPerChunk, bool extendAutomatically = false)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(elementsPerChunk);
#else
        if (elementsPerChunk <= 0)
            throw new ArgumentOutOfRangeException(nameof(elementsPerChunk));
#endif
        if ((elementsPerChunk & (elementsPerChunk - 1)) != 0)
            throw new ArgumentException($"{nameof(elementsPerChunk)} must be a power of 2.");

        this.ElementsPerChunk = elementsPerChunk;
        this.ExtendAutomatically = extendAutomatically;
        shift_ = BitOperations.TrailingZeroCount(elementsPerChunk);
    }

    /// <summary>
    /// Copies the used elements from the <see cref="ValueBuffer{T}"/> to a new, single contiguous array.
    /// </summary>
    /// <returns>A new array containing the elements from the buffer.</returns>
    public T[] ToArray()
    {
        var array = new T[used_];
        for (var i = 0; i < chunks_.Count; i++)
            Array.Copy(chunks_[i], 0, array, i * ElementsPerChunk, Math.Min(ElementsPerChunk, used_ - (i * ElementsPerChunk)));
        return array;
    }

    /// <summary>
    /// Removes all elements from the <see cref="ValueBuffer{T}"/> and releases all internal array chunks.
    /// </summary>
    public void Clear()
    {
        chunks_.Clear();
        capacity_ = used_ = 0;
    }

    /// <summary>
    /// Adds an element to the end of the <see cref="ValueBuffer{T}"/>.
    /// </summary>
    /// <param name="value">The element to add.</param>
    public void Add(T value)
    => this[used_] = value;

    /// <summary>
    /// Ensures that the capacity of this buffer is at least the specified value.
    /// </summary>
    /// <param name="minCapacity">The minimum required capacity.</param>
    /// <returns>The new capacity of the buffer, which will be a multiple of <see cref="ElementsPerChunk"/>.</returns>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="minCapacity"/> is negative.</exception>
    public int ExtendCapacity(int minCapacity)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(minCapacity);
#else
        if (minCapacity < 0)
            throw new ArgumentOutOfRangeException(nameof(minCapacity));
#endif

        var capacity = (minCapacity & (ElementsPerChunk - 1)) != 0 ? (minCapacity + ElementsPerChunk) & ~(ElementsPerChunk - 1) : (minCapacity == 0 ? ElementsPerChunk : minCapacity);
        var loop = (capacity - capacity_) >> shift_;
        while (loop-- > 0)
            chunks_.Add(new T[this.ElementsPerChunk]);
        return capacity_ = capacity;
    }

    /// <summary>
    /// Returns an enumerable collection of the internal array chunks.
    /// </summary>
    /// <returns>An <see cref="IEnumerable{T}"/> (<typeparamref name="T"/> is an array) that allows iteration over the raw internal buffers.</returns>
    /// <remarks>This method is intended for advanced scenarios where direct read-only access to the underlying chunks is required.</remarks>
    public IEnumerable<T[]> EnumerateChunks()
    => chunks_;

    /// <summary>
    /// Appends a pre-allocated array chunk directly to the internal list of buffers, increasing the capacity.
    /// </summary>
    /// <param name="chunk">The array to append. Its length must be exactly equal to <see cref="ElementsPerChunk"/>.</param>
    /// <remarks>This is an advanced optimization method that avoids an array allocation and copy by using a caller-provided array.</remarks>
    /// <exception cref="ArgumentNullException"><paramref name="chunk"/> is null.</exception>
    /// <exception cref="ArgumentException">The length of <paramref name="chunk"/> is not equal to <see cref="ElementsPerChunk"/>.</exception>
    public void AppendChunkDirectly(T[] chunk)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(chunk);
#else
        if (chunk == null)
            throw new ArgumentNullException(nameof(chunk));
#endif
        if (chunk.Length != this.ElementsPerChunk)
            throw new ArgumentException($"Length of {nameof(chunk)} is not equals {this.ElementsPerChunk}.");

        chunks_.Add(chunk);
        capacity_ += this.ElementsPerChunk;
    }

    /// <summary>
    /// Directly sets the number of used elements and optionally shrinks the buffer's capacity.
    /// </summary>
    /// <param name="used">The new count of used elements. This value must be between 0 and <see cref="Capacity"/>.</param>
    /// <param name="shrinkAutomatically">If true, deallocates unused array chunks from the end of the buffer to fit the new used count.</param>
    /// <remarks>
    /// This is an advanced method and should be used with caution. Setting an incorrect value can lead to an inconsistent state or data corruption.
    /// </remarks>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="used"/> is negative or greater than <see cref="Capacity"/>.</exception>
    public void SetUsedDirectly(int used, bool shrinkAutomatically = false)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(used);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(used, capacity_);
#else
        if (used < 0)
            throw new ArgumentOutOfRangeException(nameof(used));
        if (used > capacity_)
            throw new ArgumentOutOfRangeException(nameof(used), $"{nameof(used)} must be less than or equal {capacity_}.");
#endif

        used_ = used;
        if (shrinkAutomatically && used_ + this.ElementsPerChunk >= capacity_)
        {
            // var capacity = used_ % ArrayAlignment != 0 ? used_ + ArrayAlignment - (used_ % ArrayAlignment) : (used_ == 0 ? ArrayAlignment : used_);
            // var loop = (capacity_ - capacity) / ArrayAlignment;
            var capacity = (used_ & (ElementsPerChunk - 1)) != 0 ? (used_ + ElementsPerChunk) & ~(ElementsPerChunk - 1) : (used_ == 0 ? ElementsPerChunk : used_);
            var loop = (capacity_ - capacity) >> shift_;
            while (loop-- > 0)
                chunks_.RemoveAt(chunks_.Count - 1);
            capacity_ = capacity;
        }
    }

    /// <summary>
    /// Fills a range of elements in the buffer with a specified value.
    /// </summary>
    /// <param name="value">The value to assign to each element in the range.</param>
    /// <param name="index">The zero-based starting index of the range to fill.</param>
    /// <param name="count">The number of elements in the range to fill.</param>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="index"/> or <paramref name="count"/> is negative.</exception>
    /// <exception cref="ArgumentException">The sum of <paramref name="index"/> and <paramref name="count"/> is greater than the buffer's capacity and <see cref="ExtendAutomatically"/> is false.</exception>
    public void Fill(T value, int index, int count)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(index);
        ArgumentOutOfRangeException.ThrowIfNegative(count);
#else
        if (index < 0)
            throw new ArgumentOutOfRangeException(nameof(index));
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));
#endif

        if (count == 0)
            return;

        if (index + count > capacity_)
        {
            if (!this.ExtendAutomatically)
                throw new ArgumentException($"{nameof(index)}+{nameof(count)} must less than {capacity_}.");
            ExtendCapacity(index + count);
        }

        var (ai1, bi1) = (index >> shift_, index & (ElementsPerChunk - 1));
        var (ai2, bi2) = ((index + count - 1) >> shift_, (index + count - 1) & (ElementsPerChunk - 1));
        if (ai1 == ai2)
            Array.Fill(chunks_[ai1], value, bi1, count);
        else
        {
            Array.Fill(chunks_[ai1], value, bi1, this.ElementsPerChunk - bi1);
            for (var i = ai1 + 1; i < ai2; i++)
                Array.Fill(chunks_[i], value);
            Array.Fill(chunks_[ai2], value, 0, bi2 + 1);
        }
        used_ = index + count;
    }

    /// <summary>
    /// Returns an enumerator that iterates through the used elements in the buffer.
    /// </summary>
    /// <returns>An enumerator for the <see cref="ValueBuffer{T}"/>.</returns>
    public IEnumerator<T> GetEnumerator()
    {
        for (var i = 0; i < used_; i++)
            yield return this[i];
    }

    IEnumerator IEnumerable.GetEnumerator()
    => GetEnumerator();

    /// <summary>
    /// Determines whether the <see cref="ValueBuffer{T}"/> contains a specific value.
    /// </summary>
    /// <param name="item">The object to locate in the <see cref="ValueBuffer{T}"/>.</param>
    /// <returns>true if item is found in the <see cref="ValueBuffer{T}"/>; otherwise, false.</returns>
    public bool Contains(T item)
    {
        for (var i = 0; i < used_; i++)
            if (this[i].Equals(item))
                return true;
        return false;
    }

    /// <summary>
    /// Copies the elements of the <see cref="ValueBuffer{T}"/> to an <see cref="Array"/>, starting at a particular <see cref="Array"/> index.
    /// </summary>
    /// <param name="array">The one-dimensional <see cref="Array"/> that is the destination of the elements copied from <see cref="ValueBuffer{T}"/>.</param>
    /// <param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.</param>
    /// <exception cref="ArgumentNullException"><paramref name="array"/> is null.</exception>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.</exception>
    /// <exception cref="ArgumentException">The number of elements in the source buffer is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.</exception>
    public void CopyTo(T[] array, int arrayIndex)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(array);
        ArgumentOutOfRangeException.ThrowIfNegative(arrayIndex);
#else
        if (array == null)
            throw new ArgumentNullException(nameof(array));
        if (arrayIndex < 0)
            throw new ArgumentOutOfRangeException(nameof(arrayIndex));
#endif

        if (used_ > array.Length - arrayIndex)
            throw new ArgumentException($"Capacity of {array} must be grater than or equal {used_}.");

        for (var i = 0; i < used_; i++)
            array[i + arrayIndex] = this[i];
    }

    /// <summary>
    /// Finds the first occurrence of an item and replaces it with the default value of type <typeparamref name="T"/>.
    /// </summary>
    /// <param name="item">The item to locate and replace.</param>
    /// <returns>true if the item was found and replaced; otherwise, false.</returns>
    /// <remarks>
    /// This method does not actually remove the element in a way that would shift subsequent elements. It only overwrites the found element with <c>default(T)</c>.
    /// The <see cref="Count"/> and <see cref="Used"/> properties are not changed by this operation.
    /// </remarks>
    public bool Remove(T item)
    {
        // throw new NotImplementedException();
        for (var i = 0; i < used_; i++)
        {
            if (this[i].Equals(item))
            {
                this[i] = default;
                return true;
            }
        }
        return false;
    }
}
