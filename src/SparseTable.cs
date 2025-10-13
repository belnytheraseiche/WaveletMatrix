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

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides a data structure for answering range queries on a static array in O(1) time
/// after an O(N log N) preprocessing step. This version is optimized for idempotent operations
/// like finding the minimum or maximum, and it also tracks the index of the result.
/// </summary>
/// <typeparam name="T">The type of elements in the array. Must be comparable.</typeparam>
/// <remarks>
/// An operation is idempotent if applying it multiple times to the same value does not change the result (e.g., min(x, x) = x).
/// This implementation is suitable for Range Minimum/Maximum Queries (RMQ).
/// </remarks>
public sealed class SparseTable<T> where T : IComparable<T>
{
    readonly ValueWithIndex[] table_;
    readonly int[] logs_;
    readonly int n_;
    readonly int k_;
    readonly Func<T, T, bool> comparer_;

    /// <summary>
    /// Gets the total number of elements in the sequence.
    /// </summary>
    public int Size => n_;

    /// <exclude />
    public Init InnerData => new(table_, n_);

    // 
    // 

    /// <exclude />
    public SparseTable(Init init, Func<T, T, bool> comparer)
    {
        (n_, comparer_) = (init.N, comparer);

        if (n_ == 0)
        {
            (table_, logs_) = ([], []);
            return;
        }

        k_ = (int)Math.Log(n_, 2.0) + 1;
        (table_, logs_) = (init.Table, new int[n_ + 1]);

        for (var i = 2; i <= n_; i++)
            logs_[i] = logs_[i >> 1] + 1;
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="SparseTable{T}"/> class.
    /// The constructor performs preprocessing which takes O(N log N) time and space.
    /// </summary>
    /// <param name="array">The static array of data to be queried. The array is not modified.</param>
    /// <param name="comparer">
    /// A function that compares two elements. It should return <c>true</c> if the first argument is considered "better"
    /// than the second (e.g., for a minimum query, the comparer would be `(a, b) =&gt; a.CompareTo(b) &lt; 0`).
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="comparer"/> is null.</exception>
    public SparseTable(ReadOnlySpan<T> array, Func<T, T, bool> comparer)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(comparer);
#else
        if (comparer == null)
            throw new ArgumentNullException(nameof(comparer));
#endif

        (n_, comparer_) = (array.Length, comparer);

        if (n_ == 0)
        {
            (table_, logs_) = ([], []);
            return;
        }

        k_ = (int)Math.Log(n_, 2.0) + 1;
        (table_, logs_) = (new ValueWithIndex[n_ * k_], new int[n_ + 1]);

        for (var i = 2; i <= n_; i++)
            logs_[i] = logs_[i >> 1] + 1;
        for (var i = 0; i < n_; i++)
            table_[i] = new(array[i], i);
        for (var i = 1; i < k_; i++)
        {
            var length = 1 << i;
            var half = length >> 1;
            for (var j = 0; j + length <= n_; j++)
            {
                var left = table_[(i - 1) * n_ + j];
                var right = table_[(i - 1) * n_ + j + half];
                table_[i * n_ + j] = comparer_(left.Value, right.Value) ? left : right;
            }
        }
    }

    /// <summary>
    /// Queries to find the best value according to the comparer and its original index.
    /// This operation takes O(1) time.
    /// </summary>
    /// <returns>A <see cref="ValueWithIndex"/> record containing the best value and its original index in the input array.</returns>
    /// <remarks>This method is a shorthand for calling Query(0, <see cref="Size"/>).</remarks>
    public ValueWithIndex Query()
    => Query(0, n_);

    /// <summary>
    /// Queries the range [start, end) to find the best value according to the comparer and its original index.
    /// This operation takes O(1) time.
    /// </summary>
    /// <param name="start">The inclusive, zero-based start of the range.</param>
    /// <param name="end">The exclusive, zero-based end of the range.</param>
    /// <returns>A <see cref="ValueWithIndex"/> record containing the best value and its original index in the input array.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> are outside the valid bounds.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public ValueWithIndex Query(int start, int end)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, n_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > n_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({n_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}.", nameof(start));

        var log = logs_[end - start];
        var left = table_[log * n_ + start];
        var right = table_[log * n_ + (end - (1 << log))];
        return comparer_(left.Value, right.Value) ? left : right;
    }

    // 
    // 

    /// <summary>
    /// Represents a value and its original index in the input array.
    /// </summary>
    /// <param name="Value">The element's value.</param>
    /// <param name="Index">The original zero-based index of the element.</param>
    public record struct ValueWithIndex(T Value, int Index);

    /// <exclude />
    public record Init(ValueWithIndex[] Table, int N);
}

/// <summary>
/// Provides a data structure for answering range queries on a static array.
/// This version is more general and supports non-idempotent operations like sum, but its Query operation is not O(1).
/// Preprocessing is O(N log N) and queries are O(log N) but dependent on the aggregator function's properties.
/// </summary>
/// <typeparam name="T">The type of elements in the array. Must be comparable.</typeparam>
/// <remarks>
/// This class is suitable for operations like Range Sum Query, where overlapping subproblems cannot be used in the same way as with idempotent operations.
/// Note: The provided Query method is a simplification and may not be O(1) for all aggregator functions. True O(log N) or O(1) for sum requires a different structure like a Fenwick Tree or prefix sum array.
/// </remarks>
public sealed class AggregateSparseTable<T> where T : IComparable<T>
{
    readonly T[] table_;
    readonly int[] logs_;
    readonly int n_;
    readonly int k_;
    readonly Func<T, T, T> aggregator_;

    /// <summary>
    /// Gets the total number of elements in the sequence.
    /// </summary>
    public int Size => n_;

    /// <exclude />
    public Init<T> InnerData => new(table_, n_);

    // 
    // 

    /// <exclude />
    public AggregateSparseTable(Init<T> init, Func<T, T, T> aggregator)
    {
        (n_, aggregator_) = (init.N, aggregator);

        if (n_ == 0)
        {
            (table_, logs_) = ([], []);
            return;
        }

        k_ = (int)Math.Log(n_, 2.0) + 1;
        (table_, logs_) = (init.Table, new int[n_ + 1]);

        for (var i = 2; i <= n_; i++)
            logs_[i] = logs_[i >> 1] + 1;
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="AggregateSparseTable{T}"/> class.
    /// The constructor performs preprocessing which takes O(N log N) time and space.
    /// </summary>
    /// <param name="array">The static array of data to be queried.</param>
    /// <param name="aggregator">A function that aggregates two elements (e.g., `(a, b) => a + b` for a sum).</param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="aggregator"/> is null.</exception>
    public AggregateSparseTable(ReadOnlySpan<T> array, Func<T, T, T> aggregator)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(aggregator);
#else
        if (aggregator == null)
            throw new ArgumentNullException(nameof(aggregator));
#endif

        n_ = array.Length;
        aggregator_ = aggregator;

        if (n_ == 0)
        {
            (table_, logs_) = ([], []);
            return;
        }

        k_ = (int)Math.Log(n_, 2.0) + 1;
        (table_, logs_) = (new T[n_ * k_], new int[n_ + 1]);

        for (var i = 2; i <= n_; i++)
            logs_[i] = logs_[i >> 1] + 1;
        for (var i = 0; i < n_; i++)
            table_[i] = array[i];
        for (var i = 1; i < k_; i++)
        {
            var length = 1 << i;
            var half = length >> 1;
            for (var j = 0; j + length <= n_; j++)
                table_[i * n_ + j] = aggregator_(table_[(i - 1) * n_ + j], table_[(i - 1) * n_ + j + half]);
        }
    }

    /// <summary>
    /// Queries to get an aggregated value.
    /// Note: The time complexity of this query is not guaranteed to be O(1) for non-idempotent operations.
    /// </summary>
    /// <returns>The aggregated value.</returns>
    /// <remarks>This method is a shorthand for calling Query(0, <see cref="Size"/>).</remarks>
    public T Query()
    => Query(0, n_);

    /// <summary>
    /// Queries the range [start, end) to get an aggregated value.
    /// Note: This operation takes O(log N) time for non-idempotent operations, but dependent on the aggregator function's properties.
    /// </summary>
    /// <param name="start">The inclusive, zero-based start of the range.</param>
    /// <param name="end">The exclusive, zero-based end of the range.</param>
    /// <returns>The aggregated value for the specified range.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> are outside the valid bounds.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal <paramref name="end"/>.</exception>
    public T Query(int start, int end)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, n_);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > n_)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({n_}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}.", nameof(start));

        T result = default!;
        var first = true;
        for (var i = k_ - 1; i >= 0; i--)
        {
            var offset = 1 << i;
            if (start + offset <= end)
            {
                if (!first)
                    result = aggregator_(result, table_[i * n_ + start]);
                else
                    (result, first) = (table_[i * n_ + start], false);
                start += offset;
            }
        }

        return result;
    }

    // 
    // 

    /// <exclude />
    public record Init<Tx>(Tx[] Table, int N);
}
