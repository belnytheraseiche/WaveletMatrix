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

using System.Buffers;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides a data structure for answering range queries on a static array in O(1) time
/// after an O(N log N) preprocessing step. This version is optimized for **idempotent** operations
/// like finding the minimum or maximum, and it also tracks the index of the result.
/// </summary>
/// <typeparam name="T">The type of elements in the array. Must be comparable.</typeparam>
/// <remarks>
/// An operation is idempotent if applying it multiple times to the same value does not change the result (e.g., min(x, x) = x).
/// This implementation is a classic and efficient solution for the static Range Minimum/Maximum Query (RMQ) problem.
/// For an O(N) preprocessing solution, see <see cref="FischerHeunSparseTable{T}"/>.
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
    /// The constructor performs preprocessing which takes O(N log N) time and O(N log N) space.
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
/// Provides a general-purpose data structure for answering range aggregation queries on a static array.
/// Preprocessing is O(N log N), and queries are **O(log N)**.
/// </summary>
/// <typeparam name="T">The type of elements in the array. Must be comparable.</typeparam>
/// <remarks>
/// This class is suitable for **non-idempotent** (and associative) operations like Range Sum Query.
/// Unlike <see cref="SparseTable{T}"/>, it correctly handles operations where overlapping subproblems would lead to incorrect results.
/// For faster prefix-sum-based queries, consider using a Fenwick Tree.
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
    /// The constructor performs preprocessing which takes O(N log N) time and O(N log N) space.
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
    /// Queries the range [start, end) to get an aggregated value.
    /// This operation takes **O(log N)** time.
    /// </summary>
    /// <returns>The aggregated value.</returns>
    /// <remarks>This method is a shorthand for calling Query(0, <see cref="Size"/>).</remarks>
    public T Query()
    => Query(0, n_);

    /// <summary>
    /// Queries the range [start, end) to get an aggregated value.
    /// This operation takes **O(log N)** time.
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

/// <summary>
/// Provides a data structure for answering range queries on a static array in **O(1)** time
/// after an **O(N)** preprocessing step, based on the Fischer-Heun structure.
/// This implementation is the state-of-the-art solution for idempotent operations like finding the minimum or maximum.
/// </summary>
/// <typeparam name="T">The type of elements in the array. Must be comparable.</typeparam>
/// <remarks>
/// This advanced algorithm achieves O(1) query time with O(N) space by dividing the input array into small blocks.
/// It precomputes answers for all query patterns within the small blocks and uses a standard <see cref="SparseTable{T}"/>
/// to answer queries across blocks. This makes it one of the asymptotically fastest solutions for the static Range Minimum/Maximum Query (RMQ) problem.
/// </remarks>
public sealed class FischerHeunSparseTable<T> where T : IComparable<T>
{
    static readonly ArrayPool<int> intPool_ = ArrayPool<int>.Shared;

    readonly ReadOnlyMemory<T> memory_;
    readonly int blockSize_;
    readonly int blockCount_;
    readonly int[] blockMins_;
    readonly ulong[] blockTypes_;
    readonly SparseTable<int> table_ = null!;
    readonly Dictionary<ulong, int[,]> patternRmq_ = [];
    readonly Func<T, T, bool> comparer_;

    /// <summary>
    /// Gets the total number of elements in the sequence.
    /// </summary>
    public int Size => memory_.Length;

    /// <exclude />
    public Init InnerData => new(table_.InnerData, memory_, blockSize_, blockCount_, blockMins_, blockTypes_, patternRmq_);

    // 
    // 

    /// <exclude />
    public FischerHeunSparseTable(Init init, Func<T, T, bool> comparer)
    {
        (memory_, blockSize_, blockCount_, blockMins_, blockTypes_, patternRmq_, comparer_) = (init.Memory, init.BlockSize, init.BlockCount, init.BlockMins, init.BlockTypes, init.PatternRmq, comparer);
        table_ = new(init.StInit, (i, j) => comparer_(memory_.Span[i], memory_.Span[j]));
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="FischerHeunSparseTable{T}"/> class.
    /// The constructor performs preprocessing which takes O(N) time and space.
    /// </summary>
    /// <param name="memory">The static array of data to be queried, represented as a ReadOnlyMemory.</param>
    /// <param name="comparer">
    /// A function that compares two elements. It should return <c>true</c> if the first argument is considered "better"
    /// than the second (e.g., for a minimum query, the comparer would be `(a, b) => a.CompareTo(b) &lt; 0`).
    /// </param>
    /// <exception cref="ArgumentNullException">Thrown if <paramref name="comparer"/> is null.</exception>
    public FischerHeunSparseTable(ReadOnlyMemory<T> memory, Func<T, T, bool> comparer)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(comparer);
#else
        if (comparer == null)
            throw new ArgumentNullException(nameof(comparer));
#endif

        (memory_, var memoryLength, comparer_) = (memory, memory.Length, comparer);
        if (memoryLength == 0)
        {
            (blockMins_, blockTypes_, table_) = ([], [], new(((int[])[]).AsSpan(), (i, j) => comparer_(memory_.Span[i], memory_.Span[j])));
            return;
        }

        blockSize_ = Math.Max(1, (int)Math.Log(memoryLength, 2.0) / 2);
        blockCount_ = (memoryLength + blockSize_ - 1) / blockSize_;
        (blockMins_, blockTypes_) = (new int[blockCount_], new ulong[blockCount_]);

        var span = memory_.Span;
        for (var b = 0; b < blockCount_; b++)
        {
            var start = b * blockSize_;
            var end = Math.Min(memoryLength, start + blockSize_);

            var min = start;
            for (var i = start + 1; i < end; i++)
                if (comparer_(span[i], span[min]))
                    min = i;
            blockMins_[b] = min;

            var shape = __EncodeCartesianShape(span, start, end, comparer_);
            blockTypes_[b] = shape;
            if (!patternRmq_.ContainsKey(shape))
                patternRmq_[shape] = __PrecomputeBlockRmqFromShape(shape);
        }

        table_ = new SparseTable<int>(blockMins_, (i, j) => comparer_(memory_.Span[i], memory_.Span[j]));

        #region @@
        static ulong __EncodeCartesianShape(ReadOnlySpan<T> array, int start, int end, Func<T, T, bool> comparer)
        {
            var length = end - start;
            if (length <= 0)
                return 0ul;

            var (rent1, rent2, rent3) = (intPool_.Rent(length), intPool_.Rent(length), intPool_.Rent(length));
            try
            {
                var left = rent1.AsSpan(0, length);
                var right = rent2.AsSpan(0, length);
                var parent = rent3.AsSpan(0, length);
                left.Fill(-1);
                right.Fill(-1);
                parent.Fill(-1);

                var stack = new Stack<int>(length);
                for (var i = 0; i < length; i++)
                {
                    var last = -1;
                    while (stack.TryPeek(out var tmp1) && comparer(array[start + i], array[start + tmp1]))
                        last = stack.Pop();
                    if (stack.Count != 0)
                        right[parent[i] = stack.Peek()] = i;
                    if (last != -1)
                        parent[left[i] = last] = i;
                    stack.Push(i);
                }

                var root = stack.Peek();
                while (parent[root] != -1)
                    root = parent[root];

                var (shape, bitPosition) = (0, 0);
                __Dfs(root, left, right, ref shape, ref bitPosition);
                return ((ulong)length << 32) | (uint)shape;

                #region @@
                static void __Dfs(int u, ReadOnlySpan<int> left, ReadOnlySpan<int> right, ref int shape, ref int bitPosition)
                {
                    shape |= 1 << bitPosition;
                    bitPosition++;

                    if (left[u] != -1)
                        __Dfs(left[u], left, right, ref shape, ref bitPosition);
                    if (right[u] != -1)
                        __Dfs(right[u], left, right, ref shape, ref bitPosition);

                    bitPosition++;
                }
                #endregion
            }
            finally
            {
                intPool_.Return(rent1);
                intPool_.Return(rent2);
                intPool_.Return(rent3);
            }
        }
        static int[,] __PrecomputeBlockRmqFromShape(ulong blockType)
        {
            var (length, shape) = ((int)(blockType >> 32), (int)(uint)blockType);
            var (rent1, rent2, rent3, rent4, rent5, rent6) = (intPool_.Rent(length), intPool_.Rent(length), intPool_.Rent(length), intPool_.Rent(length), intPool_.Rent(2 * length - 1), intPool_.Rent(2 * length - 1));
            try
            {
                var left = rent1.AsSpan(0, length);
                var right = rent2.AsSpan(0, length);
                var parent = rent3.AsSpan(0, length);
                var first = rent4.AsSpan(0, length);
                var euler = rent5.AsSpan(0, 2 * length - 1);
                var depth = rent6.AsSpan(0, 2 * length - 1);
                left.Fill(-1);
                right.Fill(-1);
                parent.Fill(-1);
                first.Fill(-1);

                var stack = new Stack<int>(length);
                var (root, next) = (-1, 0);
                for (var p = 0; p < 2 * length; p++)
                {
                    if (0 == ((shape >> p) & 1))
                        stack.Pop();
                    else
                    {
                        var u = next++;
                        if (!stack.TryPeek(out var tmp1))
                            root = u;
                        else
                        {
                            parent[u] = tmp1;
                            if (left[tmp1] == -1)
                                left[tmp1] = u;
                            else
                                right[tmp1] = u;
                        }
                        stack.Push(u);
                    }
                }

                var index = 0;
                __DfsEuler(root, 0, left, right, first, euler, depth, ref index);

                var M = euler.Length;
                var K = (int)Math.Log(M, 2.0) + 1;
                var t = new int[K, M];
                for (var j = 0; j < M; j++)
                    t[0, j] = j;
                for (var k = 1; k < K; k++)
                {
                    var span = 1 << k;
                    var half = span >> 1;
                    for (var j = 0; j + span <= M; j++)
                    {
                        var (a, b) = (t[k - 1, j], t[k - 1, j + half]);
                        t[k, j] = depth[a] <= depth[b] ? a : b;
                    }
                }

                var table = new int[length, length];
                for (var i = 0; i < length; i++)
                {
                    table[i, i] = i;
                    for (var j = i + 1; j < length; j++)
                        table[i, j] = euler[__RmpEuler(first[i], first[j], depth, t)];
                }

                return table;

                #region @@
                static void __DfsEuler(int u, int d, ReadOnlySpan<int> left, ReadOnlySpan<int> right, Span<int> first, Span<int> euler, Span<int> depth, ref int index)
                {
                    (euler[index], depth[index]) = (u, d);
                    if (first[u] == -1)
                        first[u] = index;
                    index++;
                    if (left[u] != -1)
                    {
                        __DfsEuler(left[u], d + 1, left, right, first, euler, depth, ref index);
                        (euler[index], depth[index]) = (u, d);
                        index++;
                    }
                    if (right[u] != -1)
                    {
                        __DfsEuler(right[u], d + 1, left, right, first, euler, depth, ref index);
                        (euler[index], depth[index]) = (u, d);
                        index++;
                    }
                }
                static int __RmpEuler(int L, int R, ReadOnlySpan<int> depth, int[,] t)
                {
                    if (L > R)
                        (L, R) = (R, L);
                    var k = (int)Math.Log(R - L + 1, 2.0);
                    var (a, b) = (t[k, L], t[k, R - (1 << k) + 1]);
                    return depth[a] <= depth[b] ? a : b;
                }
                #endregion
            }
            finally
            {
                intPool_.Return(rent1);
                intPool_.Return(rent2);
                intPool_.Return(rent3);
                intPool_.Return(rent4);
                intPool_.Return(rent5);
                intPool_.Return(rent6);
            }
        }
        #endregion
    }

    /// <summary>
    /// Queries to find the best value according to the comparer and its original index.
    /// This operation takes O(1) time.
    /// </summary>
    /// <returns>A <see cref="SparseTable{T}.ValueWithIndex"/> record containing the best value and its original index in the input array.</returns>
    /// <remarks>This method is a shorthand for calling Query(0, <see cref="Size"/>).</remarks>
    public SparseTable<T>.ValueWithIndex Query()
    => Query(0, this.Size);

    /// <summary>
    /// Queries the range [start, end) to find the best value and its original index.
    /// This operation takes O(1) time.
    /// </summary>
    /// <param name="start">The inclusive, zero-based start of the range.</param>
    /// <param name="end">The exclusive, zero-based end of the range.</param>
    /// <returns>A <see cref="SparseTable{T}.ValueWithIndex"/> record containing the best value and its original index in the input array.</returns>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="start"/> or <paramref name="end"/> are outside the valid bounds.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="start"/> is greater than or equal to <paramref name="end"/>.</exception>
    public SparseTable<T>.ValueWithIndex Query(int start, int end)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(start);
        ArgumentOutOfRangeException.ThrowIfGreaterThan(end, this.Size);
#else
        if (start < 0)
            throw new ArgumentOutOfRangeException(nameof(start));
        if (end > this.Size)
            throw new ArgumentOutOfRangeException(nameof(end), $"{nameof(end)} must be less than or equal the size of sequence ({this.Size}).");
#endif
        if (start >= end)
            throw new ArgumentException($"{nameof(start)} must be less than {nameof(end)}.", nameof(start));

        var (lb, rb) = (start / blockSize_, (end - 1) / blockSize_);
        var best = -1;
        if (lb == rb)
            best = lb * blockSize_ + __QueryWithinBlock(lb, start % blockSize_, (end - 1) % blockSize_ + 1);
        else
        {
            var li = lb * blockSize_ + __QueryWithinBlock(lb, start % blockSize_, blockSize_);
            var ri = rb * blockSize_ + __QueryWithinBlock(rb, 0, (end - 1) % blockSize_ + 1);
            best = __ArgMin(li, ri);
            if (lb + 1 <= rb - 1)
                best = __ArgMin(best, table_.Query(lb + 1, rb).Index);
        }

        return new(memory_.Span[best], best);

        #region @@
        int __ArgMin(int i, int j)
        => comparer_(memory_.Span[j], memory_.Span[i]) ? j : i;
        int __QueryWithinBlock(int block, int l, int r)
        => patternRmq_[blockTypes_[block]][l, r - 1];
        #endregion
    }

    // 
    // 

    /// <exclude />
    public record Init(SparseTable<int>.Init StInit, ReadOnlyMemory<T> Memory, int BlockSize, int BlockCount, int[] BlockMins, ulong[] BlockTypes, Dictionary<ulong, int[,]> PatternRmq);
}
