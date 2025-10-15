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

using System.Runtime.CompilerServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Provides a mutable bit set for efficiently building large bit vectors.
/// </summary>
/// <param name="arrayAlignment">The alignment size for the internal buffer chunks. Must be a power of 2. Defaults to 65536.</param>
public sealed class BitSet(int arrayAlignment = 65536)
{
    readonly ValueBuffer<ulong> buffer_ = new(arrayAlignment, true);
    int count_;

    /// <summary>
    /// Gets the alignment size used by the internal buffer.
    /// </summary>
    public int ArrayAlignment { get; } = arrayAlignment;

    // 
    // 

    /// <summary>
    /// Gets the bit value at the specified index.
    /// </summary>
    /// <param name="index">The zero-based index of the bit to get.</param>
    /// <returns>The bit value (<c>true</c> for 1, <c>false</c> for 0) at the specified index.</returns>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="index"/> is less than 0 or greater than or equal to the number of bits in the set.</exception>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public bool Get(int index)
    {
#if NET8_0_OR_GREATER
        ArgumentOutOfRangeException.ThrowIfNegative(index);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(index, count_);
#else
        if (index < 0 || index >= count_)
            throw new ArgumentOutOfRangeException(nameof(index));
#endif

        var ai = (int)((uint)index >> 6);
        var bi = index & 0x3F;
        return 0 != ((buffer_[ai] >> bi) & 1ul);
    }

    /// <summary>
    /// Adds a bit to the end of the bit set.
    /// </summary>
    /// <param name="bit">The bit to add (<c>true</c> for 1, <c>false</c> for 0).</param>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Add(bool bit)
    {
        var ai = count_ >> 6;
        buffer_[ai] |= bit ? 1ul << (count_ & 0x3F) : 0ul;
        count_++;
    }

    /// <summary>
    /// Creates an immutable, read-only version of this bit set.
    /// </summary>
    /// <returns>A new <see cref="ImmutableBitSet"/> containing the current data.</returns>
    public ImmutableBitSet ToImmutable()
    => ToImmutable(this);

    /// <summary>
    /// Creates an immutable version of a specified <see cref="BitSet"/>, optimized for high-performance Rank and Select operations.
    /// </summary>
    /// <param name="createAuxDir">
    /// A value indicating whether to immediately create the auxiliary directories required for
    /// fast Rank and Select operations.
    /// </param>
    /// <returns>A new <see cref="RankSelectBitSet"/> containing the data from the provided bit set and pre-calculated auxiliary indexes.</returns>
    /// <remarks>
    /// If <paramref name="createAuxDir"/> is set to <c>false</c>, this method is very fast, but the returned
    /// <see cref="RankSelectBitSet"/> will not be ready for Rank/Select operations until its auxiliary directories
    /// are created and set via the <c>SetAuxDir()</c> method.
    /// If set to <c>true</c> (the default), the auxiliary directories are created during this call, which takes more processing time
    /// but results in a fully initialized and ready-to-use object.
    /// </remarks>
    public RankSelectBitSet ToRankSelect(bool createAuxDir = true)
    => ToRankSelect(this, createAuxDir);

    /// <summary>
    /// Creates an immutable, read-only version of a specified <see cref="BitSet"/>.
    /// </summary>
    /// <param name="bitSet">The mutable bit set to convert.</param>
    /// <returns>A new <see cref="ImmutableBitSet"/> containing the data from the provided bit set.</returns>
    /// <exception cref="ArgumentNullException"><paramref name="bitSet"/> is null.</exception>
    public static ImmutableBitSet ToImmutable(BitSet bitSet)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(bitSet);
#else
        if (bitSet == null)
            throw new ArgumentNullException(nameof(bitSet));
#endif

        return new(bitSet.buffer_.ToArray(), bitSet.count_);
    }

    /// <summary>
    /// Creates an immutable version of a specified <see cref="BitSet"/>, optimized for high-performance Rank and Select operations.
    /// </summary>
    /// <param name="bitSet">The mutable bit set to convert.</param>
    /// <param name="createAuxDir">
    /// A value indicating whether to immediately create the auxiliary directories required for
    /// fast Rank and Select operations.
    /// </param>
    /// <returns>A new <see cref="RankSelectBitSet"/> containing the data from the provided bit set and pre-calculated auxiliary indexes.</returns>
    /// <exception cref="ArgumentNullException"><paramref name="bitSet"/> is null.</exception>
    /// <remarks>
    /// If <paramref name="createAuxDir"/> is set to <c>false</c>, this method is very fast, but the returned
    /// <see cref="RankSelectBitSet"/> will not be ready for Rank/Select operations until its auxiliary directories
    /// are created and set via the <c>SetAuxDir()</c> method.
    /// If set to <c>true</c> (the default), the auxiliary directories are created during this call, which takes more processing time
    /// but results in a fully initialized and ready-to-use object.
    /// </remarks>
    public static RankSelectBitSet ToRankSelect(BitSet bitSet, bool createAuxDir = true)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(bitSet);
#else
        if (bitSet == null)
            throw new ArgumentNullException(nameof(bitSet));
#endif

        return ImmutableBitSet.ToRankSelect(bitSet.ToImmutable(), createAuxDir);
    }
}

/// <summary>
/// Represents a read-only, immutable bit set.
/// </summary>
/// <param name="buffer">The underlying ulong array that stores the bits.</param>
/// <param name="count">The total number of bits in the set.</param>
public class ImmutableBitSet(ulong[] buffer, int count)
{
    /// <summary>
    /// Gets the underlying ulong array that stores the bits.
    /// </summary>
    public ulong[] Buffer { get; } = buffer;
    /// <summary>
    /// Gets the total number of bits in the set.
    /// </summary>
    public int Count { get; } = count;

    /// <summary>
    /// Gets the bit value at the specified index.
    /// </summary>
    /// <param name="index">The zero-based index of the bit to get.</param>
    /// <returns>The bit value (<c>true</c> for 1, <c>false</c> for 0).</returns>
    /// <exception cref="ArgumentOutOfRangeException"><paramref name="index"/> is out of range.</exception>
    public bool this[int index]
    {
        get
        {
#if NET8_0_OR_GREATER
            ArgumentOutOfRangeException.ThrowIfNegative(index);
            ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(index, Count);
#else
            if (index < 0 || index >= Count)
                throw new ArgumentOutOfRangeException(nameof(index));
#endif

            var ai = (int)((uint)index >> 6);
            var bi = index & 0x3F;
            return 0 != ((this.Buffer[ai] >> bi) & 1ul);
        }
    }

    /// <summary>
    /// Gets the bit value at the specified index.
    /// </summary>
    /// <param name="index">The zero-based index of the bit to get.</param>
    /// <returns>The bit value (<c>true</c> for 1, <c>false</c> for 0).</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public bool At(int index)
    => this[index];

    /// <summary>
    /// Creates a new <see cref="RankSelectBitSet"/> from the specified <see cref="ImmutableBitSet"/>.
    /// </summary>
    /// <param name="createAuxDir">
    /// A value indicating whether to immediately create the auxiliary directories required for
    /// fast Rank and Select operations.
    /// </param>
    /// <returns>A new <see cref="RankSelectBitSet"/> instance.</returns>
    /// <remarks>
    /// If <paramref name="createAuxDir"/> is set to <c>false</c>, this method is very fast, but the returned
    /// <see cref="RankSelectBitSet"/> will not be ready for Rank/Select operations until its auxiliary directories
    /// are created and set via the <c>SetAuxDir()</c> method.
    /// If set to <c>true</c> (the default), the auxiliary directories are created during this call, which takes more processing time
    /// but results in a fully initialized and ready-to-use object.
    /// </remarks>
    public RankSelectBitSet ToRankSelect(bool createAuxDir = true)
    => ToRankSelect(this, createAuxDir);

    /// <summary>
    /// Creates a new <see cref="RankSelectBitSet"/> from the specified <see cref="ImmutableBitSet"/>.
    /// </summary>
    /// <param name="bitSet">The source immutable bit set to convert.</param>
    /// <param name="createAuxDir">
    /// A value indicating whether to immediately create the auxiliary directories required for
    /// fast Rank and Select operations.
    /// </param>
    /// <returns>A new <see cref="RankSelectBitSet"/> instance.</returns>
    /// <exception cref="ArgumentNullException"><paramref name="bitSet"/> is null.</exception>
    /// <remarks>
    /// If <paramref name="createAuxDir"/> is set to <c>false</c>, this method is very fast, but the returned
    /// <see cref="RankSelectBitSet"/> will not be ready for Rank/Select operations until its auxiliary directories
    /// are created and set via the <c>SetAuxDir()</c> method.
    /// If set to <c>true</c> (the default), the auxiliary directories are created during this call, which takes more processing time
    /// but results in a fully initialized and ready-to-use object.
    /// </remarks>
    public static RankSelectBitSet ToRankSelect(ImmutableBitSet bitSet, bool createAuxDir = true)
    {
#if NET6_0_OR_GREATER
        ArgumentNullException.ThrowIfNull(bitSet);
#else
        if (bitSet == null)
            throw new ArgumentNullException(nameof(bitSet));
#endif

        var newSet = new RankSelectBitSet(bitSet.Buffer, bitSet.Count);
        if (createAuxDir)
            newSet.SetAuxDir(RankSelectBitSet.CreateAuxDir(bitSet));
        return newSet;
    }
}

/// <summary>
/// An immutable bit set optimized for high-performance Rank and Select operations.
/// </summary>
/// <remarks>
/// This data structure uses auxiliary indexes (lookup tables) to perform Rank (counting bits) and
/// Select (finding the n-th bit) operations in near-constant time, making it suitable for
/// succinct data structures like LOUDS Tries.
/// </remarks>
/// <param name="buffer">The underlying ulong array that stores the bits.</param>
/// <param name="count">The total number of bits in the set.</param>
public sealed class RankSelectBitSet(ulong[] buffer, int count) : ImmutableBitSet(buffer, count)
{
    const int PrimaryAuxDirInterval = 512;
    const int SecondaryAuxDirInterval = 64;
    int[] rankPrimaryAuxDir_ = [];
    short[] rankSecondaryAuxDir_ = [];
    int[] selectAuxDir_ = [];
    int lastRank0_ = -1;
    int totalOnes_ = -1;

    int LastRank0 { get => lastRank0_ == -1 ? lastRank0_ = Rank0(this.Count) : lastRank0_; }

    int TotalOnes
    {
        get
        {
            if (totalOnes_ == -1)
            {
                var k = this.Count - 1;
                var rank = rankPrimaryAuxDir_[k / PrimaryAuxDirInterval] + rankSecondaryAuxDir_[k / SecondaryAuxDirInterval];
                var word = this.Buffer[k / 64];
                var offset = (k & 63) + 1;
                var mask = offset == 64 ? UInt64.MaxValue : ((1ul << offset) - 1);
                totalOnes_ = rank + PopCount(word & mask);
            }

            return totalOnes_;
        }
    }

    // 
    // 

    /// <summary>
    /// Calculates the rank of a bit '1' before a specified position (exclusive).
    /// Rank1 is the total number of set bits (1s) in the range [0, k).
    /// </summary>
    /// <param name="k">The zero-based exclusive end of the range.</param>
    /// <returns>The number of set bits before position <paramref name="k"/>.</returns>
    public int Rank1(int k)
    {
        if (k <= 0)
            return 0;
        else if (k >= this.Count)
            return this.TotalOnes;

        var rank = rankPrimaryAuxDir_[k / PrimaryAuxDirInterval] + rankSecondaryAuxDir_[k / SecondaryAuxDirInterval];
        var word = this.Buffer[k / 64];
        var offset = k & 63;
        var mask = offset == 0 ? 0ul : ((1ul << offset) - 1);
        return rank + PopCount(word & mask);
    }

    /// <summary>
    /// Calculates the rank of a bit '0' before a specified position (exclusive).
    /// Rank0 is the total number of unset bits (0s) in the range [0, k).
    /// </summary>
    /// <param name="k">The one-based rank of the unset bit to find (e.g., k=1 for the first '0').</param>
    /// <returns>The zero-based index of the k-th unset bit, or -1 if not found.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public int Rank0(int k)
    => k - Rank1(k);

    /// <summary>
    /// Finds the position of the k-th set bit (1).
    /// </summary>
    /// <param name="k">The one-based rank of the set bit to find (e.g., k=1 for the first '1').</param>
    /// <returns>The zero-based index of the k-th set bit, or -1 if not found.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public int Select1(int k)
    => k > 0 && k <= selectAuxDir_.Length ? selectAuxDir_[k - 1] : -1;

    /// <summary>
    /// Finds the position of the k-th unset bit (0).
    /// </summary>
    /// <param name="k">The one-based rank of the unset bit to find (e.g., k=1 for the first '0').</param>
    /// <returns>The zero-based index of the k-th unset bit, or -1 if not found.</returns>
    public int Select0(int k)
    {
        if (k <= 0 || k > this.LastRank0)
            return -1;

        var (lo, hi) = (0, this.Count);
        while (lo < hi)
        {
            var mid = lo + ((hi - lo) >> 1);
            if (Rank0(mid + 1) >= k)
                hi = mid;
            else
                lo = mid + 1;
        }

        return lo;
    }

    /// <summary>
    /// Sets the pre-calculated auxiliary directories for Rank and Select operations.
    /// </summary>
    /// <param name="auxDir">A tuple containing the rank primary directory, rank secondary directory, and the select directory.</param>
    public void SetAuxDir((int[], short[], int[]) auxDir)
    {
        (rankPrimaryAuxDir_, rankSecondaryAuxDir_, selectAuxDir_) = auxDir;
    }

    /// <summary>
    /// Gets the pre-calculated auxiliary directories for Rank and Select operations.
    /// </summary>
    /// <returns>A tuple containing the rank primary directory, rank secondary directory, and the select directory.</returns>
    public (int[], short[], int[]) GetAuxDir()
    => (rankPrimaryAuxDir_, rankSecondaryAuxDir_, selectAuxDir_);

    /// <summary>
    /// Creates the auxiliary directories required for fast Rank and Select operations from a given bit set.
    /// </summary>
    /// <param name="bitSet">The immutable bit set to process.</param>
    /// <returns>A tuple containing the generated directories for rank and select.</returns>
    public static (int[], short[], int[]) CreateAuxDir(ImmutableBitSet bitSet)
    {
        var rankPrimaryAuxDir = new int[(bitSet.Count + PrimaryAuxDirInterval - 1) / PrimaryAuxDirInterval];
        var rankSecondaryAuxDir = new short[(bitSet.Count + SecondaryAuxDirInterval - 1) / SecondaryAuxDirInterval];

        var rankPrimary = 0;
        var rankSecondary = 0;
        var rank = 0;
        var l = new List<int>();
        for (var i = 0; i < bitSet.Count; i++)
        {
            if (i % PrimaryAuxDirInterval == 0)
                (rankPrimaryAuxDir[i / PrimaryAuxDirInterval], rankSecondary) = (rankPrimary, 0);
            if (i % SecondaryAuxDirInterval == 0)
                rankSecondaryAuxDir[i / SecondaryAuxDirInterval] = (short)rankSecondary;
            if (bitSet.At(i))
            {
                rankPrimary++;
                rankSecondary++;
                rank++;
                l.Add(i);
            }
        }
        return (rankPrimaryAuxDir, rankSecondaryAuxDir, [.. l]);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static int PopCount(ulong value)
    {
#if NETCOREAPP3_0_OR_GREATER && !NO_BITOP_POPCOUNT
        return System.Numerics.BitOperations.PopCount(value);
#else
        value -= (value >> 1) & 0x5555555555555555ul;
        value = (value & 0x3333333333333333ul) + ((value >> 2) & 0x3333333333333333ul);
        return (int)(unchecked(((value + (value >> 4)) & 0xF0F0F0F0F0F0F0Ful) * 0x101010101010101ul) >> 56);
#endif
    }
}
