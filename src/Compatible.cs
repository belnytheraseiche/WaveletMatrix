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
using System.ComponentModel;

#if NETSTANDARD2_1
namespace System.Runtime.CompilerServices
{
    [EditorBrowsable(EditorBrowsableState.Never)]
    internal class IsExternalInit { }
}
#endif

#if NETSTANDARD2_1
namespace BelNytheraSeiche.WaveletMatrix
{
    /// <exclude />
    public static class BitOperations
    {
        public static ulong RotateLeft(ulong value, int count)
        => (value << count) | (value >> (64 - count));

        public static int TrailingZeroCount(int value)
        => (int)Math.Log(value, 2);
    }

    /// <exclude />
    public static class StreamExtensions
    {
        public static void ReadExactly(this Stream stream, byte[] buffer)
        => ReadExactly(stream, buffer, 0, buffer.Length);

        public static void ReadExactly(this Stream stream, byte[] buffer, int offset, int count)
        => ReadExactly(stream, buffer.AsSpan(offset, count));

        public static void ReadExactly(this Stream stream, Span<byte> buffer)
        {
            while (buffer.Length != 0)
            {
                var count = stream.Read(buffer);
                if (count == 0)
                    throw new EndOfStreamException();
                buffer = buffer[count..];
            }
        }
    }

    /// <exclude />
    public static class LinqExtensions
    {
        public static T FirstOrDefault<T>(this IEnumerable<T> source, Func<T, bool> predicate, T defaultValue)
        {
            foreach (var element in source)
                if (predicate(element))
                    return element;
            return defaultValue;
        }
        public static IOrderedEnumerable<T> Order<T>(this IEnumerable<T> source)
        => source.OrderBy(n => n);

        public static IOrderedEnumerable<T> Order<T>(this IEnumerable<T> source, IComparer<T> comparer)
        => source.OrderBy(n => n, comparer);

        public static IOrderedEnumerable<T> OrderDescending<T>(this IEnumerable<T> source)
        => source.OrderByDescending(n => n);

        public static IOrderedEnumerable<T> OrderDescending<T>(this IEnumerable<T> source, IComparer<T> comparer)
        => source.OrderByDescending(n => n, comparer);

        public static T MinBy<T, U>(this IEnumerable<T> source, Func<T, U> keySelector) where U : IComparable<U>
        => source.Aggregate((acc, current) => keySelector(current).CompareTo(keySelector(acc)) < 0 ? current : acc);

        public static T MaxBy<T, U>(this IEnumerable<T> source, Func<T, U> keySelector) where U : IComparable<U>
        => source.Aggregate((acc, current) => keySelector(current).CompareTo(keySelector(acc)) > 0 ? current : acc);
    }
}
#endif

#if NETSTANDARD2_1 || !NET8_0_OR_GREATER
namespace BelNytheraSeiche.WaveletMatrix
{
    /// <exclude />
    public static class BitArrayExtensions
    {
        public static bool HasAnySet(this BitArray bits)
        {
            for (var i = 0; i < bits.Length; i++)
                if (bits[i])
                    return true;
            return false;
        }

        public static bool HasAllSet(this BitArray bits)
        {
            for (var i = 0; i < bits.Length; i++)
                if (!bits[i])
                    return false;
            return true;
        }
    }
}
#endif
