using System.Runtime.CompilerServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Proivdes helper methods for generic math operations on unmanaged types.
/// This allows for high-performance bit-wise operations without requiring .NET 7+ Generic Math interfaces
/// in projects that need to support older targets like netstandard2.1.
/// </summary>
internal static unsafe class GenericMathHelper
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ulong ToUInt64<T>(T value) where T : unmanaged
    {
        if (sizeof(T) == 8) return Unsafe.As<T, ulong>(ref value);
        if (sizeof(T) == 4) return Unsafe.As<T, uint>(ref value);
        if (sizeof(T) == 2) return Unsafe.As<T, ushort>(ref value);
        if (sizeof(T) == 1) return Unsafe.As<T, byte>(ref value);
        throw new NotSupportedException($"Type {typeof(T).Name} of size {sizeof(T)} is not supported.");
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T FromUInt64<T>(ulong value) where T : unmanaged
    {
        if (sizeof(T) == 8) return Unsafe.As<ulong, T>(ref value);
        if (sizeof(T) == 4)
        {
            uint v = (uint)value;
            return Unsafe.As<uint, T>(ref v);
        }
        if (sizeof(T) == 2)
        {
            ushort v = (ushort)value;
            return Unsafe.As<ushort, T>(ref v);
        }
        if (sizeof(T) == 1)
        {
            byte v = (byte)value;
            return Unsafe.As<byte, T>(ref v);
        }
        throw new NotSupportedException($"Type {typeof(T).Name} of size {sizeof(T)} is not supported.");
    }
}
