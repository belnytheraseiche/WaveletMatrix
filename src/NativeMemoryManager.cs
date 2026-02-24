using System.Buffers;
using System.Runtime.InteropServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// A MemoryManager that allows wrapping an unmanaged pointer and exposing it as Memory{T}.
/// This is essential for zero-allocation memory-mapped file access.
/// </summary>
/// <typeparam name="T">The type of the elements in the memory block.</typeparam>
public sealed unsafe class NativeMemoryManager<T> : MemoryManager<T> where T : unmanaged
{
    private readonly T* _pointer;
    private readonly int _length;

    /// <summary>
    /// Initializes a new instance of the <see cref="NativeMemoryManager{T}"/> class.
    /// </summary>
    /// <param name="pointer">The unmanaged pointer to the memory block.</param>
    /// <param name="length">The number of elements in the memory block.</param>
    public NativeMemoryManager(T* pointer, int length)
    {
        _pointer = pointer;
        _length = length;
    }

    /// <summary>
    /// Obtains a span that represents the unmanaged memory block.
    /// </summary>
    /// <returns>A span representing the memory block.</returns>
    public override Span<T> GetSpan() => new Span<T>(_pointer, _length);

    /// <summary>
    /// Pins the unmanaged memory. For NativeMemoryManager, this is a no-op as the memory is already "pinned" (unmanaged).
    /// </summary>
    /// <param name="elementIndex">The element index to pin from.</param>
    /// <returns>A handle to the pinned memory.</returns>
    public override MemoryHandle Pin(int elementIndex = 0)
    {
        if (elementIndex < 0 || elementIndex >= _length)
            throw new ArgumentOutOfRangeException(nameof(elementIndex));

        return new MemoryHandle(_pointer + elementIndex);
    }

    /// <summary>
    /// Unpins the memory. No-op for unmanaged memory.
    /// </summary>
    public override void Unpin() { }

    /// <summary>
    /// Releases all resources.
    /// </summary>
    /// <param name="disposing">True if called from Dispose.</param>
    protected override void Dispose(bool disposing) { }
}
