using System.IO.MemoryMappedFiles;
using System.Runtime.InteropServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// A high-performance serializer that maps a Wavelet Matrix directly from a flat binary file.
/// This implementation uses Memory-Mapped Files (MMF) for zero-allocation and near-instant loading.
/// This specialized serializer is designed for int-based cores.
/// </summary>
public sealed class FlatMemoryMappedSerializer : IWaveletSerializer
{
    /// <summary>
    /// Deserializes a Wavelet Core from the specified stream. 
    /// Note: This specialized serializer requires a file for mapping.
    /// </summary>
    public WaveletMatrixCore Deserialize(Stream stream, WaveletMatrixOptions? options = null)
    {
        throw new NotSupportedException("FlatMemoryMappedSerializer requires a file path for mapping. Use the Overload that accepts a string.");
    }

    /// <summary>
    /// Deserializes a Wavelet Core from the specified file path using memory mapping.
    /// </summary>
    public unsafe WaveletMatrixCore Deserialize(string filePath, WaveletMatrixOptions? options = null)
    {
        if (!File.Exists(filePath))
            throw new FileNotFoundException("Wavelet Matrix data file not found.", filePath);

        // Open the file and create a memory-mapped file
        var fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.Read);
        var mmf = MemoryMappedFile.CreateFromFile(fileStream, null, 0, MemoryMappedFileAccess.Read, HandleInheritability.None, false);
        
        var accessor = mmf.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read);
        byte* pointer = null;
        accessor.SafeMemoryMappedViewHandle.AcquirePointer(ref pointer);

        try
        {
            // Header: [Magic (4)] [TypeSize (4)] [Length (4)] [Depth (4)] [Data...]
            var magic = *(int*)pointer;
            if (magic != 0x574D5847) // "WMXG" in hex
                throw new InvalidDataException("Invalid Wavelet Matrix file format.");

            var typeSize = *(int*)(pointer + 4);
            if (typeSize != 4)
                throw new InvalidDataException($"Specialized FlatMemoryMappedSerializer currently only supports 4-byte types (int).");

            var length = *(int*)(pointer + 8);
            var depth = *(int*)(pointer + 12);

            // Calculate offsets for bitsets and zeros
            var currentOffset = 16L;
            var matrix = new RankSelectBitSet[depth];
            var zeros = new int[depth];

            for (int i = 0; i < depth; i++)
            {
                var bitCount = *(int*)(pointer + currentOffset);
                currentOffset += 4;
                
                var bufferSize = (bitCount + 63) / 64;
                var bufferPointer = (ulong*)(pointer + currentOffset);
                
                var manager = new NativeMemoryManager<ulong>(bufferPointer, bufferSize);
                var immutableBitSet = new ImmutableBitSet(manager.Memory, bitCount);
                matrix[i] = immutableBitSet.ToRankSelect();
                
                currentOffset += bufferSize * 8;
                zeros[i] = *(int*)(pointer + currentOffset);
                currentOffset += 4;
            }

            // Create the core with the mapped memory
            var init = new WaveletMatrixCore.Init(length, zeros, matrix);
            return new WaveletMatrixCore(init);
        }
        catch
        {
            accessor.SafeMemoryMappedViewHandle.ReleasePointer();
            accessor.Dispose();
            mmf.Dispose();
            throw;
        }
    }

    /// <summary>
    /// Serialization to flat file is currently not implemented for this specialized mapper.
    /// Typically used for reading pre-compiled matrices.
    /// </summary>
    public void Serialize(Stream stream, WaveletMatrixCore core, WaveletMatrixOptions? options = null)
    {
        throw new NotSupportedException("FlatMemoryMappedSerializer is optimized for high-speed reading. Use a standard serializer for writing.");
    }
}
