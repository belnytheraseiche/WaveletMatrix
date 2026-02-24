using System.IO;
using System.IO.Compression;
using System.IO.Hashing;
using System.Buffers.Binary;
using System.Runtime.InteropServices;

namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Strategy for Wavelet Matrix serialization.
/// </summary>
public interface IWaveletSerializer
{
    /// <summary>
    /// Serializes the Wavelet Matrix to a stream.
    /// </summary>
    void Serialize(Stream stream, WaveletMatrixCore wm, WaveletMatrixOptions? options = null);

    /// <summary>
    /// Deserializes the Wavelet Matrix from a stream.
    /// </summary>
    WaveletMatrixCore Deserialize(Stream stream, WaveletMatrixOptions? options = null);
}

/// <summary>
/// Legacy serializer using Brotli compression.
/// </summary>
public class BrotliWaveletSerializer : IWaveletSerializer
{
    public void Serialize(Stream stream, WaveletMatrixCore wm, WaveletMatrixOptions? options = null)
    {
        var firstPosition = stream.Position;
        var xxh = new XxHash32();
        Span<byte> buffer0 = stackalloc byte[64];
        stream.Write(buffer0);

        // zeros_
        var sizeZeros = 0;
        {
            using var memoryStream = new MemoryStream();
            {
                using var compressStream = new BrotliStream(memoryStream, CompressionLevel.Optimal);
                __WriteStreamFromInt32Memory(compressStream, wm.Zeros);
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
                using var compressStream = new BrotliStream(memoryStream, CompressionLevel.Optimal);
                Span<byte> buffer1 = stackalloc byte[8];
                foreach (var n in wm.Matrix)
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
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[8..], wm.Size);
        // 12: int * 1, length of zeros_
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[12..], wm.Zeros.Length);
        // 16: int * 1, size of zeros_ buffer
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[16..], sizeZeros);
        // 20: int * 1, depth of matrix_
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[20..], wm.Matrix.Length);
        // 24: int * 1, size of matrix_ buffer
        BinaryPrimitives.WriteInt32LittleEndian(buffer0[24..], sizeMatrix);
        // 28- empty
        stream.Seek(firstPosition, SeekOrigin.Begin);
        stream.Write(buffer0);

        stream.Seek(lastPosition, SeekOrigin.Begin);
    }

    public WaveletMatrixCore Deserialize(Stream stream, WaveletMatrixOptions? options = null)
    {
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

        return new WaveletMatrixCore(new WaveletMatrixCore.Init(size, zeros, matrix));
    }

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
            var buffer = new byte[1024];
            var offset = 0;
            while (offset < memory.Length)
            {
                var length = Math.Min(128, memory.Length - offset);
                var span = memory.Slice(offset, length).Span;
                for (var i = 0; i < span.Length; i++)
                    BinaryPrimitives.WriteUInt64LittleEndian(buffer.AsSpan(i * 8, 8), span[i]);
                stream.Write(buffer.AsSpan(0, 8 * length));
                offset += length;
            }
        }
    }

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
}
