using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace BelNytheraSeiche.WaveletMatrix.Tests;

[TestClass]
public class MemoryMappedTests
{
    [TestMethod]
    public unsafe void FlatMapped_Serialization_And_Mapping_Works()
    {
        // 1. Prepare data (coordinate compressed to 0-9)
        var data = Enumerable.Range(0, 1000).Select(i => i % 10).ToArray();
        // Bypass coordinate compression to force the core to be direct integers
        var options = new WaveletMatrixOptions { BypassCoordinateCompression = true };
        var wm = WaveletMatrixGeneric<int>.Create(data, options);
        
        var tempFile = Path.GetTempFileName();
        try
        {
            // 2. Manual serialization to the flat format for testing
            using (var fs = new FileStream(tempFile, FileMode.Create))
            using (var writer = new BinaryWriter(fs))
            {
                writer.Write(0x574D5847); // Magic
                writer.Write(4);           // TypeSize (int)
                writer.Write(data.Length); // Length
                writer.Write(4);           // Depth (for 0-9 values)
                
                var core = (WaveletMatrixCore)typeof(WaveletMatrixGeneric<int>)
                    .GetField("core_", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)!
                    .GetValue(wm)!;
                    
                var matrix = core.Matrix; // This is RankSelectBitSet[]
                var zeros = core.Zeros.Span;
                
                for(int i = 0; i < matrix.Length; i++)
                {
                    var bitset = matrix[i];
                    writer.Write(bitset.Count);
                    
                    // RankSelectBitSet inherits from ImmutableBitSet which has a public Buffer property
                    var buffer = bitset.Buffer;
                    writer.Write(MemoryMarshal.AsBytes(buffer.Span).ToArray());
                    writer.Write(zeros[i]);
                }
            }

            // 3. Map it back
            var mappedWm = WaveletMatrixGeneric<int>.Map(tempFile, options);

            // 4. Verify queries
            Assert.AreEqual(data.Length, mappedWm.Size);
            Assert.AreEqual(2, mappedWm.Access(2));
            Assert.AreEqual(100, mappedWm.RangeCount(5)); 
            Assert.AreEqual(5, mappedWm.Quantile(500)); 
            Assert.AreEqual(100, mappedWm.TopK(1).First().Frequency);
        }
        finally
        {
            if (File.Exists(tempFile)) 
            {
                try { File.Delete(tempFile); } catch { /* ignore */ }
            }
        }
    }
}
