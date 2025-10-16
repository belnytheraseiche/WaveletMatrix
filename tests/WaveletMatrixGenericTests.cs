
using BelNytheraSeiche.WaveletMatrix;

[TestClass]
public class WaveletMatrixGenericTests
{
    WaveletMatrixGeneric<int> wmInt_ = null!;
    WaveletMatrixGeneric<string> wmString_ = null!;
    readonly int[] sequenceInt_ = [30, 10, 40, 10, 50, 90, 20, 50, 30, 50, 80, 90];
    readonly string[] sequenceString_ = ["C", "A", "D", "A", "E", "I", "B", "E", "C", "E", "H", "I"];

    [TestInitialize]
    public void Init()
    {
        wmInt_ = WaveletMatrixGeneric<int>.Create(sequenceInt_);
        wmString_ = WaveletMatrixGeneric<string>.Create(sequenceString_);
    }

    [TestMethod]
    public void Access()
    {
        Assert.AreEqual(50, wmInt_.Access(4));
        Assert.AreEqual("E", wmString_.Access(4));
    }

    [TestMethod]
    public void Rank()
    {
        // count of 50 in the first 10 elements
        Assert.AreEqual(3, wmInt_.Rank(10, 50));
        // count of "A" in the first 5 elements
        Assert.AreEqual(2, wmString_.Rank(5, "A"));
    }

    [TestMethod]
    public void Select()
    {
        // position of the 2nd 50
        Assert.AreEqual(7, wmInt_.Select(2, 50));
        // position of the 2nd "A"
        Assert.AreEqual(3, wmString_.Select(2, "A"));
    }

    [TestMethod]
    public void RangeCount()
    {
        // sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50], count of 50
        Assert.AreEqual(3, wmInt_.RangeCount(3, 10, 50));
        // sequence[0, 5) -> ["C", "A", "D", "A", "E"], count of A
        Assert.AreEqual(2, wmString_.RangeCount(0, 5, "A"));
    }

    [TestMethod]
    public void Quantile()
    {
        // sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50]. unique and sorted: [10, 20, 30, 50, 90]
        // first smallest is 10
        Assert.AreEqual(10, wmInt_.Quantile(3, 10, 0));
        // second smallest is 20
        Assert.AreEqual(20, wmInt_.Quantile(3, 10, 1));
    }

    [TestMethod]
    public void RangeFreq()
    {
        // how many values are in [30, 60)? [30, 40, 50, 30, 50, 50] -> 6 values
        Assert.AreEqual(6, wmInt_.RangeFreq(0, wmInt_.Size, 30, 60));
        // how many values are in ["C", "F")? ["C", "D", "E", "E", "C", "E"] -> 6 values
        Assert.AreEqual(6, wmString_.RangeFreq(0, wmString_.Size, "C", "F"));
    }

    [TestMethod]
    public void RangeMode()
    {
        // sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50], most frequent value? 50 (3 times).
        var result = wmInt_.RangeMode(3, 10);
        Assert.AreEqual(50, result.Value);
        Assert.AreEqual(3, result.Frequency);
    }

    [TestMethod]
    public void TopK()
    {
        // sequence[1, 10) -> [10, 40, 10, 50, 90, 20, 50, 30, 50], top 2 frequent values? 50 (3 times), 10 (2 times).
        var result = wmInt_.TopK(1, 10, 2).ToArray();
        Assert.AreEqual(2, result.Length);
        Assert.AreEqual((50, 3), (result[0].Value, result[0].Frequency));
        Assert.AreEqual((10, 2), (result[1].Value, result[1].Frequency));
    }

    [TestMethod]
    public void SmallerValue()
    {
        // find smaller(nearest) than 40 in sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50]
        Assert.AreEqual(30, wmInt_.SmallerValue(3, 10, 40));
        // find smaller(nearest) than 10 in sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50] : not exists
        Assert.AreEqual(Int32.MaxValue, wmInt_.SmallerValue(3, 10, 10, Int32.MaxValue));
    }

    [TestMethod]
    public void LargerValue()
    {
        // find larger(nearest) than 80 in sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50]
        Assert.AreEqual(90, wmInt_.LargerValue(3, 10, 80));
        // find larger(nearest) than 90 in sequence[3, 10) -> [10, 50, 90, 20, 50, 30, 50] : not exists
        Assert.AreEqual(Int32.MinValue, wmInt_.LargerValue(3, 10, 90, Int32.MinValue));
    }

    [TestMethod]
    public void SerializeAndDeserialize()
    {
        var wmInt = WaveletMatrixGeneric<int>.Deserialize(WaveletMatrixGeneric<int>.Serialize(wmInt_));
        Assert.AreEqual(wmInt_.Rank(10, 50), wmInt.Rank(10, 50));
        Assert.AreEqual(wmInt_.Select(2, 50), wmInt.Select(2, 50));
    }

    [TestMethod]
    public void SerializeAndDeserializeCustom()
    {
        var stringSerializer = new StringSerializer();
        var wmString = WaveletMatrixGeneric<string>.Deserialize(WaveletMatrixGeneric<string>.Serialize(wmString_, stringSerializer), stringSerializer);
        Assert.AreEqual(wmString_.Rank(5, "A"), wmString.Rank(5, "A"));
        Assert.AreEqual(wmString_.Access(4), wmString.Access(4));
    }

    // 

    class StringSerializer : WaveletMatrixGeneric<string>.IGenericSerializer<string>
    {
        public byte[] TypeIdentifier => "STR_"u8.ToArray();

        public void WriteResolveMap(Stream stream, string[] resolveMap)
        {
            using var writer = new BinaryWriter(stream);
            writer.Write(resolveMap.Length);
            foreach (var s in resolveMap)
                writer.Write(s);
        }

        public string[] ReadResolveMap(Stream stream)
        {
            using var reader = new BinaryReader(stream);
            var length = reader.ReadInt32();
            var map = new string[length];
            for (int i = 0; i < length; i++)
                map[i] = reader.ReadString();
            return map;
        }
    }
}
