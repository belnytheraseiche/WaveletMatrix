
using BelNytheraSeiche.WaveletMatrix;

[TestClass]
public class WaveletMatrixCoreTests
{
    WaveletMatrixCore wm_ = null!;
    readonly int[] sequence_ = [3, 1, 4, 1, 5, 9, 2, 5, 3, 5, 8, 9];

    [TestInitialize]
    public void Init()
    {
        wm_ = WaveletMatrixCore.Create(sequence_);
    }

    [TestMethod]
    public void Access()
    {
        // get value at 0
        Assert.AreEqual(3, wm_.Access(0));
        // get value at 4
        Assert.AreEqual(5, wm_.Access(4));
        // get value at 11
        Assert.AreEqual(9, wm_.Access(11));
    }

    [TestMethod]
    public void Rank()
    {
        // count 5 in [0, 10)
        Assert.AreEqual(3, wm_.Rank(10, 5));
        // count 1 in [0, 5)
        Assert.AreEqual(2, wm_.Rank(5, 1));
        // count 7 in [0, 12)
        Assert.AreEqual(0, wm_.Rank(12, 7));
    }

    [TestMethod]
    public void Select()
    {
        // find 5 in sequence, get index of secondary founds.
        Assert.AreEqual(7, wm_.Select(2, 5));
        // find 4 in sequence, get index of primary founds
        Assert.AreEqual(2, wm_.Select(1, 4));
        // find 1 in sequence, get index of 3rd found.
        Assert.AreEqual(-1, wm_.Select(3, 1));
    }

    [TestMethod]
    public void RangeCount()
    {
        // count 5 in sequence[3, 10)
        Assert.AreEqual(3, wm_.RangeCount(3, 10, 5));
        // count 1 in sequence[0, 4)
        Assert.AreEqual(2, wm_.RangeCount(0, 4, 1));
    }

    [TestMethod]
    public void Quantile()
    {
        // sequence[3, 10), sorted result is [1, 2, 3, 5, 5, 5, 9].
        // the smallest value is 1.
        Assert.AreEqual(1, wm_.Quantile(3, 10, 0));
        // the 3rd smallest value is 5.
        Assert.AreEqual(5, wm_.Quantile(3, 10, 3));
    }

    [TestMethod]
    public void RangeFreq()
    {
        // find value in sequence[0, 12), value >= 3 and < 6.
        Assert.AreEqual(6, wm_.RangeFreq(0, 12, 3, 6));
    }

    [TestMethod]
    public void RangeMode()
    {
        // most frequent value in sequence[3, 10)
        var result = wm_.RangeMode(3, 10);
        Assert.AreEqual(5, result.Value);
        Assert.AreEqual(3, result.Frequency);
    }

    [TestMethod]
    public void TopK()
    {
        // top 3 frequent values in sequence[3, 10)
        var results = wm_.TopK(3, 10, 3).ToArray();
        Assert.AreEqual(3, results.Length);
        Assert.AreEqual(5, results[0].Value);
        Assert.AreEqual(3, results[0].Frequency);
        var others = results.Skip(1).Select(r => r.Value).ToHashSet();
        Assert.IsTrue(others.Contains(1) || others.Contains(2) || others.Contains(3) || others.Contains(9));
    }

    [TestMethod]
    public void SmallerValue()
    {
        // find smaller(nearest) than 4 in sequence[3, 10)
        Assert.AreEqual(3, wm_.SmallerValue(3, 10, 4));
        // find smaller(nearest) than 1 in sequence[3, 10) : not exists.
        Assert.IsNull(wm_.SmallerValue(3, 10, 1));
    }

    [TestMethod]
    public void LargerValue()
    {
        // sequence[3, 10) -> { 1, 5, 9, 2, 5, 3, 5 }
        // find larger(nearest) than 4 in sequence[3, 10)
        Assert.AreEqual(5, wm_.LargerValue(3, 10, 4));
        // find larger(nearest) than 9 in sequence[3, 10) : not exists.
        Assert.IsNull(wm_.LargerValue(3, 10, 9));
    }

    [TestMethod]
    public void SerializeAndDeserialize()
    {
        var wm = WaveletMatrixCore.Deserialize(WaveletMatrixCore.Serialize(wm_));
        Assert.AreEqual(wm_.Size, wm.Size);
        Assert.AreEqual(wm_.Rank(10, 5), wm.Rank(10, 5));
        Assert.AreEqual(wm_.Select(2, 5), wm.Select(2, 5));
        Assert.AreEqual(wm_.Quantile(3, 10, 3), wm.Quantile(3, 10, 3));
    }

    // 
}
