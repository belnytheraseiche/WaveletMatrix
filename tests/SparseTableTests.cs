
using BelNytheraSeiche.WaveletMatrix;

[TestClass]
public class SparseTableTests
{
    readonly int[] testData_ = [2, 3, 1, 4, 5, 2, 6, 1, 9];

    [TestMethod]
    public void SparseTableQueryMininum()
    {
        var table = new SparseTable<int>(testData_, (a, b) => a < b);
        var result = table.Query(2, 7);
        Assert.AreEqual(1, result.Value, "The minimum value in the range [2, 7) should be 1.");
        Assert.AreEqual(2, result.Index, "The index of minimum value should be 2.");
    }

    [TestMethod]
    public void SparseTableQueryMaximum()
    {
        var table = new SparseTable<int>(testData_, (a, b) => a > b);
        var result = table.Query(1, 5);
        Assert.AreEqual(5, result.Value, "The maximum value in the range [1, 5) should be 5.");
        Assert.AreEqual(4, result.Index, "The index of the maximum value should be 4.");
    }

    [TestMethod]
    public void SparseTableQueryFullRange()
    {
        var table = new SparseTable<int>(testData_, (a, b) => a < b);
        var result = table.Query();
        Assert.AreEqual(1, result.Value, "The minimum value should be 1.");
        CollectionAssert.Contains((int[])[2, 7], result.Index, "The index of the minimum value should be 2.");
    }

    [TestMethod]
    public void SparseTableQueryInvalidRange()
    {
        var table = new SparseTable<int>(testData_, (a, b) => a < b);
        bool exceptionThrowed = false;
        try
        {
            table.Query(10, 1);
        }
        catch
        {
            exceptionThrowed = true;
        }
        Assert.IsTrue(exceptionThrowed);
    }

    [TestMethod]
    public void AggregateSparseTableQuerySum()
    {
        var table = new AggregateSparseTable<int>(testData_, (a, b) => a + b);
        var result = table.Query(1, 4);
        Assert.AreEqual(testData_[1..4].Sum(), result);
    }

    [TestMethod]
    public void AggregateSparseTableQueryFullRange()
    {
        var table = new AggregateSparseTable<int>(testData_, (a, b) => a + b);
        var result = table.Query();
        Assert.AreEqual(testData_.Sum(), result);
    }

    [TestMethod]
    public void AggregateSparseTableQueryInvalidRange()
    {
        var table = new AggregateSparseTable<int>(testData_, (a, b) => a + b);
        bool exceptionThrowed = false;
        try
        {
            table.Query(10, 1);
        }
        catch
        {
            exceptionThrowed = true;
        }
        Assert.IsTrue(exceptionThrowed);
    }

    // 
}
