
using BelNytheraSeiche.WaveletMatrix;

[TestClass]
public class FMIndexTests
{
    FMIndex fm_ = null!;
    const string text_ = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";

    [TestInitialize]
    public void Init()
    {
        fm_ = FMIndex.Create(text_);
    }

    [TestMethod]
    public void Count()
    {
        Assert.AreEqual(2, fm_.Count("fox"));
        Assert.AreEqual(2, fm_.Count("The"));
        Assert.AreEqual(2, fm_.Count("the"));
        Assert.AreEqual(0, fm_.Count("cat"));
    }

    [TestMethod]
    public void Locate()
    {
        var result1 = fm_.Locate("lazy", FMIndex.SortOrder.Unordered).ToArray();
        var result2 = fm_.Locate("quick", FMIndex.SortOrder.Ascending).ToArray();
        var result3 = fm_.Locate("jumps", FMIndex.SortOrder.Descending).ToArray();
        CollectionAssert.AreEquivalent((int[])[35, 80], result1, "Unsorted locate should find all correct positions.");
        CollectionAssert.AreEqual((int[])[4, 49], result2, "Ascending locate should find all correct positions.");
        CollectionAssert.AreEqual((int[])[65, 20], result3, "Descending locate should find all correct positions.");
    }

    [TestMethod]
    public void RestoreSourceText()
    {
        Assert.AreEqual(text_, fm_.RestoreSourceText(), "Reconstructed text should match the original.");
    }

    [TestMethod]
    public void GetSnippet1()
    {
        var (snippet, index) = fm_.GetSnippet(10, 5, 20);// brown at position 10 and length 5.
        Assert.AreEqual("e quick brown fox ju", snippet);
        Assert.AreEqual(8, index, "Keyword should start index 6 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet2()
    {
        var (snippet, index) = fm_.GetSnippet(4, 5, 20);// quick at position 4 and length 5.
        Assert.AreEqual("The quick brown fox ", snippet);
        Assert.AreEqual(4, index, "Keyword should start index 4 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet3()
    {
        var (snippet, index) = fm_.GetSnippet(80, 4, 20);// lazy at position 80 and length 4.
        Assert.AreEqual("s over the lazy fox.", snippet);
        Assert.AreEqual(11, index, "Keyword should start index 11 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet4()
    {
        var (snippet, index) = fm_.GetSnippet(20, 10, 10);// jumps over is longer than or equal total length.
        Assert.AreEqual("jumps over", snippet);
        Assert.AreEqual(0, index, "Keyword should start index 0 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet5()
    {
        var (snippet, index) = fm_.GetSnippet(40, 3, 20, 0.9);// dog at position 40 and length 3. set leading ratio to 0.9.
        Assert.AreEqual(" over the lazy dog. ", snippet);
        Assert.AreEqual(15, index, "Keyword should start index 15 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet6()
    {
        var (snippet, index) = fm_.GetSnippet(5, 5, 20, 1.0);// quick at position 5 and length 5. set leading ratio to 1.0
        Assert.AreEqual("The quick brown fox ", snippet);
        Assert.AreEqual(5, index, "Keyword should start index 5 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet7()
    {
        var (snippet, index) = fm_.GetSnippet(80, 4, 20, 0);// lazy at position 80 and length 4. set leading ratio to 0
        Assert.AreEqual("s over the lazy fox.", snippet);
        Assert.AreEqual(11, index, "Keyword should start index 11 in the snippet.");
    }

    [TestMethod]
    public void SerializeAndDeserialize()
    {
        var fm = FMIndex.Deserialize(FMIndex.Serialize(fm_));
        Assert.AreEqual(fm_.RestoreSourceText(), fm.RestoreSourceText());
        Assert.AreEqual(fm_.Count("dog"), fm.Count("dog"));
        CollectionAssert.AreEqual(fm_.Locate("brown").ToArray(), fm.Locate("brown").ToArray());
    }

    // 
}
