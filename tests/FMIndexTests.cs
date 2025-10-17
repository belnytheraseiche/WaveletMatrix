
using BelNytheraSeiche.WaveletMatrix;

[TestClass]
public class FMIndexTests
{
    FMIndex fm1_ = null!;
    FMIndex fm2_ = null!;
    const string text_ = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";

    [TestInitialize]
    public void Init()
    {
        fm1_ = FMIndex.Create(text_);
        fm2_ = FMIndex.Create(text_, 8);
    }

    [TestMethod]
    public void Count()
    {
        Assert.AreEqual(2, fm1_.Count("fox"));
        Assert.AreEqual(2, fm1_.Count("The"));
        Assert.AreEqual(2, fm1_.Count("the"));
        Assert.AreEqual(0, fm1_.Count("cat"));

        Assert.AreEqual(2, fm2_.Count("fox"));
        Assert.AreEqual(2, fm2_.Count("The"));
        Assert.AreEqual(2, fm2_.Count("the"));
        Assert.AreEqual(0, fm2_.Count("cat"));
    }

    [TestMethod]
    public void Locate()
    {
        var result1 = fm1_.Locate("lazy", SortOrder.Unordered).ToArray();
        var result2 = fm1_.Locate("quick", SortOrder.Ascending).ToArray();
        var result3 = fm1_.Locate("jumps", SortOrder.Descending).ToArray();
        CollectionAssert.AreEquivalent((int[])[35, 80], result1, "Unsorted locate should find all correct positions.");
        CollectionAssert.AreEqual((int[])[4, 49], result2, "Ascending locate should find all correct positions.");
        CollectionAssert.AreEqual((int[])[65, 20], result3, "Descending locate should find all correct positions.");

        var result4 = fm2_.Locate("lazy", SortOrder.Unordered).ToArray();
        var result5 = fm2_.Locate("quick", SortOrder.Ascending).ToArray();
        var result6 = fm2_.Locate("jumps", SortOrder.Descending).ToArray();
        CollectionAssert.AreEquivalent((int[])[35, 80], result4, "Unsorted locate should find all correct positions.");
        CollectionAssert.AreEqual((int[])[4, 49], result5, "Ascending locate should find all correct positions.");
        CollectionAssert.AreEqual((int[])[65, 20], result6, "Descending locate should find all correct positions.");
    }

    [TestMethod]
    public void LocateFuzzy()
    {
        var result1 = fm1_.LocateFuzzy("lazy fax", 2).Select(n => (n.Position, n.Length, n.EditDistance)).ToArray();
        CollectionAssert.AreEqual(((int, int, int)[])[(80, 8, 1), (81, 7, 2)], result1);

        var result2 = fm2_.LocateFuzzy("lazy fax", 2).Select(n => (n.Position, n.Length, n.EditDistance)).ToArray();
        CollectionAssert.AreEqual(((int, int, int)[])[(80, 8, 1), (81, 7, 2)], result2);

        var result3 = fm1_.LocateFuzzy("?he", 0, true, true, '?').Select(n => (n.Position, n.Length, n.EditDistance)).ToArray();
        CollectionAssert.AreEqual(((int, int, int)[])[(0, 3, 0), (31, 3, 0), (45, 3, 0), (76, 3, 0)], result3);

        var result4 = fm2_.LocateFuzzy("?he", 0, true, true, '?').Select(n => (n.Position, n.Length, n.EditDistance)).ToArray();
        CollectionAssert.AreEqual(((int, int, int)[])[(0, 3, 0), (31, 3, 0), (45, 3, 0), (76, 3, 0)], result4);
    }

    [TestMethod]
    public void LocateWildcard()
    {
        var result1 = fm1_.LocateWildcard("f_x", '_').ToArray();
        CollectionAssert.AreEqual((int[])[16, 85], result1);

        var result2 = fm2_.LocateWildcard("f_x", '_').ToArray();
        CollectionAssert.AreEqual((int[])[16, 85], result2);
    }

    [TestMethod]
    public void LocateSingleGapped()
    {
        var result1 = fm1_.LocateSingleGapped("T*q").Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 5), (45, 5)], result1);

        var result2 = fm2_.LocateSingleGapped("T*q").Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 5), (45, 5)], result2);
    }

    [TestMethod]
    public void LocateMultiGapped()
    {
        var result1 = fm1_.LocateMultiGapped("T*q*b*n").Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 15), (45, 15)], result1);

        var result2 = fm2_.LocateMultiGapped("T*q*b*n").Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 15), (45, 15)], result2);

        var result3 = fm1_.LocateMultiGapped("T*q*b*n", false).Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 15), (0, 60), (0, 60), (0, 60), (45, 15)], result3);

        var result4 = fm2_.LocateMultiGapped("T*q*b*n", false).Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 15), (0, 60), (0, 60), (0, 60), (45, 15)], result4);
    }

    [TestMethod]
    public void RestoreSourceText()
    {
        Assert.AreEqual(text_, fm1_.RestoreSourceText(), "Reconstructed text should match the original.");
        Assert.AreEqual(text_, fm2_.RestoreSourceText(), "Reconstructed text should match the original.");
    }

    [TestMethod]
    public void GetSnippet1()
    {
        var snippet1 = fm1_.GetSnippet(10, 5, 20);// brown at position 10 and length 5.
        Assert.AreEqual("e quick brown fox ju", snippet1.Text);
        Assert.AreEqual(8, snippet1.KeyPosition, "Keyword should start index 8 in the snippet.");

        var snippet2 = fm2_.GetSnippet(10, 5, 20);// brown at position 10 and length 5.
        Assert.AreEqual("e quick brown fox ju", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(8, snippet2.KeyPosition, "Keyword should start index 8 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet2()
    {
        var snippet1 = fm1_.GetSnippet(4, 5, 20);// quick at position 4 and length 5.
        Assert.AreEqual("The quick brown fox ", snippet1.Text);
        Assert.AreEqual(4, snippet1.KeyPosition, "Keyword should start index 4 in the snippet.");

        var snippet2 = fm2_.GetSnippet(4, 5, 20);// quick at position 4 and length 5.
        Assert.AreEqual("The quick brown fox ", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(4, snippet2.KeyPosition, "Keyword should start index 4 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet3()
    {
        var snippet1 = fm1_.GetSnippet(80, 4, 20);// lazy at position 80 and length 4.
        Assert.AreEqual("s over the lazy fox.", snippet1.Text);
        Assert.AreEqual(11, snippet1.KeyPosition, "Keyword should start index 11 in the snippet.");

        var snippet2 = fm2_.GetSnippet(80, 4, 20);// lazy at position 80 and length 4.
        Assert.AreEqual("s over the lazy fox.", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(11, snippet2.KeyPosition, "Keyword should start index 11 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet4()
    {
        var snippet1 = fm1_.GetSnippet(20, 10, 10);// jumps over is longer than or equal total length.
        Assert.AreEqual("jumps over", snippet1.Text);
        Assert.AreEqual(0, snippet1.KeyPosition, "Keyword should start index 0 in the snippet.");

        var snippet2 = fm2_.GetSnippet(20, 10, 10);// jumps over is longer than or equal total length.
        Assert.AreEqual("jumps over", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(0, snippet2.KeyPosition, "Keyword should start index 0 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet5()
    {
        var snippet1 = fm1_.GetSnippet(40, 3, 20, 0.9);// dog at position 40 and length 3. set leading ratio to 0.9.
        Assert.AreEqual(" over the lazy dog. ", snippet1.Text);
        Assert.AreEqual(15, snippet1.KeyPosition, "Keyword should start index 15 in the snippet.");

        var snippet2 = fm2_.GetSnippet(40, 3, 20, 0.9);// dog at position 40 and length 3. set leading ratio to 0.9.
        Assert.AreEqual(" over the lazy dog. ", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(15, snippet2.KeyPosition, "Keyword should start index 15 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet6()
    {
        var snippet1 = fm1_.GetSnippet(5, 5, 20, 1.0);// quick at position 5 and length 5. set leading ratio to 1.0
        Assert.AreEqual("The quick brown fox ", snippet1.Text);
        Assert.AreEqual(5, snippet1.KeyPosition, "Keyword should start index 5 in the snippet.");

        var snippet2 = fm2_.GetSnippet(5, 5, 20, 1.0);// quick at position 5 and length 5. set leading ratio to 1.0
        Assert.AreEqual("The quick brown fox ", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(5, snippet2.KeyPosition, "Keyword should start index 5 in the snippet.");
    }

    [TestMethod]
    public void GetSnippet7()
    {
        var snippet1 = fm1_.GetSnippet(80, 4, 20, 0);// lazy at position 80 and length 4. set leading ratio to 0
        Assert.AreEqual("s over the lazy fox.", snippet1.Text);
        Assert.AreEqual(11, snippet1.KeyPosition, "Keyword should start index 11 in the snippet.");

        var snippet2 = fm1_.GetSnippet(80, 4, 20, 0);// lazy at position 80 and length 4. set leading ratio to 0
        Assert.AreEqual("s over the lazy fox.", text_[snippet2.Index .. (snippet2.Index + snippet2.Length)]);
        Assert.AreEqual(11, snippet2.KeyPosition, "Keyword should start index 11 in the snippet.");
    }

    [TestMethod]
    public void SerializeAndDeserialize()
    {
        var fm1 = FMIndex.Deserialize(FMIndex.Serialize(fm1_));
        Assert.AreEqual(fm1_.RestoreSourceText(), fm1.RestoreSourceText());
        Assert.AreEqual(fm1_.Count("dog"), fm1.Count("dog"));
        CollectionAssert.AreEqual(fm1_.Locate("brown").ToArray(), fm1.Locate("brown").ToArray());

        var fm2 = FMIndex.Deserialize(FMIndex.Serialize(fm2_));
        Assert.AreEqual(fm2_.RestoreSourceText(), fm2.RestoreSourceText());
        Assert.AreEqual(fm2_.Count("dog"), fm2.Count("dog"));
        CollectionAssert.AreEqual(fm2_.Locate("brown").ToArray(), fm2.Locate("brown").ToArray());
    }

    // 
}
