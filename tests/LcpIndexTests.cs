
using BelNytheraSeiche.WaveletMatrix;

[TestClass]
public class LcpIndexTests
{
    [TestMethod]
    public void GetLcp()
    {
        var text = "abracadabra";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        Assert.AreEqual(4, lcpIndex.GetLcp(0, 7), "LCP of 'abracadabra' and 'abra' should be 4.");
        Assert.AreEqual(3, lcpIndex.GetLcp(8, 1), "LCP of 'bra' and 'bracadabra' should be 3.");
        Assert.AreEqual(1, lcpIndex.GetLcp(10, 7), "LCP of 'a' and 'abra' should be 1.");
        Assert.AreEqual(text.Length - 5, lcpIndex.GetLcp(5, 5), "LCP of a suffix with itself should be its length.");
    }

    [TestMethod]
    public void CountUniqueSubstrings()
    {
        var text = "banana";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        // b,a,n,ba,an,na,ban,ana,nan,bana,anan,nana,banan,anana,banana -> 15 unique
        var count = (int)lcpIndex.CountUniqueSubstrings();
        Assert.AreEqual(15, count);
    }

    [TestMethod]
    public void FindTandemRepeats()
    {
        var text = "abcabcabcxyz";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        var repeats = lcpIndex.FindTandemRepeats().ToArray();
        Assert.AreEqual(1, repeats.Length, "Should find one maximal tandem repeat group.");
        var repeat = repeats[0];
        Assert.AreEqual(0, repeat.Position, "Position should be 0.");
        Assert.AreEqual(3, repeat.Length, "Length of unit 'abc' should be 3.");
        Assert.AreEqual(3, repeat.Count, "Count of 'abc' should be 3.");
    }

    [TestMethod]
    public void FindRepeats()
    {
        var text = "abcxabcyabczabcabc";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        var repeats = lcpIndex.FindRepeats(3).ToDictionary(r => r.Text);
        Assert.AreEqual(1, repeats.Count, "Should find one unique repeat of minLength 3.");
        Assert.IsTrue(repeats.ContainsKey("abc"), "Should contain 'abc'.");
        CollectionAssert.AreEqual((int[])[0, 4, 8, 12, 15], repeats["abc"].Positions);
    }

    [TestMethod]
    public void CalculateZivLempelComplexity()
    {
        var text = "abracadabra";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        int complexity = lcpIndex.CalculateZivLempelComplexity();
        // a|b|r|ac|ad|ab|ra
        Assert.AreEqual(7, complexity, "LZ78 complexity of 'abracadabra' should be 7.");
    }

    [TestMethod]
    public void SerializeAndDeserialize()
    {
        var text = "abcxabcyabczabcabc,abcabcabcxyz";
        var sa = SuffixArray.Create(text);
        var lcpIndex1 = LcpIndex.Create(sa);
        var lcpIndex2 = LcpIndex.Deserialize(LcpIndex.Serialize(lcpIndex1));
        var data1 = lcpIndex1.InnerData;
        var data2 = lcpIndex2.InnerData;
        Assert.AreEqual(data1.LcpRmp.InnerData.StInit.N, data2.LcpRmp.InnerData.StInit.N);
        CollectionAssert.AreEqual(data1.LcpRmp.InnerData.StInit.Table.Select(n => (n.Index, n.Value)).ToArray(), data2.LcpRmp.InnerData.StInit.Table.Select(n => (n.Index, n.Value)).ToArray());
        Assert.AreEqual(data1.SA.Text.Span.ToString(), data2.SA.Text.Span.ToString());
        CollectionAssert.AreEqual(data1.SA.SA.ToArray(), data2.SA.SA.ToArray());
        CollectionAssert.AreEqual(data1.SA.Lcp.ToArray(), data2.SA.Lcp.ToArray());
        CollectionAssert.AreEqual(data1.SA.Rank.ToArray(), data2.SA.Rank.ToArray());
        Assert.AreEqual(lcpIndex1.GetLcp(0, 8), lcpIndex2.GetLcp(0, 8));
        Assert.AreEqual(lcpIndex1.CountUniqueSubstrings(), lcpIndex2.CountUniqueSubstrings());
        Assert.AreEqual(lcpIndex1.CalculateZivLempelComplexity(), lcpIndex2.CalculateZivLempelComplexity());
        var result1 = lcpIndex1.FindRepeats(3).Select(n => (n.Text, String.Join(',', n.Positions.Select(m => m.ToString())))).ToArray();
        var result2 = lcpIndex2.FindRepeats(3).Select(n => (n.Text, String.Join(',', n.Positions.Select(m => m.ToString())))).ToArray();
        CollectionAssert.AreEqual(result1, result2);
        var result3 = lcpIndex1.FindTandemRepeats().Select(n => (n.Position, n.Length)).ToArray();
        var result4 = lcpIndex2.FindTandemRepeats().Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(result3, result4);
    }

    // 
}
