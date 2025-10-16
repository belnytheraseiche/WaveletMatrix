
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
    public void FindLongestRepeats()
    {
        var text = "ababcxabcyabczabcabcbc";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        var repeats = lcpIndex.FindLongestRepeats();
        Assert.IsNotNull(repeats);
        Assert.AreEqual("abc", repeats.Text);
        CollectionAssert.AreEquivalent((int[])[2, 6, 10, 14, 17], repeats.Positions);
    }

    [TestMethod]
    public void SimilarityMatches()
    {
        var text1 = "011000000";
        var text2 = "999111999111";
        var matcher = LcpIndex.CreateSimilarityMatcher(text1, text2);
        var result = matcher.Matches().Select(n => (n.Position1, n.Position2, n.Length)).ToArray();
        CollectionAssert.AreEquivalent(((int, int, int)[])[(1, 9, 2), (1, 10, 2)], result);
    }

    [TestMethod]
    public void SimilarityLongestMatch()
    {
        var text1 = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var text2 = "The quick brown cat jumps over the lazy dog. The quick brown rabbit jumps over the lazy fox.";
        var matcher = LcpIndex.CreateSimilarityMatcher(text1, text2);
        var result = matcher.LongestMatch();
        Assert.IsNotNull(result);
        Assert.AreEqual((19, 19, 42), (result.Position1, result.Position2, result.Length));
    }

    [TestMethod]
    public void FindPalindrome()
    {
        var text = "xabcdefggfedcbay";
        var matcher = LcpIndex.CreateSimilarityMatcherForPalindrome(text);
        var result = matcher.FindPalindrome();
        Assert.IsNotNull(result);
        Assert.AreEqual((1, 14), (result.Position, result.Length));
    }

    [TestMethod]
    public void CountOccurrences()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        var result = lcpIndex.CountOccurrences("o");
        Assert.AreEqual(8, result);
    }

    [TestMethod]
    public void Locate()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var lcpIndex = LcpIndex.Create(sa);
        var result = lcpIndex.Locate("o").ToArray();
        CollectionAssert.AreEqual((int[])[12, 17, 26, 41, 57, 62, 71, 86], result);
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
    public void FindShortestUniqueSubstring()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var lcpIndex = LcpIndex.Create(SuffixArray.Create(text));
        var result = lcpIndex.FindShortestUniqueSubstring();
        Assert.IsNotNull(result);
        Assert.AreEqual((" T", 44), (result.Text, result.Position));
    }

    [TestMethod]
    public void FindAllShortestUniqueSubstrings()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var lcpIndex = LcpIndex.Create(SuffixArray.Create(text));
        var result = lcpIndex.FindAllShortestUniqueSubstrings().ToArray();
        Assert.AreEqual(6, result.Length);
        CollectionAssert.AreEqual(((string, int)[])[("x ", 18), ("g.", 42), (". ", 43), (" T", 44), ("g ", 63), ("x.", 87)], result.Select(n => (n.Text, n.Position)).ToArray());
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
