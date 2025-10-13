
using BelNytheraSeiche.WaveletMatrix;

namespace Tests;

[TestClass]
public class SuffixArrayTests
{
    [TestMethod]
    public void Create()
    {
        var text = "Hello World!";
        var sa = SuffixArray.Create(text);

        var saValues = sa.SA.ToArray();
        Assert.AreEqual(text.Length, saValues.Length, "Length of SA-values should mutch original text length.");
        Assert.AreEqual(text.Length, sa.Text.Length, "Stored text in SA should match original text.");
        CollectionAssert.AreEqual(saValues, (int[])[5, 11, 0, 6, 10, 1, 9, 2, 3, 4, 7, 8]);
    }

    [TestMethod]
    public void SearchSingleOccurrence()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown cat jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var pattern = "cat";
        var positions = sa.Search(pattern).ToArray();
        CollectionAssert.AreEqual((int[])[61], positions, $"Position of '{pattern}' should be 61.");
    }

    [TestMethod]
    public void SearchMultipleOccurrences()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var pattern = "dog";
        var positions = sa.Search(pattern).ToArray();
        CollectionAssert.AreEqual((int[])[40, 61], positions, $"Positions of '{pattern}' should be [40, 61].");
    }

    [TestMethod]
    public void SearchNoneOccurrences()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var pattern = "rabbit";
        var positions = sa.Search(pattern).ToArray();
        CollectionAssert.AreEqual((int[])[], positions, $"Positions of '{pattern}' should be [].");
    }

    [TestMethod]
    public void SearchLongestRepeatedSubstring()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var result = sa.SearchLongestRepeated().FirstOrDefault();
        Assert.IsNotNull(result, "A longest repeated substring should be found.");
        Assert.AreEqual(" jumps over the lazy ", result.Text, "The longest repeat should be ' jumps over the lazy '.");
        CollectionAssert.AreEqual((int[])[19, 64], result.Positions, "Positions of ' jumps over the lazy ' should be [19, 64].");
    }

    [TestMethod]
    public void SearchLongestCommonSubstring()
    {
        var text1 = "The quick brown fox jumps over the lazy dog.";
        var text2 = "The lazy dog jumps over the quick fox.";
        var result = SuffixArray.SearchLongestCommon(text1, text2).FirstOrDefault();
        Assert.IsNotNull(result, "A longest repeated substring should be found.");
        Assert.AreEqual(" jumps over the ", result.Text, $"The longest common substring should be ' jumps over the '. '{result.Text}'");
        Assert.AreEqual(1, result.Positions1.Length);
        Assert.AreEqual(19, result.Positions1[0]);
        Assert.AreEqual(1, result.Positions2.Length);
        Assert.AreEqual(12, result.Positions2[0]);
    }

    [TestMethod]
    public void SearchRepeated()
    {
        var text = "abcabcabc";
        var sa = SuffixArray.Create(text);
        var repeats = sa.SearchRepeated(3).ToDictionary(n => n.Text);
        Assert.IsTrue(repeats.ContainsKey("abc"), "Should contain 'abc'.");
        CollectionAssert.AreEqual((int[])[0, 3, 6], repeats["abc"].Positions);
        Assert.IsTrue(repeats.ContainsKey("bca"), "Should contain 'bca'.");
        CollectionAssert.AreEqual((int[])[1, 4], repeats["bca"].Positions);
        Assert.IsTrue(repeats.ContainsKey("cab"), "Should contain 'cab'.");
        CollectionAssert.AreEqual((int[])[2, 5], repeats["cab"].Positions);
    }

    [TestMethod]
    public void SearchRepeatedNoResults()
    {
        var text = "abcdefghijk";
        var sa = SuffixArray.Create(text);
        var repeats = sa.SearchRepeated().ToDictionary(n => n.Text);
        Assert.AreEqual(0, repeats.Count, "Should find no repeated substrings.");
    }

    [TestMethod]
    public void SearchCommonSubstrings()
    {
        var text1 = "abracadabra";
        var text2 = "cadabras";
        var commons = SuffixArray.SearchCommon(text1, text2, 4).ToDictionary(n => n.Text);
        CollectionAssert.AreEquivalent((string[])["abra", "adab", "adabr", "adabra", "cada", "cadab", "cadabr", "cadabra", "dabr", "dabra"], commons.Keys, "Should find ['abra', 'adab', 'adabr', 'adabra', 'cada', 'cadab', 'cadabr', 'cadabra', 'dabr', 'dabra'].");
        CollectionAssert.AreEqual((int[])[0, 7], commons["abra"].Positions1);
        CollectionAssert.AreEqual((int[])[3], commons["abra"].Positions2);
    }

    [TestMethod]
    public void SearchWildcardWithQuestion()
    {
        var text = "axayazaaaa";
        var sa = SuffixArray.Create(text);
        var pattern = "a?a";
        var matches = sa.SearchWildcard(pattern).Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 3), (2, 3), (4, 3), (6, 3), (7, 3)], matches);
    }

    [TestMethod]
    public void SearchWildcardWithAsterisk()
    {
        var text = "axayazaaaa";
        var sa = SuffixArray.Create(text);
        var pattern = "a*a";
        var matches = sa.SearchWildcard(pattern).Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(0, 3), (2, 3), (4, 3), (6, 2), (7, 2), (8, 2)], matches);
    }

    [TestMethod]
    public void SearchWildcardMixed()
    {
        var text = "The quiet little brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa = SuffixArray.Create(text);
        var pattern = "qu*wn";
        var matches = sa.SearchWildcard(pattern).Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(4, 18), (56, 11)], matches);
    }

    [TestMethod]
    public void SearchWildCardWithOptions()
    {
        var text = "ab_a12b_a123b_a123456b_a1234567b";
        var sa = SuffixArray.Create(text);
        var pattern = "a*b";
        var options = new SuffixArray.WildcardOptions()
        {
            AsteriskMinLength = 3,
            AsteriskMaxLength = 6,
            StopCharacters = ['_'],
        };
        var matches = sa.SearchWildcard(pattern, options).Select(n => (n.Position, n.Length)).ToArray();
        CollectionAssert.AreEqual(((int, int)[])[(8, 5), (14, 8)], matches);
    }

    [TestMethod]
    public void SerializeAndDeserialize()
    {
        var text = "The quick brown fox jumps over the lazy dog. The quick brown dog jumps over the lazy fox.";
        var sa1 = SuffixArray.Create(text);
        var sa2 = SuffixArray.Deserialize(SuffixArray.Serialize(sa1));
        Assert.AreEqual(sa1.Text.ToString(), sa2.Text.ToString());
        CollectionAssert.AreEqual(sa1.SA.ToArray(), sa2.SA.ToArray());
        CollectionAssert.AreEqual(sa1.Lcp.ToArray(), sa2.Lcp.ToArray());
        CollectionAssert.AreEqual(sa1.Rank.ToArray(), sa2.Rank.ToArray());
    }

    // 
}
