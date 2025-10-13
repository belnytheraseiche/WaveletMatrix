
using BelNytheraSeiche.WaveletMatrix;

namespace Tests;

[TestClass]
public class BurrowsWheelerTransformTests
{
    [TestMethod]
    public void Transform()
    {
        var text = "abracadabra$";
        var (_, bwtString, index) = BurrowsWheelerTransform.Transform(text.AsMemory());
        Assert.AreEqual("ard$rcaaaabb", bwtString);
        Assert.AreEqual(3, index);
    }
}
