namespace BelNytheraSeiche.WaveletMatrix;

/// <summary>
/// Configuration options for the Wavelet Matrix.
/// </summary>
public record WaveletMatrixOptions
{
    /// <summary>
    /// Gets the default options.
    /// </summary>
    public static readonly WaveletMatrixOptions Default = new();

    /// <summary>
    /// If true, coordinate compression is bypassed. 
    /// Useful for already dense sequences like LLM token IDs (0 to N).
    /// Defaults to false.
    /// </summary>
    public bool BypassCoordinateCompression { get; init; } = false;

    /// <summary>
    /// The depth of the wavelet matrix (number of bits).
    /// If null, it is automatically determined from the maximum value in the input.
    /// </summary>
    public int? Depth { get; init; } = null;

    /// <summary>
    /// The serializer strategy to use.
    /// Defaults to <see cref="BrotliWaveletSerializer"/>.
    /// </summary>
    public IWaveletSerializer? Serializer { get; init; } = null;
}
