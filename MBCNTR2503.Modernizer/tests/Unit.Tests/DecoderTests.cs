using Cnp.Decoders;
using Xunit;

public class DecoderTests
{
    [Theory]
    [InlineData(new byte[] { 0x12, 0x3C }, 0, 123)]   // +123
    [InlineData(new byte[] { 0x12, 0x3D }, 0, -123)]  // -123
    [InlineData(new byte[] { 0x12, 0x3C }, 2, 1.23)]  // +1.23
    public void PackedDecimal_Decode(byte[] bytes, int scale, decimal expected)
    {
        Assert.True(PackedDecimal.TryDecode(bytes, scale, out var value));
        Assert.Equal(expected, value);
    }

    [Fact]
    public void PackedDecimal_InvalidNibble_Fails()
    {
        var ok = PackedDecimal.TryDecode(new byte[] { 0x1A, 0x3C }, 0, out _);
        Assert.False(ok);
    }

    [Theory]
    [InlineData(new byte[] { 0xF1, 0xF2, 0xC3 }, 0, 123)]   // +123
    [InlineData(new byte[] { 0xF1, 0xF2, 0xD3 }, 0, -123)]  // -123
    [InlineData(new byte[] { 0xF1, 0xF2, 0xC3 }, 2, 1.23)]  // +1.23
    public void ZonedDecimal_Decode(byte[] bytes, int scale, decimal expected)
    {
        Assert.True(ZonedDecimal.TryDecode(bytes, scale, out var value));
        Assert.Equal(expected, value);
    }
}

