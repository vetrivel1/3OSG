
using Xunit;
using Cnp.Decoders;
using System;
using System.Text;

public class EbcdicAsciiConverterTests
{
    [Fact]
    public void ConvertPacked_Simple123()
    {
        // 0x12 0x3C => hi1,lo2,hi3 => "123"
        var source = new byte[] { 0x12, 0x3C };
        var destination = new byte[3];
        EbcdicAsciiConverter.Convert(source.AsSpan(), destination, 3, EbcdicAsciiConverter.ConversionMode.Packed);
        var result = Encoding.ASCII.GetString(destination);
        Assert.Equal("123", result);
    }

    [Fact]
    public void ConvertPacked_PadTruncate()
    {
        // 0x01 0x2C => hi0,lo1,hi2 => "012"; with length2 it becomes "12"
        var source = new byte[] { 0x01, 0x2C };
        var destination = new byte[2];
        EbcdicAsciiConverter.Convert(source.AsSpan(), destination, 2, EbcdicAsciiConverter.ConversionMode.Packed);
        var result = Encoding.ASCII.GetString(destination);
        Assert.Equal("12", result);
    }
    
    [Fact]
    public void ConvertZonedDecimal_PositiveDigits()
    {
        // EBCDIC digits F1,F2 -> ASCII '1','2'
        var source = new byte[] { 0xF1, 0xF2 };
        var destination = new byte[2];
        EbcdicAsciiConverter.Convert(source.AsSpan(), destination, 2, EbcdicAsciiConverter.ConversionMode.ZonedDecimal);
        var result = Encoding.ASCII.GetString(destination);
        Assert.Equal("12", result);
    }
    
    [Fact]
    public void ConvertZonedDecimal_SignHandlingNegative()
    {
        // EBCDIC digits F1,D3 -> '1' and negative sign mapping: D3 -> p+(0xD3-208)=p+3='s'
        var source = new byte[] { 0xF1, 0xD3 };
        var destination = new byte[2];
        EbcdicAsciiConverter.Convert(source.AsSpan(), destination, 2, EbcdicAsciiConverter.ConversionMode.ZonedDecimal);
        var result = Encoding.ASCII.GetString(destination);
        Assert.Equal("1s", result);
    }
    
    [Fact]
    public void ConvertStandard_DateDigits()
    {
        // EBCDIC for '2','0','2','5' is F2,F0,F2,F5
        var source = new byte[] { 0xF2, 0xF0, 0xF2, 0xF5 };
        var destination = new byte[4];
        EbcdicAsciiConverter.Convert(source.AsSpan(), destination, 4, EbcdicAsciiConverter.ConversionMode.Standard);
        var result = Encoding.ASCII.GetString(destination);
        Assert.Equal("2025", result);
    }
}
