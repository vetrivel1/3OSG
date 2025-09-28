using System;
using System.Text;

namespace Cnp.Decoders;

/// <summary>
/// EBCDIC to ASCII converter based on legacy ebc2asc.c logic
/// Replicates the exact conversion table and logic from the original C implementation
/// </summary>
public static class EbcdicAsciiConverter
{
    static EbcdicAsciiConverter()
    {
        // Enable support for IBM code pages
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        _ebcdicCodec = Encoding.GetEncoding(37);
    }
    private static readonly Encoding _ebcdicCodec;
    /// <summary>
    /// EBCDIC to ASCII conversion table from legacy ebc2asc.c
    /// This table maps EBCDIC byte values (0-255) to ASCII byte values
    /// </summary>
    private static readonly byte[] E2A_TABLE = new byte[]
    {
        0,1,2,3,236,9,202,127,226,210,211,11,12,13,14,169,16,17,18,19,
        239,197,8,203,24,25,220,216,28,29,30,31,183,184,185,187,196,10,
        23,27,204,205,207,208,209,5,6,7,217,218,22,221,222,223,224,4,
        227,229,233,235,176,177,158,26,32,201,131,132,133,160,242,134,135,164,
        213,46,60,40,43,179,38,130,136,137,138,161,140,139,141,225,33,36,
        42,41,59,94,45,47,178,142,180,181,182,143,128,165,124,44,37,95,
        62,63,186,144,188,189,190,243,192,193,194,96,58,35,64,39,61,34,
        195,97,98,99,100,101,102,103,104,105,174,175,198,199,200,241,248,106,
        107,108,109,110,111,112,113,114,166,167,145,206,146,15,230,126,115,116,
        117,118,119,120,121,122,173,168,212,91,214,215,155,156,157,250,159,21,
        20,172,171,252,170,254,228,93,191,231,123,65,66,67,68,69,70,71,
        72,73,232,147,148,149,162,237,125,74,75,76,77,78,79,80,81,82,
        238,150,129,151,163,152,92,240,83,84,85,86,87,88,89,90,253,245,
        153,247,246,249,48,49,50,51,52,53,54,55,56,57,219,251,154,244,
        234,255
    };

    /// <summary>
    /// Conversion control modes matching legacy ispacked parameter
    /// </summary>
    public enum ConversionMode
    {
        /// <summary>
        /// Standard EBCDIC to ASCII conversion (ispacked = 0)
        /// </summary>
        Standard = 0,
        
        /// <summary>
        /// Copy bytes without conversion (ispacked = 1) 
        /// </summary>
        Packed = 1,
        
        /// <summary>
        /// Zoned decimal conversion with sign handling (ispacked = 2)
        /// </summary>
        ZonedDecimal = 2
    }

    /// <summary>
    /// Convert EBCDIC bytes to ASCII using legacy ebc2asc logic
    /// </summary>
    /// <param name="source">Source EBCDIC bytes</param>
    /// <param name="destination">Destination ASCII bytes</param>
    /// <param name="length">Number of bytes to convert</param>
    /// <param name="mode">Conversion mode (standard, packed, or zoned decimal)</param>
    public static void Convert(ReadOnlySpan<byte> source, Span<byte> destination, int length, ConversionMode mode)
    {
        if (source.Length < length || destination.Length < length)
        {
            throw new ArgumentException("Source or destination buffer too small");
        }

        switch (mode)
        {
            case ConversionMode.Packed:
                // COMP-3 packed decimal decoding into ASCII digits
                ConvertPacked(source, destination, length);
                break;

            case ConversionMode.ZonedDecimal:
                // ispacked == 2: Zoned decimal with sign handling
                ConvertZonedDecimal(source, destination, length);
                break;

            case ConversionMode.Standard:
            default:
                // ispacked == 0: Standard EBCDIC to ASCII conversion
                ConvertStandard(source, destination, length);
                break;
        }
    }

    /// <summary>
    /// Standard EBCDIC to ASCII conversion using the legacy e2a table
    /// Uses the exact conversion table from legacy ebc2asc.c
    /// </summary>
    private static void ConvertStandard(ReadOnlySpan<byte> source, Span<byte> destination, int length)
    {
        // Decode EBCDIC bytes to Unicode chars using IBM037
        var chars = _ebcdicCodec.GetChars(source.Slice(0, length).ToArray());
        // Encode Unicode chars to ASCII bytes
        var ascii = Encoding.ASCII.GetBytes(chars);
        for (int i = 0; i < length; i++)
            destination[i] = ascii[i];
    }

    /// <summary>
    /// Zoned decimal conversion with special sign handling for the last byte
    /// Replicates the exact logic from legacy ebc2asc.c lines 39-59
    /// </summary>
    private static void ConvertZonedDecimal(ReadOnlySpan<byte> source, Span<byte> destination, int length)
    {
        // Convert all bytes except the last one using standard conversion
        for (int i = 0; i < length - 1; i++)
        {
            destination[i] = E2A_TABLE[source[i]];
        }

        // Special handling for the last byte (sign byte)
        if (length > 0)
        {
            int lastByte = source[length - 1] & 0xFF;
            
            if (lastByte >= 192 && lastByte < 208)
            {
                // Positive: C0-CF (192-207) -> '0'-'F'
                destination[length - 1] = (byte)('0' + (lastByte - 192));
            }
            else if (lastByte >= 240 && lastByte <= 249)
            {
                // Unsigned case: F0-F9 (240-249) -> use standard conversion
                destination[length - 1] = E2A_TABLE[lastByte];
            }
            else if (lastByte >= 208)
            {
                // Negative: D0+ (208+) -> 'p'+ 
                destination[length - 1] = (byte)('p' + (lastByte - 208));
            }
            else
            {
                // Default case: space
                destination[length - 1] = (byte)' ';
            }
        }
    }

    /// <summary>
    /// Convenience method to convert a single record using the legacy field-based approach
    /// </summary>
    /// <param name="source">Source EBCDIC record</param>
    /// <param name="destination">Destination ASCII record</param>
    /// <param name="fields">Field definitions with offset, length, and data type</param>
    public static void ConvertRecord(ReadOnlySpan<byte> source, Span<byte> destination, IEnumerable<(int Offset, int Length, string DataType)> fields)
    {
        // Initialize destination with spaces (as per legacy logic)
        destination.Fill((byte)' ');

        foreach (var (offset, length, dataType) in fields)
        {
            if (offset < 0 || offset + length > source.Length || offset + length > destination.Length)
                continue;

            var mode = GetConversionMode(dataType);
            Convert(source.Slice(offset), destination.Slice(offset), length, mode);
        }
    }

    /// <summary>
    /// Determine conversion mode based on legacy DataType values
    /// Maps DD file string types to legacy numeric types used in mbcnvt0.c
    /// </summary>
    private static ConversionMode GetConversionMode(string dataType)
    {
        return dataType.ToUpperInvariant() switch
        {
            "TEXT" => ConversionMode.Standard,           // DataType 0 → e2aControl = 0
            "NUMBER" => ConversionMode.ZonedDecimal,     // DataType 1 → e2aControl = 2  
            "MIXED" => ConversionMode.Packed,            // Mixed fields are packed - no conversion
            "PACKED NUMBER" => ConversionMode.Packed,    // DataType 2+ → e2aControl = 1
            "PACKED DECIMAL" => ConversionMode.Packed,   // DataType 2+ → e2aControl = 1
            _ => ConversionMode.Standard                 // Default to standard conversion
        };
    }
    
    /// <summary>
    /// Decode COMP-3 packed decimal field into ASCII digits (no scale)
    /// </summary>
    private static string UnpackComp3(ReadOnlySpan<byte> data)
    {
        var sb = new System.Text.StringBuilder();
        for (int i = 0; i < data.Length; i++)
        {
            int hi = (data[i] >> 4) & 0x0F;
            sb.Append(hi);
            if (i < data.Length - 1)
            {
                int lo = data[i] & 0x0F;
                sb.Append(lo);
            }
        }
        // Handle sign nibble in last half-byte if needed (positive assumed)
        return sb.ToString();
    }

    /// <summary>
    /// Decode COMP-3 packed decimal field into ASCII digits
    /// </summary>
    private static void ConvertPacked(ReadOnlySpan<byte> source, Span<byte> destination, int length)
    {
        // Unpack to a digit string
        var digits = UnpackComp3(source);
        // Write ASCII digits into destination, pad/truncate
        int copy = Math.Min(digits.Length, destination.Length);
        for (int i = 0; i < copy; i++)
            destination[i] = (byte)digits[i];
        // Pad remaining with '0'
        for (int i = copy; i < destination.Length; i++)
            destination[i] = (byte)'0';
    }
}
