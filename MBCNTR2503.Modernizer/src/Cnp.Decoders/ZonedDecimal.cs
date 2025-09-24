namespace Cnp.Decoders;

public static class ZonedDecimal
{
    // EBCDIC zoned decimal: each byte low nibble is digit; last byte high nibble carries sign
    // F/C = positive, D = negative (A/B treated as positive for our purposes)
    public static bool TryDecode(ReadOnlySpan<byte> src, int scale, out decimal value)
    {
        long mantissa = 0;
        value = default;
        if (src.Length == 0) return false;
        int last = src.Length - 1;
        for (int i = 0; i < last; i++)
        {
            int digit = src[i] & 0x0F;
            if (digit > 9) return false;
            mantissa = checked(mantissa * 10 + digit);
        }
        int lastDigit = src[last] & 0x0F;
        if (lastDigit > 9) return false;
        mantissa = checked(mantissa * 10 + lastDigit);
        int zone = (src[last] >> 4) & 0x0F;
        int sign = zone == 0x0D ? -1 : 1;
        try
        {
            value = sign * (decimal)mantissa / Pow10(scale);
            return true;
        }
        catch
        {
            value = default;
            return false;
        }
    }

    private static decimal Pow10(int s)
    {
        return s switch
        {
            0 => 1m,
            1 => 10m,
            2 => 100m,
            3 => 1000m,
            4 => 10000m,
            5 => 100000m,
            6 => 1000000m,
            _ => decimal.Parse("1" + new string('0', s))
        };
    }
}


