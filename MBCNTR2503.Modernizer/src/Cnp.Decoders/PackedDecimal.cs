namespace Cnp.Decoders;

public static class PackedDecimal
{
    public static bool TryDecode(ReadOnlySpan<byte> src, int scale, out decimal value)
    {
        long mantissa = 0;
        value = default;
        if (src.Length == 0) return false;
        int last = src.Length - 1;
        byte signNibble = (byte)(src[last] & 0x0F);
        int sign = (signNibble == 0x0D) ? -1 : 1; // D=neg; C/F/A/B=pos treated as +
        for (int i = 0; i < last; i++)
        {
            byte b = src[i];
            int hi = (b >> 4) & 0x0F;
            int lo = b & 0x0F;
            if (hi > 9 || lo > 9) return false;
            mantissa = checked(mantissa * 100 + hi * 10 + lo);
        }
        int hiLast = (src[last] >> 4) & 0x0F;
        if (hiLast > 9) return false;
        mantissa = checked(mantissa * 10 + hiLast);
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


