// Program.cs
using System;

class Program
{
    static void Main(string[] args)
    {
        // Sample packed bytes for MB-ARM-FULLY-AM-PI
        byte[] comp3 = new byte[] { 0x40, 0x40, 0x40, 0x40, 0x40, 0x40 };
        int scale = 2;
        string digits = UnpackComp3(comp3);
        Console.WriteLine($"Unpacked digits: {digits}");
        if (long.TryParse(digits, out long num))
        {
            double scaled = num / Math.Pow(10, scale);
            Console.WriteLine($"Scaled (scale={scale}): {scaled:F2}");
        }
    }

    static string UnpackComp3(byte[] data)
    {
        var chars = new System.Text.StringBuilder();
        for (int i = 0; i < data.Length; i++)
        {
            int hi = data[i] >> 4;
            int lo = data[i] & 0x0F;
            chars.Append(hi);
            if (i < data.Length - 1)
                chars.Append(lo);
        }
        // Determine sign via last nibble if needed
        return chars.ToString();
    }
}
