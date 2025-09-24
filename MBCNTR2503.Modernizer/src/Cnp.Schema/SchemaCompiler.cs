using System.Collections.Immutable;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

namespace Cnp.Schema;

public static class SchemaCompiler
{
    public static CompiledSchema Compile(string schemaDir)
    {
        var sha = ComputeShaForDir(schemaDir);
        var ddPath = Path.Combine(schemaDir, "mblps.dd");
        var iomapPath = Path.Combine(schemaDir, "mblps.dd.iomap");

        var fields = ParseDd(ddPath);
        var layout = new RecordLayout("mblps", 4000, fields.ToImmutableArray());
        var ioMap = ParseIoMap(iomapPath);

        return new CompiledSchema(
            Sha: sha,
            SourceDir: schemaDir,
            Container4000: layout,
            IoMap: ioMap
        );
    }

    public static void Emit(CompiledSchema schema, string outDir)
    {
        Directory.CreateDirectory(outDir);
        var file = Path.Combine(outDir, $"{schema.Sha}.schema.json");
        var json = JsonSerializer.Serialize(schema, new JsonSerializerOptions{ WriteIndented = true });
        File.WriteAllText(file, json, new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));
    }

    private static List<FieldModel> ParseDd(string ddPath)
    {
        var list = new List<FieldModel>();
        foreach (var raw in File.ReadAllLines(ddPath))
        {
            var line = raw.Trim();
            if (line.Length == 0 || line.StartsWith("#")) continue;
            var parts = line.Split(',', StringSplitOptions.TrimEntries);
            if (parts.Length < 4) continue;
            var name = parts[0];
            if (!int.TryParse(parts[1], out var offset)) continue;
            if (!int.TryParse(parts[2], out var length)) continue;
            var type = parts[3];
            var scale = 0;
            if (parts.Length >= 5) int.TryParse(parts[4], out scale);
            list.Add(new FieldModel(name, offset, length, type, scale));
        }
        return list;
    }

    private static ImmutableDictionary<string,string> ParseIoMap(string path)
    {
        var dict = new Dictionary<string,string>(StringComparer.OrdinalIgnoreCase);
        foreach (var raw in File.ReadAllLines(path))
        {
            var line = raw.Trim();
            if (line.Length == 0 || line.StartsWith("#")) continue;
            var idx = line.IndexOf(',');
            if (idx <= 0) continue;
            var left = line.Substring(0, idx).Trim();
            var right = line.Substring(idx + 1).Trim();
            dict[left] = right;
        }
        return dict.ToImmutableDictionary();
    }

    private static string ComputeShaForDir(string dir)
    {
        using var sha = SHA256.Create();
        var files = Directory.GetFiles(dir).OrderBy(f => f, StringComparer.OrdinalIgnoreCase);
        foreach (var f in files)
        {
            var nameBytes = Encoding.UTF8.GetBytes(Path.GetFileName(f));
            sha.TransformBlock(nameBytes, 0, nameBytes.Length, null, 0);
            var bytes = File.ReadAllBytes(f);
            sha.TransformBlock(bytes, 0, bytes.Length, null, 0);
        }
        sha.TransformFinalBlock(Array.Empty<byte>(), 0, 0);
        return Convert.ToHexString(sha.Hash!).ToLowerInvariant();
    }
}


