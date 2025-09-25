using System.Text;
using System.Text.Json;
using System.Linq;
using Cnp.Schema;

namespace Cnp.Pipeline;

public sealed class Step1Orchestrator
{
	private readonly CompiledSchema _schema;
	private readonly Dictionary<string, DdOverride> _overrides;
	private readonly Step1DebugOptions _debug;
	public Step1Orchestrator(CompiledSchema schema)
	{
		_schema = schema;
		_overrides = LoadOverrides(Path.Combine(schema.SourceDir, "step1.overrides.json"));
		_debug = Step1DebugOptions.FromEnvironment();
	}

	private sealed record DdEntry(string Name, string RecordId, int RecordIdOffset, string ContainerId, string DdNumber, string Type);
	private sealed record DdField(string Name, int Offset, int Length, string DataType, int Scale);
	private readonly Dictionary<string, List<DdField>> _ddCache = new(StringComparer.OrdinalIgnoreCase);
	private readonly Dictionary<string, int> _debugCounts = new(StringComparer.OrdinalIgnoreCase);

	public void Run(string jobId, string inputDatPath, string outDir)
	{
		Directory.CreateDirectory(outDir);
		var work2Path = Path.Combine(outDir, $"{jobId}.4300");
		var rectypePath = Path.Combine(outDir, $"{jobId}.dat.rectype");
		var totalPath = Path.Combine(outDir, $"{jobId}.dat.total");

		var ddEntries = LoadDdList(Path.Combine(_schema.SourceDir, "ddcontrol.txt"));
		var counts = ddEntries.ToDictionary(d => d.ContainerId, d => 0);

		long primaryCount = 0;
		long pCount = 0;
		long tCount = 0;
		long rCount = 0;
		int recordLen = 4000;
		using var fs = File.OpenRead(inputDatPath);
		using var outFs = File.Create(work2Path);
		var buffer = new byte[recordLen];
		var outRec = new byte[4300];
		int read;
		while ((read = fs.Read(buffer, 0, recordLen)) == recordLen)
		{
			rCount++;
			DdEntry? matched = null;
			foreach (var d in ddEntries)
			{
				if (d.RecordId.Equals("ALL_OTHER_RECORDS", StringComparison.OrdinalIgnoreCase))
				{
					matched ??= d;
					continue;
				}
				if (d.RecordId.StartsWith("HEX_", StringComparison.OrdinalIgnoreCase))
				{
					// Match a specific hex byte value at the given offset (e.g., HEX_3C)
					if (d.RecordIdOffset >= 0 && d.RecordIdOffset < recordLen)
					{
						var hex = d.RecordId.Substring("HEX_".Length);
						if (byte.TryParse(hex, System.Globalization.NumberStyles.HexNumber, null, out var target)
							&& buffer[d.RecordIdOffset] == target)
						{
							matched = d;
							break;
						}
					}
					continue;
				}
				if (d.RecordId.Length == 1 && d.RecordIdOffset >= 0 && d.RecordIdOffset < recordLen)
				{
					char ch = DecodeEbcChar(buffer[d.RecordIdOffset]);
					if (ch == d.RecordId[0])
					{
						matched = d;
						break;
					}
				}
			}
			if (matched != null && counts.ContainsKey(matched.ContainerId))
			{
				counts[matched.ContainerId]++;
				if (matched.Type.Equals("PRIMARY", StringComparison.OrdinalIgnoreCase))
				{
					primaryCount++;
					pCount++;
					tCount = 1;
				}
				else if (matched.Type.Equals("SECONDARY", StringComparison.OrdinalIgnoreCase))
				{
					tCount++;
				}

				// initialize output record with ASCII spaces for non-field bytes
				Array.Fill(outRec, (byte)' ');
				_overrides.TryGetValue(matched.Name, out var ddOverride);
				if (ddOverride != null)
				{
					foreach (var window in ddOverride.RawWindows)
					{
						if (window.FillMode == RawFillMode.Raw)
						{
							CopyRange(buffer, outRec, window.Start, window.End);
						}
						else if (window.FillMode == RawFillMode.AsciiSpace)
						{
							Array.Fill(outRec, (byte)' ', window.Start, Math.Max(0, window.End - window.Start));
						}
					}
				}
				// apply per-field mapping (no option overlays in Step 1 per Handover)
				var ddFields = GetDdFields(matched.Name);
				foreach (var f in ddFields)
				{
					if (f.Offset < 0 || f.Offset + f.Length > 4000) continue;
					var dt = f.DataType.Trim().ToLowerInvariant();
					// Normalize type labels used across configs
					var normalizedType = dt.Replace("  ", " ");
					// 1) Text: convert EBCDIC -> ASCII
					if (normalizedType == "text")
					{
						if (ddOverride != null && ddOverride.KeepRawFields.Contains(f.Name))
						{
							CopyRange(buffer, outRec, f.Offset, f.Offset + f.Length);
							DebugField(matched.Name, f, "raw", buffer);
							continue;
						}
						var seg = EbcToAscii(buffer, f.Offset, f.Length);
						Buffer.BlockCopy(seg, 0, outRec, f.Offset, seg.Length);
						DebugField(matched.Name, f, "text", seg);
					}
					// 2) Number/Int: treat as display numerics when present
					else if (normalizedType == "number" || normalizedType == "int")
					{
						if (ddOverride != null && ddOverride.KeepRawFields.Contains(f.Name))
						{
							CopyRange(buffer, outRec, f.Offset, f.Offset + f.Length);
							DebugField(matched.Name, f, "raw", buffer);
							continue;
						}
						if (!IsAllEbcSpaces(buffer, f.Offset, f.Length) && IsDigitsOrSpaces(buffer, f.Offset, f.Length))
						{
							ConvertDisplayNumericEbcToAscii(buffer, f.Offset, f.Length, outRec, f.Offset);
							DebugField(matched.Name, f, "number", outRec, f.Offset, f.Length);
						}
						else
						{
							// legacy writes ASCII spaces for blank display numerics
							Array.Fill(outRec, (byte)' ', f.Offset, f.Length);
							DebugField(matched.Name, f, "blank-number", outRec, f.Offset, f.Length);
						}
					}
					// 3) Mixed / Packed / Zoned: leave raw in .4300 per Step 1 rules
					else
					{
						Buffer.BlockCopy(buffer, f.Offset, outRec, f.Offset, f.Length);
						DebugField(matched.Name, f, normalizedType, buffer);
					}
				}
				// trailer
				var trailer = Encoding.ASCII.GetBytes(string.Format("{0:000000000}{1:000000}{2:00000000}", rCount, tCount, pCount));
				Buffer.BlockCopy(trailer, 0, outRec, 4300 - 100, trailer.Length);
				var cid = Encoding.ASCII.GetBytes(matched.ContainerId.PadRight(5).Substring(0,5));
				Buffer.BlockCopy(cid, 0, outRec, 4300 - 6, cid.Length);
				outRec[4299] = (byte)(matched.DdNumber.Length > 0 ? (byte)matched.DdNumber[0] : (byte)'x');
				outFs.Write(outRec, 0, outRec.Length);
			}
		}

		var sb = new StringBuilder();
		foreach (var d in ddEntries)
		{
			if (d.Type.Equals("PRIMARY", StringComparison.OrdinalIgnoreCase)
				|| d.Type.Equals("SECONDARY", StringComparison.OrdinalIgnoreCase)
				|| d.Type.Equals("HEADER", StringComparison.OrdinalIgnoreCase)
				|| d.Type.Equals("NCPJAX", StringComparison.OrdinalIgnoreCase))
			{
				if (!counts.TryGetValue(d.ContainerId, out var c)) c = 0;
				sb.Append(d.ContainerId.PadRight(5).Substring(0,5));
				sb.Append(' ');
				sb.Append(c.ToString().PadLeft(9, '0'));
			}
		}
		File.WriteAllText(rectypePath, sb.ToString(), new UTF8Encoding(false));

		using (var tfs = File.Create(totalPath))
		{
			var zeros = new byte[8192];
			long remaining = primaryCount;
			while (remaining > 0)
			{
				int chunk = (int)Math.Min(remaining, zeros.Length);
				tfs.Write(zeros, 0, chunk);
				remaining -= chunk;
			}
		}
	}

	private List<DdField> GetDdFields(string ddName)
	{
		if (_ddCache.TryGetValue(ddName, out var cached)) return cached;
		var path = Path.Combine(_schema.SourceDir, ddName);
		var list = new List<DdField>();
		if (!File.Exists(path))
		{
			_ddCache[ddName] = list;
			return list;
		}
		foreach (var raw in File.ReadAllLines(path))
		{
			var line = raw.Trim();
			if (line.Length == 0 || line.StartsWith("#")) continue;
			var parts = line.Split(',', StringSplitOptions.TrimEntries);
			if (parts.Length < 4) continue;
			var name = parts[0];
			if (!int.TryParse(parts[1], out var off)) continue;
			if (!int.TryParse(parts[2], out var len)) continue;
			var dt = parts[3];
			var scale = 0; if (parts.Length >= 5) int.TryParse(parts[4], out scale);
			list.Add(new DdField(name, off, len, dt, scale));
		}
		_ddCache[ddName] = list;
		return list;
	}

	private static List<DdEntry> LoadDdList(string ddControlPath)
	{
		var entries = new List<DdEntry>();
		bool inList = false;
		foreach (var raw in File.ReadAllLines(ddControlPath))
		{
			var line = raw.Trim();
			if (line.Length == 0) continue;
			if (line.StartsWith("DDLIST")) { inList = true; continue; }
			if (!inList) continue;
			var parts = line.Split(',', StringSplitOptions.TrimEntries);
			if (parts.Length < 6) continue;
			var name = parts[0];
			var recId = parts[1];
			if (!int.TryParse(parts[2], out var recOff)) recOff = -1;
			var container = parts[3];
			var ddNum = parts[4];
			var type = parts[5];
			entries.Add(new DdEntry(name, recId, recOff, container, ddNum, type));
		}
		return entries;
	}

	private static char DecodeEbcChar(byte b)
	{
		return b switch
		{
			0xC1 => 'A', 0xC2 => 'B', 0xC3 => 'C', 0xC4 => 'D', 0xC5 => 'E', 0xC6 => 'F',
			0xC7 => 'G', 0xC8 => 'H', 0xC9 => 'I', 0xD1 => 'J', 0xD2 => 'K', 0xD3 => 'L',
			0xD4 => 'M', 0xD5 => 'N', 0xD6 => 'O', 0xD7 => 'P', 0xD8 => 'Q', 0xD9 => 'R',
			0xE2 => 'S', 0xE3 => 'T', 0xE4 => 'U', 0xE5 => 'V', 0xE6 => 'W', 0xE7 => 'X',
			0xE8 => 'Y', 0xE9 => 'Z',
			0xF0 => '0', 0xF1 => '1', 0xF2 => '2', 0xF3 => '3', 0xF4 => '4',
			0xF5 => '5', 0xF6 => '6', 0xF7 => '7', 0xF8 => '8', 0xF9 => '9',
			_ => '?' };
	}

	private static byte[] EbcToAscii(byte[] ebc, int offset, int length)
	{
		var ebcdic = Encoding.GetEncoding(37);
		var ascii = Encoding.ASCII;
		var chars = ebcdic.GetChars(ebc, offset, length);
		return ascii.GetBytes(chars);
	}

	private static bool IsHeaderDd(string ddName)
	{
		return ddName.Equals("mba.dd", StringComparison.OrdinalIgnoreCase)
			|| ddName.Equals("mbd.dd", StringComparison.OrdinalIgnoreCase);
	}

	private static bool IsAllEbcSpaces(byte[] buffer, int offset, int length)
	{
		for (int i = 0; i < length; i++)
		{
			if (buffer[offset + i] != 0x40) return false; // EBCDIC space
		}
		return true;
	}

	private static bool IsDigitsOrSpaces(byte[] buffer, int offset, int length)
	{
		bool seenDigit = false;
		for (int i = 0; i < length; i++)
		{
			byte b = buffer[offset + i];
			if (b == 0x40) continue; // space
			if (b >= 0xF0 && b <= 0xF9) { seenDigit = true; continue; } // digits 0-9
			return false;
		}
		return seenDigit; // require at least one digit to consider it numeric
	}

	private static void ConvertDisplayNumericEbcToAscii(byte[] src, int srcOffset, int length, byte[] dest, int destOffset)
	{
		for (int i = 0; i < length; i++)
		{
			byte b = src[srcOffset + i];
			if (b == 0x40)
			{
				dest[destOffset + i] = 0x20; // ASCII space
			}
			else if (b >= 0xF0 && b <= 0xF9)
			{
				dest[destOffset + i] = (byte)('0' + (b - 0xF0));
			}
			else
			{
				// fallback: leave raw copied earlier
			}
		}
	}

	private static void CopyRange(byte[] src, byte[] dest, int start, int end)
	{
		if (start < 0) start = 0;
		if (end > 4000) end = 4000;
		if (end <= start) return;
		Buffer.BlockCopy(src, start, dest, start, end - start);
	}

	private void DebugField(string ddName, DdField field, string action, byte[] source, int offsetOverride = -1, int lengthOverride = -1)
	{
		if (!_debug.Enabled) return;
		var key = $"{ddName}:{field.Name}:{action}";
		if (_debugCounts.TryGetValue(key, out var count) && count >= _debug.LimitPerField) return;
		count++;
		_debugCounts[key] = count;
		int offset = offsetOverride >= 0 ? offsetOverride : field.Offset;
		int length = lengthOverride >= 0 ? lengthOverride : field.Length;
		var preview = GetHexPreview(source, offset, length, _debug.HexPreviewLength);
		Console.WriteLine($"[STEP1][{action}] dd={ddName} field={field.Name} off={offset} len={length} preview={preview}");
	}

	private static string GetHexPreview(byte[] buffer, int offset, int length, int maxBytes)
	{
		var bytes = buffer.Skip(offset).Take(Math.Min(length, maxBytes)).ToArray();
		return string.Join(' ', bytes.Select(b => b.ToString("X2")));
	}

	private static Dictionary<string, DdOverride> LoadOverrides(string path)
	{
		if (!File.Exists(path)) return new();
		var json = File.ReadAllText(path);
		var dto = JsonSerializer.Deserialize<OverridesFile>(json) ?? new OverridesFile();
		return dto.Dds
			.Select(kvp => new KeyValuePair<string, DdOverride>(kvp.Key, new DdOverride(
				KeepRawFields: kvp.Value.KeepRawFields?.ToHashSet(StringComparer.OrdinalIgnoreCase) ?? new(),
				RawWindows: kvp.Value.RawWindows?.Select(w => new RawWindow(w.Start, w.End, Enum.TryParse<RawFillMode>(w.FillMode, true, out var mode) ? mode : RawFillMode.Raw)).ToList() ?? new())) )
			.ToDictionary(kvp => kvp.Key, kvp => kvp.Value, StringComparer.OrdinalIgnoreCase);
	}

private sealed record DdOverride(HashSet<string> KeepRawFields, List<RawWindow> RawWindows);
private sealed record RawWindow(int Start, int End, RawFillMode FillMode);
private enum RawFillMode { Raw, AsciiSpace }
	private sealed class OverridesFile
	{
		public Dictionary<string, OverrideEntry> Dds { get; init; } = new(StringComparer.OrdinalIgnoreCase);

		public sealed class OverrideEntry
		{
			public List<string>? KeepRawFields { get; init; }
			public List<Window>? RawWindows { get; init; }

			public sealed class Window
			{
				public int Start { get; init; }
				public int End { get; init; }
				public string? FillMode { get; init; }
			}
		}
	}

	private sealed record Step1DebugOptions(bool Enabled, int LimitPerField, int HexPreviewLength)
	{
		public static Step1DebugOptions FromEnvironment()
		{
			bool enabled = Environment.GetEnvironmentVariable("STEP1_DEBUG")?.Equals("1", StringComparison.OrdinalIgnoreCase) == true;
			int limit = 5;
			int hexPreview = 32;
			if (int.TryParse(Environment.GetEnvironmentVariable("STEP1_DEBUG_LIMIT"), out var parsedLimit) && parsedLimit > 0)
			{
				limit = parsedLimit;
			}
			if (int.TryParse(Environment.GetEnvironmentVariable("STEP1_DEBUG_HEX"), out var parsedHex) && parsedHex > 0)
			{
				hexPreview = parsedHex;
			}
			return new Step1DebugOptions(enabled, limit, hexPreview);
		}
	}
}
