using System.Text;
using Cnp.Schema;

namespace Cnp.Pipeline;

public sealed class Step1Orchestrator
{
	private readonly CompiledSchema _schema;
	public Step1Orchestrator(CompiledSchema schema) => _schema = schema;

	private sealed record DdEntry(string Name, string RecordId, int RecordIdOffset, string ContainerId, string DdNumber, string Type);
	private sealed record DdField(string Name, int Offset, int Length, string DataType, int Scale);
	private readonly Dictionary<string, List<DdField>> _ddCache = new(StringComparer.OrdinalIgnoreCase);

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
					continue;
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

				// initialize output record with spaces and copy raw 4000 bytes
				Array.Fill(outRec, (byte)' ');
				Buffer.BlockCopy(buffer, 0, outRec, 0, 4000);
				// apply EBCDIC->ASCII only on Text fields from matched dd layout
				var ddFields = GetDdFields(matched.Name);
				foreach (var f in ddFields)
				{
					if (f.Offset < 0 || f.Offset + f.Length > 4000) continue;
					var dt = f.DataType.Trim().ToLowerInvariant();
					if (dt == "text")
					{
						var seg = EbcToAscii(buffer, f.Offset, f.Length);
						Buffer.BlockCopy(seg, 0, outRec, f.Offset, seg.Length);
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
}
