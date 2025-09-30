using System.Collections.Generic;
using System.Text;

namespace Cnp.Pipeline;

/// <summary>
/// Merges .4300.txt files with trailing bytes from .4300 binary files
/// Generates .4300.txt.new and .4300.txt.length files
/// Replicates legacy ncpcntr5.c functionality
/// </summary>
public sealed class TxtNewMerger
{
    private const int CLIENT_OFFSET = 300; // Last 300 bytes reserved for client-specific info

    /// <summary>
    /// Merge .4300.txt file with trailing bytes from .4300 binary file
    /// Replicates logic from ncpcntr5.c lines 37-142
    /// </summary>
    public void MergeTextWithBinary(string jobId, string txtFile, string binaryFile, string outputDir)
    {
        Console.WriteLine($"[TXT-NEW-MERGER] Starting txt.new merge for job {jobId}");
        Console.WriteLine($"[TXT-NEW-MERGER] Text input: {txtFile}");
        Console.WriteLine($"[TXT-NEW-MERGER] Binary input: {binaryFile}");
        Console.WriteLine($"[TXT-NEW-MERGER] Output: {outputDir}");

        if (!File.Exists(txtFile))
        {
            throw new FileNotFoundException($"Text file not found: {txtFile}");
        }

        if (!File.Exists(binaryFile))
        {
            throw new FileNotFoundException($"Binary file not found: {binaryFile}");
        }

        Directory.CreateDirectory(outputDir);

        var lengthFile = Path.Combine(outputDir, $"{jobId}.4300.txt.length");
        var newFile = Path.Combine(outputDir, $"{jobId}.4300.txt.new");

        // Step 1: Calculate maximum line length from text file (replicating legacy fgets + strlen logic)
        int maxLineLength = CalculateMaxLineLength(txtFile);
    // Note: Some legacy environments showed a +1 discrepancy for .length, but expected
    // baselines in this project do NOT include that quirk. Align to expected by writing
    // the exact record length (text + client trailer bytes).
    int recordLength = maxLineLength + CLIENT_OFFSET;  // Actual record length for .new file
    int lengthFileValue = recordLength;                // Write exact record length (no +1)

        Console.WriteLine($"[TXT-NEW-MERGER] Max line length: {maxLineLength}, Length file value: {lengthFileValue}, Record length: {recordLength}");

    // Step 2: Write .length file (aligned to expected – no legacy +1)
        WriteTextLengthFile(lengthFile, lengthFileValue);

        // Step 3: Generate .new file by merging text with binary trailing bytes (using actual record length)
        MergeTextWithBinaryTrailingBytes(txtFile, binaryFile, newFile, recordLength);

        Console.WriteLine($"[TXT-NEW-MERGER] Completed merge:");
        Console.WriteLine($"[TXT-NEW-MERGER] Length file: \"{lengthFile}\"");
        Console.WriteLine($"[TXT-NEW-MERGER] New file: \"{newFile}\"");
    }

    /// <summary>
    /// Calculate maximum line length from text file
    /// Replicates ncpcntr5.c lines 59-71
    /// </summary>
    private static int CalculateMaxLineLength(string txtFile)
    {
        int maxLength = 0;
        int recordCount = 0;

        using var stream = new FileStream(txtFile, FileMode.Open, FileAccess.Read, FileShare.Read);
        using var buffered = new BufferedStream(stream);

        int currentLength = 0;
        int currentByte;
        while ((currentByte = buffered.ReadByte()) != -1)
        {
            currentLength++;

            if (currentByte == '\n')
            {
                recordCount++;
                if (currentLength > maxLength)
                {
                    maxLength = currentLength;
                }

                currentLength = 0;
            }
        }

        if (currentLength > 0)
        {
            recordCount++;
            if (currentLength > maxLength)
            {
                maxLength = currentLength;
            }
        }

        Console.WriteLine($"[TXT-NEW-MERGER] Records processed: {recordCount}, Max length: {maxLength}");
        return maxLength;
    }

    /// <summary>
    /// Write .length file with calculated output length
    /// Replicates ncpcntr5.c lines 73-83
    /// </summary>
    private static void WriteTextLengthFile(string lengthFile, int outputLength)
    {
        using var writer = new StreamWriter(lengthFile, false, new UTF8Encoding(false));
        writer.WriteLine(outputLength);
    }

    /// <summary>
    /// Merge text lines with trailing bytes from binary records
    /// Replicates ncpcntr5.c lines 87-142
    /// </summary>
    private static void MergeTextWithBinaryTrailingBytes(string txtFile, string binaryFile, string newFile, int outputLength)
    {
        const int BINARY_RECORD_LENGTH = 4300; // .4300 file record length

        using var binaryReader = new FileStream(binaryFile, FileMode.Open, FileAccess.Read);
        using var newWriter = new FileStream(newFile, FileMode.Create, FileAccess.Write);

        var binaryBuffer = new byte[BINARY_RECORD_LENGTH];
        var outputBuffer = new byte[outputLength];
        int recordCount = 0;

        foreach (var recordBytes in ReadRecordsWithTerminators(txtFile))
        {
            recordCount++;

            // Read corresponding binary record
            int bytesRead = binaryReader.Read(binaryBuffer, 0, BINARY_RECORD_LENGTH);
            if (bytesRead != BINARY_RECORD_LENGTH)
            {
                throw new InvalidDataException($"Binary file truncated at record {recordCount}");
            }

            // Initialize output buffer with spaces (ASCII 0x20)
            Array.Fill(outputBuffer, (byte)' ');

            var textCopyLength = Math.Min(recordBytes.Length, outputLength - CLIENT_OFFSET);
            Array.Copy(recordBytes, 0, outputBuffer, 0, textCopyLength);

            if (recordBytes.Length > textCopyLength && recordBytes[^1] == (byte)'\n')
            {
                outputBuffer[textCopyLength] = (byte)'\n';
            }

            // Copy last CLIENT_OFFSET bytes from binary record to end of output buffer
            Array.Copy(binaryBuffer, BINARY_RECORD_LENGTH - CLIENT_OFFSET, 
                      outputBuffer, outputLength - CLIENT_OFFSET, CLIENT_OFFSET);

            // Write merged record to output
            newWriter.Write(outputBuffer, 0, outputLength);
        }

        Console.WriteLine($"[TXT-NEW-MERGER] Records merged: {recordCount}");
    }

    private static IEnumerable<byte[]> ReadRecordsWithTerminators(string txtFile)
    {
        using var stream = new FileStream(txtFile, FileMode.Open, FileAccess.Read, FileShare.Read);
        using var buffered = new BufferedStream(stream);

        var record = new List<byte>(512);
        int currentByte;

        while ((currentByte = buffered.ReadByte()) != -1)
        {
            record.Add((byte)currentByte);

            if (currentByte == '\n')
            {
                yield return record.ToArray();
                record.Clear();
            }
        }

        if (record.Count > 0)
        {
            yield return record.ToArray();
        }
    }
}
