using System.Text;

namespace Cnp.Pipeline;

/// <summary>
/// Validates .4300.txt files for suspect characters and generates .4300.txt.suspect files
/// Replicates legacy ncpcntrextractvalidation.c functionality
/// </summary>
public sealed class SuspectValidator
{
    /// <summary>
    /// Validate a .4300.txt file and generate corresponding .suspect file
    /// Replicates logic from ncpcntrextractvalidation.c lines 48-77
    /// </summary>
    public void ValidateTextFile(string jobId, string inputTxtFile, string outputDir)
    {
        Console.WriteLine($"[SUSPECT-VALIDATOR] Starting suspect validation for job {jobId}");
        Console.WriteLine($"[SUSPECT-VALIDATOR] Input: {inputTxtFile}");
        Console.WriteLine($"[SUSPECT-VALIDATOR] Output: {outputDir}");

        if (!File.Exists(inputTxtFile))
        {
            throw new FileNotFoundException($"Input text file not found: {inputTxtFile}");
        }

        Directory.CreateDirectory(outputDir);

        var suspectFile = Path.Combine(outputDir, $"{jobId}.4300.txt.suspect");
        
        int readCount = 0;
        int writeCount = 0;

        using var reader = new StreamReader(inputTxtFile, Encoding.ASCII);
        using var writer = new StreamWriter(suspectFile, false, new UTF8Encoding(false));

        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            readCount++;
            
            int suspectCount = CountSuspectCharacters(line);
            
            // Legacy threshold: if suspectCount > 4, write line to suspect file
            if (suspectCount > 4)
            {
                writer.WriteLine(line);
                writeCount++;
                Console.WriteLine($"[SUSPECT-VALIDATOR] Suspect line found at ReadCount={readCount}");
            }
        }

        Console.WriteLine($"[SUSPECT-VALIDATOR] Completed validation:");
        Console.WriteLine($"[SUSPECT-VALIDATOR] Output file: \"{suspectFile}\", writeCount={writeCount} of {readCount}");
    }

    /// <summary>
    /// Count suspect characters in a line following legacy logic
    /// Replicates ncpcntrextractvalidation.c lines 50-70
    /// </summary>
    private static int CountSuspectCharacters(string line)
    {
        int suspectCount = 0;
        
        for (int i = 0; i < line.Length; i++)
        {
            char c = line[i];
            byte b = (byte)c;

            // Check for LF without preceding CR (adds 4 to count)
            if (c == '\n' && (i == 0 || line[i - 1] != '\r'))
            {
                suspectCount += 4;
            }
            // Check for HIGH-VALUES (0xFF) fields
            else if (b == 255 && (i == 0 || line[i - 1] == '|'))
            {
                // Find end of HIGH-VALUES sequence
                int endIndex = i;
                while (endIndex < line.Length && (byte)line[endIndex] == 255)
                {
                    endIndex++;
                }

                // If HIGH-VALUES field ends with '|' or is at end of line, it's valid
                if (endIndex < line.Length && line[endIndex] == '|')
                {
                    i = endIndex; // Skip to end of valid HIGH-VALUES field
                }
                else if (endIndex >= line.Length)
                {
                    i = endIndex - 1; // Skip to end of line
                }
                else
                {
                    // Invalid HIGH-VALUES sequence, count as suspect
                    suspectCount++;
                }
            }
            // Check for non-printable characters
            else if (!IsPrintableCharacter(c))
            {
                suspectCount++;
            }
        }

        return suspectCount;
    }

    /// <summary>
    /// Check if character is printable (following legacy isprint() logic)
    /// </summary>
    private static bool IsPrintableCharacter(char c)
    {
        // Standard ASCII printable range: 32-126 (space through tilde)
        // Plus common line ending characters
        return c >= 32 && c <= 126 || c == '\r' || c == '\n' || c == '\t';
    }
}
