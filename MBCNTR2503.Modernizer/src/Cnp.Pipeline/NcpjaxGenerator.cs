using System;
using System.IO;
using System.Text;

namespace Cnp.Pipeline
{
    /// <summary>
    /// Generates .ncpjax files by reading ddcontrol.txt and extracting NCPJAX configuration values.
    /// Replicates the functionality of legacy cntrvalue.c program.
    /// </summary>
    public class NcpjaxGenerator
    {
        private const string CLIENT_OFFSET = "300";

        /// <summary>
        /// Generates the .ncpjax file for a given job by parsing ddcontrol.txt
        /// </summary>
        /// <param name="jobId">Job identifier (e.g., "69172")</param>
        /// <param name="ddControlPath">Path to ddcontrol.txt file</param>
        /// <param name="outputDir">Output directory for the .ncpjax file</param>
        public void GenerateNcpjaxFile(string jobId, string ddControlPath, string outputDir)
        {
            Console.WriteLine($"[NCPJAX-GENERATOR] Starting .ncpjax generation for job {jobId}");
            Console.WriteLine($"[NCPJAX-GENERATOR] Reading ddcontrol.txt from: {ddControlPath}");

            if (!File.Exists(ddControlPath))
            {
                throw new FileNotFoundException($"ddcontrol.txt not found at: {ddControlPath}");
            }

            // Ensure output directory exists
            Directory.CreateDirectory(outputDir);

            string? ncpjaxValue = null;
            string? cntrKeyValue = null;

            // Read ddcontrol.txt and look for NCPJAX and CNTRKEY entries
            using (var reader = new StreamReader(ddControlPath))
            {
                string? line;
                while ((line = reader.ReadLine()) != null)
                {
                    line = line.Trim();
                    if (string.IsNullOrEmpty(line)) continue;

                    // Look for CONTAINER KEY entries (legacy format from cntrvalue.c)
                    if (line.StartsWith("CONTAINER KEY,"))
                    {
                        string keyPart = line.Substring("CONTAINER KEY,".Length).Trim();
                        cntrKeyValue = keyPart;
                        Console.WriteLine($"[NCPJAX-GENERATOR] Found CONTAINER KEY: {cntrKeyValue}");
                    }
                    // Look for NCPJAX entries in ddcontrol.txt format
                    else if (line.Contains(", NCPJAX"))
                    {
                        // Parse line like: "mblps.dd, , , , x, NCPJAX"
                        int commaIndex = line.IndexOf(',');
                        if (commaIndex > 0)
                        {
                            ncpjaxValue = line.Substring(0, commaIndex).Trim();
                            Console.WriteLine($"[NCPJAX-GENERATOR] Found NCPJAX value: {ncpjaxValue}");
                        }
                    }
                }
            }

            // Generate .ncpjax file if NCPJAX value was found
            if (!string.IsNullOrEmpty(ncpjaxValue))
            {
                string ncpjaxPath = Path.Combine(outputDir, $"{jobId}.ncpjax");
                
                // Write the value + newline (replicating legacy sprintf + write behavior)
                using (var writer = new FileStream(ncpjaxPath, FileMode.Create, FileAccess.Write))
                {
                    byte[] content = Encoding.ASCII.GetBytes($"{ncpjaxValue}\n");
                    writer.Write(content, 0, content.Length);
                }

                Console.WriteLine($"[NCPJAX-GENERATOR] Generated {ncpjaxPath} ({new FileInfo(ncpjaxPath).Length} bytes)");
                Console.WriteLine($"[NCPJAX-GENERATOR] Content: {ncpjaxValue}");
            }
            else
            {
                Console.WriteLine($"[NCPJAX-GENERATOR] WARNING: No NCPJAX value found in ddcontrol.txt");
            }

            // Generate .cntrkey file if CNTRKEY value was found
            if (!string.IsNullOrEmpty(cntrKeyValue))
            {
                string cntrKeyPath = Path.Combine(outputDir, $"{jobId}.cntrkey");
                
                using (var writer = new FileStream(cntrKeyPath, FileMode.Create, FileAccess.Write))
                {
                    byte[] content = Encoding.ASCII.GetBytes($"{cntrKeyValue}\n");
                    writer.Write(content, 0, content.Length);
                }

                Console.WriteLine($"[NCPJAX-GENERATOR] Generated {cntrKeyPath} ({new FileInfo(cntrKeyPath).Length} bytes)");
                Console.WriteLine($"[NCPJAX-GENERATOR] Content: {cntrKeyValue}");
            }

            Console.WriteLine($"[NCPJAX-GENERATOR] Completed .ncpjax generation for job {jobId}");
        }
    }
}
