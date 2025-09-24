using System;
using System.IO;
using System.Threading.Tasks;

namespace MBCNTR2503.Pipeline
{
    class Program
    {
    static async Task Main(string[] args)
        {
            Console.WriteLine("🚀 MBCNTR2503 Pipeline Runner");
            Console.WriteLine("==================================================");

            try
            {
                // Parse command line arguments
                if (args.Length < 1)
                {
                    Console.WriteLine("Usage: PipelineRunner <job_number>");
                    Console.WriteLine("Example: PipelineRunner 69172");
                    return;
                }

                string jobNumber = args[0];
                bool debugEbcdic = args.Length > 1 && string.Equals(args[1], "--debug-ebcdic", StringComparison.OrdinalIgnoreCase);

                // Configure the pipeline
                // Create test configuration
                var workspaceRoot = @"c:\Users\Shan\Documents\3OSG\MBCNTR2503.Modernizer";
                var publicPath = Path.Combine(workspaceRoot, "Pipeline_Test", "Public");
                var programsPath = Path.Combine(workspaceRoot, "Pipeline_Test", "Programs");
                
                var config = new PipelineConfiguration
                {
                    Client = "2503",
                    Work2Length = 4300,
                    Project = "mblps",
                    ProjectBase = programsPath,
                    PublicPath = publicPath,
                    ProgramsPath = programsPath,
                    
                };

                // Create directories
                Directory.CreateDirectory(config.PublicPath);
                Directory.CreateDirectory(config.ProgramsPath);

                // Create supplemental table file for testing
                var suppTablePath = Path.Combine(config.ProgramsPath, "2503supptable.txt");
                if (!File.Exists(suppTablePath))
                {
                    await File.WriteAllTextAsync(suppTablePath, "# Sample supplemental table\n# This is a test file for pipeline execution\n");
                    Console.WriteLine($"📝 Created test supplemental table: {suppTablePath}");
                }

                // Copy real input file from Legacy Application
                var inputPath = Path.Combine(config.PublicPath, $"{jobNumber}.dat");
                var legacyInputPath = Path.Combine(workspaceRoot, "..", "Legacy Application", "Input", $"{jobNumber}.dat");
                
                if (File.Exists(legacyInputPath))
                {
                    File.Copy(legacyInputPath, inputPath, overwrite: true);
                    Console.WriteLine($"📝 Copied real input file: {inputPath} ({new FileInfo(inputPath).Length} bytes)");
                }
                else
                {
                    Console.WriteLine($"❌ Error: Input file not found: {legacyInputPath}");
                    return;
                }

                Console.WriteLine($"📋 Pipeline Configuration:");
                Console.WriteLine($"   Job Number: {jobNumber}");
                Console.WriteLine($"   Client: {config.Client}");
                Console.WriteLine($"   Work2Length: {config.Work2Length}");
                Console.WriteLine($"   Public Path: {config.PublicPath}");
                Console.WriteLine();

                // Create logger
                var logger = new ConsoleLogger();

                // Optional: quick EBCDIC parse debug to validate packed decimal extraction (MB_ACCOUNT)
                if (debugEbcdic)
                {
                    try
                    {
                        var fieldDefsPath = Path.Combine(AppContext.BaseDirectory, "FieldDefinitions_Generated.json");
                        if (!File.Exists(fieldDefsPath))
                        {
                            Console.WriteLine($"❌ Field definitions not found at {fieldDefsPath}");
                        }
                        else
                        {
                            Console.WriteLine("🔬 Running EBCDIC parse debug (first 5 matching records)...");
                            var parser = new EbcdicRecordParser(fieldDefsPath, logger);
                            var records = parser.ParseEbcdicFile(inputPath, layoutName: "MBLPS", clientCode: "503");

                            int count = 0;
                            foreach (var rec in records)
                            {
                                count++;
                                var acct = rec.GetFieldAsDecimal("MB_ACCOUNT");
                                var formatted = rec.GetFieldAsString("MB_FORMATTED_ACCOUNT");
                                Console.WriteLine($"   Rec#{rec.RecordNumber} MB_ACCOUNT={acct} MB_FORMATTED_ACCOUNT='{formatted}'");
                                if (count >= 5) break;
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine($"⚠️ EBCDIC parse debug failed: {ex.Message}");
                    }
                }

                // Create orchestrator
                var orchestrator = new PipelineOrchestrator(config, logger);

                // Execute pipeline
                Console.WriteLine("🔄 Starting pipeline execution...");
                Console.WriteLine();

                var result = await orchestrator.ExecutePipelineAsync(jobNumber, "real");

                Console.WriteLine();
                if (result.Success)
                {
                    Console.ForegroundColor = ConsoleColor.Green;
                    Console.WriteLine($"🎉 Pipeline completed successfully!");
                    Console.WriteLine($"   Duration: {result.Duration.TotalSeconds:F2} seconds");
                    Console.WriteLine($"   Steps completed: {result.Steps.Count}");
                    Console.ResetColor();

                    Console.WriteLine();
                    Console.WriteLine("📊 Step Summary:");
                    foreach (var step in result.Steps)
                    {
                        var status = step.Success ? "✅" : "❌";
                        Console.WriteLine($"   {status} {step.Name} ({step.Duration.TotalMilliseconds:F0}ms)");
                    }
                }
                else
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.WriteLine($"❌ Pipeline failed: {result.ErrorMessage}");
                    Console.ResetColor();
                }
            }
            catch (Exception ex)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine($"💥 Unexpected error: {ex.Message}");
                Console.WriteLine($"Stack trace: {ex.StackTrace}");
                Console.ResetColor();
            }

            Console.WriteLine();
            // Avoid blocking in automated runs
            var noPause = Environment.GetEnvironmentVariable("NO_PAUSE");
            var ci = Environment.GetEnvironmentVariable("CI");
            if (!string.Equals(noPause, "1", StringComparison.OrdinalIgnoreCase) &&
                !string.Equals(ci, "true", StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine("Press any key to exit...");
                Console.ReadKey();
            }
        }
    }
}