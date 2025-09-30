using Cnp.Schema;
using System;
using System.IO;
using System.Text;
using System.Diagnostics;

namespace Cnp.Pipeline;

public class PipelineOrchestrator
{
    private readonly CompiledSchema _schema;
    private readonly string _baseDir;
    private readonly bool _verbose;

    public PipelineOrchestrator(CompiledSchema schema, string baseDir, bool verbose = false)
    {
        _schema = schema;
        _baseDir = baseDir;
        _verbose = verbose;
    }

    public void Run(string jobId)
    {
        // Debug loaded schema record length
        Console.WriteLine($"[PIPELINE][DBG] Loaded Container4000.Length={_schema.Container4000.Length}");
        var inputDat = Path.Combine(_baseDir, "MBCNTR2503.Modernizer", "Input", $"{jobId}.dat");
        var outDir = Path.Combine(_baseDir, "MBCNTR2503.Modernizer", "out", jobId);
        
        // Clear the output directory to ensure a clean run
        if (Directory.Exists(outDir))
        {
            Directory.Delete(outDir, recursive: true);
        }
        Directory.CreateDirectory(outDir);

        Console.WriteLine($"--- Running full pipeline for job {jobId} ---");

        // Step 1: Container Step 1 Artifacts
        Console.WriteLine("  Step 1: Generating Container Step 1 artifacts...");
        var step1Orchestrator = new Step1Orchestrator(_schema);
        var containerFile = Path.Combine(outDir, $"{jobId}.4300");
        var rectypeFile = Path.Combine(outDir, $"{jobId}.dat.rectype");
        var totalFile = Path.Combine(outDir, $"{jobId}.dat.total");
        step1Orchestrator.Run(jobId, inputDat, outDir);
        Console.WriteLine("    ✅ .4300, .dat.rectype, .dat.total files generated.");

        // Step 1a: Text Extraction
        var textExtractor = new TextExtractor(_schema);
        var extractedTextFile = Path.Combine(outDir, $"{jobId}.4300.txt");
        textExtractor.ExtractText(jobId, containerFile, outDir);
        Console.WriteLine("    ✅ .4300.txt file generated.");

        // Step 1b: Suspect Validation
        var suspectValidator = new SuspectValidator();
        suspectValidator.ValidateTextFile(jobId, extractedTextFile, outDir);
        Console.WriteLine("    ✅ .4300.txt.suspect file generated.");

        // Step 1c: Text-Binary Merge
        var txtNewMerger = new TxtNewMerger();
        txtNewMerger.MergeTextWithBinary(jobId, extractedTextFile, containerFile, outDir);
        Console.WriteLine("    ✅ .4300.txt.new and .length files generated.");
        
        // Step 1d: NCPJAX Key Generation with client-specific schema
        var ncpjaxGenerator = new NcpjaxGenerator();
        // Detect client number from input file header
        var clientBuffer = new byte[4];
        string clientNumber = "";
        using (var fs = File.OpenRead(inputDat))
        {
            if (fs.Read(clientBuffer, 0, 4) == 4)
            {
                var ebcdic = Encoding.GetEncoding(37);
                clientNumber = new string(ebcdic.GetChars(clientBuffer, 0, 4));
                Console.WriteLine($"[PIPELINE] Detected client number: {clientNumber}");
            }
        }
        // Use the configured schema directory (CLI --schema) for control file and overrides
        var schemaDirToUse = _schema.SourceDir;
        var ddControlPath = Path.Combine(schemaDirToUse, "ddcontrol.txt");
        ncpjaxGenerator.GenerateNcpjaxFile(jobId, ddControlPath, outDir);
        Console.WriteLine("    ✅ .ncpjax file generated.");
        Console.WriteLine("  Step 1: Done.");


        // Step 2: EBCDIC to ASCII
        Console.WriteLine("  Step 2: EBCDIC to ASCII conversion...");
        var ebcdicProcessor = new EbcdicProcessor(_schema, _verbose);
        ebcdicProcessor.ProcessDatToAsc(jobId, inputDat, outDir);
        var datAscFile = Path.Combine(outDir, $"{jobId}.dat.asc");
        Console.WriteLine("  Step 2: Done.");

        // Step 3: Key Enrichment
        Console.WriteLine("  Step 3: Key enrichment...");
        var pFile = Path.Combine(outDir, $"{jobId}.dat.asc.11.1.p");
        var sFile = Path.Combine(outDir, $"{jobId}.dat.asc.11.1.s");
        var keyedPFile = Path.Combine(outDir, $"{jobId}.dat.asc.11.1.p.keyed");

        // Use hardcoded record length 1500 for key enrichment (P and S records)
        const int peRecLen = 1500;
        const int seRecLen = 1500;
        Console.WriteLine($"[PIPELINE][DBG] KeyEnrichment recordLen p={peRecLen}, s={seRecLen}");
        var keyEnrichment = new KeyEnrichmentProcessor(peRecLen, seRecLen, 4, 4, 7, 1080, 7, 3, true);
        keyEnrichment.ProcessFiles(pFile, sFile, keyedPFile);
        Console.WriteLine("  Step 3: Done.");

        // Step 4: MB2000 Conversion with client-specific overrides
        Console.WriteLine("  Step 4: MB2000 conversion...");
        var clientOverrideFile = Path.Combine(schemaDirToUse, $"mb2000.overrides.{clientNumber}.json");
        var overridePath = File.Exists(clientOverrideFile)
            ? clientOverrideFile
            : Path.Combine(schemaDirToUse, "mb2000.overrides.json");
        Console.WriteLine(File.Exists(clientOverrideFile)
            ? $"[PIPELINE] Using client-specific overrides: {clientOverrideFile}"
            : $"[PIPELINE] Using base overrides: {overridePath}");
        var mapper = new MB2000FieldMapper(_schema, overridePath);
        var outputFile = Path.Combine(outDir, $"{jobId}p.set");

        // *** FIX: Use original EBCDIC .dat file (4000-byte pure EBCDIC records) ***
        // This allows all 566 overrides to be mapped with correct EBCDIC→ASCII conversion
        using (var inStream = File.OpenRead(inputDat))
        using (var outStream = new FileStream(outputFile, FileMode.Create, FileAccess.Write))
        {
            // Read pure EBCDIC 4000-byte records from input .dat file
            const int container4000Len = 4000;
            Console.WriteLine($"[PIPELINE][DBG] Reading input .dat with recordLen={container4000Len} (filtering P-records only)");
            var buffer = new byte[container4000Len];
            int bytesRead;
            int totalRecords = 0;
            int pRecordsProcessed = 0;
            
            while ((bytesRead = inStream.Read(buffer, 0, container4000Len)) == container4000Len)
            {
                totalRecords++;
                
                // Filter to P-records only: Check byte 11 for record type
                // In pure EBCDIC .dat file, byte 11 contains EBCDIC 'P' (0xD7) for P-records
                if (buffer[11] == 0xD7)  // EBCDIC 'P'
                {
                    pRecordsProcessed++;
                    var mapped = mapper.Map(buffer);
                    outStream.Write(mapped, 0, mapped.Length);
                }
            }
            
            Console.WriteLine($"[PIPELINE] MB2000: Processed {pRecordsProcessed} P-records out of {totalRecords} total records");
        }
        Console.WriteLine("  Step 4: Done.");

        Console.WriteLine($"--- Pipeline complete for job {jobId} ---");
    }
}
