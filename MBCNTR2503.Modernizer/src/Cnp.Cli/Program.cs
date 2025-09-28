using Cnp.Schema;
using Cnp.Pipeline;
using System.Text;
using System;
using System.IO;
using NJsonSchema;
using NJsonSchema.CodeGeneration.CSharp;
using Newtonsoft.Json.Linq;
using System.Text.Json;
using System.Diagnostics; // Added for parity check

if (args.Length > 0 && args[0] == "build-schema")
{
    string? schemaDir = null;
    string? outDir = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
    }
    if (schemaDir == null || outDir == null)
    {
        Console.Error.WriteLine("Missing --schema or --out");
        Environment.Exit(1);
    }
    var compiled = SchemaCompiler.Compile(schemaDir);
    SchemaCompiler.Emit(compiled, outDir);
    Console.WriteLine($"Schema built: {compiled.Sha}");
    return;
}
else if (args.Length > 0 && args[0] == "run-step1")
{
    string? job = null; string? input = null; string? outDir = null; string? schemaDir = null;
    bool debug = false;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--input" && i + 1 < args.Length) input = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
        else if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
        else if (args[i] == "--debug") debug = true;
    }
    if (job == null || input == null || outDir == null || schemaDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --input --out --schema");
        Environment.Exit(1);
    }
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    var compiled = SchemaCompiler.Compile(schemaDir);
    if (debug) Environment.SetEnvironmentVariable("STEP1_DEBUG", "1");
    var step1 = new Step1Orchestrator(compiled);
    step1.Run(job, input, outDir);
    return;
}
else if (args.Length > 0 && args[0] == "extract-text")
{
    string? job = null; string? input = null; string? outDir = null; string? schemaDir = null;
    bool debug = false;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--input" && i + 1 < args.Length) input = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
        else if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
        else if (args[i] == "--debug") debug = true;
    }
    if (job == null || input == null || outDir == null || schemaDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --input --out --schema");
        Environment.Exit(1);
    }
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    var compiled = SchemaCompiler.Compile(schemaDir);
    if (debug) Environment.SetEnvironmentVariable("STEP1_DEBUG", "1");
    var textExtractor = new TextExtractor(compiled);
    textExtractor.ExtractText(job, input, outDir);
    return;
}
else if (args.Length > 0 && args[0] == "ebcdic-to-ascii")
{
    string? job = null; string? input = null; string? outDir = null; string? schemaDir = null;
    bool debug = false;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--input" && i + 1 < args.Length) input = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
        else if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
        else if (args[i] == "--debug") debug = true;
    }
    if (job == null || input == null || outDir == null || schemaDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --input --out --schema");
        Environment.Exit(1);
    }
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    Console.WriteLine("Compiling schema...");
    var compiled = SchemaCompiler.Compile(schemaDir);
    Console.WriteLine("Schema compiled successfully.");
    if (debug) Environment.SetEnvironmentVariable("STEP1_DEBUG", "1");
    var ebcdicProcessor = new EbcdicProcessor(compiled);
    ebcdicProcessor.ProcessDatToAsc(job, input, outDir);
    return;
}
else if (args.Length > 0 && args[0] == "validate-suspect")
{
    string? job = null; string? input = null; string? outDir = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--input" && i + 1 < args.Length) input = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
    }
    if (job == null || input == null || outDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --input --out");
        Environment.Exit(1);
    }
    var suspectValidator = new SuspectValidator();
    suspectValidator.ValidateTextFile(job, input, outDir);
    return;
}
else if (args.Length > 0 && args[0] == "merge-txt-new")
{
    string? job = null; string? txtInput = null; string? binaryInput = null; string? outDir = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--txt-input" && i + 1 < args.Length) txtInput = args[++i];
        else if (args[i] == "--binary-input" && i + 1 < args.Length) binaryInput = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
    }
    if (job == null || txtInput == null || binaryInput == null || outDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --txt-input --binary-input --out");
        Environment.Exit(1);
    }
    var txtNewMerger = new TxtNewMerger();
    txtNewMerger.MergeTextWithBinary(job, txtInput, binaryInput, outDir);
    return;
}
else if (args.Length > 0 && args[0] == "generate-ncpjax")
{
    string? job = null; string? ddControlPath = null; string? outDir = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--ddcontrol" && i + 1 < args.Length) ddControlPath = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
    }
    if (job == null || ddControlPath == null || outDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --ddcontrol --out");
        Environment.Exit(1);
    }
    var ncpjaxGenerator = new NcpjaxGenerator();
    ncpjaxGenerator.GenerateNcpjaxFile(job, ddControlPath, outDir);
    return;
}
else if (args.Length > 0 && args[0] == "enrich-keys")
{
    string? job = null; string? pFile = null; string? sFile = null; string? outDir = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--p-file" && i + 1 < args.Length) pFile = args[++i];
        else if (args[i] == "--s-file" && i + 1 < args.Length) sFile = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
    }
    if (job == null || pFile == null || sFile == null || outDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --p-file --s-file --out");
        Environment.Exit(1);
    }
    
    // Create output path following legacy naming convention
    string outputPath = Path.Combine(outDir, $"{job}.dat.asc.11.1.p.keyed");
    
    // Initialize processor with legacy parameters (matching setmb2000.script line 59)
    var keyEnrichment = new KeyEnrichmentProcessor(
        r1Length: 1500,        // $OutLength 
        r2Length: 1500,        // $OutLength
        a1Offset: 4,           // Account offset in P records
        a2Offset: 4,           // Account offset in S records  
        accountLength: 7,      // Account field length
        r2KeyOffset: 1080,     // Key write position
        r2KeyLength: 7,        // Key number length
        r2KeyCountLength: 3,   // Count number length
        keyFirst: true         // Key before Count format
    );
    
    keyEnrichment.ProcessFiles(pFile, sFile, outputPath);
    Console.WriteLine($"Key enrichment completed: {outputPath}");
    return;
}
else if (args.Length > 0 && args[0] == "mb2000-convert")
{
    string? job = null; string? input = null; string? outDir = null; string? schemaDir = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--input" && i + 1 < args.Length) input = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outDir = args[++i];
        else if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
    }
    if (job == null || input == null || outDir == null || schemaDir == null)
    {
        Console.Error.WriteLine("Missing required args: --job --input --out --schema");
        Environment.Exit(1);
    }
    var overridePath = Path.Combine(schemaDir, "mb2000.overrides.json");
    var compSchema = SchemaCompiler.Compile(schemaDir);
    var mapper = new MB2000FieldMapper(compSchema, overridePath);
    // Process keyed input as 1500-byte records and write 2000-byte .set records
    const int keyedLen = 1500;
    Directory.CreateDirectory(outDir);
    var outputFile = Path.Combine(outDir, $"{job}p.set");
    using (var inStream = File.OpenRead(input))
    using (var outStream = new FileStream(outputFile, FileMode.Create, FileAccess.Write))
    {
        var buffer = new byte[keyedLen];
        int bytesRead;
        while ((bytesRead = inStream.Read(buffer, 0, keyedLen)) == keyedLen)
        {
            var mapped = mapper.Map(buffer);
            outStream.Write(mapped, 0, mapped.Length);
        }
    }
    Console.WriteLine($"MB2000 conversion completed for job: {job}");
    // Run parity check via debug-parity.py
    // Locate debug-parity.py at the root of the Modernizer project
    var parityScript = Path.GetFullPath(Path.Combine(schemaDir, "..", "..", "..", "debug-parity.py"));
    var parityInfo = new ProcessStartInfo
    {
        FileName = "python3",
        Arguments = $"{parityScript} {job}",
        RedirectStandardOutput = true,
        UseShellExecute = false
    };
    using (var parityProc = Process.Start(parityInfo))
    {
        string parityOutput = parityProc!.StandardOutput.ReadToEnd();
        parityProc.WaitForExit();
        Console.WriteLine(parityOutput);
        if (parityOutput.Contains("Found 0 total field differences"))
        {
            Console.WriteLine($"🎯 PARITY SUCCESS: 100% field parity achieved for job: {job} ✅");
        }
        else
        {
            Console.Error.WriteLine($"❌ PARITY FAILURE: Field differences detected for job: {job}. Conversion incomplete.");
            Environment.Exit(1);
        }
    }
    return;
}
else if (args.Length > 0 && args[0] == "generate-models")
{
    string compiledDir = null; string outputProj = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--compiled-schemas" && i + 1 < args.Length) compiledDir = args[++i];
        else if (args[i] == "--out-project" && i + 1 < args.Length) outputProj = args[++i];
    }
    if (compiledDir == null || outputProj == null)
    {
        Console.Error.WriteLine("Missing required args: --compiled-schemas <dir> --out-project <proj-path>");
        Environment.Exit(1);
    }
    var schemaFiles = Directory.GetFiles(compiledDir, "*.schema.json");
    foreach (var schemaFile in schemaFiles)
    {
        var schema = JsonSchema.FromFileAsync(schemaFile).GetAwaiter().GetResult();
        var settings = new CSharpGeneratorSettings
        {
            Namespace = "Cnp.Schema.Generated",
            ClassStyle = CSharpClassStyle.Poco,
            GenerateDataAnnotations = true
        };
        var generator = new CSharpGenerator(schema, settings);
        var className = Path.GetFileNameWithoutExtension(schemaFile).Replace(".schema", "");
        var code = generator.GenerateFile(className);
        var modelDir = Path.Combine(outputProj, "Models");
        Directory.CreateDirectory(modelDir);
        var codePath = Path.Combine(modelDir, className + ".cs");
        File.WriteAllText(codePath, code);
        Console.WriteLine($"Generated {codePath}");
    }
    return;
}
else if (args.Length > 0 && args[0] == "init-overrides")
{
    string templatePath = null; string schemaPath = null; string outPath = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--template" && i + 1 < args.Length) templatePath = args[++i];
        else if (args[i] == "--schema" && i + 1 < args.Length) schemaPath = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outPath = args[++i];
    }
    if (templatePath == null || schemaPath == null || outPath == null)
    {
        Console.Error.WriteLine("Missing required args: --template <template.json> --schema <schema.json> --out <overrides.json>");
        Environment.Exit(1);
    }
    // Load template
    var templateJson = File.ReadAllText(templatePath);
    var template = Newtonsoft.Json.Linq.JObject.Parse(templateJson);
    // Load output schema and get properties
    var schema = NJsonSchema.JsonSchema.FromFileAsync(schemaPath).GetAwaiter().GetResult();
    var overridesArray = new Newtonsoft.Json.Linq.JArray();
    foreach (var prop in schema.Properties)
    {
        var field = prop.Key;
        var entry = new Newtonsoft.Json.Linq.JObject
        {
            ["source"] = field,
            ["target"] = field,
            ["sourceOffset"] = 0,
            ["sourceLength"] = 0,
            ["mode"] = "copyTrim",
            ["trimOutput"] = true
        };
        overridesArray.Add(entry);
    }
    template["overrides"] = overridesArray;
    // Write output
    File.WriteAllText(outPath, template.ToString(Newtonsoft.Json.Formatting.Indented));
    Console.WriteLine($"Initialized overrides file: {outPath}");
    return;
}
else if (args.Length > 0 && args[0] == "bootstrap-overrides-from-compiled")
{
    string schemaDir = null; string outPath = null;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
        else if (args[i] == "--out" && i + 1 < args.Length) outPath = args[++i];
    }
    if (schemaDir == null || outPath == null)
    {
        Console.Error.WriteLine("Missing required args: bootstrap-overrides-from-compiled --schema <schemaDir> --out <overrides.json>");
        Environment.Exit(1);
    }
    // Compile schema
    var compiled = SchemaCompiler.Compile(schemaDir);
    var overrides = new System.Collections.Generic.List<object>();
    foreach (var field in compiled.Container4000.Fields)
    {
        string mode;
        if (field.DataType.Contains("Packed", StringComparison.OrdinalIgnoreCase) || field.DataType.Contains("COMP-3", StringComparison.OrdinalIgnoreCase))
            mode = "packed";
        else if (field.DataType.Contains("Decimal", StringComparison.OrdinalIgnoreCase)
                 || field.DataType.Contains("Number", StringComparison.OrdinalIgnoreCase)
                 || field.DataType.Equals("Int", StringComparison.OrdinalIgnoreCase))
            mode = "zonedDecimal";
        else
            mode = "copyTrim";
        overrides.Add(new
        {
            source = field.Name,
            target = field.Name,
            sourceOffset = field.Offset,
            sourceLength = field.Length,
            mode,
            trimOutput = mode == "copyTrim"
        });
    }
    var root = new { overrides };
    var opts = new System.Text.Json.JsonSerializerOptions { WriteIndented = true };
    File.WriteAllText(outPath, System.Text.Json.JsonSerializer.Serialize(root, opts));
    Console.WriteLine($"Bootstrapped overrides from compiled schema: {outPath}");
    return;
}
else if (args.Length > 0 && args[0] == "run-pipeline")
{
    string? job = null; string? schemaDir = null; bool allJobs = false; bool verbose = false;
    for (int i = 1; i < args.Length; i++)
    {
        if (args[i] == "--job" && i + 1 < args.Length) job = args[++i];
        else if (args[i] == "--schema" && i + 1 < args.Length) schemaDir = args[++i];
        else if (args[i] == "--all-jobs") allJobs = true;
        else if (args[i] == "--verbose") verbose = true;
    }
    if (schemaDir == null)
    {
        Console.Error.WriteLine("Missing required arg: --schema <dir>");
        Environment.Exit(1);
    }
    if (job == null && !allJobs)
    {
        Console.Error.WriteLine("Missing required arg: --job <id> or --all-jobs");
        Environment.Exit(1);
    }
    
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    var compiled = SchemaCompiler.Compile(schemaDir);
    var baseDir = Path.GetFullPath(Path.Combine(schemaDir, "..", "..", "..", ".."));
    var orchestrator = new PipelineOrchestrator(compiled, baseDir, verbose);
    
    if (allJobs)
    {
        var jobs = new[] { "69172", "80147", "80299", "80362" };
        foreach (var j in jobs)
        {
            orchestrator.Run(j);
        }
    }
    else
    {
        orchestrator.Run(job!);
    }
    return;
}
else
{
    Console.WriteLine("cnp build-schema --schema <dir> --out <dir>");
    Console.WriteLine("cnp run-step1 --job <id> --input <file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp extract-text --job <id> --input <4300-file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp ebcdic-to-ascii --job <id> --input <dat-file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp enrich-keys --job <id> --p-file <asc.11.1.p-file> --s-file <asc.11.1.s-file> --out <dir>");
    Console.WriteLine("cnp validate-suspect --job <id> --input <4300-txt-file> --out <dir>");
    Console.WriteLine("cnp merge-txt-new --job <id> --txt-input <4300-txt-file> --binary-input <4300-file> --out <dir>");
    Console.WriteLine("cnp generate-ncpjax --job <id> --ddcontrol <ddcontrol-txt-file> --out <dir>");
    Console.WriteLine("cnp mb2000-convert --job <id> --input <p.keyed-file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp generate-models --compiled-schemas <dir> --out-project <proj-path>");
    Console.WriteLine("cnp init-overrides --template <path> --schema <dir> --out <path>");
    Console.WriteLine("cnp run-pipeline --job <id> --schema <dir> [--all-jobs] [--verbose]");
}


