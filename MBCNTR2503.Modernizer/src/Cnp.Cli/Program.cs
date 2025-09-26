using Cnp.Schema;
using Cnp.Pipeline;
using System.Text;

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
    var compiled = SchemaCompiler.Compile(schemaDir);
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
else
{
    Console.WriteLine("cnp build-schema --schema <dir> --out <dir>");
    Console.WriteLine("cnp run-step1 --job <id> --input <file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp extract-text --job <id> --input <4300-file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp ebcdic-to-ascii --job <id> --input <dat-file> --out <dir> --schema <dir>");
    Console.WriteLine("cnp validate-suspect --job <id> --input <4300-txt-file> --out <dir>");
    Console.WriteLine("cnp merge-txt-new --job <id> --txt-input <4300-txt-file> --binary-input <4300-file> --out <dir>");
    Console.WriteLine("cnp generate-ncpjax --job <id> --ddcontrol <ddcontrol-txt-file> --out <dir>");
}


