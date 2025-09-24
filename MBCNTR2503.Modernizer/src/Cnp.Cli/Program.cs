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
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    var compiled = SchemaCompiler.Compile(schemaDir);
    var step1 = new Step1Orchestrator(compiled);
    step1.Run(job, input, outDir);
    return;
}
else
{
    Console.WriteLine("cnp build-schema --schema <dir> --out <dir>");
    Console.WriteLine("cnp run-step1 --job <id> --input <file> --out <dir> --schema <dir>");
}


