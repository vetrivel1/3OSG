using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using System.Linq;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// Main orchestrator for the MBCNTR2503 modernized pipeline
    /// Replicates the legacy mainframe processing workflow
    /// Based on legacy script: mbcntr2503.script
    /// </summary>
    public class PipelineOrchestrator
    {
        private readonly PipelineConfiguration _config;
        private readonly ILogger _logger;

        public PipelineOrchestrator(PipelineConfiguration config, ILogger logger)
        {
            _config = config;
            _logger = logger;
        }

        /// <summary>
        /// Execute the complete MBCNTR2503 pipeline
        /// Matches the legacy processing workflow step by step
        /// </summary>
        public async Task<PipelineResult> ExecutePipelineAsync(string jobNumber, string sourceFile)
        {
            var result = new PipelineResult
            {
                JobNumber = jobNumber,
                SourceFile = sourceFile,
                StartTime = DateTime.UtcNow,
                Steps = new List<PipelineStep>()
            };

            try
            {
                _logger.LogInformation($"🚀 Starting MBCNTR2503 Pipeline for Job {jobNumber}");
                _logger.LogInformation($"📁 Source File: {sourceFile}");

                // Step 1: Initialize and validate inputs
                await ExecuteStep(result, "Initialize", () => InitializePipeline(jobNumber, sourceFile));

                // Step 2: Copy supplemental table (legacy: cp 2503supptable.txt $job.se1)
                await ExecuteStep(result, "Copy Supplemental Table", () => CopySupplementalTable(jobNumber));

                // Step 3: Container Step 1 (legacy: ncpcntr5v2.script)
                await ExecuteStep(result, "Container Processing", () => ContainerProcessing(jobNumber, sourceFile));

                // Step 4: Convert to option record (legacy: setmb2000.script)
                await ExecuteStep(result, "Convert to Option Records", () => ConvertToOptionRecords(jobNumber));

                // Step 5: E-bill split (legacy: cnpsplit4.out)
                await ExecuteStep(result, "E-bill Split", () => EbillSplit(jobNumber));

                // Step 6: Container processing for split files (legacy: ncpcntr0v2.script)
                await ExecuteStep(result, "Process Split Files", () => ProcessSplitFiles(jobNumber));

                // Step 7: Add Product Codes (legacy: tagit10.script)
                await ExecuteStep(result, "Add Product Codes", () => AddProductCodes(jobNumber));

                // Step 8: First Pass GMC (legacy: mbcntrfirstpassgmc.bat)
                await ExecuteStep(result, "First Pass GMC", () => FirstPassGMC(jobNumber));

                // Step 9: Generate samples and totals (legacy: ncpcntrsample0.out)
                await ExecuteStep(result, "Generate Samples and Totals", () => GenerateSamplesAndTotals(jobNumber));

                // Step 10: Create consolidated outputs (legacy: cat *.cntr.grp)
                await ExecuteStep(result, "Create Consolidated Outputs", () => CreateConsolidatedOutputs(jobNumber));

                // Step 11: Mail tracking container (legacy: cntrimbremit.script)
                await ExecuteStep(result, "Mail Tracking Container", () => MailTrackingContainer(jobNumber));

                result.EndTime = DateTime.UtcNow;
                result.Success = true;
                result.Duration = result.EndTime - result.StartTime;

                _logger.LogInformation($"✅ Pipeline completed successfully in {result.Duration.TotalSeconds:F2} seconds");
                return result;
            }
            catch (Exception ex)
            {
                result.EndTime = DateTime.UtcNow;
                result.Success = false;
                result.ErrorMessage = ex.Message;
                result.Duration = result.EndTime - result.StartTime;

                _logger.LogError($"❌ Pipeline failed: {ex.Message}");
                throw;
            }
        }

        private async Task ExecuteStep(PipelineResult result, string stepName, Func<Task> stepAction)
        {
            var step = new PipelineStep
            {
                Name = stepName,
                StartTime = DateTime.UtcNow
            };

            try
            {
                _logger.LogInformation($"🔄 Executing: {stepName}");
                await stepAction();
                
                step.EndTime = DateTime.UtcNow;
                step.Success = true;
                step.Duration = step.EndTime - step.StartTime;
                
                _logger.LogInformation($"✅ Completed: {stepName} ({step.Duration.TotalSeconds:F2}s)");
            }
            catch (Exception ex)
            {
                step.EndTime = DateTime.UtcNow;
                step.Success = false;
                step.ErrorMessage = ex.Message;
                step.Duration = step.EndTime - step.StartTime;
                
                _logger.LogError($"❌ Failed: {stepName} - {ex.Message}");
                throw;
            }
            finally
            {
                result.Steps.Add(step);
            }
        }

        #region Pipeline Steps

        private async Task InitializePipeline(string jobNumber, string sourceFile)
        {
            // Validate job number
            if (!IsValidJobNumber(jobNumber))
                throw new ArgumentException($"Invalid job number: {jobNumber}");

            // Validate source file exists
            var inputPath = Path.Combine(_config.PublicPath, $"{jobNumber}.dat");
            if (!File.Exists(inputPath))
                throw new FileNotFoundException($"Source file not found: {inputPath}");

            // Create working directories
            Directory.CreateDirectory(_config.PublicPath);
            Directory.CreateDirectory(_config.OutputPath);

            _logger.LogInformation($"📋 Pipeline Configuration:");
            _logger.LogInformation($"   Client: {_config.Client}");
            _logger.LogInformation($"   Work2Length: {_config.Work2Length}");
            _logger.LogInformation($"   Project: {_config.Project}");
            _logger.LogInformation($"   Input Path: {inputPath}");
        }

        private async Task CopySupplementalTable(string jobNumber)
        {
            // Legacy: cp /users/programs/2503supptable.txt /users/public/$job.se1
            var sourceTable = Path.Combine(_config.ProgramsPath, "2503supptable.txt");
            var destinationTable = Path.Combine(_config.PublicPath, $"{jobNumber}.se1");

            if (!File.Exists(sourceTable))
                throw new FileNotFoundException($"Supplemental table not found: {sourceTable}");

            File.Copy(sourceTable, destinationTable, overwrite: true);
            _logger.LogInformation($"📋 Copied supplemental table: {destinationTable}");
            await Task.CompletedTask;
        }

        private async Task ContainerProcessing(string jobNumber, string sourceFile)
        {
            // Legacy: ncpcntr5v2.script j-$job $InPath c-$Client 2-$Work2Len r-$Project e-$ProjectBase
            var containerProcessor = new ContainerProcessor(_config, _logger);
            var inputPath = Path.Combine(_config.PublicPath, $"{jobNumber}.dat");
            
            await containerProcessor.ProcessAsync(jobNumber, inputPath, _config.Client, 
                                                 _config.Work2Length, _config.Project, _config.ProjectBase);
        }

        private async Task ConvertToOptionRecords(string jobNumber)
        {
            // Legacy: /users/scripts/setmb2000.script 0503 $job $job.dat
            var optionConverter = new OptionRecordConverter(_config, _logger);
            await optionConverter.ConvertAsync("0503", jobNumber, $"{jobNumber}.dat");
        }

        private async Task EbillSplit(string jobNumber)
        {
            // Legacy: /users/programs/cnpsplit4.out 2000 1318 1 E 0 0 /users/public/$job'e.txt' ASCII /users/public/$job'p.asc'
            var splitter = new EbillSplitter(_config, _logger);
            
            var inputFile = Path.Combine(_config.PublicPath, $"{jobNumber}e.txt");
            var outputFile = Path.Combine(_config.PublicPath, $"{jobNumber}p.asc");
            
            await splitter.SplitAsync(2000, 1318, 1, "E", 0, 0, inputFile, "ASCII", outputFile);
            
            // Legacy file moves:
            // mv /users/public/$job'p.asc' /users/public/$job'p.asc.org'
            // mv /users/public/$job'p.asc.match' /users/public/$job'e.asc'
            // mv /users/public/$job'p.asc.unmatch' /users/public/$job'p.asc'
            
            var fileManager = new FileManager(_config.PublicPath);
            await fileManager.MoveFileAsync($"{jobNumber}p.asc", $"{jobNumber}p.asc.org");
            await fileManager.MoveFileAsync($"{jobNumber}p.asc.match", $"{jobNumber}e.asc");
            await fileManager.MoveFileAsync($"{jobNumber}p.asc.unmatch", $"{jobNumber}p.asc");
        }

        private async Task ProcessSplitFiles(string jobNumber)
        {
            // Legacy: find /users/public/$job*.asc -prune -size +0c -exec ncpcntr0v2.script...
            var splitProcessor = new SplitFileProcessor(_config, _logger);
            var ascFiles = Directory.GetFiles(_config.PublicPath, $"{jobNumber}*.asc")
                                  .Where(f => new FileInfo(f).Length > 0)
                                  .ToList();

            foreach (var ascFile in ascFiles)
            {
                await splitProcessor.ProcessAsync(ascFile, _config.OptionLength, _config.Project,
                                                _config.ProjectBase, jobNumber, _config.Client,
                                                _config.PieceAttribute, _config.ClientDept,
                                                _config.ServiceType, _config.ContainerKey,
                                                _config.Work2Length);
            }
        }

        private async Task AddProductCodes(string jobNumber)
        {
            // Legacy: find *$job'p.cntr.grp' -exec /users/moonpie/tagit10.script {} 1000 0170 N \;
            var productCodeAdder = new ProductCodeAdder(_config, _logger);
            
            var pGroupFiles = Directory.GetFiles(_config.PublicPath, $"*{jobNumber}p.cntr.grp");
            foreach (var file in pGroupFiles)
            {
                await productCodeAdder.AddProductCodesAsync(file, 1000, "0170", "N");
            }

            // Legacy: /users/moonpie/tagit10.script $job'e.cntr.grp' 1041 0170 N
            var eGroupFile = Path.Combine(_config.PublicPath, $"{jobNumber}e.cntr.grp");
            if (File.Exists(eGroupFile))
            {
                await productCodeAdder.AddProductCodesAsync(eGroupFile, 1041, "0170", "N");
            }
        }

        private async Task FirstPassGMC(string jobNumber)
        {
            // Legacy: find *$job*.cntr.grp -exec ssh 207.98.205.66 'cmd /c d:\scripts\mbcntrfirstpassgmc.bat' {} $WFDProgram \;
            var gmcProcessor = new GMCProcessor(_config, _logger);
            var wfdProgram = "gjs_002503_0001_lpsbill.wfd";
            
            var groupFiles = Directory.GetFiles(_config.PublicPath, $"*{jobNumber}*.cntr.grp");
            foreach (var file in groupFiles)
            {
                await gmcProcessor.FirstPassAsync(file, wfdProgram);
            }

            // Clean up intermediate files and generate count
            var autoCountProcessor = new AutoCountProcessor(_config, _logger);
            await autoCountProcessor.ProcessCountsAsync(jobNumber);
        }

        private async Task GenerateSamplesAndTotals(string jobNumber)
        {
            // Legacy: /users/programs/ncpcntrsample0.out processing
            var sampleProcessor = new SampleProcessor(_config, _logger);
            
            // Process a$job'p.cntr.grp' if exists, otherwise $job'p.cntr.grp'
            var aPFile = Path.Combine(_config.PublicPath, $"a{jobNumber}p.cntr.grp");
            var pFile = Path.Combine(_config.PublicPath, $"{jobNumber}p.cntr.grp");
            
            if (File.Exists(aPFile))
            {
                await sampleProcessor.ProcessSampleAsync(aPFile, "P0001", 97, 1);
                await FileManager.MoveFileStaticAsync(
                    Path.Combine(_config.PublicPath, $"a{jobNumber}p.cntr.grp.total"),
                    Path.Combine(_config.PublicPath, $"{jobNumber}p.cntr.grp.total"));
                await FileManager.MoveFileStaticAsync(
                    Path.Combine(_config.PublicPath, $"a{jobNumber}p.cntr.grp.sample"),
                    Path.Combine(_config.PublicPath, $"{jobNumber}p.cntr.grp.sample"));
            }
            else if (File.Exists(pFile))
            {
                await sampleProcessor.ProcessSampleAsync(pFile, "P0001", 97, 1);
            }

            // Process e.cntr.grp
            var eFile = Path.Combine(_config.PublicPath, $"{jobNumber}e.cntr.grp");
            if (File.Exists(eFile))
            {
                await sampleProcessor.ProcessSampleAsync(eFile, "P0001", 97, 1);
            }

            // Generate auto labels
            var labelProcessor = new AutoLabelProcessor(_config, _logger);
            await labelProcessor.GenerateLabelsAsync("MBILLCNTR", "2503", "test", jobNumber);
            
            // Generate 4300 output files (work2 files)
            var outputGenerator = new OutputFileGenerator(_config, _logger);
            await outputGenerator.GenerateWork2OutputFiles(jobNumber);
        }

        private async Task CreateConsolidatedOutputs(string jobNumber)
        {
            // Legacy: cat /users/public/*$job*.cntr.grp > /users/public/$job.all.grp
            var consolidator = new OutputConsolidator(_config, _logger);
            await consolidator.ConsolidateGroupFilesAsync(jobNumber);
        }

        private async Task MailTrackingContainer(string jobNumber)
        {
            // Legacy: cntrimbremit.script and cnpimbremit1.script
            var mailTracker = new MailTrackingProcessor(_config, _logger);
            
            var pGroupFile = Path.Combine(_config.PublicPath, $"{jobNumber}p.cntr.grp");
            await mailTracker.ProcessRemitAsync(jobNumber, pGroupFile, 1, "000546", "050", "1000", "2503");
            await mailTracker.ProcessImbRemitAsync(jobNumber, "2503", _config.PublicPath);
        }

        #endregion

        #region Helper Methods

        private bool IsValidJobNumber(string jobNumber)
        {
            return !string.IsNullOrEmpty(jobNumber) && 
                   jobNumber.All(char.IsDigit) && 
                   jobNumber.Length >= 4 && 
                   jobNumber.Length <= 8;
        }

        #endregion
    }

    #region Supporting Classes

    public class PipelineConfiguration
    {
        public string Client { get; set; } = "2503";
        public int Work2Length { get; set; } = 4300;
        public string Project { get; set; } = "mblps";
        public string ProjectBase { get; set; } = "/users/programs/container";
        public int ContainerKey { get; set; } = 1941;
        public int OptionLength { get; set; } = 2000;
        public string ClientDept { get; set; } = "250301";
        public string ServiceType { get; set; } = "320";
        public string PieceAttribute { get; set; } = "ncp1stclass1ozimbnew";
        
        public string PublicPath { get; set; } = @"C:\Temp\Pipeline\Public";
        public string ProgramsPath { get; set; } = @"C:\Temp\Pipeline\Programs";
        public string OutputPath { get; set; } = @"C:\Temp\Pipeline\Output";
    }

    public class PipelineResult
    {
        public string JobNumber { get; set; } = string.Empty;
        public string SourceFile { get; set; } = string.Empty;
        public DateTime StartTime { get; set; }
        public DateTime EndTime { get; set; }
        public TimeSpan Duration { get; set; }
        public bool Success { get; set; }
        public string ErrorMessage { get; set; } = string.Empty;
        public List<PipelineStep> Steps { get; set; } = new List<PipelineStep>();
    }

    public class PipelineStep
    {
        public string Name { get; set; } = string.Empty;
        public DateTime StartTime { get; set; }
        public DateTime EndTime { get; set; }
        public TimeSpan Duration { get; set; }
        public bool Success { get; set; }
        public string ErrorMessage { get; set; } = string.Empty;
    }

    public interface ILogger
    {
        void LogInformation(string message);
        void LogError(string message);
        void LogWarning(string message);
    }

    #endregion
}