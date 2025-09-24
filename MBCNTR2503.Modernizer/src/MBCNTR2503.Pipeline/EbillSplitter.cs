using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// E-bill splitting functionality
    /// Implements the legacy cnpsplit4.out behavior for the pipeline
    /// </summary>
    public class EbillSplitter
    {
        private readonly PipelineConfiguration _config;
        private readonly ILogger _logger;
        private readonly RecordExtractor _recordExtractor;
        private readonly EbcdicConverter _ebcdicConverter;

        public EbillSplitter(PipelineConfiguration config, ILogger logger)
        {
            _config = config;
            _logger = logger;
            _ebcdicConverter = new EbcdicConverter(logger);
            _recordExtractor = new RecordExtractor(_ebcdicConverter, logger);
        }

        /// <summary>
        /// Split e-bill records based on legacy cnpsplit4.out parameters
        /// Legacy: /users/programs/cnpsplit4.out 2000 1318 1 E 0 0 /users/public/$job'e.txt' ASCII /users/public/$job'p.asc'
        /// </summary>
        public async Task SplitAsync(int recordLength, int offset, int length, string selectionValue, 
                                   int maxReads, int maxMatches, string inputFile, string searchType, string outputFile)
        {
            _logger.LogInformation($"🔄 Starting E-bill split operation");
            _logger.LogInformation($"   Record Length: {recordLength}");
            _logger.LogInformation($"   Selection Offset: {offset}");
            _logger.LogInformation($"   Selection Length: {length}");
            _logger.LogInformation($"   Selection Value: {selectionValue}");
            _logger.LogInformation($"   Input: {inputFile}");
            _logger.LogInformation($"   Output: {outputFile}");

            var splitOptions = new SplitOptions
            {
                InputFile = inputFile,
                OutputFile = outputFile,
                RecordLength = recordLength,
                SelectionOffset = offset,
                SelectionLength = length,
                SelectionList = selectionValue,
                MaxReads = maxReads,
                MaxMatches = maxMatches,
                SearchType = searchType.Equals("ASCII", StringComparison.OrdinalIgnoreCase) 
                    ? SearchType.ASCII : SearchType.Any
            };

            var result = await _recordExtractor.SplitRecordsAsync(splitOptions);

            if (result.Success)
            {
                _logger.LogInformation($"✅ E-bill split completed successfully");
                _logger.LogInformation($"   Matched records: {result.ProcessingStats.GetValueOrDefault("MatchedRecords", 0)}");
                _logger.LogInformation($"   Unmatched records: {result.ProcessingStats.GetValueOrDefault("UnmatchedRecords", 0)}");
                _logger.LogInformation($"   Total processed: {result.ProcessingStats.GetValueOrDefault("TotalRecords", 0)}");
            }
            else
            {
                throw new Exception($"E-bill split failed: {result.ErrorMessage}");
            }
        }
    }

    /// <summary>
    /// File management utilities for the pipeline
    /// </summary>
    public class FileManager
    {
        private readonly string _basePath;

        public FileManager(string basePath)
        {
            _basePath = basePath;
        }

        public async Task MoveFileAsync(string sourceFileName, string destinationFileName)
        {
            var sourcePath = Path.Combine(_basePath, sourceFileName);
            var destinationPath = Path.Combine(_basePath, destinationFileName);

            if (File.Exists(sourcePath))
            {
                if (File.Exists(destinationPath))
                {
                    File.Delete(destinationPath);
                }
                
                File.Move(sourcePath, destinationPath);
                await Task.CompletedTask; // Make it async for consistency
            }
        }

        public static async Task MoveFileStaticAsync(string sourcePath, string destinationPath)
        {
            if (File.Exists(sourcePath))
            {
                if (File.Exists(destinationPath))
                {
                    File.Delete(destinationPath);
                }
                
                File.Move(sourcePath, destinationPath);
                await Task.CompletedTask; // Make it async for consistency
            }
        }
    }
}