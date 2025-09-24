using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Linq;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// Record extraction and splitting functionality
    /// Based on legacy cnpsplit4.c, cnpextractvalues.c, and ncpsplitall.c
    /// </summary>
    public class RecordExtractor
    {
        private readonly EbcdicConverter _ebcdicConverter;
        private readonly ILogger _logger;

        public RecordExtractor(EbcdicConverter ebcdicConverter, ILogger logger)
        {
            _ebcdicConverter = ebcdicConverter;
            _logger = logger;
        }

        /// <summary>
        /// Split records based on selection criteria
        /// Equivalent to cnpsplit4.c functionality
        /// </summary>
        public async Task<SplitResult> SplitRecordsAsync(SplitOptions options)
        {
            _logger.LogInformation($"🔄 Starting record split: {options.InputFile}");
            
            var result = new SplitResult
            {
                InputFile = options.InputFile,
                OutputFile = options.OutputFile,
                MatchedRecords = new List<string>(),
                UnmatchedRecords = new List<string>(),
                ProcessingStats = new Dictionary<string, int>()
            };

            try
            {
                // Load selection list
                var selectionList = await LoadSelectionListAsync(options.SelectionList);
                _logger.LogInformation($"📋 Loaded {selectionList.Count} selection criteria");

                // Process input file
                using var inputStream = new FileStream(options.InputFile, FileMode.Open, FileAccess.Read);
                byte[] buffer = new byte[options.RecordLength];
                int recordNumber = 0;
                int matchedCount = 0;
                int unmatchedCount = 0;

                while (await inputStream.ReadAsync(buffer, 0, buffer.Length) == buffer.Length)
                {
                    recordNumber++;
                    
                    // Extract selection field
                    byte[] selectionFieldBytes = new byte[options.SelectionLength];
                    Array.Copy(buffer, options.SelectionOffset, selectionFieldBytes, 0, options.SelectionLength);
                    
                    string selectionValue = options.SearchType == SearchType.ASCII 
                        ? _ebcdicConverter.ConvertEbcdicToAscii(selectionFieldBytes)
                        : Encoding.UTF8.GetString(selectionFieldBytes);

                    // Check if selection value matches criteria
                    bool isMatch = CheckSelectionMatch(selectionValue, selectionList);
                    
                    // Convert record to output format
                    string recordOutput = options.SearchType == SearchType.ASCII
                        ? _ebcdicConverter.ConvertEbcdicToAscii(buffer)
                        : Convert.ToHexString(buffer);

                    if (isMatch)
                    {
                        result.MatchedRecords.Add(recordOutput);
                        matchedCount++;
                        
                        // Check maximum matches limit
                        if (options.MaxMatches > 0 && matchedCount >= options.MaxMatches)
                            break;
                    }
                    else
                    {
                        result.UnmatchedRecords.Add(recordOutput);
                        unmatchedCount++;
                    }

                    // Check maximum reads limit
                    if (options.MaxReads > 0 && recordNumber >= options.MaxReads)
                        break;

                    if (recordNumber % 1000 == 0)
                    {
                        _logger.LogInformation($"Processed {recordNumber} records...");
                    }
                }

                // Write output files
                await WriteMatchedRecordsAsync(options.OutputFile + ".match", result.MatchedRecords);
                await WriteUnmatchedRecordsAsync(options.OutputFile + ".unmatch", result.UnmatchedRecords);
                await WriteMainOutputAsync(options.OutputFile, result.MatchedRecords);

                result.ProcessingStats["TotalRecords"] = recordNumber;
                result.ProcessingStats["MatchedRecords"] = matchedCount;
                result.ProcessingStats["UnmatchedRecords"] = unmatchedCount;
                result.Success = true;

                _logger.LogInformation($"✅ Split completed: {matchedCount} matched, {unmatchedCount} unmatched");
                return result;
            }
            catch (Exception ex)
            {
                result.Success = false;
                result.ErrorMessage = ex.Message;
                _logger.LogError($"❌ Split failed: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// Extract field values from records
        /// Equivalent to cnpextractvalues.c functionality
        /// </summary>
        public async Task<ExtractionResult> ExtractFieldValuesAsync(ExtractionOptions options)
        {
            _logger.LogInformation($"🔄 Starting field extraction: {options.InputFile}");

            var result = new ExtractionResult
            {
                InputFile = options.InputFile,
                ExtractedRecords = new List<Dictionary<string, object>>(),
                FieldDefinitions = options.FieldDefinitions
            };

            try
            {
                using var inputStream = new FileStream(options.InputFile, FileMode.Open, FileAccess.Read);
                byte[] buffer = new byte[options.RecordLength];
                int recordNumber = 0;

                while (await inputStream.ReadAsync(buffer, 0, buffer.Length) == buffer.Length)
                {
                    recordNumber++;
                    
                    var extractedRecord = new Dictionary<string, object>
                    {
                        ["RecordNumber"] = recordNumber,
                        ["RecordLength"] = buffer.Length
                    };

                    // Extract each defined field
                    foreach (var field in options.FieldDefinitions)
                    {
                        try
                        {
                            var fieldValue = _ebcdicConverter.ExtractAndConvertField(
                                buffer, field.StartPosition, field.Length, 
                                (FieldType)field.Type, field.DecimalPlaces);
                            
                            extractedRecord[field.Name] = fieldValue;
                        }
                        catch (Exception ex)
                        {
                            extractedRecord[field.Name] = $"ERROR: {ex.Message}";
                            _logger.LogWarning($"Field extraction error for {field.Name}: {ex.Message}");
                        }
                    }

                    result.ExtractedRecords.Add(extractedRecord);

                    if (recordNumber % 1000 == 0)
                    {
                        _logger.LogInformation($"Extracted {recordNumber} records...");
                    }
                }

                result.Success = true;
                result.TotalRecords = recordNumber;

                _logger.LogInformation($"✅ Field extraction completed: {recordNumber} records processed");
                return result;
            }
            catch (Exception ex)
            {
                result.Success = false;
                result.ErrorMessage = ex.Message;
                _logger.LogError($"❌ Field extraction failed: {ex.Message}");
                throw;
            }
        }

        /// <summary>
        /// Split all records in multiple files
        /// Equivalent to ncpsplitall.c functionality
        /// </summary>
        public async Task<MultiSplitResult> SplitAllAsync(MultiSplitOptions options)
        {
            _logger.LogInformation($"🔄 Starting multi-file split operation");

            var result = new MultiSplitResult
            {
                ProcessedFiles = new List<string>(),
                Results = new List<SplitResult>(),
                TotalProcessed = 0
            };

            try
            {
                // Get all input files matching pattern
                var inputFiles = GetInputFiles(options.InputPattern, options.InputDirectory);
                _logger.LogInformation($"📁 Found {inputFiles.Count} files to process");

                foreach (var inputFile in inputFiles)
                {
                    var splitOptions = new SplitOptions
                    {
                        InputFile = inputFile,
                        OutputFile = GenerateOutputPath(inputFile, options.OutputDirectory),
                        RecordLength = options.RecordLength,
                        SelectionOffset = options.SelectionOffset,
                        SelectionLength = options.SelectionLength,
                        SelectionList = options.SelectionList,
                        MaxReads = options.MaxReads,
                        MaxMatches = options.MaxMatches,
                        SearchType = options.SearchType
                    };

                    var splitResult = await SplitRecordsAsync(splitOptions);
                    result.Results.Add(splitResult);
                    result.ProcessedFiles.Add(inputFile);
                    result.TotalProcessed += splitResult.ProcessingStats.GetValueOrDefault("TotalRecords", 0);
                }

                result.Success = true;
                _logger.LogInformation($"✅ Multi-split completed: {result.ProcessedFiles.Count} files, {result.TotalProcessed} total records");
                return result;
            }
            catch (Exception ex)
            {
                result.Success = false;
                result.ErrorMessage = ex.Message;
                _logger.LogError($"❌ Multi-split failed: {ex.Message}");
                throw;
            }
        }

        #region Helper Methods

        private async Task<List<string>> LoadSelectionListAsync(string selectionList)
        {
            var selections = new List<string>();

            if (File.Exists(selectionList))
            {
                // Load from file
                var lines = await File.ReadAllLinesAsync(selectionList);
                selections.AddRange(lines.Where(line => !string.IsNullOrWhiteSpace(line)));
            }
            else
            {
                // Parse comma-separated values
                var values = selectionList.Split(',', StringSplitOptions.RemoveEmptyEntries);
                foreach (var value in values)
                {
                    // Replace underscores with spaces (legacy convention)
                    var processedValue = value.Trim().Replace('_', ' ');
                    selections.Add(processedValue);
                }
            }

            return selections;
        }

        private bool CheckSelectionMatch(string selectionValue, List<string> selectionList)
        {
            if (string.IsNullOrEmpty(selectionValue))
                return false;

            var trimmedValue = selectionValue.Trim();
            
            return selectionList.Any(criteria => 
                string.Equals(trimmedValue, criteria, StringComparison.OrdinalIgnoreCase) ||
                trimmedValue.Contains(criteria, StringComparison.OrdinalIgnoreCase));
        }

        private async Task WriteMatchedRecordsAsync(string filePath, List<string> records)
        {
            if (records.Count > 0)
            {
                await File.WriteAllLinesAsync(filePath, records);
                _logger.LogInformation($"📝 Written {records.Count} matched records to {filePath}");
            }
        }

        private async Task WriteUnmatchedRecordsAsync(string filePath, List<string> records)
        {
            if (records.Count > 0)
            {
                await File.WriteAllLinesAsync(filePath, records);
                _logger.LogInformation($"📝 Written {records.Count} unmatched records to {filePath}");
            }
        }

        private async Task WriteMainOutputAsync(string filePath, List<string> records)
        {
            await File.WriteAllLinesAsync(filePath, records);
            _logger.LogInformation($"📝 Written {records.Count} records to main output {filePath}");
        }

        private List<string> GetInputFiles(string pattern, string directory)
        {
            var files = new List<string>();
            
            if (Directory.Exists(directory))
            {
                var searchPattern = Path.GetFileName(pattern);
                files.AddRange(Directory.GetFiles(directory, searchPattern));
            }
            else if (File.Exists(pattern))
            {
                files.Add(pattern);
            }

            return files;
        }

        private string GenerateOutputPath(string inputFile, string outputDirectory)
        {
            var fileName = Path.GetFileNameWithoutExtension(inputFile);
            return Path.Combine(outputDirectory, fileName + ".split");
        }

        #endregion
    }

    #region Supporting Classes and Enums

    public class SplitOptions
    {
        public string InputFile { get; set; } = string.Empty;
        public string OutputFile { get; set; } = string.Empty;
        public int RecordLength { get; set; }
        public int SelectionOffset { get; set; }
        public int SelectionLength { get; set; }
        public string SelectionList { get; set; } = string.Empty;
        public int MaxReads { get; set; } = 0; // 0 = unlimited
        public int MaxMatches { get; set; } = 0; // 0 = unlimited
        public SearchType SearchType { get; set; } = SearchType.ASCII;
    }

    public class ExtractionOptions
    {
        public string InputFile { get; set; } = string.Empty;
        public int RecordLength { get; set; }
        public List<FieldDefinition> FieldDefinitions { get; set; } = new();
    }

    public class MultiSplitOptions
    {
        public string InputPattern { get; set; } = string.Empty;
        public string InputDirectory { get; set; } = string.Empty;
        public string OutputDirectory { get; set; } = string.Empty;
        public int RecordLength { get; set; }
        public int SelectionOffset { get; set; }
        public int SelectionLength { get; set; }
        public string SelectionList { get; set; } = string.Empty;
        public int MaxReads { get; set; } = 0;
        public int MaxMatches { get; set; } = 0;
        public SearchType SearchType { get; set; } = SearchType.ASCII;
    }

    public class SplitResult
    {
        public string InputFile { get; set; } = string.Empty;
        public string OutputFile { get; set; } = string.Empty;
        public List<string> MatchedRecords { get; set; } = new();
        public List<string> UnmatchedRecords { get; set; } = new();
        public Dictionary<string, int> ProcessingStats { get; set; } = new();
        public bool Success { get; set; }
        public string ErrorMessage { get; set; } = string.Empty;
    }

    public class ExtractionResult
    {
        public string InputFile { get; set; } = string.Empty;
        public List<Dictionary<string, object>> ExtractedRecords { get; set; } = new();
        public List<FieldDefinition> FieldDefinitions { get; set; } = new();
        public int TotalRecords { get; set; }
        public bool Success { get; set; }
        public string ErrorMessage { get; set; } = string.Empty;
    }

    public class MultiSplitResult
    {
        public List<string> ProcessedFiles { get; set; } = new();
        public List<SplitResult> Results { get; set; } = new();
        public int TotalProcessed { get; set; }
        public bool Success { get; set; }
        public string ErrorMessage { get; set; } = string.Empty;
    }

    public enum SearchType
    {
        Any = 0,
        ASCII = 1
    }

    #endregion
}