using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// EBCDIC Record Parser - Extracts fields from EBCDIC data using field definitions
    /// Replaces hardcoded synthetic data with real EBCDIC field extraction
    /// Based on proven DataMapper logic
    /// </summary>
    public class EbcdicRecordParser
    {
        private readonly FieldDefinitionsRoot _fieldDefinitions;
        private readonly Encoding _ebcdicEncoding;
        private readonly ILogger _logger;

        public EbcdicRecordParser(string fieldDefinitionsPath, ILogger logger)
        {
            _logger = logger;
            
            // Load field definitions from JSON
            string jsonContent = File.ReadAllText(fieldDefinitionsPath);
            _fieldDefinitions = JsonSerializer.Deserialize<FieldDefinitionsRoot>(jsonContent)!;
            
            // Set up EBCDIC encoding (IBM037)
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
            _ebcdicEncoding = Encoding.GetEncoding("IBM037");
            
            _logger.LogInformation($"📋 Loaded field definitions with {_fieldDefinitions.RecordLayouts.Count} layouts");
        }

        /// <summary>
        /// Parse EBCDIC file and extract records for specified client
        /// </summary>
        public List<EbcdicRecord> ParseEbcdicFile(string datFilePath, string layoutName = "MBLPS", string clientCode = "503")
        {
            var results = new List<EbcdicRecord>();
            
            if (!_fieldDefinitions.RecordLayouts.ContainsKey(layoutName))
            {
                throw new Exception($"{layoutName} layout not found in field definitions");
            }

            var layout = _fieldDefinitions.RecordLayouts[layoutName];
            byte[] buffer = new byte[layout.RecordLength];
            
            _logger.LogInformation($"📖 Processing EBCDIC file: {datFilePath} using layout {layoutName}");
            
            using (var fileStream = new FileStream(datFilePath, FileMode.Open, FileAccess.Read))
            {
                int recordNumber = 0;
                int clientMatchCount = 0;
                
                while (fileStream.Read(buffer, 0, buffer.Length) == buffer.Length)
                {
                    recordNumber++;
                    
                    // Convert EBCDIC to ASCII for client code check
                    string clientCodeFromRecord = _ebcdicEncoding.GetString(buffer, 0, 3).Trim();
                    
                    // Only process records for the specified client code
                    if (clientCodeFromRecord == clientCode)
                    {
                        clientMatchCount++;
                        var record = ExtractRecord(buffer, layout, recordNumber);
                        results.Add(record);
                    }
                }
                
                _logger.LogInformation($"📊 Processed {recordNumber} total records, {clientMatchCount} matching client {clientCode}");
            }
            
            return results;
        }

        /// <summary>
        /// Extract all fields from a single EBCDIC record
        /// </summary>
        private EbcdicRecord ExtractRecord(byte[] recordBytes, RecordLayout layout, int recordNumber)
        {
            var record = new EbcdicRecord
            {
                RecordNumber = recordNumber,
                RecordLength = recordBytes.Length,
                Fields = new Dictionary<string, object>()
            };

            // Debug: Show first 30 bytes of record for analysis
            if (recordNumber <= 3)
            {
                var firstBytes = recordBytes.Take(30).Select(b => $"0x{b:X2}").ToArray();
                _logger.LogInformation($"🔍 Record {recordNumber} first 30 bytes: {string.Join(" ", firstBytes)}");
            }

            foreach (var field in layout.Fields)
            {
                try
                {
                    // JSON field definitions are 1-based in legacy specs; convert to 0-based index for slicing
                    int startIndex = Math.Max(0, field.StartPosition - 1);

                    // Ensure we don't read beyond the record boundaries
                    int maxReadable = Math.Min(field.Length, Math.Max(0, recordBytes.Length - startIndex));
                    // Initialize with EBCDIC spaces (0x40) to avoid NULs in string fields when partial reads occur
                    byte[] fieldBytes = new byte[field.Length];
                    for (int i = 0; i < fieldBytes.Length; i++)
                    {
                        fieldBytes[i] = 0x40; // EBCDIC space
                    }
                    if (maxReadable > 0)
                    {
                        Array.Copy(recordBytes, startIndex, fieldBytes, 0, maxReadable);
                    }
                    
                    object value = ConvertFieldValue(fieldBytes, field);
                    record.Fields[field.Name] = value;

                    // Debug: Log key fields that we're tracking
                    if (field.Name == "MB_ACCOUNT" || field.Name == "MB_PRODUCT_CODE" || 
                        field.Name.Contains("PRODUCT") || field.Name.Contains("ACCOUNT"))
                    {
                        var hexBytes = fieldBytes.Select(b => $"0x{b:X2}").ToArray();
                        _logger.LogInformation($"🔍 Record {recordNumber} Field '{field.Name}' pos:{field.StartPosition} len:{field.Length} type:{field.Type} " +
                                             $"bytes:[{string.Join(" ", hexBytes)}] -> value:'{value}'");
                    }
                }
                catch (Exception ex)
                {
                    record.Fields[field.Name] = $"ERROR: {ex.Message}";
                    _logger.LogWarning($"⚠️ Error extracting field {field.Name}: {ex.Message}");
                }
            }

            return record;
        }

        /// <summary>
        /// Convert field value based on field type
        /// </summary>
        private object ConvertFieldValue(byte[] fieldBytes, FieldDefinition field)
        {
            switch (field.Type)
            {
                case 1: // Numeric
                    return ConvertNumericField(fieldBytes, field);
                    
                case 2: // String/Character
                    return ConvertStringField(fieldBytes, field);
                    
                case 3: // Packed decimal
                    return ConvertPackedDecimal(fieldBytes, field);
                    
                default:
                    // Default to string conversion
                    return ConvertStringField(fieldBytes, field);
            }
        }

        /// <summary>
        /// Convert numeric field from EBCDIC
        /// </summary>
        private object ConvertNumericField(byte[] fieldBytes, FieldDefinition field)
        {
            try
            {
                // Check if this is a packed decimal field based on description OR decimal places
                if ((field.Description != null && field.Description.Contains("Packed", StringComparison.OrdinalIgnoreCase)) ||
                    field.DecimalPlaces > 0)
                {
                    return ConvertPackedDecimal(fieldBytes, field);
                }

                // Check if all bytes are EBCDIC spaces (0x40) or null bytes (0x00) - this should be treated as zero
                bool allSpacesOrNulls = fieldBytes.All(b => b == 0x40 || b == 0x00);
                if (allSpacesOrNulls)
                {
                    return 0;
                }

                string numericString = _ebcdicEncoding.GetString(fieldBytes).Trim();
                
                if (string.IsNullOrEmpty(numericString))
                {
                    return 0;
                }

                // Try to parse as integer first, then decimal
                if (field.DecimalPlaces == 0)
                {
                    if (long.TryParse(numericString, out long longValue))
                        return longValue;
                }
                else
                {
                    if (decimal.TryParse(numericString, out decimal decimalValue))
                        return decimalValue;
                }

                return 0;
            }
            catch (Exception)
            {
                return 0;
            }
        }

        /// <summary>
        /// Convert string field from EBCDIC
        /// </summary>
        private string ConvertStringField(byte[] fieldBytes, FieldDefinition field)
        {
            string result = _ebcdicEncoding.GetString(fieldBytes);
            
            // Trim trailing spaces but preserve leading spaces if configured
            if (field.PreserveLeadingSpaces)
            {
                return result.TrimEnd();
            }
            else
            {
                return result.Trim();
            }
        }

        /// <summary>
        /// Convert packed decimal field (COMP-3)
        /// </summary>
        private decimal ConvertPackedDecimal(byte[] fieldBytes, FieldDefinition field)
        {
            // Use smart packed decimal conversion logic from DataMapper
            return SmartConvertPackedDecimal(fieldBytes, field);
        }

        /// <summary>
        /// Smart packed decimal conversion with multiple length attempts
        /// </summary>
        private decimal SmartConvertPackedDecimal(byte[] fieldBytes, FieldDefinition field)
        {
            // Debug: Special logging for MB_ACCOUNT field
            if (field.Name == "MB_ACCOUNT")
            {
                _logger?.LogInformation($"🔍 SmartConvertPackedDecimal for MB_ACCOUNT:");
                _logger?.LogInformation($"🔍   Field bytes: [{string.Join(" ", fieldBytes.Select(b => $"0x{b:X2}"))}]");
            }

            // Try different strategies to find packed decimal data
            
            // Strategy 1: Try from the end (most common - packed decimal at end of field)
            for (int len = Math.Min(fieldBytes.Length, 8); len >= 2; len--)
            {
                byte[] subset = new byte[len];
                Array.Copy(fieldBytes, fieldBytes.Length - len, subset, 0, len);

                if (field.Name == "MB_ACCOUNT")
                {
                    _logger?.LogInformation($"🔍   Strategy 1 - trying length {len}: [{string.Join(" ", subset.Select(b => $"0x{b:X2}"))}]");
                }

                if (IsValidPackedDecimal(subset))
                {
                    if (field.Name == "MB_ACCOUNT")
                    {
                        _logger?.LogInformation($"🔍   Valid packed decimal found! Converting...");
                    }
                    try
                    {
                        decimal value = ConvertBasicPackedDecimal(subset, field.DecimalPlaces);
                        if (value != 0) // Found a non-zero value
                        {
                            if (field.Name == "MB_ACCOUNT")
                            {
                                _logger?.LogInformation($"🔍   Success! Value: {value}");
                            }
                            return value;
                        }
                    }
                    catch
                    {
                        // Continue to try shorter length
                        if (field.Name == "MB_ACCOUNT")
                        {
                            _logger?.LogInformation($"🔍   Conversion failed, trying shorter length");
                        }
                    }
                }
                else if (field.Name == "MB_ACCOUNT")
                {
                    _logger?.LogInformation($"🔍   Not valid packed decimal");
                }
            }
            
            // Strategy 2: Try from the beginning (alternative layout)
            for (int len = Math.Min(fieldBytes.Length, 8); len >= 2; len--)
            {
                byte[] subset = new byte[len];
                Array.Copy(fieldBytes, subset, len);

                if (IsValidPackedDecimal(subset))
                {
                    try
                    {
                        decimal value = ConvertBasicPackedDecimal(subset, field.DecimalPlaces);
                        if (value != 0) // Found a non-zero value
                        {
                            return value;
                        }
                    }
                    catch
                    {
                        // Continue to try shorter length
                    }
                }
            }

            // Strategy 3: Scan for a trailing packed-decimal SUFFIX within mixed content
            // Rationale: Some fields may be alphanumeric/zoned for a prefix and end with a short COMP-3
            //            e.g., [0x40 0xC1 0xF0 0xF0 0xF1 0x12 0x5F] where last 2 bytes (0x12 0x5F) == 125.
            // We scan from the end to locate the shortest valid suffix (2-8 bytes) that forms a packed number.
            for (int end = fieldBytes.Length - 1; end >= 1; end--)
            {
                // The last nibble of the last byte must be a valid sign nibble (A-F)
                byte last = fieldBytes[end];
                byte signNibble = (byte)(last & 0x0F);
                bool validSign = signNibble >= 0x0A && signNibble <= 0x0F;
                if (!validSign) continue;

                // The high nibble of the last byte must be a digit (0-9)
                byte lastHighNibble = (byte)((last >> 4) & 0x0F);
                if (lastHighNibble > 9) continue;

                // Try suffixes of length 2..8 ending at `end`
                for (int len = 2; len <= 8; len++)
                {
                    int start = end - len + 1;
                    if (start < 0) break;

                    // Validate that all nibbles prior to the sign are digits (0-9)
                    bool digitsOk = true;
                    for (int i = start; i <= end; i++)
                    {
                        byte b = fieldBytes[i];
                        byte hi = (byte)((b >> 4) & 0x0F);
                        byte lo = (byte)(b & 0x0F);

                        if (i == end)
                        {
                            // last byte: hi must be digit, low is sign already checked
                            if (hi > 9) { digitsOk = false; break; }
                        }
                        else
                        {
                            if (hi > 9 || lo > 9) { digitsOk = false; break; }
                        }
                    }

                    if (!digitsOk) continue;

                    // We have a candidate suffix; copy and convert
                    byte[] suffix = new byte[len];
                    Array.Copy(fieldBytes, start, suffix, 0, len);

                    if (field.Name == "MB_ACCOUNT")
                    {
                        _logger?.LogInformation($"🔍   Strategy 3 - suffix candidate [{string.Join(" ", suffix.Select(b => $"0x{b:X2}"))}] start:{start} len:{len}");
                    }

                    try
                    {
                        decimal value = ConvertBasicPackedDecimal(suffix, field.DecimalPlaces);
                        // Accept zero only if all digits are zero; otherwise prefer non-zero findings
                        bool allZeroDigits = suffix.Take(len - 1).All(b => ((b >> 4) & 0x0F) == 0 && (b & 0x0F) == 0) && ((suffix[len - 1] >> 4) & 0x0F) == 0;
                        if (value != 0 || allZeroDigits)
                        {
                            if (field.Name == "MB_ACCOUNT")
                            {
                                _logger?.LogInformation($"🔍   Strategy 3 - success! Value: {value}");
                            }
                            return value;
                        }
                    }
                    catch (Exception ex)
                    {
                        if (field.Name == "MB_ACCOUNT")
                        {
                            _logger?.LogInformation($"🔍   Strategy 3 - conversion error: {ex.Message}");
                        }
                        // Continue trying other candidates
                    }
                }
            }

            // If no valid packed decimal found, return 0
            return 0;
        }

        /// <summary>
        /// Check if byte array contains valid packed decimal data
        /// </summary>
        private bool IsValidPackedDecimal(byte[] data)
        {
            if (data.Length == 0) return false;

            // Last nibble should be a valid sign (0xA-0xF for positive, 0xD for negative, 0xC for positive unsigned)
            byte lastByte = data[data.Length - 1];
            byte lastNibble = (byte)(lastByte & 0x0F);
            bool validSign = lastNibble >= 0x0A && lastNibble <= 0x0F;

            if (!validSign) return false;

            // Check for patterns that indicate invalid packed decimal
            // - Too many consecutive null bytes
            int consecutiveNulls = 0;
            for (int i = 0; i < data.Length - 1; i++) // Don't check last byte (sign)
            {
                if (data[i] == 0x00)
                {
                    consecutiveNulls++;
                    if (consecutiveNulls > 2) return false; // More than 2 consecutive nulls is suspicious
                }
                else
                {
                    consecutiveNulls = 0;
                }
            }

            return true;
        }

        /// <summary>
        /// Convert basic packed decimal to decimal value
        /// Based on proven algorithm from CodeProject EBCDIC converter
        /// </summary>
        private decimal ConvertBasicPackedDecimal(byte[] data, int decimalPlaces)
        {
            if (data.Length == 0) return 0;

            // Debug: Log the raw bytes being processed
            var hexBytes = data.Select(b => $"0x{b:X2}").ToArray();
            _logger?.LogInformation($"🔍 Converting packed decimal: [{string.Join(" ", hexBytes)}] decimalPlaces:{decimalPlaces}");

            // Check for all 0xFF bytes (indicates empty/null field)
            bool allFF = data.All(b => b == 0xFF);
            if (allFF) 
            {
                _logger?.LogInformation($"🔍 All FF bytes detected, returning 0");
                return 0;
            }

            long lo = 0;
            long mid = 0;
            long hi = 0;
            bool isNegative;

            // First nibble (rightmost) stores only the sign, not a digit
            // 0xC hex is positive, 0xD hex is negative, and 0xF hex is unsigned
            byte firstNibble = GetNibble(data, 0);
            _logger?.LogInformation($"🔍 Sign nibble: 0x{firstNibble:X}");
            
            switch (firstNibble)
            {
                case 0x0D:
                    isNegative = true;
                    break;
                case 0x0F:
                case 0x0C:
                    isNegative = false;
                    break;
                default:
                    // Invalid sign, try to parse as regular EBCDIC
                    _logger?.LogInformation($"🔍 Invalid sign nibble 0x{firstNibble:X}, returning 0");
                    return 0;
            }

            // Process digits from right to left (excluding sign nibble)
            for (int j = data.Length * 2 - 1; j > 0; j--)
            {
                // Multiply current value by 10
                long intermediate = lo * 10;
                lo = intermediate & 0xffffffff;
                long carry = intermediate >> 32;

                intermediate = mid * 10 + carry;
                mid = intermediate & 0xffffffff;
                carry = intermediate >> 32;

                intermediate = hi * 10 + carry;
                hi = intermediate & 0xffffffff;

                // Get the digit
                byte digit = GetNibble(data, j);
                if (digit > 9)
                {
                    // Invalid digit, return 0
                    return 0;
                }

                // Add the digit
                intermediate = lo + digit;
                lo = intermediate & 0xffffffff;
                carry = intermediate >> 32;

                if (carry > 0)
                {
                    intermediate = mid + carry;
                    mid = intermediate & 0xffffffff;
                    carry = intermediate >> 32;

                    if (carry > 0)
                    {
                        intermediate = hi + carry;
                        hi = intermediate & 0xffffffff;
                    }
                }
            }

            // Create decimal with proper sign and decimal places
            decimal result = new decimal((int)lo, (int)mid, (int)hi, isNegative, (byte)decimalPlaces);
            
            // Debug: Log the final converted value
            _logger?.LogInformation($"🔍 Packed decimal conversion result: {result} (lo:{lo}, mid:{mid}, hi:{hi}, negative:{isNegative})");
            
            return result;
        }

        /// <summary>
        /// Get nibble from packed decimal byte array
        /// Nibble 0 is the rightmost (sign), nibble 1 is next digit, etc.
        /// </summary>
        private byte GetNibble(byte[] data, int nibbleNo)
        {
            int byteIndex = data.Length - 1 - nibbleNo / 2;
            byte b = data[byteIndex];
            return (byte)((nibbleNo % 2 == 0) ? (b & 0x0F) : (b >> 4));
        }
    }

    /// <summary>
    /// Represents a parsed EBCDIC record with extracted fields
    /// </summary>
    public class EbcdicRecord
    {
        public int RecordNumber { get; set; }
        public int RecordLength { get; set; }
        public Dictionary<string, object> Fields { get; set; } = new();
        
        /// <summary>
        /// Get field value as string
        /// </summary>
        public string GetFieldAsString(string fieldName)
        {
            if (Fields.TryGetValue(fieldName, out object? value))
            {
                return value?.ToString() ?? "";
            }
            return "";
        }
        
        /// <summary>
        /// Get field value as decimal
        /// </summary>
        public decimal GetFieldAsDecimal(string fieldName)
        {
            if (Fields.TryGetValue(fieldName, out object? value))
            {
                if (value is decimal d) return d;
                if (value is int i) return i;
                if (value is long l) return l;
                if (decimal.TryParse(value?.ToString(), out decimal result))
                    return result;
            }
            return 0;
        }
        
        /// <summary>
        /// Get field value as integer
        /// </summary>
        public int GetFieldAsInt(string fieldName)
        {
            if (Fields.TryGetValue(fieldName, out object? value))
            {
                if (value is int i) return i;
                if (value is long l) return (int)l;
                if (int.TryParse(value?.ToString(), out int result))
                    return result;
            }
            return 0;
        }
    }

    // Field definition classes (must match JSON structure)
    public class FieldDefinition
    {
        [JsonPropertyName("name")]
        public string Name { get; set; } = string.Empty;
        
        [JsonPropertyName("startPosition")]
        public int StartPosition { get; set; }
        
        [JsonPropertyName("length")]
        public int Length { get; set; }
        
        [JsonPropertyName("type")]
        public int Type { get; set; }
        
        [JsonPropertyName("preserveLeadingSpaces")]
        public bool PreserveLeadingSpaces { get; set; }
        
        [JsonPropertyName("description")]
        public string Description { get; set; } = string.Empty;
        
        [JsonPropertyName("decimalPlaces")]
        public int DecimalPlaces { get; set; }
        
        [JsonPropertyName("isRequired")]
        public bool IsRequired { get; set; }
    }

    public class RecordLayout
    {
        [JsonPropertyName("description")]
        public string Description { get; set; } = string.Empty;
        
        [JsonPropertyName("recordLength")]
        public int RecordLength { get; set; }
        
        [JsonPropertyName("fields")]
        public List<FieldDefinition> Fields { get; set; } = new();
    }

    public class FieldDefinitionsRoot
    {
        [JsonPropertyName("recordLayouts")]
        public Dictionary<string, RecordLayout> RecordLayouts { get; set; } = new();
        
        [JsonPropertyName("configuration")]
        public Configuration Configuration { get; set; } = new();
    }

    public class Configuration
    {
        public string EbcdicEncoding { get; set; } = "IBM037";
    }
}