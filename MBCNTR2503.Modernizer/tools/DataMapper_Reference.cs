using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace MBCNTR2503.DataMapper
{
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
        [JsonPropertyName("ebcdicEncoding")]
        public string EbcdicEncoding { get; set; } = string.Empty;
    }

    public class DataMapper
    {
        private readonly FieldDefinitionsRoot _fieldDefinitions;
        private readonly Encoding _ebcdicEncoding;

        public DataMapper(string fieldDefinitionsPath)
        {
            string jsonContent = File.ReadAllText(fieldDefinitionsPath);
            _fieldDefinitions = JsonSerializer.Deserialize<FieldDefinitionsRoot>(jsonContent)!;
            
            // Set up EBCDIC encoding (IBM037)
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
            _ebcdicEncoding = Encoding.GetEncoding("IBM037");
        }

        public List<Dictionary<string, object>> ProcessDatFile(string datFilePath, string layoutName = "MBLPS", string clientCode = "503")
        {
            var results = new List<Dictionary<string, object>>();
            
            // Use specified layout
            if (!_fieldDefinitions.RecordLayouts.ContainsKey(layoutName))
            {
                throw new Exception($"{layoutName} layout not found in field definitions");
            }

            var layout = _fieldDefinitions.RecordLayouts[layoutName];
            byte[] buffer = new byte[layout.RecordLength];
            
            using (var fileStream = new FileStream(datFilePath, FileMode.Open, FileAccess.Read))
            {
                int recordNumber = 0;
                
                while (fileStream.Read(buffer, 0, buffer.Length) == buffer.Length)
                {
                    recordNumber++;
                    
                    // Convert EBCDIC to ASCII for client code check
                    string clientCodeFromRecord = _ebcdicEncoding.GetString(buffer, 0, 3).Trim();
                    
                    // Only process records for the specified client code
                    if (clientCodeFromRecord == clientCode)
                    {
                        var recordData = ExtractRecord(buffer, layout.Fields, recordNumber);
                        results.Add(recordData);
                    }
                }
            }
            
            return results;
        }

        private Dictionary<string, object> ExtractRecord(byte[] recordBytes, List<FieldDefinition> fields, int recordNumber)
        {
            var record = new Dictionary<string, object>
            {
                ["RecordNumber"] = recordNumber,
                ["RecordLength"] = recordBytes.Length
            };

            foreach (var field in fields)
            {
                try
                {
                    // Ensure we don't read beyond the record
                    if (field.StartPosition + field.Length > recordBytes.Length)
                    {
                        record[field.Name] = $"ERROR: Field extends beyond record boundary";
                        continue;
                    }

                    // Extract the field bytes
                    byte[] fieldBytes = new byte[field.Length];
                    Array.Copy(recordBytes, field.StartPosition, fieldBytes, 0, field.Length);

                    // Debug for payment amount field positioning
                    if (field.Name == "MB_PAYMENT_AMOUNT" || field.StartPosition == 352 || field.Name == "MBW_FEE_AMT")
                    {
                        Console.WriteLine($"DEBUG: {field.Name} at pos {field.StartPosition}, raw bytes: {BitConverter.ToString(fieldBytes)}, length: {field.Length}, decimalPlaces: {field.DecimalPlaces}");
                        
                        // Also try position - 1 in case it's 1-based
                        if (field.StartPosition > 0)
                        {
                            byte[] altBytes = new byte[field.Length];
                            Array.Copy(recordBytes, field.StartPosition - 1, altBytes, 0, field.Length);
                            Console.WriteLine($"DEBUG: Alternative position {field.StartPosition - 1}, raw bytes: {BitConverter.ToString(altBytes)}");
                        }
                    }

                    // Convert based on field type
                    object value = ConvertFieldValue(fieldBytes, field);
                    record[field.Name] = value;
                }
                catch (Exception ex)
                {
                    record[field.Name] = $"ERROR: {ex.Message}";
                }
            }

            return record;
        }

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
                    return 0;

                if (field.DecimalPlaces > 0)
                {
                    if (decimal.TryParse(numericString, out decimal decimalValue))
                    {
                        return decimalValue / (decimal)Math.Pow(10, field.DecimalPlaces);
                    }
                }
                else
                {
                    if (long.TryParse(numericString, out long longValue))
                    {
                        return longValue;
                    }
                }
                
                return numericString; // Return as string if parsing fails
            }
            catch
            {
                return $"[RAW: {Convert.ToHexString(fieldBytes)}]";
            }
        }

        private string ConvertStringField(byte[] fieldBytes, FieldDefinition field)
        {
            try
            {
                // Check if all bytes are EBCDIC spaces (0x40) - return empty string
                bool allSpaces = fieldBytes.All(b => b == 0x40);
                if (allSpaces)
                {
                    return "";
                }

                string stringValue = _ebcdicEncoding.GetString(fieldBytes);
                return field.Name.Contains("TRIM") || !field.Name.Contains("PRESERVE") 
                    ? stringValue.Trim() 
                    : stringValue;
            }
            catch
            {
                return $"[RAW: {Convert.ToHexString(fieldBytes)}]";
            }
        }

        private decimal ConvertPackedDecimal(byte[] fieldBytes, FieldDefinition field)
        {
            try
            {
                // Check if field contains only EBCDIC spaces (0x40) - this represents zero/empty for packed decimal
                bool allSpaces = fieldBytes.All(b => b == 0x40);
                if (allSpaces)
                {
                    return 0;
                }

                // Check if field contains only null bytes (0x00) - also represents zero
                bool allNulls = fieldBytes.All(b => b == 0x00);
                if (allNulls)
                {
                    return 0;
                }

                // Smart packed decimal detection - try different lengths to find meaningful data
                return SmartConvertPackedDecimal(fieldBytes, field);

            }
            catch (Exception ex)
            {
                return 0; // Return 0 for any conversion errors
            }
        }

        private decimal SmartConvertPackedDecimal(byte[] fieldBytes, FieldDefinition field)
        {
            // Debug for POST2_PET_PAID_TO_DATE field
            if (field.Name.Contains("POST2_PET_PAID_TO_DATE") || field.StartPosition == 352)
            {
                Console.WriteLine($"DEBUG: SmartConvertPackedDecimal called for {field.Name} at pos {field.StartPosition}, bytes: {Convert.ToHexString(fieldBytes)}");
            }

            // Try different lengths starting from full length down to minimum (3 bytes for meaningful packed decimal)
            for (int len = fieldBytes.Length; len >= 3; len--)
            {
                byte[] subset = new byte[len];
                Array.Copy(fieldBytes, subset, len);

                // Check if this looks like valid packed decimal:
                // - Last nibble should be a valid sign (0xA-0xF)
                // - Should not have consecutive null bytes in the middle
                if (IsValidPackedDecimal(subset))
                {
                    try
                    {
                        decimal value = ConvertBasicPackedDecimal(subset, field.DecimalPlaces);
                        if (value != 0) // Found a non-zero value
                        {
                            // Special debug for POST2_PET_PAID_TO_DATE field
                            if (field.Name.Contains("POST2_PET_PAID_TO_DATE") || field.StartPosition == 352)
                            {
                                Console.WriteLine($"DEBUG: {field.Name} found valid packed decimal in {len} bytes: {Convert.ToHexString(subset)} = {value}");
                            }
                            return value;
                        }
                    }
                    catch
                    {
                        // Continue to try shorter length
                    }
                }
            }

            // If no valid packed decimal found, return 0
            return 0;
        }

        private bool IsValidPackedDecimal(byte[] data)
        {
            if (data == null || data.Length < 1)
                return false;

            // Check last nibble for valid sign (0xA-0xF are valid signs)
            byte lastByte = data[data.Length - 1];
            int signNibble = lastByte & 0x0F;
            bool validSign = signNibble >= 0x0A;

            // Check for reasonable digit nibbles (0-9) in all bytes
            bool hasValidDigits = true;
            for (int i = 0; i < data.Length; i++)
            {
                byte currentByte = data[i];
                int highNibble = (currentByte >> 4) & 0x0F;
                int lowNibble = currentByte & 0x0F;

                if (i == data.Length - 1)
                {
                    // Last byte: only check high nibble as digit, low nibble is sign
                    if (highNibble > 9)
                        hasValidDigits = false;
                }
                else
                {
                    // Other bytes: both nibbles should be digits (0-9)
                    if (highNibble > 9 || lowNibble > 9)
                        hasValidDigits = false;
                }
            }

            return validSign && hasValidDigits;
        }

        private decimal ConvertBasicPackedDecimal(byte[] data, int decimalPlaces)
        {
            if (data == null || data.Length == 0)
                return 0;

            long value = 0;
            bool isNegative = false;

            // Process each byte
            for (int i = 0; i < data.Length; i++)
            {
                byte currentByte = data[i];
                
                if (i == data.Length - 1)
                {
                    // Last byte: high nibble is digit, low nibble is sign
                    int digit = (currentByte >> 4) & 0x0F;
                    value = value * 10 + digit;
                    
                    int sign = currentByte & 0x0F;
                    isNegative = (sign == 0x0D || sign == 0x0B);
                }
                else
                {
                    // Other bytes: both nibbles are digits
                    int highDigit = (currentByte >> 4) & 0x0F;
                    int lowDigit = currentByte & 0x0F;
                    
                    value = value * 10 + highDigit;
                    value = value * 10 + lowDigit;
                }
            }

            decimal result = (decimal)value / (decimal)Math.Pow(10, decimalPlaces);
            return isNegative ? -result : result;
        }

        public void SaveAsJson(List<Dictionary<string, object>> records, string outputPath)
        {
            var options = new JsonSerializerOptions
            {
                WriteIndented = true,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase
            };

            string jsonString = JsonSerializer.Serialize(records, options);
            File.WriteAllText(outputPath, jsonString);
        }

        public void SaveSummary(List<Dictionary<string, object>> records, string summaryPath)
        {
            var summary = new
            {
                ProcessingDate = DateTime.Now,
                TotalRecords = records.Count,
                ClientCode = records.Count > 0 ? records[0].GetValueOrDefault("MB_CLIENT3", "Unknown") : "None",
                SampleRecord = records.FirstOrDefault(),
                FieldSummary = records.Count > 0 
                    ? records[0].Keys.Where(k => !k.StartsWith("Record")).Take(10).ToList()
                    : new List<string>()
            };

            var options = new JsonSerializerOptions { WriteIndented = true };
            string summaryJson = JsonSerializer.Serialize(summary, options);
            File.WriteAllText(summaryPath, summaryJson);
        }

        static void Main(string[] args)
        {
            if (args.Length < 4)
            {
                Console.WriteLine("Usage: DataMapper <input_dat_file> <field_definitions_json> <layout> <client_code> [output_json]");
                Console.WriteLine("Example: DataMapper 69172.dat FieldDefinitions_Generated.json MBLPS 503 output.json");
                return;
            }

            string datFile = args[0];
            string fieldDefsFile = args[1];
            string layout = args[2];
            string clientCode = args[3];
            string outputFile = args.Length > 4 ? args[4] : Path.ChangeExtension(datFile, ".json");
            string summaryFile = Path.ChangeExtension(outputFile, ".summary.json");

            try
            {
                Console.WriteLine($"Processing {datFile} using {layout} layout for client code {clientCode}...");
                
                var mapper = new DataMapper(fieldDefsFile);
                var records = mapper.ProcessDatFile(datFile, layout, clientCode);
                
                Console.WriteLine($"Found {records.Count} records for client {clientCode}");
                
                if (records.Count > 0)
                {
                    mapper.SaveAsJson(records, outputFile);
                    mapper.SaveSummary(records, summaryFile);
                    
                    Console.WriteLine($"Output saved to: {outputFile}");
                    Console.WriteLine($"Summary saved to: {summaryFile}");
                    
                    // Show first record sample
                    Console.WriteLine("\nSample fields from first record:");
                    var firstRecord = records[0];
                    var sampleFields = new[] { "MB_CLIENT3", "MB_ACCOUNT", "MB_FORMATTED_ACCOUNT", "POST2_PET_PAID_TO_DATE" };
                    
                    foreach (var fieldName in sampleFields)
                    {
                        if (firstRecord.ContainsKey(fieldName))
                        {
                            Console.WriteLine($"  {fieldName}: {firstRecord[fieldName]}");
                        }
                    }
                }
                else
                {
                    Console.WriteLine($"No records found for client code {clientCode}");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
                Console.WriteLine($"Stack trace: {ex.StackTrace}");
            }
        }
    }
}