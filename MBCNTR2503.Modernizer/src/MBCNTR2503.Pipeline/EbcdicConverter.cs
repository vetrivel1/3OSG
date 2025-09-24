using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Linq;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// EBCDIC to ASCII conversion module
    /// Based on proven logic from DataMapper Program.cs
    /// Replicates functionality of legacy ebc2asc.c and asc2ebc.c
    /// </summary>
    public class EbcdicConverter
    {
        private readonly Encoding _ebcdicEncoding;
        private readonly Encoding _asciiEncoding;
        private readonly ILogger _logger;

        public EbcdicConverter(ILogger logger)
        {
            _logger = logger;
            
            // Set up EBCDIC encoding (IBM037) - same as DataMapper
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
            _ebcdicEncoding = Encoding.GetEncoding("IBM037");
            _asciiEncoding = Encoding.ASCII;
        }

        /// <summary>
        /// Convert EBCDIC data to ASCII
        /// Equivalent to legacy ebc2asc.c functionality
        /// </summary>
        public string ConvertEbcdicToAscii(byte[] ebcdicData, bool preservePacked = false)
        {
            if (ebcdicData == null || ebcdicData.Length == 0)
                return string.Empty;

            try
            {
                if (preservePacked)
                {
                    // For packed decimal fields, handle specially
                    return ConvertEbcdicWithPackedPreservation(ebcdicData);
                }
                else
                {
                    // Standard EBCDIC to ASCII conversion
                    return _ebcdicEncoding.GetString(ebcdicData).Trim();
                }
            }
            catch (Exception ex)
            {
                _logger.LogError($"Error converting EBCDIC to ASCII: {ex.Message}");
                return string.Empty;
            }
        }

        /// <summary>
        /// Convert ASCII data to EBCDIC
        /// Equivalent to legacy asc2ebc.c functionality
        /// </summary>
        public byte[] ConvertAsciiToEbcdic(string asciiData, int targetLength = 0)
        {
            if (string.IsNullOrEmpty(asciiData))
                return targetLength > 0 ? new byte[targetLength] : new byte[0];

            try
            {
                var ebcdicBytes = _ebcdicEncoding.GetBytes(asciiData);
                
                if (targetLength > 0)
                {
                    // Pad or truncate to target length
                    var result = new byte[targetLength];
                    if (ebcdicBytes.Length >= targetLength)
                    {
                        Array.Copy(ebcdicBytes, result, targetLength);
                    }
                    else
                    {
                        Array.Copy(ebcdicBytes, result, ebcdicBytes.Length);
                        // Fill remaining with EBCDIC spaces (0x40)
                        for (int i = ebcdicBytes.Length; i < targetLength; i++)
                        {
                            result[i] = 0x40;
                        }
                    }
                    return result;
                }

                return ebcdicBytes;
            }
            catch (Exception ex)
            {
                _logger.LogError($"Error converting ASCII to EBCDIC: {ex.Message}");
                return targetLength > 0 ? new byte[targetLength] : new byte[0];
            }
        }

        /// <summary>
        /// Convert EBCDIC file to ASCII file
        /// Equivalent to running ebc2asc.c on a file
        /// </summary>
        public async Task ConvertFileEbcdicToAsciiAsync(string inputPath, string outputPath, 
                                                       int recordLength = 0, bool preservePackedFields = false)
        {
            _logger.LogInformation($"Converting EBCDIC file to ASCII: {inputPath} -> {outputPath}");

            using var inputStream = new FileStream(inputPath, FileMode.Open, FileAccess.Read);
            using var outputStream = new FileStream(outputPath, FileMode.Create, FileAccess.Write);
            using var writer = new StreamWriter(outputStream, _asciiEncoding);

            if (recordLength > 0)
            {
                // Fixed-length record processing
                await ConvertFixedLengthRecordsAsync(inputStream, writer, recordLength, preservePackedFields);
            }
            else
            {
                // Variable length processing - read entire file
                var allBytes = new byte[inputStream.Length];
                await inputStream.ReadAsync(allBytes, 0, allBytes.Length);
                
                string asciiContent = ConvertEbcdicToAscii(allBytes, preservePackedFields);
                await writer.WriteAsync(asciiContent);
            }

            _logger.LogInformation($"EBCDIC to ASCII conversion completed: {outputPath}");
        }

        /// <summary>
        /// Convert ASCII file to EBCDIC file
        /// Equivalent to running asc2ebc.c on a file
        /// </summary>
        public async Task ConvertFileAsciiToEbcdicAsync(string inputPath, string outputPath, 
                                                       int recordLength = 0)
        {
            _logger.LogInformation($"Converting ASCII file to EBCDIC: {inputPath} -> {outputPath}");

            using var inputStream = new FileStream(inputPath, FileMode.Open, FileAccess.Read);
            using var outputStream = new FileStream(outputPath, FileMode.Create, FileAccess.Write);
            using var reader = new StreamReader(inputStream, _asciiEncoding);

            if (recordLength > 0)
            {
                // Fixed-length record processing
                string line;
                while ((line = await reader.ReadLineAsync()) != null)
                {
                    var ebcdicBytes = ConvertAsciiToEbcdic(line, recordLength);
                    await outputStream.WriteAsync(ebcdicBytes, 0, ebcdicBytes.Length);
                }
            }
            else
            {
                // Variable length processing
                string content = await reader.ReadToEndAsync();
                var ebcdicBytes = ConvertAsciiToEbcdic(content);
                await outputStream.WriteAsync(ebcdicBytes, 0, ebcdicBytes.Length);
            }

            _logger.LogInformation($"ASCII to EBCDIC conversion completed: {outputPath}");
        }

        /// <summary>
        /// Extract and convert field value from EBCDIC record
        /// Based on DataMapper's ConvertFieldValue logic
        /// </summary>
        public object ExtractAndConvertField(byte[] recordBytes, int startPosition, int length, 
                                           FieldType fieldType, int decimalPlaces = 0)
        {
            if (startPosition + length > recordBytes.Length)
                return "ERROR: Field extends beyond record boundary";

            try
            {
                // Extract the field bytes
                byte[] fieldBytes = new byte[length];
                Array.Copy(recordBytes, startPosition, fieldBytes, 0, length);

                // Convert based on field type
                return ConvertFieldValue(fieldBytes, fieldType, decimalPlaces);
            }
            catch (Exception ex)
            {
                return $"ERROR: {ex.Message}";
            }
        }

        /// <summary>
        /// Convert field value based on type
        /// Based on DataMapper's conversion logic
        /// </summary>
        private object ConvertFieldValue(byte[] fieldBytes, FieldType fieldType, int decimalPlaces)
        {
            switch (fieldType)
            {
                case FieldType.Numeric:
                    return ConvertNumericField(fieldBytes, decimalPlaces);
                    
                case FieldType.String:
                    return ConvertStringField(fieldBytes);
                    
                case FieldType.PackedDecimal:
                    return ConvertPackedDecimal(fieldBytes, decimalPlaces);
                    
                default:
                    return ConvertStringField(fieldBytes);
            }
        }

        private object ConvertNumericField(byte[] fieldBytes, int decimalPlaces)
        {
            try
            {
                // Check if this is a packed decimal field based on decimal places
                if (decimalPlaces > 0)
                {
                    return ConvertPackedDecimal(fieldBytes, decimalPlaces);
                }

                // Check if all bytes are EBCDIC spaces (0x40) or null bytes (0x00)
                bool allSpacesOrNulls = fieldBytes.All(b => b == 0x40 || b == 0x00);
                if (allSpacesOrNulls)
                {
                    return 0;
                }

                string numericString = _ebcdicEncoding.GetString(fieldBytes).Trim();
                
                if (string.IsNullOrEmpty(numericString))
                    return 0;

                if (long.TryParse(numericString, out long longValue))
                    return longValue;
                    
                if (decimal.TryParse(numericString, out decimal decimalValue))
                    return decimalValue;
                    
                return numericString; // Return as string if not parseable as number
            }
            catch
            {
                return 0;
            }
        }

        private string ConvertStringField(byte[] fieldBytes)
        {
            return _ebcdicEncoding.GetString(fieldBytes).TrimEnd();
        }

        private decimal ConvertPackedDecimal(byte[] fieldBytes, int decimalPlaces)
        {
            try
            {
                // Check if field contains only EBCDIC spaces (0x40) or nulls (0x00)
                bool allSpaces = fieldBytes.All(b => b == 0x40);
                bool allNulls = fieldBytes.All(b => b == 0x00);
                
                if (allSpaces || allNulls)
                {
                    return 0;
                }

                // Smart packed decimal detection - same as DataMapper
                return SmartConvertPackedDecimal(fieldBytes, decimalPlaces);
            }
            catch
            {
                return 0;
            }
        }

        /// <summary>
        /// Smart packed decimal conversion - from DataMapper logic
        /// Tries different lengths to find meaningful data
        /// </summary>
        private decimal SmartConvertPackedDecimal(byte[] fieldBytes, int decimalPlaces)
        {
            // Try different lengths starting from full length down to minimum (3 bytes)
            for (int len = fieldBytes.Length; len >= 3; len--)
            {
                byte[] subset = new byte[len];
                Array.Copy(fieldBytes, subset, len);

                if (IsValidPackedDecimal(subset))
                {
                    try
                    {
                        decimal value = ConvertBasicPackedDecimal(subset, decimalPlaces);
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

            return 0;
        }

        /// <summary>
        /// Validate packed decimal format - from DataMapper logic
        /// </summary>
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

        /// <summary>
        /// Convert basic packed decimal - from DataMapper logic
        /// </summary>
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

        private async Task ConvertFixedLengthRecordsAsync(FileStream inputStream, StreamWriter writer, 
                                                         int recordLength, bool preservePackedFields)
        {
            byte[] buffer = new byte[recordLength];
            int recordNumber = 0;

            while (await inputStream.ReadAsync(buffer, 0, buffer.Length) == buffer.Length)
            {
                recordNumber++;
                string asciiRecord = ConvertEbcdicToAscii(buffer, preservePackedFields);
                await writer.WriteLineAsync(asciiRecord);

                if (recordNumber % 1000 == 0)
                {
                    _logger.LogInformation($"Processed {recordNumber} records...");
                }
            }

            _logger.LogInformation($"Conversion completed: {recordNumber} records processed");
        }

        private string ConvertEbcdicWithPackedPreservation(byte[] ebcdicData)
        {
            // For data that may contain packed decimal fields, convert carefully
            // This is a simplified approach - in real scenarios, you'd need field definitions
            var result = new StringBuilder();
            
            for (int i = 0; i < ebcdicData.Length; i++)
            {
                byte b = ebcdicData[i];
                
                // Check if this might be part of a packed decimal field
                if (IsLikelyPackedDecimalByte(b))
                {
                    // Preserve as hex representation
                    result.Append($"\\x{b:X2}");
                }
                else
                {
                    // Convert normally
                    char[] chars = _ebcdicEncoding.GetChars(new[] { b });
                    if (chars.Length > 0)
                        result.Append(chars[0]);
                }
            }
            
            return result.ToString();
        }

        private bool IsLikelyPackedDecimalByte(byte b)
        {
            // Simple heuristic: if the nibbles suggest packed decimal format
            int highNibble = (b >> 4) & 0x0F;
            int lowNibble = b & 0x0F;
            
            // Both nibbles are digits (0-9) or low nibble is a sign (A-F)
            return (highNibble <= 9 && lowNibble <= 9) || 
                   (highNibble <= 9 && lowNibble >= 0x0A);
        }
    }

    /// <summary>
    /// Field types for conversion
    /// </summary>
    public enum FieldType
    {
        String = 1,
        Numeric = 2,
        PackedDecimal = 3
    }
}