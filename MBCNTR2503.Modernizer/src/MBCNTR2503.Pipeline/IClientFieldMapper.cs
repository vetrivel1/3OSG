using System;
using System.Collections.Generic;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// Interface for client-specific field mappers
    /// Each client has unique field mapping logic based on legacy COBOL BUILD-XXXX-FIELDS
    /// </summary>
    public interface IClientFieldMapper
    {
        string ClientCode { get; }
        SampleRecord MapARecord(EbcdicRecord ebcdicRecord);
        SampleRecord MapDRecord(EbcdicRecord ebcdicRecord);
        SampleRecord MapPRecord(EbcdicRecord ebcdicRecord);
        SampleRecord MapSRecord(EbcdicRecord ebcdicRecord);
        SampleRecord MapVRecord(EbcdicRecord ebcdicRecord);
        SampleRecord MapFRecord(EbcdicRecord ebcdicRecord);
    }

    /// <summary>
    /// Factory for creating client-specific field mappers
    /// Based on legacy COBOL client conditional processing logic
    /// </summary>
    public class ClientFieldMapperFactory
    {
        private readonly Dictionary<string, IClientFieldMapper> _mappers;

        public ClientFieldMapperFactory()
        {
            _mappers = new Dictionary<string, IClientFieldMapper>();
            
            // Register known client mappers
            RegisterMapper(new Client503FieldMapper());
        }

        private void RegisterMapper(IClientFieldMapper mapper)
        {
            _mappers[mapper.ClientCode] = mapper;
        }

        /// <summary>
        /// Get the appropriate field mapper for a client code
        /// </summary>
        public IClientFieldMapper GetMapper(string clientCode)
        {
            if (string.IsNullOrWhiteSpace(clientCode))
            {
                throw new ArgumentException("Client code cannot be null or empty", nameof(clientCode));
            }

            // Normalize client code - pad to 4 digits with leading zeros
            var normalizedClientCode = clientCode.PadLeft(4, '0');
            
            if (_mappers.TryGetValue(normalizedClientCode, out var mapper))
            {
                return mapper;
            }
            
            // If no specific mapper found, throw exception - no fallback logic
            throw new NotSupportedException($"No field mapper found for client code: {clientCode} (normalized: {normalizedClientCode})");
        }

        /// <summary>
        /// Check if a mapper exists for the given client code
        /// </summary>
        public bool HasMapper(string clientCode)
        {
            if (string.IsNullOrWhiteSpace(clientCode))
            {
                return false;
            }
            
            var normalizedClientCode = clientCode.PadLeft(4, '0');
            return _mappers.ContainsKey(normalizedClientCode);
        }

        /// <summary>
        /// Get all registered client codes
        /// </summary>
        public IEnumerable<string> GetRegisteredClientCodes()
        {
            return _mappers.Keys;
        }
    }
}