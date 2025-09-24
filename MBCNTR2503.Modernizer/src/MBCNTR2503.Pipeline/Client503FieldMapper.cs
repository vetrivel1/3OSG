using System;

namespace MBCNTR2503.Pipeline
{
    /// <summary>
    /// Client 503 field mapper implementing legacy COBOL BUILD-0503-FIELDS logic
    /// Based on: IF WS-CLIENT = '0503' PERFORM BUILD-0503-FIELDS
    /// Specific mappings for client code 503
    /// </summary>
    public class Client503FieldMapper : IClientFieldMapper
    {
        public string ClientCode => "0503";

        public Client503FieldMapper()
        {
            // No dependencies needed - works directly with EbcdicRecord objects
        }

        /// <summary>
        /// Map fields for A records using client 503 specific logic
        /// </summary>
        public SampleRecord MapARecord(EbcdicRecord ebcdicRecord)
        {
            var record = new SampleRecord();
            
            // Client code is always "503" for this mapper
            record.ClientCode = "503";
            
            // Seq number from MB_SEQUENCE
            record.SequenceNumber = ebcdicRecord.GetFieldAsString("MB_SEQUENCE");
            
            // Record type is always "A"
            record.RecordType = "A";
            
            // Sub sequence from MB_SUB_SEQUENCE  
            record.SubSequenceNumber = ebcdicRecord.GetFieldAsString("MB_SUB_SEQUENCE");
            
            // Client 503 specific: Branch code comes from MB_PLS_CLT_ID -> MB_FLEXFIELD2
            // This matches COBOL: MOVE MB1100-PLS-CLT-ID TO MB-FLEXFIELD2
            var plsClientId = ebcdicRecord.GetFieldAsString("MB_PLS_CLT_ID");
            record.BranchCode = string.IsNullOrWhiteSpace(plsClientId) ? "0" : plsClientId.Trim();
            
            return record;
        }

        /// <summary>
        /// Map fields for D records using client 503 specific logic  
        /// </summary>
        public SampleRecord MapDRecord(EbcdicRecord ebcdicRecord)
        {
            var record = new SampleRecord();
            
            // Client code is always "503" for this mapper
            record.ClientCode = "503";
            
            // Seq number from MB_SEQUENCE
            record.SequenceNumber = ebcdicRecord.GetFieldAsString("MB_SEQUENCE");
            
            // Record type is always "D"
            record.RecordType = "D";
            
            // Sub sequence from MB_SUB_SEQUENCE
            record.SubSequenceNumber = ebcdicRecord.GetFieldAsString("MB_SUB_SEQUENCE");
            
            // Client 503 specific: Product code mapping 
            // Based on legacy analysis, D records should have ProductCode "301"
            record.ProductCode = "301";
            
            return record;
        }

        /// <summary>
        /// Map fields for P records using client 503 specific logic
        /// </summary>
        public SampleRecord MapPRecord(EbcdicRecord ebcdicRecord)
        {
            var record = new SampleRecord();
            
            // Client code is always "503" for this mapper
            record.ClientCode = "503";
            
            // Seq number from MB_SEQUENCE
            record.SequenceNumber = ebcdicRecord.GetFieldAsString("MB_SEQUENCE");
            
            // Record type is always "P"
            record.RecordType = "P";
            
            // Sub sequence from MB_SUB_SEQUENCE
            record.SubSequenceNumber = ebcdicRecord.GetFieldAsString("MB_SUB_SEQUENCE");
            
            return record;
        }

        /// <summary>
        /// Map fields for S records using client 503 specific logic
        /// </summary>
        public SampleRecord MapSRecord(EbcdicRecord ebcdicRecord)
        {
            var record = new SampleRecord();
            
            // Client code is always "503" for this mapper
            record.ClientCode = "503";
            
            // Seq number from MB_SEQUENCE
            record.SequenceNumber = ebcdicRecord.GetFieldAsString("MB_SEQUENCE");
            
            // Record type is always "S" 
            record.RecordType = "S";
            
            // Sub sequence from MB_SUB_SEQUENCE
            record.SubSequenceNumber = ebcdicRecord.GetFieldAsString("MB_SUB_SEQUENCE");
            
            return record;
        }

        /// <summary>
        /// Map fields for V records using client 503 specific logic
        /// </summary>
        public SampleRecord MapVRecord(EbcdicRecord ebcdicRecord)
        {
            var record = new SampleRecord();
            
            // Client code is always "503" for this mapper
            record.ClientCode = "503";
            
            // Seq number from MB_SEQUENCE
            record.SequenceNumber = ebcdicRecord.GetFieldAsString("MB_SEQUENCE");
            
            // Record type is always "V"
            record.RecordType = "V";
            
            // Sub sequence from MB_SUB_SEQUENCE
            record.SubSequenceNumber = ebcdicRecord.GetFieldAsString("MB_SUB_SEQUENCE");
            
            return record;
        }

        /// <summary>
        /// Map fields for F records using client 503 specific logic
        /// </summary>
        public SampleRecord MapFRecord(EbcdicRecord ebcdicRecord)
        {
            var record = new SampleRecord();
            
            // Client code is always "503" for this mapper
            record.ClientCode = "503";
            
            // Seq number from MB_SEQUENCE
            record.SequenceNumber = ebcdicRecord.GetFieldAsString("MB_SEQUENCE");
            
            // Record type is always "F"
            record.RecordType = "F";
            
            // Sub sequence from MB_SUB_SEQUENCE
            record.SubSequenceNumber = ebcdicRecord.GetFieldAsString("MB_SUB_SEQUENCE");
            
            return record;
        }
    }
}