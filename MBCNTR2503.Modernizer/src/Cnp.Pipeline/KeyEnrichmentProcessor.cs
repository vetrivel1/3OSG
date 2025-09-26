using System;
using System.IO;
using System.Text;

namespace Cnp.Pipeline
{
    /// <summary>
    /// Key Enrichment Processor - Replicates legacy cnpfilekeys.c algorithm
    /// 
    /// Matches P records with S records based on account numbers and generates
    /// keys indicating which S records are associated with each P record.
    /// 
    /// Based on: /Legacy Application/Scripts/MBCNTR2503/Original Programs+Scripts/cnpfilekeys.c
    /// Usage: /users/programs/cnpfilekeys.out 1500 1500 4 4 7 1080 7 3 $OutPath.11.1.p $OutPath.11.1.s
    /// </summary>
    public class KeyEnrichmentProcessor
    {
        private readonly int _r1Length;           // Record 1 Length (P records)
        private readonly int _r2Length;           // Record 2 Length (S records)  
        private readonly int _a1Offset;           // Account offset in P records
        private readonly int _a2Offset;           // Account offset in S records
        private readonly int _accountLength;      // Account number field length
        private readonly int _r2KeyOffset;        // Key write position in P records
        private readonly int _r2KeyLength;        // Key number portion length
        private readonly int _r2KeyCountLength;   // Count number portion length
        private readonly bool _keyFirst;          // Key before Count format

        /// <summary>
        /// Initialize Key Enrichment Processor with parameters matching legacy cnpfilekeys.c
        /// </summary>
        /// <param name="r1Length">Record 1 Length (P records) - typically 1500</param>
        /// <param name="r2Length">Record 2 Length (S records) - typically 1500</param>
        /// <param name="a1Offset">Account offset in P records - typically 4</param>
        /// <param name="a2Offset">Account offset in S records - typically 4</param>
        /// <param name="accountLength">Account number field length - typically 7</param>
        /// <param name="r2KeyOffset">Key write position - typically 1080</param>
        /// <param name="r2KeyLength">Key number length - typically 7</param>
        /// <param name="r2KeyCountLength">Count number length - typically 3</param>
        /// <param name="keyFirst">Key before Count format - default true</param>
        public KeyEnrichmentProcessor(
            int r1Length = 1500,
            int r2Length = 1500, 
            int a1Offset = 4,
            int a2Offset = 4,
            int accountLength = 7,
            int r2KeyOffset = 1080,
            int r2KeyLength = 7,
            int r2KeyCountLength = 3,
            bool keyFirst = true)
        {
            _r1Length = r1Length;
            _r2Length = r2Length;
            _a1Offset = a1Offset;
            _a2Offset = a2Offset;
            _accountLength = accountLength;
            _r2KeyOffset = r2KeyOffset;
            _r2KeyLength = r2KeyLength;
            _r2KeyCountLength = r2KeyCountLength;
            _keyFirst = keyFirst;

            // Validation (replicating cnpfilekeys.c validation logic)
            if (_r1Length <= 0)
                throw new ArgumentException($"Record1Length ({_r1Length}) must be > 0");
            if (_r2Length <= 0)
                throw new ArgumentException($"Record2Length ({_r2Length}) must be > 0");
            if (_a1Offset < 0)
                throw new ArgumentException($"Account1 offset ({_a1Offset}) must be >= 0");
            if (_a2Offset < 0)
                throw new ArgumentException($"Account2 offset ({_a2Offset}) must be >= 0");
            if (_accountLength < 1)
                throw new ArgumentException($"Account length ({_accountLength}) must be > 0");
            if (_a1Offset + _accountLength > _r1Length)
                throw new ArgumentException($"Account offset+length ({_a1Offset}+{_accountLength}) must be <= Record length ({_r1Length})");
            if (_a2Offset + _accountLength > _r2Length)
                throw new ArgumentException($"Account offset+length ({_a2Offset}+{_accountLength}) must be <= Record length ({_r2Length})");
            if (_r2KeyOffset + _r2KeyLength + _r2KeyCountLength > _r1Length)
                throw new ArgumentException($"Key offset+length ({_r2KeyOffset}+{_r2KeyLength}+{_r2KeyCountLength}) must be <= Record length ({_r1Length})");
        }

        /// <summary>
        /// Process P and S files to generate keyed output - replicates cnpfilekeys.c main algorithm
        /// </summary>
        /// <param name="pFilePath">Input P file path (.asc.11.1.p)</param>
        /// <param name="sFilePath">Input S file path (.asc.11.1.s)</param>
        /// <param name="outputPath">Output keyed file path (.asc.11.1.p.keyed)</param>
        public void ProcessFiles(string pFilePath, string sFilePath, string outputPath)
        {
            Console.WriteLine("\n\n******* B E G I N   C N F I L E K E Y S *******\n");

            // Algorithm variables (matching cnpfilekeys.c lines 22-27)
            int r1, r2, wCount, r2Count, r2Key, r2KeyCount;
            byte[] r1Buffer = new byte[_r1Length];
            byte[] r2Buffer = new byte[_r2Length];
            byte[] scratch = new byte[8192];

            // Initialize counters (matching cnpfilekeys.c lines 100-102)
            wCount = r2Count = 0;
            r2Key = -1;
            r2KeyCount = 0;

            // Calculate total records for progress tracking
            var fileInfo = new FileInfo(pFilePath);
            int rTotal = (int)(fileInfo.Length / _r1Length);

            using var pFileStream = new FileStream(pFilePath, FileMode.Open, FileAccess.Read);
            using var sFileStream = new FileStream(sFilePath, FileMode.Open, FileAccess.Read);
            using var outputStream = new FileStream(outputPath, FileMode.Create, FileAccess.Write);

            // Initial read (matching cnpfilekeys.c lines 111-114)
            r1 = pFileStream.Read(r1Buffer, 0, _r1Length);
            r2 = sFileStream.Read(r2Buffer, 0, _r2Length);
            if (r2 > 0)
                r2Count++;

            int pctDone = 0, pctDoneSave = 0;

            // Main processing loop (matching cnpfilekeys.c lines 115-177)
            while (r1 > 0 && r2 >= 0)
            {
                if (r2 == 0)
                {
                    // No more S records - write P record with current key (lines 117-132)
                    WriteKeyToRecord(r1Buffer, r2Key, r2KeyCount, scratch);
                    outputStream.Write(r1Buffer, 0, _r1Length);
                    wCount++;
                    r1 = pFileStream.Read(r1Buffer, 0, _r1Length);
                    r2Key = -1;
                    r2KeyCount = 0;
                }
                else
                {
                    // Compare account numbers (line 134)
                    int ret = CompareAccounts(r1Buffer, r2Buffer);

                    if (ret > 0)
                    {
                        // P account > S account - advance S record (lines 135-139)
                        r2 = sFileStream.Read(r2Buffer, 0, _r2Length);
                        if (r2 > 0)
                            r2Count++;
                    }
                    else if (ret < 0)
                    {
                        // P account < S account - write P record (lines 141-156)
                        WriteKeyToRecord(r1Buffer, r2Key, r2KeyCount, scratch);
                        outputStream.Write(r1Buffer, 0, _r1Length);
                        wCount++;
                        r1 = pFileStream.Read(r1Buffer, 0, _r1Length);
                        r2Key = -1;
                        r2KeyCount = 0;
                    }
                    else
                    {
                        // P account = S account - match found (lines 157-164)
                        if (r2Key == -1)
                            r2Key = r2Count;  // First match - capture S record number
                        r2KeyCount++;         // Increment match counter
                        r2 = sFileStream.Read(r2Buffer, 0, _r2Length);
                        if (r2 > 0)
                            r2Count++;
                    }
                }

                // Progress tracking (lines 172-176)
                pctDone = (int)(100.0 * (double)wCount / (double)rTotal);
                if (pctDone != pctDoneSave)
                {
                    Console.WriteLine($"{pctDone} % complete");
                    pctDoneSave = pctDone;
                }
            }

            Console.WriteLine($"\nKey enrichment completed. Processed {wCount} P records.");
        }

        /// <summary>
        /// Compare account numbers between P and S records - replicates memcmp logic
        /// </summary>
        /// <param name="r1Buffer">P record buffer</param>
        /// <param name="r2Buffer">S record buffer</param>
        /// <returns>-1 if P < S, 0 if equal, 1 if P > S</returns>
        private int CompareAccounts(byte[] r1Buffer, byte[] r2Buffer)
        {
            // Exact replication of: memcmp(r1Buffer+a1Offset, r2Buffer+a2Offset, AccountLength)
            for (int i = 0; i < _accountLength; i++)
            {
                byte b1 = r1Buffer[_a1Offset + i];
                byte b2 = r2Buffer[_a2Offset + i];
                
                if (b1 < b2) return -1;
                if (b1 > b2) return 1;
            }
            return 0;
        }

        /// <summary>
        /// Write key information to P record buffer - replicates sprintf/memcpy logic
        /// </summary>
        /// <param name="r1Buffer">P record buffer to modify</param>
        /// <param name="r2Key">S record sequence number (-1 if no matches)</param>
        /// <param name="r2KeyCount">Count of matching S records</param>
        /// <param name="scratch">Scratch buffer for formatting</param>
        private void WriteKeyToRecord(byte[] r1Buffer, int r2Key, int r2KeyCount, byte[] scratch)
        {
            if (r2Key == -1)
            {
                // No matches found - write all zeros (line 119 & 143)
                // memset(r1Buffer+r2KeyOffset, '0', r2KeyLength+r2KeyCountLength);
                for (int i = 0; i < _r2KeyLength + _r2KeyCountLength; i++)
                {
                    r1Buffer[_r2KeyOffset + i] = (byte)'0';
                }
            }
            else
            {
                // Matches found - format key (lines 121-125 & 145-149)
                string keyString;
                if (_keyFirst)
                {
                    // sprintf(scratch, "%0.*d%0.*d", r2KeyLength, r2Key, r2KeyCountLength, r2KeyCount);
                    keyString = $"{r2Key.ToString().PadLeft(_r2KeyLength, '0')}{r2KeyCount.ToString().PadLeft(_r2KeyCountLength, '0')}";
                }
                else
                {
                    // sprintf(scratch, "%0.*d%0.*d", r2KeyCountLength, r2KeyCount, r2KeyLength, r2Key);
                    keyString = $"{r2KeyCount.ToString().PadLeft(_r2KeyCountLength, '0')}{r2Key.ToString().PadLeft(_r2KeyLength, '0')}";
                }

                // memcpy(r1Buffer+r2KeyOffset, scratch, strlen(scratch));
                byte[] keyBytes = Encoding.ASCII.GetBytes(keyString);
                Array.Copy(keyBytes, 0, r1Buffer, _r2KeyOffset, Math.Min(keyBytes.Length, _r2KeyLength + _r2KeyCountLength));
            }
        }
    }
}
