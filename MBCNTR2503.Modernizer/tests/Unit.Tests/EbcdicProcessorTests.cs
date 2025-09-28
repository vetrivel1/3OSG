using System;
using System.IO;
using Xunit;
using Cnp.Schema;
using Cnp.Pipeline;

namespace Unit.Tests
{
    public class EbcdicProcessorTests
    {
        [Fact]
        public void ProcessDatToAsc_ConvertsDRecordEBCDICtoASCII()
        {
            // Arrange: create temp schema directory with minimal dd and iomap
            var tempSchema = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
            Directory.CreateDirectory(tempSchema);
            // Create minimal mblps.dd with one field (not used for D record conversion)
            File.WriteAllText(Path.Combine(tempSchema, "mblps.dd"), "DUMMY,0,1,C");
            File.WriteAllText(Path.Combine(tempSchema, "mblps.dd.iomap"), "");
            var compiled = SchemaCompiler.Compile(tempSchema);

            // Create a 4000-byte input DAT file: EBCDIC 'D' at offset 11, EBCDIC 'A' at offset 0
            var ebcdicD = (byte)0xC4; // EBCDIC code for 'D'
            var ebcdicA = (byte)0xC1; // EBCDIC code for 'A'
            var tempInput = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".dat");
            var buffer = new byte[4000];
            buffer[11] = ebcdicD;
            buffer[0] = ebcdicA;
            File.WriteAllBytes(tempInput, buffer);

            // Act: process EBCDIC to ASCII
            var outDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
            var processor = new EbcdicProcessor(compiled);
            processor.ProcessDatToAsc("JOB1", tempInput, outDir);

            // Assert: .dat.asc.11.1.d file exists and first byte is ASCII 'A'
            var dFile = Path.Combine(outDir, "JOB1.dat.asc.11.1.d");
            Assert.True(File.Exists(dFile), "D split file was not created");
            var dBytes = File.ReadAllBytes(dFile);
            Assert.NotEmpty(dBytes);
            Assert.Equal((byte)'A', dBytes[0]);
        }
    }
}
