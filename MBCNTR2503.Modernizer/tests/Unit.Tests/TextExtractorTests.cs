using System;
using System.IO;
using Xunit;
using Cnp.Schema;
using Cnp.Pipeline;

namespace Unit.Tests
{
    public class TextExtractorTests
    {
        [Fact]
        public void ExtractText_ShouldWriteFieldValueToOutput()
        {
            // Arrange: create temp schema directory with minimal dd and iomap
            var tempSchema = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
            Directory.CreateDirectory(tempSchema);
            // Create a dd file defining one field at offset 0 length 4
            File.WriteAllText(Path.Combine(tempSchema, "mblps.dd"), "TESTFIELD,0,4,C");
            // Create an empty iomap
            File.WriteAllText(Path.Combine(tempSchema, "mblps.dd.iomap"), "");
            // Compile schema
            var compiled = SchemaCompiler.Compile(tempSchema);
            // Prepare a 4300-byte record with 'S' at offset 11 and 'TEST' at offset 0
            var tempInput = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".4300");
            var buffer = new byte[4300];
            // set record type to 'S'
            buffer[11] = (byte)'S';
            // write ASCII 'TEST' at start
            var fieldBytes = System.Text.Encoding.ASCII.GetBytes("TEST");
            Array.Copy(fieldBytes, 0, buffer, 0, fieldBytes.Length);
            File.WriteAllBytes(tempInput, buffer);
            // Act: extract text
            var outDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
            var extractor = new TextExtractor(compiled);
            extractor.ExtractText("JOB1", tempInput, outDir);
            // Assert: output file exists and contains 'TEST'
            var outputFile = Path.Combine(outDir, "JOB1.4300.txt");
            Assert.True(File.Exists(outputFile), "Output text file was not created");
            var lines = File.ReadAllLines(outputFile);
            Assert.Single(lines);
            Assert.Equal("TEST", lines[0]);
        }
    }
}
