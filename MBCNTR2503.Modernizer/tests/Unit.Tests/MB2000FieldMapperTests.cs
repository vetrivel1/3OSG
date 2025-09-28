using System;
using System.IO;
using Xunit;
using Cnp.Pipeline;

namespace Unit.Tests
{
    public class MB2000FieldMapperTests
    {
        [Fact]
        public void Map_ShouldReturnIdenticalBytes_WhenNoOverrides()
        {
            // Arrange: create simple overrides file with no entries
            var tempOverride = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".json");
            File.WriteAllText(tempOverride, "{ \"overrides\": [] }");
            var mapper = new MB2000FieldMapper(tempOverride);
            var input = new byte[1500]; // sample keyed data
            new Random(0).NextBytes(input);

            // Act
            var output = mapper.Map(input);

            // Assert: since we currently passthrough, the first 1500 bytes should match input
            Assert.Equal(input, output.AsSpan(0, 1500).ToArray());
        }
    }
}
