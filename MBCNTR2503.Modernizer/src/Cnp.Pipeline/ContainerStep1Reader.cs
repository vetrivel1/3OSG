using System.Text;
using Cnp.Schema;

namespace Cnp.Pipeline;

public sealed class ContainerStep1Reader
{
    private readonly CompiledSchema _schema;
    public ContainerStep1Reader(CompiledSchema schema) => _schema = schema;

    // Stub: read fixed-length EBCDIC .dat and emit .4300 per layout (ASCII where appropriate)
    public void Process(string inputDatPath, string output4300Path)
    {
        // Placeholder: copy file while ensuring record size 4000 + 300 client/NCP blocks alignment will be filled later
        // Actual implementation will read EBCDIC, map via DD, write standardized container records
        File.Copy(inputDatPath, output4300Path, overwrite: true);
    }
}

