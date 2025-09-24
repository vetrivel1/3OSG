using System.Collections.Immutable;

namespace Cnp.Schema;

public sealed record FieldModel(
    string Name,
    int Offset,
    int Length,
    string DataType,
    int Scale
);

public sealed record RecordLayout(
    string Name,
    int Length,
    ImmutableArray<FieldModel> Fields
);

public sealed record CompiledSchema(
    string Sha,
    string SourceDir,
    RecordLayout Container4000,
    ImmutableDictionary<string,string> IoMap
);


