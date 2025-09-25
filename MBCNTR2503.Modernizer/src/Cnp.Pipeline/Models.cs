namespace Cnp.Pipeline;

public sealed record DdEntry(string Name, string RecordId, int RecordIdOffset, string ContainerId, string DdNumber, string Type);
public sealed record DdField(string Name, int Offset, int Length, string DataType, int Scale, string RawDataType);
