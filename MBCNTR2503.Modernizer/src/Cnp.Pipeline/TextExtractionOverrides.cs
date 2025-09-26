using System.Text.Json.Serialization;

namespace Cnp.Pipeline;

public class TextExtractionOverrides
{
    [JsonPropertyName("textExtraction")]
    public TextExtractionConfig? TextExtraction { get; set; }
}

public class TextExtractionConfig
{
    [JsonPropertyName("fieldSubstitutions")]
    public Dictionary<string, FieldSubstitution>? FieldSubstitutions { get; set; }
    
    [JsonPropertyName("computedFields")]
    public Dictionary<string, ComputedField>? ComputedFields { get; set; }
    
    [JsonPropertyName("fieldFormatting")]
    public FieldFormatting? FieldFormatting { get; set; }
}

public class FieldSubstitution
{
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    
    [JsonPropertyName("condition")]
    public SubstitutionCondition? Condition { get; set; }
    
    [JsonPropertyName("substituteWith")]
    public SubstitutionValue? SubstituteWith { get; set; }
    
    [JsonPropertyName("whitelist")]
    public Dictionary<string, List<int>>? Whitelist { get; set; }
    
    [JsonPropertyName("jobs")]
    public List<string>? Jobs { get; set; }
    
    [JsonPropertyName("excludeJobs")]
    public List<string>? ExcludeJobs { get; set; }
}

public class SubstitutionCondition
{
    [JsonPropertyName("field")]
    public string? Field { get; set; }
    
    [JsonPropertyName("offset")]
    public int? Offset { get; set; }
    
    [JsonPropertyName("length")]
    public int? Length { get; set; }
    
    [JsonPropertyName("dataType")]
    public string? DataType { get; set; }
    
    [JsonPropertyName("isEmpty")]
    public bool? IsEmpty { get; set; }
    
    [JsonPropertyName("equals")]
    public new string? Equals { get; set; }
    
    [JsonPropertyName("dependsOn")]
    public string? DependsOn { get; set; }
    
    [JsonPropertyName("description")]
    public string? Description { get; set; }
}

public class SubstitutionValue
{
    [JsonPropertyName("field")]
    public string? Field { get; set; }
    
    [JsonPropertyName("offset")]
    public int? Offset { get; set; }
    
    [JsonPropertyName("length")]
    public int? Length { get; set; }
    
    [JsonPropertyName("dataType")]
    public string? DataType { get; set; }
    
    [JsonPropertyName("decimals")]
    public int? Decimals { get; set; }
    
    [JsonPropertyName("value")]
    public string? Value { get; set; }
}

public class ComputedField
{
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    
    [JsonPropertyName("formula")]
    public string? Formula { get; set; }
    
    [JsonPropertyName("fields")]
    public List<ComputedFieldInput>? Fields { get; set; }
    
    [JsonPropertyName("jobs")]
    public List<string>? Jobs { get; set; }
}

public class ComputedFieldInput
{
    [JsonPropertyName("name")]
    public string? Name { get; set; }
    
    [JsonPropertyName("offset")]
    public int Offset { get; set; }
    
    [JsonPropertyName("length")]
    public int Length { get; set; }
    
    [JsonPropertyName("dataType")]
    public string? DataType { get; set; }
    
    [JsonPropertyName("decimals")]
    public int Decimals { get; set; }
}

public class FieldFormatting
{
    [JsonPropertyName("sRecords")]
    public SRecordFormatting? SRecords { get; set; }
}

public class SRecordFormatting
{
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    
    [JsonPropertyName("textFields")]
    public TextFieldFormatting? TextFields { get; set; }
}

public class TextFieldFormatting
{
    [JsonPropertyName("trimSpaces")]
    public bool TrimSpaces { get; set; }
    
    [JsonPropertyName("description")]
    public string? Description { get; set; }
}
