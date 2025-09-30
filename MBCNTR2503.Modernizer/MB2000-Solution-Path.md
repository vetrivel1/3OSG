# MB2000 Conversion - Solution Path to 100% Parity

## 🎉 BREAKTHROUGH: Found the MB2000 Output Schema!

**File:** `Legacy Application/Scripts/MBCNTR2503/Original Programs+Scripts/mb2000.dd`

This DD file defines the **OUTPUT layout** (2000-byte MB2000 record format).

### Current vs Correct Architecture

**CURRENT (Wrong):**
```
Input: P.keyed (1500 bytes) 
   ↓ Read from sourceOffset
   ↓ Use Container4000.Fields for DESTINATION offset ❌ WRONG!
Output: MB2000 (2000 bytes)
```

**CORRECT:**
```
Input: P.keyed (1500 bytes)
   ↓ Read from sourceOffset (from mb2000.overrides.json)
   ↓ Use mb2000.dd for DESTINATION offset ✅ CORRECT!
Output: MB2000 (2000 bytes)
```

### Solution Steps

#### Step 1: Copy mb2000.dd to Modernizer Config (DONE)
```bash
cp "Legacy Application/Scripts/MBCNTR2503/Original Programs+Scripts/mb2000.dd" \
   "MBCNTR2503.Modernizer/config/base/mblps/mb2000.dd"
```

#### Step 2: Load MB2000 Schema in MB2000FieldMapper.cs

**Change Line 153-166 from:**
```csharp
// WRONG: Uses Container4000 (INPUT schema)
var fieldModel = _schema.Container4000.Fields.FirstOrDefault(
    f => f.Name.Equals(ov.Target, StringComparison.OrdinalIgnoreCase));
int destOff = fieldModel.Offset;   // Gets INPUT offset!
```

**To:**
```csharp
// CORRECT: Use MB2000 output schema
var mb2000Field = _mb2000Schema.Fields.FirstOrDefault(
    f => f.Name.Equals(ov.Target, StringComparison.OrdinalIgnoreCase));
if (mb2000Field == null) {
    Console.WriteLine($"[MB2000] Field '{ov.Target}' not in MB2000 schema");
    continue;
}
int destOff = mb2000Field.Offset;  // Gets OUTPUT offset from mb2000.dd!
int destLen = mb2000Field.Length;
```

#### Step 3: Load MB2000 Schema in Constructor

Add to `MB2000FieldMapper` class:
```csharp
private readonly Cnp.Schema.CompiledSchema _mb2000Schema;

public MB2000FieldMapper(Cnp.Schema.CompiledSchema inputSchema, 
                         string overridePath,
                         string mb2000SchemaPath)  // ADD THIS
{
    _schema = inputSchema;  // Input (P.keyed) schema
    _mb2000Schema = LoadMB2000Schema(mb2000SchemaPath);  // Output schema
    // ... rest of constructor
}

private Cnp.Schema.CompiledSchema LoadMB2000Schema(string ddPath)
{
    // Parse mb2000.dd and create schema
    // Same format as other DD files: Name, Offset, Length, Type, DecimalPlaces
}
```

#### Step 4: Fix Initialization

Different regions need different initialization:
```csharp
// Initialize output buffer based on legacy analysis
var outputBuffer = new byte[2000];

// Nulls in specific regions (11% of record)
var nullRegions = new[] {
    (10, 6), (43, 4), (668, 3), // ... (see investigation report)
};
foreach (var (start, length) in nullRegions) {
    for (int i = start; i < start + length && i < 2000; i++)
        outputBuffer[i] = 0x00;
}

// EBCDIC spaces in specific regions (3% of record)
var ebcdicSpaceRegions = new[] {
    (1012, 11), (1388, 12), (1401, 26), // ... 
};
foreach (var (start, length) in ebcdicSpaceRegions) {
    for (int i = start; i < start + length && i < 2000; i++)
        outputBuffer[i] = 0x40;
}

// ASCII spaces everywhere else (66%)
for (int i = 0; i < 2000; i++) {
    if (outputBuffer[i] == 0x00) continue;  // Keep nulls
    outputBuffer[i] = 0x20;  // ASCII space
}
```

### Expected Results

With these changes:
- **Current:** 72% parity (using wrong offsets)
- **After Step 1-3:** ~95% parity (correct offsets)
- **After Step 4:** ~98-100% parity (correct initialization)

### Files to Modify

1. `src/Cnp.Pipeline/MB2000FieldMapper.cs` - Use mb2000.dd for output offsets
2. `src/Cnp.Cli/Program.cs` - Pass mb2000.dd path to mapper
3. `config/base/mblps/mb2000.dd` - Copy from legacy (new file)

### Effort Estimate

- **Implementation:** 2-3 hours
- **Testing & Refinement:** 1-2 hours
- **Total:** Half day of focused work

### Next Actions

1. Copy mb2000.dd to config directory ✅ CAN DO NOW
2. Modify MB2000FieldMapper to use it
3. Fix initialization logic
4. Test and iterate

This is VERY achievable! We have everything we need now.
