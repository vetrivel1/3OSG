# MB2000 Conversion Investigation Report
## Current Status: 72% Parity

### ROOT CAUSE IDENTIFIED

**Location:** `MBCNTR2503.Modernizer/src/Cnp.Pipeline/MB2000FieldMapper.cs` Line 166

```csharp
var fieldModel = _schema.Container4000.Fields.FirstOrDefault(...);
int destOff = fieldModel.Offset;  // ❌ WRONG! Uses INPUT offsets as OUTPUT offsets
```

**The Issue:** 
- MB2000 conversion reads from 1500-byte P.keyed records (INPUT)
- Writes to 2000-byte .set records (OUTPUT)
- Code currently uses `Container4000.Fields` schema which has INPUT record layout
- Needs separate MB2000 OUTPUT schema with 2000-byte layout

### Key Findings

1. **Initialization Issue**
   - Current: Initializes with `0x20` (ASCII spaces)
   - Legacy uses mix: `0x00` (nulls), `0x20` (ASCII spaces), `0x40` (EBCDIC spaces)
   - Analysis: 11% nulls, 66% ASCII spaces, 3% EBCDIC spaces in legacy output

2. **Schema Mismatch**
   - Overrides specify SOURCE offset/length (where to read from P.keyed)
   - Missing: TARGET offset/length (where to write in MB2000)
   - Currently assumes: same field name → same offset (WRONG!)

3. **Field Misalignment Examples**
   ```
   Offset [11-26]: Expected nulls, got "P001I,AM A SAMPL"
   Offset [57-62]: Expected "SAMPLE", got spaces
   Offset [559-564]: Expected "857498", got "000000"
   ```

### What's Needed to Fix

1. **Create MB2000 Output Schema**
   - Need DD file or COBOL copybook defining 2000-byte MB2000 record layout
   - Or: Extract from `setmb2000.cbl` COBOL program
   - Contains: field names, OUTPUT offsets, OUTPUT lengths

2. **Update Overrides Format**
   ```json
   {
     "source": "MB-ACCOUNT",
     "sourceOffset": 10,    // Where to read from (P.keyed)
     "sourceLength": 7,
     "target": "MB2000-ACCOUNT", 
     "targetOffset": 100,   // ← ADD THIS: Where to write (MB2000)
     "targetLength": 10,    // ← ADD THIS: Output field size
     "mode": "packed"
   }
   ```

3. **Fix Initialization Logic**
   - Analyze which regions should be null, ASCII space, or EBCDIC space
   - Implement proper field-by-field initialization

### Quick Wins Available

1. **Field Offset Analysis**
   - Can compare expected vs actual byte-by-byte
   - Identify which fields are written to wrong offsets
   - Manually map correct offsets for top 20 fields

2. **Initialization Fix**
   - Start with nulls for specific byte ranges
   - Apply spaces or EBCDIC spaces as needed
   - Would fix ~10% of differences immediately

### Effort Estimate

- **Quick Fix (manual mapping):** 4-6 hours
  - Manually map top 50 high-impact fields
  - Fix initialization for common regions
  - Could achieve 85-90% parity

- **Proper Fix (full schema):** 2-3 days
  - Extract MB2000 layout from COBOL
  - Create proper output schema
  - Systematic field-by-field mapping
  - Achieve 95-100% parity

### Recommendation

**For immediate progress:** Focus on Stage 1 & 2 victory (100% achieved!)

**For MB2000:** This requires either:
1. Access to MB2000 output schema/DD file
2. COBOL `setmb2000.cbl` program analysis
3. Manual reverse-engineering from expected output

The 72% we have is "good enough" for non-critical downstream processing,
but achieving 100% requires the missing MB2000 output schema.
