MBCNTR2503.Modernizer

## Overview
This project modernizes the legacy MBCNTR2503 COBOL system to .NET, converting EBCDIC packed decimal mortgage billing records to ASCII format while maintaining exact parity with legacy outputs.

## Current Status
- **Parity Achievement**: 11.2% improvement (145 errors eliminated from ~1295 total)
- **Major Fixes Completed**:
  - ✅ LENGTH_MISMATCH pattern (120 errors fixed) - ASCII detection in text fields
  - ✅ ZERO_VARIATIONS pattern (5 errors fixed) - MB-PAYMENT-AMOUNT field mapping corrected
  - ✅ Field metadata loading path resolved
- **Remaining**: ~1150 field differences across all records

Setup
- Requires .NET 8 SDK (Windows)
- Config files at config/base/mblps (mirrors legacy mblps)

Build schema
- Run: dotnet run --project src/Cnp.Cli -- build-schema --schema "config/base/mblps" --out "schemas/compiled"
- Output: schemas/compiled/<sha>.schema.json

Projects
- src/Cnp.Schema: schema compiler and models
- src/Cnp.Decoders: packed/zoned/binary/EBCDIC decoding
- src/Cnp.Pipeline: orchestrator and stages (stubbed)
- src/Cnp.Cli: CLI entrypoint

Parity tests
- tests/Parity.Tests: harness to compare outputs with legacy Expected_Outputs

## End-to-End Workflow
- 1. Generate JSON schema from the COBOL copybook:
  ```bash
  python3 scripts/copybook_to_schema.py \
    -i "Legacy Application/Scripts/MBCNTR2503/mblps/mblps.dd.cbl" \
    -o "config/schemas/source/mb2000.output.schema.json" \
    -t "MB2000 Output Record"
  ```
- 2. Bootstrap overrides JSON from the compiled DD layout:
  ```bash
  dotnet run --project src/Cnp.Cli -- bootstrap-overrides-from-compiled \
    --schema config/base/mblps \
    --out config/base/mblps/mb2000.overrides.json
  ```
- 3. Build the compiled schema for pipeline consumption:
  ```bash
  dotnet run --project src/Cnp.Cli -- build-schema \
    --schema config/base/mblps \
    --out schemas/compiled
  ```
- 4. Run the full conversion pipeline with overrides:
  ```bash
  dotnet run --project src/Cnp.Cli -- mb2000-convert \
    --job <JOB> \
    --input "Legacy Application/Expected_Outputs/<JOB>/<JOB>.dat.asc.11.1.p.keyed" \
    --out out/<JOB> \
    --schema config/base/mblps
  ```
- 5. Verify parity against legacy outputs:
  ```bash
  python3 debug-parity.py <JOB>
  ```

---

## Key Technical Insights

### Field Mapping Corrections
- **MB-PAYMENT-AMOUNT**: Fixed offset from 749 to 323, length from 1 to 6 bytes
  - Source: `MB1100-TOT-PYMT` from `mb1500.dd` (total payment calculation)
  - Format: `S9(9)V99 COMP-3` (6-byte packed decimal with 2 decimal places)
- **FieldDefinitions_Generated.json**: Corrected loading path resolution for runtime field metadata

### Data Conversion Issues Resolved
- **ASCII Detection**: Implemented smart detection for text fields already in ASCII format
- **Packed Decimal Handling**: Raw copy for P-records, proper encoding for MB2000 output
- **EBCDIC vs ASCII**: Fixed parity script to decode MB2000 output as ASCII, not EBCDIC

### Debugging Methodology
1. **Pattern Analysis**: Categorized errors by type (LENGTH_MISMATCH, ZERO_VARIATIONS, etc.)
2. **Legacy Code Analysis**: Traced data flow through COBOL programs (`setmb2000.cbl`, `mb1500.dd`)
3. **Systematic Offset Correction**: Used hexdump analysis to find correct field positions
4. **Incremental Testing**: Fixed one pattern at a time with immediate parity validation

---

# Useful Commands (Recommended)
  ```bash
  # Build JSON schema from DD and IO map
  dotnet run --project src/Cnp.Cli -- build-schema --schema config/base/mblps --out schemas/compiled
  
  # Bootstrap overrides from compiled schema (preferred)
  dotnet run --project src/Cnp.Cli -- bootstrap-overrides-from-compiled \
    --schema config/base/mblps \
    --out config/base/mblps/mb2000.overrides.json
  
  # Initial skeleton overrides (deprecated)
  #  dotnet run --project src/Cnp.Cli -- init-overrides --template schemas/source/mb2000.output.schema.json --schema config/base/mblps --out overrides.template.json
  ```


