# MB2000 Conversion - Deep Dive Findings

**Date:** September 30, 2025  
**Current Status:** 52-66% parity (depending on approach)

## Executive Summary

MB2000 conversion has revealed significant complexity beyond the initial EBCDIC→ASCII stages (which achieved 100% parity). Multiple approaches were tested, each revealing different aspects of the problem:

### Approaches Tested & Results

| Approach | Input Source | Parity | Overrides Applied | Finding |
|----------|--------------|--------|-------------------|---------|
| Original | 1500-byte .p.keyed | ~72% | 298/566 (53%) | Many fields skipped due to out-of-bounds source offsets |
| Pure EBCDIC | 4000-byte .dat | 66% | 566/566 (100%) | All fields processed, but source offsets from wrong schema |
| Fixed Offsets | 4000-byte .dat + Container4000 offsets | 62% | 566/566 (100%) | Corrected offsets made things WORSE - reveals schema mismatch |
| Reverted | 1500-byte .p.keyed (original) | 52% | 298/566 (53%) | Back to baseline, confirming data insufficiency |

## Key Discoveries

### 1. Legacy MB2000 Process Flow

Analyzed `setmb2000.cbl` and `setmb2000.script`:

```
Line 23: RECORD CONTAINS 1500 CHARACTERS
Line 36: copy '/users/devel/mb1500.cbl'
```

**Legacy Flow:**
1. Input: `{jobid}.dat.asc.11.1.p` (1500-byte P-records, post-EBCDIC conversion)
2. Schema: `mb1500.cbl` (1500-byte input layout)
3. Output: `{jobid}p.set` (2000-byte MB2000 format)
4. Rename: `.p.set` → `.p.asc` (for downstream processing)

**Critical:** The `{jobid}p.asc` files in Expected_Outputs are actually MB2000 **outputs**, not inputs!

### 2. Schema Misalignment

Three schemas are involved:
- **Container4000** (4000-byte EBCDIC input records)
- **mb1500** (1500-byte P-record layout - legacy MB2000 input)
- **mb2000** (2000-byte MB2000 output layout)

The `mb2000.overrides.json` was found to have:
- **Source offsets** that don't consistently match ANY schema
  - Some match Container4000 (4000-byte)
  - Some match mb1500 (1500-byte)
  - Some are completely wrong
- **506 out of 566 fields** had mismatched offsets when compared to Container4000

### 3. The Data Paradox

**Dilemma:**
- Reading from **1500-byte .p.keyed**: Only 298/566 overrides apply (53% - data missing)
- Reading from **4000-byte .dat**: 566/566 overrides apply but wrong source schema

**Best parity achieved:** 66% with 4000-byte input + EBCDIC→ASCII conversion

This suggests:
1. Some MB2000 fields ARE sourced from the full 4000-byte record
2. BUT the source offsets in `mb2000.overrides.json` are inconsistent
3. The legacy system might use a HYBRID approach (some fields from 1500-byte, some from 4000-byte)

### 4. Source Offset Analysis

Checked `mb2000.overrides.json` against Container4000 schema:

**Mismatches (first 50 fields):**
- MB-BILL-LINE-5: override says 135, Container4000 says 298 ❌
- MB-BILL-LINE-6: override says 155, Container4000 says 358 ❌
- MB-TELE-NO: override says 1080, Container4000 says 620 ❌
- MB-SEC-TELE-NO: override says 1080, Container4000 says 632 ❌
- ...29 total mismatches in first 50 fields (58%)

When we corrected these to Container4000 offsets, parity **decreased** from 66% to 62%, indicating the original overrides were manually tuned for a DIFFERENT input source.

## Root Cause Hypothesis

The `mb2000.overrides.json` file appears to be:
1. **Manually created** (not auto-generated from any schema)
2. **Tuned for a specific input format** that is neither pure Container4000 nor pure mb1500
3. **Possibly created through trial-and-error** to match legacy output

**Evidence:**
- Inconsistent source offsets (some 4000-byte, some 1500-byte)
- Fixing "wrong" offsets made things worse
- 66% parity with "wrong" offsets is better than 62% with "correct" offsets

## Technical Challenges

### 1. EBCDIC vs ASCII Detection

Initially had a bug where EBCDIC space (0x40) was detected as "already ASCII" because it falls in the printable ASCII range (0x20-0x7E). Fixed by always converting EBCDIC→ASCII for pure EBCDIC input.

### 2. MB2000 Output Schema

Successfully parsed `mb2000.cbl` COBOL copybook to generate `mb2000-output.dd` with correct 2000-byte output field definitions. This part is **confirmed correct**.

### 3. Missing mb1500 Schema

The legacy system uses `mb1500.cbl` as the input schema for MB2000, but we don't have:
- A parsed version of `mb1500.cbl`
- Understanding of which fields come from mb1500 vs Container4000
- The transformation logic that maps between schemas

## Recommended Next Steps

### Short Term (4-8 hours)
1. **Find and parse mb1500.cbl** to understand the 1500-byte P-record layout
2. **Compare mb1500 vs Container4000 schemas** to identify field mappings
3. **Analyze setmb2000.cbl COBOL logic** to understand business rules and transformations
4. **Create hybrid input approach**: Read from both 1500-byte .p.keyed AND original 4000-byte .dat to supply all needed fields

### Medium Term (1-2 days)
1. **Regenerate mb2000.overrides.json** from scratch:
   - Parse setmb2000.cbl to extract field mappings
   - Create correct source→destination mappings
   - Include all business logic (conditionals, calculations, etc.)

2. **Implement business logic** currently in COBOL:
   - Field-level transformations
   - Conditional mappings
   - Calculated fields

### Long Term (2-3 days)
1. **Full COBOL-to-C# conversion** of setmb2000.cbl logic
2. **Comprehensive testing** against all 4 jobs
3. **Field-by-field validation** to isolate remaining differences

## Files of Interest

### Schemas
- `/users/devel/mb1500.cbl` - 1500-byte P-record input schema (NEED TO FIND)
- `/users/devel/mb2000.cbl` - 2000-byte MB2000 output schema ✓ PARSED
- Container4000 schema (in compiled schemas) ✓ HAVE

### Legacy Code
- `setmb2000.cbl` - Main COBOL program for MB2000 conversion
- `setmb2000.script` - Shell script orchestrating the process
- `mbcntr2503.script` - Master script for full pipeline

### Generated
- `mb2000-output.dd` - Parsed from mb2000.cbl ✓ CORRECT
- `mb2000.overrides.json` - Field mappings ❌ INCONSISTENT
- `parse_cobol_mb2000.py` - COBOL copybook parser ✓ WORKING

## Current State

### ✅ What's Working
- Stages 1 & 2: 100% perfect parity (13 file types)
- MB2000 infrastructure: schema loading, field mapping framework
- EBCDIC→ASCII conversion: consistent and correct
- Output buffer initialization: basic structure in place

### ⚠️ What's Partially Working
- MB2000 field mappings: 66% parity achieved
- Source offset mappings: work for some fields, not all

### ❌ What's Not Working
- Consistent field mappings across all 566 fields
- Business logic from COBOL not yet implemented
- Hybrid input approach (some fields from 1500-byte, some from 4000-byte)

## Conclusion

MB2000 conversion is significantly more complex than initial EBCDIC stages due to:
1. Multiple input schemas involved (4000-byte, 1500-byte)
2. Manually-tuned field mappings that don't match documented schemas
3. COBOL business logic not yet translated
4. Lack of the mb1500.cbl schema definition

**Estimated effort to reach 95%+ parity:** 8-16 hours of focused investigation, schema analysis, and systematic field-by-field validation.

**Priority:** Find and parse `mb1500.cbl` to understand the actual input schema used by legacy MB2000.
