# Header-Window Normalization Status & Next Steps

## Current Status

**Job 69172**
- Main `.dat.asc` file: 36 mismatches remain in header window at offsets 9–10 (total diffs: 36/31500).  
- Split files (`.asc.11.1.p`, `.asc.11.1.s`, `.asc.11.1.d`): 0 diffs (100% parity).

**Other Jobs (80147, 80299, 80362)**
- Header normalization enhancements not yet applied; `.dat.asc` and split files still at ~99.9% parity.

## Next Steps

1. **Codify Legacy Header Logic for Job 69172**
   - Extract and translate the conditional mapping logic for offsets 9–10 from `mbcnvt0.c`.  
   - Implement precise per-record-type (D/P/S) branches in `NormalizeHeaderWindow` to cover all observed variants (`0x0A`, `0x2F`, `0x3F`, `0x5F`) mapping to expected (`0x25`, `0x61`, `0x5F`, `0x17`, `0x53`, `0x45`, `0x8F`).

2. **Apply Enhancements to Remaining Jobs**
   - Re-run pipeline for jobs 80147, 80299, and 80362 with the finalized `NormalizeHeaderWindow`.  
   - Verify both `.dat.asc` and split‐file parity reach 100%.

3. **Investigate Loan-Field Packing Effects**  
   - Analyze how packed vs. unpacked loan fields (offsets 4–10) influence header byte sequences.  
   - Ensure loan‐field behavior in C# matches legacy `FieldIsPacked` and conversion patterns.

4. **Clean Up JSON Overrides**  
   - Once all header normalization is in code, prune or remove any temporary `deltaAfterIBM037` rules related to header windows.

5. **Long-Term Integration**  
   - Refactor `EbcdicProcessor` to consolidate header and mid‐window normalization in a single, field‐aware component.  
   - Update documentation (`README.md`, design docs) to reflect new approach and remove obsolete override rules.

---
*Generated on \[DATE\]*
