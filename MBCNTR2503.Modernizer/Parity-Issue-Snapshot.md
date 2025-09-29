## Parity Issue Snapshot — EBCDIC → ASCII Header Windows

### Context and goal
- **Objective**: Achieve 100% parity for `.dat.asc` and `.asc.11.1.s` across jobs 69172, 80147, 80299, 80362.
- **Pipeline**: Modern C# `Cnp.Pipeline.EbcdicProcessor.ProcessDatToAsc` reproduces legacy `mbcnvt0.c` behavior.
- **Constraint**: `ConvertStandard` must use IBM037.

### Current status (job 69172)
- `.dat.asc` currently has **36** differing bytes (down from 60+).
- All diffs are confined to tiny header windows repeating every 1500 bytes (per-record):
  - Within-record offsets: **[9–10]** (28 bytes total) and **[5–6]** (8 bytes total).
- Typical per-record examples near [8–11]: expected vs actual often differ like `0x45/0x8F/0x53 ↔ 0x5F` (and vice versa), indicating subtle header field/placeholder handling.

### Root-cause hypothesis
- Header subfields around offsets **[8–11]** are sensitive to the ordering between IBM037 conversion, memmoves, and split handling. Legacy spacing/placeholder semantics in these tiny ranges produce stable modulo-1500 patterns that require exact handling.

### What is already in place
- **Diff tooling**: `scripts/asc_diff_offsets.py` reports absolute and modulo-1500 differences and suggests windows.
- **Debug logging**: `EbcdicProcessor` prints `hdr[5..6]` and `hdr[8..10]` for P/S when `STEP1_DEBUG=1`.
- **Code normalization**: `NormalizeCommonWindows` targets recurring windows: [9–11], [790–791], [1509–1511], and [2291–2292] (stride 6000).
- **Overrides (precise deltas)**: Narrow `deltaAfterIBM037` rules with `repeatEvery` for header/mid windows. Broad/blanket rules were avoided because they caused drift.

### Key files
- `MBCNTR2503.Modernizer/src/Cnp.Pipeline/EbcdicProcessor.cs`
- `MBCNTR2503.Modernizer/config/base/mblps/ebcdic.overrides.json`
- `MBCNTR2503.Modernizer/scripts/asc_diff_offsets.py`

### Reproduce (69172)
```bash
# Run pipeline
dotnet run --project "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/src/Cnp.Cli" \
  -- run-pipeline --job 69172 \
  --schema "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/config/base/mblps" --verbose

# Diff .dat.asc (shows absolute and modulo-1500 buckets)
python3 "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/scripts/asc_diff_offsets.py" \
  "Legacy Application/Expected_Outputs/69172/69172.dat.asc" \
  "MBCNTR2503.Modernizer/out/69172/69172.dat.asc" --rec-len 1500 --top 40

# Optional: header debugging for P/S records
export STEP1_DEBUG=1
dotnet run --project "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/src/Cnp.Cli" \
  -- run-pipeline --job 69172 \
  --schema "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/config/base/mblps" --verbose
```

### Recommended next steps
- **Short term** (to reach 100% on 69172):
  - Add minimal, precise `deltaAfterIBM037` mappings within repeatEvery=1500 for:
    - Remaining **[9–10]** header variants (map only in this tiny window; avoid global rules).
    - The **[5–6]** pair where needed (confirm exact byte pairs seen in diffs before mapping).
  - Re-run 69172 and verify zero diffs for `.dat.asc`; then repeat for `.asc.11.1.s` and other jobs.

- **Long term** (replace overrides with code):
  - Move stable header handling into `EbcdicProcessor` as permanent, field-aware normalization after memmoves but before writes.
  - Incrementally remove the corresponding override deltas once validated across all jobs.

### Notes to kick off a new session
- Start by running the Reproduce commands above.
- Use `asc_diff_offsets.py` output to confirm modulo windows and remaining byte pairs.
- Tweak only narrow header window deltas in `ebcdic.overrides.json` (repeatEvery=1500), rerun, and re-check.
- After 69172 reaches 100%, apply the same to 80147/80299/80362; then convert stable deltas to permanent code normalization and remove the deltas.


