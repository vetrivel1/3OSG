## Parity Issue Snapshot — EBCDIC → ASCII Header Windows

### Context and goal
- **Objective**: Achieve 100% parity for `.dat.asc` and `.asc.11.1.s` across jobs 69172, 80147, 80299, 80362.
- **Pipeline**: Modern C# `Cnp.Pipeline.EbcdicProcessor.ProcessDatToAsc` reproduces legacy `mbcnvt0.c` behavior.
- **Constraint**: `ConvertStandard` must use IBM037.

### Current status (jobs 69172 / 80147 / 80299 / 80362)
- **Header windows remain the only blockers for `.dat.asc` and `.asc.11.1.s`**. Latest parity runs (2025-09-30) still report repeating modulo-1500 windows; all other regions are byte-for-byte.
- **Job 69172**
  - `69172.dat.asc`: 40 differing bytes. `asc_diff_offsets.py` reports `[9..10]` (count=40). Expected bytes are `%_` (`0x25 0x5F`) while the modern pipeline emits ASCII spaces (`0x20 0x20`).
  - `69172.dat.asc.11.1.s`: 30 differing bytes within the same `[9..10]` window (count=30). Deltas show `%_` or `ES` (`0x45 0x8F`) swapped for ASCII spaces.
- **Job 80147**
  - `80147.dat.asc`: 403 differing bytes. Dominant ranges: `[9..10]`, `[37..121]`, `[129..146]`, `[1001..1010]`. Legacy bytes include `0x00`, `0x1F`, `%_`, and other IBM037 sentinels sourced from `.4300.txt.new`.
  - `80147.dat.asc.11.1.s`: 390 differing bytes within the same windows (`0x00/0x1F` falling back to ASCII spaces).
- **Job 80299**
  - `80299.dat.asc`: 829 differing bytes. `[9..10]` dominates, with the same secondary windows (`[37..121]`, `[129..146]`, `[1005..1010]`).
  - `80299.dat.asc.11.1.s`: 757 differing bytes mirroring the P-record windows.
- **Job 80362**
  - `80362.dat.asc`: 696 differing bytes with `[9..10]` drives plus the familiar secondary windows.
  - `80362.dat.asc.11.1.s`: 655 differing bytes across the same ranges.
- **Split P/D files** (`.asc.11.1.p`, `.asc.11.1.d`) remain ≥99.9% matched, confirming the discrepancy originates in shared header normalization prior to the split.
- **Debug signal**: with `STEP1_DEBUG=1`, `EbcdicProcessor` logs show legacy `%_`/`0x1F` bytes entering `NormalizeCommonWindows` before they get flattened to spaces (modern cache currently falls back to raw input).

### Root-cause hypothesis
- Header subfields around offsets **[8–11]** and the secondary windows **[37–121]**, **[129–146]**, **[1001–1010]** are sensitive to the ordering between IBM037 conversion, memmoves, and split handling. Legacy spacing/placeholder semantics rely on converting `%_`, `0x00`, and `0x1F` sentinels through IBM037 before any ASCII-space normalization.

### What is already in place
- **Diff tooling**: `scripts/asc_diff_offsets.py` reports absolute and modulo-1500 differences and suggests windows.
- **Debug logging**: `EbcdicProcessor` prints `hdr[5..6]` and `hdr[8..10]` for P/S when `STEP1_DEBUG=1`.
- **Code normalization**: `NormalizeCommonWindows` currently targets recurring windows [9–11], [790–791], [1509–1511], and [2291–2292] (stride 6000). Secondary windows ([37–121], [129–146], [1001–1010]) are being pulled from configuration.
- **Overrides (precise deltas)**: Narrow `deltaAfterIBM037` rules with `repeatEvery=1500` cover `[9..10]`. Broader windows are intentionally excluded for now to avoid drift; upcoming overrides should target specific ranges while remaining data-driven.
- **CLI usage tip**: invoke `dotnet run -- ... --schema "/Users/.../config/base/mblps"` (absolute path) to avoid the double-prefix issue when running commands manually. `run-single-job.sh` already does this.

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

# Repeat to inspect secondary windows or other jobs
python3 "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/scripts/asc_diff_offsets.py" \
  "Legacy Application/Expected_Outputs/<job>/<job>.dat.asc" \
  "MBCNTR2503.Modernizer/out/<job>/<job>.dat.asc" --rec-len 1500 --top 40
python3 "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/scripts/asc_diff_offsets.py" \
  "Legacy Application/Expected_Outputs/<job>/<job>.dat.asc.11.1.s" \
  "MBCNTR2503.Modernizer/out/<job>/<job>.dat.asc.11.1.s" --rec-len 1500 --top 40

# Optional: header debugging for P/S records
export STEP1_DEBUG=1
dotnet run --project "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/src/Cnp.Cli" \
  -- run-pipeline --job 69172 \
  --schema "/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/config/base/mblps" --verbose
```

### Recommended next steps
- **Short term** (to reach 100% on 69172):
  - Apply or adjust minimal JSON fallback overrides for header offsets **[9–10]** with both generic and P/S-specific rules (`deltaAfterIBM037`, `repeatEvery=1500`) so `%_` / `0x00` sentinels survive IBM037 conversion.
  - Extend overrides (or equivalent code normalization) to the secondary ranges observed in jobs 80147/80299/80362: `[37–121]`, `[129–146]`, `[1001–1010]`. Use configuration-based masks so packed digits and client-specific fields remain untouched.
  - Re-run pipeline and diffs to confirm `.dat.asc` and split files (`.asc.11.1.p`, `.asc.11.1.s`) reach 100% parity for job 69172. Capture before/after diff summaries for documentation.
  - Once confirmed, repeat the same schema overrides and diff checks for jobs 80147, 80299, and 80362. Keep overrides scoped to configuration (`override.json`)—no hardcoded client logic.

- **Long term** (replace overrides with code):
  - Move stable header handling into `EbcdicProcessor` as permanent, field-aware normalization after memmoves but before writes. Preserve IBM037 placeholders (`%_`, `0x1F`, packed zeros) before trimming to ASCII spaces.
  - Add unit coverage around header window normalization so new clients inherit the fix without overrides.
  - Incrementally remove the corresponding override deltas once validated across all jobs.

### Tips / quick reminders
- Always run manual CLI commands with absolute schema/input paths; relative paths cause the schema directory lookup to fail.
- Inspect `.4300.txt.new` to understand the exact header literals legacy injects—those bytes must be present in the cache to eliminate ASC diffs.
- `STEP1_DEBUG=1` remains invaluable for confirming raw header bytes prior to normalization.

### Verification checklist
- Capture baseline parity metrics for all four jobs via `generate_report.py` before adjusting overrides.
- Update `config/base/mblps/ebcdic.overrides.json` using `repeatEvery=1500` windows; keep configuration data-driven.
- Re-run the pipeline per job and re-run `asc_diff_offsets.py` for both `.dat.asc` and `.asc.11.1.s`; verify modulo buckets clear out.
- Enable `STEP1_DEBUG=1` on at least one run to confirm legacy header bytes persist through IBM037 conversion prior to normalization.

### Notes to kick off a new session
- Start by running the Reproduce commands above.
- Use `asc_diff_offsets.py` output to confirm modulo windows and remaining byte pairs.
- Tweak only narrow header window deltas in `ebcdic.overrides.json` (repeatEvery=1500), rerun, and re-check.
- After 69172 reaches 100%, apply the same to 80147/80299/80362; then convert stable deltas to permanent code normalization and remove the deltas.


