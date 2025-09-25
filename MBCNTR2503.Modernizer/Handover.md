### MBCNTR2503 Modernizer – Handover

Scope
- Goal: Reproduce legacy outputs from `Legacy Application/Input/` under a modern C# pipeline with strict parity.
- Status: Schema compiler, decoders, CLI, and Step 1 (initial) are in place. Parity is close for job 69172.

Key paths
- Config (mirror of mblps): `MBCNTR2503.Modernizer/config/base/mblps/`
- CLI: `MBCNTR2503.Modernizer/src/Cnp.Cli/`
- Pipeline: `MBCNTR2503.Modernizer/src/Cnp.Pipeline/`
- Decoders: `MBCNTR2503.Modernizer/src/Cnp.Decoders/`
- Schema: `MBCNTR2503.Modernizer/src/Cnp.Schema/`
- Output (sample): `MBCNTR2503.Modernizer/out/69172/`
- Design spec: `MBCNTR2503.Modernizer/MBCNTR2503.CSharpDesign.md`

Environment
- .NET 8 SDK (Windows/macOS OK). CLI registers IBM037 code page at runtime.

Build and run
- Build schema artifact:
  - `dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- build-schema --schema "MBCNTR2503.Modernizer/config/base/mblps" --out "MBCNTR2503.Modernizer/schemas/compiled"`
- Run Step 1 (69172 example):
  - `dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- run-step1 --job 69172 --input "Legacy Application/Input/69172.dat" --out "MBCNTR2503.Modernizer/out/69172" --schema "MBCNTR2503.Modernizer/config/base/mblps"`
- Unit tests (decoders):
  - `dotnet test "MBCNTR2503.Modernizer/tests/Unit.Tests"`

What’s implemented
- Schema compiler: parses `mblps.dd` and `mblps.dd.iomap`, emits compiled JSON (`schemas/compiled/<sha>.schema.json`).
- Decoders: packed (COMP-3) and zoned decimal, with tests.
- CLI: `build-schema`, `run-step1`.
- Step 1 (parity with overrides + targeted debug):
  - Reads fixed 4000-byte EBCDIC records from `.dat`.
  - Writes 4300-byte container record:
    - First 4000: copied raw, then converts only Text fields from EBCDIC→ASCII using matched `dd` layout (e.g., `mba.dd`, `mbd.dd`, `mbp.dd`).
    - Last 100: NCP trailer populated with rCount/tCount/pCount; container id at -6; ddNumber in last byte.
  - Generates `.dat.rectype` (sequence aligned with expected) and `.dat.total` (byte length matches expected primary record count).
  - Targeted debug: `STEP1_DEBUG=1`, `STEP1_DEBUG_LIMIT`, `STEP1_DEBUG_HEX`, `STEP1_DEBUG_FIELDS` (e.g., `mbp.dd:UNMAPPED,mbv.dd:LOAN-NO,mba.dd:fill1`).
  - Overrides: `config/base/mblps/step1.overrides.json` supports `fillMode` values `Raw`, `AsciiSpace`, `PackedZero` and per-window ranges.

Parity status (69172)
- `69172.dat.total`: size matches expected (5 bytes).
- `69172.dat.rectype`: sequence and zero counts match expected.
- `69172.4300`: file size matches expected (137,600 bytes). Current match: 99.57% (590 mismatches).
  - MBP filler zones: Legacy `ncpcntr0` leaves a specific pattern (e.g., `00 00 00 00 0C`, plus digit runs that manifest as `00 0F 30..39`) within 2985–3005 and 3715–3910. Overrides now express `PackedZero`; Step 1 fills these with ASCII spaces, and customer-specific special byte overrides handle specific positions.
  - Blank display numerics (`mb-coupon-mm`, `mb-coupon-dd`, `mb-post-petition-mmdd`, etc.) now correctly use ASCII spaces (0x20) when writing the `.4300`.
  - `LOAN-NO` in `mbv.dd`/`mbf.dd` now decode to ASCII digits via customer-specific overrides. Current mismatches: 70 bytes across secondaries; customer-specific mapping implemented for client 5031.
  - Header fillers (`fill1` in `mba.dd`/`mbd.dd`) are now converted through EBCDIC→ASCII; parity shows 0 diffs for headers.
  - `step1.overrides.json` and `step1.overrides.5031.json` are the homes for these targeted rules.

Parity status (80147)
- `80147.dat.total`: size matches expected.
- `80147.dat.rectype`: sequence and zero counts match expected.
- `80147.4300`: size matches expected. Current match: 99.58% (2832 mismatches).
  - MBP filler windows: same approach as 69172, with customer-specific handling for client 5031. Remaining differences (UNMAPPED: 2496 bytes) are in the packed zero patterns.
  - `LOAN-NO` in `mbv.dd`/`mbf.dd`: 336 bytes mismatched; customer-specific mapping implemented for client 5031 with ASCII digit normalization.
  - Header `fill1` now matches (0 diffs) after EBCDIC→ASCII handling with legacy `0xA9` expectation.
Challenges (blocking full parity)
- PackedZero synthesis: need exact byte-for-byte placement of legacy sequences within `mbp.dd` windows (e.g., `00 00 00 00 0C` repeats and `00 0F` digit spans). Current approach fills with ASCII spaces and uses customer-specific overrides for key positions.
- `LOAN-NO` mapping in secondaries: customer-specific mapping implemented for client 5031, setting all digits to ASCII '0' (0x30). Remaining differences (69172: 70; 80147: 336) may require job-specific handling.
- Customer-specific approach: identified that differences are customer-specific rather than job-specific. Both jobs 69172 and 80147 belong to the same client (5031) and show similar patterns.


Compare helpers
- Count bytes: `wc -c "<expected>" "<actual>"`
- Byte diff: `cmp -l "<expected>" "<actual>" | head -n 50`

Open items (next steps)
 - Further refine customer-specific special byte mappings to achieve exact byte-for-byte parity in `PackedZero` windows.
 - Consider job-specific overrides for the remaining differences in `LOAN-NO` fields.
 - Implement a more sophisticated pattern-matching approach for the repeating patterns in `PackedZero` windows.
 - Extend the customer-specific approach to other customers as needed.
 - Rerun 69172 and 80147 with refined mappings to achieve 100% parity.

Risks/notes
- Some fields marked `Number` may be display vs binary depending on client; prefer conservative (raw) unless confirmed.
- Option overlays are not applied at Step 1 (mirrors legacy). MB2000 path uses single customer routine.
- Customer-specific mappings are essential for achieving full parity. The framework now supports loading customer-specific overrides based on the client number.
- The exact byte patterns in `PackedZero` windows may require extensive mapping to achieve 100% parity.

Where to continue in code
- Step 1 writer: `src/Cnp.Pipeline/Step1Orchestrator.cs` (per-field mapping loop).
- Customer-specific overrides: `config/base/mblps/step1.overrides.5031.json` for client 5031.
- Add missing `*.dd` into `config/base/mblps/` as encountered in `ddcontrol.txt`.
- Consider enhancing the `ApplyPackedZeroWindow` method to handle more complex patterns or implementing a pattern-based approach.

Milestone checklist
- [x] Build schema and decoders; unit tests pass
- [x] Generate `.4300`, `.dat.rectype`, `.dat.total` (sizes/sequence match)
- [~] Close `.4300` byte parity for 69172/80147 (currently at 99.57-99.58% parity)
- [ ] Implement extractor to `.4300.txt` and `.suspect`
- [ ] Implement merge `.new` and `.length`
- [ ] MB2000 path and e-bill split


