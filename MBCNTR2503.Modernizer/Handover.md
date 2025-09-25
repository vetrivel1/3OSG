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
- Step 1 (initial parity):
  - Reads fixed 4000-byte EBCDIC records from `.dat`.
  - Writes 4300-byte container record:
    - First 4000: copied raw, then converts only Text fields from EBCDIC→ASCII using matched `dd` layout (e.g., `mba.dd`, `mbd.dd`, `mbp.dd`).
    - Last 100: NCP trailer populated with rCount/tCount/pCount; container id at -6; ddNumber in last byte.
  - Generates `.dat.rectype` (sequence aligned with expected) and `.dat.total` (byte length matches expected primary record count).

Parity status (69172)
- `69172.dat.total`: size matches expected (5 bytes).
- `69172.dat.rectype`: sequence and zero counts match expected.
- `69172.4300`: file size matches expected (137,600 bytes). Residual byte deltas (~0.45%) remain:
  - MBP filler zones: Legacy `ncpcntr0` leaves a specific pattern (`00 00 00 00 0C` padded with ASCII spaces) in the regions 2985–3005 and 3715–3910; current run emits ASCII spaces only. Need to reproduce the legacy packed-zero pattern when the source is blank.
  - Blank display numerics (`mb-coupon-mm`, `mb-coupon-dd`, `mb-post-petition-mmdd`, etc.) still default to EBCDIC blanks (`0x40`). Treat “all spaces” numeric fields as ASCII spaces when writing the `.4300`.
  - `LOAN-NO` in `mbv.dd`/`mbf.dd` should be ASCII digits; current output retains raw EBCDIC digits.
  - Header fillers (`fill1` in `mba.dd`/`mbd.dd`) must flow through EBCDIC→ASCII so `0xA9` appears instead of `0x0F`.
  - `step1.overrides.json` will host the final targeted rules once behavior is confirmed.

Compare helpers
- Count bytes: `wc -c "<expected>" "<actual>"`
- Byte diff: `cmp -l "<expected>" "<actual>" | head -n 50`

Open items (next steps)
- Confirm Stage 1 conversion rules in legacy `ncpcntr0` (blank display numerics, filler regions, header fillers) and mirror them in Step 1.
- Update `Step1Orchestrator` to emit ASCII spaces for blank numeric fields and preserve legacy packed-zero filler patterns.
- Convert `LOAN-NO` (secondary layouts) via text decoding instead of copying raw EBCDIC.
- Expand `step1.overrides.json` with explicit fill modes (`raw` vs `ascii-space`) once legacy behavior is verified.
- Rerun jobs 69172 and 80147 to confirm parity; repeat for remaining fixtures.

Risks/notes
- Some fields marked `Number` may be display vs binary depending on client; prefer conservative (raw) unless confirmed.
- Option overlays are not applied at Step 1 (mirrors legacy). MB2000 path uses single customer routine.

Where to continue in code
- Step 1 writer: `src/Cnp.Pipeline/Step1Orchestrator.cs` (per-field mapping loop).
- Add missing `*.dd` into `config/base/mblps/` as encountered in `ddcontrol.txt`.

Milestone checklist
- [x] Build schema and decoders; unit tests pass
- [x] Generate `.4300`, `.dat.rectype`, `.dat.total` (sizes/sequence match)
- [ ] Close `.4300` byte parity for 69172
- [ ] Implement extractor to `.4300.txt` and `.suspect`
- [ ] Implement merge `.new` and `.length`
- [ ] MB2000 path and e-bill split


