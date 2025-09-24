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
- `69172.4300`: file size matches expected (137,600 bytes). Small byte deltas remain, concentrated in header regions and a few early fields.

Compare helpers
- Count bytes: `wc -c "<expected>" "<actual>"`
- Byte diff: `cmp -l "<expected>" "<actual>" | head -n 50`

Open items (next steps)
- Extend per-field conversion across all `dd` layouts referenced in `ddcontrol.txt` (P/U/V/W/F/X/N/S…).
- Honor data types:
  - `Text`: convert EBCDIC→ASCII.
  - `Number`/`Int`: treat as display numerics (EBCDIC digits) only when present; otherwise leave raw.
  - `Mixed`: leave raw.
  - Packed/Zoned: leave raw in `.4300` (they’re binary); conversion handled later when extracting text (`.4300.txt`).
- Verify precise header (`mba.dd`/`mbd.dd`) conversion and filler bytes (keep raw vs space) per legacy behavior.
- Validate NCP trailer byte-for-byte (offsets and padding) on a sampling of records.

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


