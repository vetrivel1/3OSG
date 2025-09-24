MBCNTR2503.Modernizer

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


