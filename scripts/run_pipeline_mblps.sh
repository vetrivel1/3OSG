#!/usr/bin/env bash
set -euo pipefail
# Run the full modernization pipeline (all stages) for ALL jobs or a specified job.
# Usage:
#   bash scripts/run_pipeline_mblps.sh           # runs for ALL jobs
#   bash scripts/run_pipeline_mblps.sh 80362     # runs only for job 80362

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

CLI_PROJECT="MBCNTR2503.Modernizer/src/Cnp.Cli"
SCHEMA_DIR="MBCNTR2503.Modernizer/config/base/mblps"
COMPILED_DIR="MBCNTR2503.Modernizer/schemas/compiled"
INPUT_DIR="MBCNTR2503.Modernizer/Input"
OUT_DIR="MBCNTR2503.Modernizer/out"

JOB_ID="${1:-}"

# Resolve job list
if [[ -n "$JOB_ID" ]]; then
  JOBS=($JOB_ID)
else
  # Discover jobs by scanning Input directory for *.dat files
  if command -v find >/dev/null 2>&1; then
    mapfile -t JOBS < <(find "$INPUT_DIR" -maxdepth 1 -type f -name "*.dat" -printf "%f\n" 2>/dev/null | sed -E 's/\.dat$//' | sort)
  else
    # Fallback for Windows environments without GNU find in PATH
    mapfile -t JOBS < <(powershell -NoProfile -Command "Get-ChildItem -File -LiteralPath '$INPUT_DIR' -Filter '*.dat' | ForEach-Object { $_.BaseName } | Sort-Object" 2>$null)
  fi
fi

if [[ ${#JOBS[@]} -eq 0 ]]; then
  echo "No jobs found in $INPUT_DIR and no JOB_ID provided." >&2
  exit 1
fi

echo "[1/4] Cleaning output folder: $OUT_DIR"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

echo "[2/4] Building .NET solution..."
dotnet build 3OSG.sln -v minimal

echo "[3/4] Compiling schema (mblps) -> $COMPILED_DIR"
dotnet run --project "$CLI_PROJECT" -- build-schema \
  --schema "$SCHEMA_DIR" \
  --out "$COMPILED_DIR"

# Run full pipeline for each job
COUNTER=0
TOTAL=${#JOBS[@]}
for JOB in "${JOBS[@]}"; do
  COUNTER=$((COUNTER+1))
  IN_FILE="$INPUT_DIR/$JOB.dat"
  JOB_OUT="$OUT_DIR/$JOB"
  rm -rf "$JOB_OUT"
  mkdir -p "$JOB_OUT"

  echo "[$COUNTER/$TOTAL] Running full pipeline for job $JOB (schema=mblps) ..."
  dotnet run --project "$CLI_PROJECT" -- run-pipeline \
    --job "$JOB" \
    --schema "$SCHEMA_DIR" \
    --verbose

done

echo "Done. Outputs at: $OUT_DIR"
