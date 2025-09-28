#!/bin/bash
# MBCNTR2503 Modernizer - Generate .set files & run unit tests

set -e  # Exit on any error

# Ensure we're at the repo root
echo "🔨 Changing to project root"
cd /Users/vshanmu/3OSG

echo "🏆 MBCNTR2503 MODERNIZER - GENERATE ALL .SET & UNIT TESTS"
echo "======================================================="

echo "🔨 Building solution..."
dotnet build 3OSG.sln --configuration Release --verbosity minimal

# List of jobs to process
jobs=(69172 80147 80299 80362)

for JOB in "${jobs[@]}"; do
  echo ""
  echo "📋 Generating .set for job $JOB..."
  # Determine input keyed path, prefer out directory, else fallback to Legacy Expected_Outputs
  if [ -f "MBCNTR2503.Modernizer/out/$JOB/${JOB}.dat.asc.11.1.p.keyed" ]; then
    input_keyed="MBCNTR2503.Modernizer/out/$JOB/${JOB}.dat.asc.11.1.p.keyed"
  else
    input_keyed="/Users/vshanmu/3OSG/Legacy Application/Expected_Outputs/$JOB/${JOB}.dat.asc.11.1.p.keyed"
  fi
  echo "    Using input: $input_keyed"
  # Ensure output directory exists
  mkdir -p "MBCNTR2503.Modernizer/out/$JOB"
  dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- mb2000-convert --job $JOB \
    --input "$input_keyed" \
    --out "MBCNTR2503.Modernizer/out/$JOB" \
    --schema "MBCNTR2503.Modernizer/config/base/mblps"
  echo "  ✅ Job $JOB .set generated"
done

# Run unit tests

echo ""
echo "✅ Running unit tests..."
dotnet test "MBCNTR2503.Modernizer/tests/Unit.Tests" --verbosity normal

echo ""
echo "🏁 Generate .set & unit tests script complete"
