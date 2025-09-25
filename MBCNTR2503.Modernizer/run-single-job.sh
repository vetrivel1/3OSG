#!/bin/bash
# MBCNTR2503 Modernizer - Single Job Test Script
# Usage: ./run-single-job.sh <job_number>
# Example: ./run-single-job.sh 69172

set -e  # Exit on any error

if [ $# -eq 0 ]; then
    echo "Usage: $0 <job_number>"
    echo "Example: $0 69172"
    echo "Available jobs: 69172, 80147, 80299, 80362"
    exit 1
fi

JOB=$1
cd /Users/vshanmu/3OSG

echo "🏆 MBCNTR2503 MODERNIZER - SINGLE JOB TEST"
echo "=========================================="
echo "Processing job: $JOB"
echo ""

echo "🔧 Building schema (if needed)..."
dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- build-schema --schema "MBCNTR2503.Modernizer/config/base/mblps" --out "MBCNTR2503.Modernizer/schemas/compiled" > /dev/null

echo "📋 Step 1: Generating container file (.4300)..."
dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- run-step1 --job $JOB --input "Legacy Application/Input/$JOB.dat" --out "MBCNTR2503.Modernizer/out/$JOB" --schema "MBCNTR2503.Modernizer/config/base/mblps"

echo ""
echo "📋 Step 2: Extracting text (.4300.txt)..."
dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- extract-text --job $JOB --input "MBCNTR2503.Modernizer/out/$JOB/$JOB.4300" --out "MBCNTR2503.Modernizer/out/$JOB" --schema "MBCNTR2503.Modernizer/config/base/mblps"

echo ""
echo "🏆 RESULTS FOR JOB $JOB:"
echo "========================"
python3 MBCNTR2503.Modernizer/compare-text.py $JOB

echo ""
echo "🔍 BINARY VERIFICATION:"
echo "======================="
if [ -f "MBCNTR2503.Modernizer/tests/binary_4300_diff.py" ]; then
    echo "Binary comparison available - use: python3 MBCNTR2503.Modernizer/tests/binary_4300_diff.py --expected 'Legacy Application/Expected_Outputs/$JOB/$JOB.4300' --actual 'MBCNTR2503.Modernizer/out/$JOB/$JOB.4300' --input-dat 'Legacy Application/Input/$JOB.dat'"
else
    echo "✅ Binary files generated successfully"
    echo "Generated: MBCNTR2503.Modernizer/out/$JOB/$JOB.4300"
    echo "Generated: MBCNTR2503.Modernizer/out/$JOB/$JOB.4300.txt"
fi

echo ""
echo "✅ Job $JOB processing complete!"
echo ""
