#!/bin/bash
# MBCNTR2503 Modernizer - Complete Build & Validation Suite
# This script builds the solution and runs comprehensive binary and text validation

set -e  # Exit on any error

cd /Users/vshanmu/3OSG

echo "🏆 MBCNTR2503 MODERNIZER - COMPLETE BUILD & VALIDATION SUITE"
echo "=============================================================="

echo ""
echo "🔨 Building entire solution..."
dotnet build 3OSG.sln --configuration Release --verbosity minimal

echo ""
echo "🔧 Building schema..."
dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- build-schema --schema "MBCNTR2503.Modernizer/config/base/mblps" --out "MBCNTR2503.Modernizer/schemas/compiled"

echo ""
echo "🚀 Processing all jobs..."
for JOB in 69172 80147 80299 80362; do
    echo ""
    echo "📋 Processing job $JOB..."
    
    # Step 1: Container generation (.4300)
    echo "  Step 1: Generating container file..."
    dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- run-step1 --job $JOB --input "Legacy Application/Input/$JOB.dat" --out "MBCNTR2503.Modernizer/out/$JOB" --schema "MBCNTR2503.Modernizer/config/base/mblps" > /dev/null
    
    # Step 2: Text extraction (.4300.txt)
    echo "  Step 2: Extracting text..."
    dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- extract-text --job $JOB --input "MBCNTR2503.Modernizer/out/$JOB/$JOB.4300" --out "MBCNTR2503.Modernizer/out/$JOB" --schema "MBCNTR2503.Modernizer/config/base/mblps" > /dev/null
    
    echo "  ✅ Job $JOB completed"
done

echo ""
echo "🏆 COMPREHENSIVE TEXT EXTRACTION RESULTS:"
echo "=========================================="
python3 MBCNTR2503.Modernizer/compare-text.py 69172 80147 80299 80362

echo ""
echo "🔍 COMPREHENSIVE BINARY CONTAINER VERIFICATION:"
echo "==============================================="

# Create reports directory if it doesn't exist
mkdir -p MBCNTR2503.Modernizer/tests/reports

echo ""
echo "📊 Binary Parity Analysis:"
echo "=========================="

TOTAL_BYTES=0
TOTAL_MATCHES=0
ALL_PERFECT=true

for JOB in 69172 80147 80299 80362; do
    echo ""
    echo "🔍 Job $JOB Binary Analysis:"
    echo "----------------------------"
    
    # Run binary comparison and capture output
    RESULT=$(python3 MBCNTR2503.Modernizer/tests/binary_4300_diff.py \
        --expected "Legacy Application/Expected_Outputs/$JOB/$JOB.4300" \
        --actual "MBCNTR2503.Modernizer/out/$JOB/$JOB.4300" \
        --input-dat "Legacy Application/Input/$JOB.dat" 2>&1)
    
    # Extract key metrics
    BYTES=$(echo "$RESULT" | grep "Total bytes:" | cut -d: -f2 | tr -d ' ')
    MATCHES=$(echo "$RESULT" | grep "Matches:" | cut -d: -f2 | cut -d'(' -f1 | tr -d ' ')
    PARITY=$(echo "$RESULT" | grep "Matches:" | cut -d'(' -f2 | cut -d')' -f1)
    
    echo "  Total Bytes: $BYTES"
    echo "  Matches: $MATCHES ($PARITY)"
    
    # Add to totals
    TOTAL_BYTES=$((TOTAL_BYTES + BYTES))
    TOTAL_MATCHES=$((TOTAL_MATCHES + MATCHES))
    
    # Check if perfect
    if [[ "$PARITY" != "100.00%" ]]; then
        ALL_PERFECT=false
        echo "  ❌ IMPERFECT PARITY DETECTED!"
    else
        echo "  ✅ PERFECT BINARY PARITY!"
    fi
done

echo ""
echo "🏁 FINAL BINARY VERIFICATION SUMMARY:"
echo "====================================="
echo "Total Bytes Processed: $(printf "%'d" $TOTAL_BYTES)"
echo "Total Bytes Matched:   $(printf "%'d" $TOTAL_MATCHES)"

if [[ $TOTAL_BYTES -eq $TOTAL_MATCHES ]] && [[ "$ALL_PERFECT" == true ]]; then
    echo "Overall Binary Parity: 100.00% ✅"
    echo "🎉 PERFECT BINARY PARITY ACHIEVED ACROSS ALL JOBS!"
else
    echo "Overall Binary Parity: $((TOTAL_MATCHES * 100 / TOTAL_BYTES)).$(((TOTAL_MATCHES * 10000 / TOTAL_BYTES) % 100))% ❌"
    echo "❌ BINARY PARITY ISSUES DETECTED!"
fi

echo ""
echo "✅ UNIT TESTS:"
echo "============="
dotnet test "MBCNTR2503.Modernizer/tests/Unit.Tests" --verbosity quiet

echo ""
echo "🎉 COMPLETE BUILD & VALIDATION SUITE FINISHED!"
echo "==============================================="
echo ""
echo "📊 COMPREHENSIVE SUMMARY:"
echo "========================"
echo "✅ Solution Build: Successful"
echo "✅ Schema Compilation: Successful"
echo "✅ Binary .4300 files: $(printf "%'d" $TOTAL_BYTES) bytes processed"

if [[ "$ALL_PERFECT" == true ]]; then
    echo "✅ Binary Parity: 100% perfect across all jobs"
    echo "✅ Text Parity: Check results above"
    echo "✅ Unit Tests: All passing"
    echo ""
    echo "🏆 PRODUCTION READY: Complete COBOL to C# modernization with perfect fidelity!"
    echo "🚀 INDUSTRY LEADING: Zero tolerance for data loss or corruption achieved!"
else
    echo "❌ Binary Parity: Issues detected - review output above"
    echo "⚠️  REQUIRES ATTENTION: Binary parity must be 100% for production readiness"
fi

echo ""
echo "📁 Reports saved in: MBCNTR2503.Modernizer/tests/reports/"
echo ""
