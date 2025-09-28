#!/bin/bash
# MBCNTR2503 Modernizer - MB2000 Conversion Validation Suite
# This script runs MB2000 conversion and comprehensive parity validation

set -e  # Exit on any error

cd /Users/vshanmu/3OSG

echo "🔄 MBCNTR2503 MB2000 CONVERSION VALIDATION SUITE"
echo "================================================="

echo ""
echo "🔨 Building solution..."
dotnet build MBCNTR2503.Modernizer/src/Cnp.Pipeline/Cnp.Pipeline.csproj --configuration Release --verbosity minimal > /dev/null

echo ""
echo "🚀 Running MB2000 conversion for all jobs..."
for JOB in 69172 80147 80299 80362; do
    echo ""
    echo "📋 Processing job $JOB..."
    
    # Run MB2000 conversion
    echo "  Converting: $JOB.dat.asc.11.1.p.keyed → ${JOB}p.set"
    dotnet run --project "MBCNTR2503.Modernizer/src/Cnp.Cli" -- mb2000-convert \
        --job $JOB \
        --input "Legacy Application/Expected_Outputs/$JOB/$JOB.dat.asc.11.1.p.keyed" \
        --out "MBCNTR2503.Modernizer/out/$JOB" \
        --schema "MBCNTR2503.Modernizer/config/base/mblps" > /dev/null
    
    echo "  ✅ Job $JOB conversion completed"
done

echo ""
echo "🔍 COMPREHENSIVE MB2000 PARITY VERIFICATION:"
echo "============================================"

# Create reports directory if it doesn't exist
mkdir -p MBCNTR2503.Modernizer/tests/reports

echo ""
echo "📊 MB2000 Binary Parity Analysis:"
echo "================================="

TOTAL_BYTES=0
TOTAL_MATCHES=0
ALL_PERFECT=true
PERFECT_JOBS=""
IMPERFECT_JOBS=""

for JOB in 69172 80147 80299 80362; do
    echo ""
    echo "🔍 Job $JOB MB2000 Analysis:"
    echo "----------------------------"
    
    # Check if files exist
    EXPECTED_FILE="Legacy Application/Expected_Outputs/$JOB/${JOB}p.set"
    ACTUAL_FILE="MBCNTR2503.Modernizer/out/$JOB/${JOB}p.set"
    
    if [[ ! -f "$EXPECTED_FILE" ]]; then
        echo "  ⚠️  Expected file not found: $EXPECTED_FILE"
        continue
    fi
    
    if [[ ! -f "$ACTUAL_FILE" ]]; then
        echo "  ❌ Actual file not found: $ACTUAL_FILE"
        continue
    fi
    
    # Get file sizes
    EXPECTED_SIZE=$(wc -c < "$EXPECTED_FILE")
    ACTUAL_SIZE=$(wc -c < "$ACTUAL_FILE")
    
    echo "  Expected size: $(printf "%'d" $EXPECTED_SIZE) bytes"
    echo "  Actual size:   $(printf "%'d" $ACTUAL_SIZE) bytes"
    
    if [[ $EXPECTED_SIZE -ne $ACTUAL_SIZE ]]; then
        echo "  ❌ SIZE MISMATCH!"
        ALL_PERFECT=false
        IMPERFECT_JOBS="$IMPERFECT_JOBS $JOB"
        continue
    fi
    
    # Run binary comparison using cmp
    if cmp -s "$EXPECTED_FILE" "$ACTUAL_FILE"; then
        echo "  ✅ PERFECT BINARY PARITY (100.00%)!"
        PERFECT_JOBS="$PERFECT_JOBS $JOB"
        TOTAL_MATCHES=$((TOTAL_MATCHES + EXPECTED_SIZE))
    else
        # Count matching bytes
        DIFF_OUTPUT=$(cmp -l "$EXPECTED_FILE" "$ACTUAL_FILE" 2>/dev/null | wc -l || echo "0")
        DIFFERENT_BYTES=$DIFF_OUTPUT
        MATCHING_BYTES=$((EXPECTED_SIZE - DIFFERENT_BYTES))
        PARITY=$(echo "scale=2; $MATCHING_BYTES * 100 / $EXPECTED_SIZE" | bc -l)
        
        echo "  Matching bytes: $(printf "%'d" $MATCHING_BYTES) / $(printf "%'d" $EXPECTED_SIZE)"
        echo "  Parity: ${PARITY}%"
        echo "  Different bytes: $DIFFERENT_BYTES"
        
        if [[ $DIFFERENT_BYTES -eq 1 ]]; then
            # Show the exact difference for single byte differences
            DIFF_DETAIL=$(cmp -l "$EXPECTED_FILE" "$ACTUAL_FILE" 2>/dev/null | head -1)
            echo "  Difference: $DIFF_DETAIL"
        fi
        
        ALL_PERFECT=false
        IMPERFECT_JOBS="$IMPERFECT_JOBS $JOB"
        TOTAL_MATCHES=$((TOTAL_MATCHES + MATCHING_BYTES))
    fi
    
    TOTAL_BYTES=$((TOTAL_BYTES + EXPECTED_SIZE))
done

echo ""
echo "🏁 FINAL MB2000 VERIFICATION SUMMARY:"
echo "===================================="
echo "Total Bytes Processed: $(printf "%'d" $TOTAL_BYTES)"
echo "Total Bytes Matched:   $(printf "%'d" $TOTAL_MATCHES)"

if [[ $TOTAL_BYTES -gt 0 ]]; then
    OVERALL_PARITY=$(echo "scale=2; $TOTAL_MATCHES * 100 / $TOTAL_BYTES" | bc -l)
    echo "Overall Binary Parity: ${OVERALL_PARITY}%"
else
    echo "Overall Binary Parity: 0.00%"
    OVERALL_PARITY="0.00"
fi

if [[ "$ALL_PERFECT" == true ]]; then
    echo "🎉 PERFECT BINARY PARITY ACHIEVED ACROSS ALL JOBS!"
    echo "✅ Perfect Jobs: $PERFECT_JOBS"
else
    echo "❌ BINARY PARITY ISSUES DETECTED!"
    if [[ -n "$PERFECT_JOBS" ]]; then
        echo "✅ Perfect Jobs: $PERFECT_JOBS"
    fi
    if [[ -n "$IMPERFECT_JOBS" ]]; then
        echo "❌ Imperfect Jobs: $IMPERFECT_JOBS"
    fi
fi

echo ""
echo "📊 COMPREHENSIVE MB2000 SUMMARY:"
echo "==============================="
echo "✅ Solution Build: Successful"
echo "✅ MB2000 Conversion: Completed for all jobs"
echo "✅ Binary Analysis: $(printf "%'d" $TOTAL_BYTES) bytes processed"

if [[ "$ALL_PERFECT" == true ]]; then
    echo "✅ MB2000 Parity: 100% perfect across all jobs"
    echo ""
    echo "🏆 MB2000 CONVERSION READY: Complete legacy parity achieved!"
    echo "🚀 PRODUCTION QUALITY: Zero tolerance for data loss achieved!"
else
    echo "⚠️  MB2000 Parity: ${OVERALL_PARITY}% - $(echo "$TOTAL_BYTES - $TOTAL_MATCHES" | bc) bytes differ"
    echo ""
    if (( $(echo "$OVERALL_PARITY >= 99.9" | bc -l) )); then
        echo "🎯 NEAR-PERFECT: Excellent progress - minimal differences remain"
    elif (( $(echo "$OVERALL_PARITY >= 95.0" | bc -l) )); then
        echo "🔧 GOOD PROGRESS: Field mappings working well - refinement needed"
    else
        echo "⚠️  REQUIRES ATTENTION: Significant field mapping work needed"
    fi
fi

echo ""
echo "📁 Conversion outputs saved in: MBCNTR2503.Modernizer/out/{job}/"
echo ""
