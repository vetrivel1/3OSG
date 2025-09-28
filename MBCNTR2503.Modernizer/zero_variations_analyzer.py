#!/usr/bin/env python3
"""
Zero Variations Analyzer - Find fields getting zeros instead of expected data
"""

import json
import sys
import re
from collections import defaultdict, Counter

def analyze_zero_variations():
    """Analyze patterns where we're getting zeros instead of expected data"""
    
    # Run parity check and capture output
    import subprocess
    result = subprocess.run(['python3', 'debug-parity.py', '69172'], 
                          capture_output=True, text=True, cwd='/Users/vshanmu/3OSG/MBCNTR2503.Modernizer')
    
    if result.returncode != 0:
        print(f"❌ Parity check failed: {result.stderr}")
        return
    
    lines = result.stdout.split('\n')
    
    # Parse field differences
    zero_patterns = []
    current_field = None
    current_expected = None
    current_actual = None
    
    for line in lines:
        line = line.strip()
        if line.startswith('- Field:'):
            current_field = line.split(':', 1)[1].strip()
        elif line.startswith('- Expected:'):
            current_expected = line.split(':', 1)[1].strip().strip("'")
        elif line.startswith('- Actual:'):
            current_actual = line.split(':', 1)[1].strip().strip("'")
            
            # Analyze this field difference
            if current_field and current_expected and current_actual is not None:
                analyze_field_difference(current_field, current_expected, current_actual, zero_patterns)
                
            # Reset for next field
            current_field = None
            current_expected = None
            current_actual = None
    
    # Categorize and analyze patterns
    categorize_zero_patterns(zero_patterns)

def analyze_field_difference(field_name, expected, actual, zero_patterns):
    """Analyze a single field difference for zero patterns"""
    
    # Pattern 1: Expected non-zero, got zero
    if expected and expected != '0' and expected != '' and actual == '0':
        zero_patterns.append({
            'field': field_name,
            'pattern': 'EXPECTED_DATA_GOT_ZERO',
            'expected': expected,
            'actual': actual,
            'confidence': 95
        })
        return
    
    # Pattern 2: Expected number, got zeros (like '000000000')
    if expected and expected != '0' and re.match(r'^0+$', actual) and len(actual) > 1:
        zero_patterns.append({
            'field': field_name,
            'pattern': 'EXPECTED_NUMBER_GOT_ZEROS',
            'expected': expected,
            'actual': actual,
            'confidence': 90
        })
        return
    
    # Pattern 3: Expected decimal, got zero decimal (like '0000000.00')
    if expected and '.' in expected and expected != '0.00' and re.match(r'^0+\.0+$', actual):
        zero_patterns.append({
            'field': field_name,
            'pattern': 'EXPECTED_DECIMAL_GOT_ZERO_DECIMAL',
            'expected': expected,
            'actual': actual,
            'confidence': 85
        })
        return
    
    # Pattern 4: Expected data, got spaces that decode as zeros
    if expected and expected != '' and actual in ['202020202.02', '20.20202', '2020202.02']:
        zero_patterns.append({
            'field': field_name,
            'pattern': 'SPACES_DECODED_AS_ZEROS',
            'expected': expected,
            'actual': actual,
            'confidence': 80
        })
        return
    
    # Pattern 5: Expected empty, got zero (might be correct)
    if expected == '' and actual == '0':
        zero_patterns.append({
            'field': field_name,
            'pattern': 'EMPTY_TO_ZERO_CONVERSION',
            'expected': expected,
            'actual': actual,
            'confidence': 30  # Low confidence - might be correct
        })
        return

def categorize_zero_patterns(zero_patterns):
    """Categorize and prioritize zero patterns"""
    
    if not zero_patterns:
        print("✅ No zero variation patterns found!")
        return
    
    # Group by pattern type
    pattern_groups = defaultdict(list)
    for item in zero_patterns:
        pattern_groups[item['pattern']].append(item)
    
    print("🔍 ZERO VARIATIONS ANALYSIS")
    print("=" * 50)
    
    total_issues = len(zero_patterns)
    print(f"📊 Total zero variation issues found: {total_issues}")
    print()
    
    # Sort by impact (count * confidence)
    pattern_impact = []
    for pattern, items in pattern_groups.items():
        avg_confidence = sum(item['confidence'] for item in items) / len(items)
        impact_score = len(items) * (avg_confidence / 100)
        pattern_impact.append((pattern, items, impact_score, avg_confidence))
    
    pattern_impact.sort(key=lambda x: x[2], reverse=True)
    
    # Display results
    for i, (pattern, items, impact_score, avg_confidence) in enumerate(pattern_impact, 1):
        print(f"🎯 PATTERN #{i}: {pattern}")
        print(f"   📈 Count: {len(items)} fields ({len(items)/total_issues*100:.1f}%)")
        print(f"   🎯 Confidence: {avg_confidence:.1f}%")
        print(f"   💥 Impact Score: {impact_score:.1f}")
        print()
        
        # Show examples
        print("   📋 Examples:")
        for j, item in enumerate(items[:5], 1):
            print(f"      {j}. {item['field']}: '{item['expected']}' → '{item['actual']}'")
        if len(items) > 5:
            print(f"      ... and {len(items) - 5} more")
        print()
        
        # Suggest solutions
        suggest_solution(pattern, items[:3])
        print("-" * 50)

def suggest_solution(pattern, examples):
    """Suggest solutions for each pattern type"""
    
    print("   💡 RECOMMENDED SOLUTION:")
    
    if pattern == 'EXPECTED_DATA_GOT_ZERO':
        print("      • Check if fields are being initialized to zero instead of reading source data")
        print("      • Verify source offsets and lengths in overrides")
        print("      • Look for missing field mappings")
        
    elif pattern == 'EXPECTED_NUMBER_GOT_ZEROS':
        print("      • Check packed decimal decoding - might be reading wrong offset")
        print("      • Verify field lengths match expected data size")
        print("      • Check for endianness issues in numeric fields")
        
    elif pattern == 'EXPECTED_DECIMAL_GOT_ZERO_DECIMAL':
        print("      • Check decimal scaling in packed decimal conversion")
        print("      • Verify decimal places configuration in FieldDefinitions")
        print("      • Look for precision loss in numeric conversion")
        
    elif pattern == 'SPACES_DECODED_AS_ZEROS':
        print("      • This is the ASCII spaces issue we've seen before")
        print("      • Check if these fields need the ASCII detection fix")
        print("      • Verify EBCDIC vs ASCII source data detection")
        
    elif pattern == 'EMPTY_TO_ZERO_CONVERSION':
        print("      • This might be correct behavior (empty → zero)")
        print("      • Verify if legacy system actually outputs empty or zero")
        print("      • Low priority unless specifically required")

if __name__ == "__main__":
    analyze_zero_variations()
