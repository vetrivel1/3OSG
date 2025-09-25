#!/usr/bin/env python3
"""
Text file comparison script for .4300.txt files
Provides detailed analysis of differences between expected and actual text output
"""

import sys
import os
from pathlib import Path

def compare_text_files(expected_path, actual_path, job_id):
    """Compare two text files line by line and provide detailed analysis"""
    
    print(f"🔍 TEXT COMPARISON ANALYSIS - Job {job_id}")
    print("=" * 60)
    
    if not os.path.exists(expected_path):
        print(f"❌ Expected file not found: {expected_path}")
        return False
        
    if not os.path.exists(actual_path):
        print(f"❌ Actual file not found: {actual_path}")
        return False
    
    # Read both files
    with open(expected_path, 'r', encoding='ascii', errors='replace') as f:
        expected_lines = f.readlines()
    
    with open(actual_path, 'r', encoding='ascii', errors='replace') as f:
        actual_lines = f.readlines()
    
    print(f"📊 SUMMARY:")
    print(f"   Expected lines: {len(expected_lines)}")
    print(f"   Actual lines:   {len(actual_lines)}")
    
    # Compare line counts
    line_count_match = len(expected_lines) == len(actual_lines)
    if line_count_match:
        print(f"   ✅ Line count matches")
    else:
        print(f"   ❌ Line count mismatch!")
        
    print()
    
    # Compare lines
    max_lines = max(len(expected_lines), len(actual_lines))
    differences = []
    perfect_matches = 0
    
    for i in range(max_lines):
        expected_line = expected_lines[i].rstrip('\n\r') if i < len(expected_lines) else ""
        actual_line = actual_lines[i].rstrip('\n\r') if i < len(actual_lines) else ""
        
        if expected_line == actual_line:
            perfect_matches += 1
        else:
            differences.append({
                'line_num': i + 1,
                'expected': expected_line,
                'actual': actual_line
            })
    
    # Calculate parity
    total_lines = max_lines
    parity_percent = (perfect_matches / total_lines * 100) if total_lines > 0 else 0
    
    print(f"📈 PARITY ANALYSIS:")
    print(f"   Perfect matches: {perfect_matches}/{total_lines}")
    print(f"   Differences:     {len(differences)}")
    print(f"   Parity:          {parity_percent:.2f}%")
    
    if parity_percent == 100.0:
        print(f"   🎉 PERFECT PARITY ACHIEVED!")
        return True
    
    print()
    print(f"🔍 DETAILED DIFFERENCES:")
    print("-" * 60)
    
    # Show first 10 differences in detail
    for i, diff in enumerate(differences[:10]):
        line_num = diff['line_num']
        expected = diff['expected']
        actual = diff['actual']
        
        print(f"\n📍 Line {line_num}:")
        
        # Analyze the difference
        if len(expected) != len(actual):
            print(f"   Length: Expected {len(expected)}, Actual {len(actual)}")
        
        # Split by pipes to analyze field by field
        expected_fields = expected.split('|')
        actual_fields = actual.split('|')
        
        max_fields = max(len(expected_fields), len(actual_fields))
        
        field_diffs = []
        for j in range(max_fields):
            exp_field = expected_fields[j] if j < len(expected_fields) else ""
            act_field = actual_fields[j] if j < len(actual_fields) else ""
            
            if exp_field != act_field:
                field_diffs.append({
                    'field_num': j,
                    'expected': exp_field,
                    'actual': act_field
                })
        
        if field_diffs:
            print(f"   Field differences: {len(field_diffs)}")
            for fd in field_diffs[:5]:  # Show first 5 field differences
                print(f"     Field {fd['field_num']}: '{fd['expected']}' → '{fd['actual']}'")
        
        # Show first 100 characters of each line for context
        print(f"   Expected: {expected[:100]}{'...' if len(expected) > 100 else ''}")
        print(f"   Actual:   {actual[:100]}{'...' if len(actual) > 100 else ''}")
    
    if len(differences) > 10:
        print(f"\n... and {len(differences) - 10} more differences")
    
    print()
    print(f"💡 PATTERN ANALYSIS:")
    analyze_patterns(differences)
    
    return parity_percent == 100.0

def analyze_patterns(differences):
    """Analyze patterns in the differences to help identify systematic issues"""
    
    if not differences:
        return
    
    # Common patterns to look for
    trailing_pipe_issues = 0
    field_count_issues = 0
    packed_decimal_issues = 0
    empty_field_issues = 0
    
    for diff in differences:
        expected = diff['expected']
        actual = diff['actual']
        
        # Check for trailing pipe issues
        if expected.endswith('|') and actual.endswith('||'):
            trailing_pipe_issues += 1
        elif expected.endswith('|') != actual.endswith('|'):
            trailing_pipe_issues += 1
            
        # Check field counts
        exp_fields = len(expected.split('|'))
        act_fields = len(actual.split('|'))
        if exp_fields != act_fields:
            field_count_issues += 1
            
        # Check for common packed decimal issues (numbers that look wrong)
        if '303' in actual and '1' in expected:
            packed_decimal_issues += 1
        elif '404' in actual:
            packed_decimal_issues += 1
            
        # Check for empty field issues
        if '||' in actual and '||' not in expected:
            empty_field_issues += 1
    
    print(f"   Common issues found:")
    if trailing_pipe_issues > 0:
        print(f"   • Trailing pipe format: {trailing_pipe_issues} lines")
    if field_count_issues > 0:
        print(f"   • Field count mismatches: {field_count_issues} lines")
    if packed_decimal_issues > 0:
        print(f"   • Packed decimal issues: {packed_decimal_issues} lines")
    if empty_field_issues > 0:
        print(f"   • Empty field issues: {empty_field_issues} lines")
    
    if trailing_pipe_issues == 0 and field_count_issues == 0 and packed_decimal_issues == 0:
        print(f"   • No obvious systematic patterns detected")

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 compare-text.py <job_id> [job_id2 ...]")
        print("Example: python3 compare-text.py 69172 80147")
        sys.exit(1)
    
    job_ids = sys.argv[1:]
    all_perfect = True
    
    print(f"🚀 MBCNTR2503 TEXT EXTRACTION PARITY CHECKER")
    print(f"Checking {len(job_ids)} job(s): {', '.join(job_ids)}")
    print()
    
    for job_id in job_ids:
        expected_path = f"Legacy Application/Expected_Outputs/{job_id}/{job_id}.4300.txt"
        actual_path = f"MBCNTR2503.Modernizer/out/{job_id}/{job_id}.4300.txt"
        
        is_perfect = compare_text_files(expected_path, actual_path, job_id)
        all_perfect = all_perfect and is_perfect
        
        if len(job_ids) > 1:
            print("\n" + "="*60 + "\n")
    
    print(f"🏁 FINAL RESULT:")
    if all_perfect:
        print(f"   🎉 ALL JOBS ACHIEVED PERFECT TEXT PARITY!")
    else:
        print(f"   📈 Some jobs need additional work to achieve perfect parity")
    
    return 0 if all_perfect else 1

if __name__ == "__main__":
    sys.exit(main())
