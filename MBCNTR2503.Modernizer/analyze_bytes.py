#!/usr/bin/env python3
"""
Analyze byte-level differences between two files.
"""

import sys
import os
from pathlib import Path

def analyze_byte_differences(file1_path, file2_path, max_diff_to_show=20):
    """
    Compare two files byte by byte and report on differences.
    """
    with open(file1_path, 'rb') as f1, open(file2_path, 'rb') as f2:
        content1 = f1.read()
        content2 = f2.read()
    
    size1 = len(content1)
    size2 = len(content2)
    
    print(f"File 1: {file1_path} ({size1} bytes)")
    print(f"File 2: {file2_path} ({size2} bytes)")
    
    # Size comparison
    if size1 != size2:
        print(f"\nWARNING: Files have different sizes. Size diff: {abs(size1 - size2)} bytes")
    else:
        print(f"\nFiles have identical sizes: {size1} bytes")
    
    # Count and locate differences
    min_len = min(size1, size2)
    diff_positions = []
    
    for i in range(min_len):
        if content1[i] != content2[i]:
            diff_positions.append(i)
    
    total_diffs = len(diff_positions)
    diff_percentage = (total_diffs / min_len) * 100
    
    print(f"\nTotal differences: {total_diffs} bytes ({diff_percentage:.2f}% of compared content)")
    
    # Show sample of differences
    if diff_positions:
        print("\nSample differences (position: file1_byte -> file2_byte):")
        
        for i, pos in enumerate(diff_positions[:max_diff_to_show]):
            print(f"Pos {pos}: {content1[pos]:02X} -> {content2[pos]:02X} (Decimal: {content1[pos]} -> {content2[pos]})")
        
        if total_diffs > max_diff_to_show:
            print(f"... and {total_diffs - max_diff_to_show} more differences")
        
        # Look for patterns in the differences
        print("\nAnalyzing byte differences for patterns...")
        
        # Group differences by byte value changes
        byte_changes = {}
        for pos in diff_positions:
            change_key = f"{content1[pos]:02X}->{content2[pos]:02X}"
            if change_key not in byte_changes:
                byte_changes[change_key] = 0
            byte_changes[change_key] += 1
        
        # Show most common byte changes
        print("Most common byte changes:")
        for change, count in sorted(byte_changes.items(), key=lambda x: x[1], reverse=True)[:5]:
            from_byte, to_byte = change.split("->")
            print(f"  {from_byte} -> {to_byte}: {count} occurrences ({(count/total_diffs)*100:.1f}% of all differences)")
        
        # Check if differences are concentrated in certain regions
        if len(diff_positions) > 1:
            diff_gaps = [diff_positions[i+1] - diff_positions[i] for i in range(len(diff_positions)-1)]
            avg_gap = sum(diff_gaps) / len(diff_gaps)
            print(f"\nAverage gap between differences: {avg_gap:.2f} bytes")
            
            consecutive_diffs = sum(1 for gap in diff_gaps if gap == 1)
            consecutive_percentage = (consecutive_diffs / (len(diff_positions)-1)) * 100 if len(diff_positions) > 1 else 0
            print(f"Consecutive differences: {consecutive_diffs} ({consecutive_percentage:.2f}% of differences)")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python analyze_bytes.py <file1> <file2> [max_diff_to_show]")
        sys.exit(1)
    
    file1 = sys.argv[1]
    file2 = sys.argv[2]
    max_diff_to_show = int(sys.argv[3]) if len(sys.argv) > 3 else 20
    
    analyze_byte_differences(file1, file2, max_diff_to_show)
