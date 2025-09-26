#!/usr/bin/env python3
"""
Simple validation script for EBCDIC to ASCII conversion
Compares our generated files with legacy expected outputs
"""

import os
import sys

def compare_files(expected_file, actual_file, max_differences=20):
    """Compare two files byte by byte and report differences"""
    if not os.path.exists(expected_file):
        print(f"❌ Expected file not found: {expected_file}")
        return False
    
    if not os.path.exists(actual_file):
        print(f"❌ Actual file not found: {actual_file}")
        return False
    
    with open(expected_file, 'rb') as f1, open(actual_file, 'rb') as f2:
        expected_data = f1.read()
        actual_data = f2.read()
    
    if len(expected_data) != len(actual_data):
        print(f"❌ Size mismatch: Expected {len(expected_data)} bytes, Actual {len(actual_data)} bytes")
        return False
    
    differences = []
    for i in range(len(expected_data)):
        if expected_data[i] != actual_data[i]:
            differences.append((i, expected_data[i], actual_data[i]))
            if len(differences) >= max_differences:
                break
    
    if differences:
        print(f"❌ Found {len(differences)} differences (showing first {max_differences}):")
        for i, (pos, exp, act) in enumerate(differences):
            record_num = pos // 1500 + 1
            record_offset = pos % 1500
            print(f"  {i+1}. Byte {pos} (Record {record_num}, Offset {record_offset}): Expected 0x{exp:02X} ({chr(exp) if 32 <= exp <= 126 else '?'}), Actual 0x{act:02X} ({chr(act) if 32 <= act <= 126 else '?'})")
        return False
    else:
        print(f"✅ Files match perfectly! ({len(expected_data)} bytes)")
        return True

def main():
    job = "69172"
    base_dir = "/Users/vshanmu/3OSG"
    expected_dir = f"{base_dir}/Legacy Application/Expected_Outputs/{job}"
    actual_dir = f"{base_dir}/MBCNTR2503.Modernizer/out/{job}"
    
    print(f"🔍 EBCDIC to ASCII Validation - Job {job}")
    print("=" * 50)
    
    files_to_check = [
        f"{job}.dat.asc",
        f"{job}.dat.asc.11.1.d", 
        f"{job}.dat.asc.11.1.p",
        f"{job}.dat.asc.11.1.s"
    ]
    
    all_match = True
    for filename in files_to_check:
        print(f"\n📄 Checking {filename}:")
        expected_file = os.path.join(expected_dir, filename)
        actual_file = os.path.join(actual_dir, filename)
        
        if compare_files(expected_file, actual_file):
            print(f"✅ {filename} - PERFECT MATCH!")
        else:
            print(f"❌ {filename} - DIFFERENCES FOUND")
            all_match = False
    
    print("\n" + "=" * 50)
    if all_match:
        print("🎉 ALL FILES MATCH PERFECTLY - 100% PARITY ACHIEVED!")
    else:
        print("⚠️  SOME FILES HAVE DIFFERENCES - FURTHER DEBUGGING NEEDED")
    
    return 0 if all_match else 1

if __name__ == "__main__":
    sys.exit(main())
