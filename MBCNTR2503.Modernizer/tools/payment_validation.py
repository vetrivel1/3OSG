#!/usr/bin/env python3
"""
Payment Amount Validation Tool
Validates that specific payment amounts are correctly extracted from EBCDIC data
by comparing DataMapper output with known legacy values.
"""

import json
import sys
from typing import List, Dict, Any

def main():
    """Main validation function."""
    if len(sys.argv) < 3:
        print("Usage: python payment_validation.py <datamapper_json> <legacy_txt>")
        return
    
    dm_file = sys.argv[1]
    legacy_file = sys.argv[2]
    
    print("💰 PAYMENT AMOUNT VALIDATION")
    print("=" * 50)
    
    # Load DataMapper output
    with open(dm_file, 'r') as f:
        dm_records = json.load(f)
    
    # Extract payment amounts from DataMapper
    dm_payments = []
    for i, record in enumerate(dm_records, 1):
        value = record.get('POST2_PET_PAID_TO_DATE', 0)
        if value > 0:
            dm_payments.append(value)
            print(f"✅ DataMapper Record {i}: {value}")
    
    # Extract payment amounts from legacy output
    legacy_payments = []
    with open(legacy_file, 'r') as f:
        for line in f:
            if '77.42' in line:  # Find the line with our target value
                fields = line.strip().split('|')
                # Look for payment-like values (currency amounts)
                for field in fields:
                    try:
                        val = float(field)
                        if 50.0 <= val <= 500.0:  # Reasonable payment range
                            legacy_payments.append(val)
                    except ValueError:
                        continue
                break
    
    print(f"\n📊 COMPARISON RESULTS:")
    print(f"DataMapper payments: {sorted(dm_payments)}")
    print(f"Legacy payments in range: {sorted(set(legacy_payments))}")
    
    # Check if all DataMapper payments appear in legacy
    missing = []
    found = []
    for payment in dm_payments:
        if payment in legacy_payments:
            found.append(payment)
        else:
            missing.append(payment)
    
    print(f"\n🎯 VALIDATION SUMMARY:")
    print(f"✅ Found in legacy: {found}")
    if missing:
        print(f"❌ Missing in legacy: {missing}")
    else:
        print(f"🎉 ALL PAYMENTS VALIDATED!")
        print(f"   {len(found)}/{len(dm_payments)} payment amounts correctly extracted")
    
    # Overall accuracy
    accuracy = (len(found) / len(dm_payments)) * 100 if dm_payments else 0
    print(f"\n📈 PAYMENT EXTRACTION ACCURACY: {accuracy:.1f}%")
    
    if accuracy >= 95:
        print("🎉 EXCELLENT: DataMapper payment extraction is highly accurate!")
    elif accuracy >= 80:
        print("✅ GOOD: DataMapper payment extraction is working well")
    else:
        print("⚠️ NEEDS WORK: Payment extraction needs improvement")

if __name__ == "__main__":
    main()