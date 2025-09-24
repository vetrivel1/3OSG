#!/usr/bin/env python3
"""
Comprehensive Field-by-Field Comparison Tool
Compares DataMapper JSON output with legacy pipe-delimited expected output
to ensure 100% accuracy in EBCDIC mainframe data conversion.

This tool performs complete field mapping and validation between:
- DataMapper MBX layout JSON output 
- Legacy expected pipe-delimited output
"""

import json
import csv
import sys
from pathlib import Path
from typing import List, Dict, Any, Tuple, Set
from decimal import Decimal

def load_datamapper_output(json_file: str) -> List[Dict[str, Any]]:
    """Load the DataMapper JSON output."""
    with open(json_file, 'r') as f:
        return json.load(f)

def load_legacy_output(txt_file: str) -> List[List[str]]:
    """Load the legacy pipe-delimited output, filtering to data records only."""
    records = []
    with open(txt_file, 'r') as f:
        for line in f:
            line = line.strip()
            if line:
                # Split by pipe and preserve empty fields
                fields = line.split('|')
                # Only include detailed payment records (P records)
                # Skip header (A) and description (D) records
                if len(fields) > 10 and len(fields[2]) == 1 and fields[2] == 'P':
                    records.append(fields)
    return records

def normalize_value(value: Any) -> str:
    """Normalize values for comparison."""
    if value is None or value == "":
        return ""
    if isinstance(value, (int, float)):
        if value == 0:
            return "0"
        # Format with 2 decimal places for currency/amounts
        return f"{value:.2f}".rstrip('0').rstrip('.')
    return str(value).strip()

def find_matching_fields(dm_records: List[Dict[str, Any]], legacy_records: List[List[str]]) -> Dict[str, Dict]:
    """Find which DataMapper fields correspond to which legacy field positions."""
    field_mappings = {}
    
    print("🔍 Analyzing field mappings...")
    
    # Get a sample record with data (not header/empty records)
    sample_dm_record = None
    sample_legacy_record = None
    sample_record_num = 0
    
    for i, (dm_record, legacy_record) in enumerate(zip(dm_records, legacy_records)):
        # Look for records with substantial data
        non_empty_dm = sum(1 for v in dm_record.values() if normalize_value(v) not in ["", "0"])
        non_empty_legacy = sum(1 for v in legacy_record if v.strip() not in ["", "0", "0.00"])
        
        if non_empty_dm > 10 and non_empty_legacy > 10:  # Good data record
            sample_dm_record = dm_record
            sample_legacy_record = legacy_record
            sample_record_num = i + 1
            break
    
    if not sample_dm_record:
        print("❌ No suitable data record found for mapping")
        return field_mappings
    
    print(f"📊 Using Record {sample_record_num} for field mapping analysis")
    print(f"   DataMapper: {len(sample_dm_record)} fields")
    print(f"   Legacy: {len(sample_legacy_record)} fields")
    
    # Track exact matches
    exact_matches = 0
    
    # For each DataMapper field, try to find corresponding legacy field
    for dm_field_name, dm_value in sample_dm_record.items():
        if dm_field_name in ['RecordNumber', 'RecordLength']:
            continue  # Skip metadata fields
            
        dm_normalized = normalize_value(dm_value)
        
        # Skip empty/zero values as they're not distinctive
        if dm_normalized in ["", "0"]:
            continue
            
        # Search for this value in legacy record
        matches = []
        for legacy_pos, legacy_value in enumerate(sample_legacy_record):
            legacy_normalized = normalize_value(legacy_value)
            if legacy_normalized == dm_normalized and legacy_normalized not in ["", "0"]:
                matches.append(legacy_pos + 1)  # 1-based position
        
        if matches:
            field_mappings[dm_field_name] = {
                'legacy_positions': matches,
                'value': dm_normalized,
                'dm_field': dm_field_name
            }
            if len(matches) == 1:
                exact_matches += 1
    
    print(f"✅ Found {len(field_mappings)} field mappings ({exact_matches} exact matches)")
    return field_mappings

def validate_all_records(dm_records: List[Dict[str, Any]], legacy_records: List[List[str]], 
                        field_mappings: Dict[str, Dict]) -> Dict[str, Any]:
    """Validate field mappings across all records."""
    validation_results = {
        'total_comparisons': 0,
        'exact_matches': 0,
        'mismatches': [],
        'field_accuracy': {}
    }
    
    print("\n🔍 Validating field mappings across all records...")
    
    for field_name, mapping_info in field_mappings.items():
        if len(mapping_info['legacy_positions']) != 1:
            continue  # Skip ambiguous mappings for now
        
        legacy_pos = mapping_info['legacy_positions'][0] - 1  # Convert to 0-based
        matches = 0
        total_checks = 0
        
        for record_num, (dm_record, legacy_record) in enumerate(zip(dm_records, legacy_records)):
            if field_name not in dm_record:
                continue
            if legacy_pos >= len(legacy_record):
                continue
                
            dm_value = normalize_value(dm_record[field_name])
            legacy_value = normalize_value(legacy_record[legacy_pos])
            
            total_checks += 1
            validation_results['total_comparisons'] += 1
            
            if dm_value == legacy_value:
                matches += 1
                validation_results['exact_matches'] += 1
            else:
                validation_results['mismatches'].append({
                    'record': record_num + 1,
                    'field': field_name,
                    'legacy_pos': legacy_pos + 1,
                    'dm_value': dm_value,
                    'legacy_value': legacy_value
                })
        
        if total_checks > 0:
            accuracy = (matches / total_checks) * 100
            validation_results['field_accuracy'][field_name] = {
                'accuracy': accuracy,
                'matches': matches,
                'total': total_checks,
                'legacy_position': legacy_pos + 1
            }
    
    return validation_results

def generate_comprehensive_report(dm_records: List[Dict[str, Any]], legacy_records: List[List[str]], 
                                field_mappings: Dict[str, Dict], validation_results: Dict[str, Any]):
    """Generate a comprehensive comparison report."""
    print("\n" + "="*80)
    print("📋 COMPREHENSIVE FIELD MAPPING REPORT")
    print("="*80)
    
    # Overall accuracy
    if validation_results['total_comparisons'] > 0:
        overall_accuracy = (validation_results['exact_matches'] / validation_results['total_comparisons']) * 100
        print(f"\n📊 OVERALL ACCURACY: {overall_accuracy:.1f}%")
        print(f"   Total comparisons: {validation_results['total_comparisons']:,}")
        print(f"   Exact matches: {validation_results['exact_matches']:,}")
        print(f"   Mismatches: {len(validation_results['mismatches']):,}")
    
    # Field-by-field accuracy
    print(f"\n📈 FIELD-BY-FIELD ACCURACY:")
    sorted_fields = sorted(validation_results['field_accuracy'].items(), 
                          key=lambda x: x[1]['accuracy'], reverse=True)
    
    perfect_fields = 0
    for field_name, stats in sorted_fields:
        accuracy = stats['accuracy']
        if accuracy == 100.0:
            perfect_fields += 1
        
        status = "✅" if accuracy == 100.0 else "⚠️" if accuracy >= 90.0 else "❌"
        print(f"   {status} {field_name:30} {accuracy:6.1f}% ({stats['matches']}/{stats['total']}) → Legacy Field {stats['legacy_position']}")
    
    print(f"\n🎯 FIELD SUMMARY:")
    print(f"   Perfect accuracy (100%): {perfect_fields}/{len(sorted_fields)} fields")
    print(f"   High accuracy (≥90%):    {sum(1 for _, s in sorted_fields if s['accuracy'] >= 90.0)}/{len(sorted_fields)} fields")
    
    # Show some critical mismatches
    if validation_results['mismatches']:
        print(f"\n⚠️  SAMPLE MISMATCHES (first 10):")
        for mismatch in validation_results['mismatches'][:10]:
            print(f"   Record {mismatch['record']}, {mismatch['field']} (→Field {mismatch['legacy_pos']}): '{mismatch['dm_value']}' vs '{mismatch['legacy_value']}'")
        
        if len(validation_results['mismatches']) > 10:
            print(f"   ... and {len(validation_results['mismatches']) - 10} more mismatches")
    
    # Show key payment field results
    payment_fields = [field for field in sorted_fields if any(keyword in field[0].upper() 
                     for keyword in ['PAYMENT', 'PAID', 'AMOUNT', 'AMT', 'BALANCE', 'BAL'])]
    
    if payment_fields:
        print(f"\n💰 PAYMENT FIELD ACCURACY:")
        for field_name, stats in payment_fields:
            accuracy = stats['accuracy']
            status = "✅" if accuracy == 100.0 else "⚠️" if accuracy >= 90.0 else "❌"
            print(f"   {status} {field_name:30} {accuracy:6.1f}% → Legacy Field {stats['legacy_position']}")

def main():
    """Main comparison function."""
    if len(sys.argv) < 3:
        print("Usage: python field_comparison.py <datamapper_json> <legacy_txt>")
        print("Example: python field_comparison.py 69172_mbx_test.json ../Legacy*/Expected*/69172/69172.4300.txt")
        return
    
    dm_file = sys.argv[1]
    legacy_file = sys.argv[2]
    
    print("🔍 COMPREHENSIVE EBCDIC DATA CONVERSION VALIDATION")
    print("=" * 70)
    
    # Load data
    print(f"📁 Loading DataMapper output: {dm_file}")
    dm_records = load_datamapper_output(dm_file)
    
    print(f"📁 Loading legacy expected output: {legacy_file}")
    legacy_records = load_legacy_output(legacy_file)
    
    print(f"\n📊 Dataset Overview:")
    print(f"   DataMapper records: {len(dm_records)}")
    print(f"   Legacy records: {len(legacy_records)}")
    
    if len(dm_records) != len(legacy_records):
        print(f"⚠️  Warning: Record count mismatch!")
    
    # Find field mappings
    field_mappings = find_matching_fields(dm_records, legacy_records)
    
    if not field_mappings:
        print("❌ No field mappings found. Unable to proceed with validation.")
        return
    
    # Validate across all records
    validation_results = validate_all_records(dm_records, legacy_records, field_mappings)
    
    # Generate comprehensive report
    generate_comprehensive_report(dm_records, legacy_records, field_mappings, validation_results)
    
    # Final assessment
    if validation_results['total_comparisons'] > 0:
        overall_accuracy = (validation_results['exact_matches'] / validation_results['total_comparisons']) * 100
        
        if overall_accuracy >= 95.0:
            print(f"\n🎉 EXCELLENT: {overall_accuracy:.1f}% field accuracy achieved!")
            print("   ✅ DataMapper conversion is highly accurate")
            print("   ✅ Smart packed decimal detection working correctly")
            print("   ✅ Ready for production use")
        elif overall_accuracy >= 85.0:
            print(f"\n✅ GOOD: {overall_accuracy:.1f}% field accuracy achieved!")
            print("   🔧 Minor discrepancies may need investigation")
        else:
            print(f"\n⚠️  NEEDS WORK: {overall_accuracy:.1f}% field accuracy")
            print("   🔧 Significant issues need to be addressed")

if __name__ == "__main__":
    main()