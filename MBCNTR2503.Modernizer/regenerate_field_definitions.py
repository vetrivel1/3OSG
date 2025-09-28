#!/usr/bin/env python3
"""
Regenerate FieldDefinitions_Generated.json with correct decimal places from DD file
"""

import json
import re
from pathlib import Path

def parse_dd_file(dd_path):
    """Parse the DD file to extract field definitions with decimal places"""
    fields = {}
    
    with open(dd_path, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line or line.startswith('#'):
                continue
                
            # Parse DD format: FIELD-NAME, offset, length, type, decimal_places
            parts = [p.strip() for p in line.split(',')]
            if len(parts) >= 5:
                field_name = parts[0]
                try:
                    offset = int(parts[1])
                    length = int(parts[2])
                    field_type = parts[3]
                    decimal_places = int(parts[4])
                    
                    # Convert field type
                    type_num = 0  # Default to text
                    if 'Packed Number' in field_type:
                        type_num = 1
                    elif 'Number' in field_type:
                        type_num = 2
                    
                    fields[field_name] = {
                        'name': field_name.replace('-', '_'),  # Convert to underscore format
                        'startPosition': offset,
                        'length': length,
                        'type': type_num,
                        'decimalPlaces': decimal_places,
                        'preserveLeadingSpaces': True,
                        'description': f"{field_type} field"
                    }
                    
                    # DEBUG: Show specific fields
                    if 'PAYMENT-AMOUNT' in field_name:
                        print(f"DEBUG: Parsed {field_name}: offset={offset}, length={length}, type={field_type}, decimal_places={decimal_places}")
                    
                except ValueError as e:
                    print(f"Warning: Could not parse line {line_num}: {line} - {e}")
                    continue
            else:
                print(f"Warning: Invalid DD format on line {line_num}: {line}")
    
    return fields

def regenerate_field_definitions():
    """Regenerate FieldDefinitions_Generated.json with correct decimal places"""
    
    dd_path = Path('/Users/vshanmu/3OSG/MBCNTR2503.Modernizer/config/base/mblps/mblps.dd')
    output_path = Path('/Users/vshanmu/3OSG/FieldDefinitions_Generated.json')
    
    if not dd_path.exists():
        print(f"❌ DD file not found: {dd_path}")
        return False
    
    print(f"🔍 Parsing DD file: {dd_path}")
    fields = parse_dd_file(dd_path)
    
    if not fields:
        print("❌ No fields parsed from DD file")
        return False
    
    print(f"✅ Parsed {len(fields)} fields from DD file")
    
    # Create the JSON structure (matching expected format)
    field_definitions = {
        "recordLayouts": {
            "mblps": {
                "fields": list(fields.values())
            }
        }
    }
    
    # Write to file
    print(f"📝 Writing field definitions to: {output_path}")
    with open(output_path, 'w') as f:
        json.dump(field_definitions, f, indent=2)
    
    print("✅ Field definitions regenerated successfully!")
    
    # Show some examples of fields with decimal places
    decimal_fields = [f for f in fields.values() if f['decimalPlaces'] > 0]
    print(f"\n📊 Found {len(decimal_fields)} fields with decimal places:")
    for field in decimal_fields[:10]:
        print(f"   • {field['name']}: {field['decimalPlaces']} decimal places")
    if len(decimal_fields) > 10:
        print(f"   ... and {len(decimal_fields) - 10} more")
    
    return True

if __name__ == "__main__":
    success = regenerate_field_definitions()
    if not success:
        exit(1)
