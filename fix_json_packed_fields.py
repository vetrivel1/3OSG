#!/usr/bin/env python3
"""
Fix FieldDefinitions_Generated.json to correctly map "Packed Number" fields to type 3
Based on Legacy DD file analysis
"""

import json
import sys
import os

def load_legacy_packed_fields():
    """Load all Packed Number field names from legacy DD file"""
    dd_file = r"c:\Users\Shan\Documents\3OSG\Legacy Application\Scripts\MBCNTR2503\mblps\mblps.dd"
    packed_fields = set()
    
    try:
        with open(dd_file, 'r') as f:
            for line in f:
                line = line.strip()
                if 'Packed Number' in line:
                    # Parse: MB-ACCOUNT, 10, 7, Packed Number, 0
                    parts = [p.strip() for p in line.split(',')]
                    if len(parts) >= 4:
                        field_name = parts[0].replace('-', '_')  # Convert MB-ACCOUNT to MB_ACCOUNT
                        packed_fields.add(field_name)
        
        print(f"📋 Found {len(packed_fields)} Packed Number fields in legacy DD file")
        return packed_fields
        
    except FileNotFoundError:
        print(f"❌ Could not find DD file: {dd_file}")
        return set()

def fix_json_file():
    """Fix the JSON file to set correct types for Packed Number fields"""
    json_file = r"c:\Users\Shan\Documents\3OSG\MBCNTR2503.Modernizer\src\MBCNTR2503.Pipeline\FieldDefinitions_Generated.json"
    
    # Load legacy packed fields
    packed_fields = load_legacy_packed_fields()
    if not packed_fields:
        print("❌ No packed fields found, exiting")
        return False
    
    # Load JSON
    try:
        with open(json_file, 'r') as f:
            data = json.load(f)
    except FileNotFoundError:
        print(f"❌ Could not find JSON file: {json_file}")
        return False
    
    # Create backup
    backup_file = json_file + ".backup"
    with open(backup_file, 'w') as f:
        json.dump(data, f, indent=2)
    print(f"💾 Created backup: {backup_file}")
    
    # Fix fields in MBLPS layout
    if 'recordLayouts' not in data or 'MBLPS' not in data['recordLayouts']:
        print("❌ MBLPS layout not found in JSON")
        return False
    
    layout = data['recordLayouts']['MBLPS']
    fixed_count = 0
    
    for field in layout['fields']:
        field_name = field.get('name', '')
        if field_name in packed_fields:
            old_type = field.get('type', 0)
            if old_type != 3:  # Only change if not already type 3
                field['type'] = 3
                field['description'] = field.get('description', '') + ' (Packed Number)'
                print(f"🔧 Fixed {field_name}: type {old_type} → 3")
                fixed_count += 1
            else:
                print(f"✅ {field_name}: already type 3")
    
    # Save fixed JSON
    with open(json_file, 'w') as f:
        json.dump(data, f, indent=2)
    
    print(f"✅ Fixed {fixed_count} fields in JSON file")
    print(f"📝 Updated: {json_file}")
    return True

if __name__ == "__main__":
    print("🔧 Fixing JSON field definitions for Packed Number fields")
    print("=" * 60)
    
    success = fix_json_file()
    
    if success:
        print("\n🎉 JSON fix completed successfully!")
        print("💡 You can now remove any workaround code and rely on the JSON types")
    else:
        print("\n❌ JSON fix failed")
        sys.exit(1)