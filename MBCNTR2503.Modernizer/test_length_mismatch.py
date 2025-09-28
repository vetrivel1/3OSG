#!/usr/bin/env python3
"""
Targeted test for LENGTH_MISMATCH issues
Analyzes specific problematic fields without modifying the main codebase
"""

import json
import os

def analyze_length_mismatch_fields():
    print('🔍 TARGETED LENGTH_MISMATCH ANALYSIS')
    print('=' * 50)
    
    # Load the problematic fields from parity results
    problem_fields = [
        'MB-BILL-NAME',
        'MB-BILL-LINE-5', 
        'MB-BILL-LINE-6',
        'MB-BILL-CITY',
        'MB-BILL-STATE',
        'MB-ZIP-5',
        'MB-ZIP-4',
        'MB-PROPERTY-STREET',
        'MB-PROPERTY-CITY',
        'MB-PROPERTY-STATE'
    ]
    
    print(f'Testing {len(problem_fields)} high-impact fields')
    print()
    
    # Load schema to get current offsets
    schema_dir = 'schemas/compiled'
    schema_file = '88f02f49240f0a0084aa1b6358740a32931b6c1e3012832d52432a3e60865a9d.schema.json'
    schema_path = os.path.join(schema_dir, schema_file)
    
    try:
        with open(schema_path, 'r') as f:
            schema = json.load(f)
        
        container = schema.get('Container4000', {})
        fields = container.get('Fields', [])
        
        # Find our problem fields in schema
        problem_field_info = {}
        for field in fields:
            if field['Name'] in problem_fields:
                problem_field_info[field['Name']] = {
                    'offset': field['Offset'],
                    'length': field['Length'],
                    'type': field.get('Type', 'unknown')
                }
        
        print('📊 SCHEMA ANALYSIS:')
        print('=' * 20)
        for field_name in problem_fields:
            if field_name in problem_field_info:
                info = problem_field_info[field_name]
                print(f'{field_name}:')
                print(f'  Schema offset: {info["offset"]}')
                print(f'  Schema length: {info["length"]}')
                print(f'  Type: {info["type"]}')
            else:
                print(f'{field_name}: NOT FOUND in schema')
        print()
        
        # Load actual output file to see what's at these offsets
        try:
            with open('out/69172/69172p.set', 'rb') as f:
                my_record = f.read(2000)
            
            print('🔍 ACTUAL DATA AT SCHEMA OFFSETS:')
            print('=' * 40)
            
            for field_name in problem_fields:
                if field_name in problem_field_info:
                    info = problem_field_info[field_name]
                    offset = info['offset']
                    length = info['length']
                    
                    if offset + length <= len(my_record):
                        data = my_record[offset:offset + length]
                        hex_data = ' '.join(f'{b:02X}' for b in data)
                        ascii_data = ''.join(chr(b) if 32 <= b <= 126 else '?' for b in data)
                        
                        print(f'{field_name} (offset {offset}, length {length}):')
                        print(f'  Hex: {hex_data}')
                        print(f'  ASCII: "{ascii_data}"')
                        
                        # Analyze the data
                        if all(b == 0x00 for b in data):
                            print(f'  → All null bytes (empty)')
                        elif all(b == 0x20 for b in data):
                            print(f'  → All ASCII spaces')
                        elif all(b == 0x40 for b in data):
                            print(f'  → All EBCDIC spaces')
                        elif ascii_data.strip():
                            print(f'  → Contains text data: "{ascii_data.strip()}"')
                        else:
                            print(f'  → Binary/corrupted data')
                        print()
                    else:
                        print(f'{field_name}: Offset {offset} beyond file length')
            
            # Load expected output for comparison
            try:
                with open('Expected_Outputs/69172/69172p.set', 'rb') as f:
                    expected_record = f.read(2000)
                
                print('🎯 EXPECTED VS ACTUAL COMPARISON:')
                print('=' * 40)
                
                for field_name in problem_fields[:5]:  # Test first 5 fields
                    if field_name in problem_field_info:
                        info = problem_field_info[field_name]
                        offset = info['offset']
                        length = info['length']
                        
                        if offset + length <= len(expected_record) and offset + length <= len(my_record):
                            exp_data = expected_record[offset:offset + length]
                            my_data = my_record[offset:offset + length]
                            
                            exp_ascii = ''.join(chr(b) if 32 <= b <= 126 else '?' for b in exp_data)
                            my_ascii = ''.join(chr(b) if 32 <= b <= 126 else '?' for b in my_data)
                            
                            print(f'{field_name}:')
                            print(f'  Expected: "{exp_ascii}"')
                            print(f'  Actual:   "{my_ascii}"')
                            
                            if exp_data == my_data:
                                print(f'  ✅ MATCH - schema offset is correct')
                            else:
                                print(f'  ❌ MISMATCH - schema offset might be wrong')
                                
                                # Try to find the expected data elsewhere
                                if exp_ascii.strip():
                                    search_text = exp_ascii.strip().encode('ascii')
                                    pos = my_record.find(search_text)
                                    if pos != -1:
                                        print(f'  💡 Expected data found at offset {pos} (diff: {pos - offset:+d})')
                            print()
                
            except FileNotFoundError:
                print('❌ Expected output file not found')
                
        except FileNotFoundError:
            print('❌ Actual output file not found')
            
    except FileNotFoundError:
        print('❌ Schema file not found')
    except Exception as e:
        print(f'❌ Error: {e}')
    
    print('🚀 TARGETED TEST CONCLUSIONS:')
    print('=' * 30)
    print('1. If fields show empty/null data → Schema offset wrong')
    print('2. If expected data found elsewhere → Calculate offset correction')
    print('3. If data corrupted with ? → EBCDIC conversion issue')
    print('4. Apply targeted fixes only to identified problem fields')

if __name__ == '__main__':
    analyze_length_mismatch_fields()
