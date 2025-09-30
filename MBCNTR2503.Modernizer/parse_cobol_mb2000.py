#!/usr/bin/env python3
"""
Parse COBOL mb2000.cbl copybook and generate correct DD file with output offsets
"""

import re
from pathlib import Path

def calculate_cobol_field_size(pic_clause):
    """
    Calculate field size in bytes from COBOL PIC clause
    
    Examples:
    - PIC 9(3) -> 3 bytes
    - PIC X(60) -> 60 bytes
    - PIC S9(11)V99 COMP-3 -> 7 bytes (packed decimal: (11+2+1)/2 rounded up)
    - PIC S9(9)V99 COMP-3 -> 6 bytes (packed decimal: (9+2+1)/2 rounded up)
    - PIC 9(2) -> 2 bytes
    - PIC XX -> 2 bytes
    """
    pic = pic_clause.upper().strip()
    
    # Check for COMP-3 (packed decimal)
    is_comp3 = 'COMP-3' in pic or 'COMPUTATIONAL-3' in pic
    
    # Remove COMP-3 from pic for parsing
    pic = pic.replace('COMP-3', '').replace('COMPUTATIONAL-3', '').replace('COMP', '').strip()
    
    # Extract sign and decimal info
    has_sign = pic.startswith('S')
    if has_sign:
        pic = pic[1:]  # Remove S
    
    # Split on V (implied decimal point)
    if 'V' in pic:
        parts = pic.split('V')
        integer_part = parts[0]
        decimal_part = parts[1] if len(parts) > 1 else ''
    else:
        integer_part = pic
        decimal_part = ''
    
    # Calculate total digits
    total_digits = 0
    
    # Parse integer part (e.g., "9(11)" or "999" or "X(60)")
    integer_match = re.search(r'([9X])\((\d+)\)', integer_part)
    if integer_match:
        total_digits += int(integer_match.group(2))
    else:
        # Count consecutive 9s or Xs
        total_digits += len(re.findall(r'[9X]', integer_part))
    
    # Parse decimal part if present
    if decimal_part:
        decimal_match = re.search(r'9\((\d+)\)', decimal_part)
        if decimal_match:
            total_digits += int(decimal_match.group(1))
        else:
            total_digits += len(re.findall(r'9', decimal_part))
    
    # Calculate final size
    if is_comp3:
        # COMP-3 (packed decimal): (digits + 1) / 2, rounded up
        # +1 for the sign nibble
        return (total_digits + 1 + 1) // 2
    else:
        # Regular display format
        return total_digits

def parse_cobol_copybook(copybook_path):
    """Parse COBOL copybook and calculate field offsets"""
    fields = []
    current_offset = 0
    level_stack = [(1, 0, None)]  # (level, offset, redefines_base_offset) stack
    in_redefines = False
    redefines_base_offset = None
    redefines_max_offset = None
    
    with open(copybook_path, 'r') as f:
        lines = f.readlines()
    
    for line_num, line in enumerate(lines, 1):
        # Skip comments and blank lines
        if line.strip().startswith('*') or not line.strip():
            continue
        
        # Check for level number at start of field definition
        match = re.match(r'\s+(\d+)\s+([A-Z0-9-]+)(\s+.*)?', line)
        if not match:
            continue
        
        level = int(match.group(1))
        field_name = match.group(2)
        rest_of_line = match.group(3) if match.group(3) else ''
        
        # Check for REDEFINES
        redefines_match = re.search(r'REDEFINES\s+([A-Z0-9-]+)', rest_of_line, re.IGNORECASE)
        if redefines_match:
            # Find the field being redefined and use its offset
            redefined_field = redefines_match.group(1)
            for f in fields:
                if f['name'] == redefined_field:
                    in_redefines = True
                    redefines_base_offset = f['offset']
                    redefines_max_offset = f['offset']
                    current_offset = redefines_base_offset
                    break
            # Don't add the REDEFINES field itself, only its children
            continue
        
        # Check if we're exiting a REDEFINES section (level drops to base level or below)
        if in_redefines and level <= 5:  # MB-CLIENT-FIELDS level
            # Advance current_offset to the max reached in REDEFINES
            current_offset = redefines_max_offset
            in_redefines = False
            redefines_base_offset = None
            redefines_max_offset = None
        
        # Skip FILLER fields (unnamed) but account for their space
        if field_name == 'FILLER':
            pic_match = re.search(r'PIC\s+([^\s.]+)', rest_of_line, re.IGNORECASE)
            if pic_match:
                pic_clause = pic_match.group(1)
                if 'COMP' in rest_of_line:
                    pic_clause += ' COMP-3'
                field_size = calculate_cobol_field_size(pic_clause)
                
                # Advance offset (even in REDEFINES, track max)
                if not in_redefines:
                    current_offset += field_size
                else:
                    current_offset += field_size
                    redefines_max_offset = max(redefines_max_offset, current_offset)
            continue
        
        # Check for PIC clause
        pic_match = re.search(r'PIC\s+([^\s.]+)', rest_of_line, re.IGNORECASE)
        if pic_match:
            pic_clause = pic_match.group(1)
            
            # Check for COMP-3 on the same or next line
            if 'COMP' in rest_of_line or (line_num < len(lines) and 'COMP' in lines[line_num]):
                pic_clause += ' COMP-3'
            
            field_size = calculate_cobol_field_size(pic_clause)
            
            # Determine field type
            if 'COMP-3' in pic_clause or 'COMP' in rest_of_line:
                field_type = 'Packed Number'
            elif '9' in pic_clause:
                field_type = 'Number'
            else:
                field_type = 'Text'
            
            # Determine decimal places for packed fields
            decimal_places = 0
            if 'V' in pic_clause:
                decimal_match = re.search(r'V9+\((\d+)\)|V(9+)', pic_clause)
                if decimal_match:
                    if decimal_match.group(1):
                        decimal_places = int(decimal_match.group(1))
                    else:
                        decimal_places = len(decimal_match.group(2))
            
            fields.append({
                'name': field_name,
                'offset': current_offset,
                'length': field_size,
                'type': field_type,
                'decimal_places': decimal_places,
                'level': level,
                'in_redefines': in_redefines
            })
            
            # Advance offset
            if not in_redefines:
                current_offset += field_size
            else:
                current_offset += field_size
                redefines_max_offset = max(redefines_max_offset, current_offset)
    
    return fields

def generate_dd_file(fields, output_path):
    """Generate DD file format from parsed fields"""
    with open(output_path, 'w') as f:
        for field in fields:
            # Only output leaf fields (those with actual data)
            line = f"{field['name']}, {field['offset']}, {field['length']}, {field['type']}, {field['decimal_places']}\n"
            f.write(line)
    print(f"✅ Generated DD file with {len(fields)} fields at: {output_path}")

def main():
    copybook_path = Path('Legacy Application/Scripts/MBCNTR2503/Original Programs+Scripts/mb2000.cbl')
    output_path = Path('MBCNTR2503.Modernizer/config/base/mblps/mb2000-output.dd')
    
    if not copybook_path.exists():
        print(f"❌ Copybook not found: {copybook_path}")
        return False
    
    print(f"🔍 Parsing COBOL copybook: {copybook_path}")
    fields = parse_cobol_copybook(copybook_path)
    
    if not fields:
        print("❌ No fields parsed from copybook")
        return False
    
    print(f"✅ Parsed {len(fields)} fields from copybook")
    
    # Show first 10 fields for verification
    print("\n📊 First 10 fields:")
    for field in fields[:10]:
        print(f"   {field['name']:<30} offset={field['offset']:4d}  length={field['length']:3d}  type={field['type']}")
    
    # Generate DD file
    generate_dd_file(fields, output_path)
    
    # Show total record size
    if fields:
        last_field = max(fields, key=lambda f: f['offset'] + f['length'])
        total_size = last_field['offset'] + last_field['length']
        print(f"\n📏 Total record size: {total_size} bytes")
        if total_size != 2000:
            print(f"⚠️  WARNING: Expected 2000 bytes, got {total_size} bytes")
    
    return True

if __name__ == "__main__":
    success = main()
    if not success:
        exit(1)
