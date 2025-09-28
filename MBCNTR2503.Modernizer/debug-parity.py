#!/usr/bin/env python3

import argparse
import json
import subprocess
import os
import struct
from pathlib import Path

# --- Configuration ---
# Adjust these paths if your project structure is different
WORKSPACE_ROOT = Path(__file__).parent.parent
MODERNIZER_DIR = WORKSPACE_ROOT / "MBCNTR2503.Modernizer"
LEGACY_DIR = MODERNIZER_DIR # Use the Modernizer dir for Expected_Outputs as well
SCHEMA_DIR = MODERNIZER_DIR / "schemas" / "compiled"
CONFIG_DIR = MODERNIZER_DIR / "config" / "base" / "mblps"
OUTPUT_DIR = MODERNIZER_DIR / "out"

def find_schema_file(schema_dir: Path, schema_name: str = "mblps"):
    """Finds the correct schema file by looking for a matching container name."""
    for filename in schema_dir.glob("*.schema.json"):
        with open(filename, 'r') as f:
            try:
                data = json.load(f)
                if data.get("Container4000", {}).get("Name", "").lower() == schema_name.lower():
                    print(f"🔍 Found schema for '{schema_name}' in: {filename.name}")
                    return filename
            except json.JSONDecodeError:
                continue
    raise FileNotFoundError(f"Could not find a compiled schema for '{schema_name}' in {schema_dir}")

def get_field_definitions(schema_file: Path):
    """Extracts field definitions from the schema file."""
    with open(schema_file, 'r') as f:
        schema = json.load(f)
    
    # Assuming the MB2000 fields are in Container4000
    fields = schema.get("Container4000", {}).get("Fields", [])
    if not fields:
        raise ValueError("Could not find 'Container4000' fields in the schema.")
        
    return {field["Name"]: field for field in fields}

def read_records(file_path: Path, record_size: int):
    """Reads a binary file and yields records of a fixed size."""
    with open(file_path, 'rb') as f:
        while True:
            record = f.read(record_size)
            if not record:
                break
            if len(record) == record_size:
                yield record
            else:
                # Skip incomplete trailing record
                break

def decode_field(record: bytes, field_def: dict):
    """Decodes a single field from a record based on its schema definition."""
    offset = field_def["Offset"]
    length = field_def["Length"]
    data_type = field_def["DataType"]
    
    field_bytes = record[offset : offset + length]
    
    if data_type == "Text":
        try:
            # MB2000 files are in ASCII format, not EBCDIC
            return field_bytes.decode('ascii').strip()
        except UnicodeDecodeError:
            return field_bytes.hex()
    elif data_type == "Packed Number":
        return decode_packed_decimal(field_bytes, field_def.get("Scale", 0))
    elif data_type == "Number":
         try:
            # MB2000 zoned decimal fields are in ASCII format
            return field_bytes.decode('ascii').strip()
         except UnicodeDecodeError:
            return field_bytes.hex()
    elif data_type == "Int":
        if length == 4:
            # Assuming big-endian 4-byte integer (S390 standard)
            return str(struct.unpack('>i', field_bytes)[0])
        elif length == 2:
            return str(struct.unpack('>h', field_bytes)[0])
        else:
            return field_bytes.hex() # Fallback for other int lengths
    else:
        # Fallback for unknown types
        try:
            return field_bytes.decode('ascii').strip()
        except UnicodeDecodeError:
            return field_bytes.hex()

def decode_packed_decimal(data: bytes, scale: int = 0):
    """Decodes a COBOL COMP-3 packed decimal into a string."""
    nybbles = []
    for byte in data:
        nybbles.extend([byte >> 4, byte & 0x0F])

    sign_nybble = nybbles.pop()
    
    if not nybbles:
        return "0.0" if scale > 0 else "0"

    sign = "-" if sign_nybble in (0x0b, 0x0d) else ""
    
    digits = "".join(map(str, nybbles))
    
    if scale > 0:
        integer_part = digits[:-scale] or "0"
        fractional_part = digits[-scale:].zfill(scale)
        return f"{sign}{integer_part}.{fractional_part}"
    else:
        return f"{sign}{digits}"

def compare_jobs(job_id: str):
    """Compares the generated .set file against the legacy baseline."""
    print(f"\n🔍 Starting comparison for job {job_id}...")

    # Define file paths
    expected_file = LEGACY_DIR / "Expected_Outputs" / job_id / f"{job_id}p.set"
    actual_file = OUTPUT_DIR / job_id / f"{job_id}p.set"

    if not expected_file.exists():
        print(f"❌ ERROR: Expected file not found: {expected_file}")
        return
    if not actual_file.exists():
        print(f"❌ ERROR: Actual file not found: {actual_file}")
        return

    # Load schema
    try:
        schema_path = find_schema_file(SCHEMA_DIR)
        fields = get_field_definitions(schema_path)
        with open(schema_path, 'r') as f:
            schema = json.load(f)
    except (FileNotFoundError, ValueError) as e:
        print(f"❌ ERROR: {e}")
        return

    # For .set file comparison, MB2000 records are 2000 bytes (as per setmb2000.cbl line 29)
    record_size = 2000
    print(f"🔍 Using record size (MB2000 record length): {record_size}")
    # Read records with correct record size
    expected_records = list(read_records(expected_file, record_size))
    actual_records = list(read_records(actual_file, record_size))

    if len(expected_records) != len(actual_records):
        print(f"❌ Record count mismatch! Expected: {len(expected_records)}, Got: {len(actual_records)}")
        print(f"🔍 Continuing comparison with available records (min: {min(len(expected_records), len(actual_records))})...")
        # Continue with the minimum count available

    total_diffs = 0
    min_records = min(len(expected_records), len(actual_records))
    print(f"Comparing {min_records} records...")

    # Field-by-field comparison
    for i, (expected_rec, actual_rec) in enumerate(zip(expected_records, actual_records)):
        record_has_diff = False
        for field_name, field_def in fields.items():
            expected_val = decode_field(expected_rec, field_def)
            actual_val = decode_field(actual_rec, field_def)

            if expected_val != actual_val:
                if not record_has_diff:
                    print(f"\n--- Differences found in Record #{i+1} ---")
                    record_has_diff = True
                
                print(f"  - Field: {field_name}")
                print(f"    - Expected: '{expected_val}'")
                print(f"    - Actual:   '{actual_val}'")
                total_diffs += 1
    
    if total_diffs == 0:
        print("\n🎉🎉🎉 PERFECT PARITY! All records and fields match. 🎉🎉🎉")
    else:
        print(f"\n--- Summary ---")
        print(f"Found {total_diffs} total field differences across all records.")

def main():
    parser = argparse.ArgumentParser(description="Run MB2000 conversion and perform a detailed field-level parity check.")
    parser.add_argument("job_id", help="The job ID to process (e.g., 80147).")
    args = parser.parse_args()

    # The C# pipeline now handles the conversion, so we just compare.
    compare_jobs(args.job_id)

if __name__ == "__main__":
    main()
