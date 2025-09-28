#!/usr/bin/env python3
import csv
import json
import argparse
from pathlib import Path

def load_diffs(csv_path):
    diffs = {}
    with open(csv_path, newline='') as f:
        reader = csv.DictReader(f)
        for row in reader:
            field = row['Field']
            new_offset = int(row['NewOffset'])
            new_length = int(row['NewLen'])
            diffs[field] = (new_offset, new_length)
    return diffs

def load_fielddefs(json_path):
    data = json.loads(Path(json_path).read_text())
    mapping = {}
    # assume top-level recordLayouts.MBLPS.fields
    rec = data.get('recordLayouts', {}).get('MBLPS', {})
    for fld in rec.get('fields', []):
        name = fld['name'].replace('_', '-')
        mapping[name] = (fld['startPosition'], fld['length'])
    return mapping

def patch_overrides(overrides_path, diffs, fielddefs=None):
    text = Path(overrides_path).read_text()
    data = json.loads(text)
    for entry in data.get('overrides', []):
        src = entry.get('source')
        # fielddefs override
        if fielddefs and src in fielddefs:
            off, ln = fielddefs[src]
            entry['sourceOffset'] = off
            entry['sourceLength'] = ln
        elif src in diffs:
            new_offset, new_length = diffs[src]
            entry['sourceOffset'] = new_offset
            entry['sourceLength'] = new_length
    # Backup
    backup = Path(overrides_path).with_suffix('.bak.json')
    Path(overrides_path).replace(str(backup))
    Path(overrides_path).write_text(json.dumps(data, indent=2))
    count = len([e for e in data.get('overrides', []) if (fielddefs and e['source'] in fielddefs) or e['source'] in diffs])
    print(f"Patched {count} entries; backup at {backup}")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Apply offset diffs or field definitions to overrides JSON')
    parser.add_argument('-c', '--csv', help='CSV file with Field,NewOffset,NewLen')
    parser.add_argument('-f', '--fielddefs', help='FieldDefinitions_Generated.json for authoritative offsets')
    parser.add_argument('-o', '--overrides', required=True, help='Overrides JSON to patch')
    args = parser.parse_args()
    diffs = load_diffs(args.csv) if args.csv else {}
    fielddefs = load_fielddefs(args.fielddefs) if args.fielddefs else None
    patch_overrides(args.overrides, diffs, fielddefs)
