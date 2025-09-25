#!/usr/bin/env python3
import os
import sys
import argparse
from collections import defaultdict, Counter
from datetime import datetime


def read_bytes(path: str) -> bytes:
    with open(path, 'rb') as f:
        return f.read()


def parse_ddcontrol(path: str):
    entries = []
    in_list = False
    with open(path, 'r', encoding='utf-8', errors='ignore') as f:
        for raw in f:
            line = raw.strip()
            if not line:
                continue
            if line.startswith('DDLIST'):
                in_list = True
                continue
            if not in_list:
                continue
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 6:
                continue
            name, rec_id, rec_off, container, ddnum, typ = parts[:6]
            try:
                rec_off_i = int(rec_off)
            except Exception:
                rec_off_i = -1
            entries.append({
                'name': name,
                'rec_id': rec_id,
                'rec_off': rec_off_i,
                'container': container,
                'ddnum': ddnum,
                'type': typ
            })
    return entries


def parse_dd_file(path: str):
    fields = []
    if not os.path.exists(path):
        return fields
    with open(path, 'r', encoding='utf-8', errors='ignore') as f:
        for raw in f:
            line = raw.strip()
            if not line or line.startswith('#'):
                continue
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 4:
                continue
            name = parts[0]
            try:
                off = int(parts[1])
                length = int(parts[2])
            except Exception:
                continue
            dtype = parts[3]
            scale = 0
            if len(parts) >= 5:
                try:
                    scale = int(parts[4])
                except Exception:
                    scale = 0
            fields.append({'name': name, 'offset': off, 'length': length, 'dtype': dtype, 'scale': scale})
    return fields


def match_dd_for_record(dd_entries, input_dat_record: bytes):
    # Use same rules as orchestrator: prefer explicit char id; handle HEX_; keep ALL_OTHER_RECORDS as fallback
    fallback = None
    record_len = len(input_dat_record)
    for d in dd_entries:
        rec_id = d['rec_id']
        rec_off = d['rec_off']
        if rec_id.upper() == 'ALL_OTHER_RECORDS':
            if fallback is None:
                fallback = d
            continue
        if rec_id.upper().startswith('HEX_'):
            if 0 <= rec_off < record_len:
                hexpart = rec_id[4:]
                try:
                    target = int(hexpart, 16)
                except Exception:
                    continue
                if input_dat_record[rec_off] == target:
                    return d
            continue
        # single char via EBCDIC; record-id like 'P','A','D','F','U','V','W','X','S','N'
        if len(rec_id) == 1 and 0 <= rec_off < record_len:
            # decode EBCDIC 037 for the byte at rec_off to ASCII char
            b = input_dat_record[rec_off]
            # EBCDIC 0xF0-0xF9 => '0'-'9'; 0xC1..0xE9 map to 'A'..'Z'
            e2a_map = {
                0xC1:'A',0xC2:'B',0xC3:'C',0xC4:'D',0xC5:'E',0xC6:'F',0xC7:'G',0xC8:'H',0xC9:'I',
                0xD1:'J',0xD2:'K',0xD3:'L',0xD4:'M',0xD5:'N',0xD6:'O',0xD7:'P',0xD8:'Q',0xD9:'R',
                0xE2:'S',0xE3:'T',0xE4:'U',0xE5:'V',0xE6:'W',0xE7:'X',0xE8:'Y',0xE9:'Z',
                0xF0:'0',0xF1:'1',0xF2:'2',0xF3:'3',0xF4:'4',0xF5:'5',0xF6:'6',0xF7:'7',0xF8:'8',0xF9:'9'
            }
            ch = e2a_map.get(b, '?')
            if ch == rec_id:
                return d
    return fallback


def aggregate_field_diffs(expected: bytes, actual: bytes, input_dat: bytes, dd_entries, dd_dir: str, debug=False, debug_limit=5):
    assert len(expected) == len(actual)
    record_size = 4300
    data_size = 4000
    total_bytes = len(expected)
    total_diffs = 0
    per_field = Counter()
    per_record_type = Counter()
    per_record_type_diffs = Counter()
    field_offset_samples = defaultdict(list)  # field -> list of (record_index, offset)
    debug_samples = []

    num_records = total_bytes // record_size
    for r in range(num_records):
        exp_rec = expected[r*record_size:(r+1)*record_size]
        act_rec = actual[r*record_size:(r+1)*record_size]
        in_rec = input_dat[r*data_size:(r+1)*data_size]

        # Determine dd
        dd = match_dd_for_record(dd_entries, in_rec)
        dd_name = dd['name'] if dd else 'UNKNOWN'
        per_record_type[dd_name] += 1

        # Load fields
        fields = parse_dd_file(os.path.join(dd_dir, dd_name)) if dd else []

        # Build intervals: list of (name, start, end)
        intervals = []
        for f in fields:
            off = f['offset']
            length = f['length']
            if off < 0 or off + length > data_size:
                continue
            intervals.append((f['name'], off, off+length))
        # Ensure coverage; add a catch-all for uncovered bytes in 0..4000
        intervals.append(('UNMAPPED', 0, 0))  # marker
        # Trailer bucket
        intervals.append(('TRAILER', data_size, record_size))

        # Compute diffs per field
        # We'll iterate through 0..4000 and attribute to the first matching interval, default UNMAPPED
        # Build a quick lookup mask for fields
        mask = ['UNMAPPED'] * data_size
        for name, s, e in intervals:
            if name in ('UNMAPPED','TRAILER'):
                continue
            for i in range(max(0, s), min(data_size, e)):
                mask[i] = name

        # First 4000
        for i in range(data_size):
            if exp_rec[i] != act_rec[i]:
                total_diffs += 1
                fname = mask[i]
                per_field[fname] += 1
                per_record_type_diffs[dd_name] += 1
                if len(field_offset_samples[fname]) < 200:
                    field_offset_samples[fname].append((r, i))
                if debug and len(debug_samples) < debug_limit:
                    debug_samples.append((dd_name, fname, r, i, exp_rec[i], act_rec[i]))

        # Trailer
        for i in range(data_size, record_size):
            if exp_rec[i] != act_rec[i]:
                total_diffs += 1
                per_field['TRAILER'] += 1
                per_record_type_diffs[dd_name] += 1
                if len(field_offset_samples['TRAILER']) < 200:
                    field_offset_samples['TRAILER'].append((r, i))
                if debug and len(debug_samples) < debug_limit:
                    debug_samples.append((dd_name, 'TRAILER', r, i, exp_rec[i], act_rec[i]))

    total_matches = total_bytes - total_diffs
    return {
        'total_bytes': total_bytes,
        'total_diffs': total_diffs,
        'total_matches': total_matches,
        'per_field': per_field,
        'per_record_type': per_record_type,
        'per_record_type_diffs': per_record_type_diffs,
        'field_offset_samples': field_offset_samples,
        'debug_samples': debug_samples
    }


def main():
    parser = argparse.ArgumentParser(description='Binary .4300 comparator with per-field aggregation')
    parser.add_argument('--expected', required=True, help='Path to expected legacy .4300 file')
    parser.add_argument('--actual', required=True, help='Path to actual generated .4300 file')
    parser.add_argument('--input-dat', required=True, help='Path to source input .dat (EBCDIC)')
    parser.add_argument('--schema-dir', required=False, default=None, help='Path to schema dir containing ddcontrol.txt and dd files')
    parser.add_argument('--top', type=int, default=25, help='Top N fields to print by diff count')
    parser.add_argument('--debug', action='store_true', help='Print debug samples')
    parser.add_argument('--debug-limit', type=int, default=5, help='Debug sample count')

    args = parser.parse_args()

    expected = read_bytes(args.expected)
    actual = read_bytes(args.actual)
    if len(expected) != len(actual):
        print(f"ERROR: files differ in length: expected={len(expected)} actual={len(actual)}")
        sys.exit(2)

    input_dat = read_bytes(args.input_dat)
    # Default schema dir
    schema_dir = args.schema_dir or os.path.join(os.path.dirname(os.path.dirname(__file__)), 'config', 'base', 'mblps')
    ddcontrol_path = os.path.join(schema_dir, 'ddcontrol.txt')
    dd_entries = parse_ddcontrol(ddcontrol_path)

    results = aggregate_field_diffs(expected, actual, input_dat, dd_entries, schema_dir, debug=args.debug, debug_limit=args.debug_limit)
    total = results['total_bytes']
    diffs = results['total_diffs']
    matches = results['total_matches']
    pct_match = (matches / total) * 100.0 if total else 0.0
    pct_diff = (diffs / total) * 100.0 if total else 0.0

    print(f"Total bytes: {total}")
    print(f"Matches: {matches} ({pct_match:.2f}%)")
    print(f"Mismatches: {diffs} ({pct_diff:.2f}%)")

    # Top fields
    print("\nTop differing fields:")
    for name, count in results['per_field'].most_common(args.top):
        print(f"  {name}: {count}")

    # Per-record-type breakdown
    print("\nRecord type breakdown (records, diffs):")
    for rt, cnt in results['per_record_type'].items():
        rd = results['per_record_type_diffs'].get(rt, 0)
        print(f"  {rt}: {cnt} records, {rd} diffs")

    if args.debug and results['debug_samples']:
        print("\nDebug samples (first mismatches):")
        for dd_name, field, rec_idx, offset, exp_byte, act_byte in results['debug_samples']:
            print(f"  dd={dd_name} field={field} record={rec_idx} offset={offset} expected={exp_byte:02X} actual={act_byte:02X}")

    # Save report
    ts = datetime.now().strftime('%Y%m%d_%H%M%S')
    report_dir = os.path.join(os.path.dirname(__file__), 'reports')
    os.makedirs(report_dir, exist_ok=True)
    report_path = os.path.join(report_dir, f"4300_diff_report_{ts}.txt")
    with open(report_path, 'w', encoding='utf-8') as f:
        f.write(f"Total bytes: {total}\n")
        f.write(f"Matches: {matches} ({pct_match:.2f}%)\n")
        f.write(f"Mismatches: {diffs} ({pct_diff:.2f}%)\n\n")
        f.write("Top differing fields:\n")
        for name, count in results['per_field'].most_common(1000):
            f.write(f"  {name}: {count}\n")
        f.write("\nRecord type breakdown (records, diffs):\n")
        for rt, cnt in results['per_record_type'].items():
            rd = results['per_record_type_diffs'].get(rt, 0)
            f.write(f"  {rt}: {cnt} records, {rd} diffs\n")
        if results['debug_samples']:
            f.write("\nDebug samples (expected vs actual hex):\n")
            for dd_name, field, rec_idx, offset, exp_byte, act_byte in results['debug_samples']:
                f.write(f"  dd={dd_name} field={field} record={rec_idx} offset={offset} expected={exp_byte:02X} actual={act_byte:02X}\n")
        f.write("\nField offset samples (record_index, offset):\n")
        for name, samples in results['field_offset_samples'].items():
            f.write(f"  {name}: {samples}\n")
    print(f"\nReport saved to: {report_path}")

    return 0


if __name__ == '__main__':
    sys.exit(main())


