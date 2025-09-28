#!/usr/bin/env python3
import re
import sys
import argparse

def pic_length(pic):
    # Handles X(n), 9(n), S9(n)V9(m), COMP-3
    m = re.match(r"X\((\d+)\)", pic)
    if m: return int(m.group(1)), 'Text', 0
    m = re.match(r"9\((\d+)\)", pic)
    if m: return int(m.group(1)), 'Number', 0
    # Handle zoned/packed decimal without explicit scale
    m = re.match(r"S?9\((\d+)\)\s*COMP-3", pic, re.IGNORECASE)
    if m:
        digits = int(m.group(1))
        length = (digits + 2) // 2
        return length, 'Packed Number', 0
    m = re.match(r"S9\((\d+)\)V9\((\d+)\) COMP-3", pic, re.IGNORECASE)
    if m:
        # Packed decimal length in bytes
        digits = int(m.group(1)) + int(m.group(2))
        length = (digits + 2) // 2
        return length, 'Packed Number', int(m.group(2))
    # Fallback: assume 1
    return 1, 'Text', 0

def parse_copybook(path):
    # Flatten sequence ignoring group nesting except for REDEFINES
    pattern = re.compile(r'^\s*(\d+)\s+([A-Z0-9-]+)(?:\s+REDEFINES\s+([A-Z0-9-]+))?\s+PIC\s+([^\.]+)\.')
    entries = []
    offset_map = {}
    current_offset = 0
    with open(path) as f:
        for line in f:
            m = pattern.match(line)
            if not m: continue
            name, redef, pic = m.group(2), m.group(3), m.group(4).strip()
            length, dtype, scale = pic_length(pic)
            if redef and redef in offset_map:
                off = offset_map[redef]
            else:
                off = current_offset
            entries.append((name, off, length, dtype, scale))
            offset_map[name] = off
            if not redef:
                current_offset = off + length
    return entries

if __name__ == '__main__':
    p = argparse.ArgumentParser()
    p.add_argument('-i','--input', required=True)
    p.add_argument('-o','--output', required=True)
    args = p.parse_args()
    rows = parse_copybook(args.input)
    with open(args.output, 'w') as out:
        for name,off,len_,dtype,scale in rows:
            out.write(f"{name},{off},{len_},{dtype},{scale}\n")
    print(f"Generated DD CSV: {args.output}")
