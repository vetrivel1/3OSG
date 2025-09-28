#!/usr/bin/env python3
import re
import argparse

def parse_copybook_pic(path):
    pic_pattern = re.compile(r'^\s*\d+\s+([A-Z0-9-]+)(?:\s+REDEFINES\s+[A-Z0-9-]+)?\s+PIC\s+([^\.]+)\.')
    pics = {}
    with open(path) as f:
        for line in f:
            m = pic_pattern.match(line)
            if m:
                name = m.group(1)
                pic = m.group(2).strip().upper()
                pics[name] = pic
    return pics


def load_dd(path):
    dd = {}
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('*'):
                continue
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 4:
                continue
            name, offset, length, dtype = parts[:4]
            dd[name] = dtype
    return dd


def classify_pic(pic):
    if pic.startswith('X'):
        return 'string'
    if 'COMP-3' in pic or 'V' in pic or pic.startswith('S9') or '9' in pic:
        return 'number'
    return 'string'


def classify_dd(dtype):
    dt = dtype.lower()
    if 'packed' in dt or 'comp-3' in dt or 'number' in dt:
        return 'number'
    return 'string'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--copybook', required=True)
    parser.add_argument('-d', '--dd', required=True)
    args = parser.parse_args()

    pics = parse_copybook_pic(args.copybook)
    dd = load_dd(args.dd)

    mismatches = []
    for name, pic in pics.items():
        ctype = classify_pic(pic)
        if name in dd:
            dtype = classify_dd(dd[name])
            if ctype != dtype:
                mismatches.append((name, pic, dd[name], ctype, dtype))

    print('Type mismatches between copybook and DD:')
    for name, pic, ddtype, ctype, dtype in mismatches:
        print(f"{name}: PIC='{pic}' -> {ctype}, DD='{ddtype}' -> {dtype}")
    print(f"Total mismatches: {len(mismatches)}")

if __name__ == '__main__':
    main()
