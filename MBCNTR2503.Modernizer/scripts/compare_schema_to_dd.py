#!/usr/bin/env python3
import json
import argparse

def load_dd(path):
    dd = {}
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 4:
                continue
            name, offset, length, dtype = parts[:4]
            dd[name] = dtype
    return dd


def flatten_schema(props, prefix=None, out=None):
    if out is None:
        out = {}
    for k, v in props.items():
        # record this field (object or leaf)
        out[k] = v.get('type')
        # if object, recurse into children
        if v.get('type') == 'object' and 'properties' in v:
            flatten_schema(v['properties'], k if prefix is None else f"{prefix}.{k}", out)
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--schema', required=True)
    parser.add_argument('-d', '--dd', required=True)
    args = parser.parse_args()

    schema = json.load(open(args.schema))
    props = schema.get('properties', {})
    flat = flatten_schema(props)
    dd = load_dd(args.dd)

    missing_in_dd = [f for f in flat if f not in dd]
    missing_in_schema = [f for f in dd if f not in flat]
    type_mismatch = []
    for f, jtype in flat.items():
        if f in dd:
            dtype = dd[f].lower()
            # map json number-> number or packed; string-> text
            if jtype == 'number' and 'text' in dtype:
                type_mismatch.append((f, dtype, jtype))
            if jtype == 'string' and not 'text' in dtype:
                type_mismatch.append((f, dtype, jtype))
    print("Fields in schema not in DD:", missing_in_dd)
    print("Fields in DD not in schema:", missing_in_schema)
    print("Type mismatches:")
    for f, dtype, jtype in type_mismatch:
        print(f, dtype, jtype)

if __name__ == '__main__':
    main()
