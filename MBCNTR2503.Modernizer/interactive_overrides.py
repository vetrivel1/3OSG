#!/usr/bin/env python3
import json
import shutil
import subprocess
import sys
from pathlib import Path
import shlex  # Use shell-like splitting to handle quoted paths

def load_overrides(path):
    with open(path, 'r') as f:
        return json.load(f)

def save_overrides(data, path):
    with open(path, 'w') as f:
        json.dump(data, f, indent=2)

def list_fields(overrides):
    for entry in overrides.get('overrides', []):
        print(f"{entry['target']}: source={entry['source']}, offset={entry.get('sourceOffset')}, length={entry.get('sourceLength')}")

def set_field(overrides, target, offset, length):
    for entry in overrides.get('overrides', []):
        if entry['target'] == target:
            entry['sourceOffset'] = offset
            entry['sourceLength'] = length
            print(f"Updated {target}")
            return
    print(f"Field {target} not found")

def run_conversion(job, keyed, outdir, schema_dir):
    cmd = [
        'dotnet', 'run', '--project', 'src/Cnp.Cli', '--',
        'mb2000-convert',
        '--job', job,
        '--input', keyed,
        '--out', outdir,
        '--schema', schema_dir
    ]
    subprocess.run(cmd, check=True)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Interactive overrides editor')
    parser.add_argument('--overrides', '-o', required=True, help='Path to overrides JSON file')
    parser.add_argument('--schema-dir', '-s', default=None, help='Schema directory for test commands')
    args = parser.parse_args()
    overrides_path = Path(args.overrides)
    if not overrides_path.exists():
        print(f'Override file not found: {overrides_path}')
        sys.exit(1)
    # Determine schema dir: either provided or parent schema directory
    if args.schema_dir:
        config_dir = Path(args.schema_dir)
    else:
        config_dir = overrides_path.parent
    overrides = load_overrides(overrides_path)
    print('Interactive MB2000 Overrides Editor')
    print('Commands: list, set <field> <offset> <length>, save, test <job> <keyed> <outdir>, exit')
    while True:
        try:
            line = input('> ').strip()
        except EOFError:
            break
        if not line:
            continue
        parts = shlex.split(line)
        cmd = parts[0]
        if cmd == 'list':
            list_fields(overrides)
        elif cmd == 'set' and len(parts) == 4:
            set_field(overrides, parts[1], int(parts[2]), int(parts[3]))
        elif cmd == 'save':
            save_overrides(overrides, overrides_path)
            print(f'Saved overrides to {overrides_path}')
        elif cmd == 'test' and len(parts) >= 2:
            job = parts[1]
            keyed = parts[2] if len(parts) > 2 else None
            outdir = parts[3] if len(parts) > 3 else None
            run_conversion(job, keyed or '', outdir or '', str(config_dir))
        elif cmd == 'exit':
            break
        else:
            print('Unknown command or wrong args')
