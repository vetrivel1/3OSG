#!/usr/bin/env python3
import json
import os

# Paths
solution_root = os.getcwd()
project_root = os.path.join(solution_root, 'MBCNTR2503.Modernizer')
job = '69172'
overrides_file = os.path.join(project_root, 'config', 'base', 'mblps', 'mb2000.overrides.json')
expected_file = os.path.join(solution_root, 'Legacy Application', 'Expected_Outputs', job, f'{job}p.set')
actual_file = os.path.join(project_root, 'out', job, f'{job}p.set')

# Load config
with open(overrides_file, 'r') as f:
    cfg = json.load(f)
mb2o = cfg['mb2000Conversion']['fieldPositions']['mb2000Output']
if 'MB-BILL-LINE-6' not in mb2o:
    print('Field MB-BILL-LINE-6 not found in config')
    exit(1)
field = mb2o['MB-BILL-LINE-6']
offset = field['offset']
length = field['length']

# Read files
exp = open(expected_file, 'rb').read()
act = open(actual_file, 'rb').read()

# Extract slices
exp_slice = exp[offset:offset+length]
act_slice = act[offset:offset+length]

# Print hex and ASCII
def to_hex(b): return ' '.join(f'{x:02X}' for x in b)
print('Expected (hex):', to_hex(exp_slice))
print('Actual   (hex):', to_hex(act_slice))
print('Expected ASCII:', exp_slice.decode('ascii', errors='replace'))
print('Actual   ASCII:', act_slice.decode('ascii', errors='replace'))
