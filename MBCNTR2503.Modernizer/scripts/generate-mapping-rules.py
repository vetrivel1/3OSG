#!/usr/bin/env python3
import json
import os

def main():
    overrides_path = 'MBCNTR2503.Modernizer/config/base/mblps/mb2000.overrides.json'
    with open(overrides_path, 'r') as f:
        data = json.load(f)

    fieldpos = data['mb2000Conversion']['fieldPositions']
    in_fields = fieldpos['mb1500Input']
    out_fields = fieldpos['mb2000Output']

    new_rules = []
    for input_name, defn in in_fields.items():
        if not input_name.startswith('MB1100-'):
            continue
        out_name = 'MB-' + input_name[len('MB1100-'):]
        if out_name in out_fields:
            new_rules.append({
                'source': input_name,
                'target': out_name,
                'mode': 'copyTrim',
                'trimOutput': True
            })

    # Apply to all clients with cobolFieldMapping
    for client_key, client_cfg in data['mb2000Conversion']['clients'].items():
        mapping = client_cfg.get('cobolFieldMapping')
        if mapping is not None:
            mapping['mappingRules'] = new_rules

    with open(overrides_path, 'w') as f:
        json.dump(data, f, indent=2)
    print(f"Updated mappingRules with {len(new_rules)} entries for all clients.")

if __name__ == '__main__':
    main()
