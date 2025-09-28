#!/usr/bin/env python3
import re
import json
import argparse

def parse_copybook(path):
    # Match level, name, optional REDEFINES, optional PIC
    pattern = re.compile(r'^\s*(\d+)\s+([A-Z0-9-]+)' \
                          r'(?:\s+REDEFINES\s+([A-Z0-9-]+))?' \
                          r'(?:\s+PIC\s+([^\.]+))?\.')
    root = {'name': 'ROOT', 'level': 0, 'children': []}
    stack = [root]
    with open(path, 'r') as f:
        for line in f:
            m = pattern.match(line)
            if not m:
                continue
            level = int(m.group(1))
            name = m.group(2)
            redefines = m.group(3)
            pic = m.group(4).strip() if m.group(4) else None
            node = {'name': name, 'level': level, 'redefines': redefines, 'pic': pic, 'children': []}
            # find correct parent
            while stack and stack[-1]['level'] >= level:
                stack.pop()
            stack[-1]['children'].append(node)
            stack.append(node)
    return root


def node_to_schema(node):
    # Group node
    if node.get('children'):
        props = {}
        for child in node['children']:
            props[child['name']] = node_to_schema(child)
        return {
            'type': 'object',
            'properties': props
        }
    # Leaf node
    pic = node.get('pic', '')
    # Simple PIC to JSON type mapping
    if pic.upper().startswith('X'):
        return {'type': 'string'}
    else:
        return {'type': 'number'}


def build_schema(root, title):
    schema = {
        '$schema': 'http://json-schema.org/draft-07/schema#',
        'title': title,
        'type': 'object',
        'properties': {}
    }
    for child in root['children']:
        schema['properties'][child['name']] = node_to_schema(child)
    return schema


def main():
    parser = argparse.ArgumentParser(description='Convert COBOL copybook to JSON Schema')
    parser.add_argument('--input', '-i', required=True, help='Path to copybook .cbl file')
    parser.add_argument('--output', '-o', required=True, help='Path to write JSON schema')
    parser.add_argument('--title', '-t', default='Generated Schema', help='Title for JSON schema')
    args = parser.parse_args()

    root = parse_copybook(args.input)
    schema = build_schema(root, args.title)
    with open(args.output, 'w') as out:
        json.dump(schema, out, indent=2)

if __name__ == '__main__':
    main()
