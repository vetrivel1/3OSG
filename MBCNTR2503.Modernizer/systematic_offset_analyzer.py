#!/usr/bin/env python3
"""
Systematic Offset Analyzer - Independent Test Framework
Identifies schema offset issues and generates systematic fixes
"""

import json
import os
from collections import defaultdict

class OffsetAnalyzer:
    def __init__(self):
        self.schema_path = 'schemas/compiled/88f02f49240f0a0084aa1b6358740a32931b6c1e3012832d52432a3e60865a9d.schema.json'
        self.actual_file = 'out/69172/69172p.set'
        self.expected_file = 'Expected_Outputs/69172/69172p.set'
        self.parity_file = 'parity_results_proper_zero_fix.txt'
        
    def load_parity_errors(self):
        """Load LENGTH_MISMATCH errors from parity results"""
        print('📊 Loading LENGTH_MISMATCH errors...')
        
        errors = []
        try:
            with open(self.parity_file, 'r') as f:
                content = f.read()
            
            lines = content.split('\n')
            current_field = None
            expected_value = None
            actual_value = None
            
            for line in lines:
                line = line.strip()
                if 'Field:' in line:
                    current_field = line.split('Field:')[1].strip()
                elif 'Expected:' in line and current_field:
                    expected_value = line.split('Expected:')[1].strip().strip("'")
                elif 'Actual:' in line and current_field and expected_value:
                    actual_value = line.split('Actual:')[1].strip().strip("'")
                    
                    # Check if this is a LENGTH_MISMATCH
                    if len(expected_value) != len(actual_value):
                        errors.append({
                            'field': current_field,
                            'expected': expected_value,
                            'actual': actual_value
                        })
                    
                    current_field = None
                    expected_value = None
                    actual_value = None
            
            print(f'Found {len(errors)} LENGTH_MISMATCH errors')
            return errors
            
        except FileNotFoundError:
            print(f'❌ Parity file not found: {self.parity_file}')
            return []
    
    def load_schema(self):
        """Load schema field definitions"""
        try:
            with open(self.schema_path, 'r') as f:
                schema = json.load(f)
            
            container = schema.get('Container4000', {})
            fields = container.get('Fields', [])
            
            # Create field lookup
            field_lookup = {}
            for field in fields:
                field_lookup[field['Name']] = {
                    'offset': field['Offset'],
                    'length': field['Length'],
                    'type': field.get('Type', 'unknown')
                }
            
            return field_lookup, schema
            
        except FileNotFoundError:
            print(f'❌ Schema file not found: {self.schema_path}')
            return {}, {}
    
    def analyze_offset_patterns(self, errors, field_lookup):
        """Analyze offset patterns to find systematic corrections"""
        print('🔍 Analyzing offset patterns...')
        
        corrections = []
        
        try:
            # Load actual and expected files
            with open(self.actual_file, 'rb') as f:
                actual_record = f.read(2000)
            with open(self.expected_file, 'rb') as f:
                expected_record = f.read(2000)
            
            for error in errors[:20]:  # Analyze first 20 errors for patterns
                field_name = error['field']
                expected_text = error['expected'].strip()
                
                if field_name not in field_lookup:
                    continue
                
                schema_info = field_lookup[field_name]
                schema_offset = schema_info['offset']
                schema_length = schema_info['length']
                
                # Skip if expected text is empty or too short
                if len(expected_text) < 3:
                    continue
                
                print(f'\n🔍 Analyzing {field_name}:')
                print(f'  Schema offset: {schema_offset}, length: {schema_length}')
                print(f'  Expected: "{expected_text}"')
                
                # Check what's at the schema offset in actual file
                if schema_offset + schema_length <= len(actual_record):
                    actual_data = actual_record[schema_offset:schema_offset + schema_length]
                    actual_text = ''.join(chr(b) if 32 <= b <= 126 else '?' for b in actual_data).strip()
                    print(f'  At schema offset: "{actual_text}"')
                
                # Search for expected text in the actual file
                search_bytes = expected_text.encode('ascii', errors='ignore')
                if search_bytes:
                    pos = actual_record.find(search_bytes)
                    if pos != -1 and pos != schema_offset:
                        offset_diff = pos - schema_offset
                        print(f'  💡 Found expected text at offset {pos} (diff: {offset_diff:+d})')
                        
                        corrections.append({
                            'field': field_name,
                            'current_offset': schema_offset,
                            'correct_offset': pos,
                            'offset_diff': offset_diff,
                            'length': schema_length,
                            'expected_text': expected_text
                        })
                    else:
                        print(f'  ❌ Expected text not found in actual file')
                else:
                    print(f'  ⚠️  Cannot encode expected text as ASCII')
            
            return corrections
            
        except FileNotFoundError as e:
            print(f'❌ File not found: {e}')
            return []
    
    def find_systematic_patterns(self, corrections):
        """Find systematic offset patterns"""
        print('\n🎯 Finding systematic patterns...')
        
        # Group by offset difference
        diff_groups = defaultdict(list)
        for correction in corrections:
            diff = correction['offset_diff']
            diff_groups[diff].append(correction)
        
        print('Offset difference patterns:')
        for diff, group in sorted(diff_groups.items(), key=lambda x: len(x[1]), reverse=True):
            print(f'  {diff:+4d} bytes: {len(group)} fields')
            for item in group[:3]:  # Show first 3 examples
                print(f'    - {item["field"]} ({item["current_offset"]} → {item["correct_offset"]})')
            if len(group) > 3:
                print(f'    ... and {len(group) - 3} more')
        
        return diff_groups
    
    def generate_schema_corrections(self, diff_groups, field_lookup, schema):
        """Generate schema corrections that can be applied"""
        print('\n🚀 Generating schema corrections...')
        
        # Find the most common offset difference (likely systematic)
        if not diff_groups:
            print('No systematic patterns found')
            return None
        
        # Get the most frequent offset difference
        most_common_diff = max(diff_groups.keys(), key=lambda x: len(diff_groups[x]))
        most_common_group = diff_groups[most_common_diff]
        
        print(f'Most common offset difference: {most_common_diff:+d} bytes ({len(most_common_group)} fields)')
        
        # Generate correction instructions
        correction_instructions = {
            'offset_adjustment': most_common_diff,
            'affected_fields': [item['field'] for item in most_common_group],
            'field_count': len(most_common_group),
            'confidence': 'high' if len(most_common_group) >= 5 else 'medium'
        }
        
        return correction_instructions
    
    def run_analysis(self):
        """Run complete analysis"""
        print('🔍 SYSTEMATIC OFFSET ANALYSIS')
        print('=' * 50)
        
        # Load data
        errors = self.load_parity_errors()
        if not errors:
            return None
        
        field_lookup, schema = self.load_schema()
        if not field_lookup:
            return None
        
        # Analyze patterns
        corrections = self.analyze_offset_patterns(errors, field_lookup)
        if not corrections:
            print('No offset corrections found')
            return None
        
        # Find systematic patterns
        diff_groups = self.find_systematic_patterns(corrections)
        
        # Generate correction instructions
        instructions = self.generate_schema_corrections(diff_groups, field_lookup, schema)
        
        if instructions:
            print('\n✅ SYSTEMATIC CORRECTION IDENTIFIED:')
            print('=' * 45)
            print(f'Offset adjustment: {instructions["offset_adjustment"]:+d} bytes')
            print(f'Affected fields: {instructions["field_count"]}')
            print(f'Confidence: {instructions["confidence"]}')
            print('\nThis correction can be applied systematically to fix LENGTH_MISMATCH issues')
            
            return instructions
        
        return None

if __name__ == '__main__':
    analyzer = OffsetAnalyzer()
    result = analyzer.run_analysis()
    
    if result:
        print('\n🎯 NEXT STEPS:')
        print('1. Review the systematic correction')
        print('2. Apply the offset adjustment to affected fields')
        print('3. Test the results')
        print('4. Merge successful fixes to main codebase')
    else:
        print('\n❌ No systematic patterns found')
        print('Manual analysis may be required')
