#!/usr/bin/env python3
"""
Advanced Pattern Analyzer - Handles multiple systematic patterns
Identifies different types of LENGTH_MISMATCH issues and their solutions
"""

import json
import os
from collections import defaultdict

class AdvancedPatternAnalyzer:
    def __init__(self):
        self.schema_path = 'schemas/compiled/88f02f49240f0a0084aa1b6358740a32931b6c1e3012832d52432a3e60865a9d.schema.json'
        self.actual_file = 'out/69172/69172p.set'
        self.expected_file = 'Expected_Outputs/69172/69172p.set'
        self.parity_file = 'parity_results_proper_zero_fix.txt'
        
    def analyze_all_patterns(self):
        """Comprehensive pattern analysis"""
        print('🔍 ADVANCED PATTERN ANALYSIS')
        print('=' * 50)
        
        # Load LENGTH_MISMATCH errors
        errors = self.load_length_mismatch_errors()
        if not errors:
            return
        
        # Categorize errors by pattern type
        patterns = self.categorize_error_patterns(errors)
        
        # Analyze each pattern type
        solutions = {}
        for pattern_type, pattern_errors in patterns.items():
            print(f'\n📊 Analyzing {pattern_type}: {len(pattern_errors)} errors')
            solution = self.analyze_pattern_type(pattern_type, pattern_errors)
            if solution:
                solutions[pattern_type] = solution
        
        # Generate comprehensive fix strategy
        self.generate_fix_strategy(solutions)
        
        return solutions
    
    def load_length_mismatch_errors(self):
        """Load LENGTH_MISMATCH errors"""
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
                    
                    if len(expected_value) != len(actual_value):
                        errors.append({
                            'field': current_field,
                            'expected': expected_value,
                            'actual': actual_value,
                            'exp_len': len(expected_value),
                            'act_len': len(actual_value)
                        })
                    
                    current_field = None
                    expected_value = None
                    actual_value = None
            
            print(f'Loaded {len(errors)} LENGTH_MISMATCH errors')
            return errors
            
        except FileNotFoundError:
            print(f'❌ Parity file not found')
            return []
    
    def categorize_error_patterns(self, errors):
        """Categorize errors by pattern type"""
        patterns = {
            'EMPTY_ACTUAL': [],      # Expected has data, actual is empty
            'QUESTION_MARKS': [],    # Actual has ??? corruption
            'WRONG_LENGTH': [],      # Different lengths but both have data
            'ZERO_PADDING': [],      # Actual has zeros instead of data
            'OTHER': []
        }
        
        for error in errors:
            expected = error['expected']
            actual = error['actual']
            
            if actual.strip() == '':
                patterns['EMPTY_ACTUAL'].append(error)
            elif '?' in actual:
                patterns['QUESTION_MARKS'].append(error)
            elif actual.replace('0', '').strip() == '':
                patterns['ZERO_PADDING'].append(error)
            elif expected.strip() != '' and actual.strip() != '':
                patterns['WRONG_LENGTH'].append(error)
            else:
                patterns['OTHER'].append(error)
        
        # Show pattern distribution
        print('\\n📊 PATTERN DISTRIBUTION:')
        for pattern_type, pattern_errors in patterns.items():
            if pattern_errors:
                percentage = (len(pattern_errors) / len(errors)) * 100
                print(f'  {pattern_type}: {len(pattern_errors)} errors ({percentage:.1f}%)')
        
        return patterns
    
    def analyze_pattern_type(self, pattern_type, errors):
        """Analyze specific pattern type"""
        if pattern_type == 'EMPTY_ACTUAL':
            return self.analyze_empty_actual_pattern(errors)
        elif pattern_type == 'QUESTION_MARKS':
            return self.analyze_question_marks_pattern(errors)
        elif pattern_type == 'WRONG_LENGTH':
            return self.analyze_wrong_length_pattern(errors)
        elif pattern_type == 'ZERO_PADDING':
            return self.analyze_zero_padding_pattern(errors)
        else:
            return None
    
    def analyze_empty_actual_pattern(self, errors):
        """Analyze empty actual value pattern - likely schema offset issues"""
        print('  → Analyzing EMPTY_ACTUAL pattern (schema offset issues)')
        
        # Load schema and files
        field_lookup = self.load_schema_lookup()
        if not field_lookup:
            return None
        
        try:
            with open(self.actual_file, 'rb') as f:
                actual_record = f.read(2000)
            
            offset_corrections = []
            
            for error in errors[:10]:  # Analyze first 10
                field_name = error['field']
                expected_text = error['expected'].strip()
                
                if field_name not in field_lookup or len(expected_text) < 3:
                    continue
                
                schema_offset = field_lookup[field_name]['offset']
                
                # Search for expected text in actual file
                search_bytes = expected_text.encode('ascii', errors='ignore')
                if search_bytes:
                    pos = actual_record.find(search_bytes)
                    if pos != -1 and pos != schema_offset:
                        offset_diff = pos - schema_offset
                        offset_corrections.append({
                            'field': field_name,
                            'offset_diff': offset_diff,
                            'current_offset': schema_offset,
                            'correct_offset': pos
                        })
            
            if offset_corrections:
                # Find most common offset difference
                diff_counts = defaultdict(int)
                for correction in offset_corrections:
                    diff_counts[correction['offset_diff']] += 1
                
                most_common_diff = max(diff_counts.keys(), key=lambda x: diff_counts[x])
                confidence = diff_counts[most_common_diff] / len(offset_corrections)
                
                return {
                    'type': 'schema_offset_correction',
                    'offset_adjustment': most_common_diff,
                    'affected_fields': len(errors),
                    'confidence': confidence,
                    'sample_corrections': offset_corrections[:5]
                }
        
        except FileNotFoundError:
            pass
        
        return None
    
    def analyze_question_marks_pattern(self, errors):
        """Analyze question marks pattern - likely EBCDIC conversion issues"""
        print('  → Analyzing QUESTION_MARKS pattern (EBCDIC conversion issues)')
        
        # This is the same issue we solved before - EBCDIC to ASCII conversion
        return {
            'type': 'ebcdic_conversion_fix',
            'affected_fields': len(errors),
            'confidence': 0.9,
            'solution': 'Apply ASCII detection logic to these fields'
        }
    
    def analyze_wrong_length_pattern(self, errors):
        """Analyze wrong length pattern"""
        print('  → Analyzing WRONG_LENGTH pattern')
        
        # Sample analysis
        samples = errors[:5]
        for sample in samples:
            print(f'    {sample["field"]}: exp_len={sample["exp_len"]}, act_len={sample["act_len"]}')
        
        return {
            'type': 'length_mismatch_analysis',
            'affected_fields': len(errors),
            'confidence': 0.5,
            'requires_manual_review': True
        }
    
    def analyze_zero_padding_pattern(self, errors):
        """Analyze zero padding pattern"""
        print('  → Analyzing ZERO_PADDING pattern')
        
        return {
            'type': 'zero_padding_fix',
            'affected_fields': len(errors),
            'confidence': 0.7,
            'solution': 'Fields being zeroed instead of populated with data'
        }
    
    def load_schema_lookup(self):
        """Load schema field lookup"""
        try:
            with open(self.schema_path, 'r') as f:
                schema = json.load(f)
            
            container = schema.get('Container4000', {})
            fields = container.get('Fields', [])
            
            field_lookup = {}
            for field in fields:
                field_lookup[field['Name']] = {
                    'offset': field['Offset'],
                    'length': field['Length']
                }
            
            return field_lookup
            
        except FileNotFoundError:
            return {}
    
    def generate_fix_strategy(self, solutions):
        """Generate comprehensive fix strategy"""
        print('\\n🚀 COMPREHENSIVE FIX STRATEGY')
        print('=' * 40)
        
        total_impact = 0
        high_confidence_fixes = []
        
        for pattern_type, solution in solutions.items():
            impact = solution['affected_fields']
            confidence = solution['confidence']
            total_impact += impact
            
            print(f'\\n{pattern_type}:')
            print(f'  Impact: {impact} fields')
            print(f'  Confidence: {confidence:.1%}')
            print(f'  Solution: {solution.get("solution", solution["type"])}')
            
            if confidence >= 0.8:
                high_confidence_fixes.append((pattern_type, solution))
        
        print(f'\\n📊 TOTAL POTENTIAL IMPACT: {total_impact} fields')
        print(f'🎯 HIGH CONFIDENCE FIXES: {len(high_confidence_fixes)}')
        
        if high_confidence_fixes:
            print('\\n✅ RECOMMENDED IMMEDIATE ACTIONS:')
            for i, (pattern_type, solution) in enumerate(high_confidence_fixes, 1):
                print(f'{i}. Fix {pattern_type} ({solution["affected_fields"]} fields, {solution["confidence"]:.1%} confidence)')
        
        return high_confidence_fixes

if __name__ == '__main__':
    analyzer = AdvancedPatternAnalyzer()
    solutions = analyzer.analyze_all_patterns()
