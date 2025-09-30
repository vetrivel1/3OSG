#python py/batch_comparison.py "users/output" "Expected_Outputs/69172"
"""
Batch File Comparison Tool
Compares files in two directory trees (recursively) and reports differences.

Defaults:
- actual_output_dir -> c:\\Users\\Shan\\Documents\\3OSG\\MBCNTR2503.Modernizer\\out
- expected_output_dir -> c:\\Users\\Shan\\Documents\\3OSG\\Legacy Application\\Expected_Outputs
"""

import os
import sys
from datetime import datetime
from typing import List, Dict, Any
# Try to import rich comparators if present; otherwise fall back to a simple implementation
FileComparator = None
FinalFileComparator = None

try:
    # Attempt import from local package context
    from file_comparison import FileComparator as _FC
    FileComparator = _FC
except Exception:
    # Try test utilities path inside Modernizer project
    try:
        _repo_root = os.path.dirname(__file__)
        tests_path = os.path.join(_repo_root, "MBCNTR2503.Modernizer", "tests")
        if os.path.isdir(tests_path):
            sys.path.append(tests_path)
            from file_comparison import FileComparator as _FC2  # type: ignore
            FileComparator = _FC2
    except Exception:
        FileComparator = None

try:
    from final_comparison import FinalFileComparator as _FFC
    FinalFileComparator = _FFC
except Exception:
    FinalFileComparator = None

# Minimal fallback comparator implementations
if FinalFileComparator is None:
    class FinalFileComparator:  # type: ignore
        """Simple line-by-line comparator returning a list of diffs.
        Output schema: list of { line_number, field_type, expected, actual }
        """
        def __init__(self, expected_path: str, actual_path: str) -> None:
            self.expected_path = expected_path
            self.actual_path = actual_path

        def find_all_differences(self):
            diffs = []
            try:
                with open(self.expected_path, 'r', encoding='utf-8', errors='ignore') as fe:
                    expected_lines = fe.readlines()
            except Exception:
                expected_lines = []
            try:
                with open(self.actual_path, 'r', encoding='utf-8', errors='ignore') as fa:
                    actual_lines = fa.readlines()
            except Exception:
                actual_lines = []

            max_len = max(len(expected_lines), len(actual_lines))
            for i in range(max_len):
                exp = expected_lines[i].rstrip('\n\r') if i < len(expected_lines) else ""
                act = actual_lines[i].rstrip('\n\r') if i < len(actual_lines) else ""
                if exp != act:
                    diffs.append({
                        'line_number': i + 1,
                        'field_type': 'TEXT',
                        'expected': exp,
                        'actual': act,
                    })
            return diffs

if FileComparator is None:
    class FileComparator:  # type: ignore
        """Fallback detailed comparator compatible with compare_files() shape.
        Produces a results dict similar to the expected interface.
        """
        def __init__(self, expected_path: str, actual_path: str) -> None:
            self.expected_path = expected_path
            self.actual_path = actual_path

        def compare_files(self):
            # Reuse the simple comparator for now
            simple = FinalFileComparator(self.expected_path, self.actual_path)
            diffs = simple.find_all_differences()
            return {
                "summary": {
                    "expected_file_name": os.path.basename(self.expected_path),
                    "actual_file_name": os.path.basename(self.actual_path),
                    "files_match": len(diffs) == 0,
                    "total_differences": len(diffs)
                },
                "differences": diffs
            }


class BatchFileComparator:
    def __init__(self, actual_output_dir: str, expected_output_dir: str):
        """
        Initialize the batch file comparator.
        
        Args:
            actual_output_dir: Directory containing actual output files
            expected_output_dir: Directory containing expected output files
        """
        self.actual_output_dir = actual_output_dir
        self.expected_output_dir = expected_output_dir
        self.comparison_results = []
        
    def _walk_files(self, base_dir: str) -> set[str]:
        """Recursively collect files under base_dir as relative paths (POSIX-style)."""
        rels: set[str] = set()
        for root, _, files in os.walk(base_dir):
            for f in files:
                abs_path = os.path.join(root, f)
                rel_path = os.path.relpath(abs_path, base_dir)
                # Normalize to forward slashes for stable reporting
                rels.add(rel_path.replace('\\', '/'))
        return rels

    def get_common_files(self) -> List[str]:
        """Get list of common files between the two directory trees (recursive)."""
        actual_files = self._walk_files(self.actual_output_dir)
        expected_files = self._walk_files(self.expected_output_dir)
        common = sorted(actual_files.intersection(expected_files))
        return common
    
    def find_character_differences(self, expected_line: str, actual_line: str) -> List[Dict]:
        """Find character-level differences between two lines."""
        differences = []
        max_len = max(len(expected_line), len(actual_line))
        
        for i in range(max_len):
            expected_char = expected_line[i] if i < len(expected_line) else ""
            actual_char = actual_line[i] if i < len(actual_line) else ""
            
            if expected_char != actual_char:
                differences.append({
                    'position': i + 1,
                    'expected_char': expected_char,
                    'actual_char': actual_char,
                    'expected_hex': hex(ord(expected_char)) if expected_char else 'EOF',
                    'actual_hex': hex(ord(actual_char)) if actual_char else 'EOF'
                })
        
        return differences

    def compare_single_file(self, relpath: str, detailed: bool = False) -> Dict[str, Any]:
        """Compare a single file given by relative path in both trees."""
        # Ensure OS paths
        rel_os = relpath.replace('/', os.sep)
        actual_path = os.path.join(self.actual_output_dir, rel_os)
        expected_path = os.path.join(self.expected_output_dir, rel_os)
        
        try:
            if detailed:
                # Use detailed comparison
                comparator = FileComparator(expected_path, actual_path)
                results = comparator.compare_files()
            else:
                # Use simple comparison
                comparator = FinalFileComparator(expected_path, actual_path)
                differences = comparator.find_all_differences()
                
                # Add character-level differences to each difference
                for diff in differences:
                    if 'character_differences' not in diff:
                        # Read the lines to get character-level differences
                        try:
                            with open(expected_path, 'r', encoding='utf-8', errors='ignore') as f:
                                expected_lines = f.readlines()
                            with open(actual_path, 'r', encoding='utf-8', errors='ignore') as f:
                                actual_lines = f.readlines()
                            
                            line_num = diff['line_number'] - 1  # Convert to 0-based index
                            if line_num < len(expected_lines) and line_num < len(actual_lines):
                                expected_line = expected_lines[line_num].rstrip('\n\r')
                                actual_line = actual_lines[line_num].rstrip('\n\r')
                                char_diffs = self.find_character_differences(expected_line, actual_line)
                                diff['character_differences'] = char_diffs
                        except Exception:
                            diff['character_differences'] = []
                
                results = {
                    "summary": {
                        "expected_file_name": os.path.basename(expected_path),
                        "actual_file_name": os.path.basename(actual_path),
                        "files_match": len(differences) == 0,
                        "total_differences": len(differences)
                    },
                    "differences": differences
                }
            
            return {
                "filename": relpath,
                "success": True,
                "results": results,
                "error": None
            }
            
        except Exception as e:
            return {
                "filename": relpath,
                "success": False,
                "results": None,
                "error": str(e)
            }
    
    def compare_all_files(self, detailed: bool = False) -> List[Dict[str, Any]]:
        """Compare all common files between the two directories."""
        common_files = self.get_common_files()
        
        if not common_files:
            print("No common files found between the directories.")
            return []
        
        print(f"Found {len(common_files)} common files to compare (relative to base dirs):")
        for relpath in common_files:
            print(f"  - {relpath}")
        
        self.comparison_results = []
        
        for relpath in common_files:
            result = self.compare_single_file(relpath, detailed)
            self.comparison_results.append(result)
        
        return self.comparison_results
    
    def generate_summary_report(self) -> str:
        """Generate a summary report of all comparisons."""
        if not self.comparison_results:
            return "No comparison results available."
        
        report_lines = []
        report_lines.append("=" * 80)
        report_lines.append("BATCH FILE COMPARISON SUMMARY REPORT")
        report_lines.append("=" * 80)
        report_lines.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        report_lines.append("")
        
        # Summary statistics
        total_files = len(self.comparison_results)
        successful_comparisons = len([r for r in self.comparison_results if r['success']])
        failed_comparisons = total_files - successful_comparisons
        
        # Count matching files with proper error handling
        matching_files = 0
        for r in self.comparison_results:
            if r['success'] and r['results'] and 'summary' in r['results']:
                if r['results']['summary']['files_match']:
                    matching_files += 1
        
        non_matching_files = successful_comparisons - matching_files
        
        report_lines.append("SUMMARY STATISTICS:")
        report_lines.append("-" * 40)
        report_lines.append(f"Total files compared: {total_files}")
        report_lines.append(f"Successful comparisons: {successful_comparisons}")
        report_lines.append(f"Failed comparisons: {failed_comparisons}")
        report_lines.append(f"Files that match: {matching_files}")
        report_lines.append(f"Files that don't match: {non_matching_files}")
        report_lines.append("")
        
        # File-by-file results
        report_lines.append("FILE-BY-FILE RESULTS:")
        report_lines.append("-" * 40)
        
        for result in self.comparison_results:
            filename = result['filename']
            if result['success'] and result['results'] and 'summary' in result['results']:
                summary = result['results']['summary']
                if summary['files_match']:
                    status = "✅ MATCH"
                else:
                    status = f"❌ DIFFERENCES ({summary['total_differences']} found)"
                report_lines.append(f"{filename}: {status}")
            elif result['success']:
                report_lines.append(f"{filename}: ❌ ERROR - Invalid results structure")
            else:
                report_lines.append(f"{filename}: ❌ ERROR - {result['error']}")
        
        report_lines.append("")
        
        # Detailed results for non-matching files
        non_matching_results = [r for r in self.comparison_results if r['success'] and r['results'] and 'summary' in r['results'] and not r['results']['summary']['files_match']]
        if non_matching_results:
            report_lines.append("DETAILED RESULTS FOR NON-MATCHING FILES:")
            report_lines.append("-" * 50)
            
            for result in non_matching_results:
                filename = result['filename']
                differences = result['results']['differences']
                
                report_lines.append(f"\n{filename}:")
                report_lines.append(f"  Total differences: {len(differences)}")
                
                # Show all differences
                for i, diff in enumerate(differences):
                    if 'type' in diff:
                        # Detailed comparison format
                        report_lines.append(f"  Line {diff['line_number']}: {diff['type']}")
                        report_lines.append(f"    Expected: {diff['expected_content']}")
                        report_lines.append(f"    Actual: {diff['actual_content']}")
                        
                        # Show character-level differences if available
                        if 'character_differences' in diff and diff['character_differences']:
                            report_lines.append("    Character-level differences:")
                            for char_diff in diff['character_differences']:
                                report_lines.append(f"      Position {char_diff['position']}:")
                                report_lines.append(f"        Expected: '{char_diff['expected_char']}' ({char_diff['expected_hex']})")
                                report_lines.append(f"        Actual:   '{char_diff['actual_char']}' ({char_diff['actual_hex']})")
                    else:
                        # Simple comparison format
                        report_lines.append(f"  Line {diff['line_number']}: {diff.get('field_type', 'Unknown')}")
                        report_lines.append(f"    Expected: {diff['expected']}")
                        report_lines.append(f"    Actual: {diff['actual']}")
                        
                        # Show character-level differences if available
                        if 'character_differences' in diff and diff['character_differences']:
                            report_lines.append("    Character-level differences:")
                            for char_diff in diff['character_differences']:
                                report_lines.append(f"      Position {char_diff['position']}:")
                                report_lines.append(f"        Expected: '{char_diff['expected_char']}' ({char_diff['expected_hex']})")
                                report_lines.append(f"        Actual:   '{char_diff['actual_char']}' ({char_diff['actual_hex']})")
        
        # Add summary of all character-level differences
        all_char_diffs = []
        for result in non_matching_results:
            if result['success'] and result['results'] and 'differences' in result['results']:
                for diff in result['results']['differences']:
                    if 'character_differences' in diff and diff['character_differences']:
                        for char_diff in diff['character_differences']:
                            all_char_diffs.append({
                                'filename': result['filename'],
                                'line_number': diff['line_number'],
                                'position': char_diff['position'],
                                'expected_char': char_diff['expected_char'],
                                'actual_char': char_diff['actual_char'],
                                'expected_hex': char_diff['expected_hex'],
                                'actual_hex': char_diff['actual_hex']
                            })
        
        if all_char_diffs:
            report_lines.append("\n\nALL CHARACTER-LEVEL DIFFERENCES SUMMARY:")
            report_lines.append("-" * 60)
            for char_diff in all_char_diffs:
                report_lines.append(f"{char_diff['filename']} - Line {char_diff['line_number']}, Position {char_diff['position']}:")
                report_lines.append(f"  Expected: '{char_diff['expected_char']}' ({char_diff['expected_hex']})")
                report_lines.append(f"  Actual:   '{char_diff['actual_char']}' ({char_diff['actual_hex']})")
                report_lines.append("")
        
        return "\n".join(report_lines)
    
    def save_summary_report(self) -> str:
        """Save the summary report to a file."""
        report = self.generate_summary_report()
        
        # Extract job number from expected output directory
        expected_dir_name = os.path.basename(self.expected_output_dir)
        job_number = expected_dir_name  # Use the directory name as job number

        # Create output directory if it doesn't exist
        output_dir = "py/output"
        os.makedirs(output_dir, exist_ok=True)
        
        # Generate output filename
        output_file = os.path.join(output_dir, f"{job_number}_batch_comparison_summary.txt")
        
        try:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(report)
            return output_file
        except Exception as e:
            print(f"Error saving summary report: {e}")
            return None


def main():
    """Run the batch comparison. If no args, use repo defaults from attachments."""
    # Defaults pointing to attached folders
    default_actual = r"c:\\Users\\Shan\\Documents\\3OSG\\MBCNTR2503.Modernizer\\out"
    default_expected = r"c:\\Users\\Shan\\Documents\\3OSG\\Legacy Application\\Expected_Outputs"

    # Parse args
    if len(sys.argv) >= 3:
        actual_dir = sys.argv[1]
        expected_dir = sys.argv[2]
    else:
        actual_dir = default_actual
        expected_dir = default_expected
        print("No arguments provided. Using defaults:")
        print(f"  actual_output_dir  = {actual_dir}")
        print(f"  expected_output_dir= {expected_dir}")

    detailed = "--detailed" in sys.argv
    
    # Check if directories exist
    if not os.path.exists(actual_dir):
        print(f"Error: Actual output directory not found - {actual_dir}")
        return
    
    if not os.path.exists(expected_dir):
        print(f"Error: Expected output directory not found - {expected_dir}")
        return
    
    # Create batch comparator and run comparison
    batch_comparator = BatchFileComparator(actual_dir, expected_dir)
    
    print(f"Starting batch comparison...")
    print(f"Actual directory: {actual_dir}")
    print(f"Expected directory: {expected_dir}")
    print(f"Mode: {'Detailed' if detailed else 'Simple'}")
    
    # Run comparisons
    results = batch_comparator.compare_all_files(detailed)
    
    # Generate and save summary report
    summary_file = batch_comparator.save_summary_report()
    if summary_file:
        print(f"\nSummary report saved to: {summary_file}")
    
    # Print summary to console
    print("BATCH COMPARISON COMPLETE")


if __name__ == "__main__":
    main()
