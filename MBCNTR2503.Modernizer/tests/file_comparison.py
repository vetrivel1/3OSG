#python file_comparison.py "69172.4300.txt" --legacy-dir "Legacy Application/Expected_Outputs/69172" --actual-dir "Pipeline_Test/Public"
"""
File Comparison Tool for MBCNTR2503 Pipeline
Compares legacy expected outputs with modernized pipeline outputs.
Automatically detects workspace structure and file paths.
"""

import os
import sys
import argparse
from datetime import datetime
from typing import List, Tuple, Dict, Any


class FileComparator:
    def __init__(self, file_name: str, legacy_dir: str = None, actual_dir: str = None, output_path: str = None):
        """
        Initialize the file comparator.
        
        Args:
            file_name: Name of the file to compare (e.g., "69172.4300.txt")
            legacy_dir: Directory containing legacy expected outputs
            actual_dir: Directory containing actual pipeline outputs
            output_path: Path for the comparison report (optional)
        """
        self.file_name = file_name
        self.workspace_root = self._find_workspace_root()
        
        # Set default directories based on current pipeline structure
        self.legacy_dir = legacy_dir or self._get_default_legacy_dir()
        self.actual_dir = actual_dir or self._get_default_actual_dir()
        
        # Construct full file paths
        self.expected_file_path = os.path.join(self.workspace_root, self.legacy_dir, file_name)
        self.actual_file_path = os.path.join(self.workspace_root, self.actual_dir, file_name)
        
        self.output_path = output_path or self._generate_output_path()
        self.differences = []
        self.summary = {}
    
    def _find_workspace_root(self) -> str:
        """Find the workspace root directory automatically."""
        current_dir = os.path.dirname(os.path.abspath(__file__))
        
        # Look for the 3OSG directory (our workspace root)
        while current_dir and os.path.basename(current_dir) != '3OSG':
            parent = os.path.dirname(current_dir)
            if parent == current_dir:  # Reached filesystem root
                break
            current_dir = parent
        
        if os.path.basename(current_dir) == '3OSG':
            return current_dir
        
        # Fallback: use the directory containing this script's parent directories
        script_dir = os.path.dirname(os.path.abspath(__file__))
        return os.path.join(script_dir, '..', '..')
    
    def _get_default_legacy_dir(self) -> str:
        """Get default legacy directory based on file name."""
        # Extract job number from file name (e.g., "69172" from "69172.4300.txt")
        job_number = self.file_name.split('.')[0]
        return os.path.join("Legacy Application", "Expected_Outputs", job_number)
    
    def _get_default_actual_dir(self) -> str:
        """Get default actual output directory."""
        return os.path.join("MBCNTR2503.Modernizer", "Pipeline_Test", "Public")
    
    def _generate_output_path(self) -> str:
        """Generate output path based on job number from file name."""
        # Extract job number from file name
        job_number = self.file_name.split('.')[0]
        
        # Create output directory in the current pipeline test directory
        output_dir = os.path.join(self.workspace_root, "MBCNTR2503.Modernizer", "Pipeline_Test", "comparison_reports")
        os.makedirs(output_dir, exist_ok=True)
        
        # Generate output filename with timestamp
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        output_filename = f"{job_number}_{self.file_name.replace('.', '_')}_comparison_{timestamp}.txt"
        return os.path.join(output_dir, output_filename)
        
    def get_file_info(self) -> Dict[str, Any]:
        """Get file information for both expected and actual files."""
        info = {
            "workspace_root": self.workspace_root,
            "legacy_dir": self.legacy_dir,
            "actual_dir": self.actual_dir,
            "file_name": self.file_name,
            "expected_path": self.expected_file_path,
            "actual_path": self.actual_file_path,
            "expected_exists": os.path.exists(self.expected_file_path),
            "actual_exists": os.path.exists(self.actual_file_path)
        }
        
        if info["expected_exists"]:
            info["expected_size"] = os.path.getsize(self.expected_file_path)
        
        if info["actual_exists"]:
            info["actual_size"] = os.path.getsize(self.actual_file_path)
            
        return info
        
    def read_file_lines(self, file_path: str) -> List[str]:
        """Read file and return lines as a list."""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                return file.readlines()
        except FileNotFoundError:
            print(f"Error: File not found - {file_path}")
            return []
        except Exception as e:
            print(f"Error reading file {file_path}: {e}")
            return []
    
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

    def compare_files(self) -> Dict[str, Any]:
        """
        Compare the two files and return detailed comparison results.
        
        Returns:
            Dictionary containing comparison results
        """
        # Get file info first
        file_info = self.get_file_info()
        
        if not file_info["expected_exists"]:
            return {"error": f"Expected file not found: {self.expected_file_path}"}
        
        if not file_info["actual_exists"]:
            return {"error": f"Actual file not found: {self.actual_file_path}"}
        
        print(f"Reading expected file: {self.expected_file_path}")
        expected_lines = self.read_file_lines(self.expected_file_path)
        
        print(f"Reading actual file: {self.actual_file_path}")
        actual_lines = self.read_file_lines(self.actual_file_path)
        
        if not expected_lines and not actual_lines:
            return {"error": "Both files are empty"}
        
        # Basic file info
        self.summary = {
            "expected_file_name": os.path.basename(self.expected_file_path),
            "actual_file_name": os.path.basename(self.actual_file_path),
            "expected_full_path": self.expected_file_path,
            "actual_full_path": self.actual_file_path,
            "expected_lines": len(expected_lines),
            "actual_lines": len(actual_lines),
            "expected_size": file_info.get("expected_size", 0),
            "actual_size": file_info.get("actual_size", 0),
            "comparison_time": datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
            "workspace_root": self.workspace_root
        }
        
        # Compare line by line
        max_lines = max(len(expected_lines), len(actual_lines))
        differences = []
        
        for i in range(max_lines):
            line_num = i + 1
            
            # Get lines (empty string if line doesn't exist)
            expected_line = expected_lines[i].rstrip('\n\r') if i < len(expected_lines) else ""
            actual_line = actual_lines[i].rstrip('\n\r') if i < len(actual_lines) else ""
            
            # Check if lines are different
            if expected_line != actual_line:
                diff_type = "MISMATCH"
                if i >= len(expected_lines):
                    diff_type = "MISSING_IN_EXPECTED"
                elif i >= len(actual_lines):
                    diff_type = "MISSING_IN_ACTUAL"
                
                # Find character-level differences
                char_diffs = self.find_character_differences(expected_line, actual_line)
                
                differences.append({
                    "line_number": line_num,
                    "type": diff_type,
                    "expected_content": expected_line,
                    "actual_content": actual_line,
                    "expected_length": len(expected_line),
                    "actual_length": len(actual_line),
                    "character_differences": char_diffs
                })
        
        self.differences = differences
        self.summary["total_differences"] = len(differences)
        self.summary["files_match"] = len(differences) == 0
        
        return {
            "summary": self.summary,
            "differences": differences
        }
    
    def generate_report(self) -> str:
        """Generate a detailed comparison report."""
        results = self.compare_files()
        
        if "error" in results:
            return f"Error: {results['error']}"
        
        report_lines = []
        report_lines.append("=" * 80)
        report_lines.append("FILE COMPARISON REPORT")
        report_lines.append("=" * 80)
        report_lines.append(f"Generated: {self.summary['comparison_time']}")
        report_lines.append("")
        
        # File information
        report_lines.append("FILE INFORMATION:")
        report_lines.append("-" * 40)
        report_lines.append(f"Workspace Root: {self.summary['workspace_root']}")
        report_lines.append("")
        report_lines.append(f"Expected (Legacy): {self.summary['expected_file_name']}")
        report_lines.append(f"  Full Path: {self.summary['expected_full_path']}")
        report_lines.append(f"  Lines: {self.summary['expected_lines']}")
        report_lines.append(f"  Size: {self.summary['expected_size']} bytes")
        report_lines.append("")
        report_lines.append(f"Actual (Pipeline): {self.summary['actual_file_name']}")
        report_lines.append(f"  Full Path: {self.summary['actual_full_path']}")
        report_lines.append(f"  Lines: {self.summary['actual_lines']}")
        report_lines.append(f"  Size: {self.summary['actual_size']} bytes")
        report_lines.append("")
        
        # Summary
        report_lines.append("COMPARISON SUMMARY:")
        report_lines.append("-" * 40)
        if self.summary['files_match']:
            report_lines.append("✅ FILES MATCH EXACTLY")
        else:
            report_lines.append("❌ FILES DO NOT MATCH")
            report_lines.append(f"Total differences found: {self.summary['total_differences']}")
        report_lines.append("")
        
        # Detailed differences
        if self.differences:
            report_lines.append("DETAILED DIFFERENCES:")
            report_lines.append("-" * 40)
            
            for diff in self.differences:
                report_lines.append(f"Line {diff['line_number']}: {diff['type']}")
                report_lines.append(f"  Expected ({self.summary['expected_file_name']}):")
                report_lines.append(f"    Length: {diff['expected_length']}")
                report_lines.append(f"    Content: '{diff['expected_content']}'")
                report_lines.append(f"  Actual ({self.summary['actual_file_name']}):")
                report_lines.append(f"    Length: {diff['actual_length']}")
                report_lines.append(f"    Content: '{diff['actual_content']}'")
                
                # Show character-level differences
                if diff['character_differences']:
                    report_lines.append("  Character-level differences:")
                    for char_diff in diff['character_differences']:
                        report_lines.append(f"    Position {char_diff['position']}:")
                        report_lines.append(f"      Expected: '{char_diff['expected_char']}' ({char_diff['expected_hex']})")
                        report_lines.append(f"      Actual:   '{char_diff['actual_char']}' ({char_diff['actual_hex']})")
                
                report_lines.append("")
        
        report_content = "\n".join(report_lines)
        
        # Save report to file
        try:
            with open(self.output_path, 'w', encoding='utf-8') as f:
                f.write(report_content)
            print(f"Report saved to: {self.output_path}")
        except Exception as e:
            print(f"Error saving report: {e}")
        
        return report_content
    
    def print_summary(self):
        """Print a quick summary to console."""
        file_info = self.get_file_info()
        
        print(f"\n📁 File Comparison Setup:")
        print(f"   Workspace: {file_info['workspace_root']}")
        print(f"   File: {file_info['file_name']}")
        print(f"   Legacy Dir: {file_info['legacy_dir']}")
        print(f"   Pipeline Dir: {file_info['actual_dir']}")
        print()
        
        if not file_info["expected_exists"]:
            print(f"❌ Expected file not found: {file_info['expected_path']}")
            return
            
        if not file_info["actual_exists"]:
            print(f"❌ Actual file not found: {file_info['actual_path']}")
            return
        
        results = self.compare_files()
        
        if "error" in results:
            print(f"❌ Error: {results['error']}")
            return
        
        print(f"📊 Comparison Summary:")
        print(f"   Expected (Legacy): {self.summary['expected_file_name']} ({self.summary['expected_lines']} lines, {self.summary['expected_size']} bytes)")
        print(f"   Actual (Pipeline): {self.summary['actual_file_name']} ({self.summary['actual_lines']} lines, {self.summary['actual_size']} bytes)")
        print()
        
        if self.summary['files_match']:
            print("✅ Files match exactly!")
        else:
            print(f"❌ Files do not match - {self.summary['total_differences']} differences found")
            print(f"📄 Detailed report saved to: {self.output_path}")


def create_comparison_parser():
    """Create argument parser for file comparison."""
    parser = argparse.ArgumentParser(
        description="Compare MBCNTR2503 pipeline outputs with legacy expected outputs",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Compare a specific file using default directories
  python file_comparison.py 69172.4300.txt
  
  # Compare with custom directories
  python file_comparison.py 69172.4300.txt --legacy-dir "Legacy Application/Expected_Outputs/69172" --actual-dir "Pipeline_Test/Public"
  
  # Compare multiple files (planned feature)
  python file_comparison.py 69172.4300.txt 69172.4300.txt.new 69172.4300.txt.length
        """
    )
    
    parser.add_argument(
        'file_name',
        help='Name of the file to compare (e.g., "69172.4300.txt")'
    )
    
    parser.add_argument(
        '--legacy-dir',
        help='Directory containing legacy expected outputs (relative to workspace root)',
        default=None
    )
    
    parser.add_argument(
        '--actual-dir', 
        help='Directory containing actual pipeline outputs (relative to workspace root)',
        default=None
    )
    
    parser.add_argument(
        '--output-file',
        help='Output file for the comparison report',
        default=None
    )
    
    parser.add_argument(
        '--summary-only',
        action='store_true',
        help='Show only summary without generating detailed report'
    )
    
    return parser


def main():
    """Main function to run the file comparison."""
    parser = create_comparison_parser()
    args = parser.parse_args()
    
    print("🔍 MBCNTR2503 Pipeline File Comparison Tool")
    print("=" * 50)
    
    # Create comparator and run comparison
    comparator = FileComparator(
        file_name=args.file_name,
        legacy_dir=args.legacy_dir,
        actual_dir=args.actual_dir,
        output_path=args.output_file
    )
    
    print("Starting file comparison...")
    comparator.print_summary()
    
    # Generate detailed report unless summary-only is requested
    if not args.summary_only:
        print("\n📄 Generating detailed report...")
        comparator.generate_report()
        print("✅ Comparison complete!")
    else:
        print("✅ Summary complete!")


def compare_multiple_files(file_names: List[str], legacy_dir: str = None, actual_dir: str = None):
    """
    Compare multiple files and generate a combined report.
    Future enhancement for batch comparisons.
    """
    print(f"🔍 Comparing {len(file_names)} files...")
    
    all_results = []
    for file_name in file_names:
        print(f"\n📄 Comparing {file_name}...")
        comparator = FileComparator(file_name, legacy_dir, actual_dir)
        comparator.print_summary()
        
        if not comparator.summary.get('files_match', False):
            comparator.generate_report()
            
        all_results.append(comparator.summary)
    
    # Generate combined summary
    total_files = len(all_results)
    matching_files = sum(1 for result in all_results if result.get('files_match', False))
    
    print(f"\n📊 Batch Comparison Summary:")
    print(f"   Total files compared: {total_files}")
    print(f"   Files matching: {matching_files}")
    print(f"   Files with differences: {total_files - matching_files}")
    
    if matching_files == total_files:
        print("✅ All files match exactly!")
    else:
        print("❌ Some files have differences - check individual reports")

if __name__ == "__main__":
    main()
