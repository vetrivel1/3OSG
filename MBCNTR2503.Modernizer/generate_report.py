#!/usr/bin/env python3
"""
Generate comprehensive file comparison reports for all jobs in the MBCNTR2503.Modernizer project.
This script compares generated files against expected outputs and provides detailed match percentages.
"""

import argparse
import json
import os
from pathlib import Path
from datetime import datetime

# --- Configuration ---
WORKSPACE_ROOT = Path(__file__).parent.parent
MODERNIZER_DIR = WORKSPACE_ROOT / "MBCNTR2503.Modernizer"
LEGACY_DIR = MODERNIZER_DIR  # Use the Modernizer dir for Expected_Outputs as well
OUTPUT_DIR = MODERNIZER_DIR / "out"

def calculate_file_similarity(file1: Path, file2: Path):
    """Calculate percentage similarity between two files based on size and content."""
    if not file1.exists() or not file2.exists():
        return {"size_match": 0.0, "content_match": 0.0, "size_diff": 0, "content_diff": 0}
    
    # Size comparison
    size1 = file1.stat().st_size
    size2 = file2.stat().st_size
    size_diff = abs(size1 - size2)
    size_match = (1.0 - (size_diff / max(size1, size2))) * 100 if max(size1, size2) > 0 else 100.0
    
    # Content comparison (byte-by-byte)
    with open(file1, 'rb') as f1, open(file2, 'rb') as f2:
        content1 = f1.read()
        content2 = f2.read()
    
    min_len = min(len(content1), len(content2))
    max_len = max(len(content1), len(content2))
    
    if max_len == 0:
        content_match = 100.0
        content_diff = 0
    else:
        # Count matching bytes
        matching_bytes = 0
        for i in range(min_len):
            if content1[i] == content2[i]:
                matching_bytes += 1
        
        content_diff = max_len - matching_bytes
        content_match = (matching_bytes / max_len) * 100
    
    return {
        "size_match": size_match,
        "content_match": content_match,
        "size_diff": size_diff,
        "content_diff": content_diff,
        "size1": size1,
        "size2": size2
    }

def get_available_jobs():
    """Get list of available job IDs from the Expected_Outputs directory."""
    expected_dir = LEGACY_DIR / "Expected_Outputs"
    if not expected_dir.exists():
        return []
    
    jobs = []
    for item in expected_dir.iterdir():
        if item.is_dir() and item.name.isdigit():
            jobs.append(item.name)
    
    return sorted(jobs)

def get_file_status(size_match: float, content_match: float, expected_exists: bool, actual_exists: bool):
    """Determine the status of a file based on its match percentages."""
    if not expected_exists or not actual_exists:
        return "[MISSING]"
    
    if size_match == 100.0 and content_match == 100.0:
        return "[OK] Perfect Match"
    elif size_match == 100.0 and content_match < 100.0:
        return f"[NOT OK] Content ({content_match:.2f}%)"
    elif size_match < 100.0 and content_match == 100.0:
        return f"[NOT OK] Size ({size_match:.2f}%)"
    else:
        return f"[NOT OK] Size & Content ({size_match:.2f}%/{content_match:.2f}%)"

def compare_job_files(job_id: str):
    """Compare all generated files against expected outputs for a specific job."""
    print(f"\n[REPORT] Generating file comparison report for job {job_id}...")
    
    # Define all file pairs to compare
    file_pairs = [
        (f"{job_id}.4300", f"{job_id}.4300"),
        (f"{job_id}.4300.txt", f"{job_id}.4300.txt"),
        (f"{job_id}.4300.txt.length", f"{job_id}.4300.txt.length"),
        (f"{job_id}.4300.txt.new", f"{job_id}.4300.txt.new"),
        (f"{job_id}.4300.txt.suspect", f"{job_id}.4300.txt.suspect"),
        (f"{job_id}.dat.asc", f"{job_id}.dat.asc"),
        (f"{job_id}.dat.asc.11.1.d", f"{job_id}.dat.asc.11.1.d"),
        (f"{job_id}.dat.asc.11.1.p", f"{job_id}.dat.asc.11.1.p"),
        (f"{job_id}.dat.asc.11.1.p.keyed", f"{job_id}.dat.asc.11.1.p.keyed"),
        (f"{job_id}.dat.asc.11.1.s", f"{job_id}.dat.asc.11.1.s"),
        (f"{job_id}.dat.rectype", f"{job_id}.dat.rectype"),
        (f"{job_id}.dat.total", f"{job_id}.dat.total"),
        (f"{job_id}.ncpjax", f"{job_id}.ncpjax"),
        (f"{job_id}p.set", f"{job_id}p.set"),
    ]
    
    results = []
    
    for expected_name, actual_name in file_pairs:
        expected_file = LEGACY_DIR / "Expected_Outputs" / job_id / expected_name
        actual_file = OUTPUT_DIR / job_id / actual_name
        
        similarity = calculate_file_similarity(expected_file, actual_file)
        status = get_file_status(
            similarity["size_match"], 
            similarity["content_match"], 
            expected_file.exists(), 
            actual_file.exists()
        )
        
        results.append({
            "filename": expected_name,
            "expected_exists": expected_file.exists(),
            "actual_exists": actual_file.exists(),
            "status": status,
            **similarity
        })
    
    return results

def print_job_report(job_id: str, results: list):
    """Print a detailed report for a single job."""
    print(f"\n{'='*120}")
    print(f"FILE COMPARISON SUMMARY FOR JOB {job_id}")
    print(f"{'='*120}")
    print(f"{'Filename':<30} {'Status':<35} {'Size Match %':<12} {'Content Match %':<15} {'Size Diff':<12} {'Content Diff':<15}")
    print(f"{'-'*140}")
    
    total_size_match = 0
    total_content_match = 0
    valid_files = 0
    
    for result in results:
        status = result["status"]
        size_match = result["size_match"]
        content_match = result["content_match"]
        size_diff = result["size_diff"]
        content_diff = result["content_diff"]
        
        print(f"{result['filename']:<30} {status:<35} {size_match:>10.2f}% {content_match:>13.2f}% {size_diff:>10} {content_diff:>13}")
        
        if result["expected_exists"] and result["actual_exists"]:
            total_size_match += size_match
            total_content_match += content_match
            valid_files += 1
    
    print(f"{'-'*140}")
    
    if valid_files > 0:
        avg_size_match = total_size_match / valid_files
        avg_content_match = total_content_match / valid_files
        print(f"{'AVERAGE':<30} {'':<35} {avg_size_match:>10.2f}% {avg_content_match:>13.2f}% {'':<12} {'':<15}")
    else:
        avg_size_match = 0
        avg_content_match = 0
        print(f"{'AVERAGE':<30} {'':<35} {'N/A':>10} {'N/A':>13} {'':<12} {'':<15}")
    
    print(f"{'='*140}")
    print(f"Summary: {valid_files} files compared successfully")
    if valid_files > 0:
        print(f"Average Size Match: {avg_size_match:.2f}%")
        print(f"Average Content Match: {avg_content_match:.2f}%")
    else:
        print(f"Average Size Match: N/A (no files to compare)")
        print(f"Average Content Match: N/A (no files to compare)")
    print(f"{'='*140}")
    
    return {
        "job_id": job_id,
        "valid_files": valid_files,
        "avg_size_match": avg_size_match,
        "avg_content_match": avg_content_match,
        "total_files": len(results),
        "file_details": results  # Include file details for README
    }

def generate_summary_report(job_summaries: list):
    """Generate an overall summary report for all jobs."""
    print(f"\n{'='*80}")
    print(f"OVERALL SUMMARY REPORT")
    print(f"{'='*80}")
    print(f"{'Job ID':<10} {'Files':<8} {'Size Match %':<15} {'Content Match %':<18} {'Status':<15}")
    print(f"{'-'*80}")
    
    total_jobs = len(job_summaries)
    total_valid_files = 0
    total_avg_size = 0
    total_avg_content = 0
    
    for summary in job_summaries:
        status = "GOOD" if summary["avg_content_match"] > 80 else "NEEDS WORK" if summary["avg_content_match"] > 50 else "POOR"
        
        print(f"{summary['job_id']:<10} {summary['valid_files']:<8} {summary['avg_size_match']:>13.2f}% {summary['avg_content_match']:>16.2f}% {status:<15}")
        
        total_valid_files += summary["valid_files"]
        total_avg_size += summary["avg_size_match"]
        total_avg_content += summary["avg_content_match"]
    
    print(f"{'-'*80}")
    
    if total_jobs > 0:
        overall_avg_size = total_avg_size / total_jobs
        overall_avg_content = total_avg_content / total_jobs
        
        print(f"{'OVERALL':<10} {total_valid_files:<8} {overall_avg_size:>13.2f}% {overall_avg_content:>16.2f}% {'':<15}")
    
    print(f"{'='*80}")
    print(f"Total Jobs Processed: {total_jobs}")
    print(f"Total Files Compared: {total_valid_files}")
    print(f"Overall Average Size Match: {overall_avg_size:.2f}%")
    print(f"Overall Average Content Match: {overall_avg_content:.2f}%")
    print(f"{'='*80}")
    
    return {
        "total_jobs": total_jobs,
        "total_valid_files": total_valid_files,
        "overall_avg_size": overall_avg_size if total_jobs > 0 else 0,
        "overall_avg_content": overall_avg_content if total_jobs > 0 else 0
    }

def update_readme_with_report(job_summaries: list, overall_stats: dict):
    """Update the README.md file with the latest report results."""
    readme_path = MODERNIZER_DIR / "README.md"
    
    if not readme_path.exists():
        print(f"[WARN] README.md not found at {readme_path}")
        return
    
    # Read current README content
    with open(readme_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Generate the new report section
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    report_section = f"""## 📊 Latest Parity Report

*This report was generated by running `generate_report.py` on {timestamp}*

### Overall Summary
- **Total Jobs Processed**: {overall_stats['total_jobs']}
- **Total Files Compared**: {overall_stats['total_valid_files']}
- **Overall Size Match**: {overall_stats['overall_avg_size']:.2f}%
- **Overall Content Match**: {overall_stats['overall_avg_content']:.2f}%

### Job Status Details

| Job ID | Files | Size Match % | Content Match % | Status |
|--------|-------|--------------|-----------------|--------|
"""
    
    for summary in job_summaries:
        status = "🟢 GOOD" if summary["avg_content_match"] > 80 else "🟡 NEEDS WORK" if summary["avg_content_match"] > 50 else "🔴 POOR"
        if summary["valid_files"] == 0:
            status = "⚪ NO OUTPUT"
        
        report_section += f"| {summary['job_id']} | {summary['valid_files']} | {summary['avg_size_match']:.2f}% | {summary['avg_content_match']:.2f}% | {status} |\n"
    
    report_section += f"""
### File-by-File Details

"""
    
    # Add detailed file information for jobs with outputs
    for summary in job_summaries:
        if summary["valid_files"] > 0:
            report_section += f"#### Job {summary['job_id']}\n"
            report_section += f"- **Files Compared**: {summary['valid_files']}/{summary['total_files']}\n"
            report_section += f"- **Size Match**: {summary['avg_size_match']:.2f}%\n"
            report_section += f"- **Content Match**: {summary['avg_content_match']:.2f}%\n"
            report_section += f"- **Status**: {'🟢 GOOD' if summary['avg_content_match'] > 80 else '🟡 NEEDS WORK' if summary['avg_content_match'] > 50 else '🔴 POOR'}\n\n"
            
            # Add file-by-file details if available
            if 'file_details' in summary:
                report_section += f"**File Details:**\n"
                for file_detail in summary['file_details']:
                    status_emoji = "✅" if file_detail['status'].startswith('[OK]') else "❌"
                    report_section += f"- {status_emoji} `{file_detail['filename']}` - {file_detail['status']}\n"
                report_section += "\n"
        else:
            report_section += f"#### Job {summary['job_id']}\n"
            report_section += f"- **Status**: ⚪ NO OUTPUT (pipeline not run yet)\n\n"
    
    report_section += f"""
---
*To regenerate this report, run: `python generate_report.py`*

"""
    
    # Find the start of the existing report section (if any)
    start_marker = "## 📊 Latest Parity Report"
    end_marker = "---\n*To regenerate this report, run: `python generate_report.py`*"
    
    # Check if there's already a report section
    if start_marker in content:
        # Find the end of the existing report section
        start_idx = content.find(start_marker)
        end_idx = content.find(end_marker, start_idx)
        
        if end_idx != -1:
            # Replace existing report section
            end_idx += len(end_marker) + 1  # Include the end marker and newline
            new_content = content[:start_idx] + report_section + content[end_idx:]
        else:
            # If end marker not found, just replace from start marker to end of file
            new_content = content[:start_idx] + report_section
    else:
        # Insert new report section after the overview section
        overview_end = content.find("## Prerequisites")
        if overview_end != -1:
            new_content = content[:overview_end] + report_section + "\n" + content[overview_end:]
        else:
            # If no prerequisites section found, add at the beginning
            new_content = report_section + "\n" + content
    
    # Write updated content back to README
    with open(readme_path, 'w', encoding='utf-8') as f:
        f.write(new_content)
    
    print(f"[INFO] Updated README.md with latest report results")

def main():
    parser = argparse.ArgumentParser(description="Generate comprehensive file comparison reports for MBCNTR2503 jobs.")
    parser.add_argument("--jobs", nargs="+", help="Specific job IDs to process (e.g., 69172 80147). If not provided, processes all available jobs.")
    parser.add_argument("--summary-only", action="store_true", help="Show only the summary report, skip detailed job reports.")
    args = parser.parse_args()
    
    # Determine which jobs to process
    if args.jobs:
        jobs_to_process = args.jobs
    else:
        jobs_to_process = get_available_jobs()
        if not jobs_to_process:
            print("[ERROR] No jobs found in Expected_Outputs directory.")
            return
    
    print(f"[INFO] Processing {len(jobs_to_process)} jobs: {', '.join(jobs_to_process)}")
    
    job_summaries = []
    
    for job_id in jobs_to_process:
        try:
            # Check if job has expected outputs
            expected_dir = LEGACY_DIR / "Expected_Outputs" / job_id
            if not expected_dir.exists():
                print(f"[WARN] Skipping job {job_id} - no expected outputs found")
                continue
            
            # Check if job has generated outputs
            actual_dir = OUTPUT_DIR / job_id
            if not actual_dir.exists():
                print(f"[WARN] Skipping job {job_id} - no generated outputs found")
                continue
            
            # Generate report for this job
            results = compare_job_files(job_id)
            
            if not args.summary_only:
                summary = print_job_report(job_id, results)
            else:
                # Calculate summary without printing detailed report
                valid_files = sum(1 for r in results if r["expected_exists"] and r["actual_exists"])
                if valid_files > 0:
                    avg_size = sum(r["size_match"] for r in results if r["expected_exists"] and r["actual_exists"]) / valid_files
                    avg_content = sum(r["content_match"] for r in results if r["expected_exists"] and r["actual_exists"]) / valid_files
                else:
                    avg_size = 0
                    avg_content = 0
                
                summary = {
                    "job_id": job_id,
                    "valid_files": valid_files,
                    "avg_size_match": avg_size,
                    "avg_content_match": avg_content,
                    "total_files": len(results),
                    "file_details": results  # Include file details for README
                }
            
            job_summaries.append(summary)
            
        except Exception as e:
            print(f"[ERROR] Failed to process job {job_id}: {e}")
            continue
    
    # Generate overall summary
    if job_summaries:
        overall_stats = generate_summary_report(job_summaries)
        
        # Update README.md with the report
        update_readme_with_report(job_summaries, overall_stats)
    else:
        print("[ERROR] No jobs were successfully processed.")

if __name__ == "__main__":
    main()
