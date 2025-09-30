#!/usr/bin/env python3
"""Generate stage-based parity tables for all available jobs."""

from __future__ import annotations

import math
from pathlib import Path
from typing import Dict, Iterable, List, Sequence


# --- Paths ---
WORKSPACE_ROOT = Path(__file__).parent.parent
MODERNIZER_DIR = WORKSPACE_ROOT / "MBCNTR2503.Modernizer"
EXPECTED_OUTPUTS_DIR = MODERNIZER_DIR / "Expected_Outputs"
ACTUAL_OUTPUTS_DIR = MODERNIZER_DIR / "out"


# --- Stage definitions ---
STAGES: Sequence[Dict[str, object]] = (
    {
        "name": "Stage 1",
        "description": "INITIAL SET UP & CONTAINER PROCESSING",
        "script": "mbcntr2503.script",
        "patterns": (
            "*.se1",
            "*.4300",
            "*.dat.rectype",
            "*.dat.total",
            "*.4300.txt",
            "*.4300.txt.suspect",
            "*.4300.txt.new",
            "*.4300.txt.length",
            "*.ncpjax",
        ),
    },
    {
        "name": "Stage 2",
        "description": "DATA CONVERSION & ASCII PROCESSING",
        "script": "estmb2000.script",
        "patterns": (
            "*.dat.asc",
            "*.dat.asc.11.1.p",
            "*.dat.asc.11.1.s",
            "*.dat.asc.11.1.d",
            "*.dat.asc.11.1.p.keyed",
            "*p.asc",
            "*p.set",
            "*e.asc",
            "*e.txt",
        ),
    },
)


# --- Helpers ---
def list_jobs() -> List[str]:
    if not EXPECTED_OUTPUTS_DIR.exists():
        return []

    jobs: List[str] = []
    for item in EXPECTED_OUTPUTS_DIR.iterdir():
        if item.is_dir() and item.name.isdigit():
            jobs.append(item.name)

    return sorted(jobs)


def resolve_filename(job_id: str, pattern: str) -> str:
    if not pattern.startswith("*"):
        raise ValueError(f"Unsupported pattern '{pattern}'. Patterns must begin with '*'.")
    suffix = pattern[1:]
    if suffix.startswith("."):
        return f"{job_id}{suffix}"
    return f"{job_id}{suffix}"


def calculate_file_similarity(expected_file: Path, actual_file: Path) -> Dict[str, float]:
    if not expected_file.exists() or not actual_file.exists():
        return {
            "size_match": 0.0,
            "content_match": 0.0,
        }

    size1 = expected_file.stat().st_size
    size2 = actual_file.stat().st_size

    if max(size1, size2) == 0:
        size_match = 100.0
    else:
        size_match = (1.0 - (abs(size1 - size2) / max(size1, size2))) * 100

    with expected_file.open("rb") as f1, actual_file.open("rb") as f2:
        content1 = f1.read()
        content2 = f2.read()

    max_len = max(len(content1), len(content2))
    if max_len == 0:
        content_match = 100.0
    else:
        matches = sum(1 for a, b in zip(content1, content2) if a == b)
        content_match = (matches / max_len) * 100

    return {
        "size_match": size_match,
        "content_match": content_match,
    }


def format_percentage(value: float) -> str:
    rounded = round(value, 2)
    if math.isclose(rounded, round(rounded)):
        text = str(int(round(rounded)))
    else:
        text = (f"{rounded:.2f}").rstrip("0").rstrip(".")
    return f"{text}%"


def render_stage_table(stage: Dict[str, object], jobs: Sequence[str]) -> None:
    name = stage["name"]
    description = stage["description"]
    script = stage["script"]
    patterns = stage["patterns"]

    print()
    print(f"{name}: {description} ({script})")

    if not jobs:
        print("No jobs available.")
        return

    column_widths = [max(len("file name"), max(len(pattern) for pattern in patterns))]
    column_widths.extend(max(len(job), 8) for job in jobs)

    header_cells = ["file name"] + list(jobs)
    print(" ".join(cell.ljust(width) for cell, width in zip(header_cells, column_widths)))

    for pattern in patterns:
        row_cells: List[str] = [pattern]
        for job in jobs:
            filename = resolve_filename(job, pattern)
            expected_file = EXPECTED_OUTPUTS_DIR / job / filename
            actual_file = ACTUAL_OUTPUTS_DIR / job / filename

            if not expected_file.exists():
                row_cells.append("MISSING-E")
                continue
            if not actual_file.exists():
                row_cells.append("MISSING-A")
                continue

            similarity = calculate_file_similarity(expected_file, actual_file)
            row_cells.append(format_percentage(similarity["content_match"]))

        print(" ".join(cell.ljust(width) for cell, width in zip(row_cells, column_widths)))


def main() -> None:
    jobs = list_jobs()
    if not jobs:
        print("[ERROR] No jobs found in Expected_Outputs directory.")
        return

    print("[INFO] Stage parity summary for jobs: " + ", ".join(jobs))

    for stage in STAGES:
        render_stage_table(stage, jobs)


if __name__ == "__main__":  # pragma: no cover
    main()


