#!/usr/bin/env python3
import argparse

def load_intervals(path):
    intervals = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('*'):
                continue
            parts = [p.strip() for p in line.split(',')]
            if len(parts) < 4:
                continue
            name = parts[0]
            try:
                offset = int(parts[1])
                length = int(parts[2])
            except ValueError:
                continue
            intervals.append((offset, offset + length, name))
    return sorted(intervals, key=lambda x: x[0])


def analyze(intervals):
    gaps = []
    overlaps = []
    if not intervals:
        return gaps, overlaps
    # Determine record start and end
    start = intervals[0][0]
    prev_end = intervals[0][1]
    for off, end, name in intervals[1:]:
        if off > prev_end:
            gaps.append((prev_end, off))
        if off < prev_end:
            overlaps.append((off, prev_end, name))
        prev_end = max(prev_end, end)
    return gaps, overlaps


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--dd', required=True, help='Path to DD CSV file')
    args = parser.parse_args()

    intervals = load_intervals(args.dd)
    gaps, overlaps = analyze(intervals)
    print('Gaps (start, end):')
    for g in gaps:
        print(f"  {g[0]} to {g[1]}")
    print('Overlaps (offset, prev_end, field):')
    for o in overlaps:
        print(f"  field starting at {o[0]} overlaps previous end {o[1]}: {o[2]}")

if __name__ == '__main__':
    main()
