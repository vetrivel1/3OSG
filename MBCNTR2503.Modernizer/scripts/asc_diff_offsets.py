#!/usr/bin/env python3
import argparse
from pathlib import Path
from collections import Counter, defaultdict

def load(path: Path) -> bytes:
    return path.read_bytes()

def main():
    p = argparse.ArgumentParser(description="Analyze byte-level diffs and report recurring within-record offsets")
    p.add_argument("expected", help="Path to expected file (legacy)")
    p.add_argument("actual", help="Path to actual file (modern)")
    p.add_argument("--rec-len", type=int, default=1500, help="Logical record length for modulo bucketing (default: 1500)")
    p.add_argument("--top", type=int, default=50, help="Top differing absolute offsets to print")
    args = p.parse_args()

    exp = load(Path(args.expected))
    act = load(Path(args.actual))
    max_len = min(len(exp), len(act))

    diffs = []
    for i in range(max_len):
        if exp[i] != act[i]:
            diffs.append(i+1)  # 1-based like cmp

    print(f"Total diffs: {len(diffs)} (file sizes: exp={len(exp)}, act={len(act)})")
    if not diffs:
        return

    # Print first N absolute positions with hex bytes
    print("\nFirst diffs (absolute):")
    for off in diffs[:args.top]:
        e = exp[off-1]
        a = act[off-1]
        print(f"  {off:>8}  exp=0x{e:02X} act=0x{a:02X}")

    # Bucket by modulo rec-len
    bucket = Counter(((off-1) % args.rec_len) for off in diffs)
    print("\nModulo buckets (offset_in_record -> count):")
    for off_in_rec, cnt in sorted(bucket.items()):
        print(f"  {off_in_rec:>5} -> {cnt}")

    # Group into small contiguous ranges within record for suggestion
    ranges = []
    for off_in_rec in sorted(bucket):
        if not ranges or off_in_rec > ranges[-1][1] + 1:
            ranges.append([off_in_rec, off_in_rec])
        else:
            ranges[-1][1] = off_in_rec
    print("\nSuggested within-record ranges (for repeatEvery rules):")
    for a,b in ranges:
        print(f"  [{a}..{b}] count={sum(bucket[x] for x in range(a,b+1))}")

if __name__ == "__main__":
    main()


