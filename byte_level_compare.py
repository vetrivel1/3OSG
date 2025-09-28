
import sys

def compare_records(file1_path, file2_path, record_size=2000):
    try:
        with open(file1_path, 'rb') as f1, open(file2_path, 'rb') as f2:
            record1 = f1.read(record_size)
            record2 = f2.read(record_size)

            if len(record1) != record_size or len(record2) != record_size:
                print("Error: Could not read a full record from one or both files.")
                return

            diffs_found = 0
            for i in range(record_size):
                if record1[i] != record2[i]:
                    if diffs_found == 0:
                        print("First 10 differences found (Offset | Expected | Actual):")
                    if diffs_found < 10:
                        print(f"  {i:04d}  |   0x{record1[i]:02x}   |  0x{record2[i]:02x}")
                    diffs_found += 1
            
            if diffs_found == 0:
                print("🎉 The first records are identical at the byte level.")
            else:
                print(f"\nFound a total of {diffs_found} differing bytes in the first record.")

    except FileNotFoundError as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    job_id = "69172"
    expected_file = f"Legacy Application/Expected_Outputs/{job_id}/{job_id}p.set"
    actual_file = f"MBCNTR2503.Modernizer/out/{job_id}/{job_id}p.set"
    
    print(f"Comparing first record of:\n- Expected: {expected_file}\n- Actual:   {actual_file}\n")
    compare_records(expected_file, actual_file)
