import sys

def unpack_comp3(hex_str, scale):
    data = bytes.fromhex(hex_str)
    digits = []
    for i, b in enumerate(data):
        hi = b >> 4
        lo = b & 0x0F
        digits.append(str(hi))
        if i < len(data) - 1:
            digits.append(str(lo))
    num_str = ''.join(digits)
    # Determine sign from low nibble of last byte (not needed for hex_str '40..')
    sign = 1
    # Compute scaled value
    value = int(num_str) * sign / (10 ** scale)
    print(f"Hex bytes: {hex_str}")
    print(f"Unpacked digits: {num_str}")
    print(f"Scaled (scale={scale}): {value:.2f}")

if __name__ == '__main__':
    # MB-ARM-FULLY-AM-PI: 6 bytes at hex 404040404040, scale=2
    unpack_comp3('404040404040', 2)
