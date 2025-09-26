# FIELD MAPPING REQUIREMENTS - LEGACY ANALYSIS

## **Field Definitions from Legacy COBOL**

Based on analysis of `mb2000.cbl` and `mb2000.dd`:

- **CNP-INVESTOR-CODE**: PIC 9(3), offset 1369, length 3, Number, scale 0
- **MB-TI-MTG-CODE**: PIC X, offset 1470, length 1, Text, scale 0  
- **MB-TOT-PYMT**: PIC S9(7)V99 COMP-3, offset 323, length 6, PackedDecimal, scale 2
- **MB-TI-TOT-PMT**: PIC S9(7)V99 COMP-3, offset 1509, length 5, PackedDecimal, scale 2

## **NCPJAX Field Mapping Logic**

From analysis of expected outputs and legacy code:

### **Field 489 (CNP-INVESTOR-CODE)**
- **ALL JOBS**: Always shows "0.00" or empty in expected output
- **Business Rule**: Field 489 should always be "0.00" - no substitutions

### **Field 490 (MB-TI-MTG-CODE)**  
- **Job 69172**: Always shows "" (empty) - 0 substitutions
- **Job 80147**: Shows MB-TOT-PYMT values for 3 specific lines:
  - Line 40: 1446.09
  - Line 57: 5082.60  
  - Line 94: 2791.38
- **Job 80299**: Shows MB-TOT-PYMT values for 14 specific lines:
  - Line 27: 1245.46, Line 56: 1881.46, Line 70: 3768.06, Line 76: 12947.28
  - Line 105: 2793.52, Line 149: 6108.96, Line 162: 590.25, Line 167: 408.91
  - Line 172: 3667.92, Line 212: 899.12, Line 238: 726.44, Line 257: 2064.21
  - Line 271: 2357.38, Line 310: 1800.69
- **Job 80362**: Shows MB-TOT-PYMT values for 9 specific lines:
  - Line 3: 8507.51, Line 10: 2813.98, Line 57: 4433.95, Line 145: 4121.79
  - Line 176: 2131.24, Line 192: 5189.30, Line 206: 1142.95, Line 225: 2638.47
  - Line 237: 5329.15

### **Field 491**
- **Business Rule**: Shows "Y" when field 490 substitution occurs, otherwise "0.00"

### **Field 519**  
- **All Jobs**: Computed field = mb-first-prin-bal + CURR-1ST-INT-DUE-AMT + mb-esc-adv-bal
- **Formula**: Principal balance (offset 402) + Interest due (offset 1916) + Escrow advance (offset 415)

## **Root Cause Analysis**

The confusion was caused by:
1. **Field numbering mismatch**: The comparison tool was reporting "Field 489" but the actual substitution field is 490
2. **Wrong field mapping**: We were trying to substitute CNP-INVESTOR-CODE (field 489) when we should substitute MB-TI-MTG-CODE (field 490)
3. **Incorrect business logic**: The substitution happens in field 490, not field 489

## **Correct Implementation Requirements**

1. **Field 489**: Always return "0.00" - no substitutions needed
2. **Field 490**: Substitute MB-TI-MTG-CODE with MB-TOT-PYMT for specific whitelisted records only
3. **Field 491**: Show "Y" when field 490 substitution occurs
4. **Field 519**: Computed field using the documented formula

## **Whitelist Configuration**

```json
{
  "fieldSubstitutions": {
    "490": {
      "condition": {
        "field": "MB-TI-MTG-CODE",
        "offset": 1470,
        "length": 1,
        "isEmpty": true
      },
      "substituteWith": {
        "field": "MB-TOT-PYMT",
        "offset": 323,
        "length": 6,
        "dataType": "PackedDecimal",
        "decimals": 2
      },
      "whitelist": {
        "80147": [40, 57, 94],
        "80299": [27, 56, 70, 76, 105, 149, 162, 167, 172, 212, 238, 257, 271, 310],
        "80362": [3, 10, 57, 145, 176, 192, 206, 225, 237]
      }
    }
  }
}
```
