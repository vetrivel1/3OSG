# 🔑 KEY ENRICHMENT REQUIREMENTS SPECIFICATION

## 📋 **OVERVIEW**
This document defines the complete requirements for implementing Key Enrichment functionality based on comprehensive analysis of legacy code `cnpfilekeys.c` and `setmb2000.script`.

**Purpose**: Generate `.asc.11.1.p.keyed` files from `.asc.11.1.p` and `.asc.11.1.s` files by matching P records with S records based on account numbers and embedding key information.

---

## 🎯 **LEGACY CODE ANALYSIS**

### **Source Files Analyzed**
- ✅ `/Legacy Application/Scripts/MBCNTR2503/Original Programs+Scripts/cnpfilekeys.c` - Core algorithm
- ✅ `/Legacy Application/Scripts/MBCNTR2503/Original Programs+Scripts/setmb2000.script` - Usage parameters

### **Command Line Parameters (from setmb2000.script line 59)**
```bash
/users/programs/cnpfilekeys.out $OutLength $OutLength 4 4 7 1080 7 3 $OutPath.11.1.p $OutPath.11.1.s
```

**Parameter Mapping:**
- `argv[1]` = **1500** (Record1Length - P record length)
- `argv[2]` = **1500** (Record2Length - S record length) 
- `argv[3]` = **4** (Account1Offset - account position in P records)
- `argv[4]` = **4** (Account2Offset - account position in S records)
- `argv[5]` = **7** (AccountLength - account number field length)
- `argv[6]` = **1080** (KeyOffset - where to write key in P records)
- `argv[7]` = **7** (KeyLength - length of key number portion)
- `argv[8]` = **3** (CountLength - length of count number portion)
- `argv[9]` = **Input P file** (`.asc.11.1.p`)
- `argv[10]` = **Input S file** (`.asc.11.1.s`)
- `argv[11]` = **Not provided** (defaults to KeyFirst=1)

---

## 🧠 **ALGORITHM SPECIFICATION**

### **Core Logic (from cnpfilekeys.c lines 115-177)**

#### **1. Initialization**
```c
r2Key = -1;           // Current S record sequence number
r2KeyCount = 0;       // Count of matching S records  
r2Count = 0;          // Total S records processed
KeyFirst = 1;         // Key before Count format
```

#### **2. Main Processing Loop**
```c
while(r1>0 && r2>=0)  // While P records and S records available
```

#### **3. Account Number Comparison**
```c
ret = memcmp(r1Buffer+a1Offset, r2Buffer+a2Offset, AccountLength);
```
- **ret > 0**: P account > S account → Advance S record
- **ret < 0**: P account < S account → Write P record with current key
- **ret = 0**: P account = S account → Match found

#### **4. Key Generation Logic**

**When Match Found (ret == 0):**
```c
if(r2Key == -1)
    r2Key = r2Count;    // First match - capture S record number
r2KeyCount++;           // Increment match counter
```

**When Writing P Record:**
```c
if(r2Key == -1)
    // No matches found
    memset(r1Buffer+r2KeyOffset, '0', r2KeyLength+r2KeyCountLength);
else
    // Matches found - format key
    if(KeyFirst)
        sprintf(scratch, "%0.*d%0.*d", r2KeyLength, r2Key, r2KeyCountLength, r2KeyCount);
    else
        sprintf(scratch, "%0.*d%0.*d", r2KeyCountLength, r2KeyCount, r2KeyLength, r2Key);
    memcpy(r1Buffer+r2KeyOffset, scratch, strlen(scratch));
```

#### **5. Key Format**
- **Total Length**: 10 characters (7 + 3)
- **Format**: `sprintf("%07d%03d", r2Key, r2KeyCount)` 
- **Example**: `"0000001003"` = S record #1, count 3
- **No Match**: `"0000000000"` = All zeros

---

## 📊 **EXPECTED BEHAVIOR VALIDATION**

### **Test Case: Job 69172**
Based on legacy output analysis:

| P Record | Account | Expected Key | S Record # | Count |
|----------|---------|--------------|------------|-------|
| 0        | ?       | 0000001003   | 1          | 3     |
| 1        | ?       | 0000004003   | 4          | 3     |  
| 2        | ?       | 0000007003   | 7          | 3     |
| 3        | ?       | 0000010003   | 10         | 3     |
| 4        | ?       | 0000013003   | 13         | 3     |

**Pattern Analysis**: Each P record matches exactly 3 consecutive S records.

---

## 🏗️ **IMPLEMENTATION REQUIREMENTS**

### **✅ COMPLETED ANALYSIS**
- [x] Legacy code analysis (`cnpfilekeys.c`)
- [x] Parameter extraction (`setmb2000.script`)
- [x] Algorithm specification
- [x] Expected output validation

### **✅ COMPLETED IMPLEMENTATION**

#### **Phase 1: Core Implementation** ✅
- [x] **KeyEnrichmentProcessor.cs** class
  - [x] Constructor with parameters matching legacy command line
  - [x] `ProcessFiles(pFilePath, sFilePath, outputPath)` method
  - [x] Account number extraction and comparison
  - [x] Key generation logic (exact sprintf replication)
  - [x] File I/O handling (1500-byte records)

#### **Phase 2: Integration** ✅
- [x] CLI command `enrich-keys` 
- [x] Error handling and validation

#### **Phase 3: Validation** ✅
- [x] Byte-level parity testing across all jobs
- [x] **100% PERFECT PARITY ACHIEVED**

### **🎯 VALIDATION RESULTS**
| Job   | P Records | Status              | Parity |
|-------|-----------|---------------------|---------|
| 69172 | 5         | ✅ PERFECT MATCH    | 100%   |
| 80147 | 24        | ✅ PERFECT MATCH    | 100%   |
| 80299 | 59        | ✅ PERFECT MATCH    | 100%   |
| 80362 | 42        | ✅ PERFECT MATCH    | 100%   |

**Total Records Processed**: 130 P records  
**Overall Parity**: **100% PERFECT** 🎉

---

## 🚨 **CRITICAL IMPLEMENTATION RULES**

### **❌ ABSOLUTELY NO HARDCODING**
- No hardcoded key values or patterns
- All logic must derive from account number matching
- All parameters must be configurable

### **✅ LEGACY CODE FIDELITY**
- Exact replication of `memcmp` account comparison
- Exact replication of `sprintf` key formatting  
- Exact replication of file I/O patterns
- Exact replication of buffer management

### **🔍 VALIDATION REQUIREMENTS**
- 100% byte-level parity with legacy output
- All test jobs must pass (69172, 80147, 80299, 80362)
- No assumptions about data patterns

---

## 📝 **PROGRESS TRACKING**

### **Current Status**: ✅ **MILESTONE COMPLETED** 
### **Achievement**: 🏆 **100% PERFECT PARITY ACROSS ALL JOBS**

**Implementation Date**: 2025-09-26  
**Success Metrics**: 
- ✅ Zero hardcoded values
- ✅ Exact legacy algorithm replication  
- ✅ 100% byte-level parity
- ✅ All 4 test jobs validated
- ✅ 130 P records processed successfully

**Confidence Level**: **MAXIMUM** (validated against legacy outputs)

---

## 🔗 **REFERENCES**
- Legacy source: `cnpfilekeys.c` lines 1-185
- Usage context: `setmb2000.script` line 59
- Test data: `/Legacy Application/Expected_Outputs/*/dat.asc.11.1.p.keyed`
