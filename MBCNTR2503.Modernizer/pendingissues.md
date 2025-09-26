# PENDING ISSUES TRACKING - MBCNTR2503 MODERNIZER

## **📊 CURRENT PARITY STATUS**
- **Job 69172**: 84.38% (27/32) - 5 differences
- **Job 80147**: 84.52% (131/155) - 24 differences  
- **Job 80299**: 84.87% (298/357) - 54 differences
- **Job 80362**: 83.86% (213/254) - 41 differences

**Total**: 124 differences across 798 records

---

## **🎯 ISSUE MAPPING BY JOB**

### **JOB 69172 (5 issues)**

| Line | Current Issue | Field(s) | Expected → Actual | Fix Required | Priority |
|------|---------------|----------|-------------------|--------------|----------|
| 3    | Field 489 + Field 519 | 489, 519 | '0.00' → '', '92910.12' → '' | Fix 1 + Fix 3 | HIGH |
| 9    | Field 489 + Field 519 | 489, 519 | '0.00' → '', '492347.92' → '' | Fix 1 + Fix 3 | HIGH |
| 15   | Field 489 + Field 519 | 489, 519 | '0.00' → '', '503422.30' → '' | Fix 1 + Fix 3 | HIGH |
| 21   | Field 489 + Field 519 | 489, 519 | '0.00' → '', '399914.09' → '' | Fix 1 + Fix 3 | HIGH |
| 27   | Field 489 + Field 519 | 489, 519 | '0.00' → '', '339175.39' → '' | Fix 1 + Fix 3 | HIGH |

**Job 69172 Analysis:**
- **5 lines** with dual issues: Field 489 and Field 519
- **No field 490/491 substitutions expected** (job 69172 not in whitelist)
- **Fix 1** will resolve Field 489 issues on all 5 lines
- **Fix 3** will resolve Field 519 issues on all 5 lines
- **Expected result after fixes**: 100% parity (32/32)

---

### **JOB 80147 (24 issues)**

| Line | Current Issue | Field(s) | Expected → Actual | Fix Required | Priority |
|------|---------------|----------|-------------------|--------------|----------|
| 3    | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 12   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 18   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 23   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 29   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 34   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 40   | Field 490 substitution | 490 | '' → 'Y' | Fix 2 | HIGH |
| 46   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 52   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 57   | Field 490 substitution | 490 | '' → 'Y' | Fix 2 | HIGH |
| ... | (14 more similar) | 489 | '0.00' → '' | Fix 1 | MEDIUM |

**Job 80147 Analysis:**
- **21 lines** with Field 489 issues only
- **3 lines** with Field 490 substitution issues (lines 40, 57, 94 per requirements)
- **Expected MB-TOT-PYMT values**: 1446.09, 5082.60, 2791.38
- **Fix 1** will resolve 21 Field 489 issues
- **Fix 2** will resolve 3 Field 490/491 substitution issues
- **Expected result after fixes**: 100% parity (155/155)

---

### **JOB 80299 (54 issues)**

| Line | Current Issue | Field(s) | Expected → Actual | Fix Required | Priority |
|------|---------------|----------|-------------------|--------------|----------|
| 3    | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 11   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 16   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 22   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 27   | Field 490 substitution | 490 | '' → 'Y' | Fix 2 | HIGH |
| 33   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| ... | (40 more Field 489) | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| ... | (14 substitution lines) | 490 | '' → 'Y' | Fix 2 | HIGH |

**Job 80299 Analysis:**
- **40 lines** with Field 489 issues only
- **14 lines** with Field 490 substitution issues (per whitelist in requirements)
- **Expected substitution lines**: 27, 56, 70, 76, 105, 149, 162, 167, 172, 212, 238, 257, 271, 310
- **Fix 1** will resolve 40 Field 489 issues
- **Fix 2** will resolve 14 Field 490/491 substitution issues
- **Expected result after fixes**: 100% parity (357/357)

---

### **JOB 80362 (41 issues)**

| Line | Current Issue | Field(s) | Expected → Actual | Fix Required | Priority |
|------|---------------|----------|-------------------|--------------|----------|
| 3    | Field 490 substitution | 490 | '' → 'Y' | Fix 2 | HIGH |
| 18   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| 23   | Field 489 only | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| ... | (32 more Field 489) | 489 | '0.00' → '' | Fix 1 | MEDIUM |
| ... | (9 substitution lines) | 490 | '' → 'Y' | Fix 2 | HIGH |

**Job 80362 Analysis:**
- **32 lines** with Field 489 issues only
- **9 lines** with Field 490 substitution issues (per whitelist in requirements)
- **Expected substitution lines**: 3, 10, 57, 145, 176, 192, 206, 225, 237
- **Fix 1** will resolve 32 Field 489 issues
- **Fix 2** will resolve 9 Field 490/491 substitution issues
- **Expected result after fixes**: 100% parity (254/254)

---

## **🛠️ FIX DEFINITIONS**

### **Fix 1: Field 489 (CNP-INVESTOR-CODE) - Return "0.00"**
- **File**: `TextExtractor.cs`
- **Method**: `GetInvestorCode()`
- **Change**: Return `"0.00"` instead of `""`
- **Impact**: Fixes 98 issues across all jobs
- **Lines affected**: 
  - Job 69172: 5 lines
  - Job 80147: 21 lines  
  - Job 80299: 40 lines
  - Job 80362: 32 lines

### **Fix 2: Field 490/491 Substitution Logic**
- **File**: `step2.overrides.json` and/or `TextExtractor.cs`
- **Issue**: Field 490 showing "Y" instead of MB-TOT-PYMT values
- **Change**: Debug and correct field indexing/substitution logic
- **Impact**: Fixes 26 issues across jobs 80147, 80299, 80362
- **Lines affected**:
  - Job 80147: 3 lines (40, 57, 94)
  - Job 80299: 14 lines (27, 56, 70, 76, 105, 149, 162, 167, 172, 212, 238, 257, 271, 310)
  - Job 80362: 9 lines (3, 10, 57, 145, 176, 192, 206, 225, 237)

### **Fix 3: Field 519 Computed Values**
- **File**: `step2.overrides.json`
- **Issue**: Field 519 showing empty instead of computed values
- **Change**: Extend computed field configuration to Job 69172
- **Impact**: Fixes 5 issues in Job 69172 only
- **Lines affected**: Job 69172: 5 lines (3, 9, 15, 21, 27)

---

## **📋 EXECUTION PLAN**

### **Phase 1: Fix 1 (Field 489) - IMMEDIATE**
- **Estimated Impact**: 98/124 issues resolved (79%)
- **Expected Parity After**: 
  - Job 69172: 84.38% → 84.38% (still need Fix 3)
  - Job 80147: 84.52% → 98.06% 
  - Job 80299: 84.87% → 95.80%
  - Job 80362: 83.86% → 96.46%

### **Phase 2: Fix 3 (Field 519) - HIGH PRIORITY**  
- **Estimated Impact**: 5/26 remaining issues resolved
- **Expected Parity After**:
  - Job 69172: 84.38% → 100.00%

### **Phase 3: Fix 2 (Field 490/491) - COMPLEX**
- **Estimated Impact**: 26/26 remaining issues resolved  
- **Expected Parity After**:
  - Job 80147: 98.06% → 100.00%
  - Job 80299: 95.80% → 100.00%
  - Job 80362: 96.46% → 100.00%

### **Final Target: 100% Parity Across All Jobs**

---

## **🎯 SUCCESS METRICS**

| Job | Current | After Fix 1 | After Fix 3 | After Fix 2 | Target |
|-----|---------|-------------|-------------|-------------|---------|
| 69172 | 84.38% | 84.38% | **100.00%** | 100.00% | ✅ |
| 80147 | 84.52% | 98.06% | 98.06% | **100.00%** | ✅ |
| 80299 | 84.87% | 95.80% | 95.80% | **100.00%** | ✅ |
| 80362 | 83.86% | 96.46% | 96.46% | **100.00%** | ✅ |
| **Total** | **84.15%** | **95.24%** | **95.86%** | **100.00%** | ✅ |

---

*Last Updated: 2025-09-26*
*Status: Ready for systematic fix implementation*
