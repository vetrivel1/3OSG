### Legacy Application (MBCNTR2503) – Analysis [Living Document]

This document captures the reverse‑engineered understanding of the legacy mortgage billing and container processing application under `Legacy Application`. It is intended to be updated incrementally as we explore more.

Updated: 2025‑09‑24

---

### Purpose and scope
- **Goal**: Provide a concise, navigable map of components, data flow, and outputs to support modernization and parity validation.
- **In scope**: Directory layout, key scripts/programs, DD layouts, processing stages, artifact naming, dependencies, verification references, and open questions.

---

### High‑level summary
- **What it does**: Transforms monthly mortgage billing input into compliant container outputs for print and e‑bill channels.
- **Tech stack**: Unix shell orchestration, C utilities (EBCDIC/ASCII conversion, extract/validate/split/merge), COBOL (MB2000 format), Data Definition files for layouts/mappings.

---

### Directory map (key areas)
- **Scripts and programs**: `Legacy Application/Scripts/MBCNTR2503`
  - `Original Programs+Scripts/`
    - Orchestration: `mbcntr2503.script`, `ncpcntr5v2.script`, `setmb2000.script`, `job1.script`
    - C utilities: `ncpcntr0.c`, `ncpcntrextract.c`, `ncpcntrextractvalidation.c`, `ncpcntr5.c`, `cntrvalue.c`, `mbcnvt0.c`, `ncpsplitall.c`, `cnpfilekeys.c`, `cnpsplit4.c`, plus helpers (`ebc2asc.c`, `asc2ebc.c`, packed/zoned decimal, etc.)
    - COBOL: `setmb2000.cbl` (with `mb1500.cbl`, `mb2000.cbl` copybooks)
    - Reference: `2503supptable.txt`, `mailer_id.txt`, headers like `cnp01.h`
  - `mblps/`
    - Data definitions/mappings: `*.dd`, `*.dd.cbl`, `*.css` (control/spec files), `*.iomap`, `*.fields`, `*.flexfields`, `ddcontrol.txt`

- **Inputs**: `Legacy Application/Input/` → sample inputs like `69172.dat`, `80147.dat`, `80299.dat`, `80362.dat`, `record3_full.dat`

- **Expected outputs**: `Legacy Application/Expected_Outputs/<job>/` → golden artifacts for each stage (see Flow and Artifacts below)

- **Documents/notes**:
  - `mbcntr2503-bre-v4.txt` – readable BRE describing workflow and artifacts
  - `ebcdic to ascii converter ibm037 converter mixed fields c#.txt` – modernization note for IBM037 handling in C#

---

### End‑to‑end flow (stages and artifacts)
1) **Job setup and validation**
   - Script: `mbcntr2503.script` calls `job1.script` to validate job number (e.g., `69172`).

2) **Container Step 1 – normalize/validate** (script: `ncpcntr5v2.script`)
   - `ncpcntr0.c`: reads `<job>.dat`, applies DD/IOMAP, converts EBCDIC→ASCII → outputs:
     - `<job>.4300` (standardized ASCII container)
     - `<job>.dat.rectype`, `<job>.dat.total`
   - `ncpcntrextract.c`: extracts fields to text → `<job>.4300.txt`
   - `ncpcntrextractvalidation.c`: flags anomalies → `<job>.4300.txt.suspect`
   - `ncpcntr5.c`: merges text with trailing binary segment → `<job>.4300.txt.new`, `<job>.4300.txt.length`
   - `cntrvalue.c`: reads `ddcontrol.txt` → `<job>.ncpjax` (and optionally `<job>.cntrkey`)

3) **MB2000 conversion (expand to 2000‑byte MBILL)** (script: `setmb2000.script`)
   - `mbcnvt0.c`: `<job>.dat` → `<job>.dat.asc` (ASCII)
   - `ncpsplitall.c`: split ASCII by key → `<job>.dat.asc.11.1.[p|s|d]`
   - `cnpfilekeys.c`: enrich primary with key/count → `<job>.dat.asc.11.1.p.keyed`
   - `setmb2000.cbl`: apply MB2000 rules → `<job>p.asc` → `<job>p.set`

4) **E‑bill split and grouping**
   - `cnpsplit4.c`: route by selection list → `<job>e.txt`, `<job>e.asc`, `<job>p.asc`
   - Grouping/rollups: `<job>.all.grp`, `<job>p.cntr.grp*`, `<job>e.cntr.grp*`, `.total`, `.err`, `.sample`, `.remit3`, etc.

---

### Artifact naming (examples)
- Normalization set: `<job>.4300`, `.rectype`, `.total`, `.4300.txt`, `.4300.txt.new`, `.4300.txt.length`, `.4300.txt.suspect`
- ASCII/MB2000 set: `<job>.dat.asc`, `.asc.11.1.[p|s|d]`, `.p.keyed`, `<job>p.asc`, `<job>p.set`
- Channel routing: `<job>e.txt`, `<job>e.asc`, plus `*cntr.grp*`
- Tracking: `<job>.ncpjax` (and possibly `<job>.cntrkey`)

---

### Key components (what to tweak where)
- **C programs** (under `Original Programs+Scripts/`)
  - Input standardization: `ncpcntr0.c`
  - Field extraction: `ncpcntrextract.c`
  - Data quality: `ncpcntrextractvalidation.c`
  - Final record reconstruction: `ncpcntr5.c`
  - Identifier derivation: `cntrvalue.c` (uses `ddcontrol.txt`)
  - MBILL path: `mbcnvt0.c` → `ncpsplitall.c` → `cnpfilekeys.c` → `setmb2000.cbl`
  - E‑bill split: `cnpsplit4.c`

- **Data definitions and mappings** (under `mblps/`)
  - Layouts/mappings: `mblps.dd`, `mblps.dd.iomap`, `*.dd.cbl`, `*.fields`, `*.flexfields`
  - Options: `option.*.dd` files
  - Control: `ddcontrol.txt`

---

### Field layouts and mappings (mblps)
- **Core field layout (mblps.dd)**
  - Defines fixed positions, lengths, and types (Text, Number, Packed Number) for 4000‑byte records.
  - Highlights:
    - Identifiers and addressing: `MB-ACCOUNT`, `MB-BILL-NAME`, `MB-BILL-LINE-2..6`, `MB-BILL-CITY/STATE/ZIP`.
    - Dates: split components (`YY/MM/DD`) and rolled (`-R`) numeric forms for sorting/validation.
    - Monetary values: stored as packed decimals with 2 implied decimals (e.g., `MB-TOTAL-AMOUNT-DUE`).
    - Client/option overlays: `MB-0133-*`, `MB-0140-*`, `MB-0277-*`, `MB-0346-*`, `MB-0547-*`, `MB-5020-*` within `MB-CLIENT-FIELDS` area.
    - Routing/tracking: `CONTAINER-KEY`, `MB-JOB`, `MB-TRAN-KEY/COUNT`.
  - Example snippet:
```1:6:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/mblps.dd
MB-CLIENT3, 0, 3, Number, 0
MB-ACCOUNT, 10, 7, Packed Number, 0
MB-FORMATTED-ACCOUNT, 17, 10, Number, 0
MB-SSN, 37, 5, Packed Number, 0
MB-SSN-TIN-CODE, 42, 1, Text, 0
MB-CO-SSN, 43, 5, Packed Number, 0
```

- **IOMAP mappings (mblps.dd.iomap)**
  - Maps internal field names to external output/semantic fields; includes computed fields (e.g., `StatementDate` from `MB-STATEMENT-MM/DD/YY`).
  - Provides downstream names: `Account`, `CustomerBase`, `ProductNumber`, `TrackingId`, etc.
  - Example snippet:
```1:10:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/mblps.dd.iomap
MB-BILL-NAME, Address1
MB-BILL-LINE-2, Address2
MB-BILL-LINE-3, Address3
MB-BILL-LINE-4, Address4
MB-BILL-LINE-5, Address5
MB-BILL-CITY, City
MB-BILL-STATE, State
MB-ZIP-5, Zip5
MB-ZIP-4, Zip4
MB-ACCOUNT, Account
```

- **Flex and dynamic fields**
  - `mblps.fields` enumerates extractable fields; `mblps.flexfields` defines named flexible fields present in the layout.
  - Current flex field names:
```1:4:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/mblps.flexfields
FlexField1
FlexField2
FlexField3
FlexField4
```

- **Controls (ddcontrol.txt)**
  - Specifies record format, encoding, and the DDLIST used by `ncpcntr0.c` to interpret records per type.
  - Indicates presence of `NCPJAX`/container key extraction via `mblps.dd` entry.
  - Example snippet:
```1:10:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/ddcontrol.txt
FIXED, 4000
EBCDIC
DDLIST
mba.dd, A, 11, A0001, 3, HEADER
mbd.dd, D, 11, D0001, 4, HEADER
mbp.dd, P, 11, P0001, 0, PRIMARY
mblps.dd, , , , x, NCPJAX
mbp.dd, 0, 11, P0002, x, SECONDARY
mbf.dd, F, 11, F0001, x, SECONDARY
mbu.dd, U, 11, U0001, x, SECONDARY
```

- **Option overrides (example: option.0140.dd)**
  - Adds client/option‑specific fields in the `MB-CLIENT-FIELDS` area; interacts with corresponding `MB-0140-*` fields in `mblps.dd`.
  - Example fields:
```1:2:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0140.dd
MB-0140-SAMPLE-CODE, 1077, 7, Text, 0
OptOutCod, 1070, 1, Text, 0
```

Notes:
- Treat packed numeric fields as BCD with implied decimals per the trailing precision column.
- Use IOMAP output names consistently when comparing to modernized schema.

### Monetary fields and precision (packed/scale)
| Field | Type | Scale | Meaning |
|---|---|---:|---|
| MB-TOTAL-AMOUNT-DUE | Packed Number | 2 | Total amount due this statement |
| MB-PAYMENT-AMOUNT | Packed Number | 2 | Scheduled payment amount |
| MB-FIRST-P-I | Packed Number | 2 | Principal + interest (primary) |
| MB-ESCROW-PAYMENT | Packed Number | 2 | Monthly escrow portion |
| MB-FIRST-PRIN-BAL | Packed Number | 2 | Principal balance (primary loan) |
| MB-ESCROW-BAL | Packed Number | 2 | Escrow balance |
| MB-SUSPENSE-BAL | Packed Number | 2 | Suspense balance |
| MB-ACCRUED-LATE-CHG | Packed Number | 2 | Accrued late charges |
| MB-DEFERRED-INT | Packed Number | 2 | Deferred/unpaid interest |
| MB-OTHER-FEES | Packed Number | 2 | Other fees owed |
| MB-CORP-ADV | Packed Number | 2 | Corporate advances |
| MB-COUNTY-TAX | Packed Number | 2 | County tax portion |
| MB-CITY-TAX | Packed Number | 2 | City tax portion |
| MB-HAZ-PREM | Packed Number | 2 | Hazard insurance premium |
| MB-MIP | Packed Number | 2 | Mortgage insurance premium |
| MB-L-C-AMT | Packed Number | 2 | Late charge amount |
| MB-DELQ-P-I | Packed Number | 2 | Delinquent principal + interest |
| MB-DELQ-ESC | Packed Number | 2 | Delinquent escrow |
| MB-DELQ-L-C | Packed Number | 2 | Delinquent late charges |
| MB-DELQ-INS | Packed Number | 2 | Delinquent insurance |
| MB-DELQ-OTHER | Packed Number | 2 | Other delinquent amounts |
| MB-INTEREST-DUE | Packed Number | 2 | Interest due this cycle |
| MB-PRIN-YTD | Packed Number | 2 | Year‑to‑date principal paid |
| MB-INTEREST-YTD | Packed Number | 2 | Year‑to‑date interest paid |
| MB-TAXES-YTD | Packed Number | 2 | Year‑to‑date taxes paid |
| MB-HAZARD-YTD | Packed Number | 2 | Year‑to‑date hazard insurance |
| MB-MIP-YTD | Packed Number | 2 | Year‑to‑date MIP |
| MB-L-C-YTD | Packed Number | 2 | Year‑to‑date late charges |
| MB-ANNUAL-INTEREST | Packed Number | 5 | Annual interest rate (scale 5) |
| MB-2ND-INTEREST | Packed Number | 5 | Secondary loan interest rate |
| MB-0277-SCRA-BILLING-INTEREST | Packed Number | 5 | SCRA billing interest rate |
| MB-PLANET-AMOUNT | Packed Number | 2 | Planet code amount (tracking) |

Note: Scale is the implied decimal digits; e.g., scale 2 means cents, scale 5 used for rates.

### Date fields glossary (components vs rolled)
| Family | Components | Rolled/Composite | Notes |
|---|---|---|---|
| Statement Date | `MB-STATEMENT-YY`, `MB-STATEMENT-MM`, `MB-STATEMENT-DD` | `MB-STATEMENT-YY-R` | Rolled numeric often used for sort; prefer normalized YYYY-MM-DD |
| Loan Due Date | `MB-LOAN-DUE-YY`, `MB-LOAN-DUE-MM`, `MB-LOAN-DUE-DD` | `MB-LOAN-DUE-DATE-R` | Payment due for the loan |
| Coupon Due Date | `MB-COUPON-DUE-YY`, `MB-COUPON-DUE-MM`, `MB-COUPON-DUE-DD` | — | Used for coupon/tape |
| First Due Date | `MB-1ST-DUE-YY`, `MB-1ST-DUE-MM`, `MB-1ST-DUE-DD` | — | First scheduled payment |
| Begin History | `MB-BEG-HIST-YY`, `MB-BEG-HIST-MM`, `MB-BEG-HIST-DD` | — | History window start |
| Maturity | `MB-MATURITY-YY`, `MB-MATURITY-MM` | — | Day may be implied/absent |
| ARM Interest Rate Change | `MB-ARM-IR-YY`, `MB-ARM-IR-MM`, `MB-ARM-IR-DD` | — | ARM change schedule |
| ARM P&I Change | `MB-ARM-PI-CHG-YY`, `MB-ARM-PI-CHG-MM`, `MB-ARM-PI-CHG-DD` | — | ARM payment change |
| Loan Origination | `MB-LOAN-YY`, `MB-LOAN-MM`, `MB-LOAN-DD` | — | Origination date |
| Assumption | `MB-ASSUMP-YY`, `MB-ASSUMP-MM`, `MB-ASSUMP-DD` | — | Assumption date |
| Off‑Schedule Pending | `MB-OFF-SCHD-PEND-YY`, `MB-OFF-SCHD-PEND-MM`, `MB-OFF-SCHD-PEND-DD` | — | Pending change date |
| Int‑Only Expiration | `MB-INT-ONLY-EXP-YY`, `MB-INT-ONLY-EXP-MMDD` | — | MMDD used with 2‑digit year |
| Trial Mod (Start/End) | `MB-TRIAL-MOD-START-YY/MMDD`, `MB-TRIAL-MOD-END-YY/MMDD` | — | Combine for full date |
| Modification Effective | `MB-MOD-EFF-YY`, `MB-MOD-EFF-MMDD` | — | Combine for full date |
| Post‑Petition | `MB-POST-PETITION-YY`, `MB-POST-PETITION-MMDD` | — | Bankruptcy context |
| Coupon Tape Date | `MB-COUPON-TAPE-YY/MM/DD` | `MB-COUPON-TAPE-DATE-R` | Tape generation date |
| Late Date | `MB-LATE-YYYY/MM/DD` | — | Last paid/late mark |

Example source lines:
```28:41:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/mblps.dd
MB-STATEMENT-YY, 583, 4, Text, 0
MB-STATEMENT-MM, 587, 2, Text, 0
MB-STATEMENT-DD, 589, 2, Text, 0
MB-STATEMENT-YY-R, 584, 3, Number, 0
MB-LOAN-DUE-YY, 591, 4, Text, 0
MB-LOAN-DUE-MM, 595, 2, Text, 0
MB-LOAN-DUE-DD, 597, 2, Text, 0
MB-LOAN-DUE-DATE-R, 591, 8, Number, 0
MB-COUPON-DUE-YY, 599, 4, Text, 0
MB-COUPON-DUE-MM, 603, 2, Text, 0
MB-COUPON-DUE-DD, 605, 2, Text, 0
```

Normalization guidance:
- Prefer emitting ISO `YYYY-MM-DD` strings in modern outputs; derive from components and century rules as needed.
- Treat any `-R` fields as numeric convenience keys; recompute from components where possible to avoid ambiguity.

### Option overlays in `MB-CLIENT-FIELDS`
- The `MB-CLIENT-FIELDS` region overlays client/option‑specific controls starting near offset ~1070.
```161:168:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/mblps.dd
MB-CLIENT-FIELDS, 1070, 160, Text, 0
MB-0133-ONLINE-INSERT, 1070, 1, Text, 0
MB-0133-NEW-PAL-FLAG, 1071, 1, Text, 0
MB-0133-NEW-PAL-FORM, 1072, 6, Text, 0
MB-0133-XTRA-PMI-PAGE, 1078, 1, Text, 0
MB-0133-PROD-CODE, 1079, 4, Text, 0
MB-0133-FORM-NAME, 1083, 6, Text, 0
```

- Option 0140 (form/opt‑out/pockets):
```1:2:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0140.dd
MB-0140-SAMPLE-CODE, 1077, 7, Text, 0
OptOutCod, 1070, 1, Text, 0
```

- Option 0277 (escrow/fees/messaging/inserts):
```1:8:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0277.dd
mb-0277-t-i-insurance, 1070, 6, Packed Number, 2
mb-0277-expenses-paid, 1076, 6, Packed Number, 2
mb-0277-late-charge, 1082, 6, Packed Number, 2
mb-0277-return-check-fee, 1088, 6, Packed Number, 2
mb-0277-other-fees, 1094, 6, Packed Number, 2
mb-0277-res-esc-code, 1121, 1, Text, 0
mb-0277-tax-code, 1100, 1, Text, 0
mb-0277-prop-code, 1101, 1, Text, 0
```

- Option 0346 (sample statements/audit/access):
```1:4:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0346.dd
Mb0346Bankrupt, 1109, 1, Text, 0
Mb0346Delinquent, 1110, 1, Text, 0
Mb0346Sample, 1111, 10, Text, 0
Mb0346UBT2, 1346, 1, Text, 0
```

- Option 0547 (inserts/messages/VI/1098/1099):
```5:12:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0547.dd
MB-0547-POCKET-2, 1070, 1, Text, 0
MB-0547-POCKET-3, 1071, 1, Text, 0
MB-0547-POCKET-4, 1072, 1, Text, 0
MB-0547-POCKET-5, 1073, 1, Text, 0
MB-0547-POCKET-6, 1074, 1, Text, 0
MB-0547-BILL-FORM, 1075, 6, Text, 0
MB-0547-NEW-LOAN-FORM, 1081, 6, Text, 0
MB-0547-NEW-LOAN-FORM2, 1087, 6, Text, 0
```

- Option 0628 (BK/option ARM/late charge stop/flags):
```1:4:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0628.dd
MbPropState, 517, 2, Text, 0
MbTi1098Code, 1514, 1, Text, 0
MbSample, 1076, 5, Text, 0
MbBK, 1076, 2, Text, 0
```

- Option 0310 (subclient, images, onserts, UBT):
```1:6:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0310.dd
SubClient, 1070, 3, Text, 0
MonthlyMessageImage, 1073, 18, Text, 0
MonthlyFaceImage, 1091, 18, Text, 0
MonthlyFaceImage2, 1109, 18, Text, 0
MbHudLoanNo, 1127, 15, Text, 0
MbDupLockboxCode, 1346, 1, Text, 0
```

- Option 0733 (subclient and monthly images):
```1:5:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0733.dd
SubClient, 1070, 3, Text, 0
MonthlyMessageImage, 1073, 6, Text, 0
MonthlyFaceImage, 1079, 6, Text, 0
MonthlyFaceImage2, 1085, 6, Text, 0
MbHudLoanNo, 1091, 15, Text, 0
```

- Option 0255 (client code override):
```1:1:/Users/vshanmu/3OSG/Legacy Application/Scripts/MBCNTR2503/mblps/option.0255.dd
MB-0255-Client, 0, 3, Text, 0
```

Usage guidance:
- Use option overlays to drive conditional rendering, inserts, and regulatory pages. When multiple options apply, later overlays may supersede earlier defaults within `MB-CLIENT-FIELDS`.

### Operational considerations
- Requires C toolchain (e.g., `gcc`) and COBOL compiler/runtime for `setmb2000.cbl`.
- DD/IOMAP alignment is critical; misalignment inflates `.suspect` and breaks lengths.
- EBCDIC handling covers packed/zoned decimals—numeric fields must not be decoded as strings.

---

### Modernization cues
- BRE doc (`mbcntr2503-bre-v4.txt`) already translates steps and I/O—ideal for mapping into a modern pipeline.
- C# note for IBM037 (`ebcdic…c#.txt`) hints at re‑implementing EBCDIC/mixed‑field parsing.
- Golden outputs in `Expected_Outputs/<job>/` are perfect parity fixtures.

---

### Verification fixtures (golden outputs)
- Use `Expected_Outputs/<job>/` to validate stage‑by‑stage parity:
  - Normalize: compare `<job>.4300*`, `.rectype`, `.total`.
  - Extract/validate: compare `<job>.4300.txt*` files.
  - MBILL: compare `<job>.dat.asc*`, `<job>p.asc`, `<job>p.set`.
  - Routing/grouping: compare `*cntr.grp*`, `e.*` artifacts.

---

### Open questions and assumptions
- [ ] Confirm DD/IOMAP files used by `ncpcntr0.c` for client 2503.
- [ ] Define exact selection list logic for `cnpsplit4.c` (e‑bill vs paper).
- [ ] Clarify when `<job>.cntrkey` is produced vs only `<job>.ncpjax`.
- [ ] Enumerate field‑level layouts from `mblps.dd` and `mblps.dd.iomap`.

---

### Next steps backlog
- [ ] Extract and document field layouts from `mblps.dd` and `*.iomap`.
- [ ] Map option handling from `option.*.dd` into explicit business rules.
- [ ] Describe error handling and thresholds for `.suspect` classification.
- [ ] Create parity test harness against `Expected_Outputs/<job>/`.
- [ ] Draft modernization dataflow (module boundaries, schemas, encoders).

---

### Changelog
- 2025‑09‑24: Initial skeleton with flow, artifacts, components, and backlog.

---

### How to update this document
- Add findings under the relevant section; keep bullets concise and skimmable.
- Prefer referencing file paths with backticks (e.g., `Scripts/MBCNTR2503/mblps/mblps.dd`).
- When adding a new stage or artifact, update both the Flow and Artifact sections.
- Record non‑obvious decisions in the Changelog.


