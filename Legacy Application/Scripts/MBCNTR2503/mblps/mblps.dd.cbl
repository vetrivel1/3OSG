       01  MB-REC.
           05  MB-CLIENT-ID-FIELDS.
               10  MB-CLIENT.
                   15 MB-CLIENT3                  PIC 9(3).
                   15 FILLER                      PIC X(1).
               10  FILLER                         PIC X(6).
           05  MB-ACCOUNT-FIELDS.
               10  MB-ACCOUNT                 PIC S9(13) COMP-3.
               10  MB-OTHER-ACCOUNT.
                   15 MB-FORMATTED-ACCOUNT    PIC 9(10).
                   15 FILLER                  PIC X(10).
               10  MB-SSN                     PIC 9(9)   COMP-3.
               10  MB-SSN-TIN-CODE            PIC X.
      *            88  THIS-IS-NOGOOD         VALUE SPACE.
      *            88  THIS-IS-TIN            VALUE '1'.
      *            88  THIS-IS-SSN            VALUE '2'.
               10  MB-CO-SSN                  PIC 9(9)   COMP-3.
               10  MB-CO-SSN-TIN-CODE         PIC X.
           05  MB-NAME-ADDRESS.
               10  MB-FOREIGN-ADDRESS         PIC X.
               10  MB-BILL-NAME               PIC X(60).
               10  MB-BILL-LINE-2             PIC X(60).
               10  MB-BILL-LINE-3             PIC X(60).
               10  MB-BILL-LINE-4             PIC X(60).
               10  MB-BILL-LINE-5             PIC X(60).
               10  MB-BILL-LINE-6             PIC X(53).
               10  FILLER REDEFINES MB-BILL-LINE-6.
                   15  MB-BILL-CITY           PIC X(51).
                   15  MB-BILL-STATE          PIC X(2).
               10  MB-BILL-ZIP-CODE.
                   15  MB-ZIP-5               PIC X(5).
                   15  MB-ZIP-4               PIC X(4).
      * next field only in "mb5698n.cbl" - needed for new layout?
               10  MB-ACS-CODE                PIC X(25).
           05  MB-PROPERTY-ADDRESS.
               10  MB-PROPERTY-STREET         PIC X(50).
               10  MB-PROPERTY-CITY           PIC X(30).
               10  MB-PROPERTY-STATE          PIC X(2).
               10  MB-PROPERTY-ZIP-5          PIC X(5).
               10  MB-PROPERTY-ZIP-4          PIC X(4).
               10  MB-STATE-CODE              PIC X(2).
               10  FILLER                         PIC X(29).
           05  MB-BILL-DATA.
               10  MB-TELEPHONE.
                   15 MB-TELE-NO              PIC X(12).
                   15 MB-SEC-TELE-NO          PIC X(12).
               10  MB-DATES.
                   15  MB-STATEMENT-DATE.
                       20  MB-STATEMENT-YY    PIC X(4).
                       20  MB-STATEMENT-MM    PIC X(2).
                       20  MB-STATEMENT-DD    PIC X(2).
                   15  MB-STATEMENT-DATE-R REDEFINES MB-STATEMENT-DATE.
                       20  FILLER             PIC X(1).
                       20  MB-STATEMENT-YY-R  PIC 9(3).
                       20  MB-STATEMENT-MM-R  PIC 9(2).
                       20  MB-STATEMENT-DD-R  PIC 9(2).
                   15  MB-LOAN-DUE-DATE.
                       20 MB-LOAN-DUE-YY      PIC X(4).
                       20 MB-LOAN-DUE-MM      PIC X(2).
                       20 MB-LOAN-DUE-DD      PIC X(2).
                   15  MB-LOAN-DUE-DATE-R REDEFINES MB-LOAN-DUE-DATE
                                              PIC 9(8).
                   15  MB-COUPON-DUE-DATE.
                       20 MB-COUPON-DUE-YY    PIC X(4).
                       20 MB-COUPON-DUE-MM    PIC X(2).
                       20 MB-COUPON-DUE-DD    PIC X(2).
                   15  MB-1ST-DUE-DATE.
                       20 MB-1ST-DUE-YY       PIC X(4).
                       20 MB-1ST-DUE-MM       PIC X(2).
                       20 MB-1ST-DUE-DD       PIC X(2).
                   15  MB-BEG-HIST-DATE.
                       20 MB-BEG-HIST-YY      PIC X(4).
                       20 MB-BEG-HIST-MM      PIC X(2).
                       20 MB-BEG-HIST-DD      PIC X(2).
                   15  MB-MATURITY-DATE.
                       20 MB-MATURITY-YY      PIC X(4).
                       20 MB-MATURITY-MM      PIC X(2).
                   15  MB-ARM-IR-CHG-DATE.
                       20 MB-ARM-IR-YY        PIC X(4).
                       20 MB-ARM-IR-MM        PIC X(2).
                       20 MB-ARM-IR-DD        PIC X(2).
                   15  MB-ARM-PI-CHG-DATE.
                       20 MB-ARM-PI-CHG-YY    PIC X(4).
                       20 MB-ARM-PI-CHG-MM    PIC X(2).
                       20 MB-ARM-PI-CHG-DD    PIC X(2).
                   15  MB-LOAN-DATE.
                       20  MB-LOAN-YY         PIC X(4).
                       20  MB-LOAN-MM         PIC X(2).
                       20  MB-LOAN-DD         PIC X(2).
                   15  MB-ASSUMP-DATE.
                       20  MB-ASSUMP-YY       PIC X(4).
                       20  MB-ASSUMP-MM       PIC X(2).
                       20  MB-ASSUMP-DD       PIC X(2).
               10  MB-LOAN-DATA.
                   15  MB-GRACE-DAYS          PIC 9(2).
                   15  MB-GRACE-DAYS-R REDEFINES
                       MB-GRACE-DAYS          PIC XX.
                   15  MB-PAYMENT-FREQUENCY   PIC X.
                   15  MB-ANNUAL-INTEREST     PIC S9(2)V9(5) COMP-3.
                   15  MB-2ND-INTEREST        PIC S9(2)V9(5) COMP-3.
                   15  MB-TYPE-LOAN           PIC X(2).
                   15  FILLER REDEFINES MB-TYPE-LOAN.
                       20  MB-TYPE-LOAN-A     PIC X.
                       20  MB-TYPE-LOAN-B     PIC X.
                   15  MB-2ND-LOAN-TYPE       PIC X(2).
                   15  MB-BILL-MODE           PIC X.
                       88 DRAFTER VALUE '6'.
                   15  MB-LOAN-TERM           PIC S9(3)     COMP-3.
                   15  MB-BANKRUPT-CODE       PIC X(2).
               10  MB-DOLLAR-BALS.
                   15  MB-FIRST-PRIN-BAL      PIC S9(11)V99 COMP-3.
                   15  MB-2ND-PRIN-BAL        PIC S9(7)V99  COMP-3.
                   15  MB-ESCROW-BAL          PIC S9(9)V99 COMP-3.
                   15  MB-ESCROW-ADVANCE-BAL  PIC S9(9)V99  COMP-3.
                   15  MB-SUSPENSE-BAL        PIC S9(11)V99  COMP-3.
                   15  MB-RES-ESCROW          PIC S9(9)V99  COMP-3.
                   15  MB-REP-RES-BAL         PIC S9(9)V99  COMP-3.
                   15  MB-ACCRUED-LATE-CHG    PIC S9(7)V99  COMP-3.
                   15  MB-DEFERRED-INT        PIC S9(9)V99  COMP-3.
                   15  MB-NSF-BAL             PIC S9(5)V99  COMP-3.
                   15  MB-OTHER-FEES          PIC S9(7)V99  COMP-3.
                   15  MB-CORP-ADV            PIC S9(7)V99 COMP-3.
               10  MB-CURRENT-PAYMENT.
                   15  MB-PAYMENT-AMOUNT      PIC S9(9)V99  COMP-3.
                   15  MB-FIRST-P-I           PIC S9(9)V99  COMP-3.
                   15  MB-2ND-P-I             PIC S9(5)V99  COMP-3.
                   15  MB-ESCROW-PAYMENT      PIC S9(7)V99  COMP-3.
                   15  MB-COUNTY-TAX          PIC S9(7)V99  COMP-3.
                   15  MB-CITY-TAX            PIC S9(7)V99  COMP-3.
                   15  MB-HAZ-PREM            PIC S9(7)V99  COMP-3.
                   15  MB-MIP                 PIC S9(7)V99  COMP-3.
                   15  MB-LIEN                PIC S9(7)V99  COMP-3.
                   15  MB-O-S-SPREAD          PIC S9(7)V99  COMP-3.
                   15  MB-A-H-PREM            PIC S9(5)V99  COMP-3.
                   15  MB-LIFE-PREM           PIC S9(5)V99  COMP-3.
                   15  MB-REP-RES             PIC S9(7)V99  COMP-3.
                   15  MB-MISC-AMT            PIC S9(7)V99  COMP-3.
                   15  MB-HUD-PART            PIC S9(7)V99  COMP-3.
                   15  MB-BSC-AMT             PIC S9(5)V99  COMP-3.
                   15  MB-L-C-AMT             PIC S9(7)V99  COMP-3.
               10  MB-TOTAL-DUE.
                   15  MB-TOTAL-AMOUNT-DUE    PIC S9(11)V99 COMP-3.
                   15  MB-DELQ-P-I            PIC S9(11)V99 COMP-3.
                   15  MB-DELQ-ESC            PIC S9(11)V99 COMP-3.
                   15  MB-DELQ-L-C            PIC S9(7)V99  COMP-3.
                   15  MB-DELQ-INS            PIC S9(7)V99  COMP-3.
                   15  MB-DELQ-OTHER          PIC S9(9)V99  COMP-3.
                   15  MB-INTEREST-DUE        PIC S9(5)V99  COMP-3.
               10  MB-YTD-FIELDS.
                   15  MB-PRIN-YTD            PIC S9(11)V99  COMP-3.
                   15  MB-INTEREST-YTD        PIC S9(11)V99  COMP-3.
                   15  MB-TAXES-YTD           PIC S9(9)V99  COMP-3.
                   15  MB-HAZARD-YTD          PIC S9(7)V99  COMP-3.
                   15  MB-MIP-YTD             PIC S9(7)V99  COMP-3.
                   15  MB-LIEN-YTD            PIC S9(7)V99  COMP-3.
                   15  MB-L-C-YTD             PIC S9(7)V99  COMP-3.
                   15  MB-LAST-YEAR-INT-PAID  PIC S9(7)V99  COMP-3.
                   15  MB-LAST-YEAR-TAXES-PAID PIC S9(7)V99  COMP-3.
           05  MB-HMP-FIELDS.
               10 MB-MODIFICATION-STATUS  PIC XX.
                  88 HMP-LOAN-STATUS VALUE 'MA'.
               10 MB-MODIFICATION-TYPE   PIC X.
               10 MB-MODIFICATION-STATUS-DATE.
                  15 MB-MODIFICATION-STATUS-YY PIC 9(3)  COMP-3.
                  15 MB-MOD-STATUS-MMDD      PIC 9(4).
                  15 FILLER REDEFINES MB-MOD-STATUS-MMDD.
                     20 MB-MOD-STATUS-MM     PIC 9(2).
                     20 MB-MOD-STATUS-DD     PIC 9(2).
               10 MB-MODIFICATION-PROGRAM    PIC X(4).
               10 MB-MODIFICATION-DATE.
                  15 MB-MODIFICATION-YY      PIC 9(3) COMP-3.
                  15 MB-MODIFICATION-MMDD    PIC 9(4).
               10 MB-DEFERRED-BAL-IND     PIC X.
               10 MB-PAYMENT-OPTION-IND   PIC X.
               10 MB-ORIG-LOAN-AMT        PIC S9(9) COMP-3.
               10 MB-NEG-AM-PB-CAP-VALUE  PIC S9(11)V99 COMP-3.
               10 MB-INT-ONLY-EXP-DATE.
                  15 MB-INT-ONLY-EXP-YY      PIC 9(3) COMP-3.
                  15 MB-INT-ONLY-EXP-MMDD    PIC 9(4).
               10 MB-MONTHLY-BORR-ACCR-AMT PIC S9(5)V99 COMP-3.
               10 MB-BORR-INCTV-ANNIV-DATE.
                  15 MB-BORR-INCTV-ANNIV-YY PIC 9(3) COMP-3.
                  15 MB-BORR-INCTV-ANNIV-MMDD PIC 9(4).
               10 MB-BORR-INC-ACCR-ANNV2DT PIC S9(5)V99 COMP-3.
               10 MB-TRIAL-MOD-START-DATE.
                  15 MB-TRIAL-MOD-START-YY   PIC 9(3) COMP-3.
                  15 MB-TRIAL-MOD-START-MMDD PIC 9(4).
               10 MB-TRIAL-MOD-END-DATE.
                  15 MB-TRIAL-MOD-END-YY     PIC 9(3) COMP-3.
                  15 MB-TRIAL-MOD-END-MMDD   PIC 9(4).
               10 MB-TRIAL-MOD-PAYMENT    PIC S9(9)V99 COMP-3.
               10 MB-MOD-REASON-CODE        PIC X(5).
               10 MB-MOD-EFF-YY             PIC 9(3) COMP-3.
               10 MB-MOD-EFF-MMDD           PIC 9(4).
               10 MB-2-TO-1-INDICATOR       PIC X.
               10 MB-OFF-SCHD-PEND-DATE.
                  15 MB-OFF-SCHD-PEND-YY    PIC S9(3) COMP-3.
                  15 MB-OFF-SCHD-PEND-MM    PIC 9(2).
                  15 MB-OFF-SCHD-PEND-DD    PIC 9(2).
               10 MB-OFF-SCHD-PEND-IR       PIC S99V9(5) COMP-3.
               10 MB-OFF-SCHD-PEND-PI       PIC S9(11)V99 COMP-3.
           05  MB-FLEXFIELDS.
               10 MB-FLEXFIELD1             PIC X(12).
               10 MB-FLEXFIELD2             PIC X(12).
               10 MB-FLEXFIELD3             PIC X(12).
           05  FILLER                       PIC X(4).
           05  MB-POCKET-CODES.
               10 MB-POCKET-1               PIC X.
               10 MB-POCKET-2               PIC X.
               10 MB-POCKET-3               PIC X.
               10 MB-POCKET-4               PIC X.
               10 MB-POCKET-5               PIC X.
               10 MB-POCKET-6               PIC X.
               10 MB-POCKET-7               PIC X.
           05  MB-CLIENT-FIELDS               PIC X(160).
           05  MB-0133-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10  MB-0133-ONLINE-INSERT      PIC X.
               10  MB-0133-NEW-PAL-FLAG       PIC X.
                   88 BOA-PAL                  VALUES '3','4'.
               10  MB-0133-NEW-PAL-FORM       PIC X(6).
               10  MB-0133-XTRA-PMI-PAGE      PIC X.
               10  MB-0133-PROD-CODE          PIC X(4).
               10  MB-0133-FORM-NAME          PIC X(6).
               10  MB-0133-MESS-ONE           PIC X.
                   88 BOA-CRITICAL-MESS        VALUES '1' THRU '7'.
               10  MB-0133-MESS-TWO           PIC X.
                   88 BOA-CRITICAL-MESS2       VALUES '1' THRU '7'.
               10  MB-0133-MESS-THREE         PIC X.
               10  MB-0133-INSERTS.
                   15  MB-0133-INSERT1        PIC 9.
                   15  MB-0133-INSERT2        PIC 9.
                   15  MB-0133-INSERT3        PIC 9.
                       88 BOA-PRIV             VALUE 1.
                   15  MB-0133-INSERT4        PIC 9.
               10  MB-0133-2ND-PAGE           PIC X(6).
                   88 BOA-ADDR-EXCL            VALUE 'NO MKT'.
               10  MB-0133-CHK-TYPE           PIC X(3).
                   88 BOA-IOE                  VALUE 'IOE'.
                   88 BOA-PIF                  VALUE 'PIF'.
               10  MB-0133-NO-BILL-SW         PIC X.
                   88 BOA-NO-BILL              VALUE '1'.
               10  MB-0133-NO-BILL2-SW        PIC X.
                   88 BOA-NO-BILL2             VALUE '1'.
               10  MB-0133-KILL-COUP          PIC X.
                   88 BOA-KILL-COUP            VALUE 'R'.
               10  MB-0133-BANKRUPT           PIC X(1).
                   88 BOA-BANKRUPTCY           VALUE 'B'.
                   88 BOA-ALMOST-BANKRUPTCY    VALUE 'V'.
                   88 BOA-MAN-BANKRUPTCY       VALUE '&'.
               10  MB-0133-MARKETING          PIC X(1).
                   88 BOA-MARKET-STATE         VALUE 'S'.
                   88 BOA-MARKETING            VALUE 'M'.
               10  MB-0133-BANKRUPT-X         PIC X(1).
               10  MB-0133-BNKRPT-ESC         PIC X(1).
                   88 GETS-BNKRPT-ESC-INSERT   VALUE 'Y'.
                   88 GETS-LEGAL-INSERT      VALUE 'L'.
               10  MB-0133-BILL-ADDR-FOREIGN  PIC X.
                   88 BOA-FOREIGN              VALUE '1'.
               10  MB-0133-PILOT-SW           PIC X(1).
               10  MB-0133-PILOT-TOTAL-PAGES  PIC 9(1).
               10  MB-0133-PILOT-TOTAL-LINES  PIC 9(3).
               10  MB-0133-ESC-CODE           PIC X.
                   88 BOA-Z                    VALUE 'Z'.
               10  MB-0133-CHECK              PIC X.
                   88 BOA-HAS-CHK              VALUE 'C'.
               10  MB-0133-ESC-FLAG           PIC X.
                   88 BOA-SY                   VALUE 'H'.
                   88 BOA-SHORTAGE             VALUE 'S','T'.
                   88 BOA-OVERAGE              VALUE 'O'.
               10  MB-0133-PMI-CODE           PIC X(3).
               10  ESC-0133-S-KEY             PIC 9(6).
               10  ESC-0133-S-COUNT           PIC 9(4).
                   88 BOA-CHECK-NO-ESC         VALUES 0002.
                   88 BOA-ESC-MESS             VALUES 0003 THRU 9999.
               10  MB-0133-PREV-POSTED        PIC S9(7)V99 COMP-3.
               10  MB-0133-PAL-FLAG           PIC X.
               10  MB-0133-GENERATED          PIC X.
               10  MB-0133-YE.
                   15 MB-0133-TI-REL-KEY      PIC 9(7).
                   15 MB-0133-TI-ESCROW-MTH   PIC S9(5)V99 COMP-3.
                   15 MB-0133-TI-TYPE-LOAN    PIC XX.
                   15 MB-0133-TI-STATE        PIC 99.
                   15 MB-0133-TI-PRIN-BAL     PIC S9(9)V99 COMP-3.
                   15 MB-0133-TI-PMI-CODE     PIC X(3).
                   15 MB-0133-TI-MI-CODE      PIC X(2).
                   15 MB-0133-1099-AMOUNT-1   PIC S9(7)V99 COMP-3.
                   15 MB-0133-1098-INT        PIC S9(7)V99 COMP-3.
               10  MB-0133-MESSG-SW           PIC X.
               10  MB-0133-TEMP-SY-MESS-SW    PIC X.
               10  FILLER                     PIC X(51).
           05  MB-0140-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10  MB-0140-OPT-OUT-CODE       PIC X.
               10  MB-0140-SPECIAL-MESSAGE-SW PIC X.
               10  MB-0140-POCKET-2           PIC X.
               10  MB-0140-POCKET-3           PIC X.
               10  MB-0140-POCKET-4           PIC X.
               10  MB-0140-POCKET-5           PIC X.
               10  MB-0140-POCKET-6           PIC X.
               10  MB-0140-STATEMENT-SW       PIC X.
               10  MB-0140-FORM-TYPE          PIC X(6).
               10  MB-0140-PAYOPT-FLAG        PIC X.
               10  FILLER                     PIC X(145).
           05  MB-0277-FIELDS REDEFINES MB-CLIENT-FIELDS.
      * these MB-0277 fields are set in feesec277b.c
               10  MB-0277-T-I-INSURANCE      PIC S9(9)V99 COMP-3.
               10  MB-0277-EXPENSES-PAID      PIC S9(9)V99 COMP-3.
               10  MB-0277-LATE-CHARGE        PIC S9(9)V99 COMP-3.
               10  MB-0277-RETURN-CHECK-FEE   PIC S9(9)V99 COMP-3.
               10  MB-0277-OTHER-FEES         PIC S9(9)V99 COMP-3.
               10  MB-0277-TAX-CODE           PIC X.
                   88 0277-J-CODE             VALUE 'J'.
               10  MB-0277-PROP-CODE          PIC X.
                   88 0277-M-CODE             VALUE 'M'.
               10  MB-0277-ARM-CODE           PIC X.
               10  MB-0277-TOT-BEG-BAL        PIC S9(9)V99 COMP-3.
               10  MB-0277-END-BEG-BAL        PIC S9(9)V99 COMP-3.
               10  MB-0277-INTEREST-DUE       PIC S9(9)V99 COMP-3.
               10  MB-0277-RES-ESC-CODE       PIC X.
      * preceding MB-0277 fields are set in feesec277b.c
               10  MB-0277-BILL-FORM-NAME     PIC X(6).
               10  MB-0277-BILL-BACK-NAME     PIC X(6).
               10  MB-0277-BILL-MESSAGES      PIC X(15).
               10  MB-0277-BILL-MESSAGES-RDF
                     REDEFINES MB-0277-BILL-MESSAGES.
                   15 MB-0277-BILL-MESSAGE OCCURS 15 TIMES PIC X.
      *---- FLAGS FOR EXTRA PAGES
               10  MB-0277-BILL-PAGES.
                   15 MB-0277-TRANS-PAGE         PIC X.
                   15 MB-0277-PRIVACY-PAGE       PIC X.
                   15 MB-0277-MARKETING-PAGE     PIC X.
      *---- VARIABLE POCKETS 2 - 5
      *bill-insert(1) pocket 2
      *bill-insert(2) pocket 3
      *bill-insert(3) pocket 4
      *bill-insert(4) pocket 5
               10  MB-0277-BILL-INSERTS       PIC X(4).
               10  MB-0277-BILL-INSERTS-RDF
                      REDEFINES MB-0277-BILL-INSERTS.
                   15 0277-BILL-INSERT OCCURS 4 TIMES PIC X.
               10  MB-0277-MARKETING-FORM-NAME PIC X(6).
               10  MB-0277-NUM-PAGES          PIC 9.
               10  MB-0277-BILL-WEIGHT        PIC X.
               10  MB-0277-MESSAGE-BEGIN      PIC 9V99.
               10  MB-0277-SCRA-F             PIC X.
               10  MB-0277-SCRA-DATE-3.
                   15 MB-0277-SCRA-DATE-3-MM  PIC 99.
                   15 MB-0277-SCRA-DATE-3-DD  PIC 99.
                   15 MB-0277-SCRA-DATE-3-YY  PIC 99.
               10  MB-0277-SCRA-DATE-4.
                   15 MB-0277-SCRA-DATE-4-MM  PIC 99.
                   15 MB-0277-SCRA-DATE-4-DD  PIC 99.
                   15 MB-0277-SCRA-DATE-4-YY  PIC 99.
               10  MB-0277-SCRA-BILLING-INTEREST
                                                 PIC S9(2)V9(5) COMP-3.
               10  MB-0277-SCRA-BILL-INT-R REDEFINES
                   MB-0277-SCRA-BILLING-INTEREST
                                                 PIC S9(2)V9(5) COMP-3.
               10  MB-0277-LAST-AMT           PIC 9(9)V99.
               10  MB-0277-LAST-DATE          PIC 9(6).
               10  MB-0277-1098-PAGE          PIC X.
               10  MB-0277-TI-IRS-PURPOSE-CODE PIC X.
      *pocket 6 added after the fact so had to put here
               10  MB-0277-BILL-INSERT-6      PIC X(1).
      *pocket 7 added after the fact so had to put here
               10  MB-0277-BILL-INSERT-7      PIC X(1).
               10  MB-0277-TYPE-OF-SAMPLE     PIC X(1).
               10  FILLER                     PIC X(24).
           05  MB-0589-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10 MB-PRIN-DELQ-BAL            PIC X(13).
               10 MB-FORBEARANCE-AMT          PIC S9(5)V99 COMP-3.
               10 FILLER                      PIC X(143).
           05  MB-0346-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10 MB-0346-SWITCH              PIC X.
                  88 Q-BILL                    VALUE '1'.
               10 MB-TI-TOTAL-DED-MI-0346     PIC S9(7)V99 COMP-3.
               10 MB-0346-SAMPLE-LOAN-SW      PIC X.
               10 MB-0346-SAMPLE-LOAN-TYPE    PIC X.
               10 MB-0346-SAMPLE-FORM1        PIC X(6).
               10 MB-0346-SAMPLE-FORM1B       PIC X(6).
               10 MB-0346-SAMPLE-FORM2        PIC X(6).
               10 MB-0346-SAMPLE-FORM3        PIC X(6).
               10 MB-0346-SAMPLE-MESS-CODE    PIC X(1).
               10 MB-0346-ADDL-PAGES          PIC 9(2).
               10 MB-0346-PRINT-TRANS-TOT     PIC 9(4).
               10 MB-0346-BANKRUPT            PIC X.
               10 MB-0346-DELINQUENT          PIC X.
               10 MB-0346-SAMPLE-GROUP.
                  15 MB-0346-SAMPL-TYPELOAN   PIC X.
                  15 MB-0346-SAMPL-BK         PIC X.
                  15 MB-0346-SAMPL-DELQ       PIC X.
                  15 MB-0346-SAMPL-DRAFTER    PIC X.
                  15 MB-0346-SAMPL-FC         PIC X.
                  15 MB-0346-SAMPL-AUDIT      PIC X.
                  15 FILLER                   PIC X(4).
               10 MB-0346-SUPPRESSED-SW       PIC X.
               10 MB-0346-BNKL-CLSD-REASON-CD PIC X(2).
               10 MB-0346-ACCESS-NBR          PIC X(12).
               10 FILLER                      PIC X(94).
           05  MB-0547-FIELDS REDEFINES MB-CLIENT-FIELDS.
               07  MB-0547-INSERT-CODES.
               10  MB-0547-POCKET-2           PIC X.
               10  MB-0547-POCKET-3           PIC X.
               10  MB-0547-POCKET-4           PIC X.
               10  MB-0547-POCKET-5           PIC X.
               10  MB-0547-POCKET-6           PIC X.
               07  FILLER.
               10  MB-0547-BILL-FORM          PIC X(6).
               10  MB-0547-NEW-LOAN-FORM      PIC X(6).
               10  MB-0547-NEW-LOAN-FORM2     PIC X(6).
               10  MB-0547-1098-FORM          PIC X(6).
               10  MB-0547-TOTAL-INSERTS      PIC 9.
               10  MB-0547-MESSAGE-CODES      PIC X(10).
               10  MB-0547-MESS REDEFINES MB-0547-MESSAGE-CODES
                   OCCURS 10                  PIC X.
               10  MB-0547-ARM-LTR-SW         PIC X.
                   88 0547-ARM-LETTER       VALUE 'Y'.
               10  MB-0547-ZIP-CODE-SW        PIC X.
               10  MB-0547-BILLING-SW         PIC X.
               10  MB-0547-LETTER-TYPE        PIC 9(1).
                   88 0547-LETTER2          VALUE 2.
                   88 0547-LETTER3          VALUE 3.
                   88 0547-LETTER4          VALUE 4.
                   88 0547-LETTER2-4        VALUE 2, 3, 4.
               10  MB-0547-JIT-FEE            PIC X(2).
               10  MB-0547-VI-SW              PIC X.
                   88 VISUALLY-IMPAIRED       VALUE 'Y'.
               10  MB-0547-1099-AMOUNT-1      PIC 9(11)V99 COMP-3.
               10  MB-0547-1099-AMOUNT-2      PIC 9(11)V99 COMP-3.
               10  MB-0547-1099-AMOUNT-3      PIC 9(11)V99 COMP-3.
               10  MB-0547-YE-SW.
                   15 MB-0547-1098-SW         PIC X.
                   15 MB-0547-1099-SW         PIC X.
                   15 MB-0547-FHA-SW          PIC X.
               10 MB-0547-VI-BILL-FORM        PIC X(6).
               10 MB-0547-SPECIAL             PIC X(1).
               10 MB-0547-FILLER              PIC X(82).
           05  MB-0628-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10 MB-0628-BK                  PIC X(2).
               10 MB-0628-OPTION-ARM          PIC X(1).
               10 MB-0628-ATTY-SW             PIC X(1).
               10 FILLER                      PIC X(156).
           05  MB-5020-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10  MB-5020-DEFERRED-INT       PIC S9(7)V99  COMP-3.
               10  MB-5020-INST               PIC 999.
               10  MB-5020-PIN-NUMBER         PIC X(6).
               10  MB-5020-UNAP-FUND-CD       PIC X.
               10  MB-5020-ESC-VAR            PIC S9(9)V99  COMP-3.
               10  MB-5020-UNAP-CD-2          PIC X.
               10  MB-5020-UNAP-BAL-2         PIC S9(7)V99  COMP-3.
               10  MB-5020-UNAP-CD-3          PIC X.
               10  MB-5020-UNAP-BAL-3         PIC S9(7)V99  COMP-3.
               10  MB-5020-UNAP-CD-4          PIC X.
               10  MB-5020-UNAP-BAL-4         PIC S9(7)V99  COMP-3.
               10  MB-5020-UNAP-CD-5          PIC X.
               10  MB-5020-UNAP-BAL-5         PIC S9(7)V99  COMP-3.
               10  MB-5020-ADDL-PAGES         PIC 9.
               10  MB-5020-BMSG-DATA.
                   12 MB-5020-MESSAGES OCCURS 15.
                      15 MB-5020-BMSG         PIC X(2).
                      15 MB-5020-BMSG-LINES   PIC 9(2).
                      15 MB-5020-BMSG-REQD    PIC X.
               10  MB-5020-MAIL-CD            PIC X.
                   88 CML-REG VALUE '0'.
                   88 CML-BK  VALUE '5','6','7'.
                   88 MC5     VALUE '5'.
                   88 MC6     VALUE '6'.
                   88 MC7     VALUE '7'.
      *        10  MB-5020-CUST-PHO-NBR       PIC X(10).
      *        10  MB-5020-WEB-SITE           PIC X(30).
               10  MB-5020-DART-UNIT          PIC X(20).
               10  MB-5020-BUYER-ASST-AMT     PIC S9(9)V99  COMP-3.
               10  MB-5020-INV-CODE           PIC S9(5)     COMP-3.
               10  FILLER                     PIC X(9).
           05  MB-0502-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10 MB-FEES-CODE                 PIC 9(2).
               10 MB-LAST-STMT-DATE            PIC 9(6).
               10 FILLER                       PIC X(152).
           05  MB-MBFIVAR2-FIELDS REDEFINES MB-CLIENT-FIELDS.
               10 CNP-COMPANY-NAME                PIC X(30).
               10 MB-BILL-METHOD              PIC S9(1) COMP-3.
               10 MB-SUPPRESSION-FLAG         PIC S9(5) COMP-3.
               10 MB-MESSAGE-CODES.
                  15 MB-MSG1-CODE             PIC X(2).
                  15 MB-MSG2-CODE             PIC X(2).
               10 FILLER                      PIC X(122).
           05  Email                          PIC X(66).
           05  EbpPaymentDueDate              PIC X(10).
           05  EbpPaymentAmount               PIC 9(9).99.
           05  EBP-AMT REDEFINES EbpPaymentAmount PIC X(12).
           05  MB-EBPP-SW                     PIC X(1).
           05  FILLER                         PIC X(1).
           05  MB-SERVICER-FIELDS             PIC X(150).
           05  MB-ALLTEL-FIELDS REDEFINES MB-SERVICER-FIELDS.
               10  MB-DIST-TYPE               PIC X.
               10  MB-MAN                     PIC X.
               10  MB-2ND-MAN                 PIC X.
               10  MB-ZONE                    PIC XX.
               10  MB-1ST-BASIS-CODE          PIC X(3).
               10  MB-BANK                    PIC X(3).
               10  MB-AGGR                    PIC X(3).
               10  MB-2ND-BANK                PIC X(3).
               10  MB-2ND-CAT                 PIC X(3).
               10  MB-ZONE-4.
                   15 MB-ZONE-1               PIC X(2).
                   15 MB-ZONE-2               PIC X(2).
               10  MB-TYPE-ACQ                PIC X.
               10  MB-USER-BILLING-TABLE      PIC X(20).
               10  FILLER REDEFINES MB-USER-BILLING-TABLE.
                   15 UBT-POS-1                   PIC X.
                      88 588-EBILL   VALUE 'W'.
                   15 UBT-POS-2                   PIC X.
                   15 UBT-POS-3                   PIC X.
                      88 USB-3 VALUE '2'.
                   15 UBT-POS-4                   PIC X.
                   15 UBT-POS-5                   PIC X.
                   15 UBT-POS-6                   PIC X.
                      88 USB-6 VALUE '5'.
                   15 UBT-POS-7                   PIC X.
                   15 UBT-POS-8                   PIC X.
                   15 UBT-POS-9                   PIC X.
                   15 UBT-POS-10                  PIC X.
                   15 UBT-POS-11                  PIC X.
                   15 UBT-POS-12                  PIC X.
                   15 UBT-POS-13                  PIC X.
                   15 UBT-POS-14                  PIC X.
                   15 UBT-POS-15                  PIC X.
                   15 UBT-POS-16                  PIC X.
                   15 UBT-POS-17                  PIC X.
                   15 UBT-POS-18                  PIC X.
                   15 UBT-POS-19                  PIC X.
                   15 UBT-POS-20                  PIC X.
               10  MB-STOP-BILL-FLAG          PIC X.
               10  MB-BILLING-CYCLE           PIC X(3).
               10  MB-PAY-OPTION              PIC X.
               10  MB-DONT-PROCESS            PIC X.
               10  MB-PIF-STOP                PIC X.
               10  MB-FORECLOSURE-STOP        PIC X.
               10  MB-BAD-CK-STOP             PIC X.
               10  MB-NO-NOTICES              PIC X.
               10  MB-DONT-ANALYZE            PIC X.
               10  MB-A-H-FLAG                PIC X.
               10  MB-LIFE-FLAG               PIC X.
               10  MB-DISB-STOP               PIC X.
               10  MB-ARM-PLAN-ID             PIC X(4).
               10  MB-INV                     PIC X(1).
               10  MB-PLS-CLT-ID              PIC X(3).
               10  MB-FIDELITY-OPTION-ARM-FIELDS.
                   15 MB-PAYMENT-OPTION-SWITCH PIC X.
                   15 MB-PAYMENT-OPT-4        PIC S9(9)V99 COMP-3.
                   15 MB-PAYMENT-OPT-4-TOTDUE PIC S9(9)V99 COMP-3.
                   15 MB-DIST-TYPE-1-IO-FLAG  PIC X.
                   15 MB-ARM-INT-ONLY-PI      PIC S9(9)V99 COMP-3.
                   15 MB-ARM-FULLY-AM-PI      PIC S9(9)V99 COMP-3.
                   15 MB-ARM-INT-ONLY-TOT     PIC S9(9)V99 COMP-3.
                   15 MB-ARM-FULLY-AM-TOT     PIC S9(9)V99 COMP-3.
               10  MB-POST-PETITION-YY        PIC 9(3) COMP-3.
               10  MB-POST-PETITION-MMDD      PIC 9(4).
               10  MB-POST-PETITION-AMOUNT    PIC S9(9)V99 COMP-3.
               10  MB-COUPON-TAPE-DATE.
                   15 MB-COUPON-TAPE-YY       PIC X(4).
                   15 MB-COUPON-TAPE-MM       PIC X(2).
                   15 MB-COUPON-TAPE-DD       PIC X(2).
               10  MB-COUPON-TAPE-DATE-R REDEFINES
                                    MB-COUPON-TAPE-DATE PIC 9(8).
               10  MB-BANKRUPTCY-STATUS       PIC X.
               10  MB-ARM-RATE-PI-NOT-AVAIL-IND PIC X.
               10  MB-NO-CHANGE-SCHED-IND     PIC X.
               10  MB-MTGR-LANG-PREF          PIC X(2).
               10  MB-FOR1-STATUS-CD          PIC X(1).
               10  MB-BNK-DISCHARGE-IND       PIC X(1).
               10  FILLER                     PIC X(18).
           05  MB-FISERV-FIELDS REDEFINES MB-SERVICER-FIELDS.
               10  MB-PAYMENT-DUE-COUNTER     PIC S9(3) COMP-3.
               10  MB-ESCROW-INTEREST-YTD     PIC S9(5)V99 COMP-3.
               10  MB-PROPERTY-ADDRESS-2      PIC X(35).
               10  MB-LATE-DATE.
                   15  MB-LATE-YYYY           PIC X(4).
                   15  MB-LATE-MM             PIC X(2).
                   15  MB-LATE-DD             PIC X(2).
               10  CNP-INVESTOR-CODE          PIC 9(3).
               10  FILLER                     PIC X(98).
           05  MB-TI-DATA.
               10  MB-TI-MTG-CODE             PIC X.
               10  MB-TI-TRAN-56-SW           PIC X.
               10  MB-TI-ZERO-PB-CODE         PIC X.
               10  MB-TI-LOAN-DATE.
                   15  MB-TI-LOAN-YY          PIC X(4).
                   15  MB-TI-LOAN-MM          PIC X(2).
                   15  MB-TI-LOAN-DD          PIC X(2).
               10  MB-TI-ASSUMP-DATE          PIC X(8).
               10  MB-TI-HAZARD-YTD           PIC S9(7)V99 COMP-3.
               10  MB-TI-MIP-YTD              PIC S9(7)V99 COMP-3.
               10  MB-TI-LIEN-YTD             PIC S9(7)V99 COMP-3.
               10  MB-TI-ESC-MTH              PIC S9(7)V99 COMP-3.
               10  MB-TI-TOT-PMT              PIC S9(7)V99 COMP-3.
               10  MB-TI-1098-CODE            PIC X.
               10  MB-TI-DETAIL-HISTORY       PIC X.
               10  MB-TI-SSN-TIN-CODE         PIC X.
                   88  THIS-IS-NOGOOD             VALUE SPACES.
                   88  THIS-IS-TIN                VALUE '1'.
                   88  THIS-IS-SSN                VALUE '2'.
               10  MB-TI-CO-SSN-TIN-CODE      PIC X.
               10  MB-TI-REVISED-STMT-SW      PIC X.
               10  MB-TI-PMI-CODE             PIC X(3).
               10  FILLER REDEFINES MB-TI-PMI-CODE.
                   15  TI-PMI-PRE-POST       PIC X.
                       88  PMI            VALUE '1'.
                       88  PRE-FHA        VALUE 'B'.
                       88  PST-FHA        VALUES 'A', 'C'.
                       88  PST-FHA-INSERT VALUE 'C'.
                   15  TI-PMI-STATE          PIC XX.
                       88  HUD    VALUE 'FH'.
               10  MB-TI-DOLLAR-BALS.
                   15  MB-TI-ESC-BAL-BEG      PIC S9(7)V99 COMP-3.
                   15  MB-TI-PRIN-BAL-BEG     PIC S9(9)V99 COMP-3.
               10  MB-TI-YTD-FLDS.
                   15  MB-TI-1098-INT         PIC S9(9)V99 COMP-3.
                   15  MB-TI-PRIN-YTD         PIC S9(9)V99  COMP-3.
                   15  MB-TI-TAXES-YTD        PIC S9(7)V99  COMP-3.
                   15  MB-TI-LATE-CHG-PD      PIC S9(5)V99  COMP-3.
                   15  MB-TI-POINTS-PD-BY-BORR
                                                  PIC S9(7)V99 COMP-3.
                   15  MB-TI-INT-ON-ESCROW    PIC S9(5)V99  COMP-3.
                   15  MB-TI-REIMBURSED-AMOUNT PIC S9(7)V99 COMP-3.
                   15 MB-TI-TOTAL-DED-MI       PIC S9(7)V99 COMP-3.
               10  MB-TI-MISC-FLDS.
                   15  MB-TI-AMT-DISB         PIC S9(7)V99 COMP-3.
                   15  MB-TI-ESC-DEP          PIC S9(7)V99 COMP-3.
               10  MB-TI-H-FHA-NUMBER         PIC X(14).
               10  MB-TI-H-AS-OF-DATE.
                   15  MB-TI-H-AS-OF-YY       PIC X(4).
                   15  MB-TI-H-AS-OF-MM       PIC X(2).
                   15  MB-TI-H-AS-OF-DD       PIC X(2).
               10  MB-TI-H-PAYOFF-AMOUNT      PIC S9(7)V99 COMP-3.
               10  MB-TI-STMT-SAMPLE          PIC X.
               10  MB-TI-SAMPLE-REASONX.
                   15  MB-TI-SAMPLE-REASON    PIC 99.
               10  MB-TI-SAMPLE-LOAN-SW       PIC X.
               10  MB-TI-KEY                  PIC 9(9)     COMP-3.
               10  MB-TI-COUNT                PIC 9(5)     COMP-3.
           05  MB-ADDITIONAL-INFO.
               10  MB-FLEXFIELDS-CONT.
                   15  MB-FLEXFIELD4          PIC X(25).
                   15  MB-FLEXFIELD5          PIC X(40).
                   15  MB-FLEXFIELD6          PIC X(50).
               10  FILLER                     PIC X(65).
      ***  REMITTANCE DATA 
      ***  05  FILLER                         PIC X(83).
           05  MB-REMITTANCE-IMB-CODE         PIC X(65).
           05  MB-REMITTANCE-ZIP-CODE         PIC X(11).
           05  FILLER                         PIC X(7).
      *         10  FILLER                     PIC X(83).
      *     05  MB-EBPP-FIELDS REDEFINES MB-ADDITIONAL-INFO.
      *         10  MB-EBPP-PAYMENT            PIC 9(8)V99.
      *         10  MB-EBPP-LATE-PAYMENT       PIC 9(8)V99.
      *         10  MB-EBPP-LATE-DATE.
      *             15  MB-EBPP-LATE-YY        PIC 9(4).
      *             15  MB-EBPP-LATE-MM        PIC 9(2).
      *             15  MB-EBPP-LATE-DD        PIC 9(2).
      *         10  FILLER                     PIC X(235).
      *     05  FILLER                             PIC X(263).
           05  MB-SPLIT-FIELDS.
               10  MB-PRODUCT-CODE            PIC X(4).
               10  MB-VIEW-PRODUCT-CODE       PIC X(4).
               10  FILLER                     PIC X(12).
           05  MB-SAMPLE-FIELDS.
               10  MB-STMT-SELECTED-AS-SAMPLE PIC X.
               10  MB-SAMPLE-REASONX.
                   15  MB-SAMPLE-REASON       PIC 99.
               10  CNP-FILLER                 PIC X(26).
               10  MB-SAMPLE-MESSAGE-CODE     PIC X.
           05  MB-0133-SAMPLE-FIELDS REDEFINES MB-SAMPLE-FIELDS.
               10  MB-0133-SAMPLES-INFO.
                12 MB-0133-BILL-SAMPLES.
                   15  IS-BILL-133            PIC X.
                   15  IS-ESCROW-PAGE-133     PIC X.
                   15  IS-PMI-PAGE-133        PIC X.
                   15  IS-PAL-PAGE-133        PIC X.
                   15  IS-MARKETING-PAGE-133  PIC X.
                   15  IS-GET-ENVELOPE-133    PIC X.
                   15  GETS-INSERT-133        PIC 9.
                12 MB-0133-ESCR-SAMPLES.
                   15  SAMPLES-BK-133         PIC X(2).
                   15  SAMPLES-TYPE-133       PIC X.
                   15  SAMPLES-CUSH-133       PIC 99.
                   15  SAMPLES-SPRD-133       PIC 9(2).
                   15  SAMPLES-PI-133         PIC X.
                   15  SAMPLES-FREQ-133       PIC 9(2).
                   15  SAMPLES-ESCROWED-133   PIC X(1).
               10  FILLER                     PIC X(12).
           05  MB-SECONDARY-KEYS.
               10  MB-TRAN-KEY                PIC 9(7) COMP-3.
               10  MB-TRAN-COUNT              PIC 9(3) COMP-3.
               10  CONTAINER-KEY              PIC 9(9).
               10  FILLER                     PIC X(11).
           05  MB-PROCESS-CONTROL-FIELDS.
      * this field is 4 bytes, equivalent to (unsigned int) in c
               10  MB-SEQ                     PIC 9(8) COMP.
               10  MB-INSERT-CODE             PIC X.
               10  MB-MESSAGE-CODE            PIC X.
               10  MB-JOB                     PIC X(7).
               10  MB-PLANET-CODE             PIC S9(13)  COMP-3.
               10  MB-PLANET-AMOUNT           PIC S9(7)V99 COMP-3.
               10  MB-PLANET-DATE             PIC X(6).
           05  FILLER                             PIC X(8).

