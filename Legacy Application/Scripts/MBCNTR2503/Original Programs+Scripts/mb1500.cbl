      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
      *THIS IS WHAT IS USED IN LIVE ENVIROMENT AS OF 10/1/2017
       01 MB1100-REC.
           05 MB1100-CLIENT-NO                  PIC X(3).
           05 FILLER                            PIC X(1).
           05 MB1100-LOAN-NO                    PIC S9(13)  COMP-3.
           05 MB1100-LOAN-NO-6 REDEFINES MB1100-LOAN-NO PIC 9(6).
           05 MB1100-LOAN-NO-7 REDEFINES MB1100-LOAN-NO PIC 9(7).
           05 MB1100-REC-CODE                   PIC X(1).
           05 MB1100-REC-NO                     PIC 9(3).
           05 PRIMARY-RECORD.
              10 MB1100-NAME-ADD-1.
                 15 MB1100-NAME-ADD-29          PIC X(29).
                 15 MB1100-NAME-ADD-X           PIC X.
              10 MB1100-NAME-ADD-1-R REDEFINES MB1100-NAME-ADD-1.
                 15 MB1100-NAME-BLANK           PIC X.
                 15 MB1100-NAME-REST            PIC X(29).
              10 MB1100-NAME-ADD-2.
                 15 MB1100-NAME-ADD-292         PIC X(29).
                 15 MB1100-NAME-ADD-X2          PIC X.
              10 MB1100-NAME-ADD-2-R REDEFINES MB1100-NAME-ADD-2.
                 15 MB1100-NAME-BLANK2          PIC X.
                 15 MB1100-NAME-REST2           PIC X(29).
              10 MB1100-NAME-ADD-3              PIC X(30).
              10 MB1100-NAME-ADD-4              PIC X(30).
              10 MB1100-NAME-ADD-5              PIC X(30).
              10 MB1100-NAME-ADD-6              PIC X(21).
              10 MB1100-STATE                   PIC X(2).
              10 MB1100-ZIP                     PIC X(5).
              10 MB1100-DASH                    PIC X.
              10 MB1100-ZIP-4                   PIC X(4).
              10 MB1100-PROP-LINE-1.
                 15 MB1100-PROP-LINE-1A         PIC X(6).
                 15 FILLER REDEFINES MB1100-PROP-LINE-1A.
                    20 6-DIG                    PIC X(6).
                 15 MB1100-PROP-LINE-1B         PIC X(2).
                 15 MB1100-PROP-LINE-1C         PIC X(20).
             10 MB1100-PROP-LINE-C             PIC X(21).
              10 MB1100-PROP-STATE              PIC X(2).
              10 MB1100-PROP-ZIP                PIC X(10).
              10 MB1100-TELE-NO                 PIC S9(10)    COMP-3.
              10 MB1100-SEC-TELE-NO             PIC S9(10)    COMP-3.
              10 MB1100-SS-NO                   PIC S9(9)     COMP-3.
              10 MB1100-CO-SS-NO                PIC S9(9)     COMP-3.
              10 MB1100-DUE-DATE.
                 15 MB1100-DUE-YY               PIC S9(3)     COMP-3.
                 15 MB1100-DUE-MM               PIC S9(2).
                 15 MB1100-DUE-DD               PIC 9(2).
              10 MB1100-COUPON-REQ-DATE.
                 15 COUPON-REQ-YY               PIC S9(3)     COMP-3.
                 15 COUPON-REQ-MM               PIC 99.
                 15 COUPON-REQ-DD               PIC 99.
              10 MB1100-BEG-HIST-DATE.
                 15 BEG-HIST-YY                 PIC S9(3)      COMP-3.
                 15 BEG-HIST-MM                 PIC 99.
                 15 BEG-HIST-DD                 PIC 99.
              10 MB1100-COUPON-TAPE-DATE.
                 15 MB1100-COUPON-YY            PIC S9(3)     COMP-3.
                 15 MB1100-COUPON-MM            PIC 9(2).
                 15 MB1100-COUPON-DD            PIC 9(2).
              10 MB1100-LAST-ANAL.
                 15 MB1100-LAST-ANAL-YY         PIC S9(3)     COMP-3.
                 15 MB1100-LAST-ANAL-MM         PIC 9(2).
                 15 MB1100-LAST-ANAL-DD         PIC 9(2).
              10 MB1100-1ST-IR-CHG-DATE.
                 15 MB1100-1ST-IR-CHG-YY        PIC S9(3)     COMP-3.
                 15 MB1100-1ST-IR-CHG-MM        PIC 9(2).
              10 MB1100-1ST-PI-CHG-DATE.
                 15 MB1100-1ST-PI-CHG-YY        PIC S9(3)     COMP-3.
                 15 MB1100-1ST-PI-CHG-MM        PIC 9(2).
              10 MB1100-LOAN-MATURES.
                 15 MB1100-LOAN-MAT-YY          PIC S9(3)     COMP-3.
                 15 MB1100-LOAN-MAT-MM          PIC 9(2).
      ********************* CURRENT PAYMENT-ELEMENTS ****************
              10 MB1100-TOT-PYMT                PIC S9(9)V99  COMP-3.
              10 MB1100-FIRST-P-I               PIC S9(9)V99  COMP-3.
              10 MB1100-ESC-MTH                 PIC S9(7)V99  COMP-3.
              10 MB1100-COUNTY-TAX              PIC S9(7)V99  COMP-3.
              10 MB1100-CITY-TAX                PIC S9(7)V99  COMP-3.
              10 MB1100-HAZ-PREM                PIC S9(7)V99  COMP-3.
              10 MB1100-MIP                     PIC S9(7)V99  COMP-3.
              10 MB1100-LIEN                    PIC S9(7)V99  COMP-3.
              10 MB1100-O-S-SPREAD              PIC S9(7)V99  COMP-3.
              10 MB1100-A-H-PREM                PIC S9(5)V99  COMP-3.
              10 MB1100-LIFE-PREM               PIC S9(5)V99  COMP-3.
              10 MB1100-REP-RES                 PIC S9(7)V99  COMP-3.
              10 MB1100-MISC-AMT                PIC S9(7)V99  COMP-3.
              10 MB1100-HUD-PART                PIC S9(7)V99  COMP-3.
              10 MB1100-BSC-AMT                 PIC S9(5)V99  COMP-3.
              10 MB1100-L-C-AMT                 PIC S9(7)V99  COMP-3.
      ******************** DOLLAR BALANCES ****************************
              10 MB1100-FIRST-PRIN-BAL          PIC S9(11)V99 COMP-3.
              10 MB1100-ESCROW-BAL              PIC S9(9)V99  COMP-3.
              10 MB1100-ESC-ADV-BAL             PIC S9(9)V99  COMP-3.
              10 MB1100-SUSPENSE-BAL            PIC S9(11)V99  COMP-3.
              10 MB1100-RES-ESCROW              PIC S9(9)V99  COMP-3.
              10 MB1100-REP-RES-BAL             PIC S9(9)V99  COMP-3.
              10 MB1100-ACCRUED-LATE-CHG        PIC S9(7)V99  COMP-3.
              10 MB1100-DEFERRED-INT            PIC S9(9)V99  COMP-3.
              10 MB1100-NSF-BAL                 PIC S9(5)V99  COMP-3.
              10 MB1100-OTHER-FEES              PIC S9(7)V99  COMP-3.
      ************************** YTD-FIELDS **************************
              10 MB1100-PRIN-YTD                PIC S9(11)V99  COMP-3.
              10 MB1100-INTEREST-YTD            PIC S9(11)V99  COMP-3.
              10 MB1100-TAXES-YTD               PIC S9(9)V99  COMP-3.
              10 MB1100-HAZARD-YTD              PIC S9(7)V99  COMP-3.
              10 MB1100-MIP-YTD                 PIC S9(7)V99  COMP-3.
              10 MB1100-LIEN-YTD                PIC S9(7)V99  COMP-3.
              10 MB1100-L-C-YTD                 PIC S9(7)V99  COMP-3.
      ********************** PAYMENT SUMMARY FIELDS ******************
              10 MB1100-PRIN-PD                 PIC S9(11)V99 COMP-3.
              10 MB1100-INT-PD                  PIC S9(11)V99 COMP-3.
              10 MB1100-ESCROW-PD               PIC S9(9)V99  COMP-3.
              10 MB1100-L-C-PD                  PIC S9(7)V99  COMP-3.
              10 MB1100-BSC-PD                  PIC S9(5)V99  COMP-3.
              10 MB1100-A-H-PD                  PIC S9(5)V99  COMP-3.
              10 MB1100-LIFE-PD                 PIC S9(5)V99  COMP-3.
              10 MB1100-SUSPENSE-AMT            PIC S9(11)V99 COMP-3.
              10 MB1100-TOTAL-RECD              PIC S9(11)V99 COMP-3.
              10 MB1100-TOTAL-DUE               PIC S9(11)V99 COMP-3.
      ********************* IRS REPORTING TOTALS *********************
              10 MB1100-INT-PAID                PIC S9(7)V99  COMP-3.
              10 MB1100-TAXES-PAID              PIC S9(7)V99  COMP-3.
              10 MB1100-REVISED-STMT-SW         PIC X.
      ********************** LOAN STATUS INFO   **********************
              10 MB1100-TYPE-LOAN               PIC XX.
              10 MB1100-BANK                    PIC X(3).
              10 MB1100-AGGR                    PIC X(3).
              10 MB1100-1ST-BASIS-CODE          PIC X(3).
              10 MB1100-DIST-TYPE               PIC X.
              10 MB1100-GRACE-DAYS              PIC S9(3)     COMP-3.
              10 MB1100-GRACE-DAYS-R REDEFINES MB1100-GRACE-DAYS
                                                PIC X(2).
              10 MB1100-PMT-PERIOD              PIC S9(3)     COMP-3.
              10 MB1100-ANNUAL-INT              PIC S9(2)V9(5) COMP-3.
              10 MB1100-BILL-MODE               PIC X.
              10 MB1100-ZONE.
                 15 MB1100-ZONE-1               PIC X(2).
                 15 MB1100-ZONE-2               PIC X(2).
              10 MB1100-MAN                     PIC X.
              10 MB1100-TYPE-ACQ                PIC X.
              10 MB1100-STATE-CODE              PIC 9(2).
              10 MB1100-LOAN-TERM               PIC S9(3)     COMP-3.
              10 MB1100-BANKRUPT-CODE           PIC X(2).
              10 MB1100-BANKRUPT-CODE-R REDEFINES MB1100-BANKRUPT-CODE
                                                PIC 99.
              10 MB1100-USER-BILLING-TABLE .
                 15 UBT-POS1                   PIC X.
                 15 UBT-POS2                   PIC X.
                 15 UBT-POS3                   PIC X.
                 15 UBT-POS4                   PIC X.
                 15 UBT-POS5                    PIC X.
                 15 UBT-POS6                    PIC X.
                 15 FILLER                      PIC X(7).
                 15 UBT-POS14                   PIC X.
                 15 UBT-POS15                   PIC X.
                 15 UBT-POS16                   PIC X.
                 15 FILLER                      PIC X.
                 15 UBT-POS18                   PIC X.
                 15 UBT-POS19                   PIC X.
                 15 FILLER                      PIC X.
      *---below only used for stand-alone 1098
              10 MB1100-MORE-1098-DATA
                     REDEFINES MB1100-USER-BILLING-TABLE.
                 15  MB1100-TI-MTG-CODE         PIC X.
                 15  MB1100-TI-TRAN-56-SW       PIC X.
                 15  MB1100-TI-ZERO-PB-CODE     PIC X.
                 15  MB1100-TI-LOAN-DATE        PIC X(6).
                 15  MB1100-TI-ASSUMP-DATE      PIC X(6).
                 15  FILLER                 PIC X(5).
              10 MB1100-STOP-BILL-FLAG          PIC X.
              10 MB1100-BILLING-CYCLE           PIC X(3).
              10 MB1100-PAY-OPTION              PIC X.
      ************************* DELINQUENT INFORMATION ***************
              10 MB1100-DELQ-P-I                PIC S9(11)V99 COMP-3.
              10 MB1100-DELQ-ESC                PIC S9(11)V99 COMP-3.
              10 MB1100-DELQ-L-C                PIC S9(7)V99  COMP-3.
              10 MB1100-DELQ-INS                PIC S9(7)V99  COMP-3.
              10 MB1100-DELQ-OTHER              PIC S9(9)V99  COMP-3.
              10 MB1100-3-POS-FIELD             PIC X(3).
              10 MB1100-INT-DUE                 PIC S9(5)V99  COMP-3.
              10 MB1100-360-365-FACTOR          PIC 9.
              10 MB1100-INT-CALC-OPT            PIC X.
              10 MB1100-PREV-PAID-THRU-DATE.
                 15 MB1100-PREV-PAID-THRU-YY    PIC S9(3)     COMP-3.
                 15 MB1100-PREV-PAID-THRU-MM    PIC 9(2).
                 15 MB1100-PREV-PAID-THRU-DD    PIC 9(2).
              10 MB1100-1ST-DUE-DATE.
                 15 MB1100-1ST-DUE-YY           PIC S9(3)     COMP-3.
                 15 MB1100-1ST-DUE-MM           PIC 9(2).
                 15 MB1100-1ST-DUE-DD           PIC 9(2).
      ************************ SECOND DATA ***************************
              10 MB1100-2ND-INV                 PIC X(3).
              10 MB1100-2ND-CAT                 PIC X(3).
              10 MB1100-2ND-BASIS-CODE          PIC X(3).
              10 MB1100-2ND-MAN                 PIC X(1).
              10 MB1100-2ND-TYPE.
                 15 2ND-HI-TYPE             PIC X.
                 15 2ND-LO-TYPE             PIC X.
              10 MB1100-2ND-ANNUAL-SER-FEE      PIC SV9(8)    COMP-3.
              10 MB1100-2ND-PRIN-BAL            PIC S9(7)V99  COMP-3.
              10 MB1100-2ND-P-I                 PIC S9(5)V99  COMP-3.
              10 MB1100-2ND-ANNUAL-INT          PIC SV9(7)    COMP-3.
      *****************************************************************
              10 MB1100-BILL-ADDR-FOREIGN       PIC X.
              10 MB1100-DONT-PROCESS            PIC X.
              10 MB1100-PIF-STOP                PIC X.
              10 MB1100-FORECLOSURE-STOP        PIC X.
              10 MB1100-BAD-CK-STOP             PIC X.
              10 MB1100-NO-NOTICES              PIC X.
              10 MB1100-DONT-ANALYZE            PIC X.
              10 MB1100-A-H-FLAG                PIC X.
              10 MB1100-LIFE-FLAG               PIC X.
              10 MB1100-DISB-STOP               PIC X.
      ********************* NEW ARM FIELDS ****************************
              10 MB1100-ARM-PLAN-ID             PIC X(4).
              10 MB1100-ARM-IR-CHG-YR-MO.
                 15 MB1100-ARM-IR-YY            PIC S9(3)     COMP-3.
                 15 MB1100-ARM-IR-MM            PIC 9(2).
                 15 MB1100-ARM-IR-DA            PIC 9(2).
              10 MB1100-ARM-PI-CHG-DATE.
                 15 MB1100-ARM-PI-CHG-YY        PIC S9(3)     COMP-3.
                 15 MB1100-ARM-PI-CHG-MM        PIC 9(2).
                 15 MB1100-ARM-PI-CHG-DD        PIC 9(2).
              10 FILLER REDEFINES MB1100-ARM-PI-CHG-DATE.
                 15 FILLER                      PIC X(5).
                 15 INSERT-CODE                 PIC X.
              10 MB1100-MODIFICATION-PROGRAM    PIC X(4).
              10 FILLER                     PIC X(7).
      *** 0874   10 GLB-NUMBER                 PIC 9(10).
      * 745 for 30 bytes
              10 CNP-CLIENT-CUSTOM-DATA     PIC X(29).
              10 CNP-CLIENT-0140-DATA REDEFINES CNP-CLIENT-CUSTOM-DATA.
                 15  FILLER                 PIC X(11).
                 15  MB1100-0140-OPT-OUT-CODE   PIC X.
                 15  MB1100-MODIFICATION-TYPE   PIC X.
                 15  FILLER                 PIC X(2).
      * mb-payment-option-switch, 760, 1, Text, 0
      * mb-payment-opt-4, 761, 6, Packed Number, 2
      * mb-payment-opt-4-totdue, 767, 6, Packed Number, 2
      * mb-dist-type-1-int-only-flag, 773, 1, Text, 0
                 15  MB1100-PAYMENT-OPTION-SWITCH PIC X.
                 15  MB1100-PAYMENT-OPT-4   PIC S9(9)V99 COMP-3.
                 15  MB1100-PAYMENT-OPT-4-TOTDUE PIC S9(9)V99 COMP-3.
                 15  MB1100-DIST-TYPE-1-IO-FLAG PIC X.
              10  MB1100-CORP-ADV           PIC S9(7)V99 COMP-3.
      * 779 for 113 bytes
              10 CNP-MULTI-PAY-OPT-AMTS.
      **** Client 0361 multi-pay option amounts populated by Fidelity
      **** Monthly Payment Options enhancement.  These fields could
      **** conflict with other data so it must be taken into account as
      **** nessesary.  Client 0361 is 1500 so this data gets stored in
      **** a custom area.
                 15 MB1100-PLS-CLT-ID           PIC X(3).
                 15 MB1100-MTGR-LANG-PREF       PIC X(2).
                 15 FILLER                      PIC X(80).
                 15 MB1100-15-YR-PI             PIC S9(9)V99 COMP-3.
                 15 MB1100-15-YR-TOT-DUE        PIC S9(9)V99 COMP-3.
                 15 FILLER                      PIC X(17).
      * next byte after this 10-level begins at 887
              10 CNP-MULTI-PAY-OPT-AMTS-874
                      REDEFINES CNP-MULTI-PAY-OPT-AMTS.
      **** Client 0874 multi-pay option amounts populated by Fidelity
                 15 FILLER                  PIC X(58).
                 15 FILLER                  PIC X(12).
      * NOT SURE WHAT THE PAL-FLAG IS OR WHERE IT COMES FROM YET
      * This is currently offset 849
      *          15 MB1100-PAL-FLAG             PIC X.
                 15 FILLER                      PIC X(25).
      * mb-interest-only-pi, 893, 6, Packed Number, 2
      * mb-fully-am-pi, 899, 6, Packed Number, 2
      * mb-interest-only-totdue, 905, 6, Packed Number, 2
      * mb-fully-am-totdue, 911, 6, Packed Number, 2
      *           15 MB1100-ARM-INT-ONLY-PI      PIC S9(9)V99 COMP-3.
      *           15 MB1100-ARM-FULLY-AM-PI      PIC S9(9)V99 COMP-3.
      *           15 MB1100-ARM-INT-ONLY-TOT     PIC S9(9)V99 COMP-3.
      *           15 MB1100-ARM-FULLY-AM-TOT     PIC S9(9)V99 COMP-3.
                 15 FILLER                  PIC X(19).
      **** Record resumes here.  Below "filler" is now filler still,
      **** as the gp1 error codes, but is used for a payment opt amt
      **** for novastar 0361.  It is converted to custom area 1500
      **** before the gp1 overwrites it!!
              10 MB1100-MODIFICATION REDEFINES CNP-MULTI-PAY-OPT-AMTS.
                 15 FILLER                      PIC X(5).
                 15 MB1100-MODIFICATION-STATUS-YY PIC 9(3)  COMP-3.
                 15 MB1100-MOD-STATUS-MMDD      PIC 9(4).
                 15 FILLER                      PIC X(15).
                 15 MB1100-DEFERRED-BAL-IND     PIC X.
                 15 FILLER                      PIC X(15).
                 15 MB1100-POST-PETITION-YY     PIC 9(3) COMP-3.
                 15 MB1100-POST-PETITION-MMDD   PIC 9(4).
                 15 MB1100-POST-PETITION-AMOUNT PIC S9(9)V99 COMP-3.
                 15 MB1100-MODIFICATION-STATUS  PIC XX.
                 15 MB1100-MODIFICATION-YY      PIC 9(3) COMP-3.
                 15 MB1100-MODIFICATION-MMDD    PIC 9(4).
                 15 MB1100-PAYMENT-OPTION-IND   PIC X.
                 15 MB1100-ORIG-LOAN-AMT        PIC S9(9) COMP-3.
                 15 MB1100-NEG-AM-PB-CAP-VALUE  PIC S9(11)V99 COMP-3.
                 15 MB1100-INT-ONLY-EXP-YY      PIC 9(3) COMP-3.
                 15 MB1100-INT-ONLY-EXP-MMDD    PIC 9(4).
                 15 MB1100-MONTHLY-BORR-ACCR-AMT PIC S9(5)V99 COMP-3.
                 15 MB1100-BORR-INCTV-ANNIV-YY PIC 9(3) COMP-3.
                 15 MB1100-BORR-INCTV-ANNIV-MMDD PIC 9(4).
                 15 MB1100-BORR-INC-ACCR-ANNV2DT PIC S9(5)V99 COMP-3.
                 15 MB1100-TRIAL-MOD-START-YY   PIC 9(3) COMP-3.
                 15 MB1100-TRIAL-MOD-START-MMDD PIC 9(4).
                 15 MB1100-TRIAL-MOD-END-YY     PIC 9(3) COMP-3.
                 15 MB1100-TRIAL-MOD-END-MMDD   PIC 9(4).
                 15 MB1100-TRIAL-MOD-PAYMENT    PIC S9(9)V99 COMP-3.
                 15 FILLER                      PIC X.
      * next byte is at offset 893
              10 MB1100-ARM-INT-ONLY-PI      PIC S9(9)V99 COMP-3.
              10 MB1100-ARM-FULLY-AM-PI      PIC S9(9)V99 COMP-3.
              10 MB1100-ARM-INT-ONLY-TOT     PIC S9(9)V99 COMP-3.
              10 MB1100-ARM-FULLY-AM-TOT     PIC S9(9)V99 COMP-3.
              10 MB1100-INTEREST-DUE         PIC S9(9)V99 COMP-3.
      * scra fields are for 0277 only?
              10 MB1100-SCRA-F               PIC X.
              10 MB1100-SCRA-DATE-3          PIC X(6).
              10 MB1100-SCRA-DATE-4          PIC X(6).
              10 MB1100-SCRA-BILLING-INTEREST PIC S99V9(5) COMP-3.
              10 MB1100-OFF-SCHD-PEND-DATE       PIC X(6).
              10 MB1100-OFF-SCHD-PEND-IR         PIC S99V9(5) COMP-3.
              10 MB1100-OFF-SCHD-PEND-PI         PIC S9(11)V99 COMP-3.
              10 FILLER                     PIC X(27).
              10 MB1100-PAL-FLAG            PIC X.
              10 MB1100-BOA-PREV-POSTED     PIC S9(7)V99 COMP-3.
              10 FILLER                     PIC X(10).
      * next byte is at offset 1000
           05 MB1100-LAST-100.
              10 MB1100-STATEMENT-DATE.
                 15 MB1100-STATEMENT-YY         PIC S9(3)    COMP-3.
                 15 MB1100-STATEMENT-MM         PIC X(2).
                 15 MB1100-STATEMENT-DD         PIC X(2).
              10 MB1100-MOD-REASON-CODE     PIC X(5).
              10 MB1100-MOD-EFF-YY          PIC 9(3) COMP-3.
              10 MB1100-MOD-EFF-MMDD        PIC 9(4).
              10 MB1100-2-TO-1-IND          PIC X.
              10 FILLER                     PIC X.
              10 MB1100-BOA-GENERATED       PIC X.
              10 MB1100-BOA-INIT-STMT       PIC X.
              10 MB1100-BOA-CHECK-FOUND     PIC X.
              10 MB1100-BOA-FINAL-OR-OV-SH  PIC X.
              10 MB1100-KILL-COUP           PIC X.
              10 MB1100-CHK-TYPE            PIC X(3).
              10 MB1100-BOA-PMI-CODE        PIC X(3).
              10 MB1100-BANKRUPTCY-STATUS   PIC X.
              10 MB11-ARM-RATE-PI-NOT-AVAIL-IND  PIC X.
              10 MB1100-NO-CHANGE-SCHED-IND  PIC X.
              10 MB1100-FOR1-STATUS-CD      PIC X(1).
              10 MB1100-BNK-DISCHARGE-IND   PIC X(1).
              10 FILLER                     PIC X(35).
              10 MB1100-ESC-S-KEY           PIC 9(6).
              10 MB1100-ESC-S-COUNT         PIC 9(4).
              10 MB1100-TRAN-KEY            PIC 9(7).
              10 MB1100-TRAN-COUNT          PIC 9(3).
              10 FILLER                     PIC X.
              10 MB1100-CONTAINER-KEY       PIC 9(9).
              10 MB1500-EXPANDED.
                 15 MB1500-BORR-EMAIL-ADDR  PIC X(66).
                 15 MB1500-BORR-DUE-YY      PIC S9(3) comp-3.
                 15 MB1500-BORR-DUE-MM      PIC X(2).
                 15 MB1500-BORR-DUE-DD      PIC X(2).
      *           15 MB1500-NEXT-DRAFT-AMT   PIC S9(9)V99 COMP-3.
      *           15 MB1500-FILLER39         PIC X(39).
      *           15 MB1500-AQ-YY            PIC S9(3) comp-3.
      *           15 MB1500-AQ-MM            PIC X(2).
      *           15 MB1500-AQ-DD            PIC X(2).                
      *           15 MB1500-IR-CHG-YY        PIC S9(3) comp-3.
      *           15 MB1500-IR-CHG-MM        PIC X(2).
      *           15 MB1500-IR-CHG-DD        PIC X(2).
      *           15 MB1500-DUE-DATE-IR      PIC S99V9(5) COMP-3.
      *           15 MB1500-EFF-IR-CHG-YY    PIC S9(3) comp-3.
      *           15 MB1500-EFF-IR-CHG-MM    PIC X(2).
      *           15 MB1500-EFF-IR-CHG-DD    PIC X(2). 
      *           15 MB1500-EFF-OFF-SCHED-YY PIC S9(3) comp-3.
      *           15 MB1500-EFF-OFF-SCHED-MM PIC X(2).
      *           15 MB1500-EFF-OFF-SCHED-DD PIC X(2). 
      *           15 MB1500-PROP-UNIT-NO     PIC X(12).
                 15 MB1500-E-CONSENT        PIC X.
                 15 MB1500-DRAFT-IND        PIC X.
                 15 MB1500-EBPP-IND         PIC XX.
                 15 MB1500-FOR1-SALE-DATE-YR PIC S9(3) comp-3.
                 15 MB1500-FOR1-SALE-DATE-MO PIC S9(2).
                 15 MB1500-FOR1-SALE-DATE-DA PIC S9(2).
                 15 MB1500-ACCELERATED-AMOUNT PIC S9(9)V99 comp-3.
                 15 MB1500-ACCELERATED-REASON-CODE PIC X(2).
                 15 MB1500-DELQ-DAYS               PIC 9(5) comp-3.
                 15 MB1500-CO-BORR-EMAIL-ADDR  PIC X(66).
                 15 MB1500-ACC-INT-DUE-CALC   PIC S9(9)V99 comp-3.
                 15 MB1500-0310-LANGUAGE-CODE PIC X(2).
                 15 FILLER                    PIC X(233).

