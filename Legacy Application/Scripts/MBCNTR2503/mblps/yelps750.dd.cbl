       01  CNP-TI-REC.
           05  TI-CLIENT-ID-FIELDS.
               10  TI-CLIENT                 PIC X(4).
               10  FILLER                    PIC X(6).
           05  TI-ACCOUNT-FIELDS.
               10  TI-ACCOUNT                PIC S9(13) COMP-3.
               10  TI-OTHER-ACCOUNT          PIC X(20).
               10  TI-SSN                    PIC 9(9)   COMP-3.
               10  TI-SSN-TIN-CODE           PIC X.
               10  TI-CO-SSN                 PIC 9(9)   COMP-3.
               10  TI-CO-SSN-TIN-CODE        PIC X.
           05  TI-CONTROL-FIELDS.
               10  TI-TYPE-LOAN              PIC X(2).
               10  TI-STATE                  PIC X(2).
               10  TI-MTG-CODE               PIC X.
               10  TI-TRAN-56-SW             PIC X.
               10  TI-ZERO-PB-CODE           PIC X.
               10  TI-LOAN-DATE.   
                   15  TI-LOAN-DATE-YR-MO.
                       20  TI-LOAN-DATE-YR   PIC 999 COMP-3.
                       20  TI-LOAN-DATE-MO   PIC 99.
                   15  TI-LOAN-DATE-DA       PIC 99.
               10  TI-ASSUMP-DATE.
                   15  TI-ASSUMP-YR        PIC 99.
                   15  TI-ASSUMP-MO-DA.
                       20  TI-ASSUMPTI-MO  PIC 99.
                       20  TI-ASSUMP-DA    PIC 99.
      * this field is 4 bytes, equivalent to (unsigned int) in c
               10  TI-SEQ                    PIC 9(8) COMP.
               10  TI-PROCESSING-DATE.
                   15  TI-PROCESSING-MM      PIC 99.
                   15  TI-PROCESSING-DD      PIC 99.
                   15  TI-PROCESSING-YY      PIC 99.
               10  TI-NUM-PROPERTIES         PIC 9(3) COMP-3.
               10  TI-ACQ-DATE.
                   15 TI-ACQ-YR              PIC 999 COMP-3.
                   15 TI-ACQ-MO              PIC 99.
                   15 TI-ACQ-DA              PIC 99.
               10  Y-E-ACQ-RPT-FLAG          PIC X.
               10  FILLER                    PIC X(2).
           05  TI-ALLTEL-FIELDS.
               10 TI-IOE-IORE-WH-YTD         PIC S9(7)V99 COMP-3.
               10 TI-MAN                     PIC X.
               10 TI-ZONE                    PIC XX.
               10 TI-1ST-BASIS-CODE          PIC X(3).
               10 TI-AGGR                    PIC X(3).
               10 TI-BUYDOWN-SUBSIDY-IND     PIC X.
               10 TI-PRINT-CTRL-SW           PIC X.
               10 TI-CAP-LOAN-FLAG           PIC X.
               10 FILLER                     PIC X(3).
           05  TI-SPLIT-FIELDS.
      * PMI-CODE VALUES  1 = pre, 2 = post, xx = state
               10  TI-PMI-CODE               PIC X(3).
                   88  PRE-PMI                VALUE '1  '.
                   88  PRE-CA                 VALUE '1CA'.
                   88  PRE-MN                 VALUE '1MN'.
                   88  PRE-TX                 VALUE '1TX'.
                   88  PRE-NY                 VALUE '1NY'.
                   88  PST-PMI                VALUE '2  '.
                   88  PST-CA                 VALUE '2CA'.
                   88  PST-MN                 VALUE '2MN'.
                   88  PST-TX                 VALUE '2TX'.
                   88  PST-NY                 VALUE '2NY'.
                   88  NO-PMI                 VALUE '   '.
                   88  ALLIANCE-REMOVED-PMI   VALUE 'XXX'.
                   88  GUIDANCE-REMOVED-PMI   VALUES
                   '1  ', '1CA', '1MN', '1TX', '1NY',
                   '2  ', '2CA', '2MN', '2TX', '2NY'.
               10  TI-DETAIL-HISTORY         PIC X.
               10  TI-LPMI-IND               PIC X.
               10  FILLER                    PIC X(25).
           05  TI-SAMPLE-FIELDS.
               10  TI-STMT-SELECTED-AS-SAMPLE PIC X.
               10  TI-SAMPLE-REASONX.
                   15  TI-SAMPLE-REASON       PIC 99.
               10  TI-SAMPLE-LOAN-SW          PIC X.
               10  FILLER                     PIC X(26).
           05  TI-NAME-ADDRESS.
               10  TI-FOREIGN-ADDRESS         PIC X.
               10  TI-BILL-NAME               PIC X(40).
               10  TI-BILL-LINE-2             PIC X(40).
               10  TI-BILL-LINE-3             PIC X(40).
               10  TI-BILL-LINE-4             PIC X(40).
               10  TI-BILL-LINE-5             PIC X(40).
               10  TI-BILL-LINE-6             PIC X(33).
      *         10  FILLER REDEFINES TI-BILL-LINE-6.
      *             15  TI-BILL-CITY           PIC X(31).
      *             15  TI-BILL-STATE          PIC X(2).
      *                 88 CA-OPT-OUT VALUE 'CA'.
               10  TI-BILL-ZIP-CODE.
                   15  TI-ZIP-CODE            PIC X(5).
                   15  TI-ZIP-4               PIC X(4).
               10  TI-CARRIER-ROUTE.
                   15  TI-DPBC                PIC X(3).
                   15  TI-CHECK-DP            PIC X(1).
               10  GROUP1-ERROR-CODES         PIC X(6).
               10  FILLER                     PIC X(44).
           05  TI-1098-DATA.
               10  TI-BANK                    PIC X(3).
               10  TI-DOLLAR-BALS.
                   15  TI-ESCROW-BAL          PIC S9(7)V99 COMP-3.
                   15  TI-ESC-BAL-BEG         PIC S9(7)V99 COMP-3.
                   15  TI-PRIN-BAL            PIC S9(9)V99 COMP-3.
                   15  TI-PRIN-BAL-BEG        PIC S9(9)V99 COMP-3.
               10  TI-YTD-FLDS.
                   15  TI-PRIN-YTD            PIC S9(9)V99  COMP-3.
                   15  TI-INT-APP-YTD         PIC S9(11)V99 COMP-3.
                   15  TI-TAXES-YTD           PIC S9(7)V99  COMP-3.
                   15  TI-HAZARD-YTD          PIC S9(7)V99  COMP-3.
                   15  TI-MIP-YTD             PIC S9(7)V99  COMP-3.
                   15  TI-LIEN-YTD            PIC S9(7)V99  COMP-3.
                   15  TI-1098-INT            PIC S9(11)V99 COMP-3.
                   15  TI-LATE-CHG-PD         PIC S9(7)V99  COMP-3.
                   15  TI-POINTS-PD-BY-BORROWER  PIC S9(9)V99 COMP-3.
                   15  TI-INT-ON-ESCROW       PIC S9(7)V99  COMP-3.
                   15  TI-CAL-NET-INT         PIC S9(11)V99 COMP-3.
                   15  TI-REIMBURSED-AMOUNT   PIC S9(7)V99 COMP-3.
                   15  TI-TOTAL-DEDUCTIBLE-MI PIC S9(7)V99 COMP-3.
                   15  FILLER                 PIC X(15).
               10  TI-MISC-FLDS.
                   15  TI-AMT-DISB            PIC S9(7)V99 COMP-3.
                   15  TI-ESC-DEP             PIC S9(7)V99 COMP-3.
                   15  TI-OTHER-DISB          PIC S9(7)V99 COMP-3.
                   15  TI-TOT-PAYMT           PIC S9(7)V99 COMP-3.
                   15  TI-ESCROW-MTH          PIC S9(5)V99 COMP-3.
               10  TI-DUE-DATE.
                   15  TI-DUE-YR              PIC 99.
                   15  TI-DUE-MO-DA.
                       20  TI-DUE-MO          PIC 99.
                       20  TI-DUE-DA          PIC 99.
               10  TI-REVISED-STMT-SW         PIC X.
               10  TI-H-FHA-INFO              PIC X(14).
      *         10  FILLER REDEFINES TI-H-FHA-INFO.
      *             15  TI-H-FHA-SECTION       PIC X(3).
      *             15  TI-H-FHA-NUMBER.
      *                 20  FILLER             PIC X.
      *                 20  TI-H-FHA-NUMBER-COL PIC X(9).
      *             15  FILLER REDEFINES TI-H-FHA-NUMBER.
      *                 20  TI-H-FHA-1         PIC X(3).
      *                 20  TI-H-FHA-2         PIC X(7).
      *             15  FILLER                 PIC X.
               10  TI-H-AS-OF-DATE.
                   15  TI-H-AS-OF-YY          PIC XX.
                   15  TI-H-AS-OF-MM          PIC XX.
                   15  TI-H-AS-OF-DD          PIC XX.
               10  TI-H-PAYOFF-AMOUNT         PIC S9(7)V99 COMP-3.
               10  TI-H-INSURED-DATE.
                   15  TI-H-INS-YY            PIC XX.
                   15  TI-H-INS-MM            PIC XX.
                   15  TI-H-INS-DD            PIC XX.
               10  TI-IRS-PURPOSE-CODE        PIC X.
           05  FILLER                         PIC X(46).
           05  TI-SECONDARY-FILE.
               10  TI-KEY                     PIC 9(7).
               10  TI-COUNT                   PIC 9(3).
               10  TI-TRAN-COUNT              PIC S9(3) COMP-3.
           05  TI-CNP-CONTROL-FIELDS.
               10  TI-JOB                     PIC X(6).
               10  TI-STACK                   PIC 9(6).
               10  FILLER                     PIC X(8).
           05  TI-CLIENT-FIELDS               PIC X(30).

