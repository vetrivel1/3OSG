       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SETMB2000.
       AUTHOR.      ej.
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * /users/public/12345p.asc
            SELECT IN1-FILE  ASSIGN TO DYNAMIC IN1-PATH
                   ORGANIZATION RECORD SEQUENTIAL.
      * /users/public/out/12345p.set
            SELECT OUT-FILE ASSIGN TO DYNAMIC OUT-PATH
                   ORGANIZATION RECORD SEQUENTIAL.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD IN1-FILE
           RECORD CONTAINS 1500 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS IN1-REC.
       01 IN1-REC                      PIC X(1500).

       FD OUT-FILE
           RECORD CONTAINS 2000 CHARACTERS
           DATA RECORD IS MB-REC.
       copy '/users/devel/mb2000.cbl'.
      *

       WORKING-STORAGE SECTION.

       copy '/users/devel/mb1500.cbl'.

       01  VARIABLES.
           05  IN1-PATH.
               10 FILLER               PIC X(14)
                  VALUE '/users/public/'.
               10 IN1-NAME             PIC X(64).
           05  OUT-PATH.
               10 FILLER               PIC X(14)
                  VALUE '/users/public/'.
               10 OUT-NAME             PIC X(64).
           05  REC-CTR                 PIC 9(7)  VALUE 0.
           05  EOF-SW                  PIC 9(1)  VALUE 0.
           05  ANSWER                  PIC X(1).
           05  SWITCH                  PIC X(1)  VALUE 'N'.
           05  SUB                     PIC S9(3) VALUE 0.
           05  WS-COMMAND-LINE         PIC X(100).
           05  WS-CLIENT               PIC X(4).
           05  FILLER REDEFINES WS-CLIENT.
               10  FILLER              PIC X.
               10  WS-CLIENT-3         PIC X(3).
           05  WS-JOB                  PIC X(7).
           05  DISPLAY-COUNT           PIC 9(5)  COMP-3 VALUE 0.
           05  I                       PIC S9(4) COMP.
           05  WS-LOAN-DIGITS          PIC 9(2)  VALUE 0.
           05  WS-ACCOUNT-10           PIC 9(10).
           05  WS-ACCOUNT-10-R REDEFINES WS-ACCOUNT-10.
               10 FILLER               PIC X(2).
               10 WS-ACCOUNT-2         PIC X(2).
               10 WS-ACCOUNT-6         PIC X(6).
           05  WS-ACCOUNT-7            PIC 9(7).
           05  WS-ZIP-10.
               10  WS-ZIP-5            PIC X(5).
               10  FILLER              PIC X.
               10  WS-ZIP-4            PIC X(4).
           05  WS-YYMMDD.
               10  WS-YY               PIC 9(2).
               10  WS-MM               PIC X(2).
               10  WS-DD               PIC X(2).
           05  WS-PYMMDD REDEFINES WS-YYMMDD.
               10  WS-PY               PIC S9(3) COMP-3.
               10  FILLER              PIC X(4).
           05  FILLER REDEFINES WS-YYMMDD.
               10  WS-PYMM             PIC X(4).
               10  FILLER              PIC X(2).
           05  OUT-YYYYMMDD.
               10  OUT-YY              PIC 9(4).
               10  OUT-YYX REDEFINES OUT-YY PIC X(4).
               10  OUT-MM              PIC X(2).
               10  OUT-DD              PIC X(2).

       PROCEDURE DIVISION.
      *
       A010-MAIN-LINE.
           DISPLAY SPACES UPON CRT.
           ACCEPT WS-COMMAND-LINE FROM COMMAND-LINE.
           UNSTRING WS-COMMAND-LINE DELIMITED BY ' '
               INTO WS-CLIENT IN1-NAME WS-JOB.
           DISPLAY SPACES UPON CRT.
           DISPLAY '* * * * * B E G I N   S E T M B 2 0 0 0 . C B L'
               UPON CRT AT 1401.
           DISPLAY 'F O R   Client ' UPON CRT AT 1455.
           DISPLAY WS-CLIENT UPON CRT AT 1470.

           IF WS-CLIENT = SPACES OR IN1-NAME = SPACES
               DISPLAY '!!!! ENTER CLIENT NUMBER'
                   UPON CRT AT 2301
               DISPLAY '!!!!   AND FILE NAME ON COMMAND LINE !!!!'
                   UPON CRT AT 2401
               STOP RUN.
           MOVE IN1-NAME TO OUT-NAME.
           INSPECT OUT-NAME REPLACING ALL 'pp.asc' BY 'p.set '.
           INSPECT OUT-NAME REPLACING ALL 'p.asc ' BY 'p.set '.
           INSPECT OUT-NAME REPLACING ALL 'p.all ' BY 'p.set '.
           INSPECT OUT-NAME REPLACING ALL '.bil'   BY '.set'.
           OPEN INPUT IN1-FILE.
           OPEN OUTPUT OUT-FILE.
           PERFORM READ-MBILL.
           IF MB1100-LOAN-NO-7 NUMERIC
               MOVE 7               TO WS-LOAN-DIGITS
           ELSE
           IF MB1100-LOAN-NO-6 NUMERIC
               MOVE 6               TO WS-LOAN-DIGITS
           ELSE
               MOVE 13              TO WS-LOAN-DIGITS.
           PERFORM CHECK-CLIENT.
           PERFORM 001-MAIN THRU 001-MAIN-EXIT UNTIL EOF-SW = 1.
           PERFORM END-RTN.

       CHECK-CLIENT.
           IF MB1100-CLIENT-NO <> WS-CLIENT-3
               DISPLAY WS-CLIENT-3 'COMMAND LINE CLIENT IS '
                   UPON CRT AT 1801
               DISPLAY MB1100-CLIENT-NO 'FILE CLIENT IS         '
                   UPON CRT AT 1901
               DISPLAY 'FILE / COMMAND LINE CLIENT NUMBER MISMATCH'
                   UPON CRT AT 2301
               CLOSE OUT-FILE
               OPEN OUTPUT OUT-FILE
               PERFORM END-RTN.

       END-RTN.
           DISPLAY "FINAL TOTALS FOR:" UPON CRT AT 0915.
           DISPLAY WS-CLIENT UPON CRT AT 0949.
           DISPLAY REC-CTR  "TOTAL          ="  UPON  CRT AT  1125.
           CLOSE IN1-FILE.
           CLOSE OUT-FILE.
           STOP RUN.

       READ-MBILL.
           READ IN1-FILE AT END MOVE 1 TO EOF-SW.
           IF EOF-SW = 0
              ADD 1 TO REC-CTR
              MOVE IN1-REC TO MB1100-REC.

      ******************************************
      *        START MAIN SECTION              *
      ******************************************

       001-MAIN.
           IF (WS-CLIENT = '0133' OR '0173')
           AND MB1100-BOA-GENERATED = 'G'
              PERFORM BUILD-BOA-MBILL-RECORD
           ELSE
              PERFORM BUILD-CNP-MBILL-RECORD
              PERFORM BUILD-ALLTEL-SERVICER-FIELDS
              PERFORM BUILD-ALLTEL-HMP-FIELDS.
           IF WS-CLIENT = '0140'
              PERFORM BUILD-0140-FIELDS
           ELSE
           IF WS-CLIENT = '0133' OR '0173'
              PERFORM BUILD-0133-FIELDS
           ELSE
           IF WS-CLIENT = '0277'
              PERFORM BUILD-0277-FIELDS
           ELSE
           IF WS-CLIENT = '0310'
              PERFORM BUILD-0310-FIELDS
           ELSE
           IF WS-CLIENT = '0255'
              PERFORM BUILD-0255-FIELDS
           ELSE
           IF WS-CLIENT = '0547'
              PERFORM BUILD-0547-FIELDS
           ELSE
           IF WS-CLIENT = '0346'
              PERFORM BUILD-0346-FIELDS
           ELSE
           IF WS-CLIENT = '0513'
              PERFORM BUILD-0513-FIELDS
           ELSE
           IF WS-CLIENT = '0596'
              PERFORM BUILD-0596-FIELDS
           ELSE
           IF WS-CLIENT = '0588'
              PERFORM BUILD-0588-FIELDS
           ELSE
           IF WS-CLIENT = '0102'
              PERFORM BUILD-0102-FIELDS
           ELSE
           IF WS-CLIENT = '0415'
              PERFORM BUILD-0415-FIELDS
           ELSE
           IF WS-CLIENT = '0733'
              PERFORM BUILD-0733-FIELDS
           ELSE
           IF WS-CLIENT = '0503'
              PERFORM BUILD-0503-FIELDS
           ELSE
           IF WS-CLIENT = '0281'
              PERFORM BUILD-0281-FIELDS
           ELSE
      *     IF WS-CLIENT = '0976'
      *        PERFORM BUILD-0976-FIELDS              
      *     ELSE
           IF WS-CLIENT = '0628'
              PERFORM BUILD-0628-FIELDS.
      *
      * pass SERVICER on command-line
      * so that each servicer's special fields
      * can be added as needed  ?????
      *
           MOVE MB1100-TRAN-KEY        TO MB-TRAN-KEY.
           MOVE MB1100-TRAN-COUNT      TO MB-TRAN-COUNT.
           MOVE WS-JOB              TO MB-JOB.
           MOVE REC-CTR             TO MB-SEQ.
           WRITE MB-REC.
           IF DISPLAY-COUNT = 1000
              DISPLAY REC-CTR  'TOTAL       -> ' UPON CRT AT 1125
              MOVE 0 TO DISPLAY-COUNT.
           ADD 1 TO DISPLAY-COUNT.
           PERFORM READ-MBILL.
       001-MAIN-EXIT.
           EXIT.

       BUILD-CNP-MBILL-RECORD.
           MOVE SPACES              TO MB-REC.
           MOVE MB1100-CLIENT-NO        TO MB-CLIENT.
           IF WS-LOAN-DIGITS = 7
               MOVE MB1100-LOAN-NO-7    TO MB-ACCOUNT
           ELSE
           IF WS-LOAN-DIGITS = 13
               MOVE MB1100-LOAN-NO      TO MB-ACCOUNT
           ELSE
           IF WS-LOAN-DIGITS = 6
               MOVE MB1100-LOAN-NO-6    TO MB-ACCOUNT.
           IF MB1100-SS-NO NUMERIC
               MOVE MB1100-SS-NO        TO MB-SSN.
           IF MB1100-CO-SS-NO NUMERIC
               MOVE MB1100-CO-SS-NO     TO MB-CO-SSN.
           MOVE MB1100-BILL-ADDR-FOREIGN TO MB-FOREIGN-ADDRESS.
           MOVE MB1100-NAME-ADD-1       TO MB-BILL-NAME.
           MOVE MB1100-NAME-ADD-2       TO MB-BILL-LINE-2.
           MOVE MB1100-NAME-ADD-3       TO MB-BILL-LINE-3.
           MOVE MB1100-NAME-ADD-4       TO MB-BILL-LINE-4.
           MOVE MB1100-NAME-ADD-5       TO MB-BILL-LINE-5.
           MOVE MB1100-NAME-ADD-6       TO MB-BILL-CITY.
           MOVE MB1100-STATE            TO MB-BILL-STATE.
           MOVE MB1100-ZIP              TO MB-ZIP-5.
           MOVE MB1100-ZIP-4            TO MB-ZIP-4.
           MOVE MB1100-PROP-LINE-1      TO MB-PROPERTY-STREET.
           MOVE MB1100-PROP-LINE-C      TO MB-PROPERTY-CITY.
           MOVE MB1100-PROP-STATE       TO MB-PROPERTY-STATE.
           MOVE MB1100-PROP-ZIP         TO WS-ZIP-10.
           MOVE WS-ZIP-5            TO MB-PROPERTY-ZIP-5.
           MOVE WS-ZIP-4            TO MB-PROPERTY-ZIP-4.
           MOVE MB1100-STATE-CODE       TO MB-STATE-CODE.
           MOVE MB1100-TELE-NO          TO MB-TELE-NO.
           MOVE MB1100-SEC-TELE-NO      TO MB-SEC-TELE-NO.
           MOVE MB1100-STATEMENT-DATE   TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO  MB-STATEMENT-YY.
           MOVE OUT-MM              TO  MB-STATEMENT-MM.
           MOVE OUT-DD              TO  MB-STATEMENT-DD.
      **** PLANET CODE DUE DATE SHOULD MATCH THE DUE DATE ON BILL'S COUP
           MOVE MB1100-DUE-DATE         TO WS-PYMMDD MB-PLANET-DATE.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO MB-LOAN-DUE-YY.
           MOVE OUT-MM              TO MB-LOAN-DUE-MM.
           MOVE OUT-DD              TO MB-LOAN-DUE-DD.
      *
      * MB-COUPON-DUE-DATE is computed
      *
      *    MOVE TO MB-COUPON-DUE-DATE.
           MOVE MB1100-COUPON-TAPE-DATE TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY                  TO MB-COUPON-TAPE-YY.
           MOVE OUT-MM                  TO MB-COUPON-TAPE-MM
           MOVE OUT-DD                  TO MB-COUPON-TAPE-DD.
           MOVE MB1100-1ST-DUE-DATE     TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO MB-1ST-DUE-YY.
           MOVE OUT-MM              TO MB-1ST-DUE-MM.
           MOVE OUT-DD              TO MB-1ST-DUE-DD.
           MOVE MB1100-BEG-HIST-DATE    TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO MB-BEG-HIST-YY.
           MOVE OUT-MM              TO MB-BEG-HIST-MM.
           MOVE OUT-DD              TO MB-BEG-HIST-DD.
           MOVE MB1100-LOAN-MATURES     TO WS-PYMM.
           PERFORM CONVERT-PYMM.
           MOVE OUT-YY              TO MB-MATURITY-YY.
           MOVE OUT-MM              TO MB-MATURITY-MM.
           MOVE MB1100-ARM-IR-CHG-YR-MO TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO MB-ARM-IR-YY.
           MOVE OUT-MM              TO MB-ARM-IR-MM.
           MOVE OUT-DD              TO MB-ARM-IR-DD.
           MOVE MB1100-ARM-PI-CHG-DATE  TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO MB-ARM-PI-CHG-YY.
           MOVE OUT-MM              TO MB-ARM-PI-CHG-MM.
           MOVE OUT-DD              TO MB-ARM-PI-CHG-DD.
      *    MOVE  TO  MB-LOAN-YY.
      *    MOVE  TO  MB-LOAN-MM.
      *    MOVE  TO  MB-LOAN-DD.

      *    MOVE  TO  MB-ASSUMP-YY.
      *    MOVE  TO  MB-ASSUMP-MM.
      *    MOVE  TO  MB-ASSUMP-DD.

           IF MB1100-GRACE-DAYS NUMERIC
              MOVE MB1100-GRACE-DAYS       TO MB-GRACE-DAYS
           ELSE
              MOVE 15                  TO MB-GRACE-DAYS.

      * M-monthly       / 12 payments per year
      * B-biweekly      / 26 payments per year
      * S-semi-annually / 2 payments per year
      * Q-quarterly     / 4 payments per year
      * A-annually      / 1 payment per year

      *
      * note:   MB1100-PAY-OPTION = 'B' may identify biweeklies
      * the following conversion logic should work for fidelity
      *
           IF MB1100-PMT-PERIOD = 12
              MOVE 'M'              TO MB-PAYMENT-FREQUENCY
           ELSE
           IF MB1100-PMT-PERIOD = 26
              MOVE 'B'              TO MB-PAYMENT-FREQUENCY
           ELSE
           IF MB1100-PMT-PERIOD = 6
              MOVE 'S'              TO MB-PAYMENT-FREQUENCY
           ELSE
           IF MB1100-PMT-PERIOD = 3
              MOVE 'Q'              TO MB-PAYMENT-FREQUENCY
           ELSE
           IF MB1100-PMT-PERIOD = 1
              MOVE 'A'              TO MB-PAYMENT-FREQUENCY
           ELSE
              MOVE 'M'              TO MB-PAYMENT-FREQUENCY.
           MOVE MB1100-ANNUAL-INT       TO  MB-ANNUAL-INTEREST.
           MOVE MB1100-2ND-ANNUAL-INT   TO  MB-2ND-INTEREST.
           MOVE MB1100-TYPE-LOAN        TO  MB-TYPE-LOAN.
           MOVE MB1100-2ND-TYPE         TO  MB-2ND-LOAN-TYPE.
           MOVE MB1100-BILL-MODE        TO  MB-BILL-MODE.
           MOVE MB1100-LOAN-TERM        TO  MB-LOAN-TERM.
           MOVE MB1100-BANKRUPT-CODE    TO  MB-BANKRUPT-CODE.
           MOVE MB1100-FIRST-PRIN-BAL   TO MB-FIRST-PRIN-BAL.
           MOVE MB1100-2ND-PRIN-BAL     TO MB-2ND-PRIN-BAL.
           MOVE MB1100-ESCROW-BAL       TO MB-ESCROW-BAL.
           MOVE MB1100-ESC-ADV-BAL      TO MB-ESCROW-ADVANCE-BAL.
           MOVE MB1100-SUSPENSE-BAL     TO MB-SUSPENSE-BAL.
           MOVE MB1100-RES-ESCROW       TO MB-RES-ESCROW.
           MOVE MB1100-REP-RES-BAL      TO MB-REP-RES-BAL.
           MOVE MB1100-ACCRUED-LATE-CHG TO MB-ACCRUED-LATE-CHG.
           MOVE MB1100-DEFERRED-INT     TO MB-DEFERRED-INT.
           MOVE MB1100-NSF-BAL          TO MB-NSF-BAL.
           MOVE MB1100-OTHER-FEES       TO MB-OTHER-FEES.
           MOVE MB1100-TOT-PYMT         TO MB-PAYMENT-AMOUNT
                                       MB-PLANET-AMOUNT.
           MOVE MB1100-FIRST-P-I        TO MB-FIRST-P-I.
           MOVE MB1100-2ND-P-I          TO MB-2ND-P-I.
           MOVE MB1100-ESC-MTH          TO MB-ESCROW-PAYMENT.
           MOVE MB1100-COUNTY-TAX       TO MB-COUNTY-TAX.
           MOVE MB1100-CITY-TAX         TO MB-CITY-TAX.
           MOVE MB1100-HAZ-PREM         TO MB-HAZ-PREM.
           MOVE MB1100-MIP              TO MB-MIP.
           MOVE MB1100-LIEN             TO MB-LIEN.
           MOVE MB1100-O-S-SPREAD       TO MB-O-S-SPREAD.
           MOVE MB1100-A-H-PREM         TO MB-A-H-PREM.
           MOVE MB1100-LIFE-PREM        TO MB-LIFE-PREM.
           MOVE MB1100-REP-RES          TO MB-REP-RES.
           MOVE MB1100-MISC-AMT         TO MB-MISC-AMT.
           MOVE MB1100-HUD-PART         TO MB-HUD-PART.
           MOVE MB1100-BSC-AMT          TO MB-BSC-AMT.
           MOVE MB1100-L-C-AMT          TO MB-L-C-AMT.
           MOVE MB1100-DELQ-P-I         TO MB-DELQ-P-I.
           MOVE MB1100-DELQ-ESC         TO MB-DELQ-ESC.
           MOVE MB1100-DELQ-L-C         TO MB-DELQ-L-C.
           MOVE MB1100-DELQ-INS         TO MB-DELQ-INS.
           MOVE MB1100-DELQ-OTHER       TO MB-DELQ-OTHER.
           MOVE MB1100-INT-DUE          TO MB-INTEREST-DUE.
           MOVE MB1100-TOTAL-DUE        TO MB-TOTAL-AMOUNT-DUE.
      *     COMPUTE MB-TOTAL-AMOUNT-DUE = MB-DELQ-P-I
      *                                     + MB-DELQ-ESC
      *                                     + MB-DELQ-L-C
      *                                     + MB-DELQ-INS
      *                                     + MB-DELQ-OTHER.
      * ???                                + MB-INTEREST-DUE.
           MOVE MB1100-PRIN-YTD         TO MB-PRIN-YTD.
           MOVE MB1100-INTEREST-YTD     TO MB-INTEREST-YTD.
           MOVE MB1100-TAXES-YTD        TO MB-TAXES-YTD.
           MOVE MB1100-HAZARD-YTD       TO MB-HAZARD-YTD.
           MOVE MB1100-MIP-YTD          TO MB-MIP-YTD.
           MOVE MB1100-LIEN-YTD         TO MB-LIEN-YTD.
           MOVE MB1100-L-C-YTD          TO MB-L-C-YTD.
           MOVE MB1100-INT-PAID         TO MB-LAST-YEAR-INT-PAID.
           MOVE MB1100-TAXES-PAID       TO MB-LAST-YEAR-TAXES-PAID.
           MOVE MB1100-TI-MTG-CODE      TO MB-TI-MTG-CODE.
           MOVE MB1100-TI-TRAN-56-SW    TO MB-TI-TRAN-56-SW.
           MOVE MB1100-TI-ZERO-PB-CODE  TO MB-TI-ZERO-PB-CODE.
           MOVE MB1100-TI-LOAN-DATE     TO WS-PYMMDD.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO MB-TI-LOAN-YY.
           MOVE OUT-MM              TO MB-TI-LOAN-MM.
           MOVE OUT-DD              TO MB-TI-LOAN-DD.
      *    MOVE  TO MB-TI-ASSUMP-DATE.
      *     IF MB1100-TI-HAZARD-YTD NUMERIC
      *        MOVE MB1100-TI-HAZARD-YTD TO MB-TI-HAZARD-YTD.
      *     IF MB1100-TI-MIP-YTD NUMERIC
      *        MOVE MB1100-TI-MIP-YTD    TO MB-TI-MIP-YTD.
      *     IF MB1100-TI-LIEN-YTD NUMERIC
      *        MOVE MB1100-TI-LIEN-YTD   TO MB-TI-LIEN-YTD.
      *     IF MB1100-TI-ESC-MTH NUMERIC
      *        MOVE MB1100-TI-ESC-MTH    TO MB-TI-ESC-MTH.
      *     IF MB1100-TI-TOT-PMT NUMERIC
      *        MOVE MB1100-TI-TOT-PMT    TO MB-TI-TOT-PMT.
           MOVE MB1100-CORP-ADV     TO MB-CORP-ADV.
           MOVE MB1100-CONTAINER-KEY TO CONTAINER-KEY.
           MOVE MB1100-FOR1-STATUS-CD     TO MB-FOR1-STATUS-CD.
           MOVE MB1100-BNK-DISCHARGE-IND  TO MB-BNK-DISCHARGE-IND.

       CONVERT-YYMMDD.
           MOVE WS-YY               TO OUT-YY.
           IF OUT-YY >50
              ADD 1900              TO OUT-YY
           ELSE
              ADD 2000              TO OUT-YY.
           MOVE WS-MM               TO OUT-MM.
           MOVE WS-DD               TO OUT-DD.

       CONVERT-PYMMDD.
           IF WS-PY NUMERIC
              MOVE WS-PY            TO OUT-YY
              ADD 1900              TO OUT-YY
              MOVE WS-MM            TO OUT-MM
              MOVE WS-DD            TO OUT-DD
           ELSE
              MOVE SPACES           TO OUT-YYYYMMDD.

       CONVERT-PYMM.
           IF WS-PY NUMERIC
              MOVE WS-PY            TO OUT-YY
              ADD 1900              TO OUT-YY
              MOVE WS-MM            TO OUT-MM
           ELSE
              MOVE SPACES           TO OUT-YYYYMMDD.

       BUILD-ALLTEL-SERVICER-FIELDS.
           MOVE MB1100-DIST-TYPE           TO MB-DIST-TYPE.
           MOVE MB1100-MAN                 TO MB-MAN.
           MOVE MB1100-2ND-MAN             TO MB-2ND-MAN.
           MOVE MB1100-ZONE                TO MB-ZONE.
           MOVE MB1100-1ST-BASIS-CODE      TO MB-1ST-BASIS-CODE.
           MOVE MB1100-BANK                TO MB-BANK.
           MOVE MB1100-AGGR                TO MB-AGGR.
           MOVE MB1100-2ND-INV             TO MB-2ND-BANK.
           MOVE MB1100-2ND-CAT             TO MB-2ND-CAT.
           MOVE MB1100-ZONE-1              TO MB-ZONE-1.
           MOVE MB1100-ZONE-2              TO MB-ZONE-2.
           MOVE MB1100-TYPE-ACQ            TO MB-TYPE-ACQ.
           MOVE MB1100-USER-BILLING-TABLE  TO MB-USER-BILLING-TABLE.
           MOVE MB1100-STOP-BILL-FLAG      TO MB-STOP-BILL-FLAG.
           MOVE MB1100-BILLING-CYCLE       TO MB-BILLING-CYCLE.
           MOVE MB1100-PAY-OPTION          TO MB-PAY-OPTION.
           MOVE MB1100-DONT-PROCESS        TO MB-DONT-PROCESS.
           MOVE MB1100-PIF-STOP            TO MB-PIF-STOP.
           MOVE MB1100-FORECLOSURE-STOP    TO MB-FORECLOSURE-STOP.
           MOVE MB1100-BAD-CK-STOP         TO MB-BAD-CK-STOP.
           MOVE MB1100-NO-NOTICES          TO MB-NO-NOTICES.
           MOVE MB1100-DONT-ANALYZE        TO MB-DONT-ANALYZE.
           MOVE MB1100-A-H-FLAG            TO MB-A-H-FLAG.
           MOVE MB1100-LIFE-FLAG           TO MB-LIFE-FLAG.
           MOVE MB1100-DISB-STOP           TO MB-DISB-STOP.
           MOVE MB1100-ARM-PLAN-ID         TO MB-ARM-PLAN-ID.
           MOVE MB1100-PAYMENT-OPTION-SWITCH
                                           TO MB-PAYMENT-OPTION-SWITCH.
           MOVE MB1100-PAYMENT-OPT-4       TO MB-PAYMENT-OPT-4.
           MOVE MB1100-PAYMENT-OPT-4-TOTDUE
                                           TO MB-PAYMENT-OPT-4-TOTDUE.
           MOVE MB1100-DIST-TYPE-1-IO-FLAG
                                           TO MB-DIST-TYPE-1-IO-FLAG.
           MOVE MB1100-ARM-INT-ONLY-PI     TO MB-ARM-INT-ONLY-PI.
           MOVE MB1100-ARM-FULLY-AM-PI     TO MB-ARM-FULLY-AM-PI.
           MOVE MB1100-ARM-INT-ONLY-TOT    TO MB-ARM-INT-ONLY-TOT.
           MOVE MB1100-ARM-FULLY-AM-TOT    TO MB-ARM-FULLY-AM-TOT.
           MOVE MB1100-PLS-CLT-ID          TO MB-PLS-CLT-ID.
           MOVE MB1100-MTGR-LANG-PREF      TO MB-MTGR-LANG-PREF.
           MOVE MB1100-POST-PETITION-YY
                               TO MB-POST-PETITION-YY.
           MOVE MB1100-POST-PETITION-MMDD
                               TO MB-POST-PETITION-MMDD.
           MOVE MB1100-POST-PETITION-AMOUNT
                               TO MB-POST-PETITION-AMOUNT.
           MOVE MB1100-BANKRUPTCY-STATUS
                               TO MB-BANKRUPTCY-STATUS.
           MOVE MB11-ARM-RATE-PI-NOT-AVAIL-IND
                               TO MB-ARM-RATE-PI-NOT-AVAIL-IND.
           MOVE MB1100-NO-CHANGE-SCHED-IND
                               TO MB-NO-CHANGE-SCHED-IND.
           MOVE MB1500-EBPP-IND 
                               TO MB-EBPP-IND.                    
           MOVE MB1500-E-CONSENT          TO MB-E-CONSENT.

       BUILD-ALLTEL-HMP-FIELDS.
           MOVE MB1100-MODIFICATION-STATUS
                                 TO MB-MODIFICATION-STATUS.
           MOVE MB1100-MODIFICATION-TYPE
                                 TO MB-MODIFICATION-TYPE.
           MOVE MB1100-MODIFICATION-STATUS-YY
                                 TO MB-MODIFICATION-STATUS-YY.
           MOVE MB1100-MOD-STATUS-MMDD
                                 TO MB-MOD-STATUS-MMDD.
           MOVE MB1100-MODIFICATION-PROGRAM
                                 TO MB-MODIFICATION-PROGRAM.
           MOVE MB1100-MODIFICATION-YY
                                 TO MB-MODIFICATION-YY.
           MOVE MB1100-MODIFICATION-MMDD
                                 TO MB-MODIFICATION-MMDD.
           MOVE MB1100-DEFERRED-BAL-IND
                                 TO MB-DEFERRED-BAL-IND.
           MOVE MB1100-PAYMENT-OPTION-IND
                                 TO MB-PAYMENT-OPTION-IND.
           MOVE MB1100-ORIG-LOAN-AMT
                                 TO MB-ORIG-LOAN-AMT.
           MOVE MB1100-NEG-AM-PB-CAP-VALUE
                                 TO MB-NEG-AM-PB-CAP-VALUE.
           MOVE MB1100-INT-ONLY-EXP-YY
                                 TO MB-INT-ONLY-EXP-YY.
           MOVE MB1100-INT-ONLY-EXP-MMDD
                                 TO MB-INT-ONLY-EXP-MMDD.
           MOVE MB1100-MONTHLY-BORR-ACCR-AMT
                                 TO MB-MONTHLY-BORR-ACCR-AMT.
           MOVE MB1100-BORR-INCTV-ANNIV-YY
                                 TO MB-BORR-INCTV-ANNIV-YY.
           MOVE MB1100-BORR-INCTV-ANNIV-MMDD
                                 TO MB-BORR-INCTV-ANNIV-MMDD.
           MOVE MB1100-BORR-INC-ACCR-ANNV2DT
                                 TO MB-BORR-INC-ACCR-ANNV2DT.
           MOVE MB1100-TRIAL-MOD-START-YY
                                 TO MB-TRIAL-MOD-START-YY.
           MOVE MB1100-TRIAL-MOD-START-MMDD
                                 TO MB-TRIAL-MOD-START-MMDD.
           MOVE MB1100-TRIAL-MOD-END-YY
                                 TO MB-TRIAL-MOD-END-YY.
           MOVE MB1100-TRIAL-MOD-END-MMDD
                                 TO MB-TRIAL-MOD-END-MMDD.
           MOVE MB1100-TRIAL-MOD-PAYMENT
                                 TO MB-TRIAL-MOD-PAYMENT.
           MOVE MB1100-MOD-REASON-CODE
                                 TO MB-MOD-REASON-CODE.
           MOVE MB1100-MOD-EFF-YY
                                 TO MB-MOD-EFF-YY.
           MOVE MB1100-MOD-EFF-MMDD
                                 TO MB-MOD-EFF-MMDD.
           MOVE MB1100-2-TO-1-IND
                                 TO MB-2-TO-1-INDICATOR.
           MOVE MB1100-OFF-SCHD-PEND-DATE
                                 TO MB-OFF-SCHD-PEND-DATE.
           MOVE MB1100-OFF-SCHD-PEND-IR
                                 TO MB-OFF-SCHD-PEND-IR.
           MOVE MB1100-OFF-SCHD-PEND-PI
                                 TO MB-OFF-SCHD-PEND-PI.

       BUILD-0140-FIELDS.
           MOVE MB1100-0140-OPT-OUT-CODE   TO MB-0140-OPT-OUT-CODE.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0133-FIELDS.
           MOVE MB1100-BOA-INIT-STMT      TO MB-0133-ESC-CODE.
           MOVE MB1100-BOA-CHECK-FOUND    TO MB-0133-CHECK.
           MOVE MB1100-BOA-FINAL-OR-OV-SH TO MB-0133-ESC-FLAG.
           MOVE MB1100-KILL-COUP          TO MB-0133-KILL-COUP.
           MOVE MB1100-CHK-TYPE           TO MB-0133-CHK-TYPE.
           MOVE MB1100-BOA-PMI-CODE       TO MB-0133-PMI-CODE.
           MOVE MB1100-ESC-S-KEY          TO ESC-0133-S-KEY.
           MOVE MB1100-ESC-S-COUNT        TO ESC-0133-S-COUNT.
           MOVE MB1100-BOA-GENERATED      TO MB-0133-GENERATED.
           MOVE MB1100-BOA-PREV-POSTED    TO MB-0133-PREV-POSTED.
           MOVE MB1100-PAL-FLAG           TO MB-0133-PAL-FLAG.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0102-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
           MOVE MB1500-BORR-EMAIL-ADDR TO MB-BORR-EMAIL-ADDR.
           STRING MB-LOAN-DUE-MM '/' MB-LOAN-DUE-DD '/'
                  MB-LOAN-DUE-YY DELIMITED BY SIZE
                    INTO MB-BORR-DUE-DATE.
           MOVE MB-TOTAL-AMOUNT-DUE TO MB-BORR-PMT-DUE.
           MOVE MB1500-ACCELERATED-AMOUNT TO
                                          MB-1021-ACCELERATED-AMOUNT.
           MOVE MB1500-ACC-INT-DUE-CALC   TO
                                          MB-1021-ACC-INT-DUE-CALC-AMT.
           MOVE MB1500-CO-BORR-EMAIL-ADDR TO
                                          MB-REMITTANCE-IMB-CODE.

       BUILD-0415-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
           MOVE MB1500-BORR-EMAIL-ADDR TO MB-BORR-EMAIL-ADDR.
           STRING MB-LOAN-DUE-MM '/' MB-LOAN-DUE-DD '/'
                  MB-LOAN-DUE-YY DELIMITED BY SIZE
                    INTO MB-BORR-DUE-DATE.
           MOVE MB-TOTAL-AMOUNT-DUE TO MB-BORR-PMT-DUE.
           MOVE MB1500-ACCELERATED-AMOUNT TO
                                          MB-1021-ACCELERATED-AMOUNT.
           MOVE MB1500-CO-BORR-EMAIL-ADDR TO
                                          MB-REMITTANCE-IMB-CODE.

       BUILD-0733-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0503-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
           MOVE MB1500-BORR-EMAIL-ADDR TO MB-BORR-EMAIL-ADDR.
           STRING MB-LOAN-DUE-MM '/' MB-LOAN-DUE-DD '/'
                  MB-LOAN-DUE-YY DELIMITED BY SIZE
                    INTO MB-BORR-DUE-DATE.
           MOVE MB-TOTAL-AMOUNT-DUE TO MB-BORR-PMT-DUE.
           MOVE MB1500-ACCELERATED-AMOUNT TO
                                          MB-1021-ACCELERATED-AMOUNT.
           MOVE MB1500-CO-BORR-EMAIL-ADDR TO
                                          MB-REMITTANCE-IMB-CODE.
           MOVE MB1100-BANK               TO MB-FLEXFIELD1.
           MOVE MB1100-PLS-CLT-ID         TO MB-FLEXFIELD2.

       BUILD-0281-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
           MOVE MB1500-BORR-EMAIL-ADDR    TO MB-BORR-EMAIL-ADDR.
           MOVE MB1500-DELQ-DAYS          TO MB-DELQ-DAYS.
           MOVE MB1500-DRAFT-IND          TO MB-DRAFT-IND.
           STRING MB-LOAN-DUE-MM '/' MB-LOAN-DUE-DD '/'
                  MB-LOAN-DUE-YY DELIMITED BY SIZE
                    INTO MB-BORR-DUE-DATE.
           MOVE MB-TOTAL-AMOUNT-DUE TO MB-BORR-PMT-DUE.
           MOVE MB1500-ACCELERATED-AMOUNT TO
                                          MB-1021-ACCELERATED-AMOUNT.
           MOVE MB1500-CO-BORR-EMAIL-ADDR TO
                                          MB-REMITTANCE-IMB-CODE.
           STRING WS-ACCOUNT-2 '-' WS-ACCOUNT-6 'A' DELIMITED BY SIZE
                                          INTO MB-FLEXFIELD1.

           IF MB-DELQ-DAYS > 00044
              STRING 'DLQBIL'
                      MB-STATEMENT-YY-R2
                      MB-STATEMENT-MM-R2
                      MB-STATEMENT-DD-R2  DELIMITED BY SIZE
                                          INTO MB-FLEXFIELD2
              ELSE
              STRING 'CURBIL'
                      MB-STATEMENT-YY-R2
                      MB-STATEMENT-MM-R2
                      MB-STATEMENT-DD-R2  DELIMITED BY SIZE
                                          INTO MB-FLEXFIELD2.

       BUILD-0310-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
           MOVE MB1500-ACCELERATED-AMOUNT TO
                                          MB-ACCELERATED-AMOUNT.
           MOVE MB1500-ACCELERATED-REASON-CODE TO
                                          MB-ACCELERATED-REASON-CODE.
           MOVE MB1500-DELQ-DAYS               TO
                                          MB-DELQ-DAYS.
           MOVE MB1500-DRAFT-IND          TO MB-DRAFT-IND.
      *us25467 force LPP
      *****MOVE MB1500-0310-LANGUAGE-CODE TO MB-FLEXFIELD3.
           IF MB1500-0310-LANGUAGE-CODE = 'LF'
              MOVE 'LPP'                TO MB-FLEXFIELD3.

       BUILD-0255-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
          
       BUILD-0976-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
      ***   MOVE MB1500-DRAFT-IND          TO MB-DRAFT-IND.
                            

       BUILD-0547-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-7.
           MOVE WS-ACCOUNT-7              TO MB-OTHER-ACCOUNT.

       BUILD-0346-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.
           MOVE MB1500-FOR1-SALE-DATE-YR  TO
                                          MB-FOR1-SALE-DATE-YR.
           MOVE MB1500-FOR1-SALE-DATE-MO  TO
                                          MB-FOR1-SALE-DATE-MO.
           MOVE MB1500-FOR1-SALE-DATE-DA  TO
                                          MB-FOR1-SALE-DATE-DA.

       BUILD-0513-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0596-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0588-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0277-FIELDS.
           MOVE MB1100-INTEREST-DUE       TO MB-0277-INTEREST-DUE.
           MOVE MB1100-SCRA-F             TO MB-0277-SCRA-F.
           MOVE MB1100-SCRA-DATE-3        TO MB-0277-SCRA-DATE-3.
           MOVE MB1100-SCRA-DATE-4        TO MB-0277-SCRA-DATE-4.
           MOVE MB1100-SCRA-BILLING-INTEREST
                                 TO MB-0277-SCRA-BILLING-INTEREST.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-10.
           MOVE WS-ACCOUNT-10             TO MB-OTHER-ACCOUNT.

       BUILD-0628-FIELDS.
           MOVE MB-ACCOUNT                TO WS-ACCOUNT-7.
           MOVE WS-ACCOUNT-7              TO MB-OTHER-ACCOUNT.
           IF MB1500-BORR-EMAIL-ADDR <> SPACES
      *14682    AND UBT-POS-20 = '1'
              MOVE MB1500-BORR-EMAIL-ADDR TO MB-BORR-EMAIL-ADDR
              STRING MB-LOAN-DUE-MM '/' MB-LOAN-DUE-DD '/'
                 MB-LOAN-DUE-YY DELIMITED BY SIZE
                 INTO MB-BORR-DUE-DATE
              MOVE MB-TOTAL-AMOUNT-DUE TO MB-BORR-PMT-DUE.
      **co-borrower added us16339 12/20/2019
           IF MB1500-CO-BORR-EMAIL-ADDR <> SPACES
      *****REMOVE 14862   AND UBT-POS-20 = '1'
              MOVE MB1500-CO-BORR-EMAIL-ADDR TO
                                          MB-REMITTANCE-IMB-CODE.

       BUILD-BOA-MBILL-RECORD.
           MOVE SPACES              TO MB-REC.
           MOVE MB1100-CLIENT-NO        TO MB-CLIENT.
           IF WS-LOAN-DIGITS = 7
               MOVE MB1100-LOAN-NO-7    TO MB-ACCOUNT
           ELSE
           IF WS-LOAN-DIGITS = 13
               MOVE MB1100-LOAN-NO      TO MB-ACCOUNT
           ELSE
           IF WS-LOAN-DIGITS = 6
               MOVE MB1100-LOAN-NO-6    TO MB-ACCOUNT.
           MOVE MB1100-BILL-ADDR-FOREIGN TO MB-FOREIGN-ADDRESS.
           MOVE MB1100-NAME-ADD-1       TO MB-BILL-NAME.
           MOVE MB1100-NAME-ADD-2       TO MB-BILL-LINE-2.
           MOVE MB1100-NAME-ADD-3       TO MB-BILL-LINE-3.
           MOVE MB1100-NAME-ADD-4       TO MB-BILL-LINE-4.
           MOVE MB1100-NAME-ADD-5       TO MB-BILL-LINE-5.
           MOVE MB1100-NAME-ADD-6       TO MB-BILL-CITY.
           MOVE MB1100-STATE            TO MB-BILL-STATE.
           MOVE MB1100-ZIP              TO MB-ZIP-5.
           MOVE MB1100-ZIP-4            TO MB-ZIP-4.
           MOVE MB1100-STATE-CODE       TO MB-STATE-CODE.
           MOVE MB1100-STATEMENT-DATE   TO WS-PYMMDD.
           MOVE MB1100-MAN              TO MB-MAN.
           PERFORM CONVERT-PYMMDD.
           MOVE OUT-YY              TO  MB-STATEMENT-YY.
           MOVE OUT-MM              TO  MB-STATEMENT-MM.
           MOVE OUT-DD              TO  MB-STATEMENT-DD.

