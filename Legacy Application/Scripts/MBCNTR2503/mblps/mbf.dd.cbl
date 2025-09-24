       01  LPSFCFPB-RECORD.
           05 CLIENT-NO PIC X(3).
           05 CLIENT-SUFFIX PIC X.
           05 LOAN-NO PIC X(7).
           05 REC-CODE PIC X.
           05 REC-NUMBER PIC 9(3).
           05 FEE-BALANCE-DATA.
              10 FEE-BALANCE-TABLE OCCURS 36 TIMES.
                 15  FEE-BAL PIC S9(7)V99 COMP-3.
           05 TOTAL-FEE-BAL PIC S9(9)V99 COMP-3.
           05 FILLER PIC X(3799).
           
