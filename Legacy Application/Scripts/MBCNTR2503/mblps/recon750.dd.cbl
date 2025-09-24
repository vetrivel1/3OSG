       01  CNP750-RECON-REC.
           05  R-TYPE                       PIC X.
           05  R-ACCOUNT                    PIC S9(13) COMP-3.
           05  R-DISB-DATA OCCURS 3 TIMES.
               10  R-DISB-TRAN                PIC XXX.
               10  R-DISB-DESC                PIC X(15).
               10  R-DISB-AMT                 PIC S9(7)V99 COMP-3.
           05  FILLER                          PIC X(673).

