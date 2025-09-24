       01  LPSUCFPB-RECORD.
           05 CLIENT-NO                  PIC X(3).
           05 CLIENT-SUFFIX              PIC X.
           05 LOAN-NO                    PIC X(7).
           05 REC-CODE                   PIC X.
           05 REC-NUMBER                 PIC 9(3).
           05 DELQPYMT-DUE-FIELDS OCCURS 13 TIMES.
              10 DELQ-PYMT-DUE-YR        PIC 9(3) COMP-3.
              10 DELQ-PYMT-DUE-MO        PIC 9(2).
              10 DELQ-PYMT-DUE-DA        PIC 9(2).
              10 DELQ-PYMT-DUE-AMT       PIC S9(9)V99 COMP-3.
              10 DELQ-PMT-AMT-WITH-FEES  PIC S9(9)V99 COMP-3.
              10 FILLER                  PIC X(5).
           05 HIST-TABLE-FIELDS OCCURS 12 TIMES.
              10 HIST-DUE-YY             PIC 9(3) COMP-3.
              10 HIST-DUE-MM             PIC 99.
              10 HIST-PROC-YY            PIC 9(3) COMP-3.
              10 HIST-PROC-MM            PIC 99.
              10 HIST-PROC-DD            PIC 99.
              10 HIST-EFF-YY             PIC 9(3) COMP-3.
              10 HIST-EFF-MM             PIC 99.
              10 HIST-EFF-DD             PIC 99.
              10 HIST-DUE-DD             PIC 99.
              10 FILLER                  PIC X(9).
           05 FILLER                     PIC X(3362).

