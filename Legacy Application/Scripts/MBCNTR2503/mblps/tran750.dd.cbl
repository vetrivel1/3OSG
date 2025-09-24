       01  CNP750-TRAN-REC.
           05  T-TYPE               PIC X.
           05  T-ACCOUNT            PIC S9(13) COMP-3.
           05  T-HISTORY-DISB       PIC S9(9)V99 COMP-3.
           05  T-HISTORY-1.
               10 TRAN-CODE         PIC X(3).
               10 DH-DUE-DATE       PIC X(6).
               10 PROC-DATE         PIC X(6).
           05  T-HISTORY-2.
               10 TOT-RECD                PIC S9(11)V99   COMP-3.
               10 PRIN-PD                 PIC S9(11)V99   COMP-3.
               10 INT-PD                  PIC S9(11)V99   COMP-3.
               10 ESCROW-PD               PIC S9(09)V99   COMP-3.
               10 L-C-PD                  PIC S9(07)V99   COMP-3.
               10 BSC-PD                  PIC S9(05)V99   COMP-3.
               10 A-H-PD                  PIC S9(05)V99   COMP-3.
               10 LIFE-PD                 PIC S9(05)V99   COMP-3.
               10 SUSPENSE-AMT            PIC S9(11)V99   COMP-3.
           05  FILLER               PIC X(17).
           05  CORP-ADV             PIC X(1).
           05  FILLER               PIC X(652).
