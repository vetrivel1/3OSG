       01  LPSXCFPB-RECORD.                
           05 CLIENT-NO                    PIC X(3).
           05 CLIENT-SUFFIX                PIC X.
           05 LOAN-NO                      PIC X(7).
           05 REC-CODE                     PIC X.
           05 REC-NUMBER                   PIC 9(3).
           05 BNK1-FILING-DATE.            
              10 BNK1-FILING-DATE-YR       PIC 9(3) COMP-3.
              10 BNK1-FILING-DATE-MO       PIC 9(2).
              10 BNK1-FILING-DATE-DA       PIC 9(2).
           05 BNKL-CLSD-DATE.              
              10 BNKL-CLSD-DATE-YR         PIC 9(3) COMP-3.
              10 BNKL-CLSD-DATE-MO         PIC 9(2).
              10 BNKL-CLSD-DATE-DA         PIC 9(2).
           05 BNKL-CLSD-REASON-CD          PIC X(2).
           05 ADD4-ATTY1-NAME              PIC X(30).
           05 ADD4-ATTY1-ADDR-LINE1        PIC X(30).
           05 ADD4-ATTY1-ADDR-LINE2        PIC X(30).
           05 ADD4-ATTY1-STREET            PIC X(30).
           05 ADD4-ATTY1-CITY              PIC X(21).
           05 ADD4-ATTY1-STATE             PIC X(2).
           05 ADD4-ATTY1-ZIP               PIC X(5).
           05 ADD4-ATTY1-ZIP-SUFFIX        PIC X(4).
           05 ADD4-ATTY2-NAME              PIC X(30).
           05 ADD4-ATTY2-ADDR-LINE1        PIC X(30).
           05 ADD4-ATTY2-ADDR-LINE2        PIC X(30).
           05 ADD4-ATTY2-STREET            PIC X(30).
           05 ADD4-ATTY2-CITY              PIC X(21).
           05 ADD4-ATTY2-STATE             PIC X(2).
           05 ADD4-ATTY2-ZIP               PIC X(5).
           05 ADD4-ATTY2-ZIP-SUFFIX        PIC X(4).
           05 UNPAID-POST-PET-PAYMT        PIC S9(11)V99 COMP-3.
           05 POST1-PET-PAID-TO-DATE       PIC S9(9)V99 COMP-3.
           05 POST1-PET-UNPAID-BAL         PIC S9(9)V99 COMP-3.
           05 POST2-PET-PAID-TO-DATE       PIC S9(9)V99 COMP-3.
           05 POST2-PET-UNPAID-BAL         PIC S9(9)V99 COMP-3.
           05 POST-PET-DUE-DATE-IR         PIC SV9(7) COMP-3.
           05 POST-PET-DUE-DATE-PI         PIC S9(9)V99 COMP-3.
           05 POST-PET-DUE-DATE-ESC        PIC S9(7)V99 COMP-3.
           05 POST-PET-DUE-DATE-MISC       PIC S9(7)V99 COMP-3.
           05 POST-PET-DUE-DATE-LIFE       PIC S9(5)V99 COMP-3.
           05 POST-PET-DUE-DATE-A-H        PIC S9(5)V99 COMP-3.
           05 BNK1-PREPET-BTD-BAL          PIC S9(9)V99 COMP-3.
           05 BANKRUPT-CHAP-DISCHARGE-IND  PIC X(2).
           05 BANKRUPT-CHAP-DISCHARGE-DATE.
              10 BK-CH-DISCHARGE-YR        PIC 9(3) COMP-3.
              10 BK-CH-DISCHARGE-MO        PIC 9(2).
              10 BK-CH-DISCHARGE-DA        PIC 9(2).
           05 POST-PET-AMT-DUE             PIC S9(11)V99 COMP-3.
           05 POST-PET-FEES-CLAIMED        PIC S9(9)V99 COMP-3.
           05 POST-PET-FEES-APPROVD        PIC S9(9)V99 COMP-3.
           05 POST-PET-FEES-AMT-PAID       PIC S9(9)V99 COMP-3.
           05 POST-PET-FEES-REMAIN-DUE     PIC S9(9)V99 COMP-3.
           05 POST-PET-FEES-APPROVED-SLB   PIC S9(9)V99 COMP-3.
           05 POST-PET-REPAY-AMT           PIC S9(9)V99 COMP-3.

