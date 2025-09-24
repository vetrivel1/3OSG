       01  COBOL-LAYOUT.
           05  mb-client-no                    PIC S9(4).
      * Mixed
           05  mb-loan-no                      PIC X(7).
           05  mb-rec-code                     PIC X(1).
           05  mb-rec-no                       PIC S9(3).
           05  mb-net-disb                     PIC S9(9)V9(2) COMP-3.
           05  mb-payee                        PIC X(10).
      * LZF
           05  mb-ctl-fld                      PIC S9(11) COMP-3.
           05  mb-total-recd-s                 PIC S9(9)V9(2) COMP-3.
           05  mb-prin-pd-s                    PIC S9(9)V9(2) COMP-3.
           05  mb-int-pd-s                     PIC S9(7)V9(2) COMP-3.
           05  mb-escrow-pd-s                  PIC S9(7)V9(2) COMP-3.
           05  mb-l-c-pd-s                     PIC S9(7)V9(2) COMP-3.
           05  mb-bsc-pd-s                     PIC S9(7)V9(2) COMP-3.
           05  mb-a-h-pd-s                     PIC S9(7)V9(2) COMP-3.
           05  mb-life-pd-s                    PIC S9(7)V9(2) COMP-3.
           05  mb-suspense-amt-s               PIC S9(7)V9(2) COMP-3.
           05  mb-res-esc-pd-s                 PIC S9(9)V9(2) COMP-3.
           05  mb-fha-penalty-s                PIC S9(3)V9(2) COMP-3.
           05  mb-refund-adv-s                 PIC S9(9)V9(2) COMP-3.
           05  mb-replace-res-s                PIC S9(7)V9(2) COMP-3.
           05  mb-235-fee-s                    PIC S9(3)V9(2) COMP-3.
           05  mb-hud-part-s                   PIC S9(5)V9(2) COMP-3.
           05  mb-misc-pd-s                    PIC S9(9)V9(2) COMP-3.
           05  mb-re-app-pd-s                  PIC S9(1)V9(2) COMP-3.
           05  mb-c-o-pd-s                     PIC S9(3)V9(2) COMP-3.
           05  mb-fee-code-s                   PIC X(1).
           05  mb-proc-date-s                  PIC X(6).
      * include in css
           05  filler redefines mb-proc-date-s.
               10  mb-proc-yy-s                PIC S9(2).
               10  mb-proc-mm-s                PIC S9(2).
               10  mb-proc-dd-s                PIC S9(2).
           05  mb-prev-paid-yy-s               PIC S9(3) COMP-3.
           05  mb-prev-paid-mm-s               PIC S9(2).
           05  mb-prev-paid-dd-s               PIC S9(2).
           05  mb-prev-paid-from-yy-s          PIC S9(3) COMP-3.
           05  mb-prev-paid-from-mm-s          PIC S9(2).
           05  mb-prev-paid-from-dd-s          PIC S9(2).
           05  mb-int-due-pd-s                 PIC S9(9)V9(2) COMP-3.
           05  mb-text147                      PIC X(46).
           05  mb-rec-corp-adv-reason          PIC X(4).
           05  mb-rec-corp-adv-desc            PIC X(16).
           05  filler                          PIC X(50).
           05  mb-fee-data occurs 15 times.
               10  mb-fee-tran-code            PIC X(3).
               10  mb-fee-date                 PIC X(6).
               10  mb-fee-amt                  PIC S9(7)V99 COMP-3.
               10  mb-fee-code                 PIC X(1).
           05  mb-eor                          PIC X(512).
           05  mb-2to1-ind                     PIC X(1).
           05  mb-susp-act-cd                  PIC X(4).
           05  mb-disb-proc-date.
               10  mb-disb-proc-date-yr        PIC S9(3) COMP-3.
               10  mb-disb-proc-date-mo        PIC XX.
               10  mb-disb-proc-date-da        PIC XX.
           05  PMTDEF-RSN                      PIC X(4).
           05  PMTDEF-TY                       PIC XX.
           05  PMTDEF-AM                       PIC S9(11)V99 COMP-3.
           05  PMTDEF-BA                       PIC S9(11)V99 COMP-3.
           05  PMTDEF-EFF-DT.
               10 PMTDEF-EFF-DT-YR             PIC S9(3) COMP-3.
               10 PMTDEF-EFF-DT-MO             PIC XX.
               10 PMTDEF-EFF-DT-DA             PIC XX.
           05  FILLER                          PIC X(2963).
