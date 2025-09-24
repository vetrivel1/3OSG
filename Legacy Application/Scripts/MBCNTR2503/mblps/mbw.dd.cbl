       01 w-REC.
          10 mbw-client-no                     PIC x(4).
          10 mbw-loan                          PIC s9(13) comp-3.
          10 mbw-rec-code                      PIC x.
          10 mbw-rec-no                        PIC x(3).
          10 mbw-net-disb                      PIC s9(9)v99 comp-3.
          10 mbw-payee                         PIC x(10).
      * LZF^M
          10 mbw-ctl-fld                       PIC s9(11) comp-3.
          10 mbw-total-recd                    PIC s9(9)v99 comp-3.
          10 mbw-prin-pd                       PIC s9(9)v99 comp-3.
          10 mbw-int-pd                        PIC s9(7)v99 comp-3.
          10 mbw-escrow-pd                     PIC s9(7)v99 comp-3.
          10 mbw-fee-pd                        PIC s9(7)v99 comp-3.
          10 filler                            PIC x(5).
          10 mbw-a-h-pd                        PIC s9(7)v99 comp-3.
          10 mbw-life-pd                       PIC s9(7)v99 comp-3.
          10 mbw-suspense-amt                  PIC s9(7)v99 comp-3.
          10 mbw-res-esc-pd                    PIC s9(9)v99 comp-3.
          10 filler                            PIC x(9).
          10 mbw-rep-res                       PIC s9(7)v99 comp-3.
          10 filler                            PIC x(3).
          10 mbw-hud-part                      PIC s9(5)v99 comp-3.
          10 mbw-misc-pd                       PIC s9(9)v99 comp-3.
          10 filler                            PIC x(5).
          10 mbw-fee-code                      PIC x.
          10 MBW-PROC-DATE                     PIC X(6).
      *    10 FILLER REDEFINES MBW-PROC-DATE.
      *    10 MBW-PROC-DATE.                    
      *       15 mbw-proc-date-yr               PIC X(2).
      *       15 mbw-proc-date-mm               PIC X(2).
      *       15 mbw-proc-date-dd               PIC X(2).     
      *    10 MBW-PREV-PAID-THRU-DATE           PIC X(6).
      *    10 FILLER REDEFINES MBW-PREV-PAID-THRU-DATE.
          10 MBW-PREV-PAID-THRU-DATE.
             15 mbw-pp-thru-dt-yy              PIC S9(3) COMP-3.
             15 mbw-pp-thru-dt-mm              PIC X(2).
             15 mbw-pp-thru-dt-dd              PIC X(2).     
      *    10 MBW-PREV-PAID-FROM-DATE           PIC X(6).
      *    10 FILLER REDEFINES MBW-PREV-PAID-FROM-DATE.
          10 MBW-PREV-PAID-FROM-DATE.
             15 mbw-pp-from-dt-yy              PIC S9(3) COMP-3.
             15 mbw-pp-from-dt-mm              PIC X(2).
             15 mbw-pp-from-dt-dd              PIC X(2).     
          10 filler                            PIC x(52).
          10 mbw-rec-corp-adv-reason-code      PIC x(4).
          10 mbw-rec-corp-adv-reason-code-desc PIC x(16).
          10 filler                            PIC x(50).
          10 mbw-fee-activity-data-Group OCCURS 15 TIMES.
             15 mbw-fee-tran-code              PIC x(3).
             15 mbw-fee-date                   PIC x(6).
             15 mbw-fee-amt                    PIC s9(7)v99 comp-3.
             15 mbw-fee-code                   PIC x.
          10 filler                            PIC x(512).
          10 mbw-2to1-ind                      PIC x.
          10 mbw-susp-act-cd                   PIC x(4).
          10 mbw-disb-proc-yr                  PIC 9(3) comp-3.
          10 mbw-disb-proc-mo                  PIC x(2).
          10 mbw-disb-proc-da                  PIC x(2).
          10 filler                            PIC x(2989).
