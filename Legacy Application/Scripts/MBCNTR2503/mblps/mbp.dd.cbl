       01  COBOL-LAYOUT.
           05  mb-client-no                    PIC X(4).
      * Mixed
           05  mb-loan                         PIC X(7).
           05  mb-rec-code                     PIC X(1).
           05  mb-rec-no                       PIC S9(3).
           05  mb-name-add-1                   PIC X(30).
           05  mb-name-add-2                   PIC X(30).
           05  mb-name-add-3                   PIC X(30).
           05  mb-name-add-4                   PIC X(30).
           05  mb-name-add-5                   PIC X(30).
           05  mb-city                         PIC X(21).
           05  mb-state                        PIC X(2).
           05  mb-zip                          PIC X(5).
           05  mb-dash                         PIC X(1).
           05  mb-zip-4                        PIC X(4).
           05  mb-prop-no                      PIC X(6).
           05  mb-prop-dir                     PIC X(2).
           05  mb-prop-name                    PIC X(20).
           05  mb-prop-line-2                  PIC X(33).
      * include in css
           05  filler redefines mb-prop-line-2.
               10  filler                      pic x(23).
               10  mb-prop-zip                 PIC X(10).
           05  mb-tele-no                      PIC S9(11) COMP-3.
           05  mb-sec-tele-no                  PIC S9(11) COMP-3.
           05  mb-ss-no                        PIC S9(9) COMP-3.
           05  mb-co-ss-no                     PIC S9(9) COMP-3.
           05  mb-due-yy                       PIC S9(3) COMP-3.
           05  mb-due-mm                       PIC S9(2).
           05  mb-due-dd                       PIC S9(2).
           05  coupon-req-yy                   PIC S9(3) COMP-3.
           05  coupon-req-mm                   PIC S9(2).
           05  coupon-req-dd                   PIC S9(2).
           05  beg-hist-yy                     PIC S9(3) COMP-3.
           05  beg-hist-mm                     PIC S9(2).
           05  beg-hist-dd                     PIC S9(2).
           05  mb-coupon-yy                    PIC S9(3) COMP-3.
           05  mb-coupon-mm                    PIC S9(2).
           05  mb-coupon-dd                    PIC S9(2).
           05  mb-last-anal-yy                 PIC S9(3) COMP-3.
           05  mb-last-anal-mm                 PIC S9(2).
           05  mb-last-anal-dd                 PIC S9(2).
           05  mb-1st-ir-chg-yy                PIC S9(3) COMP-3.
           05  mb-1st-ir-chg-mm                PIC S9(2).
           05  mb-1st-pi-chg-yy                PIC S9(3) COMP-3.
           05  mb-1st-pi-chg-mm                PIC S9(2).
           05  mb-loan-mat-yy                  PIC S9(3) COMP-3.
           05  mb-loan-mat-mm                  PIC S9(2).
           05  mb-tot-pymt                     PIC S9(9)V9(2) COMP-3.
           05  mb-first-p-i                    PIC S9(9)V9(2) COMP-3.
           05  mb-esc-mth                      PIC S9(7)V9(2) COMP-3.
           05  mb-county-tax                   PIC S9(7)V9(2) COMP-3.
           05  mb-city-tax                     PIC S9(7)V9(2) COMP-3.
           05  mb-haz-prem                     PIC S9(7)V9(2) COMP-3.
           05  mb-mip                          PIC S9(7)V9(2) COMP-3.
           05  mb-lien                         PIC S9(7)V9(2) COMP-3.
           05  mb-o-s-spread                   PIC S9(7)V9(2) COMP-3.
           05  mb-a-h-prem                     PIC S9(5)V9(2) COMP-3.
           05  mb-life-prem                    PIC S9(5)V9(2) COMP-3.
           05  mb-rep-res                      PIC S9(7)V9(2) COMP-3.
           05  mb-misc-amt                     PIC S9(7)V9(2) COMP-3.
           05  mb-hud-part                     PIC S9(7)V9(2) COMP-3.
           05  mb-bsc-amt                      PIC S9(5)V9(2) COMP-3.
           05  mb-l-c-amt                      PIC S9(7)V9(2) COMP-3.
           05  mb-first-prin-bal               PIC S9(11)V9(2) COMP-3.
           05  mb-escrow-bal                   PIC S9(9)V9(2) COMP-3.
           05  mb-esc-adv-bal                  PIC S9(9)V9(2) COMP-3.
           05  mb-suspense-bal                 PIC S9(11)V9(2) COMP-3.
           05  mb-res-escrow                   PIC S9(9)V9(2) COMP-3.
           05  mb-rep-res-bal                  PIC S9(9)V9(2) COMP-3.
           05  mb-accrued-late-chg             PIC S9(7)V9(2) COMP-3.
           05  mb-deferred-int                 PIC S9(9)V9(2) COMP-3.
           05  mb-nsf-bal                      PIC S9(5)V9(2) COMP-3.
           05  mb-other-fees                   PIC S9(7)V9(2) COMP-3.
           05  mb-prin-ytd                     PIC S9(11)V9(2) COMP-3.
           05  mb-interest-ytd                 PIC S9(11)V9(2) COMP-3.
           05  mb-taxes-ytd                    PIC S9(9)V9(2) COMP-3.
           05  mb-hazard-ytd                   PIC S9(7)V9(2) COMP-3.
           05  mb-mip-ytd                      PIC S9(7)V9(2) COMP-3.
           05  mb-lien-ytd                     PIC S9(7)V9(2) COMP-3.
           05  mb-l-c-ytd                      PIC S9(7)V9(2) COMP-3.
           05  mb-prin-pd                      PIC S9(11)V9(2) COMP-3.
           05  mb-int-pd                       PIC S9(11)V9(2) COMP-3.
           05  mb-escrow-pd                    PIC S9(9)V9(2) COMP-3.
           05  mb-l-c-pd                       PIC S9(7)V9(2) COMP-3.
           05  mb-bsc-pd                       PIC S9(5)V9(2) COMP-3.
           05  mb-a-h-pd                       PIC S9(5)V9(2) COMP-3.
           05  mb-life-pd                      PIC S9(5)V9(2) COMP-3.
           05  mb-suspense-amt                 PIC S9(11)V9(2) COMP-3.
           05  mb-total-recd                   PIC S9(11)V9(2) COMP-3.
           05  mb-total-due                    PIC S9(11)V9(2) COMP-3.
           05  mb-int-paid                     PIC S9(7)V9(2) COMP-3.
           05  mb-taxes-paid                   PIC S9(7)V9(2) COMP-3.
           05  mb-revised-stmt-sw              PIC X(1).
           05  mb-hi-type                      PIC X(1).
           05  mb-lo-type                      PIC X(1).
           05  mb-bank                         PIC X(3).
           05  mb-aggr                         PIC X(3).
           05  mb-1st-basis-code               PIC X(3).
           05  mb-dist-type                    PIC X(1).
           05  mb-grace-days                   PIC S9(3) COMP-3.
           05  mb-pmt-period                   PIC S9(3) COMP-3.
           05  mb-annual-int                   PIC S9(2)V9(5) COMP-3.
           05  mb-bill-mode                    PIC X(1).
           05  mb-zone                         PIC X(4).
           05  mb-man                          PIC X(1).
           05  mb-type-acq                     PIC X(1).
           05  mb-state-code                   PIC S9(2).
           05  mb-loan-term                    PIC S9(3) COMP-3.
           05  mb-bankrupt-code                PIC X(2).
           05  mb-user-billing-table           PIC X(20).
      * include in css
           05  FILLER REDEFINES MB-USER-BILLING-TABLE.
               10 UBT-POS1                     PIC X.
               10 UBT-POS2                     PIC X.
               10 UBT-POS3                     PIC X.
               10 UBT-POS4                     PIC X.
               10 UBT-POS5                     PIC X.
               10 UBT-POS6                     PIC X.
               10 UBT-POS7                     PIC X.
               10 UBT-POS8                     PIC X.
               10 UBT-POS9                     PIC X.
               10 UBT-POS10                    PIC X.
               10 UBT-POS11                    PIC X.
               10 UBT-POS12                    PIC X.
               10 UBT-POS13                    PIC X.
               10 UBT-POS14                    PIC X.
               10 UBT-POS15                    PIC X.
               10 UBT-POS16                    PIC X.
               10 UBT-POS17                    PIC X.
               10 UBT-POS18                    PIC X.
               10 UBT-POS19                    PIC X.
               10 UBT-POS20                    PIC X.
           05  mb-stop-bill-flag               PIC X(1).
           05  mb-billing-cycle                PIC X(3).
           05  mb-pay-option                   PIC X(1).
           05  mb-delq-p-i                     PIC S9(11)V9(2) COMP-3.
           05  mb-delq-esc                     PIC S9(11)V9(2) COMP-3.
           05  mb-delq-l-c                     PIC S9(7)V9(2) COMP-3.
           05  mb-delq-ins                     PIC S9(7)V9(2) COMP-3.
           05  mb-delq-other                   PIC S9(9)V9(2) COMP-3.
           05  mb-3-pos-field                  PIC X(3).
           05  mb-int-due                      PIC S9(5)V9(2) COMP-3.
           05  mb-360-365-factor               PIC S9(1).
           05  mb-int-calc-opt                 PIC X(1).
           05  mb-prev-paid-thru-yy            PIC S9(3) COMP-3.
           05  mb-prev-paid-thru-mm            PIC S9(2).
           05  mb-prev-paid-thru-dd            PIC S9(2).
           05  mb-1st-due-yy                   PIC S9(3) COMP-3.
           05  mb-1st-due-mm                   PIC S9(2).
           05  mb-1st-due-dd                   PIC S9(2).
           05  mb-2nd-inv                      PIC X(3).
           05  mb-2nd-cat                      PIC X(3).
           05  mb-2nd-basis-code               PIC X(3).
           05  mb-2nd-man                      PIC X(1).
           05  2nd-hi-type                     PIC X(1).
           05  2nd-lo-type                     PIC X(1).
           05  mb-2nd-annual-ser-fee           PIC S9(1)V9(8) COMP-3.
           05  mb-2nd-prin-bal                 PIC S9(7)V9(2) COMP-3.
           05  mb-2nd-p-i                      PIC S9(5)V9(2) COMP-3.
           05  mb-2nd-annual-int               PIC S9(0)V9(7) COMP-3.
           05  mb-bill-addr-foreign            PIC X(1).
           05  mb-dont-process                 PIC X(1).
           05  mb-pif-stop                     PIC X(1).
           05  mb-foreclosure-stop             PIC X(1).
           05  mb-bad-ck-stop                  PIC X(1).
           05  mb-no-notices                   PIC X(1).
           05  mb-dont-analyze                 PIC X(1).
           05  mb-a-h-flag                     PIC X(1).
           05  mb-life-flag                    PIC X(1).
           05  mb-disb-stop                    PIC X(1).
           05  mb-arm-plan-id                  PIC X(4).
           05  mb-arm-ir-yy                    PIC S9(3) COMP-3.
           05  mb-arm-ir-mm                    PIC S9(2).
           05  mb-arm-ir-da                    PIC S9(2).
           05  mb-arm-pi-chg-yy                PIC S9(3) COMP-3.
           05  mb-arm-pi-chg-mm                PIC S9(2).
           05  mb-arm-pi-chg-dd                PIC S9(2).
           05  mb-modification-program         PIC X(4).
      *    05  POS739-FOR-11                   PIC X(11).
           05  FILLER                          PIC X(11).
           05  2ND-DELQ-P-I                    PIC S9(11)V99 COMP-3.
           05  OPT-OUT-SOLICIT-STOP            PIC X(1).
           05  mb-modification-type            PIC X(1).
           05  TPV-DRAFT-VAL-IND               PIC X(1).
           05  TPV-DRAFT-STATUS-CD             PIC X(1).
           05  mb-payment-option-switch        PIC X(1).
           05  mb-payment-opt-4                PIC S9(9)V9(2) COMP-3.
           05  mb-payment-opt-4-totdue         PIC S9(9)V9(2) COMP-3.
           05  mb-dist-type-1-int-only-flag    PIC X(1).
           05  mb-corp-adv-bal                 PIC S9(7)V9(2) COMP-3.
           05  mb-pls-cli-id                   PIC X(3).
           05  MTGR-LANG-PREF                  PIC X(2).
           05  mb-modification-status-yy       PIC S9(3) COMP-3.
           05  mb-modification-status-mmdd     PIC S9(4).
           05  AEGIS-FIRST-PAYMENT-YR          pic s9(3) comp-3.
           05  AEGIS-FIRST-PAYMENT-NO          pic x(2).
           05  AEGIS-FIRST-PAYMENT-DA          pic x(2).
           05  AEGIS-EVENT-CODE                PIC X(1).
      *    05  POS798-FOR-8                    PIC X(8).
           05  FILLER                          PIC X(8).
           05  mb-deferred-balance-indicator   PIC X(1).
      *    05  POS807-FOR-15                   PIC X(15).
           05  FILLER                          PIC X(15).
           05  mb-post-petition-yy             PIC S9(3) COMP-3.
           05  mb-post-petition-mmdd           PIC S9(4).
           05  mb-post-petition-amount         PIC S9(9)V9(2) COMP-3.
           05  mb-modification-status          PIC X(2).
           05  mb-modification-yy              PIC S9(3) COMP-3.
           05  mb-modification-mmdd            PIC S9(4).
           05  mb-payment-option-indicator     PIC X(1).
           05  mb-original-loan-amount         PIC S9(9) COMP-3.
           05  mb-neg-am-pb-cap-value          PIC S9(11)V9(2) COMP-3.
           05  mb-interest-only-expiration-yy  PIC S9(3) COMP-3.
           05  mb-interest-only-expiration-mmdd PIC S9(4).
           05  mb-monthly-borrower-accrual-amt  PIC S9(5)V9(2) COMP-3.     
           05  mb-borrower-incentive-anniversary-yy  PIC S9(3) COMP-3.
           05  mb-borrower-incentive-anniversary-mmdd  PIC S9(4).
           05  mb-borrower-incentive-amount-accrued-anniversary-to-date
                                                 PIC S9(5)V9(2) COMP-3.
           05  mb-trial-modification-start-yy    PIC S9(3) COMP-3.
           05  mb-trial-modification-start-mmdd  PIC S9(4).
           05  mb-trial-modification-end-yy    PIC S9(3) COMP-3.
           05  mb-trial-modification-end-mmdd  PIC S9(4).
           05  mb-trial-modification-payment   PIC S9(9)V9(2) COMP-3.
           05  POS893-FOR-1                    PIC X(1).
           05  mb-interest-only-pi             PIC S9(9)V9(2) COMP-3.
           05  mb-fully-am-pi                  PIC S9(9)V9(2) COMP-3.
           05  mb-interest-only-totdue         PIC S9(9)V9(2) COMP-3.
           05  mb-fully-am-totdue              PIC S9(9)V9(2) COMP-3.
      *** this section client specific (cli 277) - not documented by LPS
           05  mb-int-due                      PIC S9(9)V9(2) COMP-3.
           05  mb-scra-f                       PIC X(1).
           05  mb-scra-date-3                  PIC X(6).
           05  mb-scra-date-4                  PIC X(6).
           05  mb-scra-billing-interest        PIC S9(2)V9(5) COMP-3.
           05  mb-140i                         PIC X(7).
           05  mb-140j                         PIC S9(7) COMP-3.
           05  mb-140k                         PIC X(49).
           05  mb-hamp-incentive-code          PIC X(1).
      *** client specifics end here
           05  FILLER                          PIC X(557).
           05  mb-modification-reason-code     PIC X(5).
           05  mb-modification-effective-yy    PIC S9(3) COMP-3.
           05  mb-modification-effective-mmdd  PIC X(4).
           05  mb-2-to-1-indicator             PIC X(1).
           05  BIA-PD-LIFE                     PIC S9(5)V99 COMP-3.
           05  mb-off-schd-pend-date-1-yy      PIC S9(3) COMP-3.
           05  mb-off-schd-pend-date-1-mmdd    PIC X(4).
           05  mb-off-schd-pend-ir-1           PIC S9(2)V9(5) COMP-3.
           05  mb-off-schd-pend-pi-1           PIC S9(11)V9(2) COMP-3.
           05  mb-off-schd-pend-date-2-yy      PIC S9(3) COMP-3.
           05  mb-off-schd-pend-date-2-mmdd    PIC X(4).
           05  mb-off-schd-pend-ir-2           PIC S9(2)V9(5) COMP-3.
           05  mb-off-schd-pend-pi-2           PIC S9(11)V9(2) COMP-3.
           05  mb-off-schd-pend-date-3-yy      PIC S9(3) COMP-3.
           05  mb-off-schd-pend-date-3-mmdd    PIC X(4).
           05  mb-off-schd-pend-ir-3           PIC S9(2)V9(5) COMP-3.
           05  mb-off-schd-pend-pi-3           PIC S9(11)V9(2) COMP-3.
           05  mb-prin-reduct-amt              PIC S9(7)V9(2) COMP-3.
           05  mb-prin-fb-amt                  PIC S9(7)V9(2) COMP-3.
           05  mb-master-poc-name              PIC X(20).
           05  mb-master-poc-phone             PIC X(12).
           05  mb-master-poc-ext               PIC X(4).
           05  mb-bankruptcy-poc-name          PIC X(20).
           05  mb-bankruptcy-poc-phone         PIC X(12).
           05  mb-bankruptcy-poc-ext           PIC X(4).
           05  mb-foreclosure-poc-name         PIC X(20).
           05  mb-foreclosure-poc-phone        PIC X(12).
           05  mb-foreclosure-poc-ext          PIC X(4).
           05  mb-loss-mitigation-poc-name     PIC X(20).
           05  mb-loss-mitigation-poc-phone    PIC X(12).
           05  mb-loss-mitigation-poc-ext      PIC X(4).
           05  mb-mgmt-office-poc-name         PIC X(20).
           05  mb-mgmt-office-poc-phone        PIC X(12).
           05  mb-mgmt-office-poc-ext          PIC X(4).
           05  mb-service-relief-poc-name      PIC X(20).
           05  mb-service-relief-poc-phone     PIC X(12).
           05  mb-service-relief-poc-ext       PIC X(4).
           05  mb-user-defined-poc-name        PIC X(20).
           05  mb-user-defined-poc-phone       PIC X(12).
           05  mb-user-defined-poc-ext         PIC X(4).
           05  mb-mil-svc-status               PIC X(1).
           05  mb-prot-begin-due-yy            PIC S9(3) COMP-3.
           05  mb-prot-begin-due-mm            PIC X(2).
           05  mb-prot-begin-due-dd            PIC X(2).
           05  mb-prot-end-due-yy              PIC S9(3) COMP-3.
           05  mb-prot-end-due-mm              PIC X(2).
           05  mb-prot-end-due-dd              PIC X(2).
           05  MS-RATE                         PIC S9(2)V9(5) COMP-3.
           05  CURR-1ST-PRIN-DUE-AMT           PIC S9(9)V9(2) COMP-3.
           05  CURR-2ND-PRIN-DUE-AMT           PIC S9(9)V9(2) COMP-3.
           05  CURR-1ST-INT-DUE-AMT            PIC S9(9)V9(2) COMP-3.
           05  CURR-2ND-INT-DUE-AMT            PIC S9(9)V9(2) COMP-3.
           05  CURR-PAYMENT-OPT-4-PRIN-DUE-AMT  PIC S9(9)V9(2) COMP-3.
           05  CURR-PAYMENT-OPT-4-INT-DUE-AMT  PIC S9(9)V9(2) COMP-3.
           05  CURR-FULLY-AM-PRIN-DUE-AMT      PIC S9(9)V9(2) COMP-3.
           05  CURR-FULLY-AM-INT-DUE-AMT       PIC S9(9)V9(2) COMP-3.
           05  PREPAY-PEN-INDIC                PIC X(1).
           05  PPP-HDR-CODE                    PIC X(3).
           05  PPP-PAYOFF-EXPIRE-YR            PIC S9(3) COMP-3.
           05  PPP-PAYOFF-EXPIRE-MO            PIC S9(2).
           05  PPP-PAYOFF-EXPIRE-DA            PIC S9(2).
           05  2ND-PREPAY-PEN-INDIC            PIC X(1).
           05  PPP-PAYOFF-CURTAILMENT-YR       PIC S9(3) COMP-3.
           05  PPP-PAYOFF-CURTAILMENT-MO       PIC S9(2).
           05  PPP-PAYOFF-CURTAILMENT-DA       PIC S9(2).
           05  DRAFT-INDICATOR                 PIC X(1).
           05  BILL-DUE-DATE-YR                PIC S9(3) COMP-3.
           05  BILL-DUE-DATE-MO                PIC S9(2).
           05  BILL-DUE-DATE-DA                PIC S9(2).
           05  LATE-CHG-ASSESS-DATE-YR         PIC S9(3) COMP-3.
           05  LATE-CHG-ASSESS-DATE-MO         PIC S9(2).
           05  LATE-CHG-ASSESS-DATE-DA         PIC S9(2).
           05  DAYS-DELQ-NO                    PIC S9(5) COMP-3.
           05  PAYMENTS-DUE-NO                 PIC S9(3).
           05  BNK1-DEBTOR-SUSP-BAL            PIC S9(9)V9(2) COMP-3.
           05  BNK1-TRUST-SUSP-BAL             PIC S9(9)V9(2) COMP-3.
           05  BNK1-POST-1-SUSP-BAL            PIC S9(9)V9(2) COMP-3.
           05  BNK1-POST-2-SUSP-BAL            PIC S9(9)V9(2) COMP-3.
           05  BNK1-POST-PET-SUSP-BAL          PIC S9(9)V9(2) COMP-3.
           05  PRE-PET-PLAN-DUE-DATE-YR        PIC S9(3) COMP-3.
           05  PRE-PET-PLAN-DUE-DATE-MO        PIC S9(2).
           05  PRE-PET-PLAN-DUE-DATE-DA        PIC S9(2).
           05  PRE-PET-PLAN-PMT-AMT            PIC S9(9)V9(2) COMP-3.
           05  PRE-PET-CLAIM-AMT               PIC S9(9)V9(2) COMP-3.
           05  PRE-PET-CONFIRMED-DATE-YR       PIC S9(3) COMP-3.
           05  PRE-PET-CONFIRMED-DATE-MO       PIC S9(2).
           05  PRE-PET-CONFIRMED-DATE-DA       PIC S9(2).
           05  POST1-PLAN-DUE-DATE-YR          PIC S9(3) COMP-3.
           05  POST1-PLAN-DUE-DATE-MO          PIC S9(2).
           05  POST1-PLAN-DUE-DATE-DA          PIC S9(2).
           05  POST1-PLAN-PMT-AMT              PIC S9(9)V9(2) COMP-3.
           05  POST2-PLAN-DUE-DATE-YR          PIC S9(3) COMP-3.
           05  POST2-PLAN-DUE-DATE-MO          PIC S9(2).
           05  POST2-PLAN-DUE-DATE-DA          PIC S9(2).
           05  POST2-PLAN-PMT-AMT              PIC S9(9)V9(2) COMP-3.
           05  LOSS-MIT-IND-CD                 PIC X(1).
           05  LOSS-MIT-STATUS-CD              PIC X(1).
           05  MASTER-LOSS-MIT-STATUS-CODE     PIC X(1).
           05  LOSS-MIT-APPRVD-DENIED-DATE-YR  PIC S9(3) COMP-3.
           05  LOSS-MIT-APPRVD-DENIED-DATE-MO  PIC S9(2).
           05  LOSS-MIT-APPRVD-DENIED-DATE-DA  PIC S9(2).
           05  STEP-FLAG-CD                    PIC X(1).
           05  LOSS-MIT-TEMPLATE-CD            PIC X(7).
           05  LOSS-MIT-TYPE                   PIC X(5).
           05  FOR1-STATUS-CD                  PIC X(1).
           05  FOR1-TEMPLATE-ID-CD             PIC X(7).
           05  FIRST-LEGAL-DATE-YR             PIC S9(3) COMP-3.
           05  FIRST-LEGAL-DATE-MO             PIC S9(2).
           05  FIRST-LEGAL-DATE-DA             PIC S9(2).
           05  FOR1-SALE-DATE-YR               PIC S9(3) COMP-3.
           05  FOR1-SALE-DATE-MO               PIC S9(2).
           05  FOR1-SALE-DATE-DA               PIC S9(2).
           05  REPAY-PLAN-TYPE                 PIC X(2).
           05  REPAY-PLAN-STATUS-CD            PIC X(1).
           05  REPAY-PLAN-STATUS-CD-DATE-YR    PIC S9(3) COMP-3.
           05  REPAY-PLAN-STATUS-CD-DATE-MO    PIC S9(2).
           05  REPAY-PLAN-STATUS-CD-DATE-DA    PIC S9(2).
           05  REPAY-PLAN-START-DATE-YR        PIC S9(3) COMP-3.
           05  REPAY-PLAN-START-DATE-MO        PIC S9(2).
           05  REPAY-PLAN-START-DATE-DA        PIC S9(2).
           05  REPAY-PLAN-END-DATE-YR          PIC S9(3) COMP-3.
           05  REPAY-PLAN-END-DATE-MO          PIC S9(2).
           05  REPAY-PLAN-END-DATE-DA          PIC S9(2).
           05  REPAY-PLAN-NXT-DUE-DATE-YR      PIC S9(3) COMP-3.
           05  REPAY-PLAN-NXT-DUE-DATE-MO      PIC S9(2).
           05  REPAY-PLAN-NXT-DUE-DATE-DA      PIC S9(2).
           05  REPAY-PLAN-NXT-DUE-AMT          PIC S9(9)V9(2) COMP-3.
           05  PAYMENTS-INSIDE-PLAN-CODE       PIC X(1).
           05  BANKRUPTCY-STATUS               PIC X(1).
           05  PRIN-BAL-LT-PI-IND              PIC X.
           05  ARM-RATE-PI-NOT-AVAIL-IND       PIC X.
           05  POST-PET-DLQ-DAYS               PIC 9(5) COMP-3.
           05  BNK-DISCHARGE-IND               PIC X.
           05  BNK-DISCHARGE-DATE.
               10  BNK-DISCHARGE-YR            PIC 9(3) COMP-3.
               10  BNK-DISCHARGE-MO            PIC XX.
               10  BNK-DISCHARGE-DA            PIC XX.
      *    05  POS2156-FOR-6                   PIC X(6).
           05  FILLER                          PIC X(6).
           05  PRE-PET-TOTAL-CLAIM-AMT         PIC S9(9)V99 COMP-3.
           05  PRE-PET-PAID-TO-DATE            PIC S9(9)V99 COMP-3.
           05  PRE-PET-REMAINING-BAL           PIC S9(9)V99 COMP-3.
           05  NO-CHANGE-SCHED-IND             PIC X.
           05  BIWEEKLY-PYMNT-TABLE OCCURS 2 TIMES.
               10  BIWK-DUE-DATE.
                   15  BIWK-DUE-YR             PIC 9(3) COMP-3.
                   15  BIWK-DUE-MO             PIC XX.
                   15  BIWK-DUE-DA             PIC XX.
               10  BIWK-TOT-PAYMT-AMT          PIC S9(9)V99 COMP-3.
               10  BIWK-PRIN-DUE-AMT           PIC S9(9)V99 COMP-3.
               10  BIWK-INT-DUE-AMT            PIC S9(9)V99 COMP-3.
               10  BIWK-ESCROW-MTH             PIC S9(7)V99 COMP-3.
               10  BIWK-A-H-PREM               PIC S9(5)V99 COMP-3.
               10  BIWK-LIFE-PREM              PIC S9(5)V99 COMP-3.
               10  BIWK-REP-RES                PIC S9(7)V99 COMP-3.
               10  BIWK-HUD-PART               PIC S9(7)V99 COMP-3.
               10  BIWK-L-C-AMT                PIC S9(7)V99 COMP-3.
               10  BIWK-MISC-AMT               PIC S9(7)V99 COMP-3.
           05  MAT-DATE-LT-BILL-DUE-DATE       PIC X.
           05  BILL-DUE-DATE-MISC-CODE         PIC X.
           05  BORR-EMAIL-ADDRESS              PIC X(66).
           05  CFPB-NEXT-DRAFT-DATE.
               10  CFPB-NEXT-DRAFT-DATE-YR     PIC 9(3) COMP-3.
               10  CFPB-NEXT-DRAFT-DATE-MO     PIC XX.
               10  CFPB-NEXT-DRAFT-DATE-DA     PIC XX.
           05  CFPB-NEXT-DRAFT-AMT             PIC S9(9)V99 COMP-3.
           05  TPV-DRAFT-ACCT-BAL              PIC S9(6)V99 COMP-3.
           05  TPV-DRAFT-BAL-YR                PIC S9(3) COMP-3.
           05  TPV-DRAFT-BAL-MO                PIC X(2).
           05  TPV-DRAFT-BAL-DA                PIC X(2).
           05  TPV-DRAFT-LAST-WDRL-YR          PIC S9(3) COMP-3.
           05  TPV-DRAFT-LAST-WDRL-MO          PIC X(2).
           05  TPV-DRAFT-LAST-WDRL-DA          PIC X(2).
           05  TPV-DRAFT-NEXT-WDRL-YR          PIC S9(3) COMP-3.
           05  TPV-DRAFT-NEXT-WDRL-MO          PIC X(2).
           05  TPV-DRAFT-NEXT-WDRL-DA          PIC X(2).
           05  TPV-DRAFT-LAST-REC-SER-FEE      PIC S9(2)V99 COMP-3.
           05  CFPB-PREV-POSTED                PIC S9(7)V99 COMP-3.
           05  CFPB-PREV-POSTED-ESCROW         PIC S9(9)V99 COMP-3.
           05  EBPP-CODE                       PIC X(2).
           05  CFPB-ACQ-DATE.
               10  CFPB-ACQ-DATE-YR            PIC 9(3) COMP-3.
               10  CFPB-ACQ-DATE-MO            PIC XX.
               10  CFPB-ACQ-DATE-DA            PIC XX.
           05  NEXT-IR-CHG-DATE.
               10  NEXT-IR-CHG-DATE-YR         PIC 9(3) COMP-3.
               10  NEXT-IR-CHG-DATE-MO         PIC XX.
               10  NEXT-IR-CHG-DATE-DA         PIC XX.
           05  BILL-DUE-DATE-IR                PIC SV9(7) COMP-3.
           05  EFF-NEXT-IR-CHG-DATE.
               10  EFF-NEXT-IR-CHG-DATE-YR     PIC 9(3) COMP-3.
               10  EFF-NEXT-IR-CHG-DATE-MO     PIC XX.
               10  EFF-NEXT-IR-CHG-DATE-DA     PIC XX.
           05  EFF-OFF-SCHD-PEND-DATE-1.
               10  EFF-OFF-SCHD-PEND-DATE-1-YR PIC 9(3) COMP-3.
               10  EFF-OFF-SCHD-PEND-DATE-1-MO PIC XX.
               10  EFF-OFF-SCHD-PEND-DATE-1-DA PIC XX.
           05  NU-PROP-UNIT-NO                 PIC X(12).
           05  E-CONSENT-CODE                  PIC X.
           05  CEASE-DESIST-STOP               PIC X.
           05  CO-BORR-EMAIL-ADDRESS           PIC X(66).
           05  CHARGE-OFF-BALANCE              PIC S9(9)V99 COMP-3.
           05  CHARGE-OFF-DATE.
               10 CHARGE-OFF-YR                PIC 9(3) COMP-3.
               10 CHARGE-OFF-MO                PIC XX.
               10 CHARGE-OFF-DA                PIC XX.
           05  LITIGATION-STATUS-CD            PIC X(2).
           05  ACCEL-DATE.
               10 ACCEL-YR                     PIC 9(3) COMP-3.
               10 ACCEL-MO                     PIC XX.
               10 ACCEL-DA                     PIC XX.
           05  ACCEL-AMOUNT                    PIC S9(9)V99 COMP-3.
           05  ACCEL-REASON-CD                 PIC XX.
           05  ACCEL-INTERST-DUE               PIC S9(9)V99 COMP-3.
           05  OPT-INS-DATA OCCURS 12 TIMES.
               10 PLAN-ID.
                  15 PLAN-TYPE                 PIC X.
                  15 PLAN-IND                  PIC X.
               10 OPT-INS-LINE-1               PIC X(24).
               10 OPT-INS-PHONE                PIC X(10).
           05  PREV-DRAFT-AMT                  PIC S9(7)V99 COMP-3.
           05  DRAFT-AMT-CHANGED-IND           PIC X.
           05  ACCEL-INT-DUE-CALC-AMT          PIC S9(9)V99 COMP-3.
           05  ACCEL-INT-DUE-CALC-IND          PIC X.
           05  FILLER                          PIC X(702).
           05  ACCEL-ALLOW-REIN-IND            PIC X.
           05  FILLER                          PIC X(12).
           05  EXPIRE-DT.
               10 EXPIRE-DT-YR                 PIC 9(3) COMP-3.
               10 EXPIRE-DT-MO                 PIC XX.
               10 EXPIRE-DT-DA                 PIC XX.
           05  DISASTER-ID                     PIC X(20).
           05  IMPACT-SEVERITY                 PIC X.
           05  DISASTER-RELIEF                 PIC X.
           05  FEMA-ASSIST                     PIC X.
           05  NON-FEMA-ASSIST                 PIC X.
           05  BIF-RSA-ADV-CODE                PIC XX.
           05  BIF-AS-OF-DATE.
               10 BIF-AS-OF-DATE-YR            PIC 9(3) COMP-3.
               10 BIF-AS-OF-DATE-MO            PIC XX.
               10 BIF-AS-OF-DATE-DA            PIC XX.
           05  BIF-1ST-INTEREST                PIC S9(9)V99 COMP-3.
           05  BIF-2ND-INTEREST                PIC S9(9)V99 COMP-3.
           05  BIF-PAYOFF-FEES                 PIC S9(9)V99 COMP-3.
           05  BIF-TOTAL-AMOUNT-DUE            PIC S9(9)V99 COMP-3.
           05  FILLER                          PIC X.
           05  PMTDEF-PRIN-BAL                 PIC S9(9)V99 COMP-3.
           05  PMTDEF-INT-BAL                  PIC S9(9)V99 COMP-3.
           05  PMTDEF-FEE-BAL                  PIC S9(9)V99 COMP-3.
           05  PMTDEF-PREVINV-PRIN-BAL         PIC S9(9)V99 COMP-3.
           05  PMTDEF-PREVINV-INT-BAL          PIC S9(9)V99 COMP-3.
           05  PMTDEF-CORP-ADV-BAL             PIC S9(9)V99 COMP-3.
           05  PMTDEF-ESC-ADV-BAL              PIC S9(9)V99 COMP-3.
           05  PMTDEF-TOTAL                    PIC S9(11)V99 COMP-3.
           05  DLQ-ACQ-IND                     PIC X.
           05  TOTAL-DEBT-AMT                  PIC S9(13)V99 COMP-3.
           05  CURRENT-CREDITOR-NAME           PIC X(45).
           05  SHORT-ESCROW-PMT                PIC X(1).
           05  PSA-BEGIN-DATE.
               10 PSA-BEGIN-DATE-YR            PIC 9(3) COMP-3.
               10 PSA-BEGIN-DATE-MO            PIC XX.
               10 PSA-BEGIN-DATE-DA            PIC XX.
           05  PSA-END-DATE.
               10 PSA-END-DATE-YR              PIC 9(3) COMP-3.
               10 PSA-END-DATE-MO              PIC XX.
               10 PSA-END-DATE-DA              PIC XX.
           05  PSA-CURRENT-BAL                 PIC S9(7)V99 COMP-3.
           05  PSA-TERM-DATE.
               10 PSA-TERM-DATE-YR             PIC 9(3) COMP-3.
               10 PSA-TERM-DATE-MO             PIC XX.
               10 PSA-TERM-DATE-DA             PIC XX.
           05  PSA-TERM-REFUND-AMT             PIC S9(7)V99 COMP-3.
           05  REPAY-PLAN-IND                  PIC X(1). 
           05  FILLER                          PIC X(89).
