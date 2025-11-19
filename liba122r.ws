
       01 LIBA122R-PANEL-WS                   PIC X(0500) VALUE
               "LIBA122R            0130".

       01 LIBA122R-WS.
           03 LCUENTA                      PIC 9(09)               .
           03 LDESDENRO                    PIC 9(08)               .
           03 LFECHA1                     .
               05 YYYY            PIC 9(4).
               05 MM              PIC 9(2).
               05 DD              PIC 9(2).
           03 LFECHA2                     .
               05 YYYY            PIC 9(4).
               05 MM              PIC 9(2).
               05 DD              PIC 9(2).
           03 LHASTANRO                    PIC 9(08)               .
           03 LNOMCUENTA                   PIC X(0030).
           03 LPERIODO                     PIC 9(01)               .
           03 LTM                          PIC X(0001).
           03 RMP--PROMPT                  PIC X(0057).
