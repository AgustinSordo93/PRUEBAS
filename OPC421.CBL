       IDENTIFICATION DIVISION.
       PROGRAM-ID.                     OPC421.
      ********** EXPORTACION AL SIAP IIBB ************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PS-IBM.
       OBJECT-COMPUTER. PS-IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CAJA.SEL.
           COPY CAJA1.SEL.
           COPY CAJA2.SEL.
           COPY EFE.SEL.
           COPY NUMECA.SEL.

           COPY CHE.SEL.
           COPY TABLA.SEL.

           COPY RETEGAN1.SEL.
           COPY RETEGAN2.SEL.
           COPY RETEGAN3.SEL.

           COPY RETEIIBB.SEL.
           COPY RETEAGIP.SEL.
           COPY RETECDBA.SEL.
           COPY RETESUSS.SEL.
           COPY RETEIVA.SEL.

           COPY BANCO.SEL.

           COPY CONTA.SEL.

           COPY INGRESO2.SEL.
           COPY VENTAS2.SEL.
           COPY EXPO2.SEL.
           COPY CPA2.SEL.
           COPY CPACSP2.SEL.
           COPY HACI2.SEL.

           COPY \USER\FRIGO\MENU\DATOS.SEL.
           COPY \USER\FRIGO\MENU\CLAVES.SEL.

           SELECT RETETXT  ASSIGN TO RANDOM ARCHI-RETETXT
                           ORGANIZATION IS LINE SEQUENTIAL
                           STATUS IS ESTATUS.

           SELECT TXT      ASSIGN TO RANDOM ARCHI-TXT
                           ORGANIZATION IS LINE SEQUENTIAL
                           STATUS IS ESTATUS.

           SELECT RETECSV  ASSIGN TO RANDOM ARCHI-RETECSV
                           ORGANIZATION IS LINE SEQUENTIAL
                           STATUS IS ESTATUS.

      ********
       DATA DIVISION.
       FILE SECTION.

           COPY CAJA.COP.
           COPY CAJA1.COP.
           COPY CAJA2.COP.
           COPY EFE.COP.
           COPY NUMECA.COP.

           COPY CHE.COP.
           COPY TABLA.COP.

           COPY RETEGAN1.COP.
           COPY RETEGAN2.COP.
           COPY RETEGAN3.COP.

           COPY RETEIIBB.COP.
           COPY RETEAGIP.COP.
           COPY RETECDBA.COP.
           COPY RETESUSS.COP.
           COPY RETEIVA.COP.

           COPY BANCO.COP.

           COPY CONTA.COP.

           COPY INGRESO2.COP.
           COPY VENTAS2.COP.
           COPY EXPO2.COP.
           COPY CPA2.COP.
           COPY CPACSP2.COP.
           COPY HACI2.COP.

           COPY \USER\FRIGO\MENU\DATOS.COP.
           COPY \USER\FRIGO\MENU\CLAVES.COP.

       FD  TXT         LABEL  RECORD STANDARD.
       01  REG-TXT     PIC X(185).

      ***************************************************************
       FD  RETETXT         LABEL  RECORD STANDARD.
       01  REG-RETETXT.
           03 CUIT-RETETXT        PIC X(11).
           03 SUCU-RETETXT        PIC X(5).
           03 FECHAO-RETETXT      PIC X(10).
           03 ALI-RETETXT         PIC 99.99.
           03 BIMP-RETETXT        PIC X(16).

      ***************************************************************
       FD  RETECSV         LABEL  RECORD STANDARD.
       01  REG-RETECSV.
           03 TRAZO-RETECSV       PIC X(1024).

      ***************************************************************
       WORKING-STORAGE SECTION.

       01  IND-CSV              PIC 9(9)      VALUE ZEROES.
       01  ARCHI-RETECSV        PIC X(30)     VALUE SPACES.

       01  RUTA-TEMP          PIC X(150)    VALUE SPACES.
       01  ARCHI-TXT-TEMP     PIC X(50)     VALUE SPACES.
       01  ARCHI-ZIP          PIC X(50)     VALUE SPACES.

      ***************************************************************
      ***** VAR-AUX.WOR                                         *****
      ***** COPY DE VARIABLES AUXILIARES COMUNES A TODOS        *****
      ***************************************************************

           COPY VAR-AUX.WOR.

      ***************************************************************
      ***** VAR-RETE.WOR                                        *****
      ***** COPY DE VARIABLES PARA EL CALCULO DE RETENCIONES    *****
      ***************************************************************

           COPY VAR-RETE.WOR.

      ***************************************************************
      ******* VARIABLES PARA RETENCIONES DE CAPITAL *****************
      ***************************************************************
       01  ARCHI-TXT            PIC X(80)     VALUE SPACES.
       01  NRO16                PIC 9(16)     VALUE ZEROES.
       01  IIBB-RETE            PIC X(10)     VALUE SPACES.
       01  CNTR-CAPFED          PIC 9(2)      VALUE ZEROES.
       01  TIPO-IVA             PIC X         VALUE SPACES.
       01  TIPO-CUIT            PIC X         VALUE SPACES.
       01  TIPO-RETE            PIC X         VALUE SPACES.
       01  NOM-RETE             PIC X(30)     VALUE SPACES.
       01  SUJ-RETE             PIC 9(9)V99   VALUE ZEROES.
       01  POR-RETE             PIC 9(9)V99   VALUE ZEROES.
       01  RET-RETE             PIC 9(9)V99   VALUE ZEROES.
       01  IMP-IVA              PIC 9(7)V99   VALUE ZEROES.
       01  CORTO-IB             PIC X         VALUE SPACES.
       
       01  AUX-14-N      PIC 9(13)V99     COMP-3 VALUE 0.
       01  AUX-14-OUT    PIC 9(13).99.

       01  M2V99              PIC ZZ.ZZ.
       01  M7V99              PIC ZZZZZZZ.ZZ.
       01  M9V99              PIC ZZZZZZZZZ.ZZ.

       01  M-CUIT.
           03 M1-CUIT         PIC 99       VALUE ZEROES.
           03 FILLER          PIC X        VALUE SPACES.
           03 M2-CUIT         PIC 9(8)     VALUE ZEROES.
           03 FILLER          PIC X        VALUE SPACES.
           03 M3-CUIT         PIC 9        VALUE ZEROES.

      *************************************************************

       01  ARCHI-RETETXT        PIC X(80)     VALUE SPACES.
       01  NRO-RETE-AUX         PIC 9(8)      VALUE ZEROES.

       01  IMPORTE-TOTAL        PIC S9(12)V99 VALUE ZEROES.
       01  IMPORTE-SUJETO       PIC S9(12)V99 VALUE ZEROES.

       01  AUX-16               PIC ZZZZZZZZZZZZZ.ZZ VALUE ZEROES.
       01  AUX-14               PIC 9999999999999.99 VALUE ZEROES.
       01  AUX-15               PIC 9(13)V99         VALUE ZEROES.
       01  REGIMEN-AUX          PIC XXX              VALUE SPACES.
       01  AUX-6                PIC 999999           VALUE ZEROES.

       01  CUIT-A.
           03 CUIT-A1         PIC XX   VALUE SPACES.
           03 CUIT-A2         PIC X    VALUE SPACES.
           03 CUIT-A3         PIC X(8) VALUE SPACES.
           03 CUIT-A4         PIC X    VALUE SPACES.
           03 CUIT-A5         PIC X    VALUE SPACES.
       01  CUIT-B.
           03 CUIT-B1         PIC XX   VALUE SPACES.
           03 CUIT-B3         PIC X(8) VALUE SPACES.
           03 CUIT-B5         PIC X    VALUE SPACES.

       01  CCFIN                PIC 9     VALUE ZEROES.
       01  CDESDE               PIC X(30) VALUE SPACES.

       01  REGISTRO-CAP.
           03 TRC1   PIC 9     VALUE ZEROES.

           03 TRC2   PIC 999.

           03 TRC3   PIC 99    VALUE ZEROES.
           03 TRC4   PIC X     VALUE SPACES.
           03 TRC5   PIC 99    VALUE ZEROES.
           03 TRC6   PIC X     VALUE SPACES.
           03 TRC7   PIC 9999  VALUE ZEROES.

           03 TRC8   PIC 99.

           03 TRC9   PIC X     VALUE SPACES.

           03 TRC10  PIC X(15) VALUE SPACES.

           03 TRC11  PIC 99    VALUE ZEROES.
           03 TRC12  PIC X     VALUE SPACES.
           03 TRC13  PIC 99    VALUE ZEROES.
           03 TRC14  PIC X     VALUE SPACES.
           03 TRC15  PIC 9999  VALUE ZEROES.

           03 TRC16  PIC X(12) VALUE SPACES.

           03 TRC17  PIC X(16) VALUE SPACES.

           03 TRC18  PIC X     VALUE SPACES.

           03 TRC19  PIC X(11) VALUE SPACES.

           03 TRC20  PIC 9     VALUE ZEROES.

           03 TRC21  PIC X(10).

           03 TRC22  PIC 9     VALUE ZEROES.

           03 TRC23  PIC X(30) VALUE SPACES.

           03 TRC24  PIC X(10) VALUE SPACES.

           03 TRC25  PIC X(10) VALUE SPACES.

           03 TRC26  PIC X(12) VALUE SPACES.

           03 TRC27  PIC X(5)  VALUE SPACES.

           03 TRC28  PIC X(12) VALUE SPACES.

           03 TRC29  PIC X(12) VALUE SPACES.

           COPY \USER\FRIGO\MENU\LISTA.WOR.
           COPY VAR.WOR.

           COPY \USER\FRIGO\MENU\TECLADO.WOR.
           COPY \USER\FRIGO\MENU\GETENV.WOR.

           COPY \USER\FRIGO\MENU\RMPANELS.WS.

           COPY LIBA122R.WS.

           COPY CUENTA.WS.

           COPY PANTA.WS.
           COPY PANTA1.WS.
           COPY PANTA2.WS.
           COPY PANTA3.WS.
           COPY PANTA4.WS.
           COPY CONSULTA.WS.
           COPY DENEGADO.WS.
           COPY FONDO.WS.
           COPY FORMATO.WS.
           COPY CORRELIS.WS.
           COPY MENSAJE.WS.

       LINKAGE SECTION.
       01  USUARIO     PIC X(10).
       01  CLAVE       PIC X(6).
       01  EKEY1       PIC 9(5).
       01  EKEY2       PIC 9(5).
       01  EKEY3       PIC 9(5).

      *****************************************************************
       PROCEDURE DIVISION USING USUARIO CLAVE EKEY1 EKEY2 EKEY3.
       DECLARATIVES.
      *****************************************************************

           COPY CAJA.DEC.
           COPY CAJA1.DEC.
           COPY CAJA2.DEC.
           COPY EFE.DEC.
           COPY NUMECA.DEC.

           COPY CHE.DEC.
           COPY TABLA.DEC.

           COPY RETEGAN1.DEC.
           COPY RETEGAN2.DEC.
           COPY RETEGAN3.DEC.

           COPY RETEIIBB.DEC.
           COPY RETEAGIP.DEC.
           COPY RETECDBA.DEC.
           COPY RETESUSS.DEC.
           COPY RETEIVA.DEC.

           COPY BANCO.DEC.

           COPY RETETXT.DEC.

           COPY CONTA.DEC.

           COPY INGRESO2.DEC.
           COPY VENTAS2.DEC.
           COPY EXPO2.DEC.
           COPY CPA2.DEC.
           COPY CPACSP2.DEC.
           COPY HACI2.DEC.

           COPY \USER\FRIGO\MENU\DATOS.DEC.
           COPY \USER\FRIGO\MENU\CLAVES.DEC.

      *****************
       END DECLARATIVES.
      *****************
       PIDO-FECHA SECTION 30.
       MUEVOFECHA.
           MOVE EKEY1 TO YEK1. 
              MOVE EKEY2 TO YEK2.
              MOVE EKEY3 TO YEK3. 
              MOVE "* S.G.Sistemas *" TO YEK4.

           ACCEPT FECHA-2000 FROM DATE.
           MOVE DD-2000 TO DD-1.
           MOVE MM-2000 TO MM-1.
           MOVE AA-2000 TO AA-1.
           IF AA-2000 > 80 ADD 1900 TO AA-1
                           ELSE
                           ADD 2000 TO AA-1.
           PERFORM CARGOMENU.
       CUERPO.
           PERFORM ABRO-ARCHIVO.
           PERFORM PROCESO THRU F-PROCESO.
           PERFORM RMP--DP-FONDO.
           PERFORM CIERRO-ARCHIVO.
       F-CUERPO.
           GOBACK.
      ************************************************************
       PROCESO.
           MOVE ZEROES TO CTRLREPE1.
           PERFORM ARMO-FORMATO.
       Q1.
           PERFORM RMP--DP-LIBA122R.
           PERFORM RMP--IAF-LIBA122R.

           MOVE FECHA-1 TO LFECHA2.
           MOVE SPACES  TO LTM.
           PERFORM RMP--DAF-LIBA122R.
       Q2.
           SET RMP--FIRST-FIELD TO TRUE.
           MOVE ZEROES TO FIN.
           DISPLAY " [ESC] para salir - [F9]-[F10] consulta archivo "
                    LINE 25 POSITION 1 LOW CONTROL TIPO-1.
           PERFORM WITH TEST AFTER UNTIL ESCAPE-KEY OR FIN = 1
                   PERFORM RMP--EE-LIBA122R
                   PERFORM RMP--VF-LIBA122R
                   IF RMP--INVALID-INPUT
                         MOVE RMP--FIELD-NAME TO RMP--NEXT-FIELD
                         MOVE 0 TO RMP--EXCEPTION-NUMBER
                         PERFORM RMP--SFE-LIBA122R
                   END-IF

                   IF NIVEL (7411) = "X"
                      EVALUATE RMP--NEXT-FIELD
                         WHEN "LFECHA2"
                              IF LFECHA1 NOT = ZEROES AND
                                 NIVEL (7408) = "X"    AND
                                 CTRLREPE (1) = 0
                                 MOVE LFECHA1 TO LFECHA2
                                 MOVE 1       TO CTRLREPE (1)
                              END-IF
                      END-EVALUATE
                   END-IF

                   IF RMP--LAST-FIELD = "LCUENTA" AND
                               RMP--EXCEPTION-NUMBER = 10 
                      PERFORM CUENTA-TABLA-CONSU THRU 
                                                 F-CUENTA-TABLA-CONSU
                      MOVE ZEROES   TO RMP--EXCEPTION-NUMBER
                      MOVE CODIGON5 TO LCUENTA
                      SET RMP--REPEAT-EVENT TO TRUE
                   END-IF

                   IF RMP--LAST-FIELD = "LCUENTA" AND 
                               RMP--EXCEPTION-NUMBER = 09 
                      MOVE LCUENTA    TO CODIGON5
                      PERFORM CUENTA-FILE-CONSU THRU 
                                               F-CUENTA-FILE-CONSU
                      MOVE ZEROES   TO RMP--EXCEPTION-NUMBER
                      SET RMP--REPEAT-EVENT TO TRUE
                   END-IF

                   IF RMP--LAST-FIELD = "LCUENTA" 

                      PERFORM LEO-CUENTA THRU F-LEO-CUENTA

                   END-IF

                   IF RMP--EXCEPTION-NUMBER = 54
                      SET RMP--FIRST-FIELD TO TRUE
                   END-IF

                   IF RMP--EXCEPTION-NUMBER = 82
                      MOVE "LPERIODO" TO RMP--NEXT-FIELD
                   END-IF

                   IF RMP--LAST-FIELD = "LPERIODO" AND
                      (RMP--EXCEPTION-NUMBER = 13 OR 
                       RMP--EXCEPTION-NUMBER = 00)
                      MOVE 1 TO FIN
                   END-IF

           END-PERFORM.
           PERFORM RMP--DAF-LIBA122R.
           DISPLAY LIMPIA LINE 25 POSITION 1 LOW CONTROL TIPO-1.
           PERFORM RMP--VAF-LIBA122R.
           IF RMP--INVALID-INPUT
                   MOVE RMP--FIELD-NAME TO RMP--NEXT-FIELD
                   MOVE 0 TO RMP--EXCEPTION-NUMBER
                   PERFORM RMP--SFE-LIBA122R
                   GO Q2.

           IF RMP--EXCEPTION-NUMBER = 27 GO F-PROCESO.

           MOVE LFECHA1 TO FECHA-D.
           MOVE LFECHA2 TO FECHA-H.
           IF FECHA-D    > FECHA-H GO PROCESO.
           IF LCUENTA = ZEROES     GO PROCESO.

           PERFORM RMP--DAF-LIBA122R.
       Q3.

           COPY \USER\FRIGO\MENU\CORRELIS.PRO.

           IF RMP--EXCEPTION-NUMBER = 27 GO F-PROCESO.
           IF SN = "S" OR SN = "s" GO Q4.
           GO Q2.

       Q4.
           PERFORM RMP--CAF-MENSAJE.
           MOVE "El archivo sera generado en C:\SIAP.                  "
                                              TO PMENSAJE (1).
           MOVE "Pulse [ENTER] - [ESC]- Salir.                       "
                                              TO PMENSAJE (2).
           PERFORM MENSAJE THRU F-MENSAJE.
           IF SALIDA = 27 GO PROCESO.
       Q5.
           DISPLAY LIMPIA LINE 25 POSITION 1 LOW CONTROL TIPO-1.
           DISPLAY " Procesando... [ESC] Interrumpe "
                   LINE 25 POSITION 1 LOW CONTROL TIPO-1 BLINK.


           MOVE SPACES TO ARCHI-RETETXT ARCHI-RETECSV.

           MOVE CUIT-DATOS TO M-CUIT.

           IF LPERIODO = 0
              STRING  "C:\SIAP\"  DELIMITED BY SIZE
                      "ER-"       DELIMITED BY SIZE
                      M1-CUIT     DELIMITED BY SIZE
                      M2-CUIT     DELIMITED BY SIZE
                      M3-CUIT     DELIMITED BY SIZE
                      "-"         DELIMITED BY SIZE
                      AA-D        DELIMITED BY SIZE
                      MM-D        DELIMITED BY SIZE
                      LPERIODO    DELIMITED BY SIZE
                      "-D7-LOTE1" DELIMITED BY SIZE
                      ".TXT"      DELIMITED BY SIZE
                      INTO ARCHI-RETETXT
              END-STRING

              STRING  "C:\SIAP\"  DELIMITED BY SIZE
                      "ER-"       DELIMITED BY SIZE
                      M1-CUIT     DELIMITED BY SIZE
                      M2-CUIT     DELIMITED BY SIZE
                      M3-CUIT     DELIMITED BY SIZE
                      "-"         DELIMITED BY SIZE
                      AA-D        DELIMITED BY SIZE
                      MM-D        DELIMITED BY SIZE
                      LPERIODO    DELIMITED BY SIZE
                      "-D7-LOTE1" DELIMITED BY SIZE
                      ".CSV"      DELIMITED BY SIZE
                      INTO ARCHI-RETECSV
              END-STRING

           ELSE
              STRING  "C:\SIAP\"  DELIMITED BY SIZE
                      "ER-"       DELIMITED BY SIZE
                      M1-CUIT     DELIMITED BY SIZE
                      M2-CUIT     DELIMITED BY SIZE
                      M3-CUIT     DELIMITED BY SIZE
                      "-"         DELIMITED BY SIZE
                      AA-D        DELIMITED BY SIZE
                      MM-D        DELIMITED BY SIZE
                      LPERIODO    DELIMITED BY SIZE
                      "-P7-LOTE1" DELIMITED BY SIZE
                      ".TXT"      DELIMITED BY SIZE
                      INTO ARCHI-RETETXT
              END-STRING

              STRING  "C:\SIAP\"  DELIMITED BY SIZE
                      "ER-"       DELIMITED BY SIZE
                      M1-CUIT     DELIMITED BY SIZE
                      M2-CUIT     DELIMITED BY SIZE
                      M3-CUIT     DELIMITED BY SIZE
                      "-"         DELIMITED BY SIZE
                      AA-D        DELIMITED BY SIZE
                      MM-D        DELIMITED BY SIZE
                      LPERIODO    DELIMITED BY SIZE
                      "-P7-LOTE1" DELIMITED BY SIZE
                      ".CSV"      DELIMITED BY SIZE
                      INTO ARCHI-RETECSV
              END-STRING

           END-IF.

           OPEN OUTPUT RETETXT RETECSV.

           IF PAGCONFI-NUMECA = "N"
              MOVE ZEROES  TO CLAVE1-CAJA1
              MOVE FECHA-D TO FECHA-CAJA1
              START CAJA1 KEY NOT LESS THAN CLAVE1-CAJA1
                                            INVALID KEY
                                            GO ARMO100
              END-START
              ELSE
              MOVE ZEROES  TO CLAVE1-CAJA2
              MOVE FECHA-D TO FECHA-CAJA2
              START CAJA2 KEY NOT LESS THAN CLAVE1-CAJA2
                                            INVALID KEY
                                            GO ARMO100
              END-START
           END-IF.
       ARMO1.
           IF PAGCONFI-NUMECA = "N"
              READ CAJA1 NEXT AT END GO ARMO100
              END-READ
              ELSE
              READ CAJA2 NEXT AT END GO ARMO100
              END-READ
              MOVE REG-CAJA2 TO REG-CAJA1
           END-IF.

           DISPLAY CLAVE-CAJA1
                   LINE 25 POSITION 35 LOW CONTROL TIPO-1.

           MOVE ZEROES TO KEY-DECIMAL.
           PERFORM GET-KEY THRU F-GET-KEY.
           IF KEY-DECIMAL = 27 GO ARMO100.

           IF FECHA-CAJA1 < FECHA-D GO ARMO1.
           IF FECHA-CAJA1 > FECHA-H GO ARMO100.

           IF NRO-CAJA1 < LDESDENRO GO ARMO1.
           IF NRO-CAJA1 > LHASTANRO GO ARMO1.

           IF CUENTA-CAJA1 NOT = LCUENTA GO ARMO1.

       ARMO2.

           MOVE NRO-CAJA1 TO NRO-INT-CAJA.
           READ CAJA INVALID KEY MOVE ZEROES TO REG-CAJA
                                 MOVE SPACES TO TIP-COMP-CAJA
                                 MOVE SPACES TO LET-COMP-CAJA
                                 MOVE ZEROES TO SUC-COMP-CAJA
                                 MOVE ZEROES TO NRO-COMP-CAJA
                                 MOVE SPACES TO TM-CAJA
                                 MOVE "S"    TO PAGCONFI-CAJA
           END-READ.

           IF LTM = SPACES GO ARMO2A.

           IF TM-CAJA NOT = LTM GO ARMO1.

       ARMO2A.

           IF PAGCONFI-NUMECA = "S" AND PAGCONFI-CAJA = "N" GO ARMO1.

           MOVE SPACES TO CUIT-RETE PROVI-RETE.
           MOVE ZEROES TO NRO-RETE-AUX.

           MOVE SPACES TO REGIMEN-AUX.

           MOVE SPACES TO PROVI-RETE.

      *SOLO PARA CUIT EXCEPTO OPPH*
      
           IF NRO-RIIB-CAJA = ZEROES GO ARMO1.

           IF TIP-COMP-CAJA = "OPPH" PERFORM TOMO-OPPH THRU F-TOMO-OPPH.
           IF TIP-COMP-CAJA = "OPPV" PERFORM TOMO-OPPV THRU F-TOMO-OPPV.
           IF TIP-COMP-CAJA = "OPPC" PERFORM TOMO-OPPC THRU F-TOMO-OPPC.

           IF NRO-RIIB-CAJA = ZEROES GO ARMO1.

       ARMO02.

           MOVE NRO-RIIB-CAJA TO NRO-RETE-AUX.

           MOVE NRO-RIIB-CAJA TO NRO-RET-RETEIIBB.
           READ RETEIIBB INVALID KEY MOVE ZEROES TO REG-RETEIIBB.
           
           MOVE ZEROES              TO REG-RETETXT.

           MOVE CUIT-RETE           TO CUIT-A.
           MOVE CUIT-A1             TO CUIT-B1.
           MOVE CUIT-A3             TO CUIT-B3.
           MOVE CUIT-A5             TO CUIT-B5.

           MOVE CUIT-A              TO CUIT-RETETXT.

           STRING DD-CAJA           DELIMITED BY SIZE
                  "/"               DELIMITED BY SIZE
                  MM-CAJA           DELIMITED BY SIZE
                  "/"               DELIMITED BY SIZE
                  AA-CAJA           DELIMITED BY SIZE
                  INTO FECHAO-RETETXT
           END-STRING.

           MOVE ZEROES TO IND.
           PERFORM WITH TEST AFTER UNTIL IND = 40
                ADD 1 TO IND

                IF SUJ-RETEIIBB (IND) NOT = ZEROES
                   ADD SUJ-RETEIIBB (IND) TO AUX-14-N
                END-IF

                MOVE POR-RETEIIBB (1) TO ALI-RETETXT

           END-PERFORM.

       ARMO02A.

           MOVE AUX-14-N   TO AUX-14-OUT
           MOVE AUX-14-OUT TO BIMP-RETETXT
      *     MOVE AUX-14              TO BIMP-RETETXT.
      
           DISPLAY BIMP-RETETXT.
           ACCEPT SN.
           DISPLAY REG-RETETXT.
           ACCEPT SN.

           INSPECT REG-RETETXT REPLACING ALL "Ñ" BY "N".
           INSPECT REG-RETETXT REPLACING ALL "ñ" BY "n".

           WRITE REG-RETETXT.

           ADD 1 TO IND-CSV.

           IF IND-CSV = 1
              MOVE SPACES TO REG-RETECSV
              STRING "CUIT DEL RETENIDO"      DELIMITED BY SIZE
                     ";"                      DELIMITED BY SIZE
                     "FECHA EMISION"          DELIMITED BY SIZE
                     ";"                      DELIMITED BY SIZE
                     "NUMERO RETENCION"       DELIMITED BY SIZE
                     ";"                      DELIMITED BY SIZE
                     "IMPORTE RETENCION"      DELIMITED BY SIZE
                     ";"                      DELIMITED BY SIZE
                     "TIPO RETENCION"         DELIMITED BY SIZE
                     ";"                      DELIMITED BY SIZE 
                     INTO TRAZO-RETECSV
              END-STRING

              WRITE REG-RETECSV AFTER 1 END-WRITE

           END-IF.

           STRING CUIT-RETETXT          DELIMITED BY SIZE
                  ";"                   DELIMITED BY SIZE
                  SUCU-RETETXT          DELIMITED BY SIZE
                  ";"                   DELIMITED BY SIZE
                  FECHAO-RETETXT        DELIMITED BY SIZE
                  ";"                   DELIMITED BY SIZE
                  ALI-RETETXT           DELIMITED BY SIZE
                  ";"                   DELIMITED BY SIZE
                  BIMP-RETETXT          DELIMITED BY SIZE
                  ";"                   DELIMITED BY SIZE
                  INTO TRAZO-RETECSV
           END-STRING.

           INSPECT TRAZO-RETECSV REPLACING ALL "," BY ".",
                                               "." BY ",".

           WRITE REG-RETECSV AFTER 1.

       ARMO3.

           GO ARMO1.

       ARMO100.

           CLOSE RETETXT RETECSV.

           MOVE ARCHI-RETETXT TO ARCHI-ZIP ARCHI-TXT-TEMP.

           INSPECT ARCHI-ZIP REPLACING ALL "TXT" BY "zip".

           MOVE SPACES TO RUTA-TEMP.
           STRING  NOMDIRFRIGO-DATOS    DELIMITED BY " "
                   "7Z a "              DELIMITED BY SIZE
                   ARCHI-ZIP            DELIMITED BY "  "
                   " "                  DELIMITED BY SIZE
                   ARCHI-TXT-TEMP       DELIMITED BY "  "
                   INTO RUTA-TEMP.

           CALL "SYSTEM" USING RUTA-TEMP.

           DISPLAY LIMPIA LINE 25 POSITION 1 LOW CONTROL TIPO-1.

           PERFORM RMP--CAF-MENSAJE.
           MOVE "ZIP generado en:"   TO PMENSAJE (1).
           MOVE ARCHI-ZIP            TO PMENSAJE (2).
           PERFORM MENSAJE THRU F-MENSAJE.

           PERFORM RMP--CAF-MENSAJE.
           MOVE "Ahora debe utilizar el aplicativo de ARBA denominado"
                                     TO PMENSAJE (1).
           MOVE "GenHash para completar el proceso. [ENTER] continuar."
                                     TO PMENSAJE (2).
           PERFORM MENSAJE THRU F-MENSAJE.

           DISPLAY LIMPIA LINE 25 POSITION 1 LOW CONTROL TIPO-1.

           PERFORM RMP--CAF-MENSAJE.
           MOVE "Proceso Concluido.                                    " 
                                              TO PMENSAJE (1).
           MOVE "Pulse [ENTER].                                        "
                                              TO PMENSAJE (2).
           PERFORM MENSAJE THRU F-MENSAJE.

           MOVE "EXPORT.SIAP IIBB    " TO N-A-AUDI.
           MOVE SPACES                 TO C-A-AUDI.
           MOVE "A"                    TO T-A-AUDI.
           PERFORM GRABO-AUDI THRU F-GRABO-AUDI.

           GO PROCESO.

       F-PROCESO. EXIT.
      ******************************************************
      *							   *
      *         S U B     -  R U T I N A S                 *
      *							   *
      ******************************************************
       CALCULO-IMPORTE.

           MOVE ZEROES TO Y.

           MOVE ZEROES TO IMPORTE-TOTAL.

       CI1.
           ADD 1 TO Y.

           IF Y > 10 GO F-CALCULO-IMPORTE.

           IF FORMA-V-CAJA (Y) = ZEROES GO CI1.

           IF SR-V-CAJA (Y) = "S"
              ADD IMP-V-CAJA (Y) TO IMPORTE-TOTAL
           END-IF.

       CI2.
           GO CI1.

       F-CALCULO-IMPORTE. EXIT.
      ******************************************************
       CALCULO-IMPORTE-SUJETO.

           MOVE ZEROES TO Y.

           MOVE ZEROES TO IMPORTE-SUJETO.

       CIS1.
           ADD 1 TO Y.

           IF Y > 40 GO F-CALCULO-IMPORTE-SUJETO.

           IF INT-RETEIIBB (Y) = ZEROES GO CIS1.

           ADD SUJ-RETEIIBB (Y) TO IMPORTE-SUJETO.

       CIS2.
           GO CIS1.

       F-CALCULO-IMPORTE-SUJETO. EXIT.
      ******************************************************
       TOMO-OPPH.

           MOVE NRO-COMP-CAJA     TO NRO-INT-PAGOHAC.

           READ PAGOHAC INVALID KEY MOVE ZEROES TO REG-PAGOHAC.

           MOVE COD-CONSI-PAGOHAC TO COD-CONSI.
           READ CONSI INVALID KEY MOVE SPACES TO CUIT-CONSI.
           MOVE PROVI-CONSI         TO PROVI-RETE.
           
           IF PROVI-CONSI NOT = "B" GO F-TOMO-OPPH.

           MOVE CUIT-CONSI          TO CUIT-RETE.
           MOVE ZEROES              TO REG-RETETXT.

           MOVE CUIT-RETE           TO CUIT-A.
           MOVE CUIT-A1             TO CUIT-B1.
           MOVE CUIT-A3             TO CUIT-B3.
           MOVE CUIT-A5             TO CUIT-B5.

           MOVE CUIT-A              TO CUIT-RETETXT.

           STRING DD-CAJA           DELIMITED BY SIZE
                  "/"               DELIMITED BY SIZE
                  MM-CAJA           DELIMITED BY SIZE
                  "/"               DELIMITED BY SIZE
                  AA-CAJA           DELIMITED BY SIZE
                  INTO FECHAO-RETETXT
           END-STRING.

           MOVE NRO-IIBB-CONSI    TO IIBB-RETE.

           MOVE NRO-INT-PAGOHAC   TO NRO-INT-PAGOHAC1.
           MOVE ZEROES            TO ITEM-PAGOHAC1.

           START PAGOHAC1 KEY NOT LESS THAN CLAVE-PAGOHAC1 INVALID KEY
                                            GO F-TOMO-OPPH.
       TOMO-OPPH1.
           READ PAGOHAC1 NEXT AT END GO F-TOMO-OPPH.

           IF NRO-INT-PAGOHAC1 < NRO-INT-PAGOHAC GO TOMO-OPPH1.
           IF NRO-INT-PAGOHAC1 > NRO-INT-PAGOHAC GO ARMO02A.

           MOVE INT-PAGOHAC1  TO NRO-INT-FACTUHAC.
           READ FACTUHAC INVALID KEY GO TOMO-OPPH3.
           
           ADD IMP-GRA-FACTUHAC       TO AUX-14-N.

           COMPUTE ALI-RETETXT =
                   (IMP-INB-FACTUHAC / IMP-GRA-FACTUHAC) * 100.

       TOMO-OPPH2.
           GO TOMO-OPPH1.

       TOMO-OPPH3.

           MOVE INT-PAGOHAC1  TO NRO-INT-DEBIHAC.
           READ DEBIHAC INVALID KEY GO F-TOMO-OPPH.
           
           ADD IMP-GRA-DEBIHAC       TO AUX-14-N.
           
           COMPUTE ALI-RETETXT = 
                   (IMP-INB-DEBIHAC / IMP-GRA-DEBIHAC) * 100.

       TOMO-OPPH4.
           GO TOMO-OPPH1.

       F-TOMO-OPPH. EXIT.
      ***************************************************************
       TOMO-OPPV.

           MOVE NRO-COMP-CAJA     TO NRO-INT-PAGOCPA.

           READ PAGOCPA INVALID KEY MOVE ZEROES TO REG-PAGOCPA.

           MOVE COD-PROVE-PAGOCPA TO COD-PROVE.
           READ PROVE INVALID KEY MOVE SPACES TO CUIT-PROVE.

           MOVE CUIT-PROVE        TO CUIT-RETE.


       F-TOMO-OPPV. EXIT.
      ***************************************************************
       TOMO-OPPC.

           MOVE NRO-COMP-CAJA     TO NRO-INT-PAGOCSP.

           READ PAGOCSP INVALID KEY MOVE ZEROES TO REG-PAGOCSP.

           MOVE COD-PROVC-PAGOCSP TO COD-PROVC.
           READ PROVC INVALID KEY MOVE SPACES TO CUIT-PROVC.

           MOVE CUIT-PROVC        TO CUIT-RETE.
           MOVE PROVI-PROVC       TO PROVI-RETE.


       F-TOMO-OPPC. EXIT.
      ***************************************************************
       LEO-CUENTA.

           MOVE LCUENTA TO COD-CUENTA.

           READ CUENTA INVALID KEY 
                       MOVE "LCUENTA" TO RMP--NEXT-FIELD
                       MOVE SPACES    TO NOM-CUENTA  
           END-READ.

           MOVE NOM-CUENTA TO LNOMCUENTA.

           PERFORM RMP--DAF-LIBA122R.

       F-LEO-CUENTA. EXIT.
      *****************************************************************

           COPY \USER\FRIGO\CONTA\CTA-TA.PRO.

      ***************************************************************
       ABRO-ARCHIVO.
           OPEN INPUT CAJA BANCO CUENTA CENTRO CONCE CP PROVI CONSI
                      USUA CLI MUNI PAIS PROVE PROVC TIPOP FORMA 
                      FACTUCPA DEBICPA SINICPA PAGOCPA PAGOCPA1
                      FACTUCSP DEBICSP SINICSP PAGOCSP PAGOCSP1
                      FACTUHAC DEBIHAC SINIHAC PAGOHAC PAGOHAC1
                      CAJA1 RETEIIBB CAJA2.
      ***************************************************************
       CARGOMENU.
           MOVE "CAJA.LIB" TO RMP--LIBRARY.
           PERFORM LEO        THRU F-LEO.
           PERFORM LEO-CLAVES THRU F-LEO-CLAVES.
           MOVE " Exportacion al SIAP IIBB.        " TO PPROG.
           MOVE NOM-DATOS                            TO PEMP.
           MOVE "[F1] - Help"                        TO PHELP.
           MOVE SPACES                               TO POP.
           STRING "{"     DELIMITED BY SIZE
                  USUARIO DELIMITED BY "  "
                  "}"     DELIMITED BY SIZE
                  INTO PUSUARIO
           END-STRING.
           MOVE FECHA-1                            TO PDATE.
           MOVE "M"          TO GETENV-OPTIONS.
           MOVE "TERMINAL"   TO ENV-NAME.
           PERFORM GET-ENV THRU F-GET-ENV.

           OPEN INPUT NUMEHAC.
           MOVE 1 TO COD-NUMEHAC.
           READ NUMEHAC INVALID KEY MOVE ZEROES TO REG-NUMEHAC.
           CLOSE NUMEHAC.

           OPEN INPUT NUMECA.
           MOVE 1 TO COD-NUMECA.
           READ NUMECA INVALID KEY MOVE SPACES TO REG-NUMECA.
           CLOSE NUMECA.

      ***************************************************************
       CIERRO-ARCHIVO.
           CLOSE CAJA BANCO CUENTA CENTRO CONCE CP PROVI CONSI
                 USUA CLI MUNI PAIS PROVE PROVC TIPOP FORMA
                 FACTUCPA DEBICPA SINICPA PAGOCPA PAGOCPA1
                 FACTUCSP DEBICSP SINICSP PAGOCSP PAGOCSP1
                 FACTUHAC DEBIHAC SINIHAC PAGOHAC PAGOHAC1
                 CAJA1 RETEIIBB CAJA2.
      ***************************************************************

           COPY LEO.PRO.

           COPY \USER\FRIGO\MENU\GETKEY.PRO.
           COPY \USER\FRIGO\MENU\GETENV.PRO.

      ***************************************************************
       ARMO-FORMATO.
           PERFORM RMP--DP-FORMATO.
           PERFORM RMP--DAF-FORMATO.
       F-ARMO-FORMATO.
      ***************************************************************

           COPY LIBA122R.PRC.

           COPY CUENTA.PRC.

           COPY PANTA.PRC.
           COPY PANTA1.PRC.
           COPY PANTA2.PRC.
           COPY PANTA3.PRC.
           COPY PANTA4.PRC.
           COPY CONSULTA.PRC.
           COPY DENEGADO.PRC.
           COPY FONDO.PRC.
           COPY FORMATO.PRC.
           COPY CORRELIS.PRC.
           COPY MENSAJE.PRC.

      ********** FIN DEL PROGRAMA OPC44.CBL ***********
