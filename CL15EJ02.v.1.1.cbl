      ******************************************************************
      * Author: GABRIELA RODRIGUEZ
      * Date: 25/09/2023
      * Purpose: ABM CON APAREO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL15EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-MAESTRO
           ASSIGN TO '../ARCH_MAESTRO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-MAESTRO.

       SELECT ENT-NOVEDADES
           ASSIGN TO '../ARCH_NOVEDADES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVEDADES.

       SELECT SAL-MAEACT
           ASSIGN TO '../MAESTRO_ACTUALIZADO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-MAEACT.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-MAESTRO.
          COPY MAESTRO.

       FD ENT-NOVEDADES.
          COPY NOVEDADES.

       FD SAL-MAEACT.
       01 WS-SAL-MAEACT                     PIC X(65).


       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-MAESTRO                     PIC X(2).
             88 FS-MAESTRO-OK                   VALUE '00'.
             88 FS-MAESTRO-EOF                  VALUE '10'.
             88 FS-MAESTRO-NFD                  VALUE '35'.
          05 FS-NOVEDADES                   PIC X(2).
             88 FS-NOVEDADES-OK                 VALUE '00'.
             88 FS-NOVEDADES-EOF                VALUE '10'.
             88 FS-NOVEDADES-NFD                VALUE '35'.
          05 FS-MAEACT                      PIC X(2).
             88 FS-MAEACT-OK                    VALUE '00'.
             88 FS-MAEACT-EOF                   VALUE '10'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-MAESTRO            PIC 9(04) VALUE 0.
          05 WS-CONT-REG-NOVEDAD            PIC 9(06) VALUE 0.
          05 WS-CONT-REG-SALIDA             PIC 9(04) VALUE 0.
          05 WS-CONT-REG-ERROR              PIC 9(04) VALUE 0.

       01 WS-VARIABLES-AUX.
           05 WS-IMP-ACUM                   PIC 9(10)V9(02) VALUE 0.
           05 WS-FORMAT-CANT                PIC ZZZ.ZZ9.
           05 WS-CLAVE-MIN                  PIC X(10).
           05 WS-GRABAR-MAESTRO             PIC X(02).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-EXIT
                UNTIL FS-MAESTRO-EOF
                  AND FS-NOVEDADES-EOF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.
      *----------------------------------------------------------------*

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-MAESTRO
              THRU 1100-ABRIR-MAESTRO-EXIT.

           PERFORM 1200-ABRIR-NOVEDAD
              THRU 1200-ABRIR-NOVEDAD-EXIT.

           PERFORM 1300-ABRIR-MAEACT
              THRU 1300-ABRIR-MAEACT-FIN.

       1000-INICIAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-MAESTRO.
      *----------------------------------------------------------------*

           OPEN INPUT ENT-MAESTRO.

           EVALUATE TRUE
               WHEN FS-MAESTRO-OK
                    PERFORM 1110-LEER-MAESTRO
                       THRU 1110-LEER-MAESTRO-EXIT
               WHEN FS-MAESTRO-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO MAESTRO'
                    DISPLAY 'FILE STATUS: ' FS-MAESTRO
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO MAESTRO'
                    DISPLAY 'FILE STATUS: ' FS-MAESTRO
           END-EVALUATE.

       1100-ABRIR-MAESTRO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-MAESTRO.
      *----------------------------------------------------------------*

           READ ENT-MAESTRO.

           EVALUATE TRUE
               WHEN FS-MAESTRO-OK
                    ADD 1                   TO WS-CONT-REG-MAESTRO
               WHEN FS-MAESTRO-EOF
                    MOVE '9999999999'       TO WS-ENT-LEGAJO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO MAESTRO'
                    DISPLAY 'FILE STATUS: ' FS-MAESTRO
           END-EVALUATE.

       1110-LEER-MAESTRO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-NOVEDAD.
      *----------------------------------------------------------------*

           OPEN INPUT ENT-NOVEDADES.

           EVALUATE TRUE
               WHEN FS-NOVEDADES-OK
                    PERFORM 1210-LEER-NOVEDAD
                       THRU 1210-LEER-NOVEDAD-EXIT
               WHEN FS-NOVEDADES-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE NOVE'
                    DISPLAY 'FILE STATUS: ' FS-NOVEDADES
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE NOVE'
                    DISPLAY 'FILE STATUS: ' FS-NOVEDADES
           END-EVALUATE.

       1200-ABRIR-NOVEDAD-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1210-LEER-NOVEDAD.
      *----------------------------------------------------------------*

           READ ENT-NOVEDADES.

            EVALUATE TRUE
               WHEN FS-NOVEDADES-OK
                    ADD 1                   TO WS-CONT-REG-NOVEDAD
               WHEN FS-NOVEDADES-EOF
                    MOVE '9999999999'       TO WS-NOV-LEGAJO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE VENTAS'
                    DISPLAY 'FILE STATUS: ' FS-NOVEDADES
           END-EVALUATE.

       1210-LEER-NOVEDAD-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1300-ABRIR-MAEACT.
      *----------------------------------------------------------------*

           OPEN OUTPUT SAL-MAEACT.

           EVALUATE FS-MAEACT
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE APAREO'
                    DISPLAY 'FILE STATUS: ' FS-MAEACT
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE APAREO'
                    DISPLAY 'FILE STATUS: ' FS-MAEACT
           END-EVALUATE.

       1300-ABRIR-MAEACT-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.
      *----------------------------------------------------------------*

      *---- GUARDO LA MENOR DE LAS CLAVES EN WS-CLAVE-MIN
           IF WS-ENT-LEGAJO < WS-NOV-LEGAJO
              MOVE WS-ENT-LEGAJO TO WS-CLAVE-MIN
           ELSE
              MOVE WS-NOV-LEGAJO TO WS-CLAVE-MIN
           END-IF.

      *----VERIFICO SI EL MAESTRO ES IGUAL A WS-CLAVE-MIN PARA VER SI
      *----ES CANDIDATO A GRABARSE EN LA SALIDA, FALTA VER SI HAY
      *----NOVEDAD DE BAJA QUE LO PODRIA DESCARTAR.
           IF WS-ENT-LEGAJO = WS-CLAVE-MIN
              MOVE 'SI'          TO WS-GRABAR-MAESTRO
              PERFORM 1110-LEER-MAESTRO
                 THRU 1110-LEER-MAESTRO-EXIT
           ELSE
              MOVE 'NO'          TO WS-GRABAR-MAESTRO
           END-IF.

      *----VERIFICO SI HAY NOVEDADES PARA EL MAESTRO LEIDO O TENGO UN
      *----ALTA DE MAESTRO

           PERFORM 2100-PROCESAR-NOVEDAD
              THRU 2100-PROCESAR-NOVEDAD-EXIT
             UNTIL FS-NOVEDADES-EOF
                OR WS-NOV-LEGAJO NOT EQUAL WS-CLAVE-MIN.

           IF WS-GRABAR-MAESTRO = 'SI'
              PERFORM 2400-GRABAR-SALIDA
                 THRU 2400-GRABAR-SALIDA-EXIT
           END-IF.

       2000-PROCESAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-NOVEDAD.
      *----------------------------------------------------------------*

           EVALUATE WS-NOV-TIPO
           WHEN 'A'
                IF WS-GRABAR-MAESTRO = 'SI'
                    DISPLAY 'REG CON ERROR - ALTA:' WS-ENT-MAESTRO
                ELSE
                    MOVE WS-NOV-NOVEDADES TO WS-SAL-MAEACT
                    MOVE 'SI'             TO WS-GRABAR-MAESTRO
                END-IF
           WHEN 'B'
                IF WS-GRABAR-MAESTRO = 'NO'
                    DISPLAY 'REG.CON ERROR - BAJA:' WS-NOV-NOVEDADES
                ELSE
                    MOVE WS-NOV-NOVEDADES TO WS-SAL-MAEACT
                    MOVE 'NO'             TO WS-GRABAR-MAESTRO
                END-IF
           WHEN 'M'
                IF WS-GRABAR-MAESTRO = 'NO'
                    DISPLAY 'REG.CON ERROR - MODI:' WS-NOV-NOVEDADES
                ELSE
                    MOVE WS-NOV-NOVEDADES TO WS-SAL-MAEACT
                    MOVE 'SI'             TO WS-GRABAR-MAESTRO
                END-IF
           WHEN OTHER
                DISPLAY 'TIPO NOVEDAD ERROR:' WS-NOV-NOVEDADES
           END-EVALUATE.

           PERFORM 1210-LEER-NOVEDAD
              THRU 1210-LEER-NOVEDAD-EXIT.

       2100-PROCESAR-NOVEDAD-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2400-GRABAR-SALIDA.
      *----------------------------------------------------------------*

           WRITE WS-SAL-MAEACT.

           EVALUATE FS-MAEACT
           WHEN '00'
               ADD 1 TO WS-CONT-REG-SALIDA
           WHEN OTHER
               DISPLAY 'ERROR AL GRABAR SALIDA: ' FS-MAEACT
           END-EVALUATE.

       2400-GRABAR-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.
      *----------------------------------------------------------------*

           MOVE WS-CONT-REG-MAESTRO      TO WS-FORMAT-CANT.
           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS: ' WS-FORMAT-CANT.

           MOVE WS-CONT-REG-NOVEDAD      TO WS-FORMAT-CANT.
           DISPLAY 'CANTIDAD DE REGISTROS NOVEDAD  : ' WS-FORMAT-CANT.

           MOVE WS-CONT-REG-SALIDA       TO WS-FORMAT-CANT.
           DISPLAY 'CANTIDAD DE REGISTROS SALIDA   : ' WS-FORMAT-CANT.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.
      *----------------------------------------------------------------*

           CLOSE ENT-MAESTRO
                 ENT-NOVEDADES
                 SAL-MAEACT.

           IF NOT FS-MAESTRO-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO MAESTRO: ' FS-MAESTRO
           END-IF.

           IF NOT FS-NOVEDADES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO NOVEDADES: ' FS-NOVEDADES
           END-IF.

           IF NOT FS-MAEACT-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO SALIDA: ' FS-MAEACT
           END-IF.


       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       END PROGRAM CL15EJ02.
