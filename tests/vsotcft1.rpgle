*SRCMBRTXT:VARIOS: COSTO FINANCIERO TOTAL         
     D CFTNOM          S             15  5
     D CFTEFE          S             15  5
     C* ... RESTAR GASTOS DE ORIGINACION AL CAPITAL Y DEJAR ESTE MONTO COMO NEGA
     C*     TIVO EN S1$IMP EN BASCTM
     C*     CALCULAR EL IMPORTE TOTAL DE CADA CUOTA Y DEJAR CADA IMPORTE EN
     C*     BASCTM COMO POSITIVO
     C*     LLAMAR AL CALCULO DE IRR
     C*
     C                   Z-ADD     30            WWQCAN            3 0
     C*
     C                   Z-ADD     *ZERO         IMPIRR           15 5
     C                   MOVE      *BLANKS       ERRTXT           50
     C                   CALL      'VSOTIRR2'
     C                   PARM                    IMPIRR
     C                   PARM                    ERRTXT
     C*
     C                   EVAL      CFTNOM = (IMPIRR/WWQCAN)*365
     C
     C                   EVAL      CFTEFE = ((CFTNOM*WWQCAN/36500+1) **
     c                                      (365/WWQCAN)-1)*100
     C*
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
