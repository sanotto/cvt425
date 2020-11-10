*SRCMBRTXT:VARIOS: CALC IRR-MET FUERZA BRUTA      
     FBASCTM    IF   E           K DISK
     D*----------------------------------------------------------------*
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*  Program status data structure.
     D*----------------------------------------------------------------*
     D ERR001          C                   CONST('DEBE HABER AL MENOS -
     D                                     UN IMPORTE POSITIVO')
     D ERR002          C                   CONST('DEBE HABER AL MENOS -
     D                                     UN IMPORTE NEGATIVO')
     D ERR003          C                   CONST('DEBE HABER AL MENOS -
     D                                     DOS IMPORTES       ')
     D*----------------------------------------------------------------*
     D CF              S             15  5 DIM(999)
     D I               S             15  0
     D J               S             15  0
     D MAXLOOPS        S             15  0 INZ(5000)
     D*----------------------------------------------------------------*
     C                   EXSR      CNTPER
     C                   EXSR      CHKPAR
     C                   MOVE      0.00001       GUESS             7 5
     C     IMPNPV        DOULE     *ZERO
     C     I             ORGT      MAXLOOPS
     C                   EXSR      CALNPV
     C                   ADD       0.00001       GUESS
     C                   ADD       1             I
     C                   ENDDO
     C                   EVAL      ERRTXT=*BLANKS
     C     IMPNPV        IFGT      *ZERO
     C                   Z-ADD     *ZERO         IMPIRR
     C                   EVAL      ERRTXT='NO CONVERGE HASTA 5%'
     C                   ENDIF
     C                   Z-ADD     GUESS         IMPIRR
     C     IMPIRR        DIV       100           IMPIRR
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* CALNPV: CALCULA NPV
     C*-------------------------------------------------------------------------
     C     CALNPV        BEGSR
     C*
     C                   Z-ADD     *ZERO         IMPNPV           15 5
     C                   FOR       J= 1 TO TOTPER
     C                   EVAL      IMPNPV=IMPNPV + (CF(J) / ((1 +GUESS) ** J))
     C                   ENDFOR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CHKPAR: CUENTA PERIODOS Y TOTALIZA POSITIVOS Y NEGATIVOS
     C*-------------------------------------------------------------------------
     C     CHKPAR        BEGSR
     C*
     C     TOTPOS        IFEQ      *ZERO
     C                   MOVEL(P)  ERR001        ERRTXT
     C                   MOVE      *ZERO         IMPIRR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C     TOTNEG        IFEQ      *ZERO
     C                   MOVEL(P)  ERR002        ERRTXT
     C                   MOVE      *ZERO         IMPIRR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C     TOTPER        IFLT      2
     C                   MOVEL(P)  ERR003        ERRTXT
     C                   MOVE      *ZERO         IMPIRR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CNTPER: CUENTA PERIODOS Y TOTALIZA POSITIVOS Y NEGATIVOS
     C*-------------------------------------------------------------------------
     C     CNTPER        BEGSR
     C*
     C                   Z-ADD     *ZERO         TOTPOS           15 5
     C                   Z-ADD     *ZERO         TOTNEG           15 5
     C                   Z-ADD     *ZERO         TOTPER           15 0
     C     KS101         CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C     S1$IMP        IFGT      *ZERO
     C                   ADD       S1$IMP        TOTPOS
     C                   ELSE
     C                   ADD       S1$IMP        TOTNEG
     C                   ENDIF
     C                   ADD       1             TOTPER
     C                   EVAL      CF(TOTPER)=S1$IMP
     C     KS101         READE     REBASCTM                               99
     C                   ENDDO
     C*
     C                   EVAL      TOTNEG = TOTNEG * -1
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     PLIST         PLIST
     C                   PARM                    IMPIRR           15 5
     C                   PARM                    ERRTXT           50
     C*
     C     KS101         KLIST
     C                   KFLD                    @PJOBN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
