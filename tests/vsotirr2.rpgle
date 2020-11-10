*SRCMBRTXT:VARIOS: CALC IRR-MET FUERZA BRUTA C/BIS
     H******************************************************************
     H*                                                                *
     H*  APPLICATION CODE : SD V00 01                                  *
     H*                                                                *
     H*  APPLICATION NAME : Sistema Integrado De Bancos                *
     H*                                                                *
     H*  PROGRAM NAME     : PRHA00MA                                   *
     H*                                                                *
     H*  PROGRAM TITLE    : PRESTAMOS-CESION HABERES-MANEJADOR PRINCIP.*
     H*                                                                *
     H*  DATE GENERATED   : 08/01/04                                   *
     H*                                                                *
     H*  AUTHOR           : LE00525                                    *
     H*                                                                *
     H*  IPG LEVEL        : IPG/400 VERSION 2 (R3.0)                   *
     H*                                                                *
     H*  PTF LEVEL        : PÑGÑV20114                                 *
     H*                                                                *
     H******************************************************************
     H* CARGAR EN BASCTM EN S1$IMP LOS IMPORTES DEBE HABER AL MENOS    *
     H* 1 IMPORTE NEGATIVO PARA QUE LA SERIE SEA CONVERGENTE           *
     H*                                                                *
     H* EL IRR DE LA SERIE DEBE ESTAR ENTRE 0% Y 20% DE LO CONTRARIO   *
     H* EL ALGORITMO NO CONVERGERA                                     *
     H*                                                                *
     H* EN CASO DE NECESITAR AMPLIAR CAMBIAR X2 Y MAXLOOPS             *
     H*                                                                *
     H*                                                                *
     H*                                                                *
     H*                                                                *
     H******************************************************************
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
     D ERR004          C                   CONST('SERIE NO CONVERGENTE-
     D                                      EN EL INT 0%-20%  ')
     D*----------------------------------------------------------------*
     D CF              S             15  5 DIM(999)
     D I               S             15  0
     D J               S             15  0
     D MAXLOOPS        S             15  0 INZ(5000)
     D ACCURACY        S              7  7 INZ(0.00001)
     D RATE            S              7  7
     D*----------------------------------------------------------------*
     C                   EXSR      CNTPER
     C                   EXSR      CHKPAR
     C                   Z-ADD     *ZERO         X1                7 5
     C                   Z-ADD     0.20          X2                7 5
     C*
     C                   Z-ADD     X1            RATE
     C                   EXSR      CALNPV
     C                   Z-ADD     IMPNPV        F1               15 5
     C*
     C                   Z-ADD     X2            RATE
     C                   EXSR      CALNPV
     C                   Z-ADD     IMPNPV        F2               15 5
     C*
     C                   FOR       I = 1 TO MAXLOOPS
     C                   IF        (F1 * F2) < 0
     C                   LEAVE
     C                   ENDIF
     C                   IF        %ABS(F1) < %ABS(F2)
     C                   EVAL      X1 = X1 + 1.6 * (X1-X2)
     C                   Z-ADD     X1            RATE
     C                   EXSR      CALNPV
     C                   Z-ADD     IMPNPV        F1               15 5
     C                   ELSE
     C                   EVAL      X2 = X2 + 1.6 * (X2-X1)
     C                   Z-ADD     X1            RATE
     C                   EXSR      CALNPV
     C                   Z-ADD     IMPNPV        F1               15 5
     C                   ENDIF
     C                   ENDFOR
     C*
     C                   IF        (F1 * F2) > 0
     C                   MOVE      ERR004        ERRTXT
     C                   Z-ADD     *ZERO         IMPIRR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   Z-ADD     X1            RATE
     C                   EXSR      CALNPV
     C                   Z-ADD     IMPNPV        F                15 5
     C*
     C                   IF        F <  0
     C                   Z-ADD     X1            RTB               9 5
     C     X2            SUB       X1            DX                9 5
     C                   ELSE
     C                   Z-ADD     X2            RTB               9 5
     C     X1            SUB       X2            DX                9 5
     C                   ENDIF
     C*
     C                   FOR       I = 1 TO MAXLOOPS
     C                   EVAL      DX = DX *0.5
     C     RTB           ADD       DX            XMID              9 5
     C                   Z-ADD     XMID          RATE
     C                   EXSR      CALNPV
     C                   Z-ADD     IMPNPV        FMID             15 5
     C                   IF        FMID <= 0
     C                   EVAL      RTB=XMID
     C                   ENDIF
     C                   IF        %ABS(FMID) < ACCURACY OR %ABS(DX) < ACCURACY
     C                   Z-ADD     XMID          IMPIRR
     C                   MULT      100           IMPIRR
     C                   EVAL      ERRTXT=*BLANKS
     C                   EXSR      ENDPGM
     C                   ENDIF
     C                   ENDFOR
     C*
     C                   MOVE      ERR004        ERRTXT
     C                   Z-ADD     *ZERO         IMPIRR
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* CALNPV: CALCULA NPV
     C*-------------------------------------------------------------------------
     C     CALNPV        BEGSR
     C*
     C                   Z-ADD     *ZERO         IMPNPV           15 5
     C                   FOR       J= 1 TO TOTPER
     C                   EVAL      IMPNPV=IMPNPV + (CF(J) / ((1 +RATE ) ** J))
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
     C     *ENTRY        PLIST
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
