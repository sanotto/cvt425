*SRCMBRTXT:Subrutina: Costo Financiero Total sin I
     H******************************************************************
     H*                                                                *
     H*  APPLICATION CODE : SD V00 01                                  *
     H*                                                                *
     H*  APPLICATION NAME : Sistema Integrado De Bancos                *
     H*                                                                *
     H*  PROGRAM NAME     : SBBACFTC                                   *
     H*                                                                *
     H*  PROGRAM TITLE    : PRESTAMOS-CALCULO DE CFT SIN IVA
     H*                                                                *
     H*                                                                *
     H******************************************************************
     H* FUNCIONA USANDO IRR (INTERNAL RATE OF RETURN, VEASE SBBACIR2)  *
     H*                                                                *
     H******************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    CFTNOM           15 5
     C                   PARM                    CFTEFE           15 5
     C*
     C                   Z-ADD     *ZERO         CFTNOM
     C                   Z-ADD     *ZERO         CFTEFE
     C*
     C                   Z-ADD     30            WWQCAN            3 0
     C*
     C                   Z-ADD     *ZERO         IMPIRR           15 5
     C                   MOVE      *BLANKS       ERRTXT           50
     C                   CALL      'SBBACIR2'
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
     C*=====================================================================
