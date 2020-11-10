*SRCMBRTXT:Subrutina: Costo Financiero Total      
     H******************************************************************
     H*                                                                *
     H*  APPLICATION CODE : SD V00 01                                  *
     H*                                                                *
     H*  APPLICATION NAME : Sistema Integrado De Bancos                *
     H*                                                                *
     H*  PROGRAM NAME     : PRHA00MA                                   *
     H*                                                                *
     H*  PROGRAM TITLE    : PRESTAMOS-CALCULO DE CFT
     H*                                                                *
     H*                                                                *
     H******************************************************************
     H* FUNCIONA USANDO IRR (INTERNAL RATE OF RETURN, VEASE SBBACIRR)  *
     H*                                                                *
     H******************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    CFTNOM           15 5
     C                   PARM                    CFTEFE           15 5
     C                   PARM                    MIVBLE           15 5
     C*
     C                   Z-ADD     *ZERO         CFTNOM
     C                   Z-ADD     *ZERO         CFTEFE
     C                   Z-ADD     *ZERO         MIVBLE
     C*
     C                   Z-ADD     30            WWQCAN            3 0
     C*
     C                   Z-ADD     *ZERO         IMPIRR           15 5
     C                   MOVE      *BLANKS       ERRTXT           50
     C                   CALL      'SBBACIRR'
     C                   PARM                    IMPIRR
     C                   PARM                    ERRTXT
     C*
     C                   EVAL      CFTNOM = (IMPIRR/WWQCAN)*365
     C
     C                   EVAL      CFTEFE = ((CFTNOM*WWQCAN/36500+1) **
     c                                      (365/WWQCAN)-1)*100
     C*
     C                   Z-ADD     CFTEFE        MIVBLE
     C*
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
