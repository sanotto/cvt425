*SRCMBRTXT:Aut.Federal-Man.Alta de Op.-Chequea bol
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: FEDERAL                                         *
     H*                                                               *
     H*  PROGRAM NAME: PRFD04R5                                       *
     H*                                                               *
     H*  PROGRAM NO: Chequea si hay movs pendientes en bolsa y si los *
     H*              hay, los cobra                                   *
     H*                                                               *
     H*  DATE: 22/11/2013                                             *
     H*                                                               *
     H*  AUTHOR: SANTIAGO OTTONELLO                                   *
     H*                                                               *
     H*****************************************************************
     FACMOVB01  IF   E           K DISK
     C*-------------------------------------------------------------------------
     C     KIN012        CHAIN     REACMOVB                           99
     C     *IN99         DOWEQ     *OFF
     C     INIMCA        IFEQ      8
     C     INIMCA        OREQ      58
     C     INIMCA        OREQ      63
     C     INIMCA        OREQ      129
     C     INIMCA        OREQ      181
     C     INIMCA        OREQ      183
     C     INIMCA        OREQ      998
     C     INIMCA        OREQ      56
     C     INIMCA        OREQ      44
     C     INIMCA        OREQ      128
     C     INIMCA        OREQ      148
     C     INIMCA        OREQ      182
     C     INIMCA        OREQ      184
     C     INIMCA        OREQ      999
     C                   CALL      'AC0106PR'
     C                   PARM                    PAISUC
     C                   PARM                    PAICAH
     C                   PARM                    PAFACR
     C                   LEAVE
     C                   ENDIF
     C     KIN012        READE     REACMOVB                               99
     C                   ENDDO
     C                   EXSR      ENDPGM
     C*--------------------------------------------------------------*
     C* *INZSR:SUBRUTINA DE INICIALIZACIO
     C*--------------------------------------------------------------*
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAISUC
     C                   PARM                    PAICAH
     C                   PARM                    PAFACR
     C*
     C     *LIKE         DEFINE    INISUC        PAISUC
     C     *LIKE         DEFINE    INICAH        PAICAH
     C     *LIKE         DEFINE    INFACR        PAFACR
     C*
     C     KIN012        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICAH
     C*
     C                   ENDSR
     C*--------------------------------------------------------------*
     C* ENDPGM: Finaliza el Programa
     C*--------------------------------------------------------------*
     C     ENDPGM        BEGSR
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
