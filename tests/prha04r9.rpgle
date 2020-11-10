*SRCMBRTXT:Ces.Hab.-Herramienta          -Aplica $
     H DEBUG
     H DECEDIT(',') DATEDIT(*YMD/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: APLICA DINERO A OPER. DE PRESTAMOS DESDE ARCH  *
     H*                TEMPORAL                                       *
     H*                                                               *
     H*  PROGRAM NO: PRHA04R9                                         *
     H*                                                               *
     H*  DATE:    14/11/2013                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*  Nota de Uso:                                                 *
     H*                                                               *
     H*    Rellenar el archivo BASCTM con los siguientes campos       *
     H*                                                               *
     H*                                                               *
     H* S1ISEQ: Periodo sobre el que se calculará el interes          *
     H* S1REFC: 'R' Si dinero es de reintegro                         *
     H* S1QCUO: Código de CtaCte que se utilizará para hacer el deb.  *
     H* S1$INP: Suc de la CtaCte de la que se debitará el dinero     *
     H* S1$CUC: Nro de la CtaCte de la que se debitará el dinero      *
     H* S1$CUI: Nro de Cheque que se asignará al debito               *
     H* S1$A03: Tipo   de Inscripción de la operación de prestamos    *
     H* S1$A04: Numero de Inscripción de la operación de prestamos    *
     H* S1ISUC: Suc de la operación                                   *
     H* S1INCR: Nro de la operación                                   *
     H* S1IDEG: Desglose de la operación                              *
     H* S1$IMP: Importe a aplicar                                     *
     H*                                                               *
     H* Actualiza:                                                    *
     H* S1DACL: Marca que la operación se realizó                     *
     H*                                                               *
     H*****************************************************************

     FBASCTM    UF   E           K DISK

     c     KS1001        Chain     REBASCTM                           99
     c                   DoW       *In99 = *Off
     c*
     C                   Z-Add     S1ISEQ        PAFAAM            6 0
     C                   MoveL     S1REFC        WXORIG            1
     C                   Move      *Blanks       WXINFO           10
     c                   Z-Add     S1QCUO        PAIMCC            3 0
     c                   Z-Add     S1$INP        PAISUC            5 0
     c                   Z-Add     S1$CUC        PAICCC           11 0
     c                   Z-Add     S1$CUI        PAICHE            7 0
     c                   Z-Add     S1$A03        PAITIN            2 0
     c                   Z-Add     S1$A04        PAININ           15 0
     c*
     c                   Call      'PRHA04RA'
     C                   PARM                    PAFAAM
     C                   PARM                    S1ISUC
     C                   PARM                    S1INCR
     C                   PARM                    S1IDEG
     C                   PARM                    S1$IMP
     C                   PARM                    WXORIG
     C                   PARM                    WXINFO
     C                   PARM                    PAIMCC
     C                   PARM                    PAISUC
     C                   PARM                    PAICCC
     C                   PARM                    PAICHE
     C                   PARM                    PAITIN
     C                   PARM                    PAININ
     c*
     c                   MoveL     WXINFO        S1DACL
     c                   Update    REBASCTM
     c*
     c     KS1001        ReadE     REBASCTM                               99
     c                   EndDo
     c*
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* *INZSR: Rutina de Inicialización
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     C*
     c     *ENTRY        PList
     c                   Parm                    PAIJOB
     C*
     c     *Like         Define    S1IJOB        PAIJOB
     C*
     C     KS1001        KList
     c                   KFld                    PAIJOB
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
