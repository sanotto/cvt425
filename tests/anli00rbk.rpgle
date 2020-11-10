*SRCMBRTXT:LINK: Calculo de Giro en Desc P/CAH LIN
     H
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: ANLI00R9                                       *
     H*                                                               *
     H*  PROGRAM NO: Genera Giro en Desc. en C. Ah.                   *
     H*                                                               *
     H*  DATE: 09/08/2013                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*----------------------------------------------------------------
     FANLIHA16  UF   E           K DISK
     FACMOVB07  IF   E           K DISK
     FACCODI    IF   E           K DISK
     FACCTAC    UF   E           K DISK
     FACCTAC11  UF   E           K DISK    RENAME(REACCTAC:R11)
     C*-------------------------------------------------------------------------
     c                   Z-Add     1000          LimiteAcuerdo    15 2
     c                   Z-Add     0.20          PorcentajeAde     5 2
     c                   Z-Add     100           Denominacion     15 2
     c*
     c                   ExSr      LimpiaGDE
     c*
     c     KAN161        Chain     REANLIHA                           99
     c                   DoW       *In99 = *Off
     c                   ExSr      IncluirCta
     c                   If        CtaIncluida=*On
     c                   ExSr      VerFlagYaAct
     c                   If        YaAct=*Off
     c                   ExSr      Calc$GDE
     c                   ExSr      ActAnliha
     c                   EndIf
     c                   EndIf
     C     KAN161        ReadE     REANLIHA                               99
     C                   EndDo
     c                   ExSr      Endpgm
     C*-------------------------------------------------------------------------
     c* LimpiaGDE: Limpia los adelantos que hubiere
     C*-------------------------------------------------------------------------
     c     LimpiaGDE     BegSr
     c*
     c                   Z-ADD     1             FUIMON
     C                   MOVE      '04'          FUIGRC
     C                   MOVE      'AN'          FUISGC
     C*
     c     KFU113        Chain     R11                                99
     C                   DoW       *In99 = *Off
     c                   Z-Add     *Zero         FU$GDE
     c                   Update    R11
     c     KFU113        ReadE     R11                                    99
     c                   EndDo
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* VerFlagYaAct: Verifica si este beneficio ya actualizó FU$GDE
     C*-------------------------------------------------------------------------
     c     VerFlagYaAct  BegSr
     c*
     c                   Move      *Off          YaAct             1
     c                   If        %subst(ANRENG:12:1)='*'
     c                   Move      *On           YaAct             1
     c                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* ActAnliha: Marca en Anliha que ya actualizó FU$GDE
     C*-------------------------------------------------------------------------
     c     ActAnliha     BegSr
     c*
     c                   Move      *Off          YaAct             1
     c                   Eval      %subst(ANRENG:12:1)='*'
     c                   Update    REANLIHA
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* IncluirCta: Verificar si este beneficio debe calcular $GDE
     C*-------------------------------------------------------------------------
     c     IncluirCta    BegSr
     c*
     c                   Move      *On           CtaIncluida       1
     c*
     c* ... Si no percibe el sueldo por dep en cta... no incluir
     c                   If        ANICAH = 0
     c                   Move      *Off          CtaIncluida       1
     c                   LeaveSr
     c                   EndIf
     c* ... Si renglon <> P00 No incluir
     c                   If        %subst(ANRENG:19:3) <> 'P00'
     c                   Move      *Off          CtaIncluida       1
     c                   LeaveSr
     c                   EndIf
     c* ... Si es distinto de UVHI, no incluir ...
     c                   If        ANIECJ <> 77
     c                   Move      *Off          CtaIncluida       1
     c                   LeaveSr
     c                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* Calc$GDE: Calcula el Importe a poner en Giro en Descubierto CAH
     C*-------------------------------------------------------------------------
     c     Calc$GDE      BegSr
     c*
     c*
     c                   Exsr      GetImpNoIncl
     C                   Z-Add     WW$LQA        Base             15 2
     c*
     c                   ExSr      CalcDebPend
     c                   Sub       ImpPen        Base
     c*
     C                   If        Base > 0
     c     Base          Mult      PorcentajeAde Acuerdo          15 2
     c                   If        Acuerdo > LimiteAcuerdo
     c                   Z-Add     LimiteAcuerdo Acuerdo
     c                   EndIf
     c*
     c     Acuerdo       Div       Denominacion  Billetes         15 0
     c     Billetes      Mult      Denominacion  Acuerdo
     c                   Else
     C                   Z-Add     *ZERO         Acuerdo
     c                   EndIf
     c*
     c     KFU000        Chain     REACCTAC                           99
     c                   Z-Add     Acuerdo       FU$GDE
     C                   Update    REACCTAC
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* GetImpNoIncl: Obtiene Importes que no se deben considerar p/calculo
     C*-------------------------------------------------------------------------
     C     GetImpNoIncl  BegSr
     C                   Z-Add     *ZEROS        WW$AGU           10 2
     C                   Z-Add     *ZEROS        WW$PRE           10 2
     C                   Z-ADD     *ZEROS        WW$LQA           10 2
     C                   Call      'BASC04GD'
     C                   Parm                    ANCUIA
     C                   Parm                    ANIECJ
     C                   Parm                    ANITBE
     C                   Parm                    ANINBE
     C                   Parm                    ANIICP
     C                   Parm                    ANIDVA
     C                   Parm                    ANIPEA
     C                   Parm                    ANFEPR
     C                   Parm                    WW$AGU
     C                   Parm                    WW$PRE
     c*
     C                   Z-ADD     AN$LQ2        WW$LQA
     C                   SUB       WW$AGU        WW$LQA
     C                   SUB       WW$PRE        WW$LQA
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* CalcDebPend: Sumarizar Debitos Pendientes en Bolsa
     C*-------------------------------------------------------------------------
     C     CalcDebPend   BegSr
     C*
     c                   Z-Add     *ZERO         ImpPen           15 2
     C     KIN072        Chain     REACMOVB                           99
     C                   DoW       *In99 = *Off
     c     INIMCA        Chain     REACCODI                           99
     c                   If        FXITMO=1
     c                             and       INIMCA<>268
     c                             and       INIMCA<>57
     c                   Add       IN$IMP        ImpPen
     c                   EndIf
     C     KIN072        ReadE     REACMOVB                               99
     c                   EndDo
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* EndPgm: Finalizar el Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR : Subrutina de inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *Entry        PList
     c                   Parm                    PAFEPR
     c*
     c     *Like         Define    ANFEPR        PAFEPR
     c*
     c     KAN161        KList
     c                   KFld                    PAFEPR
     c     KIN072        KList
     c                   KFld                    ANISUC
     c                   KFld                    ANICAH
     c     KFU000        KList
     c                   KFld                    ANISUC
     c                   KFld                    ANICAH
     c     KFU113        KList
     c                   KFld                    FUIMON
     c                   KFld                    FUIGRC
     c                   KFld                    FUISGC
     C*
     C                   EndSr
