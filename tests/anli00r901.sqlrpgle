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
     FACCODI    IF   E           K DISK
     FPRHAMV03  IF   E           K DISK
     FACMOVB01  IF   E           K DISK
     FACCTAC    UF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FBC430507  IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     FACCTAC11  UF   E           K DISK    RENAME(REACCTAC:R11)
     C*-------------------------------------------------------------------------
     c*
     c*                  ExSr      LimpiaGDE
     c*
     c     KAN161        Chain     REANLIHA                           99
     c                   DoW       *In99 = *Off
     c                   ExSr      IncluirCta
     c                   If        CtaIncluida=*On
     c                   ExSr      VerFlagYaAct
     c                   If        YaAct=*Off
     c                   ExSr      VerLimites
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
     C                   If        FirstRun=*Off
     c                   LeaveSr
     c                   EndIf
     c                   Z-ADD     1             FUIMON
     C                   MOVE      '04'          FUIGRC
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
     c* ... Por defecto, ninguna cuenta esta incluida
     c                   Move      *Off          CtaIncluida       1
     c* ... Incluir si la cuenta pertenece a alguno de estos productos
     c*                  If        %subst(ANRENG:23:4) = 'SUAF' Or
     c*                            %subst(ANRENG:23:4) = 'UVHI'
     c                   Move      *On           CtaIncluida       1
     c*                  EndIf
     c* ... Si no percibe el sueldo por dep en cta... no incluir
     c                   If        ANICAH = 0
     c                   Move      *Off          CtaIncluida       1
     c                   EndIf
     c*
     c* ... Verificar en PRTISI que tenga situación 1 caso contrario no incl.
     c                   ExSr      BuscaISIT
     c                   If        WKISIT <> 1
     c                   Move      *Off          CtaIncluida       1
     c                   EndIf
     c* ... No incluir cuentas Inmovilizadas o de baja
     c     KFU000        Chain     REACCTAC                           99
     c                   If        *In99 = *On
     c                   Move      *Off          CtaIncluida       1
     c                   EndIf
     c                   If        FUISGC='IN'
     c                   Move      *Off          CtaIncluida       1
     c                   EndIf
     c                   If        FUFBAJ <> 0
     c                   Move      *Off          CtaIncluida       1
     c                   EndIf
     c* ... Solo procesar si el beneficio es del periodo actual (No diferencias)
     c                   MoveL     ANFEPR        WWAAAA            4 0
     c                   Move      WWAAAA        WWAA              2 0
     c                   Move      ANFEPR        WWMM              2 0
     c                   Move      WWAA          WWMMAA            4 0
     c                   MoveL     WWMM          WWMMAA            4 0
     c                   If        ANIPEA <> WWMMAA
     c                   Move      *Off          CtaIncluida       1
     c                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* VerLimites: Buscar que limites se aplican para el beneficio
     C*-------------------------------------------------------------------------
     c     VerLimites    BegSr
     c*
     c                   If        %subst(ANRENG:23:4) = 'SUAF' Or
     c                             %subst(ANRENG:23:4) = 'UVHI'
     c                   Z-Add     1000          LimiteAcuerdo    15 2
     c                   Z-Add     200           MinimoAcuerdo    15 2
     c                   Z-Add     0.20          PorcentajeAde     5 2
     c                   Z-Add     100           Denominacion     15 2
     c                   Else
     c                   Z-Add     2000          LimiteAcuerdo    15 2
     c                   Z-Add     200           MinimoAcuerdo    15 2
     c                   Z-Add     0.20          PorcentajeAde     5 2
     c                   Z-Add     100           Denominacion     15 2
     c                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* BuscaISIT: Busca situación del Cliente
     C*-------------------------------------------------------------------------
     c     BuscaISIT     BegSr
     c*
     C                   Z-Add     99            WKISIT
     c                   Z-Add     ANISUC        OTISUC
     c                   Z-Add     ANICAH        OTICCL
     c                   Z-Add     1             OTITTL
     c     KOT013        Chain     REBADCCL                           99
     c                   If        *IN99 = *On
     c                   LeaveSr
     c                   EndIf
     c*
     C                   Z-Add     OTITDO        AÑITDO
     C                   Z-Add     OTINDO        AÑINDO
     C     KAÑ000        Chain     REBAPFIS                           99
     C                   If        *In99 = *On
     c                   LeaveSr
     c                   EndIf
     c*
     C                   Z-Add     AÑICUI        B5INUI
     C     KB5070        Chain     REBC4305                           99
     C                   If        *In99 = *Off
     C                   Z-ADD     B5ISIT        WKISIT
     c                   EndIf
     C                   Z-ADD     1             WKISIT
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
     c* ... Solo Actualizar Acuerdo si > Minimo y si Mayor que el que hubiera
     c*     esto último para manejar el caso de mas de un beneficio sobre la
     c*     misma cuenta, se toma el acuerdo del beneficio mayor
     c                   If        Acuerdo >= MinimoAcuerdo
     c     KFU000        Chain     REACCTAC                           99
     c                   If        *In99 = *Off
     c                   If        Acuerdo > FU$GDE
     c                   Z-Add     Acuerdo       FU$GDE
     C                   Update    REACCTAC
     c                   EndIf
     c                   EndIf
     c                   EndIf
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
     c                   ExSr      SumPres
     c                   Sub       ww$pres       WW$LQA
     c                   ExSr      SumBols
     c                   Sub       ww$bols       WW$LQA
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* SumarizaPres:  Sumarizar Prestamos que tuviera en IEMP=2
     c*                No tiene sentido buscar prestamos en otras empresas
     c*                Ya que estos prestamos se cobraran con otros ingresos
     c*                Por ejemplo si es jub y emp del banco el otro prestamo
     c*                se cobrará cuando se procese la empresa 3
     C*-------------------------------------------------------------------------
     C     SumPres       BegSr
     c*
     c                   Z-Add     ANINDA        WWINDO           15 0
     c                   Z-Add     *Zero         WW$PRES          15 2
     c     KMV033        Chain     REPRHAMV                           99
     c                   DoW       *In99 = *Off
     C                   If        MVINBE = ANINBE
     c                   Add       MV$IMP        WW$PRES
     c                   EndIf
     c     KMV033        ReadE     REPRHAMV                               99
     c                   EndDo
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* SumBols:  Sumarizar Prestamos que tuviera
     c*           No sumarizamos los 268 para que los siga tomando, pero las
     c*           comisiones e IVA hay que cobrarlas por eso no las resto
     C*-------------------------------------------------------------------------
     C     SumBols       BegSr
     c*
     c                   Z-Add     *Zero         WW$BOLS          15 2
     c     KIN012        Chain     REACMOVB                           99
     c                   DoW       *In99 = *Off
     c                   If        INIMCA <> 268
     C     KFX000        Chain     REACCODI                           99
     c                   If        FXITMO=1 and INIGRC='04'
     c                   Add       IN$IMP        WW$BOLS
     c                   EndIf
     c                   EndIf
     c     KIN012        ReadE     REACMOVB                               99
     c                   EndDo
     C*
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
     c*
     c     *Like         Define    ANFEPR        PAFEPR
     c     *Like         Define    B5ISIT        WKISIT
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
     c     KMV033        KList
     c                   KFld                    WWFAAM
     c                   KFld                    WWIEMP
     c                   KFld                    WWINDO
     c     KIN012        KList
     c                   KFld                    ANISUC
     c                   KFld                    ANICAH
     c     KFX000        KList
     c                   KFld                    INIMCA
     c     KOT013        KList
     c                   KFld                    OTISUC
     c                   KFld                    OTICCL
     c                   KFld                    OTITTL
     c     KAÑ000        KList
     c                   KFld                    AÑITDO
     c                   KFld                    AÑINDO
     c     KB5070        KList
     c                   KFld                    B5INUI
     C* ... Buscar Ultimo periodo vigente en anliha
     c/Exec Sql
     c+                  SELECT
     C+                            MAX(ANFEPR)
     C+                  INTO      :PAFEPR
     C+                  FROM      ANLIHA
     c/End-Exec
     C*
     C* ... Ver si es la primera corrida para el periodo
     c                   Move      *Blank        WWFLAG            1
     c/Exec Sql
     C+   SELECT
     C+       MAX(SUBSTR(ANRENG, 12, 1))
     C+       INTO :WWFLAG
     C+   FROM
     C+       ANLIHA
     C+   WHERE
     C+        SUBSTR(ANRENG, 23, 4) in ('SUAF', 'UVHI')
     C+    AND ANFEPR = :PAFEPR
     c/End-Exec
     c*
     C                   Move      *On           FirstRun          1
     C                   If        WWFLAG = '*'
     C                   Move      *Off          FirstRun
     c                   EndIf
     C*
     c*
     C* ... Buscar Ultimo periodo vigente para la empresa 2
     c                   Z-Add     2             WWIEMP            5 0
     c                   Z-Add     *Zero         WWFAAM            6 0
     c/Exec Sql
     c+                  SELECT
     C+                            MAX(MVFAAM)
     C+                  INTO      :WWFAAM
     C+                  FROM      PRHAMV
     C+                  WHERE
     C+                            MVIEMP=2
     c/End-Exec
     C*
     C                   EndSr
