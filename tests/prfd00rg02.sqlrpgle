*SRCMBRTXT:Aut.Federal-Inserta pedido de Aut. en P
     H DEBUG
     H DECEDIT(',') DATEDIT(*DMY/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: - Inserta Pedido de Autorización en PRAFED     *
     H*                - Si es x Fuera de Scoring =>                  *
     H*                    - Solicita Recibo                          *
     H*                    - Genera Minuta                            *
     H*                - Salva Minuta en PRMFED                       *
     H*                - Salva Movs Diarios de Prestamos en PRmovi60  *
     H*                    - Salva Movs de Originación                *
     H*                    - Salva Movs de Cancelación Prest. Ref.    *
     H*                    - Salva Movs de Ctas Vencidas              *
     H*                                                               *
     H*  PROGRAM NO: PRFD00MA                                         *
     H*                                                               *
     H*  DATE:    14/06/2012                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*****************************************************************
     FBASCOR03  IF   E           K DISK
     FBASCOR04  IF   E           K DISK    RENAME(REBASCOR:R4)
     FSGSYSV    IF   E             DISK
     FSGUSUA    IF   E           K DISK
     FPRHATB    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FBASCAR01  IF   E           K DISK
     FPRMOVI09  UF   E           K DISK
     FBASCT1    IF   E           K DISK
     FPRCRED    UF   E           K DISK
     FPRMFED    UF A E           K DISK
     FPRMOVI60  UF A E           K DISK
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     FPRAFED01  IF A E           K DISK
     FPRAFED02  IF A E           K DISK    RENAME(REPRAFED:R2)
     FBANUME    UF A E           K DISK
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     DPrFmtABAN        DS
     D dsindi                  1     20
     D dsisuc                  1      2
     D dsincr                  4     18
     D dsideg                 20     20
     D*
     DPrFmtFed         DS
     D dfindi                  1     14
     D dfisuc                  1      2
     D dfincr                  3     12
     D dfideg                 13     14
     D*----------------------------------------------------------------*
     DDSFEC            DS
     D DSFECH                  1      8  0
     D DSPER                   1      6  0
     D DSANO                   1      4  0
     D DSMES                   5      6  0
     D DSDIA                   7      8  0
     D*----------------------------------------------------------------*
     DERRDS            DS
     D ERRTXT                  1    385
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*----------------------------------------------------------------*
     c                   ExSr      ReadParams
     c* ... Si no es línea 5 u 8 (Gobierno), salir
     C     KJV           Chain(N)  REPRCRED                           25
     c                   If        *IN25 = *Off  and
     c                             (JVILCR = 5 or JVILCR=8 or
     c                              JVILCR = 1 or JVILCR=10 or JVILCR=16  )
     c* ... Verifica que la operación no este ya en PRAFED
     c                   Move      *Off          WWERRO            1
     c                   ExSr      CheckExiste
     C                   IF        WWERRO = *ON
     C                   ExSr      EndPgm
     C                   Endif
     c* ... Verifica si el area es controlada por Federal, sino salir
     c                   Move      *Off          WWERRO            1
     c                   ExSr      CheckArea
     C                   IF        WWERRO = *ON
     C                   ExSr      EndPgm
     C                   Endif
     c                   ExSr      CheckScoring
     C                   If        EsPorScoring = *Off
     c     SelRec        Tag
     c                   ExSr      SelecRecibo
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Z-Add     WWISUC        @CISUC
     c                   Z-Add     WWINCR        @CINCR
     c                   Z-Add     WWIDEG        @CIDEG
     C                   Update    @CPIUSRR
     c                   If        JVILCR = 1
     C                   ExSr      MinPcParaTodos
     c                   Else
     c                   ExSr      GenerarMinuta
     C                   EndIf
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Z-Add     WWISUC        @CISUC
     c                   Z-Add     WWINCR        @CINCR
     c                   Z-Add     WWIDEG        @CIDEG
     C                   Update    @CPIUSRR
     c                   If        @FN(12) = *On
     c                   Goto      SelRec
     C                   EndIf
     c                   Else
     c                   ExSr      RecuperarDatos
     C                   EndIf
     c*
     c                   ExSr      BloquearPrest
     c                   ExSr      InsBajas
     c                   ExSr      InsAlta
     c                   ExSr      SalvarMinuta
     c                   ExSr      SalvarMovsD
     c                   Eval      WWNCB4='OP. SUJETA A APROBACION DE FEDERAL'
     c                   ExSr      DspErr
     C                   EndIf
     c                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     c* MinPCParaTodos: Genera Minuta para PC para todos
     c*-------------------------------------------------------------------------
     c     MinPCParaTodosBegSr
     c*
     c                   Z-Add     @CISUC        PAISUC            5 0
     c                   Z-Add     @CINCR        PAINCR           15 0
     c                   Z-Add     @CIDEG        PAIDEG            4 0
     c                   Z-Add     JVICCL        PAICCL           11 0
     c                   Z-Add     JV$INP        PA$INP           15 2
     c                   Call      'PRFD11RG'
     c                   Parm                    PAISUC
     c                   Parm                    PAINCR
     c                   Parm                    PAIDEG
     c                   Parm                    PAICCL
     c*
     c                   Call      'BASC02TE'
     c                   ExSr      ReadParams
     c                   Call      'BASC00C8'
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* CheckArea: Chequea si el area esta autorizada a Utilizar Federal
     c*-------------------------------------------------------------------------
     c     CheckArea     BegSr
     C*
     c     KTB000        Chain     REPRHATB                           99
     c                   If        *IN99 = *On  or  TBDF03<>'F'
     c                   Move      *On           WWERRO            1
     c                   EndIf
     C*
     C                   EndSr
     c*-------------------------------------------------------------------------
     c* CheckExiste:Chequea si el prestamo ya esta en PRAFED
     c*-------------------------------------------------------------------------
     c     CheckExiste   BegSr
     C*
     C     KJV           CHAIN     R2                                 99
     C                   DoW       *IN99=*Off
     C                   If        AFIOPT='A' and affech=aasfei
     c                   Move      *On           WWERRO            1
     c                   Eval      WWNCB1='OP. EN ESPERA DE APR. DE FEDERAL'
     c                   Eval      WWNCB2='SI DESEA MODIFICAR LA MISMA DEBE'
     c                   Eval      WWNCB3='DAR DE BAJA LA OPERACION Y DARLE'
     c                   Eval      WWNCB4='EL ALTA NUEVAMENTE.             '
     c                   ExSr      DspErr
     c                   Leave
     c                   EndIf
     C     KJV           ReadE     R2                                     99
     c                   EndDo
     C*
     C                   EndSr
     c*-------------------------------------------------------------------------
     c* GenerarMinuta: Generar Minuta si cred. es por fuera Scoring
     c*-------------------------------------------------------------------------
     c     GenerarMinuta BegSr
     c* ... Parámetros INDO, Op. Refinanciada, cuenta cliente
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Z-Add     OTINDO        @CINDO
     c                   Z-Add     JVICCL        @CICCL
     c                   Z-Add     @CISUC        WKISUC
     c                   Z-Add     @CINCR        WKINCR
     c                   Z-Add     @CIDEG        WKIDEG
     c                   Z-Add     WWEDSU        @CSUCA
     c                   Z-Add     WWINCV        @CINCR
     c                   Z-Add     WWIDEV        @CIDEG
     c                   Update    @CPIUSRR
     c*
     C     JVILCR        IFNE      16
     C                   Call      'BASC00R3'
     C                   ENDIF
     c                   Call      'BASC02TE'
     c                   ExSr      ReadParams
     C*
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Z-Add     WKISUC        @CISUC
     c                   Z-Add     WKINCR        @CINCR
     c                   Z-Add     WKIDEG        @CIDEG
     c                   Update    @CPIUSRR
     c*
     c                   Call      'BASC00C8'
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* RecuperarDatos: Si el credito es por scoring poblar las var ww* p/Insert
     c*-------------------------------------------------------------------------
     c     RecuperarDatosBegSr
     C*
     C                   MOVEL     @CREFC        AXINDO            8 0
     C                   Z-Add     AXINDO        AUINDO           15 0
     C     KSC010        Chain     REBASCOR                           99
     C                   IF        *In99 = *Off
     c                   Move      @CREFC        WWREFC
     C*
     c                   Move      SCISEX        WWISEX
     c                   Move      SCINDO        WWINDO
     c                   Move      SCNYAP        WWNYAP
     c                   Move      SCIRED        WWIRED
     c                   Move      SCIBCF        WWIBCF
     C*
     c                   MoveL     *ZEROS        DSINDI
     c                   MoveL     @CABAN        DSINDI
     c                   Move      DSISUC        WWEDSU
     c                   Move      DSINCR        WWINCV
     c                   Move      DSIDEG        WWIDEV
     C*
     c                   EndIf
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* SelecRecibo:Pedir datos al usuario si el credito es x fuera de Scoring
     c*------------------------------------------------------------------------
     c     SelecRecibo   BegSr
     C*
     c                   Z-Add     @CISUC        WWISUC
     c                   Z-Add     @CINCR        WWINCR
     c                   Z-Add     @CIDEG        WWIDEG
     c*
     c     KOT013        Chain     REBADCCL                           99
     C                   If        *IN99 = *On
     c                   CALL      'BASC00RS'
     c                   Else
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Move      OTINDO        @CINDO
     C                   Update    @CPIUSRR
     C                   EndIf
     C*
     c     Again         Tag
     c                   CALL      'BASC10TE'
     c                   ExSr      ReadParams
     c                   If        @FN(03) = *On or  @FN(12) = *on
     c                   Eval      ErrTxt=*Blanks
     c                   Eval      WWNCB1='ELIGIO CANCELAR LA OPERACION      '
     c                   Eval      WWNCB2='NO SERA ENVIADA A FEDERAL         '
     c                   Eval      WWNCB4='F3=CONFIRMA F12=VOLVER A SEL.REC  '
     c                   ExSr      DspErr
     c                   ExSr      ReadParams
     c                   If        @FN(03) = *On
     c                   ExSr      EliminaCred
     c                   ExSr      Abort
     c                   EndIf
     c                   GoTo      Again
     C                   EndIf
     c                   ExSr      RecuperarDatos
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EliminaCred: Elimina Operación de PRCRED si fue rechazada
     c*---------------------------------------------------------------------
     c     EliminaCred   BegSr
     C*
     c     KJV           Chain     REPRCRED                           99
     c  N99              Delete    REPRCRED
     c                   Call      'PRJV03SB'
     C*
     c                   EndSr
     c*------------------------------------------------------------------------
     c* CheckScoring: Chequea si el credito se esta dando por Scoring
     c*------------------------------------------------------------------------
     c     CheckScoring  BegSr
     c* ... Dado Por Fuera de Scoring 0 o Por Scoring 1
     c                   Move      *Off          EsPorScoring      1
     C                   MOVEL     @CREFC        AXINDO            8 0
     C                   Z-Add     AXINDO        AUINDO           15 0
     C     KSC010        Chain     REBASCOR                           99
     C                   IF        *In99 = *OFF
     c                   Move      *On           EsPorScoring
     C                   EndIf
     c*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* SalvarMovsD : Salvar Movs Diarios de Prestamos
     c*------------------------------------------------------------------------
     c     SalvarMovsD   BegSr
     C*
     c* ... Salvar Movs del Prestamo Liquidado
     C                   Z-Add     @CISUC        WWISUC
     C                   Z-Add     @CINCR        WWINCR
     C                   Z-Add     @CIDEG        WWIDEG
     c                   ExSr      SalvarMovsPR
     C*
     c* ... Salvar Movs del Prestamo Refinanciado
     c     KMC010        Chain     REPRMFED                           99
     C                   DoW       *In99 = *Off
     c                   If        JVILCR <> 16
     c                   If        MCIAPC=20 AND MCITRN=4
     C                   Z-Add     MCEDSU        WWISUC
     C                   Z-Add     MCINCV        WWINCR
     C                   Z-Add     MCIDEV        WWIDEG
     c                   ExSr      SalvarMovsPR
     C                   EndIf
     C                   EndIf
     c     KMC010        ReadE     REPRMFED                               99
     C                   EndDo
     C*
     c* ... Salvar Movs del Cuotas Atrasadas
     c     KMC010        Chain     REPRMFED                           99
     C                   DoW       *In99 = *Off
     c                   If        JVILCR <> 16
     c                   If        MCIAPC=20 AND MCITRN=2
     c                   ExSr      SalvarMovsCT
     C                   EndIf
     C                   EndIf
     c     KMC010        ReadE     REPRMFED                               99
     C                   EndDo
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* SalvarMovsCT: Salvar Movs de Prestamos por cuota
     c*------------------------------------------------------------------------
     c     SalvarMovsCT  BegSr
     C*
     C                   Move      MCISUC        WWISUC
     C                   Move      MCINCR        WWINCR
     C                   Move      MCIDEG        WWIDEG
     c                   ExSr      BorrarMovsD
     c     KMI010        Chain     REPRMOVI                           99
     C                   DoW       *IN99 = *Off
     c                   If        MIICUO=MCICUO
     C                   Z-ADD     MIICUO        M2ICUO
     C                   Z-ADD     MIISUC        M2ISUC
     C                   Z-ADD     MIINCR        M2INCR
     C                   Z-ADD     MIIDEG        M2IDEG
     C                   Z-ADD     MIIPOS        M2IPOS
     C                   Z-ADD     MIIMPR        M2IMPR
     C                   Z-ADD     AASFEI        M2FASI
     C                   Z-ADD     MI$IMP        M2$IMP
     C                   Z-ADD     MIIMON        M2IMON
     C                   Move      MIIASK        M2IASK
     C                   Write     REPRMO60
     c                   EndIf
     c*
     c     KMI010        ReadE     REPRMOVI                               99
     c                   EndDo
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* SalvarMovsPR: Salvar Movs de Prestamos
     c*------------------------------------------------------------------------
     c     SalvarMovsPR  BegSr
     C*
     c                   ExSr      BorrarMovsD
     c     KMI010        Chain     REPRMOVI                           99
     C                   DoW       *IN99 = *Off
     C                   Z-ADD     MIICUO        M2ICUO
     C                   Z-ADD     MIISUC        M2ISUC
     C                   Z-ADD     MIINCR        M2INCR
     C                   Z-ADD     MIIDEG        M2IDEG
     C                   Z-ADD     MIIPOS        M2IPOS
     C                   Z-ADD     MIIMPR        M2IMPR
     C                   Z-ADD     AASFEI        M2FASI
     C                   Z-ADD     MI$IMP        M2$IMP
     C                   Z-ADD     MIIMON        M2IMON
     C                   Move      MIIASK        M2IASK
     C                   Write     REPRMO60
     c*
     c     KMI010        ReadE     REPRMOVI                               99
     c                   EndDo
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* BorrarMovsD:
     c*------------------------------------------------------------------------
     c     BorrarMovsD   BegSr
     C*
     c     KMI010        Chain     REPRMO60                           99
     C                   DoW       *IN99 = *Off
     C                   Delete    REPRMO60
     c     KMI010        ReadE     REPRMO60                               99
     c                   EndDo
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* SalvarMinuta:
     c*------------------------------------------------------------------------
     c     SalvarMinuta  BegSr
     c*
     c     KMC010        Chain     REPRMFED                           99
     C                   DoW       *In99 = *Off
     c                   Delete    REPRMFED
     c     KMC010        ReadE     REPRMFED                               99
     C                   EndDo
     C*
||   C                   Z-ADD     *ZEROS        WW$INE           15 2
     c     KMI010        Chain     REPRMOVI                           99
     C                   DoW       *In99 = *Off
|+---C     MIIASK        IFEQ      '1'
||   C                   ADD       MI$IMP        WW$INE
|+---C                   ENDIF
|+---C     MIIASK        IFEQ      '2'
||   C                   SUB       MI$IMP        WW$INE
|+---C                   ENDIF
     c     KMI010        ReadE     REPRMOVI                               99
     C                   EndDo
     C                   Z-ADD     *ZEROS        WW$DEB           15 2
     c     KS2011        Chain     REBASCT1                           99
     C                   DoW       *In99 = *Off
     C                   If        JVILCR <> 16
     C                   If        S2ILCR <> *ZERO
     C                   EVAL      MCFECH=AASFEI
     C                   EVAL      MCISUC=JVISUC
     C                   EVAL      MCINCR=JVINCR
     C                   EVAL      MCIDEG=JVIDEG
     C                   EVAL      MCIAPC=S2ILCR
     C                   EVAL      MCITRN=S2IMON
     C                   EVAL      MCICCL=S2$A03
     C                   EVAL      MC$IMP=S2$INP
     C                   EVAL      MCFAAM=S2$A04
     C                   EVAL      MCEDSU=S2ISUC
     C                   EVAL      MCINCV=S2INCR
     C                   EVAL      MCIDEV=S2IDEG
     C                   EVAL      MCICUO=S2QCUO
     c                   Write     REPRMFED
     C                   Z-ADD     S2ILCR        WWILCR            4 0
     C                   EndIf
     C                   If        S2ILCR = 2
     C                   ADD       S2$INP        WW$DEB
     C                   EndIf
     C                   EndIf
     c     KS2011        ReadE     REBASCT1                               99
     C                   EndDo
     C                   If        JVILCR = 16
     C*  Le Sacamos la plata al deudor para luego acreditarla en la SAPEM
     C                   EVAL      MCFECH=AASFEI
     C                   EVAL      MCISUC=JVISUC
     C                   EVAL      MCINCR=JVINCR
     C                   EVAL      MCIDEG=JVIDEG
     C                   EVAL      MCIAPC=2
     C                   EVAL      MCITRN=20
     C                   EVAL      MCEDSU=JVISUC
     C                   EVAL      MCICCL=JVICCL
     C     WW$INE        SUB       WW$DEB        MC$IMP
     C                   EVAL      MCFAAM=0
     C                   EVAL      MCINCV=0
     C                   EVAL      MCIDEV=0
     C                   EVAL      MCICUO=0
     c                   Write     REPRMFED
     C*  Acreditación al comercio LEDLAR S.A.P.E.M --- CC 3100009/8 Suc 5
     C                   EVAL      MCFECH=AASFEI
     C                   EVAL      MCISUC=JVISUC
     C                   EVAL      MCINCR=JVINCR
     C                   EVAL      MCIDEG=JVIDEG
     C                   EVAL      MCIAPC=1
     C                   EVAL      MCITRN=210
     C                   EVAL      MCEDSU=5
     C                   EVAL      MCICCL=31000098
     C     WW$INE        SUB       WW$DEB        MC$IMP
     C                   EVAL      MCFAAM=0
     C                   EVAL      MCINCV=0
     C                   EVAL      MCIDEV=0
     C                   EVAL      MCICUO=0
     c                   Write     REPRMFED
     C                   EndIf
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* BloquearPrest: Poner un '3' en JVIESA para que el PRESLO no lo tome
     c*------------------------------------------------------------------------
     c     BloquearPrest BegSr
     C*
     c     KJV           Chain     REPRCRED                           25
     c                   Move      '3'           JVIESA
     c                   Move      WWIECT        JVIECT
     c                   Update    REPRCRED
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* InsBajas : Insertar Pedido de Bajas (Cred. Refinanciados)
     c*---------------------------------------------------------------------
     c     InsBajas      BegSr
     C*
     c                   If        JVILCR<> 16
     c     KS2011        SetGt     REBASCT1
     c     KS2011        ReadPe    REBASCT1                               99
     C                   DoW       *In99 = *Off
     c                   If        S2ILCR=20 and S2IMON=04
     C                   ExSr      BusRegAlta
     c                   If        TieneAlta=*On
     C                   ExSr      WrtRecBaja
     C                   EndIf
     C                   EndIf
     c     KS2011        ReadPe    REBASCT1                               99
     C                   EndDo
     C                   EndIf
     C*
     C                   EndSr
     c*-------------------------------------------------------------------------
     c* WrtRecBaja: Escribe el registro de Baja
     c*-------------------------------------------------------------------------
     c     WrtRecBaja    BegSr
     c*
     C     KSC040        CHAIN     R4                                 99
     C     KSC040        Chain(N)  REPRCRED                           99
     c                   ExSr      GetLastNo
     c                   Z-Add     WWIRRN        AFIRRN
     c                   Z-Add     AASFEI        AFFECH
     c                   Z-Add     *ZERO         AFFACR
     C                   Move      *ZERO         AFICAR
     c                   Z-Add     WWISEQ        AFISEQ
     c                   Z-Add     JV$INP        AF$IMP
     c                   Z-Add     JVQCUO        AFQCUO
     c                   Move      'B'           AFIOPT
     c                   Move      WWISEX        AFISEX
     c                   Move      WWINDO        AFINDO
     c                   Move      WWNYAP        AFNYAP
     c                   Move      WWIRED        AFIRED
     c                   Move      WWIBCF        AFIBCF
     c                   Move      S2ISUC        AFISUC
     c                   Move      S2INCR        AFINCR
     c                   Move      S2IDEG        AFIDEG
     c*
     c                   Move      @CISUC        AFEDSU
     c                   Move      @CINCR        AFINCV
     c                   Move      @CIDEG        AFIDEV
     c*
     c                   Move      SCREFC        AFREFC
     C*
     c                   Move      S2ISUC        dfisuc
     c                   Move      S2INCR        dfincr
     c                   Move      S2IDEG        dfideg
     c                   Move      DfINDI        AFIND1
     C*
     c                   Move      @CISUC        dfisuc
     c                   Move      @CINCR        dfincr
     c                   Move      @CIDEG        dfideg
     c                   Move      DfINDI        AFIND2
     c                   Z-Add     BAIMPO        AF$CUO
     C*
     C                   Move      *BLANKS       AFESTA
     c                   Move      *BLANKS       AFAMRC
     c                   Z-Add     *ZERO         AFFE01
     c                   Z-Add     *ZERO         AF$SRD
     c                   MoveL(P)  @PUSER        AFDF03
     C*
     c                   Z-Add     *ZERO         AFFBAJ
     c                   Z-Add     *ZERO         AFHBAJ
     c                   Move      *BLANKS       AFIUSB
     C*
     C                   Write     REPRAFED
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* BusRegAlta: Buscar el Reg. en el cual se dio de alta la operacion
     c*---------------------------------------------------------------------
     c     BusRegAlta    BegSr
     c*
     c                   Move      *Off          TieneAlta         1
     C                   Z-ADD     *ZERO         BAIMPO           15 2
     C     KSC040        SETGT     R2
     C     KSC040        READPE    R2                                     99
     C                   DoW       *IN99=*Off
     c                   IF        AFIOPT='A' AND AFESTA='A'
     C                   Z-ADD     AF$CUO        BAIMPO           15 2
     c                   Move      *On           TieneAlta         1
     c                   Leave
     c                   EndIf
     C     KSC040        READPE    R2                                     99
     c                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* InsAlta  : Insertar Pedido de Autorización a Federal
     c*---------------------------------------------------------------------
     c     InsAlta       BegSr
     c*
     c                   clear                   REPRAFED
     C*
     C     KJV           Chain(N)  REPRCRED                           25
     C*
     c                   ExSr      GetLastNo
     c                   Z-Add     WWIRRN        AFIRRN
     c                   Z-Add     AASFEI        AFFECH
     c* ... Dado Por Fuera de Scoring 0 o Por Scoring 1
     C*     - BASC00MA y M1 Ponen @CIPF1 en 'EMISION' o 'CONSULTA' si esta
     C*       Presente en cpi => se Emitio por Scoring
     C                   Move      *ZERO         AFICAR
     c                   Z-Add     WWISEQ        AFISEQ
     c                   Z-Add     JV$INP        AF$IMP
     c                   Z-Add     JVQCUO        AFQCUO
     c                   ExSr      SRIMDS
     c                   Move      WWIMDS        AFIMDS
     c                   Move      'A'           AFIOPT
     c                   Move      WWISEX        AFISEX
     c                   Move      WWINDO        AFINDO
     c                   Move      WWNYAP        AFNYAP
     c                   Move      WWIRED        AFIRED
     c                   Move      WWIBCF        AFIBCF
     c                   Move      @CISUC        AFISUC
     c                   Move      @CINCR        AFINCR
     c                   Move      @CIDEG        AFIDEG
     c                   Move      *ZERO         AFEDSU
     c                   Move      *ZERO         AFINCV
     c                   Move      *ZERO         AFIDEV
     c                   Move      WWREFC        AFREFC
     C*
     c                   Move      @CISUC        dfisuc
     c                   Move      @CINCR        dfincr
     c                   Move      @CIDEG        dfideg
     c                   Move      DfINDI        AFIND1
     c                   Move      *BLANKS       AFIND2
     C*
     C                   ExSr      SR$CUO
     c                   Z-Add     WW$CUO        AF$CUO
     C                   Move      *BLANKS       AFFAU1
     C                   Move      *BLANKS       AFHAU1
     C                   Move      *BLANKS       AFESTA
     c                   Move      *BLANKS       AFAMRC
     c                   Z-Add     *ZERO         AFFE01
     c                   Z-Add     *ZERO         AF$SRD
     c                   MoveL(P)  @PUSER        AFDF03
     C*
     c                   Z-Add     *ZERO         AFFBAJ
     c                   Z-Add     *ZERO         AFHBAJ
     c                   Move      *BLANKS       AFIUSB
     C*
     C                   Write     REPRAFED
     C*
     C                   EndSr
     C*----------------------------------------------------------------
     C* SR$CUO - CALCULA IMPORTE CUOTA 1
     C*----------------------------------------------------------------
     C     SR$CUO        BEGSR
     c                   Z-Add     *ZERO         WW$IMP           15 2
     c                   Z-Add     AFISUC        WWISUC            5 0
     c                   Z-Add     AFINCR        WWINCR           15 0
     c                   Z-Add     AFIDEG        WWIDEG            4 0
     C                   Z-ADD     1             WWICUO            3 0
     C                   Z-ADD     *ZEROS        PASFEI            8 0
     C                   MOVE      JVFE01        DSFECH
     C                   IF        DSMES > 12
     C                   Z-ADD     1             DSMES
     C                   ADD       1             DSANO
     C                   ENDIF
     C                   MOVE      10            DSDIA
     C                   MOVE      DSFECH        PASFEI            8 0
     C* ... Llamo a creador de cupón para cuota 1
     C                   CALL      'PRHA01SC'
     C                   PARM                    DSPER
     C                   PARM                    WWISUC
     C                   PARM                    WWINCR
     C                   PARM                    WWIDEG
     C                   PARM                    WWICUO
     C                   PARM                    PASFEI
     C
     c* ... Sumarizo códigos
||   C                   Z-ADD     *ZERO         WW$CUO           15 2
     c     KMI010        Chain     REPRMOVI                           99
     C                   DoW       *IN99 = *Off
     c                   If        MIICUO=1
|+---C     MIIASK        IFEQ      '1'
||   C                   ADD       MI$IMP        WW$CUO
|+---C                   ENDIF
|+---C     MIIASK        IFEQ      '2'
||   C                   SUB       MI$IMP        WW$CUO
|+---C                   ENDIF
     c                   EndIf
     C* ... Para no dejar Basura
     c                   If        MIICUO=1
     c                   Delete    REPRMOVI
     c                   EndIf
     c     KMI010        ReadE     REPRMOVI                               99
     C                   EndDo
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* SRIMDS - BUSCA CODIGO DE DECTO A COBRAR  (SOLO PARA GOBIERNO)
     C*----------------------------------------------------------------
     C     SRIMDS        BEGSR
     C* ... Por defecto codigo de capital, esto es por si no se act BASCAR
     C                   Z-ADD     570           WWIMDS
     C     WWIBCF        CHAIN     BASCAR01                           99
+----C                   IF        *In99=*Off
|+---C                   IF        ARIOPT = 'M'
     C                   Z-ADD     571           WWIMDS
|+---C                   ENDIF
  +--C                   ENDIF
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Move      WWIFUN        @CIFUN
     C                   Update    @CPIUSRR
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* Abort : Abortar Programa (Volver con 1 en Parámetro de Error)
     c*---------------------------------------------------------------------
     c     Abort         BegSr
     C*
     c                   Move      *On           PAERRO
     c                   ExSr      EndPgm
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* *INZSR: Rutina de Inicialización
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     C*
     c     *ENTRY        Plist
     c                   Parm                    PAERRO            1
     c                   Parm                    PABATC            1
     C*
     C                   Move      *OFF          PAERRO
     C*
     c     1             Chain     SGSYSV                             99
     C*
     C     *Like         Define    AFIRRN        WWIRRN
     C     *Like         Define    AFISEQ        WWISEQ
     c     *Like         Define    AFIMDS        WWIMDS
     c     *Like         Define    AFISEX        WWISEX
     c     *Like         Define    AFINDO        WWINDO
     c     *Like         Define    AFNYAP        WWNYAP
     c     *Like         Define    AFIRED        WWIRED
     c     *Like         Define    AFIBCF        WWIBCF
     c     *Like         Define    AFEDSU        WWEDSU
     c     *Like         Define    AFINCV        WWINCV
     c     *Like         Define    AFIDEV        WWIDEV
     c     *Like         Define    AFREFC        WWREFC
     c     *Like         Define    AFIND1        WWIND1
     c     *Like         Define    AFIND2        WWIND2
     c     *Like         Define    AFISUC        WWISUC
     c     *Like         Define    AFINCR        WWINCR
     c     *Like         Define    AFIDEG        WWIDEG
     c     *Like         Define    @CIFUN        WWIFUN
     c     *Like         Define    JVIECT        WWIECT
     c     *Like         Define    AFISUC        WKISUC
     c     *Like         Define    AFINCR        WKINCR
     c     *Like         Define    AFIDEG        WKIDEG
     C*
     C     KJV           KList
     C                   KFld                    @CISUC
     C                   KFld                    @CINCR
     C                   KFld                    @CIDEG
     C     KSC010        KList
     C                   KFld                    AUINDO
     C                   KFld                    @CIEMP
     C                   KFld                    @CREFC
     C     KSC040        KList
     C                   KFld                    S2ISUC
     C                   KFld                    S2INCR
     C                   KFld                    S2IDEG
     C     KOT013        KList
     C                   KFld                    JVISUC
     C                   KFld                    JVICCL
     C                   KFld                    OTITTL
     C     KM2010        KList
     C                   KFld                    AASFEI
     C                   KFld                    @CISUC
     C                   KFld                    @CINCR
     C                   KFld                    @CIDEG
     C     KMI010        KList
     C                   KFld                    AASFEI
     C                   KFld                    WWISUC
     C                   KFld                    WWINCR
     C                   KFld                    WWIDEG
     C     KS2011        KList
     C                   KFld                    @PJOBN
     C     KMC010        KList
     C                   KFld                    AASFEI
     C                   KFld                    JVISUC
     C                   KFld                    JVINCR
     C                   KFld                    JVIDEG
     C     KTB000        KList
     C                   KFld                    JVITIN
     C                   KFld                    JVININ
     C     KWN010        KList
     C                   KFld                    WNIPF1
     C                   KFld                    WNIPF2
     C                   KFld                    WNIPF3
     C                   KFld                    WNIPF4
     C                   KFld                    WNIPF5
     C*
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Move      @CIFUN        WWIFUN
     C                   Z-Add     1             OTITTL
     C*
     C     @PUSER        Chain     SGUSUA                             80
     c                   Move      CVISUC        WWIECT
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* GetLastNo: Obtener Ultimo Número
     C*---------------------------------------------------------------------
     C     GetLastNo     BegSr
     C*
     c* ... Obtiene Ultimo IRRN
     C                   MoveL(P)  'PRFD00RG'    WNIPF1
     C                   MoveL(P)  'AFIRRN  '    WNIPF2
     C                   Move      *BLANKS       WNIPF3
     C                   Move      *BLANKS       WNIPF4
     C                   Move      *BLANKS       WNIPF5
     C     KWN010        Chain     REBANUME                           99
+----C                   If        *In99 = *On
     c/EXEC SQL
     C+ SELECT MAX(AFIRRN) +1 INTO :WNIULN FROM PRAFED
     C/END-EXEC
|    C                   Write     REBANUME
+----C                   Else
|    C                   Add       1             WNIULN
|    C                   Update    REBANUME
+----C                   EndIf
     C                   Z-Add     WNIULN        WWIRRN
     C*
     c* ... Obtiene Ultimo ISEQ para el día
     C                   MoveL(P)  'PRFD00RG'    WNIPF1
     C                   MoveL(P)  'AFISEQ  '    WNIPF2
     C                   Move(P)   AASFEI        WNIPF3
     C                   Move      *BLANKS       WNIPF4
     C                   Move      *BLANKS       WNIPF5
     C     KWN010        Chain     REBANUME                           99
+----C                   If        *In99 = *On
     c                   Z-Add     1             WNIULN
|    C                   Write     REBANUME
+----C                   Else
|    C                   Add       1             WNIULN
|    C                   Update    REBANUME
+----C                   EndIf
     C                   Z-Add     WNIULN        WWISEQ
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* ReadParms:Leer Parámetros
     C*---------------------------------------------------------------------
     C     ReadParams    BegSr
     C*
     C* Lee registro de Usuario
     C     @PJOBN        Chain(N)  @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        Chain(N)  @CPISYS                            90
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* DSPERR: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   IF        PABATC=*OFF
     C                   CALL      'BAER01RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C                   ENDIF
     C                   ENDSR
