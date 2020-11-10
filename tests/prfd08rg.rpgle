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
     DPrFmtFed         DS
     D dsindi                  1     20
     D dsisuc                  1      2
     D dsincr                  4     18
     D dsideg                 20     20
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
     c                             (JVILCR = 5 or JVILCR=8 or JVILCR=9 or
     c                              JVILCR = 1 or JVILCR=10               )
     c* ... Verifica si el area es controlada por Federal, sino salir
     c                   Move      *Off          WWERRO            1
     c                   ExSr      CheckArea
     C                   IF        WWERRO = *ON
     C                   ExSr      EndPgm
     C                   Endif
     c                   ExSr      CheckScoring
     C                   If        EsPorScoring = *Off
     c                   ExSr      SelecRecibo
     c                   ExSr      GenerarMinuta
     c                   Else
     c                   ExSr      RecuperarDatos
     c                   ExSr      GenerarMinuta
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
     c* CheckArea: Chequea si el area esta autorizada a Utilizar Federal
     c*-------------------------------------------------------------------------
     c     CheckArea     BegSr
     C*
     c     KTB000        Chain     REPRHATB                           99
     c                   If        *IN99 = *Off and TBDF03<>'F'
     c                   Move      *On           WWERRO            1
     c                   EndIf
     C*
     C                   EndSr
     c*-------------------------------------------------------------------------
     c* GenerarMinuta: Generar Minuta si cred. es por fuera Scoring
     c*-------------------------------------------------------------------------
     c     GenerarMinuta BegSr
     c* ... Parámetros INDO, Op. Refinanciada, cuenta cliente
     c                   Z-Add     @CISUC        KKISUC
     c                   Z-Add     @CINCR        KKINCR
     c                   Z-Add     @CIDEG        KKIDEG
     C     @PJOBN        Chain     @CPIUSD                            80
     c*exec sql
     C* SELECT AFISUC, AFINCR, AFIDEG INTO :@CISUC, :@CINCR, :@CIDEG FROM
     C* prafed WHERE AFFECH = 0 AND AFIBCF=:SCIBCF AND AFIRED=:SCIRED AND
     C* AFINDO= :SCINDO AND AFIOPT='A'
     c*End-exec
     c                   Z-Add     SCINDO        @CINDO
     c                   Z-Add     JVICCL        @CICCL
     c                   Z-Add     SCISUC        @CISUC
     c                   Z-Add     SCINCR        @CINCR
     c                   Z-Add     SCIDEG        @CIDEG
     c                   Update    @CPIUSRR
     c*
     C                   Call      'BASC00R3'
     c*
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Z-Add     KKISUC        @CISUC
     c                   Z-Add     KKINCR        @CINCR
     c                   Z-Add     KKIDEG        @CIDEG
     c                   Update    @CPIUSRR
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
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* SelecRecibo:Pedir datos al usuario si el credito es x fuera de Scoring
     c*------------------------------------------------------------------------
     c     SelecRecibo   BegSr
     C*
     c     KOT013        Chain     REBADCCL                           99
     C                   If        *IN99 = *On
     c                   CALL      'BASC00RS'
     c                   Else
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Move      OTINDO        @CINDO
     C                   Update    @CPIUSRR
     C                   EndIf
     C*
     C*
     c                   CALL      'BASC10TE'
     c                   ExSr      ReadParams
     C                   If        @FN(03)=*Off or @FN(12)=*Off
     c                   ExSr      Abort
     C                   EndIf
     C*
     c                   ExSr      RecuperarDatos
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* CheckScoring: Chequea si el credito se esta dando por Scoring
     c*------------------------------------------------------------------------
     c     CheckScoring  BegSr
     c* ... Dado Por Fuera de Scoring 0 o Por Scoring 1
     c                   Move      *Off          EsPorScoring      1
     c                   If        JVILEG=0
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
     C                   Z-Add     WWEDSU        WWISUC
     C                   Z-Add     WWINCV        WWINCR
     C                   Z-Add     WWIDEV        WWIDEG
     c                   ExSr      SalvarMovsPR
     C*
     c* ... Salvar Movs del Cuotas Atrasadas
     c     KMC010        Chain     REPRMFED                           99
     C                   DoW       *In99 = *Off
     c                   If        MCIAPC=20 AND MCITRN=2
     c                   ExSr      SalvarMovsCT
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
     C                   Z-ADD     MIFASI        M2FASI
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
     C                   Z-ADD     MIFASI        M2FASI
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
     c     KS2011        Chain     REBASCT1                           99
     C                   DoW       *In99 = *Off
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
     C                   EndIf
     c     KS2011        ReadE     REBASCT1                               99
     C                   EndDo
     C*
     C                   EndSr
     c*------------------------------------------------------------------------
     c* BloquearPrest: Poner un '3' en JVIESA para que el PRESLO no lo tome
     c*------------------------------------------------------------------------
     c     BloquearPrest BegSr
     C*
     c*    KJV           Chain     REPRCRED                           25
     c*                  Move      '3'           JVIESA
     c*                  Update    REPRCRED
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* InsBajas : Insertar Pedido de Bajas (Cred. Refinanciados)
     c*---------------------------------------------------------------------
     c     InsBajas      BegSr
     C*
     c     KS2011        Chain     REBASCT1                           99
     C                   DoW       *In99 = *Off
     c                   If        S2ILCR=20 and S2IMON=04
     C     KSC040        CHAIN     R4                                 99
     C                   Z-ADD     *ZERO         BAIMPO           15 2
     C     KSC040        CHAIN     R2                                 99
     C                   DoW       *IN99=*Off
     c                   IF        AFIOPT='A'
     C                   Z-ADD     AF$CUO        BAIMPO           15 2
     c                   Leave
     c                   EndIf
     C     KSC040        READE     R2                                     99
     c                   EndDo
     C     KSC040        Chain(N)  REPRCRED                           99
     c                   Add       1             WWIRRN
     c                   Z-Add     WWIRRN        AFIRRN
     c                   Z-Add     AASFEI        AFFECH
     c                   Z-Add     *ZERO         AFFACR
     c* ... Dado Por Fuera de Scoring 0 o Por Scoring 1
     C*     - BASC00MA y M1 Ponen @CIPF1 en 'EMISION' o 'CONSULTA' si esta
     C*       Presente en cpi => se Emitio por Scoring
     C                   Move      *ZERO         AFICAR
     c                   If        JVILEG=0
     C                   Move      1             AFICAR
     C                   EndIf
     c                   Add       1             WWISEQ
     c                   Z-Add     WWISEQ        AFISEQ
     c                   Z-Add     JV$INP        AF$IMP
     c                   Z-Add     JVQCUO        AFQCUO
     c                   ExSr      SRIMDS
     c                   Move      WWIMDS        AFIMDS
     c                   Move      'B'           AFIOPT
     c                   Move      WWISEX        AFISEX
     c                   Move      WWINDO        AFINDO
     c                   Move      WWNYAP        AFNYAP
     c                   Move      WWIRED        AFIRED
     c                   Move      WWIBCF        AFIBCF
     c                   Move      S2ISUC        AFISUC
     c                   Move      S2INCR        AFINCR
     c                   Move      S2IDEG        AFIDEG
     c                   Move      @CISUC        AFEDSU
     c                   Move      @CINCR        AFINCV
     c                   Move      @CIDEG        AFIDEV
     c                   Move      SCREFC        AFREFC
     C*
     c                   Move      S2ISUC        dsisuc
     c                   Move      S2INCR        dsincr
     c                   Move      S2IDEG        dsideg
     c                   Move      DSINDI        AFIND1
     C*
     c                   Move      @CISUC        dsisuc
     c                   Move      @CINCR        dsincr
     c                   Move      @CIDEG        dsideg
     c                   Move      DSINDI        AFIND2
     c                   Z-Add     BAIMPO        AF$CUO
     C*
     C*                  Move      *BLANKS       AFFAU1
     C*                  Move      *BLANKS       AFHAU1
     C                   Move      *BLANKS       AFESTA
     c                   Move      *BLANKS       AFAMRC
     c                   Z-Add     *ZERO         AFFE01
     c                   Z-Add     *ZERO         AF$SRD
     C*
     C                   Write     REPRAFED
     C                   EndIf
     c     KS2011        ReadE     REBASCT1                               99
     C                   EndDo
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* InsAlta  : Insertar Pedido de Autorización a Federal
     c*---------------------------------------------------------------------
     c     InsAlta       BegSr
     C*
     C     KJV           Chain(N)  REPRCRED                           25
     C*
     c                   Add       1             WWIRRN
     c                   Z-Add     WWIRRN        AFIRRN
     c                   Z-Add     AASFEI        AFFECH
     c* ... Dado Por Fuera de Scoring 0 o Por Scoring 1
     C*     - BASC00MA y M1 Ponen @CIPF1 en 'EMISION' o 'CONSULTA' si esta
     C*       Presente en cpi => se Emitio por Scoring
     C                   Move      *ZERO         AFICAR
     c                   If        JVILEG=0
     C                   Move      1             AFICAR
     C                   EndIf
     c                   Add       1             WWISEQ
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
     c*                  Move      WWEDSU        AFEDSU
     c*                  Move      WWINCV        AFINCV
     c*                  Move      WWIDEV        AFIDEV
     c                   Move      *ZERO         AFEDSU
     c                   Move      *ZERO         AFINCV
     c                   Move      *ZERO         AFIDEV
     c                   Move      WWREFC        AFREFC
     C*
     c                   Move      @CISUC        dsisuc
     c                   Move      @CINCR        dsincr
     c                   Move      @CIDEG        dsideg
     c                   Move      DSINDI        AFIND1
     c                   Move      *BLANKS       AFIND2
     C*
     c*                  Move      WWEDSU        dsisuc
     c*                  Move      WWINCV        dsincr
     c*                  Move      WWIDEV        dsideg
     c*                  Move      DSINDI        AFIND2
     C                   ExSr      SR$CUO
     c                   Z-Add     WW$CUO        AF$CUO
     C                   Move      *BLANKS       AFFAU1
     C                   Move      *BLANKS       AFHAU1
     C                   Move      *BLANKS       AFESTA
     c                   Move      *BLANKS       AFAMRC
     c                   Z-Add     *ZERO         AFFE01
     c                   Z-Add     *ZERO         AF$SRD
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
     c     1             Chain     SGSYSV
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
     c     *Like         Define    AFISUC        KKISUC
     c     *Like         Define    AFINCR        KKINCR
     c     *Like         Define    AFIDEG        KKIDEG
     c     *Like         Define    @CIFUN        WWIFUN
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
     C*
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Move      @CIFUN        WWIFUN
     C                   Z-Add     1             OTITTL
     C*
     c                   ExSr      GetLastNo
     C*
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* GetLastNo: Obtener Ultimo Número
     C*---------------------------------------------------------------------
     C     GetLastNo     BegSr
     C*
     C                   Z-Add     *ZERO         WWIRRN
     C*
     C     *HIVAL        SetGt     REPRAFED
     C                   ReadP     REPRAFED                               25
     C                   If        *IN25 = *Off
     C                   Z-Add     AFIRRN        WWIRRN
     C                   EndIf
     C*
     c                   Z-Add     *ZERO         WWISEQ
     C     *HIVAL        SetGt     REPRAFED
     C                   ReadP     REPRAFED                               25
     C                   If        *IN25 = *Off and AFFECH=AASFEI
     C                   Z-Add     AFISEQ        WWISEQ
     C                   EndIf
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
     C                   CALL      'BAER00RS'
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
