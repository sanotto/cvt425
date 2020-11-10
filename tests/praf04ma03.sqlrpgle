*SRCMBRTXT:Gestor de Minuta Federal               
     H DEBUG
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA - PRESTAMOS                              *
     H*                                                               *
     H*  PROGRAM NAME: PRAF04MA                                       *
     H*                                                               *
     H*  PROGRAM NO: PROGRAMA PARA VERIFICACION, CONTROL Y MODIFIC.   *
     H*              DE MINUTA DE APLICACION AUTOMATICA DE FEDERAL    *
     H*                                                               *
     H*  DATE:     28/12/2012                                         *
     H*                                                               *
     H*  AUTHOR:   Sergio Cortes                                      *
     H*                                                               *
     H*---------------------------------------------------------------*
     F@CPISYS   IF   E           K DISK
     FPRCRED    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     D**********************************************************************
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     D MSGDS           DS
     D  MSGTXT                 1    255
     D  WWNCU1                 1     55
     D  WWNCU2                56    110
     D  WWNCU3               111    165
     D  WWNCU4               166    220
     D  WWNCB1               221    275
     D  WWNCB2               276    330
     D  WWNCB3               331    385
     D  WWNCB4               386    440
     D*-------------------------------------------------------------------------
     D RC                             7A
     D*-------------------------------------------------------------------------
     c                   ExSr      PedirCredito
     c                   DoW       @FN(03)=*Off and @FN(12)= *Off
     c                   ExSr      LlenarTemporal
     c                   ExSr      MostrarTE
     C                   ExSr      ValidarTE
     c                   DoW       MsgTxt <> *Blanks and
     c                             @FN(03)=*Off and @FN(12)= *Off
     c                   ExSr      DspError
     c                   ExSr      MostrarTE
     C                   ExSr      ValidarTE
     C                   EndDo
     c                   If        MsgTxt =  *Blanks and
     c                             @FN(03)=*Off and @FN(12)= *Off
     c                   ExSr      ActMinuta
     C                   EndIf
     c                   ExSr      PedirCredito
     c                   EndDo
     C                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     c*-------------------------------------------------------------------------
     C*
     C                   Move      *ON           *INLR
     c                   Return
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     DspError      BegSr
     c*-------------------------------------------------------------------------
     C*
     C                   CALL      'BAER01RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C*
     c                   ExSr      ReadParms
     C*
     c                   EndSr
     C*--------------------------------------------------------------
     C*ReadParms: Leer Parametros
     C*--------------------------------------------------------------
     C     ReadParms     BEGSR
     C* Lee registro de Usuario
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        CHAIN     @CPISYS                            90
     C                   ENDSR
     c*-------------------------------------------------------------------------
     c     ValidarTE     BegSr
     c*-------------------------------------------------------------------------
     C*
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     ActMinuta     BegSr
     c*-------------------------------------------------------------------------
     C*
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     LlenarTemporalBegSr
     c*-------------------------------------------------------------------------
     c*
     C
     C*
     C                   EndSr
     c*--------------------------------------
     c     MostrarTE     BegSr
     c*--------------------------------------
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Editar Minuta de Federal'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Operación:'
     C                   EVAL      WWTIT3=*Blanks
     C                   EVAL      WWTIT4=*Blanks
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=''
     C                   MOVEL(P)  @PJOBN        @CIJOB
     C                   UPDATE    @CPIUSRR
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     PedirCredito  BegSr
     c*-------------------------------------------------------------------------
     C*
     C                   CALL      'PRAF04SV'
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        CHAIN     @CPISYS                            90
     C* Testea teclas de función
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     *INZSR        BegSr
     C*-------------------------------------------------------------------------
     c*
     C                   MOVE      @PPGID        PAIPGM           10
     C                   CALL      'SGACCERG'
     C                   PARM                    PAIERR            1
     C                   PARM                    PAIPGM
     C     PAIERR        IFNE      '0'
     C                   ExSr      EndPgm
     C                   ENDIF
     c*
     C* Acceso a BAICCL
     C     WKEY02        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    JVICCL
     C* Acceso a PRCRED
     C     WKEY01        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    @CINCR
     C                   KFLD                    @CIDEG
     c*
     c                   EndSr
     P*---------------------------------------------------------------------
     P* PROCEDIMIENTOS DEBEN DECLARARSE DESPUES DE LA HOJA O, SI ESTA EXISTE
     P*---------------------------------------------------------------------
     P Shell           B                   EXPORT
     D  Shell          PI             7A
     D   Command                   1024A   VALUE
     D CommLen         S             15  5

     C                   Call      'QCMDEXC'
     c                   PARM                    Command
     C                   PARM      1024          CommLen
     C                   Return    'CPF0000'
     c     *PSSR         BEGSR
     C                   Return    @PEXEP +@PEXNO
     c                   ENDSR
     c
     P Shell           E
