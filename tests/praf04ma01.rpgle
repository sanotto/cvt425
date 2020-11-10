*SRCMBRTXT:Gestor de Minuta Federal               
     H DEBUG
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
     FPRAFED02  IF   E           K DISK
     FBAICCL    IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     FSGSYSV    IF   E             DISK
     D*----------------------------------------------------------------*
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     D*-------------------------------------------------------------------------
     DERRDS            DS
     D ERRTXT                  1    255
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*
     C                   EXSR      CHKACC
     C     LOOP01        TAG
     C                   EXSR      GETNCR
     C     KFD024        CHAIN     REPRAFED                           99
     C     *IN99         IFEQ      *OFF
     C     AFFTRA        IFEQ      *ZERO
     C                   EXSR      EDTCUP
     C                   ELSE
     C                   EXSR      VIWCUP
     C                   ENDIF
     C                   ELSE
     c                   EVAL      ERRTXT='NO SE ENCONTRO EL CREDITO EN EL'+
     C                                    ' ARCHIVO DE FEDERAL            '
     C                   EXSR      DSPERR
     C                   ENDIF
     C                   GOTO      LOOP01
     C     END           TAG
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* *INZSR: RUTINA DE INICIALIZACION
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C* Acceso a PRCRED
     C     WKEY01        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    @CINCR
     C                   KFLD                    @CIDEG
     C* Acceso a BAICCL
     C     WKEY02        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    JVICCL
     C* Acceso a PRAFED02
     C     KFD024        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    @CINCR
     C                   KFLD                    @CIDEG
     C                   KFLD                    AASFEI
     C*
     C                   Z-ADD     *ZEROS        @CICCL
     C                   MOVEL     *BLANK        @CNCCL
     C*
     C     1             CHAIN     SGSYSV
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* EDTCUP: EDITAR CUPON
     C*-------------------------------------------------------------------------
     C     EDTCUP        BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C     WKEY01        CHAIN(N)  PRCRED                             85
     C  N85WKEY02        CHAIN(N)  BAICCL                             86
     C  N85
     CANN86              Z-ADD     JVICCL        @CICCL
     C  N85
     CANN86              MOVEL     OSNCCL        @CNCCL
     C                   UPDATE    @CPIUSRR
     C                   CALL      'PRAF04TM'
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VIWCUP: VISUALIZAR CUPON
     C*-------------------------------------------------------------------------
     C     VIWCUP        BEGSR
     C*
     c                   EVAL      ERRTXT='EL ARCHIVO YA FUE ENVIADO NO SE'+
     C                                    ' PUEDE EDITAR                  '
     C                   EXSR      DSPERR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* GETNCR: OBTENER NRO DE CREDITO
     C*-------------------------------------------------------------------------
     C     GETNCR        BEGSR
     C*
     C                   CALL      'PRAF04SV'
     C     @PJOBN        CHAIN(n)  @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        CHAIN     @CPISYS                            90
     C* Testea teclas de función
     C     @FN(03)       IFEQ      '1'
     C     @FN(12)       OREQ      '1'
     C                   EXSR      ENDPGM
     C                   END
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ENDPGM: FINALIZAR PROGRAMA
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CHKACC: CHEQUEAR ACCESO
     C*-------------------------------------------------------------------------
     C     CHKACC        BEGSR
     C*
     C                   MOVE      @PPGID        PAIPGM           10
     C                   CALL      'SGACCERG'
     C                   PARM                    PAIERR            1
     C                   PARM                    PAIPGM
     C     PAIERR        IFNE      '0'
     C                   GOTO      END
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* DSPERR: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   UNLOCK    @CPIUSD
     C                   CALL      'BAER00RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C                   ENDSR
