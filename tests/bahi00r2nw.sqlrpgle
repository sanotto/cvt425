*SRCMBRTXT:BAHILI: Selecciona Hoja Resumen        
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME : SIDEBA - VARIOS                                *
     H*                                                               *
     H*  PROGRAM NAME: BAHI00R2                                       *
     H*                                                               *
     H*  DESCRIPTION : REIMPRIME HOJA DE RESUMEN                      *
     H*                                                               *
     H*  PROGRAM NO  :                                                *
     H*                                                               *
     H*  DATE        : 21/12/2016                                     *
     H*                                                               *
     H*  AUTHOR      : Ottonello, Santiago                            *
     H*                                                               *
     H*---------------------------------------------------------------*
     F*
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     FSGSYSV    IF   E             DISK
     D*----------------------------------------------------------------*
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
     D query           S           4096A
     I*----------------------------------------------------------------*
     c                   ExSr      PedirFechas
     c                   ExSr      DspHojas
     c                   DoW       @FN(03)=*Off and @FN(12)=*Off
     c                   If        @CIOPT='1' or @CIOPT='2'
     c                   ExSr      EnviarCorreo
     c                   Eval      MsgTxt='Hoja de Reumen enviada a '+
     c                                    'su correo'
     c                   ExSr      DspMsg
     C                   EndIf
     c                   ExSr      DspHojas
     C                   EndDo
     C                   EXSR      EndPgm
     C*--------------------------------------------------------------
     C     PedirFechas   BegSr
     C*--------------------------------------------------------------
     c*
     c                   Call      'BA0002RS'
     c                   ExSr      ReadParms
     c                   If        @fn(3) = *On
     c                   ExSr      EndPgm
     c                   EndIf
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     C     EnviarCorreo  BegSr
     C*--------------------------------------------------------------
     c*
     c                   Call      'BAHI00R3'
     c                   Parm                    @CIPGM
     c                   Parm                    @CITER
     c                   Parm                    @CFALT
     c                   Parm                    @CISUC
     c                   Parm                    @CICCC
     c                   Parm                    @CICPO
     c                   Parm                    @CIOPT
     c*
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     C     DspHojas      BegSr
     C*--------------------------------------------------------------
     c*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     c  n80              Move      *Blanks       @CIOPT
     c  n80              Update    @CPIUSRR
      /free
        query=  'SELECT DISTINCT                                     '+
                '   HLIHUR AS "Hoja Nro.",                           '+
                '   CAST(                                            '+
                '   SUBSTR(CAST(HLFALT AS CHAR(8)), 7, 2) ||         '+
                '  ''/''                                  ||         '+
                '   SUBSTR(CAST(HLFALT AS CHAR(8)), 5, 2) ||         '+
                '  ''/''                                  ||         '+
                '   SUBSTR(CAST(HLFALT AS CHAR(8)), 1, 4)            '+
                '   AS CHAR(10))       AS "Fecha de Emisión",        '+
                '   HLISUC AS @CISUC,                                '+
                '   HLICCC AS @CICCC,                                '+
                '   HLFALT AS @CFALT,                                '+
                '   HLIHUR AS @CICPO,                                '+
                '   HLIPGM AS @CIPGM,                                '+
                '   HLIFIL AS @CITER                                 '+
                'FROM BAHILI                                         '+
                'WHERE                                               '+
                '    HLISUC = :@CISUC                                '+
                'AND HLICCC = :@CICCC                                '+
                'AND HLIPGM = '''+%TRIM(PAIPGM)+'''                  '+
                'AND HLFALT >= :@CFDES                               '+
                'AND HLFALT <= :@CFHAS                               ';

      /end-free
     c*
     c                   Eval      PGMTIT='   Seleccione Hoja a Imprimir '
     c                   Eval      HEDLI1=' '
     c                   Eval      HEDLI2=' Suc.:'+%EditC(@CISUC:'Z')+
     c                                    '  Cuenta:'+%EditC(@CICCC:'Z')
     c                   Eval      HEDLI3=' 1=Env.  a Usuario 2=Env.a Clte'
     c                   Eval      FUNKEY=*Blanks
     c                   Call      'BADSSQMB'
     c                   Parm                    PGMNME           10
     c                   Parm                    PGMTIT           30
     c                   Parm                    HEDLI1           78
     c                   Parm                    HEDLI2           78
     c                   Parm                    HEDLI3           78
     c                   Parm                    FUNKEY           64
     c                   Parm                    query          4096
     c*
     c                   ExSr      ReadParms
     c*
     C                   EndSr
     C*--------------------------------------------------------------
     C*ReadParms: Leer Parametros
     C*--------------------------------------------------------------
     C     ReadParms     BEGSR
     C* Lee registro de Usuario
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        CHAIN(N)  @CPISYS                            90
     C                   ENDSR
     C*--------------------------------------------------------------
     C*ENDPGM: Salir del Programa
     C*--------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*DSPMSG: Mostrar Errores
     C*-------------------------------------------------------------------------
     C     DSPMSG        BEGSR
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
     C                   ENDSR
     C*--------------------------------------------------------------
     C**INZSR: Inicializacion
     C*--------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SUBSYS            2
     C*
     C                   MOVE      'CC5011CL'    PAIPGM           10
     C                   IF        SUBSYS='CA'
     C                   MOVE      'AC3021CL'    PAIPGM           10
     C                   ENDIF
     C*
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C*
     C     1             CHAIN     SGSYSV
     C*
     c                   MoveL(P)  'BAHI00R2'    PGMNME           10
     C*
     c                   ExSr      ReadParms
     C*
     C                   ENDSR
