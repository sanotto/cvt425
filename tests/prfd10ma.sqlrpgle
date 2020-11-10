*SRCMBRTXT:Aut.Federal-Auditoria Previa a Bajada  
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME : SIDEBA - PRESTAMOS                             *
     H*                                                               *
     H*  PROGRAM NAME: PRFD10MA                                       *
     H*                                                               *
     H*  DESCRIPTION : CONSULTA DE FEDERAL                            *
     H*                                                               *
     H*  PROGRAM NO  :                                                *
     H*                                                               *
     H*  DATE        : 03/08/2012                                     *
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
     c                   ExSr      DspAnalisis
     c*
     C                   EXSR      ENDPGM
     C*--------------------------------------------------------------
     C     DspAnalisis   BegSr
     C*--------------------------------------------------------------
     c*
      /free
        query='SELECT                                                '+
        '   PRAFED.AFESTA AS ESTADO,                                 '+
        '   PRAFED.AFIOPT AS ACCION,                                 '+
        '   PRAFED.AFISUC AS SUCURSAL,                               '+
        '   PRAFED.AFINCR AS OPERACIO,                               '+
        '   PRAFED.AFIDEG AS DESGLOSE,                               '+
        '   PRAFED.AFINCV AS REFPOROP,                               '+
        '   CAST(                                                    '+
        '   CASE WHEN                                                '+
        '   (SELECT                                                  '+
        '      COUNT(*)                                              '+
        '    FROM PRMOVI60                                           '+
        '    WHERE                                                   '+
        '          M2ISUC=PRAFED.AFISUC                              '+
        '      AND M2INCR=PRAFED.AFINCR                              '+
        '      AND M2IDEG=PRAFED.AFIDEG                              '+
        '      AND M2FASI=PRAFED.AFFECH                              '+
        '   ) > 0 THEN ''OK'' ELSE ''ERROR'' END AS CHAR(5)) AS      '+
        '   PRTMOD60,                                                '+
        '   CAST(                                                    '+
        '   CASE WHEN                                                '+
        '   (SELECT                                                  '+
        '      COUNT(*)                                              '+
        '    FROM        PRMOVI                                      '+
        '    WHERE                                                   '+
        '          MIISUC=PRAFED.AFISUC                              '+
        '      AND MIINCR=PRAFED.AFINCR                              '+
        '      AND MIIDEG=PRAFED.AFIDEG                              '+
        '      AND MIFASI=PRAFED.AFFECH                              '+
        '   ) > 0 THEN ''OK'' ELSE ''ERROR'' END AS CHAR(5)) PRMOVI, '+
        '   CAST (                                                   '+
        '   (SELECT                                                  '+
        '      COUNT(*)                                              '+
        '    FROM        PRTMOD                                      '+
        '    WHERE                                                   '+
        '          NAISUC=PRAFED.AFISUC                              '+
        '      AND NAINCR=PRAFED.AFINCR                              '+
        '      AND NAIDEG=PRAFED.AFIDEG                              '+
        '   ) AS DEC(5, 0)) AS PRTMOD,                               '+
        '   '' ''                                                    '+
        '   FROM        PRAFED                                       '+
        '   WHERE                                                    '+
        '         AFFECH=(SELECT AASFEI FROM        SGSYSV)          '+
        '    AND  AFESTA=''A''                                       ';
      /end-free
     c*
     c                   Eval      PGMTIT='   FEDERAL-Analisis Previo    '
     c                   Eval      HEDLI1=*Blanks
     c                   Eval      HEDLI2=*Blanks
     c                   Eval      HEDLI3=*Blanks
     c                   Eval      FUNKEY=*Blanks
     c                   Call      'BADSSQMB'
     c                   Parm                    PGMNME           10
     c                   Parm                    PGMTIT           30
     c                   Parm                    HEDLI1           78
     c                   Parm                    HEDLI2           78
     c                   Parm                    HEDLI3           78
     c                   Parm                    FUNKEY           64
     c                   Parm                    query          4096
     C                   EndSr
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
     C*ReadParms: Leer Parametros
     C*--------------------------------------------------------------
     C     ReadParms     BEGSR
     C* Lee registro de Usuario
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        CHAIN(N)  @CPISYS                            90
     C                   ENDSR
     C*--------------------------------------------------------------
     C**INZSR: Inicializacion
     C*--------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C*
     C     1             CHAIN     SGSYSV
     C*
     c                   MoveL(P)  'PRFD10MA'    PGMNME           10
     C*
     C                   ENDSR
