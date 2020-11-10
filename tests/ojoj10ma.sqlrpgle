*SRCMBRTXT:OJ-Consulta de Embargos-Principal      
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME : SIDEBA - PRESTAMOS                             *
     H*                                                               *
     H*  PROGRAM NAME: LILR00MA                                       *
     H*                                                               *
     H*  DESCRIPTION : Consulta de Embargos Soj                       *
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
     c                   ExSr      PedirDocumento
     c                   DoW       @FN(03)=*Off and @FN(12) = *Off
     C                   If        @CINDO <> *ZERO
     c                   ExSr      DspOficios
     c                   ExSr      WrkOficios
     C                   EndIf
     c                   ExSr      PedirDocumento
     c                   EndDo
     c*
     C                   EXSR      ENDPGM
     C*--------------------------------------------------------------
     C     WrkOficios    BegSr
     c*
     c                   ExSr      ReadParms
     c                   If        @ZOPSL=*Blanks
     c                   LeaveSr
     c                   EndIf
     C                   Eval      MsgTxt=*Blanks
     C                   Eval      WWNCU1='EMBARGO OF.NRO.:'+
     C                             %EDITW(@CINUI:'               ')+
     C                             '-'+@CNYAP
     C                   Eval      WWNCU3='F06=Ver Resp. p/el Oficio'
     C                   Eval      WWNCU4='F07=Ver Oficios Relacionados'
     c*
     c                   ExSr      DspMsg
     c                   DoW       @FN(03) = *off And @FN(12) = *Off
     c                   Select
     c                   When      @FN(06) = *On
     C                   ExSr      DspRespuestas
     c                   When      @FN(07) = *On
     C                   ExSr      DspRelaciones
     c                   EndSl
     c                   ExSr      DspMsg
     c                   EndDo
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     C     DspOficios    BegSr
     C*--------------------------------------------------------------
     c*
      /free
        query=' SELECT                                               '+
             '   OJVIGE as "Fecha Vig."   ,                          '+
             '   OJINUI as "Of. Judicial" ,                          '+
             '   cast(OJ$CAP as dec(8, 2)) as        "Capital",      '+
             '   cast(OJ$INT as dec(8, 2)) as        "Interes",      '+
             '   cast(OJ$INT+OJ$CAP as dec(8, 2)) as "Total",        '+
             '   cast(OJ$SAL as dec(8, 2)) as        "Saldo",        '+
             '   OJNYAP as @CNYAP,                                   '+
             '   OJINUI as @CINUI,                                   '+
             '   OJININ as @CININ                                    '+
             ' FROM                                                  '+
             '   SDBFIL/OJMOFI                                       '+
             ' WHERE                                                 '+
             '  OJITRG= 1                                            '+
             '  AND (OJIINI=:@CINDO OR                               '+
             '  OJINDO=:@CINDO )                                     '+
             ' ORDER BY                                              '+
             '   OJVIGE DESC                                         ';
      /end-free
     c*
     c                   Eval      PGMTIT='   Embargos para el D.N.I.    '
     c                   Eval      HEDLI1='D.N.I.:'+%editw(
     c                             @CINDO:'               ')+'-'+@CNYAP
     c                   Eval      HEDLI2='Por favor, seleccione una opción ...'
     c                   Eval      HEDLI3='1=Seleccionar'
     c                   Eval      FUNKEY=*Blanks
     c                   Call      'SEUEP00RG'
     c                   Parm                    PGMNME           10
     c                   Parm                    PGMTIT           30
     c                   Parm                    HEDLI1           78
     c                   Parm                    HEDLI2           78
     c                   Parm                    HEDLI3           78
     c                   Parm                    FUNKEY           64
     c                   Parm                    query          4096
     C                   EndSr
     C*--------------------------------------------------------------
     C     DspRespuestas BegSr
     C*--------------------------------------------------------------
     c*
      /free
        query=' SELECT                                               '+
             '  MRIBCF as "Tipo Prod.AFIP",                          '+
             '  CPDVAT as "Descripción   ",                          '+
             '  MRIOPT as "Cliente?"      ,                          '+
             '  MRICCC as "Nro Cuenta"    ,                          '+
             '  MR$ICU as "Imp.Cubierto"  ,                          '+
             '  MRFACR as "Fecha Ret."    ,                          '+
             '  MR$INP as "Cant.Bs.  "    ,                          '+
             '  MRTSEL as "Cotización"    ,                          '+
             '  MRFING as "Fech.Cotiz"    ,                          '+
             '  MRFASI as "Fecha AFIP"    ,                          '+
             '  MRIASI as "Nro. AFIP "                               '+
             ' FROM                                                  '+
             '   SDBFIL/OJMRES                                       '+
             '   LEFT JOIN SDBFIL/OJCTPR ON CPIBCF=MRIBCF            '+
             ' WHERE                                                 '+
             '      MRINUI = :@CINUI                                 ';
      /end-free
     c*
     c                   Eval      PGMTIT='   Respuestas para Oficio     '
     C                   Eval      HEDLI1='EMBARGO OF.NRO.:'+
     C                             %EDITW(@CINUI:'               ')+
     C                             '-'+@CNYAP
     c*                  Eval      HEDLI2='Por favor, seleccione una opción ...'
     c*                  Eval      HEDLI3='1=Seleccionar'
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
     C     DspRelaciones BegSr
     C*--------------------------------------------------------------
     c*
      /free
        query=' SELECT                                               '+
             '   OJCOTR AS "Tipo of (AFip/LItis)",                   '+
             '   OJINUI AS "Nro de Oficio"       ,                   '+
             '   OJINUI AS "Nro de Oficio"       ,                   '+
             '   OJVIGE AS "Fecha Lbto."         ,                   '+
             '   TODESC AS "Tipo Oficio"         ,                   '+
             '   OJ$CAP AS "Capital    "         ,                   '+
             '   OJ$INT AS "Interes    "         ,                   '+
             '   OJABAN AS "Tfr.Bco    "         ,                   '+
             '   OJNSUC AS "Tfr.Suc    "         ,                   '+
             '   OJINCT AS "Tfr.Cuenta "         ,                   '+
             '   '' ''  AS "Final"                                   '+
             ' FROM                                                  '+
             '   SDBFIL/OJMOFI                                       '+
             '   LEFT JOIN SDBFIL/OJTOFI ON TOITRG=OJITRG            '+
             ' WHERE                                                 '+
             '      OJINUC = :@CINUI                                 ';
      /end-free
     c*
     c                   Eval      PGMTIT='    Oficios Relacionados      '
     C                   Eval      HEDLI1='EMBARGO OF.NRO.:'+
     C                             %EDITW(@CINUI:'               ')+
     C                             '-'+@CNYAP
     c*                  Eval      HEDLI2='Por favor, seleccione una opción ...'
     c*                  Eval      HEDLI3='1=Seleccionar'
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
     C     PedirDocumentoBegSr
     C*--------------------------------------------------------------
     C                   Call      'BASC00RS'
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
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C*
     C     1             CHAIN     SGSYSV
     C*
     c                   MoveL(P)  'OJOJ10MA'    PGMNME           10
     C*
     C                   ENDSR
