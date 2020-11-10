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
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     F*
     FBASCTM    UF A E           K DISK
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
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
     I*----------------------------------------------------------------*
     I*----------------------------------------------------------------*
     c                   ExSr      PedirDocumento
     c                   DoW       @FN(03)=*Off and @FN(12) = *Off
     c                   ExSr      BuscarClientes
     c                   ExSr      PedirDocumento
     c                   EndDo
     c*
     C                   EXSR      ENDPGM
     C*--------------------------------------------------------------
     C     PedirDocumentoBegSr
     C*--------------------------------------------------------------
     C                   Call      'BAPE00RG'
     c                   ExSr      ReadParms
     c*
     C                   EndSr
     C*--------------------------------------------------------------
     C     BuscarClientesBegSr
     C*--------------------------------------------------------------
     C*
     c                   ExSr      DltTmp
     c                   ExSr      AbritCurEmbar
     c                   ExSr      LeerCurEmbar
     c                   Z-Add     *Zero         Count            15 0
     c                   DoW       SQLCOD = *ZERO
     c                   Add       1             Count
     c*
     c                   Move      *Blanks       wwesta           20
     c                   Move      *Blanks       S1REFC
     c                   Select
     c                   When      wwieti= ' '
     c                   Eval      S1REFC='H'
     c                   Eval      wwesta='Activo     '
     c                   When      wwieti= 'P'
     c                   Eval      wwesta='Resp. Parc.'
     c                   When      wwieti= 'T'
     c                   Eval      wwesta='Resp. Tot.'
     c                   When      wwieti= 'L'
     c                   Eval      wwesta='Levantado'
     c                   EndSl
     c*
     c                   Eval      S1IJOB=@PJOBN
     c                   Z-Add     wwININ        S1INCR
     c                   Z-Add     wwINUI        S1$INP
     c                   Z-Add     wwNTDO        S1$CUC
     c                   Z-Add     wwIINI        S1$CUI
     c                   Movel(P)  wwIETI        S1DF02
     c                   Movel(P)  wwnyap        S1DF03
     c                   Eval      S1DACL=' '+
     c                             %EDITW(WWVIGE:'    /  /  ')+' '+
     c                             %SUBST(wwnyap:1:20) + ' ' +
     c                             %EDITW(WWINUI:'               ') + ' '+
     c                             wwesta
     c                   Write     REBASCTM
     c                   ExSr      LeerCurEmbar
     c                   EnDDo
     c                   ExSr      CerrarCurEmbar
     C*
     C                   If        Count = *Zero
     C                   Eval      MSGTXT='No se encontraron embargos para '+
     c                                    'ese documento...'
     C                   EXSR      DspErr
     c                   Else
     c                   ExSr      TrabConEmbar
     c                   EndIf
     C*
     C                   EndSr
     C*--------------------------------------------------------------
     c     CerrarCurEmbarBegSr
     C*--------------------------------------------------------------
     c/exec sql
     c+ close c1
     c/end-exec
     C*
     C                   EndSr
     C*--------------------------------------------------------------
     c     AbritCurEmbar BegSr
     C*--------------------------------------------------------------
     C                   Z-Add     @CINDO        WWINDO           15 0
     c/exec sql
     c+ DECLARE C1 CURSOR FOR
     C+ SELECT
     C+       OJVIGE,
     C+       OJINDO,
     C+       OJINUI,
     C+       OJNYAP,
     C+       OJIETI,
     C+       OJININ,
     C+       OJNTDO,
     C+       OJIINI
     C+ FROM OJMOFI
     C+ WHERE
     C+       OJITRG='1' AND
     C+                            OJINDO= :WWINDO
     C+ ORDER BY
     C+       OJIETI, OJFING DESC
     c/end-exec
     c/exec sql
     c+ open  c1 using :@cindo
     c/end-exec
     C                   EndSr
     C*--------------------------------------------------------------
     c     LeerCurEmbar  BegSr
     C*--------------------------------------------------------------
     c                   Z-Add     *Zero         wwvige            8 0
     c                   Z-Add     *Zero         wwindo           15 0
     c                   Z-Add     *Zero         wwinui           15 0
     c                   Move      *Blanks       wwnyap           50
     c                   Move      *Blanks       wwieti            1
     c                   Z-Add     *Zero         wwININ           15 0
     c                   Z-Add     *Zero         wwNTDO           15 0
     c                   Z-Add     *Zero         wwIINI           15 0
     c
     c/exec sql
     c+ fetch c1 into
     C+       :WWVIGE,
     C+       :WWINDO,
     C+       :WWINUI,
     C+       :WWNYAP,
     C+       :WWIETI,
     C+       :WWININ,
     C+       :WWNTDO,
     C+       :WWIINI
     c/end-exec
     c*
     C                   EndSr
     C*--------------------------------------------------------------
     c     TrabConEmbar  BegSr
     C*--------------------------------------------------------------
     c                   ExSr      ShowListaCltes
     c                   DoW       @FN(03) = *Off and @FN(12) = *Off
     c                   ExSr      MostrarMenu
     c                   DoW       @FN(03) = *Off and @FN(12) = *Off
     c                   Select
     c                   When      @FN(06) = *On
     c                   Call      'OJOJ00MM'
     c                   When      @FN(07) = *On
     c                   Call      'OJOJ11MA'
     c                   EndSl
     c                   ExSr      MostrarMenu
     c                   EndDo
     c                   ExSr      ShowListaCltes
     c                   EndDo
     C*
     C                   EndSr
     C*--------------------------------------------------------------
     c     ShowListaCltesBegSr
     C*--------------------------------------------------------------
     c*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Consulta de Embargos Activos  '
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Nro Doc./CUIT :'+
     C                                    x'22'+%EDITW(@cindo:'               ')
     C                   EVAL      WWTIT3='Mostrando     :'+
     C                                    x'22'+'Embargos Activos'
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+'Fec.Lib.   '+
     C                                   'Nombre o Razón Soc.  '+
     c                                   'Nro. de Oficio  ' +
     c                                   'Estado'
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
     C     @ZRRNO        CHAIN     RTMP                               99
     c     @PJOBN        Chain     @CPIUSRR                           99
     C                   Z-Add     S1INCR        @CININ
     C                   Z-Add     S1$INP        @CINUI
     C                   Z-Add     S1$CUC        @CNTDO
     C                   Z-Add     S1$CUI        @CIINI
     C                   Movel(P)  S1DF02        @CIETI
     c                   Update    @CPIUSRR
     C*
     C                   EndSr
     C*--------------------------------------------------------------
     c     MostrarMenu   BegSr
     C*--------------------------------------------------------------
     c*
     C                   Eval      WWNCU1='Cliente:'+s1df03
     C                   Eval      WWNCU2='Oficio:'+
     c                             %EDITW(@CINUI:'               ')
     C                   Eval      WWNCU3='                                '
     C                   Eval      WWNCU4='F6=Ver Oficio  F18=Ver Cuentas Emb. '
     C                   Eval      WWNCB1='F7=Ver Of.Rel. F19=Ver Cuenta Jud.  '
     C                   Eval      WWNCB2='                                    '
     C                   Eval      WWNCB3='                                    '
     C                   Eval      WWNCB4='F3=Salir F12=Cancelar               '
     C                   EXSR      DspErr
     c*
     c                   ExSr      ReadParms
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     c     ShowCtaJudic  BegSr
     C*--------------------------------------------------------------
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     c     ShowEmbActivosBegSr
     C*--------------------------------------------------------------
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     C*DLTTMP: Borra registros del archivo Temporal
     C*--------------------------------------------------------------
     C     DLTTMP        BEGSR
     C*
     C     KSCTM1        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBASCTM
     C     KSCTM1        READE     REBASCTM                               99
     C                   ENDDO
     C*
     C                   ENDSR
     C*--------------------------------------------------------------
     C*RarPrm: Leer Parametros
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
     C*DSPERR: Mostrar Errores
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   CALL      'BAER01RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
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
     C                   EVAL      CMDLNE='OVRDBF FILE(BASCTMRRN)'  +
     C                             ' TOFILE(*LIBL/BASCTM)'
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDLNE         1024
     C                   PARM      1024          CMDSZE           15 5
     C                   OPEN      BASCTMRRN
     C*
     C*
     C                   ENDSR
