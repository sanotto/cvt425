*SRCMBRTXT:Host to Host -Consulta de Movs. Compras
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME : SIDEBA - TARJETAS DE DEBITO                    *
     H*                                                               *
     H*  PROGRAM NAME: LILR01MA                                       *
     H*                                                               *
     H*  DESCRIPTION : Consulta de Transacciones Host To Host         *
     H*                Solo compras en el Exterior                    *
     H*                                                               *
     H*  DATE        : 10.06.2013                                     *
     H*                                                               *
     H*  AUTHOR      : Carolina I. Cova - PR00543                     *
     H*                                                               *
     H*****************************************************************
     FLILOGF01  IF   E           K DISK
     F*
     FBASCTM    UF A E           K DISK
     F*
     FACCTAC07  IF   E           K DISK
     FCCCTCT03  IF   E           K DISK
     F*
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     D*----------------------------------------------------------------*
     D*----------------------------------------------------------------*
     D ERRDS           DS
     D  ERRTXT                 1    255
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
     C*  Seleccion de Cuenta
     C                   CALL      'BAOS00SV'
     C                   EXSR      REDPRM
     C     @FN(03)       DOWEQ     *OFF
     C     @FN(12)       ANDEQ     *OFF
     C*  Seleccion de Fechas
     C                   CALL      'BA0002RS'
     C                   EXSR      REDPRM
     C     @FN(03)       IFEQ      *OFF
     C                   EXSR      LODSFL
     C                   ENDIF
     C                   CALL      'BAOS00SV'
     C                   EXSR      REDPRM
     C                   ENDDO
     C*
     C                   EXSR      DLTTMP
     C*
     C                   EXSR      ENDPGM
     C*
     C*--------------------------------------------------------------
     C*LODSFL: Carga y Muestra el Subfile
     C*--------------------------------------------------------------
     C     LODSFL        BEGSR
     C*
     C                   EXSR      CHKACT
     C     WWISUB        IFEQ      *BLANKS
     C                   EVAL      ERRTXT='CUENTA INEXISTENTE'
     C                   EXSR      DSPERR
     C                   ELSE
     C                   EXSR      DLTTMP
     C     KLOGF1        SETLL     RELILOGF
     C     KLOGF2        READE     RELILOGF                               99
     C     *IN99         DOWEQ     *OFF
     C* Si no es compra, chau!
     C     LRDBCO        IFNE      8
     C                   LEAVE
     C                   ENDIF
     C* Si no es en M.Ext. chau!
     C     LRCOVE        IFEQ      0
     C                   LEAVE
     C                   ENDIF
     C* Si se pasa de la fecha hasta, chau!
     C     LRFING        IFGT      @CFHAS
     C                   LEAVE
     C                   ENDIF
     C*
     C* Sin Percepción
     C     LRFING        IFLT      20120903
     C                   Z-ADD     1             WW$POR            8 2
     C                   ENDIF
     C* Percepción 15% S/RG 3378/79 Publicado en B.Oficial 32471
     C     LRFING        IFGE      20120903
     C     LRFING        ANDLT     20130423
     C                   Z-ADD     1,15          WW$POR            8 2
     C                   ENDIF
     C* Percepción 20% S/RG 3450 Publicado en B.Oficial 32624
     C     LRFING        IFGE      20130423
     C                   Z-ADD     1,20          WW$POR            8 2
     C                   ENDIF
     C
     C*
     C                   MOVEL(P)  LRNREF        WWNREF            6
     C     LRDBIM        DIV       100           WW$IMP            8 2
     C     WW$IMP        DIV       WW$POR        WW$IM2            8 2
     C     WW$IMP        SUB       WW$IM2        WW$PER            8 2
     C     WW$IMP        SUB       WW$PER        WW$ORI            8 2
     C                   Z-ADD     @PJOBN        S1IJOB
     C                   EVAl      s1dacl=' Fec.Ing.:' +
     C                                    %EDITW(LRFING:'    /  /  ')+' '+
     C                                    'Hora Ing.:'+
     C                                    %EDITW(LRHING:'  :  :  ')+' '+
     C                                    'Fec.Asto:' +
     C                                    %EDITW(LRFASI:'    /  /  ')+' '
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl=' Trn.Link:' + LRTLNK + ' ' +
     C                                    'Cód.:' +
     C                                    %EDITC(LRDBCO:'3')+' ' +
     C                                    'Imp. Total:' +
     C                                    %EDITC(WW$IMP:'J')+' '
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl=' Percepción: ' +
     C                                    %EDITC(WW$PER:'J')+' ' +
     C                                    'Imp. Original: '+
     C                                    %EDITC(WW$ORI:'J')+' '
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl= ' Tarj.Nro:'+ LRNTAR +
     C                                     ' ATM:' + LRIATM
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl= x'24'+
     C                                    'Cbte:' + WWNREF +' '+
     C                                    'Cod.Resp:' +
     C                                    %EDITC(LRCORE:'3')+' '+
     C                                    LRROBS
     C                   WRITE     REBASCTM
     C*
     C     KLOGF2        READE     RELILOGF                               99
     C                   ENDDO
     C*
     C                   EXSR      SHOWTE
     C                   ENDIF
     C                   ENDSR
     C*--------------------------------------------------------------
     C*SHOWTE: Muestra el archivo temporal
     C*--------------------------------------------------------------
     C     SHOWTE        BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Movs. ATM Compras en el Exter.'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Subsistema .........'+
     C                                    x'22'+'LI Link                      '
     C                   EVAL      WWTIT3='Menú ...............'+
     C                                    x'22'+'CO Consultas Interactivas'
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWTIT4='...'+
     C                                    x'22'+'Seleccione para imprimir Form'
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+'Detalle de Operaciones.'
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
     C*
     c                   If        @FN(03) = *OFF AND @FN(12) =*OFF
     C                   CALL      'LILR01RG'
     C                   PARM                    WWISUB
     C                   PARM                    PAISUC
     C                   PARM                    LRDBCT
     C                   PARM                    @CFDES
     C                   PARM                    @CFHAS
     C
     c                   EndIf
     C*
     C                   ENDSR
     C*--------------------------------------------------------------
     C*CHKACT: Chequea Tipo de Cuenta
     C*--------------------------------------------------------------
     C     CHKACT        BEGSR
     C*
     C                   MOVE      *BLANKS       WWISUB
     C     KCTAC7        CHAIN     REACCTAC                           99
     C  N99              MOVE      'CA'          WWISUB
     C*
     C     KCTCT3        CHAIN     RECCCTCT                           99
     C  N99              MOVE      'CC'          WWISUB
     C*
     C                   Z-ADD     @CICCL        LRDBCT
     C                   Z-ADD     @CISUC        PAISUC            5 0
     C*
     C                   ENDSR
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
     C*REDPRM: Leer Parametros
     C*--------------------------------------------------------------
     C     REDPRM        BEGSR
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
     C*--------------------------------------------------------------
     C**INZSR: Inicializacion
     C*--------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *LIKE         DEFINE    LRDBTC        WWISUB
     C*
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C     KCTAC7        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    @CICCL
     C     KCTCT3        KLIST
     C                   KFLD                    @CISUC
     C                   KFLD                    @CICCL
     C     KLOGF1        KLIST
     C                   KFLD                    WWISUB
     C                   KFLD                    @CISUC
     C                   KFLD                    LRDBCT
     C                   KFLD                    @CFDES
     C     KLOGF2        KLIST
     C                   KFLD                    WWISUB
     C                   KFLD                    @CISUC
     C                   KFLD                    LRDBCT
     C*
     C                   ENDSR
     C*================================================================
