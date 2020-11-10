*SRCMBRTXT:Emisión de cupon de cuotas-Por Linea de
     H DEBUG DATEDIT(*YMD)
     H******************************************************************
     H*                                                                *
     H*  APPLICATION CODE : SD V00 01                                  *
     H*                                                                *
     H*  APPLICATION NAME : Sistema Integrado De Bancos                *
     H*                                                                *
     H*  PROGRAM NAME     : PRKG02R0                                   *
     H*                                                                *
     H*  PROGRAM TITLE    : LIQUIDADOR DE CUOTAS POR CAJA              *
     H*                                                                *
     H*  DATE GENERATED   : 19/08/11                                   *
     H*                                                                *
     H*  AUTHOR           : LE00525                                    *
     H*                                                                *
     H*  IPG LEVEL        : IPG/400 VERSION 2 (R3.0)                   *
     H*                                                                *
     H*  PTF LEVEL        : PÑGÑV20114                                 *
     H*                                                                *
     H******************************************************************
     H*  MODIFICATION CONTROL                                          *
     H*  ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨                                          *
     H*  Date      Programmer  Description                  Ref. No.   *
     H*  ¨¨¨¨      ¨¨¨¨¨¨¨¨¨¨  ¨¨¨¨¨¨¨¨¨¨¨                  ¨¨¨¨¨¨¨¨   *
     H******************************************************************
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     FBAICCL    IF   E           K DISK
     FBADCCL06  IF   E           K DISK
     FPRCRED08  IF   E           K DISK
     FPRCUOT06  IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     F*
     FPRMOPP    IF   E           K DISK
     FPRLICR    IF   E           K DISK
     FBASCTM    UF A E           K DISK
     FPRMOVI09  IF   E           K DISK
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     F*
     F*----------------------------------------------------------------*
     F@CPISYS   UF A E           K DISK
     F@CPIUSD   UF A E           K DISK
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
     D SCRERR          C                   CONST('EL CLIENTE SELECCION-
     D                                     ADO NO POSEE CREDITO-
     D                                     S ACTIVOS')
     I*----------------------------------------------------------------*
     C                   Z-ADD     *ZERO         PAISUC
     C                   Z-ADD     *ZERO         PAINCR
     C                   Z-ADD     *ZERO         PAIDEG
     C                   Z-ADD     *ZERO         PAICUO
     C                   Z-ADD     *ZERO         PA$IMP
     C                   CALL      'BAAÑ00SV'
     C                   EXSR      REDPRM
     C     KAÑ10         CHAIN     REBAPFIS                           99
     C   99              EXSR      ENDPGM
     C                   IF        @FN(03) = *OFF AND @FN(12) = *OFF
     C                   EXSR      BUSCUO
     C                   IF        SINCRE = *ON
     C                   MOVEL(P)  SCRERR        ERRTXT
     C                   EXSR      DSPERR
     c                   ELSE
     C                   EXSR      SHOWTE
     C                   EXSR      REDPRM
     C                   IF        @FN(03) = *OFF AND @FN(12) = *OFF
     C     @ZRRNO        CHAIN     RTMP                               99
     C                   Z-ADD     S1ISUC        PAISUC
     C                   Z-ADD     S1INCR        PAINCR
     C                   Z-ADD     S1IDEG        PAIDEG
     C                   Z-ADD     S1QCUO        PAICUO
     C                   MULT      100           S1$IMP
     C                   Z-ADD     S1$IMP        PA$IMP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C*----------------
     C* BUSCAR CUOTAS
     C*----------------
     C     BUSCUO        BEGSR
     C*
     C                   MOVE      *ON           SINCRE            1
     C*
     C     @PJOBN        CHAIN     @CPISYS                            90
     C                   MOVEL     'PRXZ01TM'    @ZPGID
     C                   UPDATE    @CPISYSR
     C*
     C     KOT61         CHAIN     REBADCCL                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        OTITTL < 20
     C     KJV80         CHAIN     REPRCRED                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        JVFEPA = *ZERO
     C     KOS00         CHAIN     REBAICCL                           99
     C                   MOVE      *OFF          SINCRE            1
     C                   EXSR      GENCUO
     C                   ENDIF
     C     KJV80         READE     REPRCRED                               99
     C                   ENDDO
     C                   ENDIF
     C     KOT61         READE     REBADCCL                               99
     C                   ENDDO
     C*
     C                   ENDSR
     C*----------------
     C* GENERAR CUOTAS
     C*----------------
     C     GENCUO        BEGSR
     C*
     C                   IF        OTITTL > 19
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   Z-Add     *ZEROS        WWFVCU            8 0
     C     KKG60         CHAIN     REPRCUOT                           99
     C   99              LEAVESR
     c*
     c                   If        WWFVCU=0
     C                   Z-Add     KGFVCU        WWFVCU
     C                   MOVEL     'IN'          PACINV            2
     C                   CALL      'SBBAINFE'
     C                   PARM                    WWFVCU
     C                   PARM                    PACINV
     c                   EndIf
     c*
     C     WKEY02        CHAIN     PRMOPP                             86
     C     JVILCR        CHAIN     REPRLICR                           99
     C                   Z-ADD     AASFEI        PASFEI            8 0
     C                   MOVEL     AASFEI        WXFAAM            6 0
     c                   MOVE      *OFF          PERSONAL          1
     c                   MOVE      *OFF          PARTICULAR        1
     C     DUITRU        IFEQ      20
     C     DUIOPE        ANDEQ     15
     C     DUITRU        OREQ      30
     C     DUIOPE        ANDEQ     50
     C     DUITRU        OREQ      40
     C     DUIOPE        ANDEQ     40
     C                   MOVE      *ON           PARTICULAR
     C                   ENDIF
     C*
     C     DUITRU        IFEQ      10
     C     DUIOPE        ANDEQ     80
     C     DUITRU        OREQ      11
     C     DUIOPE        ANDEQ     80
     C     *IN86         OREQ      *OFF
     C                   MOVE      *ON           PERSONAL
     C     KGFVCU        IFGT      AASFEI
     C     KGFICC        ANDLE     AASFEI
     C                   Z-ADD     KGFVCU        PASFEI
     C                   ELSE
     C                   Z-ADD     AASFEI        PASFEI
     C                   ENDIF
     C                   ENDIF
     c* ... Controles sobre si la cuota corresponde
     C                   IF        PERSONAL   = *OFF  AND
     C                             PARTICULAR = *OFF
     C                   LEAVESR
     C                   ENDIF
     c*
     C                   IF        JVICCL > 990000000
     C                   LEAVESR
     C                   ENDIF
     c*
     C                   IF        JVICAR  > 2
     C                   LEAVESR
     C                   ENDIF
     c*
     C                   IF        JVILCR > 2010
     C                   LEAVESR
     C                   ENDIF
     C* ... Genera Cupon
     C                   CALL      'PRHA01SC'                               pon
     C                   PARM      0             WXFAAM            6 0
     C                   PARM                    KGISUC
     C                   PARM                    KGINCR
     C                   PARM                    KGIDEG
     C                   PARM                    KGICUO
     C                   PARM                    PASFEI
     c*
     C* ... Sumariza importe del cupon
     C                   Z-ADD     *ZERO         WW$TOT           13 2
     C     WKEY09        CHAIN     PRMOVI09                           99
     C                   DOW       *in99 = *off
     C     MIIASK        COMP      '1'                                    99
     C   99              ADD       MI$IMP        WW$TOT
     C     MIIASK        COMP      '2'                                    99
     C   99              SUB       MI$IMP        WW$TOT
     C     WKEY09        READE     PRMOVI09                               99
     C                   EndDo
     C* ... Es titular  ?
     C                   Move      'Otr.'        TITULA            4
     C                   If        OTITTL=1
     C                   Move      'Tit.'        TITULA            4
     c                   EndIf
     C* ... Debe mas de una cuota ?
     c                   Z-Add     KGICUO        WWICUO
     c                   Z-Add     *zero         Impagas           3 0
     C     KKG60         CHAIN     REPRCUOT                           99
     c                   DoW       *IN99 = *OFF
     c                   If        KGFVCU <= AASFEI
     c                   Add       1             Impagas           3 0
     C                   EndIf
     C     KKG60         READE     REPRCUOT                               99
     C                   EndDo
     c                   move      *BLANKS       S1REFC
     c                   If        Impagas >  1
     c                   move      'B'           S1REFC
     C                   EndIf
     C*
     C                   Move      'Otr.'        TITULA            4
     C                   If        OTITTL=1
     C                   Move      'Tit.'        TITULA            4
     c                   EndIf
     C*
     C                   Z-ADD     @PJOBN        S1IJOB
     C                   Z-ADD     KGISUC        S1ISUC
     C                   Z-ADD     KGINCR        S1INCR
     C                   Z-ADD     KGIDEG        S1IDEG
     C                   Z-ADD     WW$TOT        S1$IMP
     C                   Z-ADD     WWICUO        S1QCUO
     C                   Z-ADD     KGINCR        WWINCR            8 0
     C                   Z-ADD     KGISUC        WWISUC            2 0
     C                   Z-ADD     KGIDEG        WWIDEG            2 0
     C                   EVAl      s1dacl=%EDITW(WWISUC: ' 0 ') + ' ' +
     C                             %EDITW(WWINCR: '       0 ') +  ' ' +
     C                             %EDITW(WWIDEG: ' 0 ') +  ' ' +
     C                             %EDITW(WWICUO: '  0 ') +  ' ' +
     C                             %EDITW(WWFVCU: '  /  /    ') +  ' ' +
     C                             %EDITW(WW$TOT: '           ,  ') +
     c                             ' ' + TITULA + ' ' + OSNCCL
     C                   WRITE     REBASCTM
     C*
     C                   ENDSR
     C*----------------
     C* INICIALIZACION
     C*----------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAISUC
     C                   PARM                    PAINCR
     C                   PARM                    PAIDEG
     C                   PARM                    PAICUO
     C                   PARM                    PA$IMP
     C*
     C     *LIKE         DEFINE    KGISUC        PAISUC
     C     *LIKE         DEFINE    KGINCR        PAINCR
     C     *LIKE         DEFINE    KGIDEG        PAIDEG
     C     *LIKE         DEFINE    KGICUO        PAICUO
     C     *LIKE         DEFINE    KG$CUC        PA$IMP
     C*
     C     1             CHAIN     SGSYSV                             80
     C*  ........LDA DATA STRUCTURE AS *LDA
     C     *DTAARA       DEFINE    *LDA          LDA
     C     *LIKE         DEFINE    KGICUO        WWICUO
     C*
     C     KOS00         KLIST
     C                   KFLD                    OTISUC
     C                   KFLD                    OTICCL
     C     KOT61         KLIST
     C                   KFLD                    @CINDO
     C     KJV80         KLIST
     C                   KFLD                    OTISUC
     C                   KFLD                    OTICCL
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C     KAÑ10         KLIST
     C                   KFLD                    @CITDO
     C                   KFLD                    @CINDO
     C     KKG60         KLIST
     C                   KFLD                    JVISUC
     C                   KFLD                    JVINCR
     C                   KFLD                    JVIDEG
     C     WKEY02        KLIST
     C                   KFLD                    KGISUC
     C                   KFLD                    KGINCR
     C                   KFLD                    KGIDEG
     C                   KFLD                    KGICUO
     C     WKEY09        KLIST
     C                   KFLD                    AASFEI
     C                   KFLD                    KGISUC
     C                   KFLD                    KGINCR
     C                   KFLD                    KGIDEG
     C                   KFLD                    KGICUO
     C*
     C                   EVAL      CMDLNE='OVRDBF FILE(BASCTMRRN)'  +
     C                             ' TOFILE(*LIBL/BASCTM)'
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDLNE         1024
     C                   PARM      1024          CMDSZE           15 5
     C                   OPEN      BASCTMRRN
     C*
     C                   EXSR      DLTTMP
     C*
     C                   ENDSR
     C*----------------
     C* REDPRM:LEER PARAMETROS
     C*----------------
     C     REDPRM        BEGSR
     C* Lee registro de Usuario
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C   80              Z-ADD     @PJOBN        @ZJOBN
     C   80              WRITE     @CPIUSRR
     C* Lee registro de sistema
     C     @PJOBN        CHAIN(N)  @CPISYS                            90
     C   90              Z-ADD     @PJOBN        @ZJOBN
     C   90              WRITE     @CPISYSR
     C                   ENDSR
     C*----------------
     C* UPDPRM: ACTUALIZAR PARAMETROS
     C*----------------
     C     UPDPRM        BEGSR
     C                   UPDATE    @CPIUSRR
     C                   UPDATE    @CPISYSR
     C                   ENDSR
     C*----------------
     C* ENDPGM: SALIR DEL PROGRAMA
     C*----------------
     C     ENDPGM        BEGSR
     C*
     C                   CLOSE     BASCTMRRN
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
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
     C     SHOWTE        BEGSR
     C*--------------------------------------------------------------
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Seleccione Cuota a Cobrar'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='D.N.I......: ' +
     c                                    %EDITW(@CINDO:'               ')
     C                   EVAL      WWTIT3='Cliente ...: ' +
     C                                    AÑNYAP
     c                   IF        Impagas > 1
     C                   EVAL      WWTIT4= '                           '+
     C                                    x'28'+'Posee mas de una cuota ' +
     C                                          'vencida impaga'
     C                   Else
     c                   Eval      WWTIT4=''
     c                   EndIf
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+
     C                                            'Suc.'  + ' ' +
     C                                            'Credito '  +  ' ' +
     C                                            'Dg.'  +  ' ' +
     C                                            'Cta '  +  ' ' +
     C                                            'Fecha Vto.'  +  ' '  +
     C                                            '      Importe' + ' '+
     C                                            'Tit'  +  ' ' +
     C                                            'Cliente'
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
