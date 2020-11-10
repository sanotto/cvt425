*SRCMBRTXT:Link-Host to Host-Lanzar  Conexión Desd
     H DEBUG DATEDIT(*YMD)
     H******************************************************************
     H*                                                                *
     H*  APPLICATION CODE : SD V00 01                                  *
     H*                                                                *
     H*  APPLICATION NAME : Sistema Integrado De Bancos                *
     H*                                                                *
     H*  PROGRAM NAME     : LICO02MA                                   *
     H*                                                                *
     H*  PROGRAM TITLE    : LANZADOR DE CONEXIONES                     *
     H*                                                                *
     H*  DATE GENERATED   : 17/02/07                                   *
     H*                                                                *
     H*  AUTHOR           : LE00525                                    *
     H*                                                                *
     H*  IPG LEVEL        : IPG/400 VERSION 2 (R3.0)                   *
     H*                                                                *
     H*  PTF LEVEL        : PÑGÑV20114                                 *
     H*                                                                *
     H******************************************************************
     H*  MODIFICATION CONTROL                                          *
     H*  ~~~~~~~~~~~~~~~~~~~~                                          *
     H*  Date      Programmer  Description                  Ref. No.   *
     H*  ~~~~      ~~~~~~~~~~  ~~~~~~~~~~~                  ~~~~~~~~   *
     H******************************************************************
     FSGSYSV    IF   E             DISK
     F*----------------------------------------------------------------*
     FLICONN    UF   E           K DISK
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
     D MSGDS           DS
     D  CMD                    1     20
     D  USER                   1     10
     D  PASS                  11     20
     D*----------------------------------------------------------------*
     D ERR001          C                   CONST('LA CONEXION YA ESTA -
     D                                     ACTIVA              -
     D                                                    F10=B-
     D                                     AJAR CONEXION')
     D ERR002          C                   CONST('SE HA SOMETIDO LA CO-
     D                                     NEXION PARA:')
     D ERR003          C                   CONST('SE HA SOLICITADO LA -
     D                                     DESCONEXION PARA:')
     D SOLDES          C                   CONST('Solicitada Desconexi-
     D                                     ón')
     I*----------------------------------------------------------------*
     C                   EXSR      REDPRM
     C     'H2H'         CAT       WWICON:0      CONNME
     C                   CALL      'SBFNDJOB'
     C                   PARM                    CONNME           10
     C                   PARM                    USRNME           10
     C                   PARM                    JOBNBR            6
     C                   PARM                    STSFLG           10
     C                   PARM                    FNDFLG            1
     C     FNDFLG        IFEQ      *OFF
     C                   EXSR      BEGCON
     C                   ENDIF
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C*----------------
     C* VALLOG: VALIDA LOGIN
     C*----------------
     C     VALLOG        BEGSR
     C                   MOVE(P)   @CIUSR        USER
     C                   MOVE(P)   @CITER        PASS
     C                   MOVE(P)   *ZERO         ERROR
     C                   MOVE(P)   *BLANKS       ETXT
     C                   CALL      'SWAD04C1'
     C                   PARM                    USER             10
     C                   PARM                    PASS             10
     C                   PARM                    ERROR             1
     C                   PARM                    ETXT             50
     C                   MOVEL(P)  ETXT          ERRTXT
     C                   ENDSR
     C*----------------
     C* BEGCON: ARRANCAR CONEXION
     C*----------------
     C     BEGCON        BEGSR
     C*
     C     LOGAGA        TAG
     C                   CALL      'SWAD04RS'
     C                   EXSR      REDPRM
     C     @FN(03)       IFEQ      *OFF
     C     @FN(12)       ANDEQ     *OFF
     C                   EXSR      VALLOG
     C     ERROR         IFEQ      *ON
     C                   EXSR      DSPERR
     C                   GOTO      LOGAGA
     C                   ENDIF
     C*
     C                   EXSR      REFRES
     C*
     C     WWICON        CHAIN     RELICONN                           99
     C     *IN99         IFEQ      *OFF
     C                   CALL      'SWAD00CL'
     C                   PARM                    WWICON
     C     ERR002        CAT       WWICON:1      ERRTXT
     C                   EXSR      DSPERR
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDSR
     C*----------------
     C* REFRES: ENVIA UN REFRESH PREVIO AL ARRANQUE AUTOMATICO
     C*----------------
     C     REFRES        BEGSR
     C*
     C     WWICON        Chain     RELICONN                           99
     C                   Eval      CODMSG='Desc.En Progreso GENPBF'
     C                   Update    RELICONN
     C                   Call      'AC0084SW'
     C                   Parm                    AASFEI
     C                   Call      'AC0087SW'
     C                   Parm      'F'           RUNMDE            1
     C*
     C                   Z-Add     *DATE         WWFENU            8 0
     C                   Time                    WWHONU            6 0
     C                   Move      WWFENU        WWFECH            8
     C                   Move      WWFECH        WWMEDI            4
     C                   Move      WWHONU        WWHOCH            8
     C                   Move      *Blanks       WWPARA         1024
     C                   Eval      WWPARA=%TRIM(COITEL)+' '+WWMEDI
     C     WWICON        Chain     RELICONN                           99
     C                   Eval      CODMSG='Enviando PBF x FTP     '
     C                   Update    RELICONN
     C* ... Enviarlo por FTP al servidor en cuestion
     C                   MOVEL(P)  COITEL        WWIPAD           32
     C                   MOVE(P)   @CIUSR        WWIUSR           10
     C                   MOVE(P)   @CITER        WWIPAS           10
     C                   MOVE      AASFEI        WWMMDD            4
     C                   MOVEL(P)  COIFAX        WWRESV           15
     C                   MOVEL(P)  CODES2        WWPATH           77
     C                   Z-ADD     COINCP        WWPORT            7 0
     C                   Call      'SWAD00R4'
     C                   Parm                    WWIPAD
     C                   Parm                    WWIUSR
     C                   Parm                    WWIPAS
     C                   Parm                    WWRESV
     C                   Parm                    WWMMDD
     C                   Parm                    WWPATH
     C                   Parm                    WWPORT
     C*
     C*
     C     WWICON        Chain     RELICONN                           99
     C                   Eval      CODMSG='SWITCH PROCESANDO'
     C                   Update    RELICONN
     C*
     C                   EndSr
     C*----------------
     C* INICIALIZACION
     C*----------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    WWICON            6
     C*
     C     1             CHAIN     SGSYSV                             80
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
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
     C*--------------------
     C* DSPERR: MOSTRAR ERRORES
     C*--------------------
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
