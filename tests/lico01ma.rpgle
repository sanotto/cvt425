*SRCMBRTXT:Link-Host to Host-Manejador Menu de Con
     H DEBUG DATEDIT(*YMD)
     H******************************************************************
     H*                                                                *
     H*  APPLICATION CODE : SD V00 01                                  *
     H*                                                                *
     H*  APPLICATION NAME : Sistema Integrado De Bancos                *
     H*                                                                *
     H*  PROGRAM NAME     : PRHA00MA                                   *
     H*                                                                *
     H*  PROGRAM TITLE    : MANEJADOR DE LLAMADA A SWITCH              *
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
     FLICONN    UF   E           K DISK
     FSGSYSV    IF   E             DISK
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
     D FTPOK           C                   CONST('PBF ENVIADO')
     D SWIPRO          C                   CONST('SWITCH PROCESANDO')
     D FTPERR          C                   CONST('NO SE PUDO ENVIAR EL-
     D                                     PBF POR FTP-USE PROC-
     D                                     EDIMIENTO DE CONTING-
     D                                     ENCIA')
     I*----------------------------------------------------------------*
     C     WWICON        CHAIN     RELICONN                           99
     C     CODMSG        IFNE      FTPOK
     C     CODMSG        ORNE      SWIPRO
     C                   MOVEL(P)  FTPERR        ERRTXT
     C                   EXSR      DSPERR
     C                   ENDIF
     C     CODMSG        IFNE      SWIPRO
     C                   MOVE      *BLANKS       CODMSG
     C                   ENDIF
     C                   UPDATE    RELICONN
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C*----------------
     C* INICIALIZACION
     C*----------------
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    WWICON
     C*
     C     1             CHAIN     SGSYSV                             80
     C*  ........LDA DATA STRUCTURE AS *LDA
     C     *DTAARA       DEFINE    *LDA          LDA
     C     *LIKE         DEFINE    COIBCF        WWICON
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
