*SRCMBRTXT:Sup.Adm.-Adm. de Supervisiones         
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME : SIDEBA - PRESTAMOS                             *
     H*                                                               *
     H*  PROGRAM NAME: BASU00MA                                       *
     H*                                                               *
     H*  DESCRIPTION : Supervisiones Pendientes                       *
     H*                                                               *
     H*  PROGRAM NO  :                                                *
     H*                                                               *
     H*  DATE        : 20/04/2010                                     *
     H*                                                               *
     H*  AUTHOR      : Ottonello, Santiago                            *
     H*                                                               *
     H*---------------------------------------------------------------*
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     FBASUPE    UF   E           K DISK
     FBASUMS    IF   E           K DISK
     F*
     FBASCTM    UF A E           K DISK
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     F*
     FSGPGMS    IF   E           K DISK
     FSGUSUA    IF   E           K DISK
     FREPERS    IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     F*
     FSGSYSV    IF   E             DISK
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
     C                   EXSR      LODSFL
     C                   DOW       @FN(03) = *OFF AND @FN(12) = *OFF
     C     @ZRRNO        CHAIN     RTMP                               99
     C                   Z-ADD     S1INCR        WWIRRN
     C                   EXSR      SUPERV
     C                   EXSR      LODSFL
     C                   ENDDO
     C                   EXSR      DLTTMP
     C                   EXSR      ENDPGM
     C*--------------------------------------------------------------
     C*SUPERV: Supervisar Operación
     C*--------------------------------------------------------------
     C     SUPERV        BEGSR
     C*
     C                   EXSR      CANSUP
     C                   IF        SUPPER = *ON
     C                   CALL      'SWAD04RS'
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            90
     C                   IF        @FN(03) = *OFF AND @FN(12) = *OFF
     C                   MOVE(P)   @CIUSR        USER
     C                   MOVE(P)   @CITER        PASS
     C                   MOVE      *ZERO         ERROR
     C                   MOVE      *BLANKS       ETXT
     C                   CALL      'SWAD04C1'
     C                   PARM                    USER             10
     C                   PARM                    PASS             10
     C                   PARM                    ERROR             1
     C                   PARM                    ETXT             50
     C                   MOVEL(P)  ETXT          ERRTXT
     C                   IF        ERROR = *ON
     C                   EXSR      DSPERR
     C                   ELSE
     C     KSA02         CHAIN     REBASUPE                           99
     C                   IF        *IN99 = *OFF
     C                   IF        SAHSUP = *ZERO
     C                   TIME                    SAHSUP
     C                   MOVE      @PUSER        SAIUSA
     C                   UPDATE    REBASUPE
     C                   ELSE
     C                   EVAL      ERRTXT='Transacción ya Supervisada' +
     C                                    ' por :'+ SAIUSA +' A las:' +
     C                                    %EDITW(SAHSUP : '  :  :  ')
     C                   EXSR      DSPERR
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      ERRTXT='Ya no puede supervisar ' +
     C                                    'la Transacción.'+
     C                                    'Autorización expirada '
     C                   EXSR      DSPERR
     C                   ENDIF
     C*
     C                   ENDSR
     C*--------------------------------------------------------------
     C*LODSFL: Carga y Muestra el Subfile
     C*--------------------------------------------------------------
     C     LODSFL        BEGSR
     C*
     C                   EXSR      DLTTMP
     C*
     C                   Z-ADD     @PJOBN        S1IJOB
     C     KSA01         CHAIN     REBASUPE                           99
     C                   DOW       *IN99 = *OFF
     C                   EXSR      CANSUP
     C                   IF        SAHSUP = *ZERO AND SUPPER = *ON
     C                   Z-ADD     SAIRRN        S1INCR
     C     SAIPGM        CHAIN     SGPGMS                             99
     C   99              MOVE      *ALL'?'       CSDPGM
     C                   EVAl      s1dacl=x'22'+ 'Subsistema : '  + SAISUB +
     C                                    ' Transacción: ' + SAIPGM +
     C                                    '-'+CSDPGM
     C                   WRITE     REBASCTM
     C     SAIUSR        CHAIN     RESGUSUA                           99
     C     CVILEG        CHAIN     REREPERS                           99
     C     KAÑ00         CHAIN     REBAPFIS                           99
     C                   EVAl      s1dacl= x'22'+'Solicitante: '  + SAIUSR +
     C                                    '-'+AÑNYAP
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl= ' Sucursal ..:                '+
     C                                    %EDITC(SAISUC:'3')+' '     +
     C                                    '  Cta.Clte...: ' +
     C                                    %EDITW(SAICCL:'        / ')
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl=' N° Oper....:      ' +
     C                                    %EDITC(SAINCR:'3') +
     C                                    '   Desglose...:       ' +
     C                                    %EDITC(SAIDEG:'3')
     C                   WRITE     REBASCTM
     C                   EVAl      s1dacl=x'24'+'IMPORTE....: ' +
     C                                    %EDITC(SA$IMP:'J')+' '     +
     C                                    ' Hora Solic.:  ' +
     C                                    %EDITW(SAHORA:'  :  :  ')
     C                   WRITE     REBASCTM
     C                   ENDIF
     C     KSA01         READE     REBASUPE                               99
     C                   ENDDO
     c*
     C                   EXSR      SHOWTE
     C                   ENDSR
     C*--------------------------------------------------------------
     C*CANSUP: Puede Supervisar esta transacción ?
     C*--------------------------------------------------------------
     C     CANSUP        BEGSR
     C*
     C                   EXSR      GETNUS
     C                   MOVE      *OFF          SUPPER            1
     C     KSM02         SETGT     REBASUMS
     C     KSM02         READPE    REBASUMS                               99
     C                   IF        *IN99 = *OFF
     C                   TIME                    RIGNOW            6 0
     C                   MOVE      *OFF          SUPPER
     C                   IF        (SASFEI >= SMFDES AND SASFEI <= SMFHAS)  AND
     C                             SA$IMP <= SM$IMP                         AND
     C                             (ACUSUP + SM$IMP)  <= SM$SAL
     C                   MOVE      *ON           SUPPER            1
     C                   ENDIF
     C                   IF        (SASFEI = SMFDES AND SMHALT > RIGNOW)
     C                   MOVE      *OFF          SUPPER
     C                   ENDIF
     C                   IF        (SASFEI = SMFHAS AND SMHBAJ < RIGNOW)
     C                   MOVE      *OFF          SUPPER
     C                   ENDIF
     C                   IF        (SMFHAS = SMFDES AND SMFDES = SASFEI) AND
     C                             (SMHALT > RIGNOW OR RIGNOW > SMHBAJ)
     C                   MOVE      *OFF          SUPPER
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*--------------------------------------------------------------
     C*GETNUS: Obtiene el Total de Montos supervisados por este supervisor
     C*--------------------------------------------------------------
     C     GETNUS        BEGSR
     C*
     C                   Z-ADD     *ZERO         ACUSUP           15 2
     c/EXEC SQL
     C+ SELECT SUM(SA$IMP) INTO :ACUSUP FROM BASUPE WHERE SAIUSA =
     C+ :@PUSER AND SASFEI = :AASFEI
     C/END-EXEC
     C*
     C                   ENDSR
     C*--------------------------------------------------------------
     C*SHOWTE: Muestra el archivo temporal
     C*--------------------------------------------------------------
     C     SHOWTE        BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Supervisiones Pendientes'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Subsistema .........'+
     C                                    x'22'+'SG Seguridad                 '
     C                   EVAL      WWTIT3='Menú ...............'+
     C                                    x'22'+'OF Operaciones de Oficina'
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+'Supervisiones Pendientes'
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
     C     *LIKE         DEFINE    SAIRRN        WWIRRN
     C*
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C     KSA01         KLIST
     C                   KFLD                    AASFEI
     C     KSA02         KLIST
     C                   KFLD                    AASFEI
     C                   KFLD                    WWIRRN
     C     KSM02         KLIST
     C                   KFLD                    SAISUB
     C                   KFLD                    @PUSER
     C     KAÑ00         KLIST
     C                   KFLD                    CQITDO
     C                   KFLD                    CQINDO
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
     C                   ENDSR
