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
     C     KSA02         CHAIN     REBASUPE                           99
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
     C* ... Ver si puede sup esta transacción especificamente
     C                   EXSR      CANSUP
     C*
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
     C* ... ACTUALIZAR MODULO CUENTAS A PAGAR
     C                   Z-ADD     SAICCL        WWNROCB          15 0
     C                   TIME                    WWTIME            6 0
     C/EXEC SQL
     C+ UPDATE MOD220 SET AF220 = :AASFEI, AH220 = :WWTIME
     C+ WHERE  ID220 IN (
     C+ SELECT ID220
     C+ FROM MOD220 LEFT JOIN MOD260 ON TC220 = TC260
     C+ WHERE MOD260.PA260 = :SAIPGM AND NR220 = :WWNROCB )
     C/END-EXEC
     C*
     C                   IF        SQLCODE <> 0
     C                   EVAL      ERRTXT='NO SE PUDO ACTUALIZAR EL  ' +
     C                                    'SUBISTEMA CP.COD.ERROR:'+
     C                                    %EDITC(SQLCOD:'J')
     C                   EXSR      DSPERR
     C                   LEAVESR
     C                   ENDIF
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
     C     KA#00         CHAIN     REBAPFIS                           99
     C                   EVAl      s1dacl= x'22'+'Solicitante: '  + SAIUSR +
     C                                    '-'+A#NYAP
     C                   WRITE     REBASCTM
     C*
     C                   MOVE      *BLANKS       WWDESC           50
     C                   MOVE      *BLANKS       WWTIPC            6
     C/EXEC SQL
     C+ SELECT MOD220.DS220, TC260 INTO :WWDESC , :WWTIPC FROM MOD220
     C+ LEFT JOIN MOD260 ON TC220 = TC260 WHERE MOD260.PA260 = :SAIPGM
     C+ AND NR220 = :SAICCL
     C/END-EXEC
     C*
     C                   EVAl      s1dacl=x'22'+'DESCRIPCION: ' +
     C                                    WWDESC
     C                   WRITE     REBASCTM
     C*
     C                   EVAl      s1dacl=x'22'+'CBTE.TIPO  : ' +
     C                                    WWTIPC
     C                   WRITE     REBASCTM
     C*
     C                   EVAl      s1dacl=x'22'+'CBTE.NRO.  : ' +
     C                                    %EDITC(SAICCL:'J')
     C                   WRITE     REBASCTM
     C*
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
     C                   MOVE      SAIPGM        WWIPGM           10
     C                   EXSR      GETNUS
     C                   MOVE      *OFF          SUPPER            1
     C     KSM02         SETGT     REBASUMS
     C     KSM02         READPE    REBASUMS                               99
     C                   IF        *IN99 = *OFF
     C                   TIME                    RIGNOW            6 0
     C                   MOVE      *OFF          SUPPER
     C                   IF        (SASFEI >= SMFDES AND SASFEI <= SMFHAS)  AND
     C                             SA$IMP <= SM$IMP
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
     C+ :@PUSER AND SASFEI = :AASFEI AND SAIPGM= :WWIPGM
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
     C                   KFLD                    WWIPGM
     C     KA#00         KLIST
     C                   KFLD                    CQITDO
     C                   KFLD                    CQINDO
     C     KOS00         KLIST
     C                   KFLD                    SAISUC
     C                   KFLD                    SAICCL
     C     KJV00         KLIST
     C                   KFLD                    SAISUC
     C                   KFLD                    SAINCR
     C                   KFLD                    SAIDEG
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