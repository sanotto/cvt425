*SRCMBRTXT:Sup.Adm.-Control de Supervisiones      
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA - SUPERVISIONES                          *
     H*               Válida Si transacción necesita supervisión      *
     H*                                                               *
     H*  PROGRAM NAME: BASU00RG                                       *
     H*                                                               *
     H*  PROGRAM NO:                                                  *
     H*                                                               *
     H*  DATE:     20/04/2010                                         *
     H*                                                               *
     H*  AUTHOR:                                                      *
     H*            Ottonello, Santiago                                *
     H*---------------------------------------------------------------*
     FBASULI    IF   E           K DISK
     FSGPGMS    IF   E           K DISK
     FBANUME    UF A E           K DISK
     F*
     FPRLICR    IF   E           K DISK
     FPRCRED    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FPRICTA    IF   E           K DISK
     FSGUSUA    IF   E           K DISK
     FREPERS    IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     F*
     F*
     FBASUPE    O    E           K DISK
     F*
     FSGSYSV    IF   E             DISK
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   IF   E           K DISK
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
     D*----------------------------------------------------------------*
     D ERR001          C                   CONST('LA TRANSACCION SE HA-
     D                                      SOMETIDO A SUPERVIS-
     D                                     ION')
     I*----------------------------------------------------------------*
     C                   EXSR      CHKAUT
     c* ... Por defecto, el limite es cero, es decir se sup TODOS
     C                   Z-ADD     *ZERO         WWLIMI           15 2
     c* ... Chequea si hay un limite por usuario
     C                   MOVE      @PUSER        WWUSER
     C     KSL02         SETGT     REBASULI
     C     KSL02         READPE    REBASULI                               99
     c     *IN99         IFEQ      *OFF
     C                   Z-ADD     SL$IMP        WWLIMI           15 2
     C                   ELSE
     C* ... Chequea si hay un limite por programa (Usuario 9999999999)
     C                   MOVE      *ALL'9'       WWUSER
     C     KSL02         SETGT     REBASULI
     C     KSL02         READPE    REBASULI                               99
     c     *IN99         IFEQ      *OFF
     C                   Z-ADD     SL$IMP        WWLIMI           15 2
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          PAISUP
     c* ... Si el importe supera el límite, se genera una supervisión.
     C     PA$IMP        IFGT      WWLIMI
     C                   MOVE      *ON           PAISUP
     C                   EXSR      WRTSUP
     C                   ENDIF
     c*
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C     WRTSUP        BEGSR
     C*
     C     PAIPGM        CHAIN     SGPGMS                             99
     C   99              MOVE      *BLANKS       CSISUB
     C*
     C                   EXSR      SRNUME
     C*
     C                   Z-ADD     AASFEI        SASFEI
     C                   MOVE      CSISUB        SAISUB
     C                   Z-ADD     PAISUC        SAISUC
     C                   Z-ADD     PAICCL        SAICCL
     C                   Z-ADD     PAINCR        SAINCR
     C                   Z-ADD     PAIDEG        SAIDEG
     C                   Z-ADD     *ZERO         SAIAPC
     C                   Z-ADD     *ZERO         SAITRN
     C                   Z-ADD     *ZERO         SAICAJ
     C                   Z-ADD     PAIMON        SAIMON
     C                   MOVE      *ZERO         SAICAS
     C                   TIME                    SAHORA
     C                   Z-ADD     *ZERO         SAHSUP
     C                   Z-ADD     *ZERO         SAHTOM
     C                   Z-ADD     PA$IMP        SA$IMP
     C                   MOVE      PAIPGM        SAIPGM
     C                   MOVE      *BLANKS       SAIPGP
     C                   MOVE      @PJOBN        SAITER
     C                   MOVE      *BLANKS       SAITRL
     C                   MOVE      @PUSER        SAIUSR
     C                   MOVE      *BLANKS       SAIUSA
     C                   MOVE      PAILCR        SAILCR
     C                   MOVE      PAIGRC        SAIGRC
     C                   MOVE      PAISGC        SAISGC
     C                   Z-ADD     *ZERO         SAVF01
     C                   Z-ADD     *ZERO         SAVF02
     C                   Z-ADD     *ZERO         SAVF07
     C                   Z-ADD     *ZERO         SAVF08
     C                   Z-ADD     WNIULN        SAIRRN
     C                   WRITE     REBASUPE
     C*
     C                   MOVEL     ERR001        ERRTXT
     C                   EXSR      DSPERR
     C                   EXSR      SNDEML
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* SRNUME - Obtiene Nro Correlativo de Registro
     C*----------------------------------------------------------------
     C     SRNUME        BEGSR
     C     WKEY18        CHAIN     REBANUME                           99
+----C     *IN99         IFEQ      *ON
|    C                   Z-ADD     *ZERO         WNIULN
|    C                   WRITE     REBANUME
+----C                   ELSE
|    C                   ADD       1             WNIULN
|    C                   UPDATE    REBANUME
+----C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *LIKE         DEFINE    SAIPGM        PAIPGM
     C     *LIKE         DEFINE    SA$IMP        PA$IMP
     C     *LIKE         DEFINE    SAISUC        PAISUC
     C     *LIKE         DEFINE    SAICCL        PAICCL
     C     *LIKE         DEFINE    SAINCR        PAINCR
     C     *LIKE         DEFINE    SAIDEG        PAIDEG
     C     *LIKE         DEFINE    SAIMON        PAIMON
     C     *LIKE         DEFINE    SAILCR        PAILCR
     C     *LIKE         DEFINE    SAIGRC        PAIGRC
     C     *LIKE         DEFINE    SAISGC        PAISGC
     C     *LIKE         DEFINE    @PUSER        WWUSER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAIPGM
     C                   PARM                    PA$IMP
     C                   PARM                    PAISUC
     C                   PARM                    PAICCL
     C                   PARM                    PAINCR
     C                   PARM                    PAIDEG
     C                   PARM                    PAIMON
     C                   PARM                    PAILCR
     C                   PARM                    PAIGRC
     C                   PARM                    PAISGC
     C                   PARM                    PAISUP            1
     C*
     C     *LIKE         DEFINE    OTITTL        WWITTL
     C*
     C                   Z-ADD     1             WWITTL
     C*
     C     KSL01         KLIST
     C                   KFLD                    PAIPGM
     C     KSL02         KLIST
     C                   KFLD                    PAIPGM
     C                   KFLD                    WWUSER
     C     WKEY18        KLIST
     C                   KFLD                    WNIPF1
     C                   KFLD                    WNIPF2
     C                   KFLD                    WNIPF3
     C                   KFLD                    WNIPF4
     C                   KFLD                    WNIPF5
     C     KAÑ00         KLIST
     C                   KFLD                    CQITDO
     C                   KFLD                    CQINDO
     C     KOS00         KLIST
     C                   KFLD                    SAISUC
     C                   KFLD                    SAICCL
     C     KOT00         KLIST
     C                   KFLD                    SAISUC
     C                   KFLD                    SAICCL
     C                   KFLD                    WWITTL
     C     KJV00         KLIST
     C                   KFLD                    SAISUC
     C                   KFLD                    SAINCR
     C                   KFLD                    SAIDEG
     C*
     C                   MOVEL(P)  'BASU'        WNIPF1
     C                   MOVEL(P)  'BASUPE'      WNIPF2
     C                   MOVE      *BLANKS       WNIPF3
     C                   MOVE      *BLANKS       WNIPF4
     C                   MOVE      *BLANKS       WNIPF5
     C*
     C     1             CHAIN     SGSYSV                             99
     C*
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     CHKAUT        BEGSR
     C*
     C     @PUSER        CHAIN     RESGUSUA                           99
     C   99              EXSR      ENDPGM
     C     CVILEG        CHAIN     REREPERS                           99
     C   99              EXSR      ENDPGM
     C     PAILCR        CHAIN     REPRLICR                           99
     C   99              EXSR      ENDPGM
     C*
     C     CQISEC        IFEQ      40
     C*
     c     DUITRU        IFEQ      69
     C     DUIOPE        ANDEQ     10
     c                   EXSR      ENDPGM
     C                   ENDIF
     c     DUITRU        IFEQ      46
     C     DUIOPE        ANDEQ     10
     c                   EXSR      ENDPGM
     C                   ENDIF
     c     DUITRU        IFEQ      99
     C     DUIOPE        ANDEQ     99
     c                   EXSR      ENDPGM
     C                   ENDIF
     C                   ENDIF
     C*
     c     PAILCR        IFEQ      81
     c                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     SNDEML        BEGSR
     C*
     C                   MOVE      *BLANKS       MAILAD           10
     C                   MOVE      *BLANKS       COMMAND        4096
     C                   MOVE      *BLANKS       SUBJECT          50
     C                   MOVE      *BLANKS       MESSAGE        1024
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR
     C+ SELECT DISTINCT CVIQOU FROM basums left join sgusua on
     C+ CVIUSR=smiusr  where  cviqou is not NULL AND SMIPGM=:PAIPGM
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH C1 INTO :MAILAD
     C/END-EXEC
     C*
     C     KOS00         CHAIN     REBAICCL                           99
     C   99              MOVE      *ALL'?'       OSNCCL
     C     KOT00         CHAIN     REBADCCL                           99
     C   99              MOVE      *ALL'?'       OSNCCL
     C     KJV00         CHAIN     REPRCRED                           99
     C   99              Z-ADD     *ZERO         JVILCR
     C   99              Z-ADD     *ZERO         JVITAS
     C   99              Z-ADD     *ZERO         JVQCUO
     C     JVILCR        CHAIN     REPRLICR                           99
     C   99              MOVE      *ALL'?'       DUDPLN
     C     SAIUSR        CHAIN     RESGUSUA                           99
     C     CVILEG        CHAIN     REREPERS                           99
     C     KAÑ00         CHAIN     REBAPFIS                           99
     C                   EVAl      MESSAGE  =    'EL usuario: '  +
     C                                   %TRIM(AÑNYAP)+' solicita '+
     c                                    'superv. de la Op.de Prest.:'
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    %trim(%EDITC(SAISUC:'3'))+'-' +
     C                                    %trim(%EDITC(SAINCR:'3'))+'/' +
     C                                    %trim(%EDITC(SAIDEG:'3'))
     C*
     C     KOS00         CHAIN     REBAICCL                           99
     C   99              MOVE      *ALL'?'       OSNCCL
     C     KOT00         CHAIN     REBADCCL                           99
     C   99              MOVE      *ALL'?'       OSNCCL
     C     KJV00         CHAIN     REPRCRED                           99
     C   99              Z-ADD     *ZERO         JVILCR
     C   99              Z-ADD     *ZERO         JVITAS
     C   99              Z-ADD     *ZERO         JVQCUO
     C     JVILCR        CHAIN     REPRLICR                           99
     C   99              MOVE      *ALL'?'       DUDPLN
     C     JVITAS        CHAIN     REPRICTA                           99
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Linea:' +
     C                                    %trim(%EDITC(JVILCR:'3')) +
     C                                    '-'+ %TRIM(DUDPLN)
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Plazo:      ' +
     C                                    %trim(%EDITC(JVQCUO:'3')) +
     C                                    ' TNA: ' +
     C                                    %trim(%EDITC(HZTGLI:'3'))
     C*
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Por el Imp.: ' +
     C                                    %trim(%EDITC(SA$IMP:'J'))
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Para el Clte.:'+
     C                                    %trim(%EDITC(OTINDO:'3')) +
     C                                    '-'+
     C                                    %TRIM(OSNCCL)
     C*
     C     JVITAS        CHAIN     REPRICTA                           99
     C     SQLCOD        DOWEQ     *ZERO
     C                   Eval      subject='Ped.Sup.PRESt.F/Scor.de:'+@PUSER
     C                   Eval      command='SNDMAIL RECP('''+MAILAD+''')'  +
     C                                     ' SUBJECT('''+SUBJECT+''')   ' +
     C                                     ' MESG('''+MESSAGE+''')      '
     C                   CALL      'QCMDEXC'
     C                   PARM                    Command
     C                   PARM      4096          CmdLen           15 5
     C/EXEC SQL
     C+ FETCH C1 INTO :MAILAD
     C/END-EXEC
     C                   ENDDO
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
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
