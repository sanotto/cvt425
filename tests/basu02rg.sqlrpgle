*SRCMBRTXT:Sup.Adm.-Control de Supervisiones-Sueld
     H DEBUG DATEDIT(*YMD)
     H**********************************************************************
     H*                                                                    *
     H*  SYSTEM NAME: SIDEBA - SUPERVISIONES                               *
     H*               Sup.Adm.-Control de Supervisiones-Sueldo             *
     H*                                                                    *
     H*  PROGRAM NAME: BASU02RG                                            *
     H*                                                                    *
     H*  PROGRAM NO:                                                       *
     H*                                                                    *
     H*  DATE:     17/09/2018                                              *
     H*                                                                    *
     H*  AUTHOR:   PR00585 - Quintero Jorge                                *
     H*                                                                    *
     H*--------------------------------------------------------------------*
     FBASULI    IF   E           K DISK
     FSGPGMS    IF   E           K DISK
     FBANUME    UF A E           K DISK
     FSGUSUA    IF   E           K DISK
     FREPERS    IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     FBASUPE    O    E           K DISK
     FSGSYSV    IF   E             DISK
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   IF   E           K DISK
     D*--------------------------------------------------------------------*
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
     D*--------------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*--------------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*--------------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*--------------------------------------------------------------------*
     D ERR001          C                   CONST('LA TRANSACCION SE HA-
     D                                      SOMETIDO A SUPERVIS-
     D                                     ION')
     C*====================================================================*
     C*                    PROGRAMA PRINCIPAL
     C*====================================================================*
     C                   EXSR      CHKAUT
     C* ... Por defecto, el limite es cero, es decir se sup TODOS
     C                   Z-ADD     *ZERO         WWLIMI           15 2
     C* ... Chequea si hay un limite por usuario
     C                   MOVE      @PUSER        WWUSER
     C     KSL01         SETGT     REBASULI
     C     KSL01         READPE    REBASULI                               99
+----C     *IN99         IFEQ      *OFF
|    C                   Z-ADD     SL$IMP        WWLIMI           15 2
>    C                   ELSE
|    C* ... Chequea si hay un limite por MONTO (Usuario 9999999999)
|    C                   MOVE      *ALL'9'       WWUSER
|    C     KSL01         SETGT     REBASULI
|    C     KSL01         READPE    REBASULI                               99
|+---C     *IN99         IFEQ      *OFF
||   C                   Z-ADD     SL$IMP        WWLIMI           15 2
|+---C                   ENDIF
+----C                   ENDIF
     C* ... Si el importe supera el límite, se genera una supervisión.
     C                   MOVE      *OFF          PAISUP
+----C     PA$IMP        IFGT      WWLIMI
|    C                   MOVE      *ON           PAISUP
|    C                   EXSR      WRTSUP
+----C                   ENDIF
     C*
     C                   EXSR      ENDPGM
     C*====================================================================*
     C*CHKAUT: Chequea Autorizacion
     C*--------------------------------------------------------------------*----
     C     CHKAUT        BEGSR
     C*
     C     @PUSER        CHAIN     RESGUSUA                           99
     C   99              EXSR      ENDPGM
     C     CVILEG        CHAIN     REREPERS                           99
     C   99              EXSR      ENDPGM
     C*
     C                   ENDSR
     C*--------------------------------------------------------------------*----
     C*WRTSUP:
     C*--------------------------------------------------------------------*----
     C     WRTSUP        BEGSR
     C*
     C     PAIPGM        CHAIN     SGPGMS                             99
     C   99              MOVE      *BLANKS       CSISUB
     C*
     C                   EXSR      SRNUME
     C*
     C                   Z-ADD     AASFEI        SASFEI
     C                   Z-ADD     WNIULN        SAIRRN
     C                   MOVE      CSISUB        SAISUB
     C                   Z-ADD     PAISUC        SAISUC
     C                   Z-ADD     PAICCL        SAICCL
     C                   Z-ADD     PAINCR        SAINCR
     C                   Z-ADD     PAIDEG        SAIDEG
     C                   Z-ADD     *ZEROS        SAIAPC
     C                   Z-ADD     PAITRN        SAITRN
     C                   Z-ADD     PAICAJ        SAICAJ
     C                   Z-ADD     PAIMON        SAIMON
     C                   MOVE      *ZEROS        SAICAS
     C                   TIME                    SAHORA
     C                   Z-ADD     *ZEROS        SAHSUP
     C                   Z-ADD     *ZEROS        SAHTOM
     C                   Z-ADD     PA$IMP        SA$IMP
     C                   MOVE      PAIPGM        SAIPGM
     C                   MOVE      *BLANKS       SAIPGP
     C                   MOVE      @PJOBN        SAITER
     C                   MOVE      *BLANKS       SAITRL
     C                   MOVE      @PUSER        SAIUSR
     C                   MOVE      *BLANKS       SAIUSA
     C*                  MOVE      *BLANKS       SAILCR
     C                   MOVE      PAIGRC        SAIGRC
     C                   MOVE      PAISGC        SAISGC
     C                   Z-ADD     *ZEROS        SAVF01
     C                   Z-ADD     *ZEROS        SAVF02
     C                   Z-ADD     *ZEROS        SAVF07
     C                   Z-ADD     *ZEROS        SAVF08
     C                   WRITE     REBASUPE
     C*
     C                   EXSR      SNDEML
     C*
     C                   ENDSR
     C*--------------------------------------------------------------------*
     C* SRNUME - Obtiene Nro Correlativo de Registro
     C*--------------------------------------------------------------------*
     C     SRNUME        BEGSR
     C*
     C     KSL02         CHAIN     REBANUME                           99
+----C     *IN99         IFEQ      *ON
|    C                   Z-ADD     *ZERO         WNIULN
|    C                   WRITE     REBANUME
+----C                   ELSE
|    C                   ADD       1             WNIULN
|    C                   UPDATE    REBANUME
+----C                   ENDIF
     C*
     C                   ENDSR
     C*--------------------------------------------------------------------*----
     C*INZSR: Inicio
     C*--------------------------------------------------------------------*----
     C     *INZSR        BEGSR
     C*
     C     *LIKE         DEFINE    SAIPGM        PAIPGM
     C     *LIKE         DEFINE    SA$IMP        PA$IMP
     C     *LIKE         DEFINE    SAISUC        PAISUC
     C     *LIKE         DEFINE    SAICCL        PAICCL
     C     *LIKE         DEFINE    SAINCR        PAINCR
     C     *LIKE         DEFINE    SAIDEG        PAIDEG
     C     *LIKE         DEFINE    SAITRN        PAITRN
     C     *LIKE         DEFINE    SAICAJ        PAICAJ
     C     *LIKE         DEFINE    SAIMON        PAIMON
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
     C                   PARM                    PAITRN
     C                   PARM                    PAICAJ
     C                   PARM                    PAIMON
     C                   PARM                    PAIGRC
     C                   PARM                    PAISGC
     C                   PARM                    PAISUP            1
     C                   PARM                    RAZSOC           30
     C                   PARM                    PRODUC           30
     C                   PARM                    PACDIS            4 0
     C                   PARM                    PAIBAN            4 0
     C                   PARM                    PAIACT            3 0
     C                   PARM                    PAIPIS            3 0
     C*
     C*Acceso a BASULI
     C     KSL01         KLIST
     C                   KFLD                    PAIPGM
     C                   KFLD                    WWUSER
     C*Acceso a BANUME
     C     KSL02         KLIST
     C                   KFLD                    WNIPF1
     C                   KFLD                    WNIPF2
     C                   KFLD                    WNIPF3
     C                   KFLD                    WNIPF4
     C                   KFLD                    WNIPF5
     C*Acceso a BAPFIS
     C     KSL03         KLIST
     C                   KFLD                    CQITDO
     C                   KFLD                    CQINDO
     C*
     C                   MOVEL(P)  'BASU'        WNIPF1
     C                   MOVEL(P)  'BASUPE'      WNIPF2
     C                   MOVE      *BLANKS       WNIPF3
     C                   MOVE      *BLANKS       WNIPF4
     C                   MOVE      *BLANKS       WNIPF5
     C*
     C     1             CHAIN     SGSYSV                             99
     C*
     C                   ENDSR
     C*--------------------------------------------------------------------*----
     C*SNDEML: Envio de Correo
     C*--------------------------------------------------------------------*----
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
     C     SAIUSR        CHAIN     RESGUSUA                           99
     C     CVILEG        CHAIN     REREPERS                           99
     C     KSL03         CHAIN     REBAPFIS                           99
     C                   EVAl      MESSAGE  =    'El usuario: '  +
     C                                   %TRIM(A#NYAP)+' solicita '+
     C                                    'superv. de Acred.Sueldo.'
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Empr.: ' +
     C                                    %TRIM(%EDITC(PACDIS:'3'))+' ' +
     C                                    %TRIM(%EDITC(PAIBAN:'3'))+' ' +
     C                                    %TRIM(RAZSOC)+'-'
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Prod: ' +
     C                                    %TRIM(%EDITC(PAIACT:'3'))+' ' +
     C                                    %TRIM(%EDITC(PAIPIS:'3'))+' ' +
     C                                    %TRIM(PRODUC)+'-'
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Lote: ' +
     C                                    %trim(%EDITC(SAIDEG:'3'))+'-'
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Por el Imp.: ' +
     C                                    %TRIM(%EDITC(SA$IMP:'J'))+'-'
     C                   EVAL      MESSAGE=%TRIM(MESSAGE)+
     C                                    ' Para el periodo: '+
     C                                    %TRIM(%EDITC(SAINCR:'3'))
     C*
+----C     SQLCOD        DOWEQ     *ZERO
|    C                   Eval      subject='Ped.Sup.Acred.Sueld.de:'+@PUSER
|    C                   Eval      command='SNDMAIL RECP('''+MAILAD+''')'  +
|    C                                     ' SUBJECT('''+SUBJECT+''')   ' +
|    C                                     ' MESG('''+MESSAGE+''')      '
|    C                   CALL      'QCMDEXC'
|    C                   PARM                    Command
|    C                   PARM      4096          CmdLen           15 5
|    C/EXEC SQL
|    C+ FETCH C1 INTO :MAILAD
|    C/END-EXEC
+----C                   ENDDO
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDSR
     C*--------------------------------------------------------------------*----
     C*ENDPGM:
     C*--------------------------------------------------------------------*----
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*--------------------------------------------------------------------*----
