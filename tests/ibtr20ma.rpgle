*SRCMBRTXT:Interbanking-Transferencias-Exportador 
     H DEBUG
     H DECEDIT(',') DATEDIT(*YMD/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Gen. de Arch. Txt Transf. p/Interbanking       *
     H*                                                               *
     H*                                                               *
     H*  PROGRAM NO: PRFD01MA                                         *
     H*                                                               *
     H*  DATE:    07/06/2012                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*****************************************************************
     FIBTRAN01  UF   E           K DISK
     F@CPISYS   IF A E           K DISK
     F@CPIUSD   IF A E           K DISK
     FSGSYSV    IF   E             DISK
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
     DERRDS            DS
     D ERRTXT                  1    385
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*---------------------------------------------------------------------
     D* Prototipos para las llamadas a la API
     D*---------------------------------------------------------------------
     D*------------------
     D opendir         PR              *   EXTPROC('opendir')
     D   dirname                       *   VALUE options(*string)
     D*------------------
     D closedir        PR            10I 0 EXTPROC('closedir')
     D   dirhandle                     *   VALUE
     D*------------------
     D readdir         PR              *   EXTPROC('readdir')
     D   dirhandle                     *   VALUE
     D*------------------
     D rename          PR            10I 0 EXTPROC('Qp0lRenameUnlink')
     D   old                           *   VALUE options(*string)
     D   new                           *   VALUE options(*string)

     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value options(*string)
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)

     D fseek           PR            10I 0 ExtProc('fseek')
     D  filehandler                  10I 0 value
     D  fileoffset                   10I 0 value
     D  start_pos                    10I 0 value

     D unlink          PR            10I 0 ExtProc('unlink')
     D   path                          *   Value options(*string)

     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D Close           PR            10I 0 ExtProc('close')
     D   Sock_Desc                   10I 0 Value

     D*---------------------------------------------------------------------
     D* Estructuras de Datos que devuelven las API
     D*---------------------------------------------------------------------
      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file]
      *********************************************************
     D O_WRONLY        C                   2
     D SEEK_SET        C                   0
     D O_RDWR          C                   4
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_TEXTDATA      C                   16777216
     D O_CODEPAGE      C                   8388608

     D*** file permissions
     D S_IRUSR         S             10I 0 INZ(256)
     D S_IWUSR         S             10I 0 INZ(128)
     D S_IXUSR         S             10I 0 INZ(64)
     D S_IRWXU         S             10I 0 INZ(448)
     D S_IRGRP         S             10I 0 INZ(32)
     D S_IWGRP         S             10I 0 INZ(16)
     D S_IXGRP         S             10I 0 INZ(8)
     D S_IRWXG         S             10I 0 INZ(56)
     D S_IROTH         S             10I 0 INZ(4)
     D S_IWOTH         S             10I 0 INZ(2)
     D S_IXOTH         S             10I 0 INZ(1)
     D S_IRWXO         S             10I 0 INZ(7)
     D AsciiCodePage   S             10U 0 INZ(850)
     I*----------------------------------------------------------------*
     D*-------------------------------------------------------------------------
     DHEDDS            DS
     D  HEDLIN                 1    589
     D TIPHED                  1      1
     D CODBAN                  2      4
     D NOMFIL                  5     12
     D FECPRO                 13     20
     D*-------------------------------------------------------------------------
     DFOTDS            DS
     D  FOTLIN                 1    589
     D  linfot                 2    265
     D TIPFOT                  1      1
     D PESCAN                  2      7  0
     D PESDIA                  8     13  0
     D PESFUT                 14     19  0
     D PESIMP                 20     35  0
     D PESDEB                 36     51  0
     D PESCRE                 52     67  0
     D DOLCAN                 68     73  0
     D DOLDIA                 74     79  0
     D DOLFUT                 80     85  0
     D DOLIMP                 86    101  0
     D DOLDEB                102    117  0
     D DOLCRE                118    133  0
     D PATCAN                134    139  0
     D PATDIA                140    145  0
     D PATFUT                146    151  0
     D PATIMP                152    167  0
     D PATDEB                168    183  0
     D PATCRE                184    199  0
     D LECCAN                200    205  0
     D LECDIA                206    211  0
     D LECFUT                212    217  0
     D LECIMP                218    233  0
     D LECDEB                234    249  0
     D LECCRE                250    265  0
     D*-------------------------------------------------------------------------
     DRECDS            DS
     D  RECLIN                 1    589
     D TIPREG                  1      1
     D BANDEB                  2      4
     D FECSOL                  5     12
     D NUMTRA                 13     19
     D NUMABO                 20     26
     D TIPOPE                 27     28
     D IMPORT                 29     45
     D SUCDEB                 46     49
     D NOMSOL                 50     78
     D TIPCUE                 79     80
     D NCUECM                 81     82
     D NUMCTA                 83     99
     D FSENDB                100    105
     D HSENDB                106    109
     D OPEDB1                110    111
     D OPEDB2                112    113
     D RECHDB                114    117
     D BANCRE                118    120
     D SUCCRE                121    124
     D NOMBEN                125    153
     D TIPCRE                154    155
     D CTACRE                156    172
     D FSENCR                173    178
     D HSENCR                179    182
     D OPECR1                183    184
     D OPECR2                185    186
     D RECHCR                187    190
     D OPECON                191    192
     D OPEAU1                193    194
     D OPEAU2                195    196
     D OPEAU3                197    198
     D FECAUT                199    204
     D HORAUT                205    208
     D ESTADO                209    210
     D FECEST                211    216
     D OBSER1                217    276
     D OBSER2                277    376
     D MACUNO                377    388
     D MACDOS                389    400
     D NUMREF                401    407
     D NUMENV                408    410
     D CONSOL                411    411
     D MARTIT                412    412
     D PRIVEZ                413    413
     D RIEABO                414    414
     D RIEBCO                415    415
     D TABEST                416    555
     D CTAESP                556    556
     D CUITOR                557    567
     D CUITCR                568    578
     D TAOFFD                579    583
     D TAOFFC                584    588
     D NEWLIN                589    589
     D*-------------------------------------------------------------------------
     D OUTPAT          S            255
     D txtFile         S            255
     D FILHND          S             10I 0
     d FechaN          s              8  0
     d FechaC          s              8
     D*
     d buf             s            222
     D*
     d rc              s              3  0
     D*
     d chrCode         s              9B 0
     I*----------------------------------------------------------------*
     c                   Eval      OutPat='/home/Interbanking/'+
     c                                     'Enviados/'
     c                   ExSr      ChkYaProc
     c                   ExSr      OpenStmF
     c                   ExSr      GenFile
     c                   ExSr      CloseStmF
     c                   ExSr      DspOkMsg
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* GenFile: Graba Líneas de Archivo
     c*---------------------------------------------------------------------
     c     GenFile       BegSr
     C*
     c                   ExSr      WriteHed
     C     KIT010        Chain     REIBTRAN                           25
     C                   DoW       *IN25 = *Off
     C                   ExSr      UpdAccu
     c                   ExSr      BuildReg
     C                   ExSr      WrtLin
     c                   ExSr      MarcarReg
     C     KIT010        ReadE     REIBTRAN                               25
     C                   EndDo
     c                   ExSr      WriteFot
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* UpdAccu: Actualiz Acumuladores
     c*---------------------------------------------------------------------
     c     UpdAccu       BegSr
     C*
     C                   If        ITTCDB=01 OR ITTCDB=02
     c                   Add       1             PESCAN
     c                   Add       1             PESDIA
     c                   Add       1             PESFUT
     c                   Add       IT$IMP        PESIMP
     c                   Add       IT$IMP        PESDEB
     c                   Add       IT$IMP        PESCRE
     c                   Add       1             PESCAN
     c                   Add       1             PESDIA
     c                   Add       1             PESFUT
     C                   Endif
     C                   If        ITTCDB=13 OR ITTCDB=15
     c                   Add       IT$IMP        DOLIMP
     c                   Add       IT$IMP        DOLDEB
     c                   Add       IT$IMP        DOLCRE
     C                   Endif
     c*                  Add       1             PATCAN
     c*                  Add       1             PATDIA
     c*                  Add       1             PATFUT
     c*                  Add       IT$IMP        PATIMP
     c*                  Add       IT$IMP        PATDEB
     c*                  Add       IT$IMP        PATCRE
     c*                  Add       1             LECCAN
     c*                  Add       1             LECDIA
     c*                  Add       1             LECFUT
     c*                  Add       IT$IMP        LECIMP
     c*                  Add       IT$IMP        LECDEB
     c*                  Add       IT$IMP        LECCRE
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* WriteHed: Armar Cabecera
     c*---------------------------------------------------------------------
     c     WriteHed      BegSr
     c*
     C                   Move      *BLANKS       HEDLIN
     c                   Move      1             TIPHED
     C                   Move      '309'         CODBAN
     C                   Move      'TRANSFER'    NOMFIL
     C                   Move      AASFEI        FECPRO
     c                   CallP     write(FilHnd: %addr(HEDLIN): %size(HEDLIN))
     C                   Move      *ZERO         LINFOT
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* WriteFot: Armar Pie
     c*---------------------------------------------------------------------
     c     WriteFot      BegSr
     c*
     c                   Move      3             TIPFOT
     c                   CallP     write(FilHnd: %addr(FOTLIN): %size(FOTLIN))
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* BuildReg: Armar linea de registro en la variable buf
     c*---------------------------------------------------------------------
     c     BuildReg      BegSr
     C*
     c                   Move      *ZEROS        RECLIN
     C*
     c                   Move      2             TIPREG
     c                   Move      ITINBA        BANDEB
     c                   Move      ITFING        FECSOL
     c                   Move      ITICHE        NUMTRA
     c                   Move      ITNENT        NUMABO
     c                   Move      ITITPR        TIPOPE
     c                   Move      IT$IMP        IMPORT
     c                   Move      ITDBSU        SUCDEB
     c                   Move      ITNYA1        NOMSOL
     c                   Move      ITTCDB        TIPCUE
     c                   Move      ITCLDB        NCUECM
     c                   Move      ITDF01        NUMCTA
     c                   Move      ITFEMI        FSENDB
     c                   Move      ITHEMI        HSENDB
     c                   Move      ITIPRA        OPEDB1
     c                   Move      ITIPRB        OPEDB2
     c                   Move      ITICME        RECHDB
     c                   Move      ITIBAN        BANCRE
     c                   Move      ITCRSU        SUCCRE
     c                   Move      ITNMCR        NOMBEN
     c                   Move      ITTCCR        TIPCRE
     c                   Move      ITDF02        CTACRE
     c                   Move      ITFACR        FSENCR
     c                   Move      ITHTOM        HSENCR
     c                   Move      ITITD1        OPECR1
     c                   Move      ITITD2        OPECR2
     c                   Move      ITICMI        RECHCR
     c                   Move      ITIR01        OPECON
     c                   Move      ITIR02        OPEAU1
     c                   Move      ITIR04        OPEAU2
     c                   Move      ITIR05        OPEAU3
     c                   Move      ITFAU1        FECAUT
     c                   Move      ITHAU1        HORAUT
     c                   Move      ITCOTR        ESTADO
     c                   Move      ITFUAS        FECEST
     c                   Move      ITDTLI        OBSER1
     c                   Move      ITCBA1        OBSER2
     c                   Move      ITNGT1        MACUNO
     c                   Move      ITNGT2        MACDOS
     c                   Move      ITIRFR        NUMREF
     c                   Move      ITILOT        NUMENV
     c                   Move      ITINI1        CONSOL
     c                   Move      ITINI2        MARTIT
     c                   Move      ITINI3        PRIVEZ
     c                   Move      ITINI4        RIEABO
     c                   Move      ITINI5        RIEBCO
     c                   Move      ITT160        TABEST
     c                   Move      ITINI6        CTAESP
     c                   Move      ITCUI0        CUITOR
     c                   Move      ITCUI1        CUITCR
     c                   Move      ITINU1        TAOFFD
     c                   Move      ITINU2        TAOFFC
     c                   Move      X'25'         NEWLIN
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* MarcarReg: Marcar Registros como ya enviadas
     c*---------------------------------------------------------------------
     c     MarcarReg     BegSr
     C*
     c                   Move      *Date         ITFTRA
     c                   Time                    ITHTRA
     c                   Move      @PUSER        ITIUSP
     c                   Update    REIBTRAN
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* *INZSR: Rutina de Inicialización
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     C*
     c     1             Chain     SGSYSV
     C*
     C     KIT010        KList
     C                   KFld                    AASFEI
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* CheckYaProcesados
     c*---------------------------------------------------------------------
     c     ChkYaProc     BegSr
     C*
     C                   Move      *OFF          YaEnvFlag         1
     C*
     C     KIT010        Chain     REIBTRAN                           25
     C                   DoW       *IN25 = *Off
     C                   IF        ITfv01 <> *ZERO
     C                   Move      *ON           YaEnvFlag
     C                   EndIF
     C     KIT010        ReadE     REIBTRAN                               25
     C                   EndDo
     C*
     c                   If        YaEnvFlag = *On
     c                   Eval      ErrTxt=*blanks
     C                   Eval      ErrTxt='La información correspondiente'+
     C                             ' al día: '+%EditW(AASFEI:'    /  /  ')
     C                   Eval      WWNCU2='Ya fue envíada por:'+itIUSP
     C                   Eval      WWNCB3='Presione F10 Para Reprocesar'
     C                   Eval      WWNCB4='Presione F03 Para Cancelar  '
     C                   ExSr      DspErr
     C                   ExSr      ReadParams
     C                   If        @FN(10) <> *ON
     C                   ExSr      EndPgm
     C                   EndIf
     C                   EndIf
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* DspOkMsg: Mostrar Mensaje de Finalización OK
     c*---------------------------------------------------------------------
     c     DspOkMsg      BegSr
     c*
     c                   Eval      ErrTxt=*blanks
     C                   Eval      ErrTxt='La información correspondiente'+
     C                             ' al día: '+%EditW(AASFEI:'    /  /  ')
     C                   Eval      WWNCU2='Ha sido generada y dejada en:'
     C                   Eval      WWNCU3=txtFile
     C                   Eval      WWNCB1='No olvide enviarla a Federal'
     C                   Eval      WWNCB4='Presione F03 Para Salir     '
     C                   ExSr      DspErr
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* DspErr: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DspErr        BEGSR
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
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ReadParms:Leer Parámetros
     C*---------------------------------------------------------------------
     C     ReadParams    BegSr
     C*
     C* Lee registro de Usuario
     C     @PJOBN        Chain(N)  @CPIUSD                            80
     C   80              Z-Add     @PJOBN        @ZJOBN
     C   80              Write     @CPIUSRR
     C* Lee registro de sistema
     C     @PJOBN        Chain(N)  @CPISYS                            90
     C   90              Z-Add     @PJOBN        @ZJOBN
     C   90              Write     @CPISYSR
     C*
     C                   EndSr
     C*----------------------------------------------------------------*
     C* OpnStmF: ABRE ARCHIVO EN IFS
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     c*
     c                   Z-add     *date         FechaN
     c                   Move      FechaN        FechaC
     c                   Time                    WWTIM             6 0
     c                   Move      WWTIM         CHTIM             6
     c                   Eval      txtFile=%TRIM(OUTPAT)+'PLL_TRANSFER_309'
     C*
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHnd)
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                   O_WRONLY+O_TEXTDATA)
     c*
     C                   EndSr
     C*----------------------------------------------------------------*
     C* CloseStmF: Cierra el Archivo
     C*----------------------------------------------------------------*
     C     CloseStmF     BegSr
     c                   CallP     close(FilHnd)
     C                   EndSr
     C*----------------------------------------------------------------*
     C* WrtLin: Escribe una linea en el Stream File
     C*----------------------------------------------------------------*
     C     WrtLin        BegSr
     C*
     c                   CallP     write(FilHnd: %addr(RECLIN): %size(RECLIN))
     C*
     C                   EndSr
