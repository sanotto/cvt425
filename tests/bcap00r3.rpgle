*SRCMBRTXT:Coef.Apalancamiento - Exporta COAPALANC
     H DEBUG
     H DECEDIT(',') DATEDIT(*YMD/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: Gen. de Arch. COAPALANCA.TXT                   *
     H*                                                               *
     H*  PROGRAM NO: BCAP00R3                                         *
     H*                                                               *
     H*  DATE:    17.11.2014                                          *
     H*                                                               *
     H*  AUTHOR: CIC - PR00543                                        *
     H*                                                               *
     H*****************************************************************
     FBCAPAL02  IF   E           K DISK
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
     c                   Eval      OutPat='/home/RIESGO_OPERACIONAL/'
     c                   ExSr      OpenStmF
     c                   ExSr      GenFile
     c                   ExSr      CloseStmF
     c                   ExSr      SndMail
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* SndMail: Enviar por Mail
     c*---------------------------------------------------------------------
     c     SndMail       BegSr
     c*
     c                   Move      *Blanks       CmdLine        4096
     c                   Eval      CmdLine='SNDMAIL ' +
     c                             ' RECP(BCRL00R3)                          '+
     c                             ' SUBJECT(''Archivo COAPALANCA.TXT    '') '+
     c                             ' FILE('''+%trim(txtFile)+''')'
     c*
     c                   Call      'QCMDEXC'
     c                   Parm                    CmdLine
     c                   Parm      4096          CmdLen           15 5
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* *INZSR: Rutina de Inicialización
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     C*
     c     1             Chain     SGSYSV
     C                   ExSr      ReadParams
     C*
     c     KEY10         KLIST
     c                   KFLD                    @CFFOR
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* GenFile: Graba Líneas de Archivo
     c*---------------------------------------------------------------------
     c     GenFile       BegSr
     C*
     c     KEY10         Chain     REBCRLIQ                           25
+----C                   DoW       *IN25 = *Off
||   C                   ExSr      BuildReg
||   C                   ExSr      WrtLin
     c     KEY10         ReadE     REBCRLIQ                               25
+----C                   EndDo
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* BuildReg: Armar linea de registro en la variable buf
     c*---------------------------------------------------------------------
     c     BuildReg      BegSr
     C*
     c                   If        AP$A05 <> 0
     c                   ADD       AP$A05        WW$IMP
     c                   EndIf
     c                   Clear                   buf
     c                   Eval      buf=%trim(%EditW(
     c                                    APIGCM:'      '))+';'+
     c                                %trim(%EditW(
     c                                    WWIMON:'   '))+';'+
     c                                %trim(buf)+ %trim(%EditW(
     c                                    APIDVA:' '))+';'+
     c                                %trim(buf)+ %trim(%EditW(
     c                                    WW$IMP:'           '))+
     c                                    x'0d'+x'25'
     c                   Eval      buf=%trim(buf)

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
     c                   Z-add     AASFEI        FechaN
     c                   Move      FechaN        FechaC
     c                   Time                    WWTIM             6 0
     c                   Move      WWTIM         CHTIM             6
     c                   Eval      txtFile=%TRIM(OUTPAT)+'COAPALANCA.TXT'
     C*
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c*                                 1252    )
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
     c                   CallP     write(FilHnd: %addr(buf   ):
     c                             %scan(x'25':buf))
     C*
     C                   EndSr
