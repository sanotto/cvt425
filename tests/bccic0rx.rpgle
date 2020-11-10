*SRCMBRTXT:GENERA Archivo INFCOMP (DSF)           
     H DEBUG
     H DECEDIT(',') DATEDIT(*YMD/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: Gen. de Arch. INFCOMP.TXT                      *
     H*                                                               *
     H*  PROGRAM NO: BCC1XXRX                                         *
     H*                                                               *
     H*  DATE:    27/10/2015                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*  REVISO: Carolina I. Cova - PR00543                           *
     H*                                                               *
     H*****************************************************************
     FBC43CP    IF   E           K DISK
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
     D                 DS
     D  INFCOM                 1    401
   0 D  ICITII                 1      2  0
     D  ICINUI                 3     13
   0 D  ICCATC                14     15  0
     D  ICINCR                16     29
     D  ICIPOS                30     37
     D  ICIMON                38     40
     D  ICFORI                41     50
   0 D  IC$INP                51     62  0
   0 D  IC$SCN                63     74  0
   0 D  ICIPLA                75     77  0
   0 D  ICIDTI                78     78  0
     D  ICDETI                79    128
   0 D  ICFACT               129    131  0
     D  ICITNA               132    137
     D  ICICFT               138    143
   0 D  ICAMOR               144    144  0
     D  ICDEAM               145    194
   0 D  ICFEAM               195    195  0
     D  ICDEFA               196    245
   0 D  ICGPA1               246    257  0
   0 D  ICGPA2               258    269  0
   0 D  ICGPB1               270    281  0
   0 D  ICGPB2               282    293  0
   0 D  ICGPB3               294    305  0
   0 D  ICGPB4               306    317  0
   0 D  ICSGP1               318    329  0
   0 D  ICSGP2               330    341  0
   0 D  ICSGP3               342    353  0
   0 D  ICSGP4               354    365  0
     D  ICFE01               366    375
     D  ICFMDV               376    385
     D  ICTNAC               386    391
     D  ICFECH               392    401
     D*
     D*----------------------------------------------------------------*
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
     c                   Eval      OutPat='/home/regimen_informativo_val/'
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
     c                             ' RECP(PRZ825CL)                          '+
     c                             ' SUBJECT(''Archivo INFCOMP.TXT    '')    '+
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
     C                   EndSr
     c*---------------------------------------------------------------------
     c* GenFile: Graba Líneas de Archivo
     c*---------------------------------------------------------------------
     c     GenFile       BegSr
     C*
     c                   READ      BC43CP                                 25
+----C                   DoW       *IN25 = *Off
||   C                   ExSr      BuildReg
||   C                   ExSr      WrtLin
     c                   Read      BC43CP                                 25
+----C                   EndDo
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* BuildReg: Armar linea de registro en la variable buf
     c*---------------------------------------------------------------------
     c     BuildReg      BegSr
     C*
     c                   MOVEL     E6ISTR        INFCOM
     C*
     c                   Clear                   buf
     c/free
       Eval      buf=%trim(%EditW( ICITII :'  '         ))+';'+
                      %trim(       ICINUI               ) +';'+
                      %trim(%EditW(ICCATC :'  '         ))+';'+
                      %trim(       ICINCR               ) +';'+
                      %trim(       ICIPOS               ) +';'+
                      %trim(       ICIMON               ) +';'+
                      %trim(       ICFORI               ) +';'+
                      %trim(%EditW(IC$INP:'            '))+';'+
                      %trim(%EditW(IC$SCN:'            '))+';'+
                      %trim(%EditW(ICIPLA:'   '         ))+';'+
                      %trim(%EditW(ICIDTI:' '           ))+';'+
                      %trim(       ICDETI               ) +';'+
                      %trim(%EditW(ICFACT:'   '         ))+';'+
                      %trim(       ICITNA               ) +';'+
                      %trim(       ICICFT               ) +';'+
                      %trim(%EditW(ICAMOR:' '           ))+';'+
                      %trim(       ICDEAM               ) +';'+
                      %trim(%EditW(ICFEAM:' '           ))+';'+
                      %trim(       ICDEFA               ) +';'+
                      %trim(%EditW(ICGPA1:'            '))+';'+
                      %trim(%EditW(ICGPA2:'            '))+';'+
                      %trim(%EditW(ICGPB1:'            '))+';'+
                      %trim(%EditW(ICGPB2:'            '))+';'+
                      %trim(%EditW(ICGPB3 :'            ' ))+';'+
                      %trim(%EditW(ICGPB4 :'            ' ))+';'+
                      %trim(%EditW(ICSGP1 :'            ' ))+';'+
                      %trim(%EditW(ICSGP2 :'            ' ))+';'+
                      %trim(%EditW(ICSGP3 :'            ' ))+';'+
                      %trim(%EditW(ICSGP4 :'            ' ))+';'+
                      %trim(       ICFE01                 ) +';'+
                      %trim(       ICFMDV                 ) +';'+
                      %trim(       ICTNAC                 ) +';'+
                      %trim(       ICFECH                 )     +
                      x'0d'+x'25';
      /end-free
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
     c                   Eval      txtFile=%TRIM(OUTPAT)+'INFCOMP.TXT'
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
