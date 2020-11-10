*SRCMBRTXT:Ces.Hab.-Gen Plano            -Fto Fede
     H DEBUG
     H DECEDIT(',') DATEDIT(*YMD/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Gen. de Arch. Txt Envío de Cuotas a Cobrar     *
     H*                Formato Acordado por Cortes con Federal        *
     H*                                                               *
     H*  PROGRAM NO: PRHA02R4                                         *
     H*                                                               *
     H*  DATE:    25/10/2013                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*****************************************************************
     FPRHAMV01  UF   E           K DISK
     FBAPFIS05  IF   E           K DISK
     FBASCOR04  IF   E           K DISK
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
     d buf             s            160
     D*
     d rc              s              3  0
     D*
     d chrCode         s              9B 0
     I*----------------------------------------------------------------*
     c                   Eval      OutPat='/home/Cesion_de_haberes/Federal/'+
     c                                     'Enviados/'
     c                   ExSr      OpenStmF
     c     KMV012        Chain     REPRHAMV                           99
     c                   DoW       *In99  = *Off
     c                   ExSr      FixData
     c                   ExSr      BuildLine
     c                   ExSr      WriteLine
     c                   If        DataFixed = *On
     c                   Update    REPRHAMV
     c                   EndIf
     c     KMV012        ReadE     REPRHAMV                               99
     c                   EndDo
     c                   ExSr      CloseStmF
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* FixData: Arregla los datos del archivo PRHAMV
     c*---------------------------------------------------------------------
     c     FixData       BegSr
     C*
     C* ... Acceder a BAPFIS para recuperar el CUIL
     c     MVINDO        Chain     REBAPFIS                           99
     C* ... Recuperar Sexo de la persona para el recibo
     c                   ExSr      GetSex
     C* ... Bandera para no hacer actualizaciones al pedo
     c                   Move      *Off          DataFixed         1
     c* ... Convierte el Nro de Job de Formato NBLR a Formato Federal
     c                   ExSr      FixJob
     c* ... Valida que créditos Línea 8 no sobrepasen el monto máximo
     c                   ExSr      FixImp
     c* ... Arregla el código de descuento para credito línea 8
     c                   ExSr      FixIMDS
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* GetSex: (If you're lucky]) Recuperar el sexo
     c*---------------------------------------------------------------------
     c     GetSex        BegSr
     c*
     C                   Move      *Blanks       WWISEX            1
     c* ... Primero Buscar en el Scoring
     C     KSC040        Chain     REBASCOR                           99
     c                   If        *In99 = *off
     C                   Move      SCISEX        WWISEX
     c                   EndIf
     C* ... El sexo en scoring es valido? Sino Usar el del BAPFIS
     c                   If        WWISEX <> 'M' Or
     c                             WWISEX <> 'F'
     c                   Move      AÑISEX        WWISEX
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* FixIMDS: Arregla el código de descuento para credito línea 8
     c*---------------------------------------------------------------------
     c     FixIMDS       BegSr
     c*
     c                   If        MVIMDS = *Zeros
     c                   Move      *On           DataFixed
     c                   Move      570           MVIMDS
     c                   EndIf
     c*
     C                   If        MVILCR = 8 Or
     C                             MVILCR = 2008 Or
     C                             MVILCR = 2738
     c                   Move      *On           DataFixed
     C                   Move      572           MVIMDS
     C                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* FixImp: Valida que créditos Línea 8 no sobrepasen el monto máximo
     c*---------------------------------------------------------------------
     c     FixImp        BegSr
     c*
     C                   Z-Add     MV$IMP        PA$IMP
     c*
     C                   If        MVIEMP = 1
     C                   If        MVILCR = 8 Or
     C                             MVILCR = 2008 Or
     C                             MVILCR = 2738
     C                   CALL      'PRHA02RR'
     C                   Parm                    MVIEMP
     C                   Parm                    MVINDO
     C                   Parm                    MVISUC
     C                   Parm                    MVINCR
     C                   Parm                    MVIDEG
     C                   Parm                    PA$IMP
     c*
     C                   EndIf
     C                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* FixJob: Convierte el Nro de Job de Formato NBLR a Formato Federal
     c*---------------------------------------------------------------------
     c     FixJob        BegSr
     C*
     c                   If        MVIRED = ' '
     c                   Move      '1'           MVIRED
     c                   EndIf
     c*
     c                   Move      '01'          wwired            2
     C                   Move      MVIRED        wwired
     c*
     C                   SELECT
     C     WWIRED        WHENEQ    '0A'
     C                   MOVE      '10'          WWIRED
     C     WWIRED        WHENEQ    '0B'
     C                   MOVE      '11'          WWIRED
     C     WWIRED        WHENEQ    '0C'
     C                   MOVE      '12'          WWIRED
     C     WWIRED        WHENEQ    '0D'
     C                   MOVE      '13'          WWIRED
     C     WWIRED        WHENEQ    '0E'
     C                   MOVE      '14'          WWIRED
     C     WWIRED        WHENEQ    '0F'
     C                   MOVE      '15'          WWIRED
     C     WWIRED        WHENEQ    '0G'
     C                   MOVE      '16'          WWIRED
     C     WWIRED        WHENEQ    '0H'
     C                   MOVE      '17'          WWIRED
     C     WWIRED        WHENEQ    '0I'
     C                   MOVE      '18'          WWIRED
     C     WWIRED        WHENEQ    '0J'
     C                   MOVE      '19'          WWIRED
     C     WWIRED        WHENEQ    '0K'
     C                   MOVE      '20'          WWIRED
     C     WWIRED        WHENEQ    '0L'
     C                   MOVE      '21'          WWIRED
     C     WWIRED        WHENEQ    '0M'
     C                   MOVE      '22'          WWIRED
     C     WWIRED        WHENEQ    '0N'
     C                   MOVE      '23'          WWIRED
     C     WWIRED        WHENEQ    '0O'
     C                   MOVE      '24'          WWIRED
     C     WWIRED        WHENEQ    '0P'
     C                   MOVE      '25'          WWIRED
     C     WWIRED        WHENEQ    '0Q'
     C                   MOVE      '26'          WWIRED
     C     WWIRED        WHENEQ    '0R'
     C                   MOVE      '27'          WWIRED
     C     WWIRED        WHENEQ    '0S'
     C                   MOVE      '28'          WWIRED
     C     WWIRED        WHENEQ    '0T'
     C                   MOVE      '29'          WWIRED
     C     WWIRED        WHENEQ    '0U'
     C                   MOVE      '30'          WWIRED
     C     WWIRED        WHENEQ    '0V'
     C                   MOVE      '31'          WWIRED
     C     WWIRED        WHENEQ    '0W'
     C                   MOVE      '32'          WWIRED
     C     WWIRED        WHENEQ    '0X'
     C                   MOVE      '33'          WWIRED
     C     WWIRED        WHENEQ    '0Y'
     C                   MOVE      '34'          WWIRED
     C     WWIRED        WHENEQ    '0Z'
     C                   MOVE      '35'          WWIRED
     C                   ENDSL
     C     WWIRED        IFEQ      '00'
     C                   MOVE      '01'          WWIRED
     C                   ENDIF
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* BuildLine: Arma la línea del Archivo
     c*---------------------------------------------------------------------
     c     BuildLine     BegSr
     c                   Eval      Buf=*all' '
     c                   Eval      %subst(buf:001:006)=wwfaas
     c                   Z-Add     MVINDO        WWINDO            8 0
     c                   Eval      %subst(buf:007:008)=%trim(
     C                                           %EditW(WWINDO:'0        '))
     c                   Eval      %subst(buf:015:005)=MVIBCF
     c                   Eval      %subst(buf:020:002)=WWIRED
     c                   Eval      %subst(buf:022:001)=WWISEX
     c                   Eval      %subst(buf:023:025)=%subst(MVNYAP:1:25)
     c                   Eval      %subst(buf:048:003)=
     c                                                 %EditW(MVIMDS:'   ')
     c                   Z-add     MVISUC        wwisuc            2 0
     c                   Z-add     MVINCR        wwincr           10 0
     c                   Z-add     MVIDEG        wwideg            2 0
     c                   Eval      %subst(buf:051:014)=%trim(
     C                                           %EditW(wwisuc:'0  '))+
     c                                                 %trim(
     c                                   %EditW(wwincr:'0          '))+
     c                                                 %trim(
     C                                           %EditW(wwideg:'0  '))
     C                   MOVE      MVICUO        WWICUO            3
     c                   Eval      %subst(buf:065:003)=WWICUO
     C                   MOVE      AÑICUI        WWICUI           11
     c                   Eval      %subst(buf:068:011)=WWICUI
     C                   MOVE      MV$IMP        WW$IMP           15
     c                   Eval      %subst(buf:079:015)=WW$IMP
     c                   Eval      %subst(buf:094:015)='000000000000000'
     c                   Eval      %subst(buf:109:050)=*Blanks
     C*
     c                   Eval      %subst(buf:159:002)=X'0D'+X'25'
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
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
     C*----------------------------------------------------------------
     c* INZSR: Inicialización del Programa
     C*----------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     C*
     C     *Entry        PList
     C                   Parm                    wwfaas            6
     C                   Parm                    wwiemp            5
     C*
     c     KMV012        KList
     c                   KFld                    MVFAAM
     c                   KFld                    MVIEMP
     C*
     c     KSC040        KList
     c                   KFld                    MVISUC
     c                   KFld                    MVINCR
     c                   KFld                    MVIDEG
     C*
     C     *LIKE         Define    MV$IMP        PA$IMP
     C*
     C                   Move      WWFAAS        MVFAAM
     C                   Move      WWIEMP        MVIEMP
     C*
     C                   EndSr
     C*----------------------------------------------------------------*
     C* OpnStmF: ABRE ARCHIVO EN IFS
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     c*
     c                   Eval      txtFile=%TRIM(OUTPAT)+'NBR'+WWFAAS+
     C                                     '.txt'
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
     C* WriteLine: Escribe una linea en el Stream File
     C*----------------------------------------------------------------*
     C     WriteLine     BegSr
     C*
     c                   CallP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
