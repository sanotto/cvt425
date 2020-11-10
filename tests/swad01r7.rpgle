*SRCMBRTXT:Switch-Adapter      -Proc.Arch.ORIX/DES
     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
     FSPTRMH    O    E           K DISK
     FSGSYSV    IF   E             DISK
     F*
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
     D*--------------------------------------------------------------------
     D* Read From a File
     D*
     D* ssize_t read(int handle, void *buffer, size_t bytes);
     D*--------------------------------------------------------------------
     D read            PR            10I 0 ExtProc('read')
     D  handle                       10i 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value
     D*---------------------------------------------------------------------
     D* Estructuras de Datos que devuelven las API
     D*---------------------------------------------------------------------
     D/copy LE00525/SOCKETSRPG,errno_h
     D*---------------------------------------------------------------------
     D p_dirent        s               *
     D dirent          DS                  BASED(p_dirent)
     D  d_reserved1                  16A
     D  d_fileno_gid                 10U 0
     D  d_fileno                     10U 0
     D  d_reclen                     10U 0
     D  d_reserved3                  10I 0
     D  d_reserved4                   8A
     D  d_nlsinfo                    12A
     D    nls_ccsid                  10I 0 OVERLAY(d_nlsinfo:1)
     D    nls_entry                   2A   OVERLAY(d_nlsinfo:5)
     D    nls_lang                    3A   OVERLAY(d_nlsinfo:7)
     D    nls_reserv                  3A   OVERLAY(d_nlsinfo:10)
     D  d_namelen                    10U 0
     D  d_name                      640A
      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file]
      *********************************************************
     D SEEK_SET        C                   0
     D O_RDONLY        C                   1
     D O_WRONLY        C                   2
     D O_RDWR          C                   4
     D O_CREAT         C                   8
     D O_EXCL          C                   16
     D O_TRUNC         C                   64
     D O_APPEND        C                   256
     D O_CODEPAGE      C                   8388608
     D O_TEXTDATA      C                   16777216

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
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D filename        S            255A
     D error           S              1A
     D FilDes          S             10I 0
     D lasterror       S            255A
     D rc              S             10I 0
     Dbufferds         ds
     D  buffer                 1    289A
     D   header                1    289A
     D    heatar               1     25A
     D    heafec              26     31A
     D   detail                1    289A
     D    CodEntOr04           1      4A
     D    TarjeNro19           5     23A
     D    TarjeMbr03          24     26A
     D    CodEntDe04          27     30A
     D    CtaDesTi02          31     32A
     D    CtaDesNr19          33     51A
     D    CtaDesSu04          33     36
     D    CtaDesGr02          37     38
     D    CtaDesFi03          39     41
     D    CtaDesNr07          42     48
     D    CtaOriNr19          52     70A
     D    MsgLink             71     83
     D    MensTipo04          71     74A
     D    CodTLink06          75     80A
     D    CodDeRes03          81     83A
     D    DiaNegTr06          84     89A
     D    DiaCalTr06          90     95A
     D    HoraDeTr06          96    101A
     D    TermCodi04         102    105A
     D    TermNomb12         106    117A
     D    TermTipo02         118    119A
     D    NroDeSec12         120    131A
     D    NroDeSeA06         120    125
     D    NroDeSeB06         126    131
     D    MontTran10         132    141A
     D    MonedCta03         142    144A
     D    TipoCbio10         145    154A
     D    MismoTit01         155    155A
     D    NumeCUIT11         156    166A
     D    TranMoti03         167    169A
     D    TranRefe12         170    181A
     D    NombOrig22         182    203A
     D    DeTrCUIT11         204    214A
     D    CtaOrTip02         215    216A
     D    TraMonto15         217    231A
     D    FillerDe58         232    289A
     C*---------------------------------------------------------------------
     c                   exsr      opnfil
     c                   exsr      redhed
     c                   exsr      redlin
     c                   dow       *in99 = *off
     c                   exsr      prclin
     c                   exsr      redlin
     c                   enddo
     c                   exsr      clofil
     c                   exsr      endpgm
     C*---------------------------------------------------------------------
     c     prclin        begsr
     c*
     c*
     c                   MoveL     FileType      MHICSC
     c                   Move      CodEntOr04    MHENEM
     c                   Move      TarjeNro19    MHNCPA
     c                   Move      TarjeMbr03    MHITRL
     c                   Move      CodEntDe04    MHENDE
     c                   Move      CtaDesTi02    MHISUB
     c                   if        CodEntDe04 = '0309'
     c                   Move      CtaDesSu04    MHEDSU
     c                   Move      CtaDesGr02    wwigrc            2 0
     c     wwigrc        Mult      10000000      CtaDes           11 0
     c                   Move      CtaDesNr07    wwiccl           11 0
     c                   Add       wwiccl        CtaDes
     c                   Move      CtaDes        MHICCC
     c                   Else
     c                   Move      *Zeros        MHEDSU
     c                   Move      *Zeros        MHICCC
     c                   EndIf
     c                   Move      CtaOriNr19    MHCTAD
     c                   Move      MsgLink       MHDTXT
     C                   Z-Add     20000000      MHFECO
     c                   Move      DiaNegTr06    MHFECO
     C                   Z-Add     20000000      MHFPRE
     c                   Move      DiaCalTr06    MHFPRE
     c                   Move      HoraDeTr06    MHHALT
     c*                  Move      TermCodi04
     c*                  Move      TermNomb12
     c*                  Move      TermTipo02
     c*                  Move      NroDeSec12
     c                   Move      MontTran10    MH$MT1
     c*                  Move      MonedCta03
     c*                  Move      TipoCbio10
     c                   Move      MismoTit01    MHNRSO
     c                   Move      NumeCUIT11    MHICUI
     c                   Move      TranMoti03    MHCLTR
     c                   Move      TranRefe12    MHREFD
     c                   MoveL     NombOrig22    MHNCCL
     c                   Move      DeTrCUIT11    MHININ
     c*                  Move      CtaOrTip02
     c                   Move      TraMonto15    MH$MT2
     c*                  Move      FillerDe58
     c*
     c                   If        NroDeSeA06  = *Blanks
     c                   Move      NroDeSeB06    MHICHE
     c                   Else
     c                   Move      NroDeSeA06    MHICHE
     c                   EndIf
     c*
     c                   Write     RESPTRMH
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     redhed        begsr
     c*
     c                   exsr      redlin
     c     *in99         ifeq      *on
     c                   eval      lasterror='Arc. de formato incorrecto'
     c                   exsr      EndPgm
     c                   endif
     c                   if        filetype = 'ORIX'  and
     c                             heatar <> 'TRANSFERENCIAS POR ORIGEN'
     c                   eval      lasterror='Arc. de formato incorrecto'
     c                   exsr      EndPgm
     C                   endif
     c                   if        filetype = 'DESX'  and
     c                             heatar <> 'TRANSFERENCIAS POR DEST.'
     c                   eval      lasterror='Arc. de formato incorrecto'
     c                   exsr      EndPgm
     C                   endif
     c*
     C                   MOVE      heafec        AUFECH            8
     C                   MOVEL     '20'          AUFECH
     C                   MOVE      AUFECH        WWFECH            8 0
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     opnfil        begsr
     c*
     C                   EVAL      filename=%trim(filename)+X'00'
     C                   EVAL      FilDes = open(%ADDR(filename) :
     C                             O_RDONLY              + O_TEXTDATA
     C                                                         )
     C     FilDes        COMP      *ZERO                                25
     c   25              eval      lasterror=%str(strerror(errno))
     C   25              EXSR      EndPgm
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     clofil        begsr
     c*
     C                   CALLP     close(FilDes)
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     redlin        begsr
     c*
     c                   eval      lasterror=*blanks
     C                   eval      rc=read(FilDes:%ADDR(buffer):289)
     c     rc            comp      289                                  99
     c   99              eval      lasterror=%str(strerror(errno))
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     endpgm        begsr
     c*
     c                   seton                                        lr
     c                   return
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     *inzsr        begsr
     c*
     c     *entry        plist
     c                   parm                    filetype          4
     c                   parm                    filename
     c                   parm                    lasterror
     c*
     C     1             CHAIN     SGSYSV
     c*
     c                   endsr
     C*---------------------------------------------------------------------
      /define ERRNO_LOAD_PROCEDURE
      /copy SOCKETSRPG,errno_h
