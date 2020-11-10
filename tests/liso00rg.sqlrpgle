*SRCMBRTXT:null                                   
     H DFTACTGRP(*NO) ACTGRP(*NEW)  DATFMT(*ISO) DATEDIT(*YMD)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD  ')
     FACCTAC    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     F*qsysprt   O    F  132        printer
     D*-------------------------------------------------------------------------
     D* Definitions needed to make IFS API calls.
     D*-------------------------------------------------------------------------
     D O_WRONLY        C                   2
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_CODEPAGE      C                   8388608
     D O_TEXT          C                   16777216
     D close           PR            10I 0 ExtProc('close')
     D   Sock_Desc                   10I 0 Value
     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value options(*string)
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)
     D unlink          PR            10I 0 ExtProc('unlink')
     D   path                          *   Value options(*string)
     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value
     D die             PR
     D   peMsg                      256A   const
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
     D*-------------------------------------------------------------------------
     Dfd               S             10I 0
     DSucursal         S              5P 0
     DCuenta           S             11P 0
     DLINEA            S             21A
     DWrtBuf           S             23A
     DFilNam           S            255A
     D*
     DLRNTAR           S             19A
     DLRDBTC           S              2A
     DLRDBSU           S              5P 0
     DLRDBCT           S             11P 0
     D*-------------------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    DESDE             8
     C                   PARM                    HASTA             8
     C*
     C     KEY           KLIST
     C                   KFLD                    LRDBSU
     C                   KFLD                    LRDBCT
     C*
     C                   MOVE      DESDE         DESDEN            8 0
     C                   MOVE      HASTA         HASTAN            8 0
     C*
     C                   Z-ADD     *DATE         FECNBR            8 0
     C                   move      fecnbr        fectxt            8
     C                   eval      FilNam='/home/Link/INCENTIVO_'+
     C                                    'TAR_'+fectxt+X'00'
     c                   eval      fd = open(FilNam:
     c                             O_WRONLY+O_TRUNC+O_CREAT+O_CODEPAGE:
     c                             850     )
     c                   callp     close(fd)
     c                   eval      fd = open(FilNam:
     c                             O_WRONLY+O_TEXT)
     c                   if        fd < 0
     c                   exsr      endpgm
     c                   endif
     c                   eval      linea ='FHT'+Fectxt
     C                   exsr      wrtlin
     c                   move      *zero         cntnum            8 0
     c                   exsr      OpenC1
     c                   exsr      FetchC1
     C                   dow       sqlcod =  *Zero
     C                   SETOFF                                       98
     C                   IF        LRDBTC='AC'
     C     KEY           CHAIN     REACCTAC                           99
     C  N99FUIGRC        COMP      '02'                                   98
     C                   ELSE
     C     KEY           CHAIN     RECCCTCT                           99
     C  N99BMIGRC        COMP      '02'                                   98
     C                   ENDIF
     c   98              add       1             cntnum
     c   98              Movel(p)  LRNTAR        Linea
     c*  98              except    data
     C   98              exsr      wrtlin
     c                   exsr      FetchC1
     c                   enddo
     c                   exsr      CloseC1
     C                   move      cntnum        cntchr            8
     c                   eval      linea='FT'+CNTCHR
     C                   exsr      wrtlin
     c                   callp     close(fd)
     c                   exsr      endpgm
     C*-------------------------------------------------------------------------
     C     WRTLIN        BegSr
     c                   eval      WrtBuf=*Blanks
     c                   eval      WrtBuf=LINEA+X'0D'+X'25'
     c                   callp     write(fd: %addr(WrtBuf):%Size(WrtBuf))
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C                   Seton                                        lr
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     OpenC1        BegSr
     c/exec sql
     C+ DECLARE C1 CURSOR FOR SELECT DISTINCT LRNTAR, LRDBTC, LRDBSU,
     C+ LRDBCT FROM lilogf WHERE lricon='SWITC1' and lrdire='RSP' and
     C+ lrfing >= :DESDEN and lrfing <=:HASTAN and lrtite='75'
     c/end-exec
     c/exec sql
     C+ open  C1
     c/end-exec
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     CloseC1       BegSr
     c/exec sql
     C+ CLOSE C1
     c/end-exec
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     FetchC1       BegSr
     c/exec sql
     C+ FETCH C1 Into :LRNTAR, :LRDBTC, :LRDBSU, :LRDBCT
     c/end-exec
     C                   EndSr
