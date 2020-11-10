*SRCMBRTXT:LINK-Crea archivo de Cambio de Tarjetas
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     FSGSYSV    IF   E             DISK
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
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     d buf             s             48
     d Count           s              7S 0
     d txtfile         s            255
     d*
     d    C1NTAR       s             16  0
     d    C1ITAR       s             16  0
     c*-------------------------------------------------------------------------
     c                   ExSr      OpenStmF
     C                   ExSr      WriteHeader
     C                   ExSr      OpenC1
     C                   ExSr      FetchC1
     C                   DoW       SQLCOD = *ZERO
     c                   ExSr      WriteDetail
     C                   ExSr      FetchC1
     C                   EndDo
     C                   ExSr      WriteFooter
     c                   ExSr      CloseStmF
     C                   ExSr      CloseC1
     C                   ExSr      CloseDay
     c                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     1             Chain     RESGSYSV
     c*
     c                   Z-Add     AASFEI        WWDATE            8 0
     c                   Move      WWDATE        WWMMDD            4
     c                   Move      *Blanks       sfinme           10
     c*
     C                   Eval      sfinme='ADH'+WWMMDD
     c                   Eval      txtfile='/home/Tarjetas_de_Debitos/Masivo/'
     c                             +sfinme
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     CloseDay      BegSr
     c*
     c/exec sql
     c+ update licoad set lafpre = :aasfei where lafpre=0
     c/end-exec
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     OpenC1        BegSr
     c*-------------------------------------------------------------------------
     c/exec sql
     c+ DECLARE C1 CURSOR FOR
     c+ SELECT
     C+   LANTAR,
     C+   LAITAR
     c+ FROM LICOAD A
     c+ WHERE
     c+        A.LAFPRE = 0
     c/end-exec
     c/exec sql
     C+ OPEN C1
     c/end-exec
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     CloseC1       BegSr
     c*-------------------------------------------------------------------------
     c/exec sql
     C+ CLOSE C1
     c/end-exec
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     FetchC1       BegSr
     c*-------------------------------------------------------------------------
     c/exec sql
     C+ FETCH C1 INTO
     c+   :C1NTAR,
     c+   :C1ITAR
     c/end-exec
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     C*-------------------------------------------------------------------------
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
     C*-------------------------------------------------------------------------
     C     CloseStmF     BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   callp     close(FilHnd)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     WriteHeader   BegSr
     c*
     c                   Move      *Blanks       Buf
     c*
     c                   Time                    WWTIME            6 0
     c                   Move      WWTIME        CHTIME            6
     c                   Move      AASFEI        CHSFEI            8
     c*
     C                   Eval      %subst(buf:001:002)='FH'
     C                   Eval      %subst(buf:003:006)='0309'
     C                   Eval      %subst(buf:007:008)=CHSFEI
     C                   Eval      %subst(buf:015:006)=CHTIME
     C                   Eval      %subst(buf:021:027)=sfinme
     C*                  Eval      %subst(buf:049:002)=X'0D'+X'25'
     c*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     WriteDetail   BegSr
     C*
     C                   Add       1             Count
     C*
     c                   Move      *Blanks       Buf
     C*
     C                   MOVE      *ZEROS        TARVIE           20
     C                   MOVE      C1NTAR        TARVIE
     C                   MOVE      *ZEROS        TARNUE           20
     C                   MOVE      C1ITAR        TARNUE
     C*
     C                   Eval      %subst(buf:001:004)='0309'
     C                   Eval      %subst(buf:005:020)=TARVIE
     C                   Eval      %subst(buf:025:004)='0309'
     C                   Eval      %subst(buf:029:020)=TARNUE
     C                   Eval      %subst(buf:049:002)=X'0D'+X'25'
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     WriteFooter   BegSr
     C*
     c                   Move      *Blanks       Buf
     C*
     c                   Move      Count         CHCount           7
     C*
     C                   Eval      %subst(buf:001:002)='FT'
     C                   Eval      %subst(buf:003:007)=CHCount
     C                   Eval      %subst(buf:049:002)=X'0D'+X'25'
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     C                   EndSr
