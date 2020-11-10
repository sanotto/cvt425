*SRCMBRTXT:Banca Electrónica-Refresh de Cheques Ca
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     FACCTAC    IF   E           K DISK
     FACTARJ    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
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
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     I*----------------------------------------------------------------*
     D CTADS           DS
     D  CTALNK                 1     20
     D  LINSUC                 1      3  0
     D  LINFI2                 4      4
     D  LINGRP                 5      6
     D  LINFIL                 7      9  0
     D  LINCAH                10     16  0
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     d buf             s            300
     d txtFile         s            255
     D*---------------------------------------------------------------------
     c                   ExSr      OpenStmF
     c                   ExSr      WrtHeader
     c                   ExSr      OpenC1
     C                   ExSr      FetchC1
     c                   DoW       SQLCOD = *ZERO
     C                   ExSr      WrtDetail
     C                   ExSr      FetchC1
     C                   EndDo
     c                   ExSr      CloseC1
     c                   ExSr      CloseStmF
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C     OpenC1        BegSr
     C*
     c/Exec Sql
     c+ DECLARE C1 CURSOR FOR
    *C+ SELECT
    *C+ 'E'    AS C1IDCA,
    *C+ 'N'    AS C1SITU,
    *C+ CAST(       TKENDB     AS DEC(15, 0)) AS C1CHCT,
    *C+ TKNCHE AS C1CHNR, TKICPO AS C1ICPO,
    *C+ TKEMSU AS C1CHSU, TKENEM AS C1CHBC,
    *C+ TK$IMP AS C1$IMP, TKNTDO AS C1CHTD,
    *C+ TKFE01 AS C1CHFP,
    *C+ TKMOR1||' '||IFNULL(SFDERE, ''   ) AS C1CHMR,
    *C+ TKENEM AS C1BCDE, TKEMSU AS C1DESU,
    *C+ TKENDB AS C1DECC, 0 AS C1DECA
    *C+ FROM SPCHPR
    *C+ LEFT JOIN SPMORE ON      SFMOR1=TKMOR1
    *C+ WHERE
    *C+ TKFECO = :AASFEI
    *C+ AND TKCOTR = '27' AND TKITRL <> '   '
    *C+ UNION ALL
    *C+ SELECT
    *C+ 'D'    AS C1IDCA,
    *C+ 'N'    AS C1SITU,
    *C+ TKCTAD AS C1CHCT,
    *C+ TKNCHE AS C1CHNR, TKICPO AS C1ICPO,
    *C+ TKEMSU AS C1CHSU, TKENEM AS C1CHBC, TK$IMP AS C1$IMP,
    *C+ TKNTDO AS C1CHTD, TKFE01 AS C1CHFP,
    *C+ TKMOR1 AS C1CHMR, TKENEM AS C1BCDE,
    *C+ TKEMSU AS C1DESU, IFNULL(T9ICCC, 0), IFNULL(T9ICAH, 0)
    *C+ FROM SPCHPR
    *C+ LEFT JOIN SPMOVI ON T9EMSU=TKEMSU AND
    *C+                     T9ICHB=TKNCHE AND
    *C+                     T9ENEM=TKENEM
    *C+ WHERE
    *C+ TKFECO = :AASFEI
    *C+ AND TKCOTR = '27' AND TKITRL =  '   '
    *C+ UNION ALL
    *C+ SELECT
    *C+ 'D'                        AS C1IDCA,
    *C+ 'R'                        AS C1SITU,
    *C+ TKENDB                     AS C1CHCT,
    *C+ TKNCHE AS C1CHNR, TKICPO AS C1ICPO,
    *C+ TKEMSU AS C1CHSU, TKENEM AS C1CHBC, TK$IMP AS C1$IMP,
    *C+ TKNTDO AS C1CHTD, TKFE01 AS C1CHFP,
    *C+ TKMOR1||' '||IFNULL(SFDERE, ''   ) AS C1CHMR, TKENEM AS C1BCDE,
    *C+ TKEMSU AS C1DESU, IFNULL(T9ICCC, 0), IFNULL(T9ICAH, 0)
    *C+ FROM SPCHPR
    *C+ LEFT JOIN SPMORE ON SFMOR1=TKMOR1
    *C+ LEFT JOIN SPMOVI ON T9EMSU=TKEMSU AND
    *C+                     T9ICHB=TKNCHE AND
    *C+                     T9ENEM=TKENEM
    *C+ WHERE
    *C+ TKFECO = :HDFULP
    *C+ AND TKCOTR = '26'
    *C* AND TKITRL =  '   '
    *C+ UNION ALL
    *C+ SELECT
    *C+ 'E'                        AS C1IDCA,
    *C+ 'R'                        AS C1SITU,
    *C+ TKENDB                     AS C1CHCT,
    *C+ TKNCHE AS C1CHNR, TKICPO AS C1ICPO,
    *C+ TKEMSU AS C1CHSU, TKENEM AS C1CHBC, TK$IMP AS C1$IMP,
    *C+ TKNTDO AS C1CHTD, TKFE01 AS C1CHFP,
    *C+ TKMOR1||' '||IFNULL(SFDERE, ''   ) AS C1CHMR, TKENEM AS C1BCDE,
    *C+ TKEMSU AS C1DESU,        TKENDB      AS C1DECC, 0 AS C1DECA
    *C+ FROM SPCHPR
    *C+ LEFT JOIN SPMORE ON      SFMOR1=TKMOR1
    *C+ WHERE
    *C+ TKFECO = :HDFULP
    *C+ AND TKCOTR = '27'
    *C+ AND TKMOR1 <> '    '
    *C+ AND TKITRL <> '   '
     c/End-Exec
     c/Exec Sql
     c+ OPEN  C1
     c/End-Exec
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     CloseC1       BegSr
     C*
     c/Exec Sql
     c+ CLOSE C1
     c/End-Exec
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     FetchC1       BegSr
     C*
     C                   Move      *Blanks       C1IDCA            1
     C                   Move      *Blanks       C1SITU            1
     C                   Z-Add     *Zero         C1CHCT           15 0
     C                   Z-Add     *Zero         C1CHNR            8 0
     C                   Z-Add     *Zero         C1ICPO            5 0
     C                   Z-Add     *Zero         C1CHSU            5 0
     C                   Z-Add     *Zero         C1CHBC           15 0
     C                   Z-Add     *Zero         C1$IMP           15 2
     C                   Z-Add     *Zero         C1CHTD            2 0
     C                   Z-Add     *Zero         C1CHFP            8 0
     C                   Move      *Blanks       C1CHMR           34
     C                   Z-Add     *Zero         C1BCDE           15 0
     C                   Z-Add     *Zero         C1DESU            5 0
     C                   Z-Add     *Zero         C1DECC           11 0
     C                   Z-Add     *Zero         C1DECA           11 0
     C*
     c/Exec Sql
     c+ FETCH C1 INTO
     c+               :C1IDCA,
     c+               :C1SITU,
     c+               :C1CHCT,
     c+               :C1CHNR,
     c+               :C1ICPO,
     c+               :C1CHSU,
     c+               :C1CHBC,
     c+               :C1$IMP,
     c+               :C1CHTD,
     c+               :C1CHFP,
     c+               :C1CHMR,
     c+               :C1BCDE,
     c+               :C1DESU,
     c+               :C1DECC,
     c+               :C1DECA
     c/End-Exec
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtDetail     BegSr
     C*
     C                   Eval      buf=*Blanks
     c                   ExSr      ClearFields
     C                   ExSr      GetTipoCtaLnk
     c*                  Z-Add     1             CtaBEE
     c                   If        CtaBEE = *Zero
     c                   LeaveSr
     c                   EndIf
     C                   ExSr      GetCUIT
     C                   ExSr      FillFields
     C*
     C                   Eval      %subst(buf:001:008)='DETALLE '
     C                   Eval      %subst(buf:009:011)=DTICUI
     C                   Eval      %subst(buf:020:001)=DTCAMA
     C                   Eval      %subst(buf:021:001)=DTSITU
     C                   Eval      %subst(buf:022:011)=DTNCTA
     C                   Eval      %subst(buf:033:008)=DTCNRO
     C                   Eval      %subst(buf:041:004)=DTCOPO
     C                   Eval      %subst(buf:045:003)=DTISUC
     C                   Eval      %subst(buf:048:003)=DTCBCO
     C                   Eval      %subst(buf:051:010)=DT$IMP
     C                   Eval      %subst(buf:061:002)=DTTIDO
     C                   Eval      %subst(buf:063:008)=DTFEDE
     C                   Eval      %subst(buf:071:025)=DTMORE
     C                   Eval      %subst(buf:096:003)=DTBADE
     C                   Eval      %subst(buf:099:002)=DTTICU
     C                   Eval      %subst(buf:101:019)=DTCTAL
     C                   Eval      %subst(buf:120:180)=*BLANKS
     C                   Eval      %subst(buf:299:002)=X'0D'+X'25'
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf    ))
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     ClearFields   BegSr
     c*
     C                   MOVE      *Blanks       DTICUI           11
     C                   MOVE      *Blanks       DTCAMA            1
     C                   MOVE      *Blanks       DTSITU            1
     C                   MOVE      *Blanks       DTNCTA           11
     C                   MOVE      *Zeros        DTCNRO            8
     C                   MOVE      *Zeros        DTCOPO            4
     C                   MOVE      *Zeros        DTISUC            3
     C                   MOVE      *Zeros        DTCBCO            3
     C                   MOVE      *Zeros        DT$IMP           10
     C                   MOVE      *Blanks       DTTIDO            2
     C                   MOVE      *Blanks       DTFEDE            8
     C                   MOVE      *Blanks       DTMORE           25
     C                   MOVE      *Zeros        DTBADE            3
     C                   MOVE      *Zeros        DTTICU            2
     C                   MOVE      *Blanks       DTCTAL           19
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     GetCUIT       BegSr
     c*
     c                   Z-Add     *Zero         WWICUI           11 0
     c*
     C/exec sql
     C+   SELECT
     C+    MAX(CASE WHEN OSININ = 0  THEN IFNULL(AÑICUI, 0) ELSE
     C+         IFNULL(  OSININ, 0 )             END) INTO :WWICUI
     C+   FROM BAICCL
     C+   LEFT JOIN BADCCL ON OSISUC=OTISUC AND OSICCL=OTICCL
     C+                       AND OTITTL=1
     C+   LEFT JOIN BAPFIS ON OTITDO=AÑITDO AND OTINDO=AÑINDO
     C+   WHERE
     C+         OSISUC = :C1DESU
     C+     AND OSICCL = :C1ICCL
     c/end-exec
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     FillFields    BegSr
     C*
     C                   Move      WWICUI        DTICUI
     C                   MOVE      C1IDCA        DTCAMA            1
     C                   MOVE      C1SITU        DTSITU            1
     C                   MOVE      C1CHCT        DTNCTA           11
     C                   MOVE      C1CHNR        DTCNRO            8
     C                   MOVE      C1ICPO        DTCOPO            4
     C                   MOVE      C1CHSU        DTISUC            3
     C                   MOVE      C1CHBC        DTCBCO            3
     C                   MOVE      C1$IMP        DT$IMP           10
     C                   MOVE      C1CHTD        DTTIDO            2
     C                   MOVE      C1CHFP        DTFEDE            8
     C                   MOVEL(P)  C1CHMR        DTMORE           25
     C                   IF        DTMORE='SEL'
     C                   EVAL      DTMORE=*BLANKS
     C                   ENDIF
     C                   MOVE      C1BCDE        DTBADE            3
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtHeader     BegSr
     C*
     C                   ExSr      GetRecCount
     C*
     C                   Eval      %subst(buf:001:008)='CABECERA'
     C                   Eval      %subst(buf:009:004)='0309'
     C                   Eval      %subst(buf:013:008)=WWSFEI
     C                   Eval      %subst(buf:021:010)=RecCntChr
     C                   Eval      %subst(buf:299:002)=X'0D'+X'25'
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf    ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetRecCount   BegSr
     c*
     c                   Z-Add     *ZERO         RecCntNum        10 0
     c                   ExSr      OpenC1
     c                   ExSr      FetchC1
     C                   DoW       SQLCOD = *Zero
     C                   ExSr      GetTipoCtaLnk
     c                   If        CtaBEE <> *Zero
     c                   Add       1             RecCntNum        10 0
     c                   EndIf
     c                   ExSr      FetchC1
     c                   EndDo
     c                   ExSr      CloseC1
     C                   Move      RecCntNum     RecCntChr        10
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetTipoCtaLnk BegSr
     C*
     c                   Move      'CC'          CHISUB            2
     c                   Move      C1DECC        CHICCC           11
     c                   Move      C1DECC        C1ICCL            9 0
     C                   Eval      DTTICU = '00'
     c                   If        C1DECA <> 0
     c                   Move      'AC'          CHISUB            2
     c                   Move      C1DECA        CHICCC           11
     c                   Move      C1DECA        C1ICCL            9 0
     C                   Eval      DTTICU = '01'
     C                   EndIf
     C*
     c                   Move      C1DESU        CHISUC            3
     c                   Move      C1DESU        WWISUC
     c                   Move      C1ICCL        WWICAH
     C                   ExSr      GetLINKFmt
     C                   Movel     CTALNK        DTCTAL
     C*
     C                   Z-Add     *Zero         CtaBEE           15 0
     C/EXEC SQL
     C+ SELECT COUNT(*) INTO :CtaBEE FROM BECTAD WHERE ADCSTS ='V' AND
     C+ ADISUB= :CHISUB AND ADISUC =:C1DESU AND ADICCL =:C1ICCL
     C/END-EXEC
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetLINKFmt    BegSr
     c                   IF        C1DECA <> 0
     C     KEY           CHAIN     REACCTAC                           99
     C                   ELSE
     C     KEY           CHAIN     RECCCTCT                           99
     C                   ENDIF
     C                   IF        *IN99=*OFF
     c                   IF        C1DECA <> 0
     C                   Z-ADD     FUISUC        LINSUC
     C                   MOVE      FUIGRC        LINGRP
     C                   Z-ADD     FUICAH        LINCAH
     C                   Z-ADD     *ZEROS        LINFIL
     C                   MOVE      *ZEROS        LINFI2
     C                   ELSE
     C                   Z-ADD     BMISUC        LINSUC
     C                   MOVE      BMIGRC        LINGRP
     C                   Z-ADD     BMICCC        LINCAH
     C                   Z-ADD     *ZEROS        LINFIL
     C                   MOVE      *ZEROS        LINFI2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
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
     C*
     c                   callp     close(FilHnd)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     KEY           KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWICAH
     C*
     C                   Z-ADD     *ZERO         WWISUC            5 0
     C                   Z-ADD     *ZERO         WWICAH           11 0
     C*
     c     1             Chain     RESGSYSV
     c                   Move      AASFEI        WWSFEI            8
     c                   Move      AASFEI        WKSFEI            6
     C*
     C     *LoVal        SetLL     REACTARJ
     C                   Read      REACTARJ
     C*
     c                   Eval      txtFile='/home/LINKBEE/Refresh/'+
     c                                     'RBECQ.E0309000.FP'+WKSFEI+
     C                                     '.DATOS'
     C*
     C                   EndSr
