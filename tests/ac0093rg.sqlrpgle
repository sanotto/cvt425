*SRCMBRTXT:LINK: Refresh de Jubilados para Biometr
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
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
     F*----------------------------------------------------------------*
     D NormalizarCUIT  PR            20A
     D  inCUIT                       15A
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     d buf             s            506
     d txtfile         s            255
     d txtfiled        s            255
     d*
     d    C1ICUI       s             12
     d    C1NCAL       s             30
     d    C1IPUE       s              6
     d    C1IPIS       s              3
     d    C1IDPT       s              4
     d    C1ANEX       s             50
     d    C1TORR       s             50
     d    C1SECT       s             50
     d    C1MANZ       s             50
     d    C1NPAI       s             30
     d    C1NPRO       s             30
     d    C1NLOC       s             30
     d    C1ICPO       s              5
     d    C1ITEL       s             15
     d    C1TITE       s              4
     d    C1COEL       s             50
     d    C1NYAA       s             27
     d    C1APEL       s             50
     d    C1INDA       s              8
     c*-------------------------------------------------------------------------
     c                   ExSr      OpenStmF
     C                   ExSr      OpenC1
     C                   ExSr      FetchC1
     C                   DoW       SQLCOD = *ZERO
     c                   ExSr      WriteRecord
     C                   ExSr      FetchC1
     C                   EndDo
     c                   ExSr      CloseStmF
     C                   ExSr      CloseC1
     c                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    txtFile         255
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
     c+ SELECT DISTINCT
     c+  CAST(IFNULL(AÑICUI,  0) AS CHAR(12))   AS CUIL ,
     c+  IFNULL(AENCAL, '')                     AS CALLE ,
     c+  CAST(IFNULL(AEIPUE,  0) AS CHAR(6))    AS NUMERO ,
     c+  CAST(IFNULL(AEIPIS,  0) AS CHAR(3))    AS PISO,
     c+  IFNULL(AEIDPT, '')                     AS DEPTO,
     c+  CAST(' '  AS CHAR(50))                 AS ANEXO,
     c+  CAST(' '  AS CHAR(50))                 AS TORRE,
     c+  CAST(' '  AS CHAR(50))                 AS SECTOR,
     c+  CAST(' '  AS CHAR(50))                 AS MANZANA,
     c+  IFNULL(ANNPAI, '')                     AS PAIS,
     c+  IFNULL(APNPRO, '')                     AS PROVINCIA,
     c+  IFNULL(ALNLOC, '')                     AS LOCALIDAD,
     c+  CAST(IFNULL(AEICPO, 0 ) AS CHAR(5))    AS CODPOST,
     c+  IFNULL(AÑITEL, '')      AS TELEFONO,
     c+  CAST('FIJO' AS CHAR(4)) AS TIPO_TELEFONO,
     c+  IFNULL(QQCOEL,'')       AS EMAIL,
     c+  ANNYAA                  AS NOMBRE,
     c+  CAST(' ' AS CHAR(50))   AS APELLIDO,
     c+  CAST(ANINDA AS CHAR(8)) AS DOCUMENTO
     c+
     c+
     C+      FROM (SELECT ANINDA, MIN(ANNYAA) AS ANNYAA FROM  SDBFIL/ANLIHA
     C+      GROUP BY ANINDA
     C+       )
     C+
     c+ LEFT JOIN SDBFIL/BAPFIS ON ANINDA=AÑINDO
     c+ LEFT JOIN SDBFIL/DIRPFU ON AÑINDO=AEINDO
     c+                        AND AÑITDO=AEITDO
     c+                        AND AEITDM=0
     c+ LEFT JOIN SDBFIL/BALOCA ON ALIPAI = AEIPAI
     c+                        AND ALICPO=AEICPO
     c+ LEFT JOIN SDBFIL/BAPROV ON APIPAI=ALIPAI
     c+                        AND APICPR=ALICPR
     c+ LEFT JOIN SDBFIL/BAPFDA ON QQITDO = AÑITDO
     c+                        AND QQINDO = AÑINDO
     c+ LEFT JOIN SDBFIL/BAPAIS ON AEIPAI = ANIPAI
     c+ WHERE
     c+  ANINDA > 0
     c*+  and AÑICUI in (
     c*+              20257374271,
     c*+              27302994736,
     c*+              20321635246,
     c*+              27007839265,
     c*+              27221356883,
     c*+              23249395234,
     c*+              20387635328,
     c*+              27078967337,
     c*+              27078967337
     c*+            )
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
     C+  :C1ICUI,
     C+  :C1NCAL,
     C+  :C1IPUE,
     C+  :C1IPIS,
     C+  :C1IDPT,
     C+  :C1ANEX,
     C+  :C1TORR,
     C+  :C1SECT,
     C+  :C1MANZ,
     C+  :C1NPAI,
     C+  :C1NPRO,
     C+  :C1NLOC,
     C+  :C1ICPO,
     C+  :C1ITEL,
     C+  :C1TITE,
     C+  :C1COEL,
     C+  :C1NYAA,
     C+  :C1APEL,
     C+  :C1INDA
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
     c     WriteRecord   BegSr
     C*-------------------------------------------------------------------------
     c                   Move      *Blanks       Buf
     C                   Eval      Buf= '0309'        + '|' +
     C                                  %TRIM(C1ICUI) + '|' +
     C                                  %TRIM(C1NCAL) + '|' +
     C                                  %TRIM(C1IPUE) + '|' +
     C                                  %TRIM(C1IPIS) + '|' +
     C                                  %TRIM(C1IDPT) + '|' +
     C                                  %TRIM(C1ANEX) + '|' +
     C                                  %TRIM(C1TORR) + '|' +
     C                                  %TRIM(C1SECT) + '|' +
     C                                  %TRIM(C1MANZ) + '|' +
     C                                  %TRIM(C1NPAI) + '|' +
     C                                  %TRIM(C1NPRO) + '|' +
     C                                  %TRIM(C1NLOC) + '|' +
     C                                  %TRIM(C1ICPO) + '|' +
     C                                  ''            + '|' +
     C                                  %TRIM(C1ITEL) + '|' +
     C                                  %TRIM(C1TITE) + '|' +
     C                                  %TRIM(C1COEL) + '|' +
     C                                  %TRIM(C1NYAA) + '|' +
     C                                  %TRIM(C1APEL) + '|' +
     C                                  %TRIM(C1INDA)
     C*
     C                   Eval      Buf= %TRIM(Buf) +
     C                                  X'0D'+X'25'
     c*
     c                   CALLP     write(FilHnd: %addr(buf):
     c                              %len(%trim(buf)))
     c*
     c                   EndSr
