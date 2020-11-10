*SRCMBRTXT:Graba archivo de salida                
     H DEBUG
     H DECEDIT(',') DATEDIT(*DMY/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Datos Percepción de Sello                      *
     H*                                                               *
     H*                                                               *
     H*  PROGRAM NO: CORE30R7                                         *
     H*                                                               *
     H*  DATE:    07/06/2012                                          *
     H*                                                               *
     H*  AUTHOR: LGR                                                  *
     H*                                                               *
     F*----------------------------------------------------------------*
     FCOTEM3    IP   E           K DISK
     D*----------------------------------------------------------------*
     D*-------------------------------------------------------------------------
     D*>DATASTRUCTURE : MYPSDS
     D*>DESCRIPTION   : Data structure containing program status information
     D*>USE           : Retrieve program status information
     D*>RELATEDFUNCT  : sys_cmd
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D @PRC_NAM          *PROC
     D @PGM_STS          *STATUS
     D @PRV_STS               16     20S 0
     D @LIN_NUM               21     28
     D @ROU_NAM          *ROUTINE
     D @PAR_CNT          *PARMS
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D @PGM_LIB               81     90
     D @EXC_DTA               91    170
     D @EXC_IDE              171    174
     D @DAT_RUN              191    198
     D @DAT_YEA              199    200S 0
     D @LAS_FIL              201    208
     D @FIL_INF              209    243
     D @JOB_NAM              244    253
     D @USR_NAM              254    263
     D @JOB_NUM              264    269S 0
     D @JOB_NUM_C            264    269
     D @JOB_DTE              270    275S 0
     D @RUN_DTE              276    281S 0
     D @RUN_TIM              282    287S 0
     D @CRT_DTE              288    293
     D @CRT_TIM              294    299
     D @CPL_LVL              300    303
     D @SRC_FIL              304    313
     D @SRC_MBR              324    333
     D @PRC_PGM              334    343
     D @PRC_MOD              344    353
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
     D OUTPAT          S            255    INZ('/home/impuestos/')
     D txtFile         S            255
     D FILHND          S             10I 0
     d FechaN          s              8  0
     d FechaC          s              8
     D* rEG         +0D +0A + TERMINADOR NULO
     d buf             s            529
     I*----------------------------------------------------------------*
     C*
      *
     C*                  Eval      buf=*all'.'
     C                   Eval      buf=*blanks
     C     TENCUE        IFNE      *BLANK
     C*
     C                   SELECT
     C     TEITIN        WHENEQ    80
     C                   Move      '8'           DSTIPO            1
     C     TEITIN        WHENEQ    89
     C                   Move      '2'           DSTIPO
     C     TEITIN        WHENEQ    90
     C                   Move      '1'           DSTIPO
     C     TEITIN        WHENEQ    94
     C                   Move      '6'           DSTIPO
     C                   OTHER
     C                   Move      '7'           DSTIPO
     C                   ENDSL
     C                   Eval      %subst(buf:001:030)=*zeros
     C* -- La editw con un 0 como primer char rellena ceros a izq PERO deja un
     C*    un blanco a la izquierda, el cual volamos con trim
     C                   Eval      %subst(buf:017:015)=%trim(
     C                             %EDITW(TEICHB:'0              '))
     C                   Eval      %subst(buf:031:001)=X'05'
     c                   ExSr      InvertirFecha
     C                   Eval      %subst(buf:032:016)=%EDITW(TEFASI:
     C                                                '  /  /    ')
     C                   Eval      %subst(buf:048:001)=X'05'
     C                   Eval      %subst(buf:049:080)=TENYAP
     C                   Eval      %subst(buf:129:001)=X'05'
     C                   Eval      %subst(buf:130:001)=DSTIPO
     C                   Eval      %subst(buf:131:001)=X'05'
     C                   Eval      %subst(buf:132:015)=%editc(TEININ:'Z')
     C                   Eval      %subst(buf:147:001)=X'05'
     C                   Eval      %subst(buf:148:080)=%TRIM(TEATLO)
     C                   Eval      %subst(buf:227:001)=X'05'
     C                   Eval      %subst(buf:228:008)=TENCUE
     C                   Eval      %subst(buf:236:001)=X'05'
     c* el trim se roba 2 espacios en blanco que pone el editw
     C                   Eval      %subst(buf:237:015)=*zeros
     C                   Eval      %subst(buf:239:015)=%trim(
     C                                                %EDITW(TE$IMP:
     C                                                '0            ,  '))
     C                   Eval      %subst(buf:254:001)=X'05'
     c* el trim se roba 2 espacios en blanco que pone el editw
     C                   Eval      %subst(buf:255:015)=*zeros
     C                   Eval      %subst(buf:257:015)=%trim(
     C                                                 %EDITW(TE$SMA:
     C                                                '0            ,  '))
     C*                  Eval      %subst(buf:272:001)=X'05'
     C*                  Eval      %subst(buf:273:255)=*BLANKS
     C* -- Fin de Linea 0d queda igual 0a => 25
     C                   Eval      %subst(buf:528:002)=X'0D'+X'25'
     C*
     C                   EXSR      WRTLIN
     C*
     C                   ENDIF
     CLR                 EXSR      CLOFIL
      *
     C*----------------------------------------------------------------*
     C* InvertirFecha
     C*----------------------------------------------------------------*
     C     InvertirFecha BegSr
     C*
     C                   Call      'SBBAINFE'
     C                   Parm                    TEFASI
     C                   Parm      'IN'          mode              2
     C*
     C                   EndSr
     C*----------------------------------------------------------------*
     C*
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C*
     c                   Z-add     *date         FechaN
     c                   Move      FechaN        FechaC
     c                   Eval      txtFile=%TRIM(OUTPAT)+'sellos_'+FechaC+'.txt'
     C*
     c                   EXSR      OpenStmF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* OpnStmF: ABRE ARCHIVO EN IFS
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
     C*----------------------------------------------------------------*
     C* CLOFIL: ABRE ARCHIVO EN IFS
     C*----------------------------------------------------------------*
     C     CLOFIL        BEGSR
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* WRTLIN: ESCRIBE UNA LINEA EN EL IFS
     C*----------------------------------------------------------------*
     C     WRTLIN        BEGSR
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C                   ENDSR
