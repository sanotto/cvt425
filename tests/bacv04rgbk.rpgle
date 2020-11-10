*SRCMBRTXT:Graba archivo de salida                
     H DEBUG
     H DECEDIT('.') DATEDIT(*DMY/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Datos SAI                                      *
     H*                                                               *
     H*  PROGRAM NO: BACV04RG                                         *
     H*                                                               *
     H*  DATE:    18.07.2012                                          *
     H*                                                               *
     H*  AUTHOR: CIC - PR00543                                        *
     H*                                                               *
     F*----------------------------------------------------------------*
     FBACVTM    IP   E           K DISK
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
     C                   Eval      buf=*blanks
     C*
     C                   SELECT
     C     CVIFZA        WHENEQ    1
     C                   Move      '1'           DSIFZA            1
     C     CVIFZA        WHENEQ    2
     C                   Move      '2'           DSIFZA
     C                   ENDSL
     C*
     C                   Move      CVINUI        DSINUI           11
     C*
     C                   SELECT
     C     CVITII        WHENEQ    11
     C                   Move      '8'           DSTIPO            1
     C     CVITII        WHENEQ    89
     C                   Move      '2'           DSTIPO
     C     CVITII        WHENEQ    90
     C                   Move      '1'           DSTIPO
     C     CVITII        WHENEQ    94
     C                   Move      '6'           DSTIPO
     C                   OTHER
     C                   Move      '7'           DSTIPO
     C                   ENDSL
     C*
     C                   Eval      %subst(buf:001:001)=DSTIPO
     C                   Eval      %subst(buf:002:001)=X'05'
     C                   Eval      %subst(buf:003:020)=DSINUI
     C                   Eval      %subst(buf:023:001)=X'05'
     C                   Eval      %subst(buf:024:080)=CVNDNN
     C                   Eval      %subst(buf:104:001)=X'05'
     C                   Eval      %subst(buf:105:070)=CVNCAL
     C                   Eval      %subst(buf:135:010)=CVINUP
     C                   Eval      %subst(buf:185:001)=X'05'
     C                   Eval      %subst(buf:186:012)=*BLANKS
     C                   Eval      %subst(buf:197:001)=X'05'
     C                   Eval      %subst(buf:198:001)=DSIFZA
     C                   Eval      %subst(buf:199:001)=X'05'
     C                   Eval      %subst(buf:200:015)=%trim(
     C                             %EDITW(CVIULN:'0              '))
     C                   Eval      %subst(buf:230:001)=X'05'
     C                   ExSr      InvertirFecha
     C                   Eval      %subst(buf:231:010)=%EDITW(CVFECH:
     C                                                '  /  /    ')
     C                   Eval      %subst(buf:241:001)=X'05'
     c* el trim se roba 2 espacios en blanco que pone el editw
     C                   Eval      %subst(buf:242:015)=*zeros
     C                   Eval      %subst(buf:242:015)=%trim(
     C                                                %EDITW(CV$IMP:
     C                                                '0            .  '))
     C                   Eval      %subst(buf:257:001)=X'05'
     C* -- Fin de Linea 0d queda igual 0a => 25
     C                   Eval      %subst(buf:528:002)=X'0D'+X'25'
     C*
     C                   EXSR      WRTLIN
     C*
     CLR                 EXSR      CLOFIL
     C*
     C*----------------------------------------------------------------*
     C* InvertirFecha
     C*----------------------------------------------------------------*
     C     InvertirFecha BegSr
     C*
     C                   Call      'SBBAINFE'
     C                   Parm                    CVFECH
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
     c                   Eval      txtFile=%TRIM(OUTPAT)+'CVSAI_'+FechaC+'.txt'
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
