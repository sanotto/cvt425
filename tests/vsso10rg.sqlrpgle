*SRCMBRTXT:EXPSPLF-Manejador Principal            
     H DFTACTGRP(*NO)  ACTGRP('QILE') DATFMT(*YMD)
     H*BNDDIR('*LIBL/IM10BNDDIR')
     FBASCTM    UF A E           K DISK
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     D/INCLUDE QSYSINC/QRPGLESRC,QUSEC
     D/INCLUDE QSYSINC/QRPGLESRC,QUSRSPLA
     D/INCLUDE QSYSINC/QRPGLESRC,QSPGETSP
     D*
     D*-----------------------------------------------------------
     D* Prototipos
     D*-----------------------------------------------------------
     D PGMMSG          PR
     D   peMsg                      256A   const
     D  cmd            PR             7A
     D   Command                   1024A   VALUE
     D*
     D*-----------------------------------------------------------
     D* Estructuras
     D*-----------------------------------------------------------
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
     D @JOB_DTE_C            270    275
     D @RUN_DTE              276    281S 0
     D @RUN_TIM              282    287S 0
     D @CRT_DTE              288    293
     D @CRT_TIM              294    299
     D @CPL_LVL              300    303
     D @SRC_FIL              304    313
     D @SRC_MBR              324    333
     D @PRC_PGM              334    343
     D @PRC_MOD              344    353
     D*----------------
     D                 DS
     DPAGESN                   1      4B 0
     DPAGESA                   1      9
     D*----------------
     D*----------------
     DKEYS             DS
     D                                9B 0 INZ(201)
     D                                9B 0 INZ(202)
     D                                9B 0 INZ(203)
     D                                9B 0 INZ(204)
     D                                9B 0 INZ(205)
     D                                9B 0 INZ(206)
     D                                9B 0 INZ(207)
     D                                9B 0 INZ(211)
     D                                9B 0 INZ(216)
     D                                9B 0 INZ(218)
     D                                9B 0 INZ(219)
     D                                9B 0 INZ(222)
     D                                9B 0 INZ(223)
     D*
     DKEYN             S              9B 0 INZ(13)
     D*
     D*----------------
     DQJOBNAM          DS
     D QJOBNME                       10
     D QUSRNME                       10
     D QJOBNUM                        6
     D*----------------
     DQUSH0100         DS                  BASED(LSTPTR3)
     D QUSUA                   1     64
     D QUSSGH                 65     68B 0
     D QUSSRL                 69     72
     D QUSFN                  73     80
     D QUSAU                  81     90
     D QUSDTC                 91    103
     D QUSIS                 104    104
     D QUSSUS                105    108B 0
     D QUSOIP                109    112B 0
     D QUSSIP                113    116B 0
     D QUSOHS                117    120B 0
     D QUSSHS                121    124B 0
     D QUSOLD                125    128B 0
     D QUSSLD                129    132B 0
     D QUSNBRLE              133    136B 0
     D QUSSEE                137    140B 0
     D QUSSIDLE              141    144B 0
     D QUSCID                145    146
     D QUSLID                147    149
     D QUSSLI                150    150
     D QUSERVED00            151    192
     D*----------------
     D******************************************************************
     D*Type definition for the SPLF0200 format.
     D*******
     D*NOTE: The following type definition only defines the fixed
     D* portion of the format. Any varying length field will
     D* have to be defined by the user.
     D******************************************************************
     DQUSSPLKI         DS           100    BASED(LSTPTR2)
     D QUSLFIR02               1      4B 0
     D QUSKFFFR00              5      8B 0
     D QUSTOD02                9      9
     D QUSR300                10     12
     D QUSDL02                13     16B 0
     D*
     DQUSF0200         DS                  BASED(LSTPTR)
     D QUSNBRFR00              1      4B 0
     DOUTQNM           DS
     D OUTNAM                  1     10
     D OUTLIB                 11     20
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*-----------------------------------------------------------
     D LDA           E DS                  EXTNAME(LDA)
     D*-----------------------------------------------------------
     D MSGDS           DS
     D  MSGTXT                 1    440
     D  WWNCU1                 1     55
     D  WWNCU2                56    110
     D  WWNCU3               111    165
     D  WWNCU4               166    220
     D  WWNCB1               221    275
     D  WWNCB2               276    330
     D  WWNCB3               331    385
     D  WWNCB4               386    440
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
     D*-----------------------------------------------------------
     D* Punteros
     D*-----------------------------------------------------------
     DLSTPTR           S               *
     DLSTPTR2          S               *
     DLSTPTR3          S               *
     DSPCPTR           S               *
     DQUSPUSPTR        S               *
     D*-----------------------------------------------------------
     D* Variables
     D*-----------------------------------------------------------
     DSPCNME           S             20    INZ('SPCNAME   QTEMP ')
     DHDRSPC           S             20    INZ('                ')
     DSPCSZE           S              9B 0 INZ(2000)
     DSPCINZ           S              1    INZ(X'00')
     DARR              S              1    BASED(LSTPTR) DIM(32767)
     DSPILEN           S             10I 0 INZ(%SIZE(QUSA0200))
     DSPLHAN           S             10I 0
     DSPFNUM           S             10I 0 INZ(-1)
     DREDREC           S             10I 0 INZ(-1)
     DSPLNME           S             10    INZ('*INT      ')
     DSWTCHR           S              1    INZ('1')
     DMAXSZE           S             10I 0 INZ(16776704)
     DBUFFER           S             10I 0 INZ(-1)
     DCPFMSG           S              7
     D*QLCOD           S              7
     DQUSPUS           S              9B 0 BASED(QUSPUSPTR)
     DOFFATT           S              9B 0
     DATTLEN           S              9B 0
     DSTGCTR           S              9B 0
     DFRMOFF           S              9B 0 INZ(0)
     DTO_OFF           S              9B 0
     DSPFBUF           S             10I 0
     DSPFCNT           S             10I 0
     DSPFSZE           S             15  0
     DSELSZE           S             15  0 INZ(0)
     DSELCNT           S             10I 0 INZ(0)
     DTFROKY           S              1    INZ(*OFF)
     DOBCDAT           S            250
     DDIRPAT           S            252
     DFILPAT           S            252
     DFULPAT           S            252
     DRTNCDE           S              7
     DOBJNME           S             10
     DOBSLON           S             70
     DSQLSTM           S             50
     DSVFNME           S             10
     DDESPAT           S           4096
     DSCRLNE           S            200
     d buf             s            302
     D FILHND          S             10I 0
     D LINEA           S            300
     D err             S             10I 0
     D errmsg          S            250A
     C                   EXSR      OPNCAT
     C                   EXSR      PRCOUQ
     C                   EXSR      CLOCAT
     C                   EXSR      SHWSFL
     C                   DOW       @FN(03) = *OFF AND @FN(12) = *OFF
     C     @ZRRNO        CHAIN     RTMP                               99
     C                   EXSR      SHWMNU
     c                   DOW       @FN(03) = *Off and @FN(12) = *Off
     C                   SELECT
     C                   WHEN      @FN(06) = *ON
     C                   EXSR      DSPSPL
     C                   WHEN      @FN(07) = *ON
     c                   EXSR      EXPTXT
     c                   LEAVE
     C                   ENDSL
     C                   EXSR      SHWMNU
     c                   ENDDO
     C                   EXSR      SHWSFL
     C                   ENDDO
     C                   EXSR      EXIPGM
     C*-------------------------------------------------------------------------
     C*------------------
     C*DSPSPL: Mostrar el archivo de spool
     C*------------------
     C     DSPSPL        BEGSR
     c*
     C                   MOVEL     S1DF01        FILE             10
     C                   MOVEL     S1$A03        JOB               6
     C                   MOVEL     S1$A04        SPLNBR            4
     C                   MOVEL     S1DF02        USER             10
     C                   MOVE      S1DF01        TERM             10
     C                   EVAL      CPFMSG=cmd(
     C                             'DSPSPLF FILE('+%TRIM(FILE)+')         '+
     C                             '        JOB(                          '+
     C                             %TRIM(JOB)+'/'+
     C                             %TRIM(USER)+'/'+
     C                             %TRIM(TERM)+
     C                             '                                   )  '+
     C                             '  SPLNBR('+SPLNBR+')                 '
     C                             )
     C*
     c*
     c                   ENDSR
     C*------------------
     C*EXPTXT: Exportar Spool como TXT
     C*------------------
     C     EXPTXT        BEGSR
     C*
     c                   EXSR      CRTTMPF
     c                   EXSR      CPYSPLF
     C                   EXSR      MKDIR
     C                   EXSR      WRTSPL
     C                   EVAL      WWNCU1='Se exportó el archivo...       '
     C                   EVAL      WWNCU3='El archivo se ha dejado en:    '
     C                   EVAL      WWNCU4=DIRPAT
     C                   EVAL      WWNCB1='Y se llama                     '
     C                   EVAL      WWNCB2=FILPAT
     C                   EVAL      WWNCB4='Presione INTRO para continuar  '
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
     C     @JOB_NUM      CHAIN(N)  @CPIUSD                            80
     C     @JOB_NUM      CHAIN(N)  @CPISYS                            80
     C*
     C                   ENDSR
     C*------------------
     C*WRTSPL:Escribe el archivo de Spool
     C*------------------
     C     WRTSPL        BEGSR
     C*
     C                   Z-ADD     *DATE         WWDATE            6 0
     C                   MOVE      WWDATE        CHDATE            6
     C                   TIME                    WWTIME            6 0
     C                   MOVE      WWTIME        CHTIME            6
     C                   Eval      FILPAT='spl_'+chdate+'_'+chtime+'_'+
     C                                    %trim(FILE) +'_'+
     c                                    %trim(JOB)  +'_'+
     c                                    %trim(SPLNBR)+'_'+
     c                                    %trim(USER)+'_'+
     c                                    %trim(TERM)+'.txt'
     c                   Eval      FULPAT=%trim(dirpat)+'/'+%trim(filpat)+x'00'
     c                   EXSR      OpenStmF
     C/EXEC SQL
     C+  DECLARE C1 CURSOR FOR SELECT SPOOLES AS LINEA FROM QTEMP/SPOOLES
     C/END-EXEC
     C/EXEC SQL
     C+  OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+  FETCH C1 INTO :LINEA
     C/END-EXEC
     C                   DoW       SQLCOD = *ZERO
     c                   Eval      Buf=Linea
     C                   Eval      %subst(buf:300:002)=X'0D'+X'25'
     c                   ExSr      WrtLin
     C/EXEC SQL
     C+  FETCH C1 INTO :LINEA
     C/END-EXEC
     c                   EndDo
     C/EXEC SQL
     C+  CLOSE C1
     C/END-EXEC
     C*
     c                   callp     close(FilHnd)
     C*
     C                   ENDSR
     C*------------------
     C*MKDIR: Crear el directorio para la exportación
     C*------------------
     C     MKDIR         BEGSR
     C*
     C                   EVAL      DIRPAT='/home/spooles'
     C                   EVAL      CPFMSG=cmd(
     C                             'MKDIR DIR(''/home/spooles'') ' +
     c                             '      DTAAUT(*RWX)           ' +
     c                             '      OBJAUT(*ALL)           '
     C                             )
     C                   EVAL      DIRPAT='/home/spooles/'+%trim(@USR_NAM)
     C                   EVAL      CPFMSG=cmd(
     C                             'MKDIR DIR('''+%trim(DIRPAT)+ ''') '
     C                             )
     C*
     C                   ENDSR
     C*------------------
     C*CPYSPLF: Copiar el archivo de spool al temporal
     C*------------------
     C     CPYSPLF       BEGSR
     C*
     C                   MOVEL     S1DF01        FILE             10
     C                   MOVEL     S1$A03        JOB               6
     C                   MOVEL     S1$A04        SPLNBR            4
     C                   MOVEL     S1DF02        USER             10
     C                   MOVE      S1DF01        TERM             10
     C                   EVAL      CPFMSG=cmd(
     C                             'CPYSPLF FILE('+%TRIM(FILE)+')         '+
     C                             '        TOFILE(QTEMP/SPOOLES)         '+
     C                             '        JOB(                          '+
     C                             %TRIM(JOB)+'/'+
     C                             %TRIM(USER)+'/'+
     C                             %TRIM(TERM)+
     C                             '                                   )  '+
     C                             '  SPLNBR('+SPLNBR+')                 '
     C                             )
     C*
     C                   ENDSR
     C*------------------
     C*CRTTMPF: Crear Archivo Temporal
     C*------------------
     C     CRTTMPF       BEGSR
     C*
     C                   EVAL      CPFMSG=cmd(
     C                             'DLTF QTEMP/SPOOLES'
     C                             )
     C                   EVAL      CPFMSG=cmd(
     C                             'CRTPF FILE(QTEMP/SPOOLES) ' +
     C                             '      RCDLEN(300)         ' +
     C                             '      LVLCHK(*NO)         '
     C                             )
     C*
     C                   ENDSR
     C*------------------
     C*SHWMNU: Mostrar Menu
     C*------------------
     C     SHWMNU        BEGSR
     C*
     c                   Move      *BLANKS       MSGTXT
     C                   EVAL      WWNCU1='Que desea Hacer con el archivo?'
     C                   EVAL      WWNCU3='F6=Ver el archivo              '
     C                   EVAL      WWNCU4='F7=Exportar como TXT           '
     C                   EVAL      WWNCB4='F3=Salir F12=Cancelar          '
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
     C     @JOB_NUM      CHAIN(N)  @CPIUSD                            80
     C     @JOB_NUM      CHAIN(N)  @CPISYS                            80
     C                   ENDSR
     C*------------------
     C*SHWSFL: Mostrar Subfile
     C*------------------
     C     SHWSFL        BEGSR
     C*
     C     @JOB_NUM      CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Exportar Arch. de Spool '
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Usuario... .........'+
     C                                    x'22'+@USR_NAM
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5='ARCHIVO    '+
     C                                    'TERMINAL   '+
     C                                    'USUARIO    '+
     C                                    'COLA       '+
     C                                    'FECHA   '+
     C                                    'PAGINAS    '
     C                   MOVEL(P)  @JOB_NUM      @CIJOB
     C                   UPDATE    @CPIUSRR
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @JOB_NUM      CHAIN(N)  @CPIUSD                            80
     C     @JOB_NUM      CHAIN(N)  @CPISYS                            80
     C*
     C*
     C                   ENDSR
     C*------------------
     C*Procesar una cola de salida
     C*------------------
     C     PRCOUQ        BEGSR
     C                   EXSR      INZSAR
     C                   EXSR      OPNSPLLST
     C   25              CALLP     pgmmsg('No se pudo obtener la' +
     C                             ' lista de archivos en spool')
     C                   EXSR      TOPSPLLST
     C                   DO        ENTCNT        COUNTER           7 0
     C                   EXSR      RTVATR
     C                   EXSR      CHKDAT
     C  N29              EXSR      PRCSPF
     C                   EXSR      NEXSPLENT
     C                   ENDDO
     C                   ENDSR
     C*------------------
     C* Rutina de Inicialización
     C*------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    USRPRF           10
     C                   PARM                    OUTQNM           20
     C                   PARM                    USRDTA           10
     C                   PARM                    FECDES           07
     C                   PARM                    FECHAS           07
     C*
     C                   MOVEL(P)  '*ALL'        CATTYP           10
     C                   MOVEL(P)  '*NO'         DLTSPL           04
     C*
     C                   Z-ADD     0             QUSBPRV
     C                   CLEAR                   QJOBNAM
     C                   MOVEL(P)  '*INT'        QJOBNME
     C*
     C                   IF        OUTNAM='*ALL'
     C                   MOVE      *BLANKS       OUTLIB
     C                   ENDIF
     C*
     C                   EVAL      CMDLNE='OVRDBF FILE(BASCTMRRN)'  +
     C                             ' TOFILE(*LIBL/BASCTM)'
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDLNE         1024
     C                   PARM      1024          CMDSZE           15 5
     c                   OPEN      BASCTMRRN
     C*
     C                   CALL      '@CPIPGSD'
     C*
     C                   ENDSR
     C*------------------
     C* Abrir lista de archivos en Spool
     C*------------------
     C     OPNSPLLST     BEGSR
     C                   EVAL      EXTATR='QUSLSPL'
     C                   EVAL      SPCTXT=*BLANKS
     C                   EVAL      SPCNME='QUSLSPL   QTEMP     '
     C                   EXSR      CRTUSP
     C                   EXSR      DMPSPL
     C                   EXSR      GETPTR
     C                   EVAL      LSTPTR3=SPCPTR
     C                   MOVE      QUSNBRLE      ENTCNT           10 0
     C                   EVAL      *IN25=*ON
     C                   IF        (QUSSRL ='0100'                ) AND
     C                             (QUSIS = 'C' OR QUSIS = 'P'    ) AND
     C                             (ENTCNT   > 0                  )
     C                   EVAL      *IN25=*OFF
     C                   ENDIF
     C                   ENDSR
     C*------------------
     C* Ubicar Puntero en la primera entrada de la lista de archivos en Spool
     C*------------------
     C     TOPSPLLST     BEGSR
     C                   EVAL      LSTPTR = SPCPTR
     C                   EVAL      LSTPTR = %ADDR(ARR(QUSOLD + 1))
     C                   ENDSR
     C*------------------
     C* Avanzar Puntero a la próxima entrada
     C*------------------
     C     NEXSPLENT     BEGSR
     C                   EVAL      LSTPTR = %ADDR(ARR(QUSSEE + 1))
     C                   ENDSR
     C*------------------
     C* Recuperar para cada entrada de la lista de archivos en spool, los datos
     C*------------------
     C     RTVATR        BEGSR
     C                   Z-ADD     5             X                 9 0
     C                   EVAL      LSTPTR2 = %ADDR(ARR(X))
     C                   DO        QUSNBRFR00
     C                   MOVE      *BLANKS       ENTVAL           26
     C                   EVAL      ENTVAL = %SUBST(QUSSPLKI:17:QUSDL02)
     C                   EVAL      PAGESA = %SUBST(QUSSPLKI:17:QUSDL02)
     C                   SELECT
     C     QUSKFFFR00    WHENEQ    201
     C                   MOVEL     ENTVAL        FILNME           10
     C     QUSKFFFR00    WHENEQ    202
     C                   MOVEL(P)  ENTVAL        TERNME           10
     C     QUSKFFFR00    WHENEQ    203
     C                   MOVEL     ENTVAL        USRNME           10
     C     QUSKFFFR00    WHENEQ    204
     C                   MOVEL     ENTVAL        JOBNBR            6
     C     QUSKFFFR00    WHENEQ    205
     C                   MOVE      PAGESN        SPLNBR            4
     C     QUSKFFFR00    WHENEQ    206
     C                   MOVEL     ENTVAL        OUQNME           10
     C     QUSKFFFR00    WHENEQ    207
     C                   MOVEL     ENTVAL        OUQLIB           10
     C     QUSKFFFR00    WHENEQ    211
     C                   MOVE      PAGESN        PAGNBR            6 0
     C     QUSKFFFR00    WHENEQ    216
     C                   MOVEL     ENTVAL        SPLDAT            7
     C     QUSKFFFR00    WHENEQ    218
     C                   MOVEL     ENTVAL        IJOBID           16
     C     QUSKFFFR00    WHENEQ    219
     C                   MOVEL     ENTVAL        ISPFID           16
     C     QUSKFFFR00    WHENEQ    222
     C                   MOVE      PAGESN        SPFBUF
     C     QUSKFFFR00    WHENEQ    223
     C                   MOVE      PAGESN        SPFCNT
     C                   ENDSL
     C                   ADD       QUSLFIR02     X
     C                   EVAL      LSTPTR2 = %ADDR(ARR(X))
     C                   ENDDO
     C                   ENDSR
     C*-----------------------
     C* Verifica si la fecha de creacion del spool file esta dentro del rango pa
     C*-----------------------
     C     CHKDAT        BEGSR
     C                   MOVE      *OFF          *IN29
     C                   SELECT
     C                   WHEN      FECDES <> *ZEROS AND
     C                             FECHAS <> *ZEROS AND
     C                             (SPLDAT <  FECDES OR  SPLDAT >  FECHAS)
     C                   MOVE      *ON           *IN29
     C                   WHEN      FECDES <> *ZEROS AND
     C                             FECHAS =  *ZEROS AND
     C                             SPLDAT <  FECDES
     C                   MOVE      *ON           *IN29
     C                   WHEN      FECDES =  *ZEROS AND
     C                             FECHAS <> *ZEROS AND
     C                             SPLDAT >  FECHAS
     C                   MOVE      *ON           *IN29
     C                   ENDSL
     C                   ENDSR
     C*-----------------------
     C* Procesar este SPF
     C*-----------------------
     C     PRCSPF        BEGSR
     C                   EXSR      CATENT
     C                   ENDSR
     C*-----------------------
     C* Obtener atributos del archivo en Spool
     C*-----------------------
     C     GETSPLATR     BEGSR
     C                   CALL      QUSRSPLA
     C                   PARM                    QUSA0200
     C                   PARM                    SPILEN
     C                   PARM      'SPLA0200'    SPIFMT            8
     C                   PARM                    QJOBNAM          26
     C                   PARM                    IJOBID
     C                   PARM                    ISPFID
     C                   PARM                    SPLNME
     C                   PARM                    SPFNUM
     C                   ENDSR
     C*-----------------------
     C* Abrir el archivo en Spool
     C*-----------------------
     C     OPNSPF        BEGSR
     C                   CALL      'QSPOPNSP'
     C                   PARM                    SPLHAN
     C                   PARM                    QJOBNAM          26
     C                   PARM                    IJOBID
     C                   PARM                    ISPFID
     C                   PARM                    SPLNME
     C                   PARM                    SPFNUM
     C                   PARM                    REDREC
     C                   PARM                    QUSEC
     C                   ENDSR
     C*-----------------------
     C* Cerrar el archivo en Spool
     C*-----------------------
     C     CLOSPF        BEGSR
     C                   CALL      'QSPCLOSP'
     C                   PARM                    SPLHAN
     C                   PARM                    QUSEC
     C                   ENDSR
     C*-----------------------
     C* Crear el almacenamiento para el archivo en Spool
     C*-----------------------
     C     CRTSTG        BEGSR
     C                   MOVE      COUNTER       CHRCTR            7
     C                   MOVE      STGCTR        CHRSTG            2
     C                   EVAL      SPCNME='S'+CHRCTR+CHRSTG+'QTEMP     '
     C     HDRSPC        COMP      *BLANKS                                25
     C   25              MOVE      SPCNME        HDRSPC
     C                   EVAL      EXTATR='SPOOL_FILE'
     C                   MOVE      QUSJNBR10     AUXJNO            6
     C                   MOVE      QUSSNBR00     AUXSPF            6
     C                   MOVE      QUSTP00       AUXTOP            6
     C                   EVAL      SPCTXT=QUSSN02+'|'+QUSJN11 +
     C                                    '|'+%trim(QUSUN13) +
     C                                    '|'+AUXJNO  +
     C                                    '|'+AUXSPF+'|'+AUXTOP
     C                   EXSR      CRTUSP
     C                   ENDSR
     C*-----------------------
     C* Escribir los atributos del Spool en el User Space
     C*-----------------------
     C     WRTATR        BEGSR
     C                   EXSR      GETPTR
     C                   EVAL      QUSPUSPTR=SPCPTR+88
     C                   EVAL      OFFATT=  QUSPUS + 1
     C                   Z-ADD     1             FRMOFF
     C                   Z-ADD     4             TO_OFF
     C*
     C                   CALL      'QUSCHGUS'
     C                   PARM                    HDRSPC
     C                   PARM                    FRMOFF
     C                   PARM                    TO_OFF
     C                   PARM                    OFFATT
     C                   PARM                    SWTCHR
     c*
     C                   EVAL      ATTLEN=%SIZE(QUSA0200)
     C                   Z-ADD     5             FRMOFF
     C                   Z-ADD     4             TO_OFF
     C                   CALL      'QUSCHGUS'
     C                   PARM                    HDRSPC
     C                   PARM                    FRMOFF
     C                   PARM                    TO_OFF
     C                   PARM                    ATTLEN
     C                   PARM                    SWTCHR
     C*
     C                   SUB       1             STGCTR
     C                   Z-ADD     9             FRMOFF
     C                   Z-ADD     12            TO_OFF
     C                   CALL      'QUSCHGUS'
     C                   PARM                    HDRSPC
     C                   PARM                    FRMOFF
     C                   PARM                    TO_OFF
     C                   PARM                    STGCTR
     C                   PARM                    SWTCHR
     C*
     C                   CALL      'QUSCHGUS'
     C                   PARM                    SPCNME
     C                   PARM                    OFFATT
     C                   PARM                    ATTLEN
     C                   PARM                    QUSA0200
     C                   PARM                    SWTCHR
     C                   ENDSR
     C*-----------------------
     C* Poner el archivo en Spool en el almacenamiento (USRSPC) Creado para el
     C*-----------------------
     C     PUTSPL        BEGSR
     C                   EVAL      QUSBPRV=%SIZE(QUSEC)
     C                   EVAL      QUSEI  =*BLANKS
     C                   MOVEL(P)  '*WAIT'       EOFIND           10
     C                   MOVE      'SPFR0200'    FORMAT
     C                   CALL      QSPGETSP
     C                   PARM                    SPLHAN
     C                   PARM                    SPCNME
     C                   PARM                    FORMAT
     C                   PARM                    BUFFER
     C                   PARM                    EOFIND
     C                   PARM                    QUSEC
     C                   ENDSR
     C*-----------------------
     C* Crear un USRSPC
     C*-----------------------
     C     CRTUSP        BEGSR
     C                   CALL      'QUSCRTUS'
     C                   PARM                    SPCNME
     C                   PARM                    EXTATR           10
     C                   PARM                    SPCSZE
     C                   PARM                    SPCINZ
     C                   PARM      '*ALL'        SPCAUT           10
     C                   PARM                    SPCTXT           50
     C                   PARM      '*YES'        SPCRPL           10
     C                   PARM                    QUSEC
     C                   PARM      '*USER'       SPCDMN           10
     C                   ENDSR
     C*-----------------------
     C*    Call QUSLSPL to get all spooled files for especified criteria
     C*-----------------------
     C     DMPSPL        BEGSR
     C                   CALL      'QUSLSPL'
     C                   PARM                    SPCNME
     C                   PARM      'SPLF0200'    FORMAT            8
     C                   PARM                    USRPRF           10
     C                   PARM                    OUTQNM           20
     C                   PARM      '*ALL'        FRMTYP           10
     C                   PARM                    USRDTA           10
     C                   PARM                    QUSEC
     C                   PARM                    JOBNAM           26
     C                   PARM                    KEYS
     C                   PARM                    KEYN
     C                   ENDSR
     C*-------------------
     C* Get a resolved pointer to the User Space for performance
     C*-------------------
     C     GETPTR        BEGSR
     C                   CALL      'QUSPTRUS'
     C                   PARM                    SPCNME
     C                   PARM                    SPCPTR
     C                   PARM                    QUSEC
     C                   ENDSR
     C*------------------
     C* Eliminar el archivo en Spool una vez salvado
     C*------------------
     C     DLTSPF        BEGSR
     C                   EVAL      CPFMSG=cmd(
     C                             'DLTSPLF FILE('+%TRIM(FILNME)+') '  +
     C                             '         JOB('+%TRIM(JOBNBR) +'/'  +
     C                                            %TRIM(USRNME) +'/'   +
     C                                            %TRIM(TERNME) +') '  +
     C                             '        SPLNBR('+%TRIM(SPLNBR)+')'
     C                             )
     C                   ENDSR
     C*------------------
     C* Abrir catalogo
     C*------------------
     C     OPNCAT        BEGSR
     C                   IF        *IN90= *OFF
     C                   EVAL      *IN90=*ON
     C                   MOVEL(P)  OUTQNM        OUQNME           10
     C                   MOVE(P)   OUTQNM        OUQLIB           10
     C                   IF        CATTYP='*PRINT    ' OR CATTYP='*MNGPRT   '
     C                                                 OR CATTYP='*ALL      '
     C                   ENDIF
     C                   EXSR      OPNFCA
     C                   ENDIF
     C                   ENDSR
     C*------------------
     C* Abrir catalogo en archivo
     C*------------------
     C     OPNFCA        BEGSR
     C*
     C     @JOB_NUM      CHAIN     REBASCTM                           99
     C                   DOW       *IN99 = *OFF
     C                   DELETE    REBASCTM
     C     @JOB_NUM      READE     REBASCTM                               99
     C                   ENDDO
     C*
     C                   ENDSR
     C*------------------
     C* Escribir entrada en el catalogo
     C*------------------
     C     CATENT        BEGSR
     C                   IF        *IN90= *OFF
     C                   EXSR      OPNCAT
     C                   EVAL      *IN90=*ON
     C                   ENDIF
     C                   IF        CATTYP='*PRINT    ' OR CATTYP='*MNGPRT   '
     C                                                 OR CATTYP='*ALL      '
     C     STGCTR        ADD       1             CTRSTG            2 0
     C                   MOVEL(P)  HDRSPC        SPCHDR           10
     C                   ENDIF
     C                   MOVE      @JOB_NUM      S1IJOB
     C                   MOVEL(P)  FILNME        S1DF01
     C                   MOVE      TERNME        S1DF01
     C                   MOVEL(P)  USRNME        S1DF02
     C                   MOVEL(P)  JOBNBR        S1$A03
     C                   MOVEL(P)  SPLNBR        S1$A04
     C                   MOVEL(P)  OUQNME        S1DF03
     C                   MOVEL(P)  OUQLIB        OUQLIB
     C                   MOVEL(P)  PAGNBR        S1IEMP
     C                   MOVEL(P)  SPLDAT        S1IMON
     C                   MOVE      SPLDAT        FECHA             6
     C                   MOVE      FECHA         FECHAN            6 0
     C                   EVAL      S1DACL=FILNME+' '+
     C                                    TERNME+' '+
     C                                    USRNME+' '+
     C                                    OUQNME+' '+
     C                                    %EDITW(FECHAN:'  -  -  ') +' '+
     C                                    %EDITC(PAGNBR:'Z')
     C                   WRITE     REBASCTM
     C                   ENDSR
     C*------------------
     C* Cerrar catalogo
     C*------------------
     C     CLOCAT        BEGSR
     C                   ENDSR
     C*------------------
     C* Enviar Spool File a Servidor de Backups
     C*------------------
     C     SNDSPF        BEGSR
     C                   ENDSR
     C*------------------
     C* Inicializar area de salvado, es decir eliminar USRSPC de la qtemp
     C*------------------
     C     INZSAR        BEGSR
     C                   EVAL      CPFMSG=cmd('DLTUSRSPC USRSPC(QTEMP/S0*)')
     C                   ENDSR
     C*------------------
     C* Limpieza final antes de salir del programa
     C*------------------
     C     EXIPGM        BEGSR
     C                   EVAL      CPFMSG=cmd('DLTUSRSPC QTEMP/QUSLSPL')
     C                   EVAL      *INLR = *ON
     C                   CLOSE     BASCTMRRN
     C                   RETURN
     C                   ENDSR
     C*------------------
     C* Transferencia utilizando FTP
     C*------------------
     C     FTPTFR        BEGSR
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* OpnStmF: ABRE ARCHIVO EN IFS
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     C*
     c                   EVAL      FilHnd=open(%trim(fulpat):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHnd)
     c                   EVAL      FilHnd=open(%trim(FuLPAT):
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
     ***************************************************************************
      **************************************************************************
      * ZONA DE PROCEDIMIENTOS
      **************************************************************************
     C*-----------------------------------------------------------------
     C* Procedimiento para invocar al interprete de mandatos del AS/400
     C* y recoger el CPF generado
     C*-----------------------------------------------------------------
     P cmd             B
     D  cmd            PI             7A
     D   Command                   1024A   VALUE
     D CommLen         S             15  5
     C                   Call      'QCMDEXC'
     c                   PARM                    Command
     C                   PARM      1024          CommLen
     C                   Return    'CPF0000'
     c     *PSSR         BEGSR
     C                   Return    @EXC_TYP+@EXC_NUM
     c                   ENDSR
     c
     P cmd             E

     C*-----------------------------------------------------------------
     C* Envia un SNDPGMMSG y aborta el programa
     C*-----------------------------------------------------------------
     P PGMMSG          B
     D PGMMSG          PI
     D   peMsg                      256A   const

     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)

     D dsEC            DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256

     D wwMsgLen        S             10I 0
     D wwTheKey        S              4A

     c                   eval      wwMsgLen = %len(%trimr(peMsg))
     c                   if        wwMsgLen<1
     c                   return
     c                   endif

     c                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
     c                               peMsg: wwMsgLen: '*ESCAPE':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E
     C*=========================================================================
