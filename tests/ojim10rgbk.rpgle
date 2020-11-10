*SRCMBRTXT:OJ-Import Arch. Internet-Filtrador de C
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     FSGSYSV    IF   E             DISK
     F*  The system values file
     F*----------------------------------------------------------------*
     FBADACT    IF   E           K DISK
     FOJMOFI01  IF A E           K DISK
      *//OVRDBF FILE(UPDMBR) TOFILE(OJMOFI)
     FUPDMBR    O    E           K DISK    RENAME(REOJMOFI:ROJM)
      *//OVRDBF FILE(RECMBR) TOFILE(OJMOFI)
     FRECMBR    O    E           K DISK    RENAME(REOJMOFI:RECH)
     FBAPFIS    IF   E           K DISK    RENAME(REBAPFIS:RPF1)
     FBAPFIS06  IF   E           K DISK    RENAME(REBAPFIS:RPF6)
     FBAPJUR    IF   E           K DISK    RENAME(REBAPJUR:RPJ1)
     F@CPIUSD   UF   E           K DISK
     F*----------------------------------------------------------------*
     FOJIM10PR  O    E             PRINTER OFLIND(*IN98)

      *//CRTBNDRPG PGM(SDB02.PGM/OJim10RG) SRCFILE(SDB02.SRC/QRPGSRC)

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
     D**********************************************************************
     D*  Flags for use in open()
     D*
     D* More than one can be used -- add them together.
     D**********************************************************************
     D*                                            Reading Only
     D O_RDONLY        C                   1
     D*                                            Writing Only
     D O_WRONLY        C                   2
     D*                                            Reading & Writing
     D O_RDWR          C                   4
     D*                                            Create File if not exist
     D O_CREAT         C                   8
     D*                                            Exclusively create
     D O_EXCL          C                   16
     D*                                            Truncate File to 0 bytes
     D O_TRUNC         C                   64
     D*                                            Append to File
     D O_APPEND        C                   256
     D*                                            Convert text by code-page
     D O_CODEPAGE      C                   8388608
     D*                                            Open in text-mode
     D O_TEXTDATA      C                   16777216

     D**********************************************************************
     D*      Mode Flags.
     D*         basically, the mode parm of open(), creat(), chmod(),etc
     D*         uses 9 least significant bits to determine the
     D*         file's mode. (peoples access rights to the file)
     D*
     D*           user:       owner    group    other
     D*           access:     R W X    R W X    R W X
     D*           bit:        8 7 6    5 4 3    2 1 0
     D*
     D* (This is accomplished by adding the flags below to get the mode)
     D**********************************************************************
     D*                                         owner authority
     D S_IRUSR         C                   256
     D S_IWUSR         C                   128
     D S_IXUSR         C                   64
     D S_IRWXU         C                   448
     D*                                         group authority
     D S_IRGRP         C                   32
     D S_IWGRP         C                   16
     D S_IXGRP         C                   8
     D S_IRWXG         C                   56
     D*                                         other people
     D S_IROTH         C                   4
     D S_IWOTH         C                   2
     D S_IXOTH         C                   1
     D S_IRWXO         C                   7

     D*--------------------------------------------------------------------
     D* Open a File
     D*
     D* int open(const char *path, int oflag, . . .);
     D*--------------------------------------------------------------------
     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)

     D*--------------------------------------------------------------------
     D* Read From a File
     D*
     D* ssize_t read(int handle, void *buffer, size_t bytes);
     D*--------------------------------------------------------------------
     D read            PR            10I 0 ExtProc('read')
     D  handle                       10i 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D*--------------------------------------------------------------------
     D* Write to a file
     D*
     D* ssize_t write(int fildes, const void *buf, size_t bytes)
     D*--------------------------------------------------------------------
     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D*--------------------------------------------------------------------
     D* Close a file
     D*
     D* int close(int fildes)
     D*--------------------------------------------------------------------
     D closef          PR            10I 0 ExtProc('close')
     D  handle                       10I 0 value

     D*--------------------------------------------------------------------
     D* Convertir a Numerico
     D*
     D* int close(int fildes)
     D*--------------------------------------------------------------------
     D Val             PR            30P 9
     D Char                          32A   VALUE

     D ToDate          PR             8P 0
     D Char                          10A   VALUE


     D T               S              4    DIM(257) CTDATA PERRCD(1)
     D FilDes          S             10I 0
     D EAX             S             10I 0
     D ESCLIENTE       S               N   INZ(*OFF)
     D INFEOF          S               N   INZ(*OFF)
     D COL             S            256A   DIM(26) INZ(*BLANKS)
     D COLPTR          S              2S 0
     D COLPOS          S              3S 0
     D CHRBUF          S              1A
     D HEXBUF          S              1A
     D CUICHR          S             11A
     D CUINUM          S             12S 0
     D DOCCHR          S              8A
     D DOCNUM          S             12S 0
     D RESPONDER       S             13A
     D LINCNT          S             10S 0

     C                   EXSR      OPNINF
     C                   EXSR      GETINF
     C                   EXSR      GETINF
+----C                   DOW       NOT INFEOF
|    C                   EXSR      CHKSDB
|+---C                   IF        ESCLIENTE OR (RESPONDER = 'Siempre')
||   C                   EXSR      PUTNOV
|+---C                   ELSE
||   C                   EXSR      LODREC
||   C                   WRITE     RECH                                 25
||   C   25              WRITE     ERROFIDU2
|+---C                   ENDIF
|    C                   EXSR      GETINF
+----C                   ENDDO
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C*--------------> Abrir Archivo bajado de Internet
     C     OPNINF        BEGSR
     C                   EVAL      QUANME=%trim(QUANME)+X'00'
     C                   EVAL      FilDes = open(%ADDR(QUANME) :
     C                                               O_RDONLY  :
     C                                               850       )
     C     FilDes        COMP      *ZERO                                25
     C   25              WRITE     ERRFILNOF
     C   25              EVAL      IMPOKY='N'
     C   25              EXSR      ENDPGM
     C                   ENDSR
     C*--------------> Obtener Registro de Archivo Separado por @
     C     GETINF        BEGSR
     C                   RESET                   COL
+----C                   FOR       COLPTR= 1 TO 26
|    C                   EVAL      COLPOS=*ZERO
|    C                   EXSR      GETCHR
|+---C                   DOW       CHRBUF <> '@' AND NOT INFEOF
||+--C                   IF        COLPTR >= 26
|||+-C                   IF        HEXBUF = X'0D' OR HEXBUF='0A'
|||| C                   LEAVE
|||+-C                   ENDIF
|||  C*                  IF        %TRIM(COL(25))='Provincia'    OR
|||  C*                            %TRIM(COL(25))='99'
|||  C*                  LEAVE
|||  C*                  ENDIF
|||+-C                   IF        %TRIM(COL(26))='Id_Organismo'
|||| C                   LEAVE
|||+-C                   ENDIF
|||+-C                   IF        COLPTR=26 AND CHRBUF = ' '
|||| C                   LEAVE
|||+-C                   ENDIF
||+--C                   ELSE
|||+-C                   IF        HEXBUF = X'0D' OR HEXBUF='0A'
|||| C                   EVAL      HEXBUF=' '
|||+-C                   ENDIF
||+--C                   ENDIF
||+--C                   IF        %TRIM(COL(1))='Fin de Archivo'
|||  C                   LEAVE
||+--C                   ENDIF
||   C                   EVAL      COLPOS=COLPOS+1
||   C                   EVAL      %SUBST(COL(COLPTR):COLPOS:1)=CHRBUF
||   C                   EXSR      GETCHR
|+---C                   ENDDO
|    C*
|+---C                   IF        HEXBUF = X'0D' OR HEXBUF='0A'
||   C                   LEAVE
|+---C                   ENDIF
|    C*
|+---C                   IF        INFEOF AND COLPTR < 26
||   C                             AND %TRIM(COL(1))<>'Fin de Archivo'
||   C                   WRITE     ERRUNEEOF
||   C                   EVAL      IMPOKY='N'
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
|    C                   ENDFOR
|+---C                   IF        %TRIM(COL(1))='Fin de Archivo'
||   C                   EVAL      INFEOF=*ON
|+---C                   ENDIF
|    C                   EVAL      RESPONDER=%TRIM(COL(22))
|    C                   EVAL      LINCNT=LINCNT+1
|    C                   ENDSR
|    C*--------------> Obtener Caracter desde Archivo Separado por @
|    C     GETCHR        BEGSR
|    C                   EVAL      EAX=read(FilDes:%ADDR(CHRBUF):1)
|    C     EAX           COMP      *ZERO                                  25
|    C   25              EVAL      INFEOF=*ON
|+---C                   IF        NOT INFEOF
||   C                   MOVE      CHRBUF        CV2DIN            1
||   C                   MOVE      CHRBUF        HEXBUF
||   C                   EXSR      CV2DEC
||   C                   Z-ADD     CV2DOU        VALDEC            3 0
||   C     VALDEC        COMP      *ZERO                              25
||   C  N25              MOVE      *BLANK        CHRBUF
||   C   25VALDEC        ADD       1             VARAUX            3 0
||   C   25              MOVE      T(VARAUX)     CHRBUF            1
|+---C                   ENDIF
|    C                   ENDSR
|    C*--------------> Convertir Hexa a Decimal
|    C     CV2DEC        BEGSR
|    C                   Z-ADD     *ZERO         CV2DOU            3 0
|    C                   TESTB     '0'           CV2DIN                   30
|    C                   TESTB     '1'           CV2DIN                   31
|    C                   TESTB     '2'           CV2DIN                   32
|    C                   TESTB     '3'           CV2DIN                   33
|    C                   TESTB     '4'           CV2DIN                   34
|    C                   TESTB     '5'           CV2DIN                   35
|    C                   TESTB     '6'           CV2DIN                   36
|    C                   TESTB     '7'           CV2DIN                   37
|    C   30              ADD       128           CV2DOU
|    C   31              ADD       64            CV2DOU
|    C   32              ADD       32            CV2DOU
|    C   33              ADD       16            CV2DOU
|    C   34              ADD       8             CV2DOU
|    C   35              ADD       4             CV2DOU
|    C   36              ADD       2             CV2DOU
|    C   37              ADD       1             CV2DOU
|    C                   ENDSR
|    C*--------------> Verificar si es cliente SIDEBA
|    C     CHKSDB        BEGSR
|    C                   EVAL      ESCLIENTE=*OFF
|    C                   EVAL      CUICHR=%TRIM(COL(5))
|    C                   MOVE      CUICHR        CUINUM
|    C                   EVAL      DOCCHR=%SUBST(CUICHR:3:8)
|    C                   MOVE      DOCCHR        DOCNUM
|+---C                   IF        %TRIM(COL(23))='2'
||   C*                PERSONA JURIDICA
||   C                   EVAL      TDOC=80
||   C                   EVAL      NDOC=CUINUM
||   C     KBAP          CHAIN     RPJ1                               25
||   C  N25              EVAL      ESCLIENTE=*ON
||   C  N25              EVAL      OJITDO=80
||   C  N25              EVAL      OJINDO=CUINUM
||   C   25              EVAL      OJITDO=*ZERO
||   C   25              EVAL      OJINDO=*ZERO
|+---C                   ELSE
||   C*                PERSONA FISICA
||   C                   EVAL      TDOC=80
||   C                   EVAL      NDOC=CUINUM
||   C     KBAP          CHAIN     RPF1                               25
||   C   25              EVAL      TDOC=96
||   C   25              EVAL      NDOC=DOCNUM
||   C   25KBAP          CHAIN     RPF1                               25
||   C   25CUINUM        CHAIN     RPF6                               25
||   C  N25              EVAL      ESCLIENTE=*ON
||   C  N25              EVAL      OJITDO=AÑITDO
||   C  N25              EVAL      OJINDO=AÑINDO
||   C   25              EVAL      OJITDO=*ZERO
||   C   25              EVAL      OJINDO=*ZERO
|+---C                   ENDIF
|    C                   ENDSR
|    C*--------------> Guarda registro de Novedades
|    C     PUTNOV        BEGSR
|    C                   EVAL      OJNTDO=TDOC
|    C                   EVAL      OJIINI=NDOC
|    C                   EVAL      OJINUI=Val(COL(1))
|    C     KOJMOFI       CHAIN     REOJMOFI                           25
|+---C                   IF        *IN25
||   C                   EXSR      LODREC
||   C                   WRITE     ROJM                                 25
||   C                   EXSR      CONACT
|+---C                   ELSE
||   C                   WRITE     ERROFIDUP
|+---C                   ENDIF
|    C                   ENDSR
|    C*--------------> Consulta actividad para cliente seleccionado
|    c     CONACT        BEGSR
|    C                   MOVEL(P)  COL(4)        TIPOFI
|    C                   WRITE     OFIREG
|    C                   CALL      'OJIM11RG'
|    C                   PARM                    OJINUI
|    C                   PARM                    OJNTDO
|    C                   PARM                    OJIINI
|    C                   PARM                    OJITDO
|    C                   PARM                    OJINDO
|    C                   PARM                    OJIINC
|    C*
|    C     @KEY02        CHAIN     BADACT                             80
|    C                   EVAL      *IN81=*OFF
|+---C                   DOW       *IN80 = *OFF
||   C                   EVAL      *IN81=*ON
||   C                   MOVEL(P)  IEDACL        PROTXT
||   C                   WRITE     PROREG
||   C     @KEY02        READE     BADACT                                 80
|+---C                   ENDDO
|    C  N81              EVAL      PROTXT='CLIENTE SIN PRODUCTOS'
|    C  N81              WRITE     PROREG
|    C                   EVAL      PROTXT='*********************'
|    C                   WRITE     PROREG
|    C                   ENDSR
|    C*--------------> Mueve valores desde Array de Trabajo a Campos de Archivo
|    C     LODREC        BEGSR
|    C                   MONITOR
|    C                   EVAL      OJININ=Val(COL(26))
|    C                   EVAL      OJNTDO=Val(COL(24))
|    C                   EVAL      OJIINI=Val(COL(5))
|    C                   EVAL      OJINUI=Val(COL(1))
|    C                   EVAL      OJVIGE=ToDate(%TRIM(COL(2)))
|    C                   EVAL      OJITRG=%TRIM(COL(3))
|    C                   EVAL      OJNYAP=%TRIM(COL(6))
|    C                   EVAL      OJDDOB=%TRIM(COL(7))
|    C                   EVAL      OJCENT=VAL(%TRIM(COL(8)))
|    C                   EVAL      OJNRSO=%TRIM(COL(9))
|    C                   EVAL      OJ$CAP=VAL(COL(10))
|    C                   EVAL      OJ$INT=VAL(COL(11))
|    C                   EVAL      OJ$SAL=OJ$CAP+OJ$INT
|    C                   EVAL      OJABAN=%TRIM(COL(12))
|    C                   EVAL      OJNSUC=%TRIM(COL(13))
|    C                   EVAL      OJINCT=VAL(%TRIM(COL(14)))
|    C                   EVAL      OJISEC=VAL(%TRIM(COL(15)))
|    C                   EVAL      OJFING=ToDate(%TRIM(COL(16)))
|    C                   EVAL      OJAINF=%TRIM(COL(17))
|    C                   EVAL      OJINUC=val(COL(18))
|    C                   EVAL      OJINDA=VAL(%TRIM(COL(19)))
|+---C                   IF        %TRIM(COL(20))='NO'
||   C                   EVAL      OJLTRA='N'
|+---C                   ELSE
||   C                   EVAL      OJLTRA='S'
|+---C                   ENDIF
|+---C                   IF        %TRIM(COL(21))='Actuales y Futuras'
||   C                   EVAL      OJCVIG='1'
|+---C                   ELSE
||   C                   EVAL      OJCVIG='2'
|+---C                   ENDIF
|    C                   EVAL      OJCRRQ=%SUBST(%TRIM(COL(22)):2:1)
|+---C                   IF        %TRIM(COL(22))='Solo Clientes'
||   C                   EVAL      OJCRRQ='1'
|+---C                   ENDIF
|+---C                   IF        %TRIM(COL(22))='Siempre'
||   C                   EVAL      OJCRRQ='2'
|+---C                   ENDIF
|+---C                   IF        %TRIM(COL(23))='1'
||   C                   EVAL      OJIINC='F'
|+---C                   ELSE
||   C                   EVAL      OJIINC='J'
|+---C                   ENDIF
|    C                   EVAL      OJIETI=*BLANK
|    C                   Z-ADD     OJVIGE        PAFECH
|    C                   CALL      'SBBAINFE'
|    C                   PARM                    PAFECH
|    C                   PARM      'IN'          PAMODE            2
|    C                   Z-ADD     *ZERO         PAJULI
|    C                   CALL      'SBBACFEC'
|    C                   PARM                    PAFECH            8 0
|    C                   PARM                    PAJULI           15 0
|    C                   PARM                    PASTAT            1
|+---C                   IF        PASTAT=' '
||   C                   ADD       15            PAJULI
||   C                   Z-ADD     *ZERO         PAFECH
||   C                   CALL      'SBBACFEC'
||   C                   PARM                    PAFECH            8 0
||   C                   PARM                    PAJULI           15 0
||   C                   PARM                    PASTAT            1
||   C                   CALL      'SBBAINFE'
||   C                   PARM                    PAFECH
||   C                   PARM      'NI'          PAMODE            2
||   C*
||   C                   Z-ADD     PAFECH        OJFPRE
|+---C                   ELSE
||   C                   EVAL      OJFPRE=AASFEI
|+---C                   ENDIF
|    C                   EVAL      OJFALT=AASFEI
|    C                   EVAL      OJITER=@JOB_NAM
|    C                   EVAL      OJIUSR=@USR_NAM
|    C                   EVAL      OJIJOB=@JOB_NUM
|    C                   EVAL      OJHALT=PRCTIM
|+---C                   IF        ESCLIENTE
||   C                   EVAL      OJIOPT='S'
|+---C                   ELSE
||   C                   EVAL      OJIOPT='N'
|+---C                   ENDIF
|    C                   ON-ERROR
|    C                   WRITE     ERRFILCOR
|    C                   EVAL      IMPOKY='N'
|    C                   EXSR      ENDPGM
|    C                   ENDMON
|    C                   ENDSR
|    C*--------------> Subrutina de Inicializacion
|    C     *INZSR        BEGSR
|    C     *ENTRY        PLIST
|    C                   PARM                    QUANME          256
|    C                   PARM                    IMPOKY            1
|    C                   PARM                    ITER             10
|    C                   PARM                    IJOB              6 0
|    C                   PARM                    IUSR             10
|    C     KBAP          KLIST
|    C                   KFLD                    TDOC              2 0
|    C                   KFLD                    NDOC             15 0
|    C     KOJMOFI       KLIST
|    C                   KFLD                    OJININ
|    C                   KFLD                    OJINUI
|    C                   KFLD                    OJNTDO
|    C                   KFLD                    OJIINI
|    C     KOJMOFI1      KLIST
|    C                   KFLD                    OJININ
|    C                   KFLD                    OJINUI
|    C*      ....Detalle de actividad
|    C                   CALL      'SBRTVSYS'
|    C                   PARM                    @CISYS            8
|    C     @KEY02        KLIST
|    C                   KFLD                    @CISYS
|    C                   KFLD                    @JOB_NUM
|    C                   EVAL      FLD001=%SUBST(QUANME:   1:  78)
|    C                   EVAL      FLD002=%SUBST(QUANME:  79:  78)
|    C                   EVAL      FLD003=%SUBST(QUANME: 157: 78)
|    C     1             CHAIN     SGSYSV                             99
|    C                   EVAL      OJFALT=AASFEI
|    C                   EVAL      OJITER=@JOB_NAM
|    C                   EVAL      OJIUSR=@USR_NAM
|    C                   EVAL      OJIJOB=@JOB_NUM
|    C                   EVAL      ITER=@JOB_NAM
|    C                   EVAL      IUSR=@USR_NAM
|    C                   EVAL      IJOB=@JOB_NUM
|    C                   EVAL      IMPOKY='S'
|    C                   TIME                    PRCTIM            6 0
|    C                   WRITE     ERRTIT
|    C                   ENDSR
|    C*--------------> Finalizar programa y cerrar todo lo que hubiera abierto
|    C     ENDPGM        BEGSR
|    C     @JOB_NUM      CHAIN     @CPIUSD                            80
|    C  N80              DELETE    @CPIUSRR
|    C                   CALLP     closef(FilDes)
|    C                   EVAL      *INLR=*ON
|    C                   RETURN
|    C                   ENDSR
|    C**************************************************************************
     P Val             B
     D Val             PI            30P 9
     D Char                          32A   VALUE
|    D*
     D                 DS
     D Char1                          1
     D Num1                           1  0 OVERLAY(Char1) INZ
|    D*
     D Num             S             30P 9
     D WrkNum          S             30P 0
     D Sign            S              3  0 INZ(1)
     D DecPos          S              3  0
     D Decimal         S              1    INZ('N')
     D i               S              4  0
     D j               S              4  0
|    D*-------------------------------------------------------------------------
|    C                   Eval      Char=%triml(Char)
|    C     ' '           CHECKR    Char          j                        99
|+---C                   IF        NOT *IN99
||   C                   EVAL      j=%SIZE(Char)
|+---C                   ENDIF
|+---C     1             DO        J             i
||   C                   EVAL      CHAR1=%SUBST(CHAR:I:1)
||+--C                   SELECT
||+--C                   WHEN      CHAR1 = '-'
|||  C                   EVAL      SIGN= -1
||+--C                   WHEN      CHAR1 = '.'   OR CHAR1=','
|||  C                   EVAL      DECIMAL='Y'
||+--C                   WHEN      CHAR1 >= '0' AND CHAR1 <= '9'
|||  C                   EVAL      WRKNUM=WRKNUM*10+NUM1
|||+-C                   IF        DECIMAL='Y'
|||| C                   EVAL      DECPOS=DECPOS+1
|||+-C                   ENDIF
||+--C                   ENDSL
|+---C                   ENDDO
|    C                   EVAL(H)   NUM=(WRKNUM*SIGN/(10**DECPOS))
|    C                   RETURN    NUM
|    C     *PSSR         BEGSR
|    C                   RETURN    *ZERO
|    C                   ENDSR
     P Val             E
|    C**************************************************************************
     P ToDate          B
     D ToDate          PI             8P 0
     D Char                          10A   VALUE
|    D*
     D                 DS
     D Char1                          1
     D Num1                           1  0 OVERLAY(Char1) INZ
|    D*
     D Num             S             30P 9
     D WrkNum          S             30P 0
     D CTR             S              2P 0
     D DIA             S              2P 0
     D MES             S              2P 0
     D FECHA           S              8P 0
     D i               S              4  0
     D j               S              4  0
|    D*-------------------------------------------------------------------------
|    C                   Eval      Char=%triml(Char)
|    C     ' '           CHECKR    Char          j                        99
|+---C                   IF        NOT *IN99
||   C                   EVAL      j=%SIZE(Char)
|+---C                   ENDIF
|+---C     1             DO        J             i
||   C                   EVAL      CHAR1=%SUBST(CHAR:I:1)
||+--C                   SELECT
||+--C                   WHEN      CHAR1 = '/'
|||  C                   ADD       1             CTR
|||+-C                   SELECT
|||+-C                   WHEN      CTR=1
|||| C                   EVAL      DIA=WRKNUM
|||+-C                   WHEN      CTR=2
|||| C                   EVAL      MES=WRKNUM
|||+-C                   ENDSL
|||  C                   Z-ADD     *ZERO         WRKNUM
||+--C                   WHEN      CHAR1 >= '0' AND CHAR1 <= '9'
|||  C                   EVAL      WRKNUM=WRKNUM*10+NUM1
||+--C                   ENDSL
|+---C                   ENDDO
|    C                   EVAL(H)   FECHA=WRKNUM*10000+MES*100+DIA
|    C                   RETURN    FECHA
|    C     *PSSR         BEGSR
|    C                   RETURN    *ZERO
|    C                   ENDSR
     P ToDate          E
**
000
001
002
003
004
005
006
007
008
009
010
011
012
013
014
015
016
017
018
019
020
021
022
023
024
025
026
027
028
029
030
031
032
033!
034"
035#
036$
037%
038&
039'
040(
041)
042*
043+
044,
045-
046.
047/
0480
0491
0502
0513
0524
0535
0546
0557
0568
0579
058:
059;
060<
061=
062>
063?
064@
065A
066B
067C
068D
069E
070F
071G
072H
073I
074J
075K
076L
077M
078N
079O
080P
081Q
082R
083S
084T
085U
086V
087W
088X
089Y
090Z
091[
092\
093]
094^
095_
096`
097a
098b
099c
100d
101e
102f
103g
104h
105i
106j
107k
108l
109m
110n
111o
112p
113q
114r
115s
116t
117u
118v
119w
120x
121y
122z
123{
124|
125}
126~
127
128
129
130
131
132
133
134
135
136
137
138
139
140
141
142
143
144
145
146
147
148
149
150
151
152
153
154
155
156
156
157
158
159
160 
161¡
162¢
163£
164¤
165¥
166¦
167§
168¨
169©
170ª
171«
172¬
173­
174®
175¯
176°
177±
178²
179³
180´
181µ
182¶
183·
184¸
185¹
186EVAL
187»
188¼
189½
190¾
191¿
192À
193Á
194Â
195Ã
196Ä
197Å
198Æ
199Ç
200È
201É
202Ê
203Ë
204Ì
205Í
206Î
207Ï
208Ð
209Ñ
210Ò
211Ó
212Ô
213Õ
214Ö
215×
216Ø
217Ù
218Ú
219Û
220Ü
221Ý
222Þ
223ß
224à
225á
226â
227ã
228ä
229å
230æ
231ç
232è
233é
234ê
235ë
236ì
237í
238î
239ï
240ð
241ñ
242ò
243ó
244ô
245õ
246ö
247÷
248ø
249ù
250ú
251û
252ü
253ý
254þ
255ÿ
