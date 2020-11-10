*SRCMBRTXT:OJ-Import Res. Internet-Lector de Archi
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('TO10DP/TO10BD   ')
     FSGSYSV    IF   E             DISK
     F*  The system values file

     FOJMRES01  UF   E           K DISK
     FOJMOFI01  UF   E           K DISK
     FOJPR10PR  O    E             PRINTER OFLIND(OV)

      *//CRTBNDRPG PGM(SDB02.PGM/OJPR10RG) SRCFILE(SDB02.SRC/QRPGSRC)

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
     D COL             S            256A   DIM(10) INZ(*BLANKS)
     D COLPTR          S              2S 0
     D COLPOS          S              3S 0
     D CHRBUF          S              1A
     D HEXBUF          S              1A
     D CUICHR          S             11A
     D CUINUM          S             12S 0
     D DOCCHR          S              8A
     D DOCNUM          S             12S 0
     D RESPONDER       S             13A
     D*
     D RENNRO          S              6S 0
     D RESNRO          S             10S 0
     D RESFEC          S              8S 0
     D RESNOF          S             11S 0
     D RESTOF          S              2A
     D RESTID          S              2S 0
     D RESNDO          S             15S 0
     D RESCTA          S             20A
     D RESMOR          S             15S 2
     D RESMEN          S             15S 2
     D RESOBS          S             15A

     C                   EXSR      OPNINF
     C                   EXSR      GETINF
     C                   EXSR      GETINF
     C                   DOW       NOT INFEOF
     C                   EXSR      PUTNOV
     C                   EXSR      GETINF
     C                   ENDDO
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
     C                   FOR       COLPTR= 1 TO 10
     C                   EXSR      GETCOL
     C                   IF        INFEOF
     C                   SELECT
     C                   WHEN      (COLPTR > 1 AND COLPTR < 10)
     C                   WRITE     ERRUNEEOF
     C                   EVAL      IMPOKY='N'
     C                   EXSR      ENDPGM
     C                   WHEN      COLPTR = 1
     C                   LEAVE
     C                   WHEN      COLPTR = 10
     C                   EVAL      INFEOF=*OFF
     C                   ENDSL
     C                   ENDIF
     C                   ENDFOR
     C                   ENDSR
     C*--------------> OBTENER COLUMNA
     C     GETCOL        BEGSR
     C                   EVAL      COLPOS=*ZERO
     C                   EXSR      GETCHR
     C                   DOW       CHRBUF <> '@' AND NOT INFEOF AND
     C                             HEXBUF <>  X'0D' AND  HEXBUF<> '0A'
     C                   EVAL      COLPOS=COLPOS+1
     C                   EVAL      %SUBST(COL(COLPTR):COLPOS:1)=CHRBUF
     C                   EXSR      GETCHR
     C                   ENDDO
     C                   ENDSR
     C*--------------> Obtener Caracter desde Archivo Separado por @
     C     GETCHR        BEGSR
     C                   EVAL      EAX=read(FilDes:%ADDR(CHRBUF):1)
     C     EAX           COMP      *ZERO                                  25
     C   25              EVAL      INFEOF=*ON
     C                   IF        NOT INFEOF
     C                   MOVE      CHRBUF        CV2DIN            1
     C                   MOVE      CHRBUF        HEXBUF
     C                   EXSR      CV2DEC
     C                   Z-ADD     CV2DOU        VALDEC            3 0
     C     VALDEC        COMP      *ZERO                              25
     C  N25              MOVE      *BLANK        CHRBUF
     C   25VALDEC        ADD       1             VARAUX            3 0
     C   25              MOVE      T(VARAUX)     CHRBUF            1
     C                   ENDIF
     C                   ENDSR
     C*--------------> Convertir Hexa a Decimal
     C     CV2DEC        BEGSR
     C                   Z-ADD     *ZERO         CV2DOU            3 0
     C                   TESTB     '0'           CV2DIN                   30
     C                   TESTB     '1'           CV2DIN                   31
     C                   TESTB     '2'           CV2DIN                   32
     C                   TESTB     '3'           CV2DIN                   33
     C                   TESTB     '4'           CV2DIN                   34
     C                   TESTB     '5'           CV2DIN                   35
     C                   TESTB     '6'           CV2DIN                   36
     C                   TESTB     '7'           CV2DIN                   37
     C   30              ADD       128           CV2DOU
     C   31              ADD       64            CV2DOU
     C   32              ADD       32            CV2DOU
     C   33              ADD       16            CV2DOU
     C   34              ADD       8             CV2DOU
     C   35              ADD       4             CV2DOU
     C   36              ADD       2             CV2DOU
     C   37              ADD       1             CV2DOU
     C                   ENDSR
     C*--------------> Guarda registro de Novedades
     C     PUTNOV        BEGSR
     C                   EVAL      RESNRO=VAL(COL(1))
     C                   EVAL      RESFEC=TODATE(COL(2))
     C                   EVAL      RESNOF=VAL(COL(3))
     C                   EVAL      RESTOF=%SUBST(COL(4):1:2)
     C                   EVAL      RESTID=VAL(COL(5))
     C                   EVAL      RESNDO=VAL(COL(6))
     C                   EVAL      RESCTA=%SUBST(COL(7):1:20)
     C                   EVAL      RESMOR=VAL(COL(8))
     C                   EVAL      RESMEN=VAL(COL(9))
     C                   EVAL      RESOBS=%SUBST(COL(10):1:15)
     C                   EVAL      RENNRO=RENNRO+1
     C                   Z-ADD     RESNOF        OJINUI
     C                   Z-ADD     RESNDO        OJIINI
     C                   Z-ADD     RESTID        OJNTDO
     C                   Z-ADD     RESFEC        MRFCIM
     C   OV              WRITE     LSTTIT
     C                   WRITE     LSTDET
     C     KMOFI         CHAIN     REOJMOFI                           25
     C   25              EVAL      RESERR='RESPUESTA DESCARTADA- '+
     C                             'Oficio Judicial NO encontrado en la base '+
     C                             'de Datos de Nuestro Sistema.Verifique    '
     C   25              WRITE     LSTERR
     C     KMRES         CHAIN     REOJMRES                           26
     C   26              EVAL      RESERR='RESPUESTA DESCARTADA- '+
     C                             'Respuesta NO encontrada en la base de Dat'+
     C                             'os de Respuestas           .Verifique    '
     C   26              WRITE     LSTERR
     C                   IF        NOT *IN25 AND  NOT *IN26
     C                   IF        OJITRG<>RESTOF
     C                   EVAL      RESERR='RESPUESTA DESCARTADA- '+
     C                             'El tipo de Oficio Judicial Informado en ' +
     C                             'la respuesta es <> al de la Base de Datos.'
     C                   WRITE     LSTERR
     C                   ELSE
     C                   EVAL      MRFASI=RESFEC
     C                   EVAL      MRIASI=RESNRO
     C                   SELECT
     C*EMBARGO
     C                   WHEN      RESTOF='1 ' OR RESTOF='5 '
     C                   EVAL      OJ$SAL=OJ$SAL - RESMEN
     C                   IF        OJ$SAL <= *ZERO
     C                   EVAL      OJIETI='T'
     C                   EVAL      RESERR='Oficio respondido TOTALMENTE'
     C                   ELSE
     C                   EVAL      OJIETI='P'
     C                   EVAL      RESERR='Oficio respondido Parcialmente'
     C                   ENDIF
     C  N99              UPDATE    REOJMOFI
     C  N99              UPDATE    REOJMRES
     C*PEDIDO DE INFORMES O NOTIFICACION DE TRANSFERENCIAS
     C                   WHEN      RESTOF='2 ' OR RESTOF='4 '
     C                   EVAL      OJIETI='T'
     C  N99              UPDATE    REOJMOFI
     C  N99              UPDATE    REOJMRES
     C                   EVAL      RESERR='Oficio respondido TOTALMENTE'
     C*LEVANTAMIENTO
     C                   WHEN      RESTOF='3 ' OR RESTOF='6 '
     C                   EVAL      OJIETI='T'
     C  N99              UPDATE    REOJMOFI
     C  N99              UPDATE    REOJMRES
     C                   EVAL      NROOFI=OJINUC
     C     KMRES1        SETLL     REOJMOFI
     C     KMRES1        READE     REOJMOFI                               25
     C                   DOW       NOT *IN25
     C                   EVAL      OJIETI='L'
     C  N99              UPDATE    REOJMOFI
     C     KMRES1        READE     REOJMOFI                               25
     C                   ENDDO
     C                   EVAL      RESERR='Oficio respondido TOTALMENTE'
     C                   ENDSL
     C                   WRITE     LSTERR
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*--------------> Subrutina de Inicializacion
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    QUANME          256
     C                   PARM                    SIMMDE            1
     C                   PARM                    IMPOKY            1
     C     KMOFI         KLIST
     C                   KFLD                    OJINUI
     C                   KFLD                    OJNTDO
     C                   KFLD                    OJIINI
     C     KMRES         KLIST
     C                   KFLD                    OJINUI
     C                   KFLD                    OJNTDO
     C                   KFLD                    OJIINI
     C                   KFLD                    MRFCIM
     C     KMRES1        KLIST
     C                   KFLD                    NROOFI           11 0
     C                   EVAL      FLD001=%SUBST(QUANME:   1: 113)
     C                   EVAL      FLD002=%SUBST(QUANME: 113: 113)
     C                   EVAL      FLD003=%SUBST(QUANME: 226: 30)
     C     1             CHAIN     SGSYSV                             99
     C                   EVAL      IMPOKY='S'
     C                   TIME                    PRCTIM            6 0
     C                   WRITE     LSTTIT
     C     SIMMDE        COMP      'S'                                    99
     C                   ENDSR
     C*--------------> Finalizar programa y cerrar todo lo que hubiera abierto
     C     ENDPGM        BEGSR
     C                   CALLP     closef(FilDes)
     C                   EVAL      *INLR=*ON
     C                   EVAL      *INOV=*ON
     C                   RETURN
     C                   ENDSR
     C**************************************************************************
     P Val             B
     D Val             PI            30P 9
     D Char                          32A   VALUE
     D*
     D                 DS
     D Char1                          1
     D Num1                           1  0 OVERLAY(Char1) INZ
     D*
     D Num             S             30P 9
     D WrkNum          S             30P 0
     D Sign            S              3  0 INZ(1)
     D DecPos          S              3  0
     D Decimal         S              1    INZ('N')
     D i               S              4  0
     D j               S              4  0
     D*-------------------------------------------------------------------------
     C                   Eval      Char=%triml(Char)
     C     ' '           CHECKR    Char          j                        50
     C                   IF        NOT *IN50
     C                   EVAL      j=%SIZE(Char)
     C                   ENDIF
     C     1             DO        J             i
     C                   EVAL      CHAR1=%SUBST(CHAR:I:1)
     C                   SELECT
     C                   WHEN      CHAR1 = '-'
     C                   EVAL      SIGN= -1
     C                   WHEN      CHAR1 = '.'   OR CHAR1=','
     C                   EVAL      DECIMAL='Y'
     C                   WHEN      CHAR1 >= '0' AND CHAR1 <= '9'
     C                   EVAL      WRKNUM=WRKNUM*10+NUM1
     C                   IF        DECIMAL='Y'
     C                   EVAL      DECPOS=DECPOS+1
     C                   ENDIF
     C                   ENDSL
     C                   ENDDO
     C                   EVAL(H)   NUM=(WRKNUM*SIGN/(10**DECPOS))
     C                   RETURN    NUM
     C     *PSSR         BEGSR
     C                   RETURN    *ZERO
     C                   ENDSR
     P Val             E
     C**************************************************************************
     P ToDate          B
     D ToDate          PI             8P 0
     D Char                          10A   VALUE
     D*
     D                 DS
     D Char1                          1
     D Num1                           1  0 OVERLAY(Char1) INZ
     D*
     D Num             S             30P 9
     D WrkNum          S             30P 0
     D CTR             S              2P 0
     D DIA             S              2P 0
     D MES             S              2P 0
     D FECHA           S              8P 0
     D i               S              4  0
     D j               S              4  0
     D*-------------------------------------------------------------------------
     C                   Eval      Char=%triml(Char)
     C     ' '           CHECKR    Char          j                        50
     C                   IF        NOT *IN50
     C                   EVAL      j=%SIZE(Char)
     C                   ENDIF
     C     1             DO        J             i
     C                   EVAL      CHAR1=%SUBST(CHAR:I:1)
     C                   SELECT
     C                   WHEN      CHAR1 = '/'
     C                   ADD       1             CTR
     C                   SELECT
     C                   WHEN      CTR=1
     C                   EVAL      DIA=WRKNUM
     C                   WHEN      CTR=2
     C                   EVAL      MES=WRKNUM
     C                   ENDSL
     C                   Z-ADD     *ZERO         WRKNUM
     C                   WHEN      CHAR1 >= '0' AND CHAR1 <= '9'
     C                   EVAL      WRKNUM=WRKNUM*10+NUM1
     C                   ENDSL
     C                   ENDDO
     C                   EVAL(H)   FECHA=WRKNUM*10000+MES*100+DIA
     C                   RETURN    FECHA
     C     *PSSR         BEGSR
     C                   RETURN    *ZERO
     C                   ENDSR
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
