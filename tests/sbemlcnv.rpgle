*SRCMBRTXT:Convierte chars de un arch.IFS a BASE64
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('TO10DP/TO10BD   ')
      *//CRTBNDRPG PGM(LE00542/SBEMLCNV) SRCFILE(LE00542/EM10P)
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
     D* Declaracion de Variables
     D*--------------------------------------------------------------------
     D T               S              1    DIM(64)  CTDATA PERRCD(1)
     D FilDes          S             10I 0
     D EAX             S             10I 0
     D CHRBUF          S              1A
     D HEXBUF          S              1A
     D CUICHR          S             11A
     D CUINUM          S             12S 0
     D DOCCHR          S              8A
     D DOCNUM          S             12S 0
     D LINCNT          S             10S 0
     D REM             S              5P 0
     D CONT            S              5P 0
     D TEST            S              9P 0
     D EXPN            S              5P 0
     D DIVN            S              5P 0
     D CHROUT          S              1A
     D INFEOF          S              1A

     C                   EVAL      REM=0
     C                   EVAL      CONT=0
     C                   EXSR      OPNINF
     C                   EXSR      GETCHR
     C                   DOW       INFEOF=*OFF
     C                   EXSR      BASE64
     C                   EXSR      GETCHR
     C                   ENDDO
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* BASE64 Convierte caracteres a codigo B64
     C*-------------------------------------------------------------------------
     c*
     C     BASE64        BEGSR
     C                   ADD       1             CONT
     C                   EVAL      EXPN=4**CONT
     C                   EVAL      TEST=REM*256+VALDEC
     C                   EVAL      DIVN=%DIV(TEST:EXPN)
     C                   EVAL      REM =%REM(TEST:EXPN)
     C                   IF        CONT=3
     C*                 GUARDO 2 BASE64 DIV Y REM
     C                   EVAL      CONT=0
     C                   EVAL      REM =0
     C                   EVAL      CHROUT=T(DIVN+1)
     C                   EVAL      CHROUT=T(REM +1)
     C                   ELSE
     C                   EVAL      CHROUT=T(DIVN+1)
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*--------------> Abrir Archivo en IFS (Integrated File System)
     C     OPNINF        BEGSR
     C*                  String Tipo C, terminada con nulo
     C                   EVAL      QUANME=%trim(QUANME)+X'00'
     C                   EVAL      FilDes = open(%ADDR(QUANME) :
     C                                               O_RDONLY  :
     C                                               850       )
     C     FilDes        COMP      *ZERO                                25
     C*  25 *on Archivo No encontrado o no se puede abrir (Autorizacion?)
     C   25              EXSR      ENDPGM
     C                   ENDSR
     C*--------------> Obtener Caracter desde Archivo Separado por @
     C     GETCHR        BEGSR
     C                   EVAL      EAX=read(FilDes:%ADDR(CHRBUF):1)
     C     EAX           COMP      *ZERO                                  25
     C   25              EVAL      INFEOF=*ON
     C                   IF        INFEOF=*OFF
     C                   MOVE      CHRBUF        CV2DIN            1
     C                   MOVE      CHRBUF        HEXBUF
     C                   EXSR      CV2DEC
     C                   Z-ADD     CV2DOU        VALDEC            3 0
     C     VALDEC        COMP      *ZERO                              25
     C  N25              MOVE      *BLANK        CHRBUF
     C   25VALDEC        ADD       1             VARAUX            3 0
     C*--------------> CONVERSIÓN A ASCII (DISABLE)
     C*  25              MOVE      T(VARAUX)     CHRBUF            1
     C*---------------------------------------------------------------
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
     C*--------------> Subrutina de Inicializacion
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    QUANME          256
     C                   EVAL      INFEOF=*OFF
     C                   ENDSR
     C*--------------> Finalizar programa y cerrar todo lo que hubiera abierto
     C     ENDPGM        BEGSR
     C                   CALLP     closef(FilDes)
     C                   EVAL      *INLR=*ON
     C                   RETURN
     C                   ENDSR
**
A
B
C
D
E
F
G
H
I
J
K
L
M
N
O
P
Q
R
S
T
U
V
W
X
Y
Z
a
b
c
d
e
f
g
h
i
j
k
l
m
n
o
p
q
r
s
t
u
v
w
x
y
z
0
1
2
3
4
5
6
7
8
9
+
/
