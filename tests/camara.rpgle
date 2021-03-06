*SRCMBRTXT:Code Test  -SQL Dinamico salida hacia P
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE'                                                          )
     FEACPRE    IP   F   94        DISK


     ***************************************************************************
     ** PROTOTYPES
     ***************************************************************************
     D*-------------------------------------------------------------------------
     D*>DATASTRUCTURE : SYSERR
     D*>DESCRIPTION   : Data structure to receive error generated by OPM APIS
     D*>USE           : Retrieve OPM API Error codes
     D*>RELATEDFUNCT  : sysSndPgmMsg
     D*-------------------------------------------------------------------------
      /IF NOT DEFINED(SYSERR_DEFINED)
      /DEFINE SYSERR_DEFINED
     D SYSERR          DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256
      /ENDIF

     D Cvt2Asc         PR
     D  StrPtr                         *   value
     D  StringLen                     4S 0 value
     D*------------------------------------------------------------------------
     D*>DESCRIPTION   : Sends a program message
     D*>RETURNS       : A string containing the message key generated by the Sy
     D*>PARAMETER     : A string containig MSGI
     D*>PARAMETER     : A string containig MSGF LIB (OPT)
     D*>PARAMETER     : A string containig User data (OPT)
     D*>USAGE         : Eval      rc=sysSndPgmMsg('CPF9801':'QCPFMSG   *LIBL
     D*------------------------------------------------------------------------
     D  sysSndPgmMsg   PR             4A
     D   msgID                        7A   CONST
     D   msgF                        20A   CONST OPTIONS(*NOPASS)
     D   msgDta                     256A   CONST OPTIONS(*NOPASS)
     D   msgType                     10A   CONST OPTIONS(*NOPASS)
     D*------------------------------------------------------------------------

      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file!
      *********************************************************
     D O_WRONLY        C                   2
     D SEEK_SET        C                   0
     D O_RDWR          C                   4
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_CODEPAGE      C                   8388608

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
     D*--------------------------------------------------------------------
     D* Read From a File
     D*
     D* ssize_t read(int handle, void *buffer, size_t bytes);
     D*--------------------------------------------------------------------
     D read            PR            10I 0 ExtProc('read')
     D  handle                       10i 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D Sndpm           PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)

     DBUFFER           S             96
     DFILHND           S             10I 0
     D CV2DIN          S              1A
     D CHRVAL          S              1    DIM(257) CTDATA PERRCD(1)
     D ASCVAL          S              3  0 DIM(257) ALT(CHRVAL)
     D*------------------
     D*Number Convertion Data Structure
     D*------------------
     D CvtDs           DS
     D CvtVal                  1      4B 0
     D  CvtBy1                 1      1A
     D  CvtBy2                 2      2A
     D  CvtBy3                 3      3A
     D  CvtBy4                 4      4A
     d buf             s             96
     ***************************************************************************
     ** PROGRAM START
     ***************************************************************************
     IEACPRE    AA
     I                                  1   94  LINEA

     C                   CallP     Cvt2Asc(%ADDR(linea ):%LEN(linea ))
     c                   eval      buf   =linea+x'0d'+x'0a'
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     cLR                 callp     close(FilHnd)
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     c                   EVAL      FilHnd=open('/home/LE00525/EACPRE.TXT' :
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                    S_IRWXU + S_IRWXO
     c                                  :AsciiCodePage )

     C                   IF        FilHnd < *ZERO
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*---------------------
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
     **-------------------------------------------------------------------------
     **
     **-------------------------------------------------------------------------
     P Cvt2Asc         B                   EXPORT
     D Cvt2Asc         PI
     D  StrPtr                         *   value
     D  StringLen                     4S 0 value

     D  I              S             10I 0
     D  WrkPtr         S               *
     D  Char           S              1A   BASED(Wrkptr)
     D  RplChr         S              1A
     D  Asc            S              3S 0
     D

     C                   EVAL      WrkPtr=StrPtr
     C                   FOR       I=1 TO StringLen
     C                   EVAL      Asc=%LOOKUP(Char:CHRVAL)
     C                   IF        Asc <> *ZERO
     C                   EVAL      Asc=ASCVAL(Asc)
     C                   EVAL      CvtVal=Asc
     C                   EVAL      RplChr=CvtBy4
     C                   ELSE
     C                   EVAL      RplChr=x'20'
     C                   ENDIF
     C                   EVAL      Char=RplChr
     C                   Eval      WrkPtr=WrkPtr+1
     C                   ENDFOR

     P Cvt2Asc         E

**
 032
]033
"034
Ñ035
$036
%037
&038
'039
(040
)041
*042
+043
,044
-045
.046
/047
0048
1049
2050
3051
4052
5053
6054
7055
8056
9057
:058
;059
<060
=061
>062
?063
@064
A065
B066
C067
D068
E069
F070
G071
H072
I073
J074
K075
L076
M077
N078
O079
P080
Q081
R082
S083
T084
U085
V086
W087
X088
Y089
Z090
^091
\092
!093
¢094
_095
`096
a097
b098
c099
d100
e101
f102
g103
h104
i105
j106
k107
l108
m109
n110
o111
p112
q113
r114
s115
t116
u117
v118
w119
x120
y121
z122
{123
|124
}125
¨126
¡161
[162
£163
¤164
¥165
ñ166
§167
~168
©169
ª170
«171
¬172
­173
®174
¯175
°176
±177
²178
³179
´180
µ181
¶182
·183
¸184
¹185
º186
»187
¼188
½189
¾190
¿191
À192
Á193
Â194
Ã195
Ä196
Å197
Æ198
Ç199
È200
É201
Ê202
Ë203
Ì204
Í205
Î206
Ï207
Ð208
#209
Ò210
Ó211
Ô212
Õ213
Ö214
×215
Ø216
Ù217
Ú218
Û219
Ü220
Ý221
Þ222
ß223
à224
á225
â226
ã227
ä228
å229
æ230
ç231
è232
é233
ê234
ë235
ì236
í237
î238
ï239
ð240
¦241
ò242
ó243
ô244
õ245
ö246
÷247
ø248
ù249
ú250
û251
ü252
ý253
þ254
ÿ255
