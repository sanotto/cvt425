*SRCMBRTXT:Enviar MAIL con o sin attach(Vers. CMD)
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD   ')
     HDATEDIT(*YMD) DATFMT(*YMD)

     FBADREP    IF   E           K DISK
     FSGUSUA    IF   E           K DISK

     D*---------------------------------------------------------------------
     D* Imports
     D*--------------------------------------------------------------------
     D/copy LE00525/sockets,socket_h
     D/copy LE00525/sockets,ERRNO_H
     D/copy LE00525/sockets,sockutil_h
     D/copy LE00525/to10US,TOS1PS0RI
     D/copy LE00525/to10US,ioprots

     D*---------------------------------------------------------------------
     D* Declaración de Prototipos
     D*--------------------------------------------------------------------
     D die             PR
     D   peMsg                      256A   const
     D sndmsg          PR
     D   peMsg                      256A   const
     D  Shell          PR             7A
     D   Command                   1024A   VALUE


     D*--------------------------------------------------------------------
     D* Declaracion de Variables Globales
     D*--------------------------------------------------------------------
     D T               S              1    DIM(64)  CTDATA PERRCD(1)

     D INPBUF          S            129A
     D INPVAL          S              1    BASED(INPPTR)
     D INPPTR          S               *
     D INPCNT          S             10I 0 INZ(*ZERO)

     D OUTBUF          S          65535A   INZ(*BLANKS)
     D OUTVAL          S              1    BASED(OUTPTR)
     D OUTPTR          S               *

     D CVTDS           DS
     D  BINVAL                 1      4B 0 INZ(*ZERO)
     D  BYTE01                 1      1
     D  BYTE02                 2      2
     D  BYTE03                 3      3
     D  BYTE04                 4      4
     D SNDMAILDS       DS
     D  SVRIPN                 1     15
     D  SVRPON                16     18
     D  STSUSR                19     28
     D  STSDAT                29     36
     D  STSTIM                37     44
     D  STSCDE                45     45

     D I               S             10I 0
     D FilDes          S             10I 0
     D REM             S              5P 0
     D REM1            S              5P 0
     D CONT            S              5P 0
     D TEST            S              9P 0
     D EXPN            S              5P 0
     D DIVN            S              5P 0
     D INFEOF          S              1A
     D BUFSTR          S            256A
     D sock            S             10I 0
     D port            S              5U 0
     D addrlen         S             10I 0
     D ch              S              1A
     D host            s             32A
     D file            s             32A
     D addr            s             10U 0
     D p_Connto        S               *
     D RC              S             10I 0
     D RecBuf          S            132A
     D RecLen          S             10I 0
     D err             S             10I 0
     D x               S              3  0
     D fname           S            256A
     D CPF             S              7A


     C*-------------------------------------------------------------------------
     C* Main Program
     C*-------------------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    TOUSER           10
     C                   PARM                    subject          50
     C                   PARM                    msg            1024
     C                   PARM                    flatt           256
     C* ... Verificar si el sistema de mail esta deshabilitado
     C     *DTAARA       DEFINE                  SNDMAILDS
     C     *LOCK         IN        *DTAARA
     C                   IF        STSCDE = *OFF
     C                   EXSR      SNDOPR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C     TOUSER        CHAIN     REBADREP                           99
     C                   IF        *IN99 = *ON
     C                   EXSR      OPNINF
     C                   EVAL      INFEOF=*OFF
     C                   EXSR      SNDEML
     C                   ELSE
     C                   MOVE      TOUSER        ORIKEY           10
     C                   DOW       *IN99 = *OFF
     C     M4IUSR        CHAIN     SGUSUA                             99
     C                   IF        *IN99 = *OFF
     C                   EVAL      TOUSER=CVIQOU
     C                   EXSR      OPNINF
     C                   EVAL      INFEOF=*OFF
     C                   EXSR      SNDEML
     C                   CALLP     closef(FilDes)
     C                   ENDIF
     C     ORIKEY        READE     REBADREP                               99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   EXSR      ENDPGM

     c*---------------------------------------------------------
     c* MANDA MSG A SYSOPR
     c*---------------------------------------------------------
     c     SNDOPR        BEGSR
     c*
     C                   EVAL      CPF=SHELL('SNDMSG MSG('       +
     C                                %TRIM(SUBJECT) + ') ' +
     C                                 ' TOUSR(*SYSOPR)')
     c                   if        flatt <> *blanks
     C                   TIME                    HORANU            6 0
     C                   MOVE      HORANU        HORACH            6
     C                   Z-ADD     *DATE         FECHAN            8 0
     C                   MOVE      FECHAN        FECHAC            8
     c*
     C                   MOVE      *BLANKS       DSTDIR          255
     C                   MOVE      *BLANKS       ATTFIL          255
     C                   EVAL      DSTDIR='/home/tmp/lotus_off'
     C                   EVAL      CPF=SHELL('MKDIR '''+%trim(DSTDIR)+'''')
     C                   EVAL      DSTDIR='/home/tmp/lotus_off/'+%trim(FECHAC)
     C                   EVAL      CPF=SHELL('MKDIR '''+%trim(DSTDIR)+'''')
     C*
     C                   FOR       I= 256 DOWNTO 1
     C                   IF        %SUBST(FLATT:I:1)='/'
     C                   LEAVE
     C                   ENDIF
     C                   ENDFOR
     C*
     C                   EVAL      ATTFIL=%TRIM(DSTDIR)+'/'+%trim(FECHAC)+
     c                             '_'+HORACH+'_'+
     C                             %TRIM(%SUBST(FLATT:I+1:256-I))
     C*
     C                   EVAL      CPF=SHELL('CPY OBJ('''+
     C                                 %TRIM(FLATT) +''') '+
     C                                 ' TOOBJ('''+%TRIM(ATTFIL)+''')')
     c*
     c                   EndIf
     c*
     c                   ENDSR
     c*---------------------------------------------------------
     c* Crea cadena en base64 del archivo
     c*---------------------------------------------------------
     c     CRTATT        BEGSR
     C                   EVAL      REM=0
     C                   EVAL      CONT=0
     C                   EXSR      GETINP
     c                   DOW       INFEOF=*OFF
     C                   EXSR      SND
     C                   EXSR      GETINP
     C                   ENDDO
     c                   ENDSR
     C*-------------------------------------------------------------------------
     C*--------------> Abrir Archivo en IFS (Integrated File System)
     C     OPNINF        BEGSR
     C*                  String Tipo C, terminada con nulo
     C                   IF        FLATT<>*BLANKS
     C                   EVAL      flatt =%trim(flatt)+X'00'
     C                   EVAL      FilDes = open(%ADDR(flatt) :
     C                                               O_RDONLY  :
     C                                               850       )
     C                   if        filDes<0
     C*  25 *on Archivo No encontrado o no se puede abrir (Autorizacion?)
     c                   callp     sndmsg('Error al abrir el archivo')
     C                   EXSR      ENDPGM
     C                   endif
     C                   ENDIF
     C                   ENDSR
     C*--------------> Obtener Buffer de Entrada
     C     GETINP        BEGSR
     C                   IF        INPCNT = 0
     C                   MOVE      *ALLX'00'     INPBUF
     C                   MOVE      *ALLX'00'     outBUF
     C                   EVAL      INPCNT = read(FilDes:%ADDR(INPBUF):
     C                                           %SIZE(INPBUF))
     C                   IF        INPCNT = 0
     C                   EVAL      INFEOF=*ON
     C                   ENDIF
     C                   EVAL      INPPTR = %ADDR(INPBUF)
     C                   ENDIF
     C*
     C                   IF        INFEOF=*OFF
     C                   EVAL      REM1=%REM(INPCNT:3)
     C                   IF        REM1 = 1
     C                   EVAL      INPBUF=INPBUF+'=='
     C                   EVAL      INPCNT = %SIZE(INPBUF)
     C                   ENDIF
     C                   IF        REM1 = 2
     C                   EVAL      INPBUF=INPBUF+'='
     C                   EVAL      INPCNT = %SIZE(INPBUF)
     C                   ENDIF
     C                   EVAL      OUTPTR=%ADDR(OUTBUF)
     C                   FOR       I=1 TO INPCNT
     C                   Z-ADD     *ZERO         BINVAL
     C                   EVAL      BYTE04=INPVAL
     C                   EXSR      BASE64
     C                   EVAL      INPPTR=INPPTR+1
     C                   ENDFOR
     C                   EVAL      INPCNT=0
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* BASE64 Convierte caracteres a codigo B64
     C*-------------------------------------------------------------------------
     c*
     C     BASE64        BEGSR
     C                   ADD       1             CONT
     C                   EVAL      EXPN=4**CONT
     C                   EVAL      TEST=REM*256+BINVAL
     C                   EVAL      DIVN=%DIV(TEST:EXPN)
     C                   EVAL      REM =%REM(TEST:EXPN)
     C                   IF        CONT=3
     C                   EVAL      CONT=0
     C                   EVAL      OUTVAL=T(DIVN+1)
     C                   EVAL      OUTPTR=OUTPTR+1
     C                   EVAL      OUTVAL=T(REM +1)
     C                   EVAL      OUTPTR=OUTPTR+1
     C                   EVAL      REM =0
     C                   ELSE
     C                   EVAL      OUTVAL=T(DIVN+1)
     C                   EVAL      OUTPTR=OUTPTR+1
     C                   ENDIF
     C                   ENDSR
     C*--------------> Finalizar programa y cerrar todo lo que hubiera abierto
     C     ENDPGM        BEGSR
     C                   EVAL      *INLR=*ON
     C                   RETURN
     C                   ENDSR
     c*
     C*-------------------------------------------------------------------------
     C     SNDEML        BEGSR
     C*************************************************
     C* what port is the http service located on?
     C*************************************************
     c                   eval      p_servent = getservbyname('smtp':'tcp')
     c                   if        p_servent = *NULL
     c                   callp     die('Can''t find the SMTP service!')
     c                   return
     c                   endif
     c
     c                   eval      port = s_port
     C                   MOVE      SVRPON        port
     C*                  eval      port = 25
     C                   MOVE      SVRIPN        host
     C*                  eval      host = '39.0.1.222'
     C                   UNLOCK    SNDMAILDS

     C*************************************************
     C* Get the 32-bit network IP address for the host
     C*  that was supplied by the user:
     C*************************************************
     c                   eval      addr = inet_addr(%trim(host))
     c                   if        addr = INADDR_NONE
     c                   eval      p_hostent = gethostbyname(%trim(host))
     c                   if        p_hostent = *NULL
     c                   callp     die('No pude encontrar el host, revise' +
     c                                 ' el contenido de la DTAARA SNDMAILDS')
     c                   return
     c                   endif
     c                   eval      addr = h_addr
     c                   endif

     C*************************************************
     C* Create a socket
     C*************************************************
     c                   eval      sock = socket(AF_INET: SOCK_STREAM:
     c                                           IPPROTO_IP)
     c                   if        sock < 0
     c                   callp     die('socket(): '                        )
     c                   return
     c                   endif

     C*************************************************
     C* Create a socket address structure that
     C*   describes the host & port we wanted to
     C*   connect to
     C*************************************************
     c                   eval      addrlen = %size(sockaddr)
     c                   alloc     addrlen       p_connto

     c                   eval      p_sockaddr = p_connto
     c                   eval      sin_family = AF_INET
     c                   eval      sin_addr = addr
     c                   eval      sin_port = port
     c                   eval      sin_zero = *ALLx'00'

     C*************************************************
     C* Connect to the requested host
     C*************************************************
     C                   if        connect(sock: p_connto: addrlen) < 0
     c                   callp     close(sock)
     c                   callp     die('connect(): '                    )
     c                   return
     c                   endif
     c*leer bienvenida
     C                   eval      rc = rdline(sock: %addr(recbuf):
     c                                    %size(recbuf): *On: X'0A' : X'0D')
     c                   if        rc < 0
     c                   callp     close(sock)
     c                   callp     die('rdline(): '+%CHAR(RC)          )
     c                   return
     c                   endif

     C                   EVAL      OUTBUF='HELO '+%trim(@USR_NAM)
     c                   ExSr      SndRcv
     C                   EVAL      OUTBUF='MAIL FROM:<'+%trim(@USR_NAM)+'@'+
     C                                                  %TRIM(@USR_NAM)+'>'
     c                   ExSr      SndRcv
     C*                  EVAL      OUTBUF='RCPT TO:<'+%trim(TOUSER)+'@'+
     C*                                    'CORREO.NBLR.COM.AR>'
     C                   EVAL      OUTBUF='RCPT TO:<'+%trim(TOUSER)+'>'
     c                   ExSr      SndRcv
     C                   EVAL      OUTBUF='DATA'
     c                   ExSr      Snd
     C                   EVAL      OUTBUF='From:'+%trim(@USR_NAM)
     c                   ExSr      Snd
     C*                  EVAL      OUTBUF='To:'+%trim(TOUSER)+'@'+
     C*                                    'S104TC9M.QNBLRSA.COM.AR'
     C                   EVAL      OUTBUF='To:'+%trim(TOUSER)
     c                   ExSr      Snd
     C                   EVAL      OUTBUF='Subject:'+%trim(subject)
     c                   ExSr      Snd
     C                   EVAL      OUTBUF='MIME-Version: 1.0'
     c                   ExSr      Snd
     C*
     C                   EVAL      OUTBUF='Content-Type: multipart/mixed;'+
     C                                    'boundary="=0000="'
     C                   ExSr      Snd
     C                   EVAL      OUTBUF='Content-Transfer-Encoding: 8bit'
     C                   ExSr      Snd
     C                   EVAL      OUTBUF=x'0d'+'--=0000='
     C                   ExSr      Snd
     C                   EVAL      OUTBUF='Content-Type: text/plain'+
     C                                    ';charset=iso-8859-1 '
     C                   EXSR      SND
     C                   EVAL      OUTBUF='Content-Transfer-Encoding: 8bit'
     C                   ExSr      Snd
     C*                  EVAL      OUTBUF='Content-Disposition: inline'
     C*                  EXSR      SND
     C                   EVAL      OUTBUF=x'0d'+MSG+x'0d'
     C                   EXSR      SND
     C*
     C*
     C                   ExSr      SNDATT
     C*
     C                   EVAL      OUTBUF='.'
     C                   ExSr      Snd
     C                   EVAL      OUTBUF='QUIT'
     C                   ExSr      SndRcv
     c                   callp     close(sock)
     c                   callp     sndmsg('Email enviado a '+TOUSER)
     c                   ENDSR
     C*------------------
     C     SNDATT        BEGSR
     C                   IF        FLATT<>*BLANKS
     C                   EVAL      OUTBUF='--=0000='
     C                   ExSr      Snd
     c                   ExSr      GetFilNam
     C                   EVAL      OUTBUF='Content-Type:application/vnd.'+
     C                                    ' ms-excel;'+
     C                                    'name="'+%Trim(fname)+'"'
     C*                                   'name="planilla.xls"'
     C                   EVAL      OUTBUF='Content-Transfer-Encoding: base64'
     C
     C                   ExSr      Snd
     C                   EVAL      OUTBUF='Content-Disposition: attachment;'+
     C                                 'filename="'+%Trim(fname)+'"'+x'0d'
     C*                                'filename="planilla.xls"'+x'0d'
     C                   ExSr      Snd
     c                   EXSR      CRTATT
     C                   EVAL      OUTBUF='--=0000=--'
     C                   ExSr      Snd
     C                   ENDIF
     C                   ENDSR
     c*------------------
     c     GetFilNam     BegSr
     c                   eval      fname=flatt
     c     '/':'_'       xlate     fname         fname
     c     '\':'_'       xlate     fname         fname
     c     ':':'_'       xlate     fname         fname
     C                   EndSr
     c*-------------------------------------------------------------------------
     c*------------------
     c     SndRcv        BegSr
     C                   ExSr      Snd
     C                   ExSr      Rcv
     c                   EndSr
     c*------------------
     c     Snd           BegSr
     C                   callp     WrLine(sock: OUTBUF)
     c                   EndSr
     c*------------------
     c     Rcv           BegSr
     C*************************************************
     C* Get back the server's response codes
     C*************************************************
     C                   eval      rc = rdline(sock: %addr(recbuf):
     c                                    %size(recbuf): *On: X'0A' : X'0D')
     c                   if        rc < 0
     c                   callp     close(sock)
     c                   callp     die('rdline(): '+%CHAR(RC)           )
     c                   return
     c                   endif
     c*                  callp     sndmsg(recbuf)
     c                   Endsr


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *  This ends this program abnormally, and sends back an escape.
      *   message explaining the failure.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P die             B
     D die             PI
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
     c                               peMsg: wwMsgLen: '*INFO  ':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *  This show program info
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P sndmsg          B
     D sndmsg          PI
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
     c                               peMsg: wwMsgLen: '*INFO ' :
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E
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
