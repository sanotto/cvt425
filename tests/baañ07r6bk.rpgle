*SRCMBRTXT:Personas fisicas Supervision de terrori
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD  ')

     F@CPIUSD   UF   E           K DISK
     FBATERR    UF A E           K DISK

     D/copy LE00525/socketsrpg,socket_h
     D/copy LE00525/socketsrpg,errno_h
     D/copy LE00525/socketsrpg,sockutil_h

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
      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file]
      *********************************************************
     D O_WRONLY        C                   2
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_CODEPAGE      C                   8388608
     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value options(*string)
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)
     D unlink          PR            10I 0 ExtProc('unlink')
     D   path                          *   Value options(*string)
     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value
      *********************************************************
      * end of IFS API call definitions
      *********************************************************

     D die             PR
     D   peMsg                      256A   const

     D cmd             PR                  ExtPgm('QCMDEXC')
     D   command                    200A   const
     D   length                      15P 5 const

     D msg             S             50A
     D sock            S             10I 0
     D port            S              5U 0
     D addrlen         S             10I 0
     D ch              S              1A
     D host            s             32A
     D file            s            512A
     D addr            s             10U 0
     D p_Connto        S               *
     D RC              S             10I 0
     D RecBuf          S            132A
     D RecLen          S             10I 0
     D err             S             10I 0
     D fd              S             10I 0
     D i               S             10I 0
     D J               S             10I 0
     D tmpFile         S            255A

     I*----------------------------------------------------------------
     C*************************************************
     C* The user will supply a hostname and file
     C*  name as parameters to our program...
     C*************************************************
     c     *entry        plist
     c                   parm                    host
     c                   parm                    portno            5 0
     c                   parm                    file
     c                   parm                    rele              5 2
     c                   parm                    MINREL            5 2
     c                   parm                    match          4096
     c                   parm                    tmpFile
     c                   parm                    docu             15

     c                   if        docu <> *blanks
     c                   eval      file=%trim(file)+%trim(docu)
     c                   endif
     c                   eval      *inlr = *on

     C* ... Limpiar Baterr
     c     @JOB_NUM      chain     rebaterr                           99
     c                   dow       *in99 = *off
     c                   delete    rebaterr
     c     @JOB_NUM      reade     rebaterr                               99
     c                   enddo
     C*************************************************
     C* what port is the http service located on?
     C*************************************************
     c                   eval      p_servent = getservbyname('http':'tcp')
     c                   if        p_servent = *NULL
     c                   callp     die('Can''t find the HTTP service]')
     c                   return
     c                   endif

     c*                  eval      port = s_port
     c                   eval      port = portno

     C*************************************************
     C* Get the 32-bit network IP address for the host
     C*  that was supplied by the user:
     C*************************************************
     c                   eval      addr = inet_addr(%trim(host))
     c                   if        addr = INADDR_NONE
     c                   eval      p_hostent = gethostbyname(%trim(host))
     c                   if        p_hostent = *NULL
     c                   callp     die('Unable to find that host]')
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
     c                   callp     die('socket(): ' + %str(strerror(errno)))
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
+----C                   if        connect(sock: p_connto: addrlen) < 0
     c                   eval      err = errno
     c                   callp     close(sock)
     c                   callp     die('connect(): '+%str(strerror(err)))
     c                   return
     c                   endif

|    C*************************************************
|    C* Send a request for the file that we'd like
|    C* the http server to send us.
|    C*
|    C* Then we send a blank line to tell it we're
|    C* done sending requests, it can process them...
|    C*************************************************
     c                   callp     WrLine(sock: 'GET http://' +
     c                               %trim(host) + %trim(file) +
     c                               ' HTTP/1.0')
     c                   callp     WrLine(sock: ' ')

|    C*************************************************
|    C* Get back the server's response codes
|    C*
|    C* The HTTP server will send it's responses one
|    C* by one, then send a blank line to separate
|    C* the server responses from the actual data.
|    C*************************************************
     c                   dou       recbuf = *blanks
|    C                   eval      rc = rdline(sock: %addr(recbuf):
     c                                         %size(recbuf): *On)
     c                   if        rc < 0
     c                   eval      err = errno
     c                   callp     close(sock)
     c                   callp     die('rdline(): '+%str(strerror(err)))
     c                   return
     c                   endif
     c                   enddo

     C*************************************************
     C* Open a temporary stream file to put our
     C*   web page data into:
     C*************************************************
     c                   eval      tmpFile='/terroristas'+%trim(@JOB_NUM_C) +
     C                                     '.txt'
     c                   eval      fd = open(%trim(tmpFile)      :
     c                                  O_WRONLY+O_TRUNC+O_CREAT+O_CODEPAGE:
     c                                  511: 437)
     c                   if        fd < 0
     c                   eval      err = errno
     c                   callp     close(sock)
     c                   callp     Die('open(): '+%str(strerror(err)))
     c                   return
     c                   endif

|    C*************************************************
|    C* Write returned data to the stream file:
|    C*************************************************
|    C                   Move      *BLANKS       TFIELD         1024
|    C                   Move      *OFF          FSTFLG            1
     c                   dou       rc < 1
     c                   eval      rc = recv(sock: %addr(recbuf):
     c                                         %size(recbuf): 0)
     c                   if        rc > 0
|    C                   Eval      TFIELD=recbuf
|    C* ... Convertir  a EBCDIC
|    C                   EVAL      tknLen=%len(%trim(TFIELD ))
|    C                   MOVEL     'Q850337284'  XLTTAB           10
|    C                   CALL      'QDCXLATE'
|    C                   PARM                    TKNLEN            5 0
|    C                   PARM                    TFIELD
|    C                   PARM                    XLTTAB           10
|    C     X'25':X'40'   XLATE     TFIELD        TFIELD
|+---C                   IF        FSTFLG=*OFF
||   C                   Z-Add     *ZERO         Relevancia        6 2
||   C                   Movel     *Blanks       AUXRELE           5
||   C                   Eval      AUXRELE= %Subst(TFIELD: 1: 3)+
||   C                                      %Subst(TFIELD: 5: 2)
||   C                   Move      AUXRELE       Relevancia
||   C                   Move      AUXRELE       Rele
||   C                   MOVE      'S'           WWOPEC            1
||+--C                   IF        Relevancia > MINREL
|||  C                   MOVE      'N'           WWOPEC
||+--C                   ENDIF
||   C     @JOB_NUM      CHAIN     @CPIUSD                            80
||   C                   MOVE      WWOPEC        @COPEC
||   C                   UPDATE    @CPIUSRR
||   C                   MOVE      *ON           FSTFLG
|+---C                   ENDIF
     c                   endif
||   C                   Eval      Match=%TRIM(MATCH)+%TRIM(TFIELD)

     c     x'23':x'0D'   xlate     RECBUF        RECBUF
     c     x'7C':x'0A'   xlate     RECBUF        RECBUF
     c                   callp     write(fd: %addr(recbuf): rc)
     c                   enddo
||   C                   Eval      Match=X'0D'+%subst(MATCH : 7)
||   C
     C                   EXSR      WRTBATERR
|    C*************************************************
|    C*  We're done receiving, do the following:
|    C*       1) close the stream file & socket.
|    C*       2) display the stream file
|    C*       3) unlink (delete) the stream file
|    C*       4) end program
|    C*************************************************
     c                   callp     close(fd)
     c                   callp     close(sock)

     c                   return
     C*-------------------------------------------------------------------------
     C* ESCRIBE EN BATERR PARA IMPRIMIR EL CERTIFICADO
     C*-------------------------------------------------------------------------
     C     WRTBATERR     BEGSR
     C*
||+--C                   IF        Relevancia > MINREL
     C                   EVAL      S1DACL=''
     c                   Z-ADD     @JOB_NUM      S1IJOB
     c                   Z-ADD     *ZERO         J
     C                   FOR       I=5 TO %LEN(%TRIM(MATCH))
     C                   ADD       1             J
     C                   IF        J = 70
     C                   WRITE     REBATERR
     C                   EVAL      S1DACL=''
     c                   Z-ADD     1             J
     C                   ENDIF
     C                   IF        %SUBST(MATCH:I:1)='#' AND S1DACL <> *BLANKS
     C                   WRITE     REBATERR
     C                   EVAL      S1DACL=''
     c                   Z-ADD     1             J
     C                   ENDIF
     C                   IF        %SUBST(MATCH:I:1)<>'|' AND
     C                             %SUBST(MATCH:I:1)<>'#'
     C                   EVAL      %SUBST(S1DACL:J:1)=%SUBST(MATCH:I:1)
     c                   ELSE
     C                   SUB       1             J
     C                   ENDIF
     C                   ENDFOR
     C*
     C                   ENDIF
     C                   ENDSR
|     *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
|     *  This ends this program abnormally, and sends back an escape.
|     *   message explaining the failure.
|     *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
     c                               peMsg: wwMsgLen: '*ESCAPE':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E

      /define ERRNO_LOAD_PROCEDURE
      /copy LE00525/socketsrpg,errno_h
