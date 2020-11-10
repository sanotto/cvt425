*SRCMBRTXT:Switch-Adapter      -Envio de PBF a Swi
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD')

     Fsgsysv    if   e             disk
     D/copy LE00525/SOCKETSRPG,socket_h
     D/copy LE00525/SOCKETSRPG,errno_h
     D/copy LE00525/SOCKETSRPG,sockutil_h

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

     D msg             S          32575A
     D sock            S             10I 0
     D port            S              5U 0
     D addrlen         S             10I 0
     D ch              S              1A
     D host            s             32A
     D file            s             32A
     D addr            s             10U 0
     D p_Connto        S               *
     D RC              S             10I 0
     D RecBuf          S             50A
     D RecLen          S             10I 0
     D err             S             10I 0
     D fd              S             10I 0

     C*************************************************
     C* The user will supply a hostname and file
     C*  name as parameters to our program...
     C*************************************************

     c     *entry        Plist
     c                   Parm                    host
     c                   Parm                    user             10
     c                   Parm                    pass             10
     c                   Parm                    retsvr           15
     c                   Parm                    wwmmdd            4
     c                   Parm                    path             77
     c                   Parm                    WWPORT            7 0

     c     1             chain     resgsysv                           99
     c                   eval      *inlr = *on

     C*************************************************
     C* what port is the http service located on?
     C*************************************************
     c*                  eval      p_servent = getservbyname('http':'tcp')
     c*                  if        p_servent = *NULL
     c*                  callp     die('Can''t find the HTTP service]')
     c*                  return
     c*                  endif
      *
     c                   eval      port = WWPORT

     C*************************************************
     C* Get the 32-bit network IP address for the host
     C*  that was supplied by the user:
     C*************************************************
     c                   eval      addr = inet_addr(%trim(host))
     c                   if        addr = INADDR_NONE
     c                   eval      p_hostent = gethostbyname(%trim(host))
     c                   if        p_hostent = *NULL
     c                   callp     die('Unable to find that host]')
     c                   ExSr      EndPgm
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
     c                   ExSr      EndPgm
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
     c                   eval      err = errno
     c                   callp     close(sock)
     c                   callp     die('connect(): '+%str(strerror(err)))
     c                   ExSr      EndPgm
     c                   return
     c                   endif

     C*                  eval      rc = rdline(sock: %addr(recbuf):
     c*                                   %size(recbuf): *On: X'0A' : X'0D')
     C*************************************************
     C* create the request
     C*************************************************
     c*                  Move      AASFEI        wwmmdd            4
     C*                  eval      msg='USER='+user
     c*                  callp     WrLine(sock: %trim(MSG):-1:*ON:X'0A')
     C*                  eval      msg='PASSWD='+pass
     c*                  callp     WrLine(sock: %trim(MSG):-1:*ON:X'0A')
     C*                  eval      msg='SERVER='+retsvr
     c*                  callp     WrLine(sock: %trim(MSG):-1:*ON:X'0A')
     C*                  eval      msg='MMDD='+wwmmdd
     c*                  callp     WrLine(sock: %trim(MSG):-1:*ON:X'0A')
     C*                  eval      msg='         '
     c*                  callp     WrLine(sock: %trim(MSG))

     C                   eval      msg='NBLR:20:AUTH:0309:' +
     C                             %TRIM(RETSVR)+':'+
     C                             %TRIM(USER  )+':'+
     C                             %TRIM(PASS  )+':'+
     C                             %TRIM(PATH  )+':'+
     C                             'PBF'+WWMMDD +':NBLR:'+x'0d'+x'0a'
     c                   callp     WrLine(sock: %trim(MSG))
     c                   call      'SWAD04CL'

     C*************************************************
     C*  We're done receiving, do the following:
     C*       1) close the stream file & socket.
     C*       2) display the stream file
     C*       3) unlink (delete) the stream file
     C*       4) end program
     C*************************************************
     c                   callp     close(sock)

     c                   ExSr      EndPgm

     C     EndPgm        BegSr
     C                   Seton                                        LR
     c                   return
     C                   EndSr

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
     c                               peMsg: wwMsgLen: '*NOTIFY':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E

      /define ERRNO_LOAD_PROCEDURE
      /copy LE00525/SOCKETSRPG,errno_h
