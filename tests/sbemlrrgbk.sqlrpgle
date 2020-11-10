*SRCMBRTXT:Enviar reportes por MAIL               
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD   ')

     FBADREP    IF   E           K DISK
     FSGUSUA    IF   E           K DISK

     D/copy LE00525/sockets,socket_h
     D/copy LE00525/sockets,ERRNO_H
     D/copy LE00525/sockets,sockutil_h
     D/copy LE00525/to10US,TOS1PS0RI

     D die             PR
     D   peMsg                      256A   const
     D sndmsg          PR
     D   peMsg                      256A   const
     D  Shell          PR             7A
     D   Command                   1024A   VALUE

     D SNDMAILDS       DS
     D  SVRIPN                 1     15
     D  SVRPON                16     18
     D  STSUSR                19     28
     D  STSDAT                29     36
     D  STSTIM                37     44
     D  STSCDE                45     45

     D REQSTR          S          65535A
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
     D LINEA           S            378A
     D CPF             S              7A

     c     *entry        plist
     c                   parm                    USR              10
     c                   parm                    REP              10
     c                   parm                    subject          50
     c                   parm                    msg            1024
     C     *DTAARA       DEFINE                  SNDMAILDS
     C     *LOCK         IN        *DTAARA
     c                   IF        STSCDE=*OFF
     C                   EXSR      SNDOPR
     c                   eval      *inlr = *on
     c                   RETURN
     C                   ENDIF
     C/EXEC SQL
     C+ DECLARE C1 SCROLL CURSOR FOR SELECT REPORTE FROM QTEMP/REPORTE
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C     K1            KLIST
     C                   KFLD                    REP
     C                   IF        USR <> '*ALL      '
     C                   EVAL      CVIQOU=*BLANKS
     C     K1            SETLL     REBADREP
     C     K1            READE     REBADREP                               25
     C                   DOW       NOT *IN25
     C     M4IUSR        IFEQ      USR
     C     M4IUSR        CHAIN     RESGUSUA                           25
     C                   IF        *IN25 = *OFF AND CVIQOU <> *BLANKS
     C                   MOVEL(P)  CVIQOU        USER             32
     C                   EXSR      SNDEML
     C                   ENDIF
     C                   ENDIF
     C     K1            READE     REBADREP                               25
     C                   ENDDO
     C                   ELSE
     C     K1            SETLL     REBADREP
     C     K1            READE     REBADREP                               25
     C                   DOW       NOT *IN25
     C     M4IUSR        CHAIN     RESGUSUA                           25
     C                   IF        CVIQOU <> *BLANKS
     C                   MOVEL(P)  CVIQOU        USER             32
     C                   EXSR      SNDEML
     C                   ENDIF
     C     K1            READE     REBADREP                               25
     C                   ENDDO
     C                   ENDIF
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC

     c                   eval      *inlr = *on
     c                   RETURN

     C*-------------------------------------------------------------------------
     c*---------------------------------------------------------
     c* MANDA MSG A SYSOPR
     c*---------------------------------------------------------
     c     SNDOPR        BEGSR
     c*
     C                   EVAL      CPF=SHELL('SNDMSG MSG('       +
     C                                %TRIM(SUBJECT) + ') ' +
     C                                 ' TOUSR(*SYSOPR)')
     c*
     c                   ENDSR
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
     c*                  eval      port = 250
     c*                  eval      host = '192.168.1.2'
     C                   MOVE      SVRPON        port
     C                   MOVE      SVRIPN        host
     C                   UNLOCK    SNDMAILDS

     C*************************************************
     C* Get the 32-bit network IP address for the host
     C*  that was supplied by the user:
     C*************************************************
     c                   eval      addr = inet_addr(%trim(host))
     c                   if        addr = INADDR_NONE
     c                   eval      p_hostent = gethostbyname(%trim(host))
     c                   if        p_hostent = *NULL
     c                   callp     die('Unable to find that host!')
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

     C*************************************************
     C* Send a request for the file that we'd like
     C* the http server to send us.
     C*
     C* Then we send a blank line to tell it we're
     C* done sending requests, it can process them...
     C*************************************************
     C                   EVAL      REQSTR='HELO '+%trim(@USR_NAM)
     c                   ExSr      SndRcv
     C                   EVAL      REQSTR='MAIL FROM:<'+%trim(@USR_NAM)+'@'+
     C                                                  %TRIM(@USR_NAM)+'>'
     c                   ExSr      SndRcv
     C                   EVAL      REQSTR='RCPT TO:<'+%trim(user)+'>'
     c                   ExSr      SndRcv
     C                   EVAL      REQSTR='DATA'
     c                   ExSr      Snd
     C                   EVAL      REQSTR='From:'+%trim(@USR_NAM)
     c                   ExSr      Snd
     C                   EVAL      REQSTR='To:'+%trim(user)
     c                   ExSr      Snd
     C                   EVAL      REQSTR='Subject:'+%trim(subject)
     c                   ExSr      Snd
     C                   EVAL      REQSTR='MIME-Version: 1.0'
     c                   ExSr      SndRcv
     C                   EVAL      REQSTR='Content-Type: text/html'+X'0D'
     c                   ExSr      Snd
     C                   EVAL      REQSTR=''
     c                   ExSr      Snd
     C                   EXSR      BLDMSG
     C                   EVAL      REQSTR='.'
     c                   ExSr      SndRcv
     C                   EVAL      REQSTR='QUIT'
     c                   ExSr      SndRcv
     C*************************************************
     C*  We're done receiving, do the following:
     C*       1) close the stream file & socket.
     C*       2) display the stream file
     C*       3) unlink (delete) the stream file
     C*       4) end program
     C*************************************************
     c                   callp     close(sock)
     c                   ENDSR
     C*------------------
     C     BLDMSG        BEGSR
     C                   EVAL      REQSTR='<html><body>' +X'0D'
     C                   EXSR      SND
     C                   EVAL      REQSTR=MSG+X'0D0D'
     C                   EXSR      SND
     C                   EVAL      REQSTR='<font size=-6     >'+X'0D'
     C                   EXSR      SND
     C                   EVAL      REQSTR='<PRE> <CODE>'+X'0D'
     C                   EXSR      SND
     C/EXEC SQL
     C+ FETCH FIRST FROM C1 INTO :LINEA
     C/END-EXEC
     C                   DOW       SQLCOD = *ZERO
     C                   EVAL      REQSTR ='> '+ %TRIMR(LINEA )+X'0D0A'
     C                   EXSR      SND
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :LINEA
     C/END-EXEC
     C                   ENDDO
     C                   EVAL      REQSTR='<CODE></PRE></FONT>'+
     C                                    '</body></html>'
     C                   EXSR      SND
     C                   ENDSR
     c*-------------------------------------------------------------------------
     c*------------------
     c     SndRcv        BegSr
     C                   ExSr      Snd
     C                   ExSr      Rcv
     c                   EndSr
     c*------------------
     c     Snd           BegSr
     C                   callp     WrLine(sock: REQSTR)
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
     c                   callp     sndmsg(recbuf)
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

