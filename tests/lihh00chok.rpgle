*SRCMBRTXT:Link-Host to Host-Procedimientos de Can
      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P OpenChanel      B
     D OpenChanel      PI              N
     D  ConnNme                       6A

     D IP              s             10U 0
     D len             S             10I 0
     D bindto          S               *
     D connfrom        S               *
     D port            S              5U 0
     D clientip        S             17A
     D addrlen         S             10I 0
     D p_Connto        S               *
     D rc              S             10I 0

     C                   MoveL(P)  ConnNme       sConnName
     C     ConnNme       Chain     RELICONN
     C                   If        Not %Found()
     C                   CallP     LogError('ERR':'Def. de Conexión:'+ConnNme +
     C                             ' NO Encontrada en archivo LICONN')
     C                   Return    *ON
     C                   EndIf
     C*
     C                   ExSr      LoadFieldsDef
     C*
     C                   If        CORMDE='C'
     C                   ExSr      OpenClient
     C                   Else
     C                   ExSr      OpenServer
     C                   EndIf
     C                   Return    *Off
     C*-----------------
     C* LoadFieldsDef : Carga definiciones de Campos
     C*----------------
     C     LoadFieldsDef BegSr
     C*
     C     *LOVAL        SetLL     REISFLDS
     C                   Read      REISFLDS                               99
     C                   DoW       *IN99 = *OFF
     C                   Eval      mBitsXLong(IFNBIT)=IFCBIT
     C                   Eval      mDtaType(IFNBIT)  =IFTDAT
     C                   Eval      mLongMax(IFNBIT)  =IFLONG
     C                   Eval      mLongMin(IFNBIT)  =IFNPOI
     C                   Read      REISFLDS                               99
     C                   EndDo
     C*
     C                   EndSr
     C*-----------------
     C*
     C*----------------
     C     OpenClient    BegSr
     C*
     C                   eval      IP = inet_addr(%trim(COIPAD))
     C                   if        IP = INADDR_NONE
     C                   eval      p_hostent = gethostbyname(%trim(COIPAD))
     C                   if        p_hostent = *NULL
     c                   ExSr      TCPError
     C                   endif
     C                   eval      IP = h_addr
     C                   endif
     C* ... Create a socket
     c                   eval      csock = socket(AF_INET: SOCK_STREAM:
     c                                           IPPROTO_IP)
     c                   if        csock < 0
     c                   ExSr      TCPError
     c                   endif
     C* ... Create a socket address structure that descr the host & port we want
     c                   eval      addrlen = %size(sockaddr)
     c                   alloc     addrlen       p_connto
     c                   eval      p_sockaddr = p_connto
     c                   eval      sin_family = AF_INET
     c                   eval      sin_addr = IP
     c                   eval      sin_port = COPORT
     c                   eval      sin_zero = *ALLx'00'
     C* ... Connect to the requested host
     C                   if        connect(csock: p_connto: addrlen) < 0
     c                   ExSr      TCPError
     c                   endif
     C*
     C                   EndSr
     C*-----------------
     C*
     C*----------------
     C     OpenServer    BegSr
     C                   ExSr      MakeListener
     C                   ExSr      AcceptConn
     C                   EndSr
     C*------------------
     C* MakeListener: Seteo de Socket para Eschuchar Conexiones
     C*------------------
     C     MakeListener  BegSr
     C*
     C                   Eval      port = COPORT

     C* ... Allocar espacio para las estr. descriptoras de los Sockets
     c                   Eval      len = %size(sockaddr_in)
     c                   Alloc     len           bindto
     c                   Alloc     len           connfrom

     C* ... Crear un nuevo Socket
     c                   Eval      lsock = socket(AF_INET: SOCK_STREAM:
     c                                            IPPROTO_IP)
     c                   If        lsock < 0
     c                   ExSr      TCPError
     c                   EndIf

     C* ... Asociar el Socket al puerto
     c                   Eval      p_sockaddr = bindto
     c                   Eval      sin_family = AF_INET
     c                   Eval      sin_addr = INADDR_ANY
     c                   Eval      sin_port = port
     c                   Eval      sin_zero = *ALLx'00'

     c                   If        bind(lsock: bindto: len) < 0
     c                   ExSr      TCPError
     c                   EndIf

     C* ... Indicar que queremos escuchar conexiones
     c                   if        listen(lsock: 5) < 0
     c                   ExSr      TCPError
     c                   endif
     c
     C                   endsr

     C*------------------
     C* AcceptConn: Aceptar Conexiones
     C*------------------
     c     AcceptConn    Begsr
     c                   DoU       len = %size(sockaddr_in)
     c* ... Acceptar la conexión.
     c                   Eval      len = %size(sockaddr_in)
     c                   Eval      csock = accept(lsock: connfrom: len)
     c                   If        csock < 0
     c                   ExSr      TCPError
     c                   EndIf

     c* If socket length is not 16, then the client isn't sending the
     c*  same address family as we are using...  that scares me, so
     c*  we'll kick that guy off...
     c                   if        len <> %size(sockaddr_in)
     c                   callp     close(csock)
     c                   ExSr      TCPError
     c                   endif
     c                   enddo
     c                   eval      p_sockaddr = connfrom
     c                   eval      clientip = %str(inet_ntoa(sin_addr))
     c
     C                   endsr
     C*------------------
     C* TCPError: Loguea errores TCP/IP y retorna con *ON
     C*------------------
     C     TCPError      BegSr
     C                   CallP     LogError('ERR':'TCP:'+%str(strerror(errno)))

     C                   Return    *ON
     C                   EndSr

     P OpenChanel      E
      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P CloseChanel     B
     D CloseChanel     PI
     C                   callp     close(csock)
     C                   callp     close(lsock)
     P CloseChanel     E
      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P ReadChanel      B
     D ReadChanel      PI              N

     D*-------------------------
     D* Convertir 2 Bytes de header a Largo del Mensaje
     D*-------------------------
     D hdr2len         DS
     D  hdr                    1      2a
     D  msglen                 1      2b 0

     D rc              S             10I 0
     D leidoHasta      S             10I 0
     D auxBuffer       S            768A
     D butesFaltantes  S             10I 0

     c* ... Lee 2 Bytes con longitud del Mensaje
     C                   Clear                   iISOStr
     C                   Eval       rc=RdLine(csock: %addr(hdr)
     C                                          : %size(hdr): *Off)
     C* ... Si Pudo Leer longitud ...
     C                   If         rc > 0
     C* ... ... Leer n=Long del Mensaje Bytes
     C                   Eval       rc=RdLine(csock: %addr(iISOStr)
     C                                             : msglen    : *On)
     C* ... ... Si NO pudo Leer el mensaje completo...
     C                   If         rc < msglen
     C                   Eval      leidoHasta=rc
     C                   Eval      BytesFaltantes=msglen-rc
     C                   Eval      rc=RdLine(csock: %addr(auxBuffer)
     C                                             :bytesFaltantes: *On)
     C                   If        rc = BytesFaltantes
     C                   Eval      %subst(iISOStr:leidoHasta+1:BytesFaltantes)=
     C                             %subst(auxBuffer:1:BytesFaltantes)
     C                   Else
     C                   CallP     LogError('ERR':'RdLn:Mensaje Incompleto    -'
     C                             +%str(strerror(errno)))
     C                   Return    *ON
     C                   Endif
     C                   EndIf
     C                   Else
     C                   CallP     LogError('ERR':'RdLn:Lectura Long. Msg     -'
     C                             +%str(strerror(errno)))
     C                   EndIf
     C                   Return    *Off
     P ReadChanel      E
      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P WriteChanel     B
     D WriteChanel     PI              N

     D*-------------------------
     D* Convertir 2 Bytes de header a Largo del Mensaje
     D*-------------------------
     D hdr2len         DS
     D  hdr                    1      2a
     D  msglen                 1      2b 0

     D rc              S             10I 0

     c                   Eval      msgLen=%LEN(%TRIM(oISOStr))
     C                   Eval      rc=WrLine(csock: hdr+oISOStr)
     C                   If        rc <  0
     C                   CallP     LogError('ERR':'WrLn:'+%str(strerror(errno)))
     C                   Return    *On
     C                   EndIf
     C                   Return    *Off
     P WriteChanel     E

      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P LogError        B
     D LogError        PI
     D  errorType                     3A   CONST
     D  errorText                   256A   CONST

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
     D wwCallStkCnt    S             10I 0

     c                   eval      wwMsgLen = %len(%trimr(errorText))
     c                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
     c                             errorText: wwMsgLen: '*INFO':
     c                             '*CTLBDY':wwCallStkCnt:       wwTheKey:dsEC)
     C                   Time                    RLTIME
     C                   Time                    WWFECH           12 0
     C                   Move      WWFECH        RLFECH
     C                   MoveL(P)  errorType     RLRGID
     C                   MoveL(P)  errorText     RLDAT1
     C                   Write     RLINKF
     P LogError        E
