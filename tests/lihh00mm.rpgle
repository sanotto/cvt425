*SRCMBRTXT:Link-Host to Host-Message Manager      
     H*-------------------------------------------------------------------------
     H* Sistema     : Conexión Host To Host
     H* Módulo      : Message Manager
     H* Autor       : Santiago Ottonello
     H* Fec. Inicial: 2006/10/10
     H*
     H* Recibe      :-Comando con Acción a Efectuar
     H*                  -Generar Msg de Logon
     H*                  -Generar Msg de Logoff
     H*                  -Procesar Msg (Generar una respuesta p/el mismo)
     H*              -Puntero a Tira ISO
     H*              -Longitud  Tira ISO
     H*
     H* Devuelve    :-Rellena Buffer de salida con tira ISO de Respuesta
     H*              -Rellena Longitud de Buffer de Salida
     H*
     H**************************************************************************
     H* Responsabilidades:
     H**************************************************************************
     H* · Genera mensaje  ISO de Logon /  Logoff a solicitud· Decodifica mensaje
     H* ISO y pone en Repositorio de Mensajes· Transfiere Mensajes de Tipo trans
     H* ccional a Transaccionador· Identifica el tipo de mensaje de respuesta ad
     H* cuado.· Arma mensaje de respuesta tomando datos de repositorio de mensaj
     H* s · Envía a adaptador de línea para su escritura en la misma
     H*-------------------------------------------------------------------------
     P MessageManager  B
     D MessageManager  PI
     D  Command                       1A   CONST

+----C                   Select
+----C                   When      Command=AM_CMPRCMSG
|    C                   Exsr      ProcessMsg
+----C                   When      Command=AM_CMGENLOGON
|    C                   Exsr      GenLogon
+----C                   When      Command=AM_CMGENLOGOFF
|    C                   Exsr      GenLogoff
+----C                   EndSl
     C                   CallP     ISOEncoder()
     C                   CallP     LogTalk()
     C                   Return

     C*-------------------------------------------------------------------------
     C* ProcessMessage: Procesar Mensaje
     C*-------------------------------------------------------------------------
     C     ProcessMsg    BegSr
     C*
     C                   CallP     ISODecoder()
     C* ... Buscar Tipo de Mensaje para respuesta
     C     iMsgType      Chain     RELIMSGC
+----C                   If        %Found()
|    C* ... ... ... ...  Det el tipo de Mensaje para respuesta
|    C                   Eval      oMsgType=ICRMSG
|    C* ... ... ... ...  Det q/campos deben inf. en el msg de respuesta
|    C                   ExSr      GetOutFields
|    C     iMsgType      Chain     RELIMSGC
|+---C                   If        %Found()
||+--C                   If        ICTMSG='T'
|||  C                   ExSr      ProcTrnMsg
||+--C                   Else
|||  C                   ExSr      ProcCtlMsg
||+--C                   EndIf
|+---C                   EndIf
+----C                   Else
|    C* ... ... ... ...  oMsgType=0 => No se escribira respuesta
|    C                   Eval      oMsgType=*ZERO
|+---C                   If        iMsgType < 9000
||   C                   CallP     LogError('ERR':'MsMn:Mensaje desconocido')
|+-- C                   Else
||   C                   CallP     LogError('ERR':'MsMn:Mensaje Rebotado   ')
|+---C                   EndIf
+----C                   EndIf
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* GetOutFields: Determinar que campos deben Informarse en el Mensaje
     C*---------------------------------------------------------------------
     C     GetOutFields  BegSr
     C                   Eval      wMsgFldBmp=*ZEROS
     C     oMsgType      Chain     RELIMSGD                           99
+----C                   DoW       *IN99= *OFF
|    C                   Eval      %subst(wMsgFldBmp:IDNFLD:1)=*ON
|+---C                   If        IDNFLD > 64
||   C                   Eval      %subst(wMsgFldBmp:1:1)=*ON
|+---C                   EndIf
|    C     oMsgType      ReadE     RELIMSGD                               99
+----C                   EndDo
     C                   EndSr

     C*-------------------------------------------------------------------------
     C* GenLogon: Rellena Campos para Armar un Msg de Logon
     C*-------------------------------------------------------------------------
     C     GenLogon      BegSr
     C*
     C                   Eval      oMsgType=800
     C*
     C                   Eval      %subst(wMsgFldVal: 7:1)=*ON
     C                   Eval      %subst(wMsgFldVal:11:1)=*ON
     C                   Eval      %subst(wMsgFldVal:70:1)=*ON
     C                   Eval      %subst(wMsgFldVal:39:1)=*ON
     C*
     C                   MoveL(P)  '@DATTIM'     wMsgFldVal(7)
     C                   MoveL(P)  '@TRCNBR'     wMsgFldVal(11)
     C                   MoveL(P)  '001'         wMsgFldVal(70)
     C                   MoveL(P)  '00'          wMsgFldVal(39)
     C*
     C                   EndSr

     C*-------------------------------------------------------------------------
     C* GenLogoff: Rellena Campos para Armar un Msg de Logon
     C*-------------------------------------------------------------------------
     C     GenLogoff     BegSr
     C*
     C                   Eval      oMsgType=800
     C*
     C                   Eval      %subst(wMsgFldVal: 7:1)=*ON
     C                   Eval      %subst(wMsgFldVal:11:1)=*ON
     C                   Eval      %subst(wMsgFldVal:70:1)=*ON
     C                   Eval      %subst(wMsgFldVal:39:1)=*ON
     C*
     C                   MoveL(P)  '@DATTIM'     wMsgFldVal(7)
     C                   MoveL(P)  '@TRCNBR'     wMsgFldVal(11)
     C                   MoveL(P)  '002'         wMsgFldVal(70)
     C                   MoveL(P)  '00'          wMsgFldVal(39)
     C*
     C                   EndSr

     C*-------------------------------------------------------------------------
     C* ProcCtlMsg: Procesa Mensaje de Control
     C*-------------------------------------------------------------------------
     C     ProcCtlMsg    BegSr
     C*
+----C                   Select
|    C*            //Solicitud de Logon Aprobada X Base24
+----C                   When      iMsgType=810         and
|    C                             wMsgFldVal(39)='00'  and
|    C                             wMsgFldVal(70)='001'
|    C                   Eval      sConnStatus=CO_STONLINE
|    C*            //Solicitud de Logon Aprobada X Nosotros
+----C                   When      iMsgType=800         and
|    C                             wMsgFldVal(70)='001'
|    C                   Eval      wMsgFldVal(39)='00'
|    C                   Eval      sConnStatus=CO_STONLINE
|    C*            //Solicitud de Logoff Aprobada X Base24
+----C                   When      iMsgType=810         and
|    C                             wMsgFldVal(70)='002' and
|    C                             wMsgFldVal(39)='00'
|    C                   Eval      sConnStatus=CO_STOFFLINE
|    C*            //Solicitud de Logoff Aprobada X Nosotros
+----C                   When      iMsgType=800         and
|    C                             wMsgFldVal(70)='002'
|    C                   Eval      wMsgFldVal(39)='00'
|    C                   Eval      sConnStatus=CO_STOFFLINE
|    C*            //Solicitud de Echo Test
+----C                   When      iMsgType=800         and
|    C                             wMsgFldVal(70)='301'
|+---C                   If        sConnStatus=CO_STONLINE
||   C                   Eval      wMsgFldVal(39)='00'
|+---C                   Else
||   C                   Eval      wMsgFldVal(39)='91'
|+---C                   EndIf
+----C                   EndSl
     C*
     C                   EndSr

     C*-------------------------------------------------------------------------
     C* ProcTrnMsg: Procesa Mensaje de Transacción
     C*-------------------------------------------------------------------------
     C     ProcTrnMsg    BegSr
     C*
     C* ... Procesamos transacciones solo si estamos online
+----C                   If        sConnStatus<>CO_STONLINE
     C* ... ... Indicar que se deniega la transaccion por estar offline
     C                   Eval      wMsgFldVal(resp_cde)='05'
     C                   LeaveSr
+----C                   EndIf
     C                   Eval      wMsgFldVal(resp_cde)=ExecTrn()
     C
     C                   EndSr

     P MessageManager  E


     P LogTalk         B
     D LogTalk         PI

     D splitDS         DS
     D  msgStr                 1    768a
     D   RCDAT1                1    256A
     D   RCDAT2              257    512A
     D   RCDAT3              513    768A

     C                   Time                    RLTIME
     C                   Time                    WWFECH           12 0
     C                   Move      WWFECH        RLFECH
     C                   MoveL(P)  'IN '         RLRGID
     C                   MoveL(P)  iISOStr       msgStr
     C                   Move      RCDAT1        RLDAT1
     C                   Move      RCDAT2        RLDAT2
     C                   Move      RCDAT3        RLDAT3
     C                   Write     RLINKF
     C                   MoveL(P)  'OUT'         RLRGID
     C                   MoveL(P)  oISOStr       msgStr
     C                   Move      RCDAT1        RLDAT1
     C                   Move      RCDAT2        RLDAT2
     C                   Move      RCDAT3        RLDAT3
     C                   Write     RLINKF
     P LogTalk         E
