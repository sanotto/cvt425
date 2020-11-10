*SRCMBRTXT:Link-Host to Host-Encoder ISO          
     H*-------------------------------------------------------------------------
     H* Sistema     : Conexión Host To Host
     H* Módulo      : ISO Encoder
     H* Autor       : Santiago Ottonello
     H* Fec. Inicial: 2006/10/11
     H*
     H* Recibe      :-DS De trabajo
     H*
     H* Devuelve    :-DS De trabajo
     H*
     H**************************************************************************
     H* Responsabilidades:
     H**************************************************************************
     H* Forma una tira ISO con los campos presentes en la DS de trabajo
     H*-------------------------------------------------------------------------

     P ISOEncoder      B
     D ISOEncoder      PI

     D dtaPtr          S              3S 0
     D i               S              3S 0
     D Bit             S              1A

     D                 DS
     D DSFEHO                  1     10  0
     D DSMESN                  1      2  0
     D DSDIAN                  3      4  0
     D DSFECH                  1      4  0
     D DSHORA                  5     10  0
     D                 DS
     D DSFECI                  1      6  0
     D DSANII                  1      2  0
     D DSMESI                  3      4  0
     D DSDIAI                  5      6  0

     C* ...
     C     oMsgType      Chain     RELIMSGC
     C                   Move      oMsgType      cMsgType          4
     C                   If        Not %Found()
     C                   CallP     LogError('ERR':'Mens. de Resp.:'+cMsgType+
     C                             ' NO Encontrada en archivo ISMSGC')
     C                   Return
     C                   EndIf
     C* ... Convertir Numericos a caracter y limpiar estructuras
     C                   Move      ICHB24        wwHB24            9
     C                   Clear                   oISOStr
     C* ... Prefijo ISO
     C                   Eval      %Subst(oISOStr:1:3)='ISO'
     C* ... Header del Mensaje
     C                   Eval      %Subst(oISOStr:4:9)=wwHB24
     C* ... Tipo del Mensaje
     C                   Eval      %Subst(oISOStr:13:4)=cMsgType
     C* ... Crear primary Bitmap
     C                   Eval      %Subst(oISOStr:17:16)=CreateBMP(0)
     C* ... Asignar offset a la zona de datos
     C                   Eval      dtaPtr=33
     C* ... Determinar Si debe informarse el Secondary BMP
     C                   If        %subst(wMsgFldBmp:1:1)=*ON
     C                   Eval      wMsgFldVal(1)=CreateBMP(1)
     C                   EndiF
     C* ... Copiar Datos al Mensaje
     C                   ExSr      AddFields
     C                   Return
     C*------------------
     C* GetOutFields: Determinar que campos deben Informarse en el Mensaje
     C*------------------
     C     GetOutFields  BegSr
     C                   Eval      wMsgFldBmp=*ZEROS
     C     oMsgType      Chain     RELIMSGD                           99
     C                   DoW       *IN99= *OFF
     C                   Eval      %subst(wMsgFldBmp:IDNFLD:1)=*ON
     C                   If        IDNFLD > 64
     C                   Eval      %subst(wMsgFldBmp:1:1)=*ON
     C                   EndIf
     C     oMsgType      ReadE     RELIMSGD                               99
     C                   EndDo
     C                   EndSr
     C*------------------
     C* AddFields  : Inserta Campos en el Mensaje
     C*------------------
     C     AddFields     BegSr
     C*
     C                   For       i=1 to 128
     C                   Eval      Bit=%subst(wMsgFldBmp:i:1)
     C                   If        Bit=*BLANK
     C                   Leave
     C                   EndIf
     C                   If        Bit=*ON
     C                   ExSr      ProcessField
     C                   EndIf
     C                   EndFor
     C*
     C                   EndSr
     C*--------------
     C* ProcessField: Procesar Campo
     C*--------------
     C     ProcessField  BegSr
     C*
     C* ... Es un campo con un valor especial? Obtener el valor correcto
     C                   Select
     C                   When      wMsgFldVal(i)='@DATTIM'
     C                   ExSr      GetDate
     C                   MoveL(P)  DSHORA        wMsgFldVal(i)
     C                   When      wMsgFldVal(i)='@TCRNBR'
     C                   ExSr      GetSecNO
     C                   MoveL(P)  INNSEC        wMsgFldVal(i)
     C                   EndSl
     C* ... Es un campo de largo Fijo? agregarlo
     C                   MoveA     mBitsXLong(i) wwCBIT            1 0
     C                   MoveA     mLongMax(i)   wwLONG            4 0
     C                   Move      wMsgFldVlen(i)wwVLEN            3 0
     C                   If        wwCBIT = *ZERO
     C                   Eval      %SubSt(oISOStr:DtaPtr:wwLONG)=
     C                             %SubSt(wMsgFldVal(i):1:wwLONG)
     C                   Eval      DtaPtr=DtaPtr+wwLONG
     C                   Else
     C* ... Es un campo de largo Variable, agregarle adelante el largo
     C                   Move      wMsgFldVLen(i)CHARLONG          4
     C                   Eval      %SubSt(oISOStr:DtaPtr:wwCBIT)=
     C                             %SubSt(CHARLONG:4-wwCBIT+1:wwCBIT-1)
     C                   Eval      DtaPtr=DtaPtr+wwCBIT
     C                   Eval      %SubSt(oISOStr:DtaPtr:wwVLEN)=
     C                             %SubSt(wMsgFldVal(i):1:wwVLEN)
     C                   Eval      DtaPtr=DtaPtr+wwVLEN
     C                   EndIf
     C*
     C                   EndSr
     C*----------------
     C* GetDate: Obtiene Fecha Hora
     C*----------------
     C     getDate       BegSr
     C                   TIME                    DSHORA
      * Accede a la entidad para obtener la fecha del sistema
     C     1             CHAIN     SGSYSV
     C                   Z-ADD     AASFEI        DSFECI
     C*
     C                   Z-ADD     DSHORA        WWHORA            6 0
     C                   MOVE      DSDIAI        DSDIAN
     C                   MOVE      DSMESI        DSMESN
     C                   MOVEL     DSFECI        WWFECH            6
     C*
     C                   EndSr
     C*-----------------
     C* GetSecNo: Genera Trace Number
     C*-----------------
     C     GetSecNO      BegSr
     C*
     C     KEYBANUME     KLIST
     C                   KFLD                    WNIPF1
     C                   KFLD                    WNIPF2
     C*
     C                   EVAL      WNIPF1='LNKH2H'
     C                   EVAL      WNIPF2=sConnName
     C*
     C     KEYBANUME     CHAIN     REBANUME
     C                   IF        %FOUND
     C                   ADD       1             WNIULN
     C                   UPDATE    REBANUME
     C                   ELSE
     C                   Z-ADD     1             WNIULN
     C                   WRITE     REBANUME
     C                   ENDIF
     C                   Z-ADD     WNIULN        INNSEC            6 0
     C*
     C                   EndSr
     P ISOEncoder      E

      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P CreateBMP       B
     D CreateBMP       PI            16A
     D  Number                        1  0 Const
     D BMP             S             16A
     D ptr             S              3S 0
     D i               S              3S 0

     C                   Eval      ptr= (Number * 64) +1
     C                   For       i=1 To 16
     C                   Eval      %subst(bmp:i)=
     C                               Bits2Bytes(%subst(wMsgFldBmp:ptr:4))
     c                   Eval      ptr=ptr+4
     C                   EndFor
     C                   Return    BMP
     P CreateBMP       E
      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P Bits2Bytes      B
     D Bits2Bytes      PI             1A
     D  Token                         4A   Const
     C                   Select
     C                   When      Token='0000'
     C                   Return    '0'
     C                   When      Token='0001'
     C                   Return    '1'
     C                   When      Token='0010'
     C                   Return    '2'
     C                   When      Token='0011'
     C                   Return    '3'
     C                   When      Token='0100'
     C                   Return    '4'
     C                   When      Token='0101'
     C                   Return    '5'
     C                   When      Token='0110'
     C                   Return    '6'
     C                   When      Token='0111'
     C                   Return    '7'
     C                   When      Token='1000'
     C                   Return    '8'
     C                   When      Token='1001'
     C                   Return    '9'
     C                   When      Token='1010'
     C                   Return    'A'
     C                   When      Token='1011'
     C                   Return    'B'
     C                   When      Token='1100'
     C                   Return    'C'
     C                   When      Token='1101'
     C                   Return    'D'
     C                   When      Token='1110'
     C                   Return    'E'
     C                   When      Token='1111'
     C                   Return    'F'
     C                   EndSl
     C                   Return    '0'
     P Bits2Bytes      E
