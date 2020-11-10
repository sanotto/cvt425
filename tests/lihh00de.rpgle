*SRCMBRTXT:Link-Host to Host-Decoder ISO          
     H*-------------------------------------------------------------------------
     H* Sistema     : Conexión Host To Host
     H* Módulo      : ISO Decoder
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
     H* Toma la tira ISO presente en la DS de trabajo y la descompone en sus
     H* campos componentes
     H*-------------------------------------------------------------------------
     P ISODecoder      B                   EXPORT
     D ISODecoder      PI

     D* ... Estructura para contener el Desarme del Mensaje de Entrada

     D* ... Estructura de un Msg ISO
     D ISOMsgDS        DS                  BASED(ptrISOMsgDS)
     D   ISOPrefix             1      3a
     D   ISOHeader             4     12  0
     D   ISOMsgType           13     16  0
     D   ISOPrimaryBM         17     32

     D* ... Puntero para Basar Estructura de un Mensaje ISO
     D ptrISOMsgDS     S               *
     D* ... Bitmap convertido
     D BitMap          s            128a
     D* ... Puntero a la Zona de Inicio de los Datos de un mensaje
     D Byte            s              1A
     D dtaPtr          s              3s 0 INZ(33)
     D bit             s              1a
     D lenChr          s              4a
     D lenFld          s              4s 0
     D CprBmp          s             16A
     D HasSecBitMap    s              1A
     D i               s              3s 0

     C* ... Limpiar las estructuras de Bitmap y Campos
     C                   Clear                   BitMap
     C                   Clear                   wMsgFldVal
     C                   Clear                   wMsgFldBmp
     C                   Clear                   wMsgFldVLen
     C* ... Asignar Tira de Entrada sobre Estructura de Msg ISO
     C                   Eval      ptrISOMsgDS=%Addr(iISOStr)
     C* ... Guardar Tipo de Mensaje en Estructura de Trabajo
     C                   Eval      iMsgType=ISOMsgType
     C* ... Pone Puntero en la zona de inicio de datos del Msg (Offset 33)
     C                   Eval      dtaPtr=33
     C* ... Decodificar el PrimaryBitmap
     C                   Eval      CprBmp=ISOPrimaryBM
     C                   ExSr      DecBmp
     C* ... Verificar Presencia del Secondary Bitmap y cargarlo si corresponde
     C                   If        %subst(BitMap:1:1)=*ON
     C* ... El Sec Bmp  es el campo 1 y por lo tanto los 1eros 16 char de datos
     C                   Eval      CprBmp=%subst(iISOStr:dtaPtr:16)
     C                   ExSr      DecBmp
     C                   Endif
     C                   Eval      wMsgFldBmp=BitMap
     C* ... Ahora tenemos la lista de los campos a procesar, procesarlos...
     C     1             Do        128           y                 3 0
     C                   Eval      Bit= %SubSt(BitMap:y)
     C                   Select
     C* ... ... Si la pos correspondiente posee un ' ' se acabo el bitmap
     C                   When      Bit=*Blanks
     C                   Leave
     C* ... ... Si la pos correspondiente posee un 1 el campo esta informado
     C                   When      Bit=*ON
     C* ... ... Es de Longitud fija ?
     C                   Move      mBitsXLong(y) wwCBIT            1 0
     C                   If        wwCBIT=*ZERO
     C                   Eval      LenFld=mLongMax(y)
     C                   Else
     C* ... ... Es de Longitud Variable
     C                   Eval      LenChr=*zeros
     C* ... ... ... Recuperar el largo del campo
     C                   Eval      %subst(LenChr: 5-wwCBIT:wwCBIT)=
     C                             %subst(iISOStr:dtaPtr:wwCBIT)
     C                   Move      LenChr        LenFld
     C* ... ... Ajustar Puntero para no tomar zona de largo de campo
     C                   Eval      dtaPtr=dtaPtr+wwCBIT
     C                   EndIf
     C* ... Extraer Datos y guardar en DS de Trabajo
     C                   Eval      wMsgFldVal(y)=%subst(iISOStr:dtaPtr:LenFld)
     C                   Eval      %subst(wMsgFldBmp:y:1)=*ON
     C                   Eval      wMsgFldVLen=LenFld
     C* ... Avanzar Puntero a Próximo Campo
     C                   Eval      dtaPtr=dtaPtr+LenFld
     C                   EndSl
     C                   EndDo
     C                   Return

     C*------------------------------------------------------------------------
     C* DecBmp: Decodificar Bitmap recibido en CprBmp
     C*------------------------------------------------------------------------
     C     DecBmp        BegSr
     C     1             Do        16            x                 2 0
     C                   Eval      Byte=%subst(CprBmp:x:1)
     C                   Eval      BitMap=%trim(BitMap)+Byte2Bits(Byte)
     C                   EndDo
     C                   EndSr
     P ISODecoder      E

      *-------------------------------------------------------------------------
      *
      *-------------------------------------------------------------------------
     P Byte2Bits       B
     D Byte2Bits       PI             4A
     D  Byte                          1A
     D* ... Conversion de Dig Hexa a Digitos Binarios
     D Bits            s              4a
     C                   Select
     C                   When      Byte='0'
     C                   Eval      Bits='0000'
     C                   When      Byte='1'
     C                   Eval      Bits='0001'
     C                   When      Byte='2'
     C                   Eval      Bits='0010'
     C                   When      Byte='3'
     C                   Eval      Bits='0011'
     C                   When      Byte='4'
     C                   Eval      Bits='0100'
     C                   When      Byte='5'
     C                   Eval      Bits='0101'
     C                   When      Byte='6'
     C                   Eval      Bits='0110'
     C                   When      Byte='7'
     C                   Eval      Bits='0111'
     C                   When      Byte='8'
     C                   Eval      Bits='1000'
     C                   When      Byte='9'
     C                   Eval      Bits='1001'
     C                   When      Byte='A'
     C                   Eval      Bits='1010'
     C                   When      Byte='B'
     C                   Eval      Bits='1011'
     C                   When      Byte='C'
     C                   Eval      Bits='1100'
     C                   When      Byte='D'
     C                   Eval      Bits='1101'
     C                   When      Byte='E'
     C                   Eval      Bits='1110'
     C                   When      Byte='F'
     C                   Eval      Bits='1111'
     C                   EndSl
     C                   Return    Bits
     P Byte2Bits       E

