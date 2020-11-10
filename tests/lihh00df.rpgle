*SRCMBRTXT:Link-Host to Host-Definiciones         
     H*-------------------------------------------------------------------------
     H* Sistema     : Conexión Host To Host
     H* Módulo      : Global
     H* Autor       : Santiago Ottonello
     H* Fec. Inicial: 2006/10/11
     H*
     H* Uso         :
     H*
     H*-------------------------------------------------------------------------
     D* ... Estructura para contener el estado de la Conexión
     D ConnDS          DS
     D* ... Estado de la conexión
     D  sConnStatus                   1A
     D  sConnName                     6A
     D* ... N ormal o A lternativo
     D  sProcType                     1A
     D* ... Mensaje de Entrada
     D  iMsgType                      4S 0
     D  iISOLen                       3P 0
     D  iISOStr                     768
     D* ... Arrays de Trabajo
     D  wMsgFldVal                  255A                DIM(128)
     D  wMsgFldLen                    3P 0              DIM(128)
     D  wMsgFldBit                    1A                DIM(128)
     D  wMsgFldVar                    3P 0              DIM(128)
     D* ... Mensaje de Salida
     D  oMsgType                      4S 0
     D  oISOLen                       3P 0
     D  oISOStr                     768
     D*
