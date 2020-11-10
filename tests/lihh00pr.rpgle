*SRCMBRTXT:Link-Host to Host-Prototipos de Procedi
      /IF NOT DEFINED(LNKSRVPROT)
      /DEFINE LNKSRVPROT
     D MessageManager  PR
     D  Command                       1A   CONST

     D ISODecoder      PR

     D ISOEncoder      PR

     D GetUsrCmd       PR             1A
     D  ConnNme                       6A

     D OpenChanel      PR              N
     D  ConnNme                       6A

     D CloseChanel     PR

     D ReadChanel      PR              N

     D WriteChanel     PR              N

     D Byte2Bits       PR             4A
     D  Byte                          1A

     D LogError        PR
     D errorType                      3A   CONST
     D errorText                    256A   CONST

     D CreateBMP       PR            16A
     D  Number                        1  0 Const

     D Bits2Bytes      PR             1A
     D  Byte                          4A   Const

     D LogTalk         PR

     D ExecTrn         PR             2A
      /ENDIF
