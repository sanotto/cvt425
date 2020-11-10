*SRCMBRTXT:PRUEBA DE IMPRESION DE CODIGO DE BARRA 
     FPRUEBA01  O    E             PRINTER OFLIND(*IN98)
     ***
     D  CODBAR                 1     25
     D  ws_contante_1          1      8    inz('541/020/')
     D  ws_poliza              9     16  0
     D  ws_contante_2         17     25    inz('/000/00  ')
     C                   MOVE      '12345678'    ws_contante_1
     C                   Z-ADD     12345678      ws_poliza
     C                   MOVE      '12345678'    ws_contante_2
     C                   SETON                                          98
     C                   SETON                                        LR
