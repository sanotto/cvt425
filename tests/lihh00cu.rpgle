*SRCMBRTXT:Link-Host to Host-Procs de Comandos de 
     P GetUsrCmd       B
     D GetUsrCmd       PI             1A
     D  ConnNme                       6A

     D QConCmd         S             10A
     D Command         S              1A

     C                   Eval      QConCmd='CMD2'+ConnNme
     C                   Call      'QRCVDTAQ'
     C                   Parm                    QConCmd
     C                   Parm      'QGPL'        QLib             10
     C                   Parm      1             QDtaLen           5 0
     C                   Parm                    Command
     C                   Parm      0             QWaitTime         5 0
     C*
     C                   Return    Command
     C*

     P GetUsrCmd       E
