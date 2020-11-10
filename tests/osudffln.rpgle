*SRCMBRTXT:Ottonello: UDF Formato Link            
     H NoMain
     D LnkFmt          PR            20A
     D   parsuc                       5S 0
     D   parcta                      11S 0
     P LnkFmt          B                   Export
     D LnkFmt          PI            20A
     D   parsuc                       5S 0
     D   parcta                      11S 0
     D*
     D CTADS           DS
     D  CtaLnk                 1     20
     D  LinSuc                 1      3  0
     D  LinFi2                 4      4
     D  LinGrp                 5      6
     D  LinFil                 7      9  0
     D  LinCta                10     16  0
     D*
     C                   Move      parcta        wwncta            7
     C                   MoveL     parcta        wwgru1            4
     C                   Move      wwgru1        wwgru2            2
     C                   Move      parsuc        LinSuc
     C                   Move      wwgru2        LinGrp
     C                   Move      wwncta        LinCta
     C                   Move      *Zeros        LinFil
     C                   Move      *Zeros        LinFi2
     C*
     C                   Return    CtaLnk
     C*
     P LnkFmt          E
