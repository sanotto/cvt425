*SRCMBRTXT:BAHILI:Copia e imprime resumén         
      *//OVRDBF QTHILI SDBFIL/BAHILI
     FQTHILI    UP   E             DISK
     FBAHILI    O    E             DISK    RENAME(REBAHILI:RADD)
     FSGSYSV    IF   E             DISK
     FQSYSPRT   O    F  132        PRINTER
     c                   If        (HLCTRE='001' and
     c                              HLESAN=' '   and
     c                              %SubSt(HLDES3:14:2) ='CP') or
     c                             (HLCTRE='001' and
     c                              HLESAN=' '   and
     c                              %SubSt(HLDES3:14:2) ='CP')
     c                   Move      *Zeros        WWISUC            5
     c                   Move      *Zeros        WWICCC           11
     c                   Move      *Zeros        WWIHUR            5
     c                   Move      *Zeros        WWAFB1            1
     C                   Eval      WWAFB1=%SubSt(HLDES3:16:1)
     C                   Eval      WWISUC=%SubSt(HLDES3:17:5)
     C                   Eval      WWICCC=%SubSt(HLDES3:22:11)
     C                   Eval      WWIHUR=%SubSt(HLDES3:33:5)
     c                   Eval      %SubSt(HLDES3:14:24)=*Blanks
     c                   Move      WWISUC        PAISUC
     c                   Move      WWICCC        PAICCC
     c                   Move      WWIHUR        PAIHUR
     c                   EndIf
     C                   Move      PAIPGM        HLIPGM
     C                   Move      PAIFIL        HLIFIL
     C                   Move      AASFEI        HLFALT
     C                   Move      PAISUC        HLISUC
     C                   Move      PAICCC        HLICCC
     C                   Move      PAIHUR        HLIHUR
     C                   Move      *Zeros        HLFTRA
     C                   Move      WWAFB1        HLAFB1
     C                   Update    REBAHILI
     C                   Write     RADD
     c                   Move      *Blanks       Linea           132
     c                   Eval      Linea=%SubSt(HLDES3:5:132)
     C                   If        HLCTRE <> *BLANKS
     c                   If        HLCTRE='001'
     c                   Except    S1
     c                   Z-Add     1             CurLine           2 0
     C                   Else
     c                   Move      HLCTRE        I                 3 0
     c                   DoW       CurLine < I
     c                   Except    Blank
     c                   Add       1             CurLine
     c                   EndDo
     c                   Except    E
     c                   Move      *Blanks       Linea
     c                   EndIf
     C                   Else
     c                   Move      HLESAN        J                 1 0
     c                   DoW       J>0
     c                   Except    Blank
     c                   Sub       1             J
     c                   Add       1             CurLine
     c                   EndDo
     c                   Except    E
     c                   Move      *Blanks       Linea
     C                   EndIf
     C*-------------------------------------------------------------------------
     C* *INZSR : Subrutina de inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     c*
     c     *Like         Define    HLIPGM        PAIPGM
     c     *Like         Define    HLIFIL        PAIFIL
     c     *Like         Define    HLFALT        PAFALT
     c     *Like         Define    HLISUC        PAISUC
     c     *Like         Define    HLICCC        PAICCC
     c     *Like         Define    HLIHUR        PAIHUR
     c*
     c     1             Chain     RESGSYSV
     c*
     C     *Entry        PList
     c                   Parm                    PAIPGM
     c                   Parm                    PAIFIL
     c*
     C                   EndSr
     OQSYSPRT   E            S1                1
     O                       Linea
     O          E            E           0  0
     O                       Linea
     O          E            Blank       1
