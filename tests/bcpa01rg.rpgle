*SRCMBRTXT:GENERA Archivo plano Operaciones Pasiva
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Genera archivo Plano BCOPPA                    *
     H*                 c/salida a Carpeta                            *
     H*                                                               *
     H*  PROGRAM NO: BCPA01RG                                         *
     H*                                                               *
     H*  DATE:    16/11/2001                                          *
     H*                                                               *
     H*  AUTHOR:  SERGIO CORTES                                       *
     H*                                                               *
     F*----------------------------------------------------------------*
     FBCOPPA    IP   E             DISK
     FBCOPPL    UF A E             DISK
     D*----------------------------------------------------------------*
     D                 DS
     D  DSTOD1                 1    489
     D  NSUC                   1      3  0
     D  TOPER                  4      7  0
     D  NCTA                   8     22  0
     D  NPFIJO                23     37  0
     D  TPERSO                38     38
     D  FOPER                 39     46  0
     D  PLAZO                 47     51  0
     D  CAPITA                52     66  2
     D  INTDEV                67     81  2
     D  CER                   82     96  2
     D  MONTO                 97    111  2
     D  TPACTA               112    119  4
     D  TREFE                120    127  4
     D  TID01                128    130  0
     D  NIDE01               131    144  0
     D  IDE01                145    179
     D  TID02                180    182  0
     D  NIDE02               183    196  0
     D  IDE02                197    231
     D  TIDE03               232    234  0
     D  NIDE03               235    248  0
     D  IDE03                249    283
     D  TIDE04               284    286  0
     D  NIDE04               287    300  0
     D  IDE04                301    335
     D  TIDE05               336    338  0
     D  NIDE05               339    352  0
     D  IDE05                353    387
     D  CTACON               388    399  0
     D  B2DOLA               400    414  2
     D  BLIBOR               415    429  2
     D  B3PESO               430    444  2
     D  BODE13               445    459  2
     D  BODE06               460    474  2
     D  BODE05               475    489  2
     C                   EXSR      SRPROC
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para inicio del programa
     C*----------------------------------------------------------------*
      *
     C     *INZSR        BEGSR
     C                   MOVE      *ON           *IN99
      *
     C     *ENTRY        PLIST
     C                   PARM                    PAFAAM            6 0
      *
     C                   EXSR      LIMPIA
     C                   ENDSR
      *
     C*----------------------------------------------------------------*
     C* LIMPIA - Limpia e inicializa campos
     C*----------------------------------------------------------------*
      *
     C     LIMPIA        BEGSR
     C                   MOVE      *BLANK        DSTOD1
     C                   MOVE      *BLANK        PLISTR
     C                   ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRPROC - Genera Operaciones Pasivas (plano)
     C*----------------------------------------------------------------*
      *
     C     SRPROC        BEGSR
     C*
     C                   Z-ADD     P1ISUC        NSUC
     C                   Z-ADD     P1IMON        TOPER
     C                   Z-ADD     P1INCT        NCTA
     C                   Z-ADD     P1INCE        NPFIJO
     C                   MOVE      P1TIPP        TPERSO
     C                   Z-ADD     P1FALT        FOPER
     C                   Z-ADD     P1QDPL        PLAZO
     C                   Z-ADD     P1$CAP        CAPITA
     C                   Z-ADD     P1$IND        INTDEV
     C                   Z-ADD     P1$MAJ        CER
     C                   Z-ADD     P1$IMP        MONTO
     C                   Z-ADD     P1TTNA        TPACTA
     C                   Z-ADD     P1PCOM        TREFE
     C                   Z-ADD     P1IT01        TID01
     C                   Z-ADD     P1CUI1        NIDE01
     C                   MOVE      P1DNI1        IDE01
     C                   Z-ADD     P1IT02        TID02
     C                   Z-ADD     P1CUI2        NIDE02
     C                   MOVE      P1DNI2        IDE02
     C                   Z-ADD     P1IT03        TIDE03
     C                   Z-ADD     P1CUI3        NIDE03
     C                   MOVE      P1DNI3        IDE03
     C                   Z-ADD     P1IT04        TIDE04
     C                   Z-ADD     P1CUI4        NIDE04
     C                   MOVE      P1DNI4        IDE04
     C                   Z-ADD     P1IT05        TIDE05
     C                   Z-ADD     P1CUI5        NIDE05
     C                   MOVE      P1DNI5        IDE05
     C                   Z-ADD     P1ICCE        CTACON
     C                   Z-ADD     P1$BUU        B2DOLA
     C                   Z-ADD     P1$BUA        BLIBOR
     C                   Z-ADD     P1$P03        B3PESO
     C                   Z-ADD     P1$A13        BODE13
     C                   Z-ADD     P1$A06        BODE06
     C                   Z-ADD     P1$A05        BODE05
     C*
     C                   MOVEL(P)  DSTOD1        PLISTR
     C*
     C                   WRITE     REBCOPPL
     C*
     C                   ENDSR
     C*
     C*=====================================================================
