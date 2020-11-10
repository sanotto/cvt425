*SRCMBRTXT:Link-Refresh de Movimientos de CA Home 
     H
     FACCTAC03  IP   E           K DISK
     FACMOVD02  IF   E           K DISK
     FACMOVH05  IF   E           K DISK
     FACCODI    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     FUMOV      O    F  300        DISK
     D DSMOV1          DS
     D  V                      1    250
     D                                     DIM(10)
     D  MOVSTR                 1    250
     D DSMOV2          DS
     D  MOVENT                 1     25
     D  MOVFEC                 1      6  0
     D  MOVDSC                 7     16
     D  MOVSIG                17     17
     D  MOVIMP                18     25  0
     D CTADS           DS
     D  CTALNK                 1     18
     D  LINSUC                 1      4  0
     D  LINGRP                 5      6
     D  LINFIL                 7      9  0
     D  LINCAH                10     16  0
     C*-------------------------------------------------------------------------
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D Symbols         C                   '|°¬!"#$%&/()=?\¡¿*+~[]{}_-;,:.<>'
     D SymBlanks       C                   '                                '
     D Acentos         C                   'ñÑáéíóúäëïöüãõàèìòùâêîôû@'
     D AceBlanks       C                   'nNAEIOUAEIOUAOAEIOUAEIOU '
     D Apos            C                   ''''
     D APosBlank       C                   ' '
     C*-------------------------------------------------------------------------
     IREACCTAC      01
     I                                          FUIMON        L1
     C   L1              EXSR      WRTHED
     C   01              EXSR      WRTMOV
     C*-------------------------------------------------------------------------
     C* *INZSR: Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C*    *ENTRY        PLIST
     C*                  PARM                    RUNMDE            1
     C*
     C     WKEY01        KLIST
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICAH
     C*
     C     1             CHAIN     SGSYSV                             25
     C                   MOVE      AASFEI        FEEXT             6 0
     C                   Z-ADD     AASFEI        FECCOR            8 0
     C                   MOVEL     AASFEN        FEDDMM            4 0
     C*
     C* Contador
     C                   Z-ADD     *ZERO         X                 8 0
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTHED: Escritura Registro Cabecera
     C*-------------------------------------------------------------------------
     C     WRTHED        BEGSR
     C*
     C                   SELECT
     C     FUIMON        WHENEQ    01
     C                   MOVE      '11'          LINTC             2
     C     FUIMON        WHENEQ    02
     C                   MOVE      '15'          LINTC             2
     C                   OTHER
     C                   MOVE      '00'          LINTC             2
     C                   ENDSL
     C*
     C                   SETOFF                                       99
     C*
     C                   Z-ADD     2             CNTREG            8 0
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTMOV: Escribir Movimientos
     C*-------------------------------------------------------------------------
     C     WRTMOV        BEGSR
     C*
     C     LINTC         IFNE      '00'
     C     FUIUCA        ANDEQ     'S'
     C     FUFBAJ        ANDEQ     *ZERO
     C     FUIBAC        ANDEQ     *ZERO
     C     FUIGRC        ANDNE     'IN'
     C*
     C*                  CLEAR                   V
     C*
     C     WKEY01        CHAIN     REACMOVD                           25
     C*    RUNMDE        IFNE      'F'
     C     *IN25         IFEQ      *ON
     C                   GOTO      SINCAM
     C                   ENDIF
     C                   ADD       1             X
     C*
     C*    WKEY01        SETGT     REACMOVH
     C*    WKEY01        READPE    REACMOVH                               25
     C*    *IN25         DOWEQ     *OFF
     C*    X             ANDLT     10
     C*                  IF        GDIASC = 0
     C*    GDFASI        IFLE      FECCOR
     C*                  ADD       1             X
     C*                  MOVE      GDFALT        MOVFEC
     C*                  MOVE      GC$IMP        MOVIMP
     C*    GDIMCA        CHAIN     REACCODI                           25
     C*                  MOVEL(P)  FXNCOD        MOVDSC
     C*                  MOVE      MOVENT        V(X)
     C*                  ENDIF
     C*                  ENDIF
     C*    WKEY01        READPE    REACMOVH                               25
     C*                  ENDDO
     C*
     C  N99              EXCEPT    HEDREC
     C  N99              SETON                                        99
     C     X             IFGT      *ZERO
     C                   ADD       1             CNTREG
     C                   Z-ADD     FUISUC        LINSUC
     C                   MOVE      FUIGRC        LINGRP
     C                   Z-ADD     FUICAH        LINCAH
     C                   Z-ADD     *ZEROS        LINFIL
     C     FU$SOP        IFGE      *ZERO
     C                   MOVE      '+'           LINSIG            1
     C                   Z-ADD     FU$SOP        LINSAL            8 2
     C                   ELSE
     C                   MOVE      '-'           LINSIG
     C     -1            MULT      FU$SOP        LINSAL            8 2
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     SINCAM        ENDSR
     C*-------------------------------------------------------------------------
     C* WRTTRL: Escribir registro Final
     C*-------------------------------------------------------------------------
     C     WRTTRL        BEGSR
     C*
     C     LINTC         IFNE      '00'
     C     *IN99         ANDNE     *OFF
     C*                  EXCEPT    TRLREC
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FixDsc: Elimina caracteres "raros" de descripción
     C*-------------------------------------------------------------------------
     C     FixDsc        BegSr
     C*
     C                   Eval      MOVDSC = %XLATE(Symbols:SymBlanks:MOVDSC)
     C                   Eval      MOVDSC = %XLATE(Acentos:AceBlanks:MOVDSC)
     C                   Eval      MOVDSC = %XLATE(Apos:AposBlank:MOVDSC)
     C                   Eval      MOVDSC = %XLATE(lo:up:MOVDSC)
     C*
     C                   EndSr
     OUMOV      E            HEDREC
     O                                            5 'RHBMC'
     O                       FEDDMM               9
     O                                           11 '00'
     O                                           15 '0309'
     O                                           19 '.dat'
     O                                           29 '309'
     O                       FECCOR              37
     O                       X                   45
