*SRCMBRTXT:Switch-Adapter      -Ultimos 10 Movimie
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD00R5                                       *
     H*                                                               *
     H*  PROGRAM NO:   ADAPTADOR DE SWITCH-ULTIMOS MOVIMIENTOS        *
     H*                                                               *
     H*  DATE: 18/03/2007                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     H*---------------------------------------------------------------*
     FACCTAC    IF   E           K DISK
     FACMOVD02  IF   E           K DISK
     FACMOVH05  IF   E           K DISK
     FACCODI    IF   E           K DISK
     F*
     FCCCTCT    IF   E           K DISK
     FCCMOCT01  IF   E           K DISK
     FCCMHCT06  IF   E           K DISK
     FCCCODI    IF   E           K DISK
     F*
     FSGSYSV    IF   E             DISk
     D*-------------------------------------------------------------------------
     D UMOVDS          DS                  OCCURS(8)
     D  fecumv                        8
     D  dscumv                       19
     D  impumv                       12

     D UMVSTR          S            312    BASED(PTRUMV)
     D PTRUMV          S               *
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
     C                   If        WKDBTC='CC'
     C                   ExSr      CCUMOV
     C                   Else
     C                   ExSr      ACUMOV
     C                   EndIf
     C*
     C     1             Occur     UMOVDS
     C                   Eval      PTRUMV=%ADDR(UMOVDS)
     C                   MoveL(P)  UMVSTR        WKUMOV
     C*
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C* CCUMOV: Ultimos Movimientos de Ctas Ctes
     C*-------------------------------------------------------------------------
     C     CCUMOV        BegSr
     C*
     C                   Z-ADD     WKDBSU        WWISUC
     C                   Z-ADD     WKDBCT        WWICAH
     C* ... Control de Existencia de cuenta
     C                   Move      *ZEROS        WKIERR
     C     KeyCuenta     Chain     RECCCTCT                           99
     C                   If        *IN99 = *ON
     C                   Move      '107'         WKIERR
     C                   ExSr      EndPgm
     C                   EndIf
     C*
     C* ... Determianr moneda, necesario para armar la clave
     C     KeyCuenta     Chain     RECCCTCT
     C                   Z-Add     BMIMON        WWIMON
     C* ... Movimientos del Dia
     C                   Z-ADD     1             I                 1 0
     C     KeyCuenta     Chain     RECCMOCT                           99
     C                   DoW       *IN99= *OFF and I<= 8
     C                   If        CFIASC = 0
     C     I             Occur     UMOVDS
     C                   Move      CFFING        fecumv
     C     CFIMCC        Chain     RECCCODI                           99
     C  N99              Movel(P)  BLNCOD        dscumv
     C   99              Movel(P)  *BLANKS       dscumv
     C                   ExSr      FixDsc
     C                   MOVE      CF$IMP        impumv
     C                   ADD       1             I                 1 0
     C                   EndIf
     C     KeyCuenta     ReadE     RECCMOCT                               99
     C                   EndDo
     C* ... Movimientos Historicos
     C     KeyHistCC     SetGT     RECCMHCC                           99
     C     KeyHistCC     ReadPE    RECCMHCC                               99
     C                   DoW       *IN99= *OFF and I<= 8
     C                   If        CEIASC = 0 and CEFASI <= AASFEI
     C     I             Occur     UMOVDS
     C                   Move      CEFING        fecumv
     C     CEIMCC        Chain     RECCCODI                           99
     C
     C  N99              Movel(P)  BLNCOD        dscumv
     C   99              Movel(P)  *BLANKS       dscumv
     C                   ExSr      FixDsc
     C                   MOVE      CE$IMP        impumv
     C                   ADD       1             I                 1 0
     C                   EndIf
     C     KeyHistCC     ReadPE    RECCMHCC                               99
     C                   EndDo
     C*
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* ACUMOV: Ultimos Movimientos de Caja de Ahorro
     C*-------------------------------------------------------------------------
     C     ACUMOV        BegSr
     C*
     C                   Z-ADD     WKDBSU        WWISUC
     C                   Z-ADD     WKDBCT        WWICAH
     C* ... Control de Existencia de cuenta
     C                   Move      *ZEROS        WKIERR
     C     KeyCuenta     Chain     REACCTAC                           99
     C                   If        *IN99 = *ON
     C                   Move      '207'         WKIERR
     C                   ExSr      EndPgm
     C                   EndIf
     C* ... Movimientos del Dia
     C                   Z-ADD     1             I                 1 0
     C     KeyCuenta     Chain     REACMOVD                           99
     C                   DoW       *IN99= *OFF and I<= 8
     C                   If        GCIASC = 0
     C     I             Occur     UMOVDS
     C                   Move      GCFING        fecumv
     C     GCIMCA        Chain     REACCODI                           99
     C  N99              Movel(P)  FXNCOD        dscumv
     C   99              Movel(P)  *BLANKS       dscumv
     C*
     C                   Z-ADD     GCISUC        WWISUC            5 0
     C                   Z-ADD     GCICAH        WWICAH           11 0
     C                   Z-ADD     GCFASI        WWFASI            8 0
     C                   MOVE      'IN'          PACINV            2
     C                   CALL      'SBBAINFE'
     C                   PARM                    WWFASI
     C                   PARM                    PACINV
     C*
     C                   Z-ADD     GCICHE        WWICHE            7 0
     C                   Z-ADD     GC$IMP        WW$LQ2           12 2
     C                   Z-ADD     GCIMCA        WWIMCA            3 0
     C*
     C                   ExSr      FixDsc
     C                   MOVE      GC$IMP        impumv
     C                   ADD       1             I                 1 0
     C                   EndIf
     C     KeyCuenta     ReadE     REACMOVD                               99
     C                   EndDo
     C* ... Movimientos Historicos
     C     KeyCuenta     SetGT     REACMOVH                           99
     C     KeyCuenta     ReadPE    REACMOVH                               99
     C                   DoW       *IN99= *OFF and I<= 8
     C                   If        GDIASC = 0 and GDFASI <= AASFEI
     C     I             Occur     UMOVDS
     C                   Move      GDFALT        fecumv
     C     GDIMCA        Chain     REACCODI                           99
     C  N99              Movel(P)  FXNCOD        dscumv
     C   99              Movel(P)  *BLANKS       dscumv
     C*
     C                   Z-ADD     GDISUC        WWISUC            5 0
     C                   Z-ADD     GDICAH        WWICAH           11 0
     C                   Z-ADD     GDFASI        WWFASI            8 0
     C                   MOVE      'IN'          PACINV            2
     C                   CALL      'SBBAINFE'
     C                   PARM                    WWFASI
     C                   PARM                    PACINV
     C*
     C                   Z-ADD     GDICHE        WWICHE            7 0
     C                   Z-ADD     GD$IMP        WW$LQ2           12 2
     C                   Z-ADD     GDIMCA        WWIMCA            3 0
     C*
     C                   ExSr      FixDsc
     C                   MOVE      GD$IMP        impumv
     C                   ADD       1             I                 1 0
     C                   EndIf
     C     KeyCuenta     ReadPE    REACMOVH                               99
     C                   EndDo
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR: Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *ENTRY        PLIST
     C                   PARM                    WKDBTC            2
     C                   PARM                    WKDBSU            5 0
     C                   PARM                    WKDBCT           10 0
     C                   PARM                    WKUMOV          312
     C                   PARM                    WKIERR            3
     C*
     C     *LIKE         Define    GCISUC        WWISUC
     C     *LIKE         Define    GCICAH        WWICAH
     C     *LIKE         Define    BMIMON        WWIMON
     C*
     C     KeyCuenta     KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWICAH
     C*
     C     KeyHistCC     KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWIMON
     C                   KFLD                    WWICAH
     C*
     C     1             CHAIN     SGSYSV
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* FixDsc: Elimina caracteres "raros" de descripción
     C*-------------------------------------------------------------------------
     C     FixDsc        BegSr
     C*
     c                   If        WWIMCA=247 or WWIMCA=248
     C                   Z-ADD     *ZERO         WWIMCA
     c                   Call      'BAPR00R4'
     c                   Parm                    WWISUC
     C                   Parm                    WWICAH
     c                   Parm                    WWFASI
     C                   Parm                    WWICHE
     C                   Parm                    WW$LQ2
     C                   Parm                    dscumv
     c                   EndIf
     C*
     C                   Eval      dscumv = %XLATE(Symbols:SymBlanks:dscUMv)
     C                   Eval      dscumv = %XLATE(Acentos:AceBlanks:dscumv)
     C                   Eval      dscumv = %XLATE(Apos:AposBlank:dscumv)
     C                   Eval      dscumv = %XLATE(lo:up:dscumv)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* EndPgm: Finalizar el Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
