*SRCMBRTXT:Switch-Adapter      -Leyendas de Movimi
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
     FACCODI    IF   E           K DISK
     F*
     FCCMOCT01  IF   E           K DISK
     FCCCODI    IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER

     D DSCUMV          S             19
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
     C                   MOVE      'CA'          TIPOMO            2
     C     *Loval        Setll     REACCODI
     C                   Read      REACCODI                               99
     c                   dow       *in99 =*off
     C                   Z-ADD     FXIMCA        CODMOV            3 0
     C                   Movel(P)  FXNCOD        dscumv
     C                   ExSr      FixDsc
     c                   Except    texto
     C                   Read      REACCODI                               99
     c                   enddo
     C                   MOVE      'CC'          TIPOMO            2
     C     *Loval        Setll     RECCCODI
     C                   Read      RECCCODI                               99
     c                   dow       *in99 =*off
     C                   Z-ADD     BLIMCC        CODMOV            3 0
     C                   Movel(P)  BLNCOD        dscumv
     C                   ExSr      FixDsc
     c                   Except    texto
     C                   Read      RECCCODI                               99
     c                   enddo
     c
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C* FixDsc: Elimina caracteres "raros" de descripción
     C*-------------------------------------------------------------------------
     C     FixDsc        BegSr
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
     OQSYSPRT   E            TEXTO
     O                       TIPOMO
     O                                        +   1 ', '
     O                       CODMOV
     O                                        +   1 ', '
     O                       DSCUMV           +   1
