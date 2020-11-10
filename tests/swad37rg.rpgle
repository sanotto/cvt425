*SRCMBRTXT:Switch-Adapter      -PF-380000-Imprime 
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD10RG                                       *
     H*                                                               *
     H*  PROGRAM NO: Consulta Plazos Fijos                            *
     H*                                                               *
     H*  DATE: 13/09/2017                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*----------------------------------------------------------------
     D BUFFDS          DS                  OCCURS(999)
     D  buflin                       27
     D*
     D PAGEDS          DS                  OCCURS(10)
     D  txtlin                       27

     D PAGSTR          S            270    BASED(PAGLIN)
     D PAGLIN          S               *

     D I               S              3  0
     D J               S              3  0
     D Desde           S              3  0
     D Hasta           S              3  0
     D BUFLEN          S              3  0
     D CANPAG          S              3  0
     D*----------------------------------------------------------------
     c*
     c                   ExSr      GenTicket
     C     1             Occur     PAGEDS
     c                   Eval      PAGLIN=%ADDR(PAGEDS)
     c                   Move      *Blanks       PARESP
     C                   MoveL(P)  PAGSTR        PARESP
     C*
     c                   ExSr      EndPgm
     c*--------------------------------------------------------------------
     c* GenTicket: Genera Ticket
     c*--------------------------------------------------------------------
     c     GenTicket     BegSr
     c*
     c     1             Occur     PageDS
     c*                                    123456789012345678901234567_9012
     c                   Eval      txtlin='P.FIJO COMP.CAM INST. EFEC.'
     c     2             Occur     PageDS
     c                   Eval      txtlin='TIT.:'+%subst(PANCCL:1:25)
     c     3             Occur     PageDS
     C                   Z-ADD     PAINDO        WWINDO            8 0
     c                   Eval      txtlin='DNI:'+%EditW(WWINDO:'        0')
     c     4             Occur     PageDS
     c                   Eval      txtlin='NRO:'+%EditW(PAISUC:'      0')+'-'
     c                                          +%EditW(PAINCE:'            0')
     c     5             Occur     PageDS
     c                   Z-Add     PAFALT        WWFECH            8 0
     c                   Call      'SBBAINFE'
     c                   Parm                    WWFECH
     c                   Parm      'IN'          EDTMDE            2
     c                   Z-Add     PAQDPL        WKQDPL            3 0
     c                   Eval      txtlin='FEC.:'+%EditW(WWFECH:'  /  /    ')+
     c                                    ' PZO:'+ %EditW(WKQDPL:'  0')
     c     6             Occur     PageDS
     c                   Z-Add     PAFVEN        WWFECH            8 0
     c                   Call      'SBBAINFE'
     c                   Parm                    WWFECH
     c                   Parm      'IN'          EDTMDE            2
     c                   Z-Add     PATTNA        WWTTNA            6 3
     c                   Eval      txtlin='VTO.:'+%EditW(WWFECH:'  /  /    ')+
     c                                    ' TNA:'+ %EditW(WWTTNA:'  0,   ')
     c*
     c                   Z-Add     PA$CAP        ww$cap           15 2
     C                   If        PATIPF='F'
     c                   Z-Add     PACUVA        ww$cap           15 2
     c                   endif
     c     7             Occur     PageDS
     c                   Eval      txtlin='CAP.:'+%EditW(ww$CAP:
     c                                            '             0,  ')
     c*
     c                   Z-Add     PA$INT        ww$int           15 2
     C                   If        PATIPF='F'
     C     PACOTI        div       1000000       ww$cot           12 6
     c     ww$int        div       ww$cot        ww$int           15 2
     c                   endif
     c     8             Occur     PageDS
     c                   Eval      txtlin='INT.:'+%EditW(WW$INT:
     c                                            '             0,  ')
     c     9             Occur     PageDS
     c                   Eval      txtlin='---------------------------'
     C                   If        PATIPF='F'
     C     PAPREC        div       1000000       WWprec           12 6
     c                   Eval      txtlin='TASA PRECA:'+%EditW(wwprec:
     c                                            '     0,      ')
     c                   EndIf
     c*                                    123456789012345678901234567_9012
     c     10            Occur     PageDS
     c                   Z-Add     PA$INE        ww$ine           15 2
     C                   If        PATIPF='F'
     c     ww$cap        add       ww$int        ww$ine           15 2
     c                   endif
     c                   Eval      txtlin='TOT.:'+%EditW(ww$INE:
     c                                            '             0,  ')
     c*
     c                   Move      '1P'          PANPAG
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* *InzSr: Subrutina de Inicialización
     c*--------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    PATIPF            1
     c                   Parm                    PAINDO           15 0
     c                   Parm                    PANCCL           30
     c                   Parm                    PAISUC            5 0
     c                   Parm                    PAINCE           11 0
     c                   Parm                    PAFALT            8 0
     c                   Parm                    PAFVEN            8 0
     c                   Parm                    PATTNA           13 7
     c                   Parm                    PAQDPL            5 0
     c                   Parm                    PA$CAP           15 2
     c                   Parm                    PA$INT           15 2
     c                   Parm                    PA$INE           15 2
     c                   Parm                    PACOTI           15 2
     c                   Parm                    PAPREC           15 2
     c                   Parm                    PACPES           15 2
     c                   Parm                    PACUVA           15 2
     c                   Parm                    PARESP          270
     c                   Parm                    PANPAG            2
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* EndPgm: Fin de Programa
     c*--------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
