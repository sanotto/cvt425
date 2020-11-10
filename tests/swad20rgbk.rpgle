*SRCMBRTXT:Switch-Adapter      -PF-280000-Cons PF 
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
     H*  PADBSU   5 0   Sucursal PF
     H*  PADBCT  11 0   Cuenta PF
     H*  PAIERR   3     De Salida, código de error, ver NPC12008
     H*  PARESP 360     10 lineas de 36 para el ticket
     H*  PANPAG   2     De Entrada Salida
     H*                 Si viene un nro NN devuelve página NN+1 y NN+1
     H*                 en PANPAG.
     H*                 Si hay una sola página se devuelve 1P
     H*                 Si la página NN+1 es la ultima se devuelve LP
     H*  PANCER  10 0   Nro de cert. si se selecciono 1 cero si todos
     F*----------------------------------------------------------------
     FPFCERT06  IF   E           K DISK
     FPFCERT    IF   E           K DISK    RENAME(REPFCERT:RPF)
     FBAICCL    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FPFDCCL01  IF   E           K DISK
     F*----------------------------------------------------------------
     D BUFFDS          DS                  OCCURS(999)
     D  buflin                       36
     D*
     D PAGEDS          DS                  OCCURS(10)
     D  txtlin                       27

     D PAGSTR          S            360    BASED(PAGLIN)
     D PAGLIN          S               *

     D TotLineas       S              3  0
     D TotPaginas      S              3  0
     D I               S              3  0
     D J               S              3  0
     D Desde           S              3  0
     D Hasta           S              3  0
     D BUFLEN          S              3  0
     D CANPAG          S              3  0
     D CANLXP          S              1  0  INZ(7)
     D HAYPF           S              1
     D*----------------------------------------------------------------
     c*
     c                   Move      *off          HayPf
     c                   If        PANCER = *Zero
     c                   MoveL(P)  '312'         PAIERR
     c                   ExSr      LoadAllPF
     c                   ExSr      LstOnePage
     c                   Else
     c                   MoveL(P)  '000'         PAIERR
     c                   ExSr      LstOnePF
     c                   EndIf
     c* ... Mover lineas a salida
     c*
     C     1             Occur     PAGEDS
     C                   Eval      PAGLIN=%ADDR(PAGEDS)
     C*                  MoveL     PAGST1        PARESP
     C                   MoveL     PAGSTR        PARESP
     c*
     c                   If        HAYPF = *Off
     c                   Eval      PAIERR='311PF no encontrado                 '
     C                   Move      *Blanks       PARESP
     c                   EndIf
     c*
     c                   ExSr      EndPgm
     c*--------------------------------------------------------------------
     c* LstOnePage: Listar una Página
     c*--------------------------------------------------------------------
     c     LstOnePage    BegSr
     c*
     c* ... Determinar página actual y próxima página
     c                   If        PANPAG='1P'
     c                   Z-Add     1             NroPag
     c                   Else
     c                   TestN                   PANPAG               99
     c   99              Move      PANPAG        NroPag            2 0
     c  N99              Z-Add     1             NroPag
     c                   EndIf
     c*
     c     1             Add       NroPag        ProxPag           2 0
     c* ... Cargar Página Actual desde BUFFDS a PAGEDS
     c                   Eval       i= ((NroPag -1) * CANLXP ) + 1
     c*
     c*                  If        NroPag > 1
     c*                  Sub       1             i
     c*                  EndIf
     c*
     c                   For       J=1 to CANLXP
     c     i             Occur     BuffDS
     c     j             Occur     PageDS
     c                   Eval      txtlin=buflin
     c                   Add       1             i
     c                   EndFor
     c* ... Determinar próxima página para informar
     c                   Select
     c                   When      TotPaginas = 1
     c                   Move      '1P'          PANPAG
     c                   When      TotPaginas > 1 and  ProxPag > TotPaginas
     c                   Move      'LP'          PANPAG
     c                   Other
     c                   Move      ProxPag       PANPAG
     c                   EndSl
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* LoadAllPF: Listar todos los plazos fijos para la cuenta
     c*--------------------------------------------------------------------
     c     LoadAllPF     BegSr
     c*
     c                   Z-Add     *Zero         i
     c                   ExSr      AddTit
     c*
     c     KIZ0600       SetGt     REPFCERT
     c     KIZ0600       ReadPE    REPFCERT                               99
     c                   DoW       *In99 = *Off
     c                   if        izfapa=0 and iziece='C'
     c                   If        Count = CANLXP
     c                   ExSr      WrtBlank
     c                   ExSr      AddTit
     c* ... Una sola pagina max 7
     c*                  Leave
     c                   EndIf
     c                   ExSr      AddDeta
     c                   EndIf
     c     KIZ0600       ReadPE    REPFCERT                               99
     c                   EndDo
     c*
     c                   Z-Add     i             TotLineas
     c     i             Div       CANLXP        TotPaginas
     c                   MvR                     Resto             3 0
     c                   If        Resto > *Zero
     c                   Add       1             TotPaginas
     c                   Endif
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* AddTit  : Agregar Título
     c*--------------------------------------------------------------------
     c     AddTit        BegSr
     c*
     c                   Add       1             i
     c     i             Occur     BuffDS
     c*                                    ss-123456 aammdd 1234567,89|____
     c                   Eval      BufLin='NRO.CERT. VCTO.    IMPORTE      '
     c*
     c                   Z-Add     2             Count             3 0
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* AddDeta : Agregar Detalle
     c*--------------------------------------------------------------------
     c     AddDeta       BegSr
     c*
     c                   Add       1             i
     c                   Add       1             Count
     c                   Move      *On           HayPf
     c*
     c                   Move      IZISUC        CHISUC            1
     c                   Move      IZINCE        CHINCE            6
     c                   Z-ADD     IZ$CAP        WWIMPO            9 2
     c*
     C*                  ExSr      ChkUVA
     c*
     C                   Z-Add     IZFVEN        WWFECH            8 0
     C                   Call      'SBBAINFE'
     c                   Parm                    WWFECH
     C                   Parm      'IN'          EDTMDE            2
     C                   Move      WWFECH        WWFEC6            6 0
     c*
     C     i             Occur     BuffDS
     C                   Eval      BufLin=CHISUC+'-'+CHINCE    + ' ' +
     C                             %EditW(WWFEC6:'      ') + ' ' +
     C                             %EditW(WWIMPO:'       0,  ')
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* WrtBlank : Escribe linea en blanco y agrega en la ds
     c*--------------------------------------------------------------------
     c     WrtBlank      BegSr
     c                   Add       1             i
     c     i             Occur     BuffDS
     c                   Eval      BufLin = *blanks
     c                   EndSr
     c*--------------------------------------------------------------------
     c* ChkUVA: Chequea si el PF es UVA y pone el importe en UVA
     c*--------------------------------------------------------------------
     c     ChkUVA        BegSr
     c                   If        IZIGCE ='F'
     c                   CALL      'BAMU00RG'
     c                   PARM      99999         XXISUC            5 0
     c                   PARM      50            XXIMON            9 0
     c                   PARM      *ZEROS        XXFECH            8 0
     c                   PARM                    XXCOTI           12 6
     c
     c     IZ$CAP        div       XXCOTI        WWIMPO
     c                   EndIf
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* LstOnePF: Lista 1 solo PF
     c*--------------------------------------------------------------------
     c     LstOnePF      BegSr
     c*
     c*
     C                   Move      PANCER        kkINCE            7 0
     C                   Z-Add     kkince        WKINCE
     c     KIZ0000       Chain     RPF                                99
     c                   If        *In99 = *Off
     c                   Move      *On           HayPf
     C                   ExSr      GenTicket
     c                   ExSr      EndPgm
     c                   Else
     c                   Eval      PAIERR='305PF No encontrado'
     C                   Endif
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* GenTicket: Genera Ticket
     c*--------------------------------------------------------------------
     c     GenTicket     BegSr
     c*
     C     WKEY02        Chain     REBAICCL                           99
|    C   99              Eval      PAIERR='304Cta.Cte. No encontrada'
|    C   99              LeaveSr
     C     WKEY03        Chain     REBADCCL                           99
|    C   99              Eval      PAIERR='304Cta.Cte. No encontrada'
|    C   99              LeaveSr
     c     KIZ0000       Chain     REPFDCCL                           99
|    C   99              Eval      PAIERR='304Datos de Firm.No Enc. '
|    C   99              LeaveSr
     c*
     C                   Call      'SWAD37RG'
     c                   Parm      IZIGCE        PATIPF            1
     c                   Parm      OTINDO        PAINDO           15 0
     c                   Parm      OSNCCL        PANCCL           30
     c                   Parm      IZISUC        PAISUC            5 0
     c                   Parm      IZINCE        PAINCE           11 0
     c                   Parm      IZFALT        PAFALT            8 0
     c                   Parm      IZFVEN        PAFVEN            8 0
     c                   Parm      IZTTNA        PATTNA           13 7
     c                   Parm      IZQDPL        PAQDPL            5 0
     c                   Parm      IZ$CAP        PA$CAP           15 2
     c                   Parm      IZ$INT        PA$INT           15 2
     c                   Parm      IZ$INE        PA$INE           15 2
     c                   Parm      DC$I01        PACOTI           15 2
     c                   Parm      DC$I02        PAPREC           15 2
     c                   Parm      DC$I03        PACPES           15 2
     c                   Parm      DC$I04        PACUVA           15 2
     c                   Parm                    PAGST1          270
     c                   Parm                    PANPAG            2
     c*
     c                   MoveL     PAGST1        PARESP
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* *InzSr: Inicialización del Programa
     c*--------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    PATIPF            1
     c                   Parm                    PADBSU            5 0
     c                   Parm                    PADBCT           11 0
     c                   Parm                    PARESP          360
     c                   Parm                    PANPAG            2
     c                   Parm                    PANCER           10 0
     c                   Parm                    PAIERR           40
     c*
     c     *Like         Define    IZISUC        WKISUC
     c     *Like         Define    IZINCE        WKINCE
     c     *Like         Define    IZICCL        WKICCL
     c     *Like         Define    OTITTL        WKITTL
     c*
     c*
     c                   Z-Add     PADBSU        WKISUC
     c                   Z-Add     PADBCT        WKICCL
     c                   Z-Add     1             WKITTL
     c*
     C     KIZ0600       KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICCL
     C     KIZ0000       KList
     c                   KFld                    WKISUC
     c                   KFld                    WKINCE
     c     WKEY02        KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICCL
     c     WKEY03        KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICCL
     c                   KFld                    WKITTL
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
