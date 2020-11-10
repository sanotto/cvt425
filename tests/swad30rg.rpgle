*SRCMBRTXT:Switch-Adapter      -PF-380000-Alta PF 
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
     H*  PADBCT   9 0   Cuenta PF
     H*  PAIERR   3     De Salida, código de error, ver NPC12008
     H*  PARESP 360     10 lineas de 36 para el ticket
     H*  PANPAG   2     De Entrada Salida
     H*                 Si viene un nro NN devuelve página NN+1 y NN+1
     H*                 en PANPAG.
     H*                 Si hay una sola página se devuelve 1P
     H*                 Si la página NN+1 es la ultima se devuelve LP
     H*  PASELE  28     Linea si se seleccionó una
     F*----------------------------------------------------------------
     FACCTAC    IF   E           K DISK
     FACBLOQ    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FCCBLOQ    IF   E           K DISK
     FACPGMS    IF   E           K DISK
     FCCPGMS    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FPFCERT    IF   E           K DISK
     FPFDCCL01  IF   E           K DISK
     FLILOGF01  IF   E           K DISK
     D*----------------------------------------------------------------
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
     c                   If        PATIPT='R'
     c                   ExSr      ReversaPF
     c                   Else
     c                   ExSr      AltaPF
     c                   EndIf
     c*
     c                   ExSr      EndPgm
     c*--------------------------------------------------------------------
     c* ReversaPF: Reversa Plazo Fijo
     c*--------------------------------------------------------------------
     c     ReversaPF     BegSr
     c*
     c                   Move      *Off          Found             1
     C     WKEY05        Chain     RELILOGF                           99
     C                   DoW       *In99 = *Off
     C*
     C                   If        ((LRTLNK = '181000' OR LRTLNK='182000')
     C                              AND (LRICHE = REICHE))
     c                   Move      *On           Found             1
     C                   Call      'SWAD40RG'
     C                   Parm                    REDBTC
     C                   Parm                    REDBSU
     C                   Parm                    REDBCT
     C                   Parm                    REFING
     C                   Parm                    REHING
     C                   Parm                    REICHE
     C                   Parm                    LRPFCE
     C                   Parm                    PA$IMP
     C                   Parm                    PAIERR           40
     c                   Leave
     c                   EndIf
     C*
     C     WKEY05        ReadE     RELILOGF                               99
     C                   EndDo
     c*
     c                   If        Found = *Off
     c                   Eval      PAIERR='213Mov. no encontrado para reversa'
     c                   ExSr      EndPgm
     c                   EndIf
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* AltaPF: Alta Plazo Fijo
     c*--------------------------------------------------------------------
     c     AltaPF        BegSr
     c*
     c                   Move      *Zeros        PAIERR
     c                   ExSr      CheckTipoPF
     c                   ExSr      CheckCta
     C*
     C                   ExSr      ArmaPF
     C                   ExSr      GenTicket
     C*
     c* ... Mover lineas a salida
     C*    1             Occur     PAGEDS
     C*                  Eval      PAGLIN=%ADDR(PAGEDS)
     c                   Move      *Blanks       PARESP
     C                   MoveL     PAGRES        PARESP
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* GenTicket: Genera Ticket
     c*--------------------------------------------------------------------
     c     GenTicket     BegSr
     c*
     C     WKEY02        Chain     REBAICCL                           99
|    C   99              Eval      PAIERR='304Cta.Cte. No encontrada'
|    C   99              ExSr      EndPgm
     C     WKEY03        Chain     REBADCCL                           99
|    C   99              Eval      PAIERR='304Cta.Cte. No encontrada'
|    C   99              ExSr      EndPgm
     C     WKEY04        Chain     PFCERT                             99
|    C   99              Eval      PAIERR='305Certificado No enc.   '
|    C   99              ExSr      EndPgm
     C     WKEY04        Chain     REPFDCCL                           99
|    C   99              Eval      PAIERR='305Firmantes No enc.     '
|    C   99              ExSr      EndPgm
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
     c                   Parm                    PAGRES          270
     c                   Parm                    PANPAG            2
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* CheckSaldoCta: Chequea si hay saldo
     c*--------------------------------------------------------------------
     c     CheckCta      BegSr
     C*
     C                   If        PAISUB='CC'
     c                   ExSr      ValidaCC
     C                   Else
     c                   ExSr      ValidaCA
     C                   Endif
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* ValidaCC: Valida si la Cta Cte puede permitir el movimiento
     c*--------------------------------------------------------------------
     c     ValidaCC      BegSr
     C*
     C*   .......Testea la existencia y vigencia de la cuenta
     C     WKEY01        Chain     CCCTCT                             80
+----C                   If        *In80 = *On
|    C                   Eval      PAIERR='107Cta no Existe'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*   .......Verifica si el Sub-Sistema esta Cerrado
     C                   EXSR      ChkCCSbS
     C*   .......Testea la existencia y vigencia de la cuenta
+----C                   If        BMFBAJ  <> *Zero
|    C                   Eval      PAIERR='101Cta. Cte. de baja'
|    C                   ExSr      EndPgm
+----C                   ENDIF
     c*
+----C                   If        BMISGC='IN'
|    C                   Eval      PAIERR='101Cta. Cte. de baja'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*  ........Valida bloqueo de la cuenta
     C                   EXSR      ValBloCC
+----C                   If        PAIERR <> *Zeros
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* ValidaCA: Valida si la Caja de Ahorro puede permitir el movimiento
     c*--------------------------------------------------------------------
     c     ValidaCA      BegSr
     c*
     c*
     C*   .......Testea la existencia y vigencia de la cuenta
     C     WKEY01        Chain     REACCTAC                           80
+----C     *IN80         IFEQ      *ON
|    C                   Eval      PAIERR='207La cuenta no se encuentra def.'
|    C                   EXSR      EndPgm
+----C                   ENDIF
     C*   .......Verifica si el Sub-Sistema esta Cerrado
     C                   EXSR      ChkACSbS
     C*   .......Testea la existencia y vigencia de la cuenta
+----C                   If        FUFBAJ <> *ZERO
|    C                   Eval      PAIERR='201La c.a.está dada de baja'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*
+----C                   If        FUISGC='IN'
|    C                   Eval      PAIERR='201La c.a.está dada de baja'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*
     C*   .......Compara saldo de la cuenta si tipo mov=1 (Extracciones)
|    C                   If        FU$SOP < PA$IMP
|    C                   Eval      PAIERR='206El saldo de la cta es insufic.'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*  ........Valida bloqueo operaciones de débito
     C                   ExSr      ValBloCA
+----C                   If        PAIERR <> *Zeros
|    C                   ExSr      ENDPGM
+----C                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* VAlBloCA: Valida Bloqueos en la CA.
     C*-------------------------------------------------------------------------
     C     ValBloCA      BEGSR
     C*
     C                   If        FUIBAC <> *Zero
     C*  ........Valida bloqueo operaciones de débito
|+---C     FU$SBL        Ifeq      0
||   C                   Eval      PAIERR='203La cuenta está bloq. a Deb.'
||   C                   ExSr      EndPgm
|+---C                   Else
||   C     FU$SOP        SUB       PA$IMP        WW$$SA           15 2
||+--C     WW$$SA        IfLT      FU$SBL
||   C                   Eval      PAIERR='203La cuenta está bloq. a Deb.'
|||  C                   ExSr      EndPgm
||+--C                   Endif
|+---C                   EndIf
     C*
|+---C                   EndIf
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VAlBloCC: Valida Bloqueos en la CC.
     C*-------------------------------------------------------------------------
     C     ValBloCC      BEGSR
     C*
     C                   If        BMIBCC <> *Zero
     C*  ........Valida bloqueo operaciones de débito
|+---C     BM$SBL        Ifeq      0
||   C                   Eval      PAIERR='103La cuenta está bloq. a Deb.'
||   C                   ExSr      EndPgm
|+---C                   Else
||   C     FU$SOP        SUB       PA$IMP        WW$$SA           15 2
||+--C     WW$$SA        IfLT      BM$SBL
||   C                   Eval      PAIERR='103La cuenta está bloq. a Deb.'
|||  C                   ExSr      EndPgm
||+--C                   Endif
|+---C                   EndIf
     C*
|+---C                   EndIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* ChkCCSbs: Verifica que el Sub-Sistema CC Este cerrado
     C*-------------------------------------------------------------------------
     C     ChkCCSbs      BegSr
     C*
     C                   MoveL     'CCPDDIAR'    LBIPGM
     C     LBIPGM        Chain     CCPGMS                             80
+----C                   If        *In80 = *Off
|    C                   Eval      PAIERR='108Subsis C.C. Cerrado'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* ChkACSbs: Verifica que el Sub-Sistema CA Este cerrado
     C*-------------------------------------------------------------------------
     C     ChkACSbs      BEGSR
     C*
     C                   MoveL     'ACPD01RG'    LBIPGM
     C*   .......Controla si el Subsistema de Caja no se cerro aún...
     C     LBIPGM        Chain     ACPGMS                             80
+----C                   If        *In80 = *Off
|    C                   Eval      PAIERR='208Ya fue ejecutado el cierre de CA'
|    C                   ExSr      EndPgm
+----C                   EndIf
     C*
     C                   EndSr
     c*--------------------------------------------------------------------
     c* ArmaPF: Arma Plazo Fijo
     c*--------------------------------------------------------------------
     c     ArmaPF        BegSr
     c*
     c                   Z-Add     PAPFPL        WWQDPL
     c                   Move      *Zero         PAPFAR            1
     c*
     c                   Call      'SWAD35RG'
     c                   Parm                    PAISUB            2
     c                   Parm                    PADBSU            5 0
     c                   Parm                    PADBCT            9 0
     C                   Parm                    PAFALT
     C                   Parm                    PAHALT
     C                   Parm                    PATTNA
     c                   Parm                    PA$IMP           15 2
     c                   Parm                    WWQDPL            5 0
     c                   Parm                    PAPFAR            1
     c                   Parm                    PAINCE           11 0
     c                   Parm      PAPFTI        PFTIPO            1
     C                   Parm                    PAIERR           40
     c*
     c                   If        PAIERR <> *Zeros
|    C                   ExSr      EndPgm
+----C                   EndIf
     c*
     C                   Z-Add     PAINCE        PANCER
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* CheckTipoPF: Chequea si el tipo de PF solic. es soportado p/SIDEBA
     c*--------------------------------------------------------------------
     c     CheckTipoPF   BegSr
     c*
     c* ... Solo aceptamos PF tradicionales (PAPFTI='T')
     c                   If        PAPFTI='P' or
     c                             PAPFTI='B' or
     c                             PAPFTI='V' or
     c                             PAPFTI='C'
     c*
     c                   Move      *Blanks       PARESP
     c                   Eval      PAIERR='047Tipo de PF no soportado.'
     c*
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* FillPage: Rellena la DS Multiple PAGEDS desde la BUFFDS p/pág PANPAG
     c*---------------------------------------------------------------------
     c     FillPage      BegSr
     c*
     c                   TestN                   PANPAG               99
     c  N99              Z-Add     *Zero         Page              2 0
     c   99              Move      PANPAG        Page              2 0
     c*
     c                   Add       1             Page
     c*
     c     BUFLEN        Div       10            CANPAG
     c                   MVR                     Remainder         1 0
     c     Remainder     Comp      *Zero                              9999
     c   99              Add       1             CANPAG
     c*
     c                   Select
     c                   When      CanPag = 1
     c                   Move      '1P'          PANPAG
     c                   When      Page=CanPag
     c                   Move      'LP'          PANPAG
     c                   When      Page<CanPag and CanPag > 1
     c                   Move      Page          PANPAG
     c                   EndSl
     c*
     c                   ExSr      ClearPage
     c*
     c                   Eval      Desde=((Page - 1)*10 + 1)
     c                   Eval      Hasta=Desde +  9
     c                   Z-Add     1             J
     c                   For       I= Desde to Hasta
     C     I             Occur     BUFFDS
     C     J             Occur     PAGEDS
     c                   Move      buflin        txtlin
     c                   Add       1             J
     c                   EndFor
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* ClearPage: Limpia la página de Respuesta
     c*--------------------------------------------------------------------
     c     ClearPage     BegSr
     C*
     c                   For       I= 1 to 10
     C     i             Occur     PAGEDS
     c                   Move      *Blanks       txtlin
     c                   EndFor
     C*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* *InzSr: Inicialización del Programa
     c*--------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    PATIPT            1
     C*
     c                   Parm                    PAISUB            2
     c                   Parm                    PADBSU            5 0
     c                   Parm                    PADBCT            9 0
     C                   Parm                    PAFALT            8 0
     C                   Parm                    PAHALT            6 0
     C                   Parm                    PATTNA           13 7
     c                   Parm                    PA$IMP           15 2
     c                   Parm                    PAPFPL            3 0
     c                   Parm                    PAPFFR            1
     c                   Parm                    PAPFAR            1
     c                   Parm                    PAPFTI            1
     c                   Parm                    PARESP          360
     c                   Parm                    PANPAG            2
     c*
     c*   ... Datos para reversa
     c                   Parm                    REDBTC
     c                   Parm                    REDBSU
     c                   Parm                    REDBCT
     c                   Parm                    REFING
     c                   Parm                    REHING
     c                   Parm                    REICHE
     c*
     c                   Parm                    PANCER           10 0
     c                   Parm                    PAIERR
     c*
     c     WKEY01        KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICAH
     c     WKEY02        KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICCL
     c     WKEY03        KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICCL
     c                   KFld                    WKITTL
     c     WKEY04        KList
     c                   KFld                    WKISUC
     c                   KFld                    PAINCE
     c     WKEY05        KList
     c                   KFld                    REDBTC
     c                   KFld                    REDBSU
     c                   KFld                    REDBCT
     c                   KFld                    REFING
     c                   KFld                    REHING
     c                   KFld                    REICHE
     c*
     c     *Like         Define    FUISUC        WKISUC
     c     *Like         Define    FUICAH        WKICAH
     c     *Like         Define    FUICCL        WKICCL
     c     *Like         Define    OTITTL        WKITTL
     c*
     c     *Like         Define    LRDBTC        REDBTC
     c     *Like         Define    LRDBSU        REDBSU
     c     *Like         Define    LRDBCT        REDBCT
     c     *Like         Define    LRFING        REFING
     c     *Like         Define    LRHING        REHING
     c     *Like         Define    LRICHE        REICHE
     c*
     c                   Move      PADBSU        WKISUC
     c                   Move      PADBCT        WKICAH
     c                   Move      PADBCT        WKICCL
     c                   Z-Add     1             WKITTL
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
