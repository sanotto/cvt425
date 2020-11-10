*SRCMBRTXT:Aut.Federal-Manejador de Alta de Operac
     H DEBUG
     H DECEDIT(',') DATEDIT(*DMY/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: - Lee PRAFED Por Creditos Aceptados en la Fecha*
     H*                  - Copia Movs de PRTMOD60 a PRTMOD P/Cred. Alt*
     H*                  - Marca JVIESA=1 Activa                      *
     H*                    - Genera Minuta                            *
     H*                - Salva Minuta en PRMFED                       *
     H*                - Salva Movs Diarios de Prestamos en PRTMOD60  *
     H*                                                               *
     H*  PROGRAM NO: PRFD04MA                                         *
     H*                                                               *
     H*  DATE:    22/06/2012                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*****************************************************************
     FPRAFED01  UF   E           K DISK
     FPRAFED    UF   E           K DISK    Rename(REPRAFED:R1)
     FPRCRED    UF   E           K DISK
     FPRMFED    IF   E           K DISK
     FPRMOVI60  IF   E           K DISK
     FACCTAC    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FBASUCU    IF   E           K DISK
     FTCPADR05  IF   E           K DISK
     FACMOVD11  IF   E           K DISK
     FACMOVB08  UF   E           K DISK
     FPRMOVI01  UF A E           K DISK
     FPRMOVI09  IF   E           K DISK    Rename(REPRMOVI:R9)
     FBASCOR01  UF   E           K DISK
     FBASCOR04  UF   E           K DISK    Rename(REBASCOR:R4)
     F@CPIUSD   UF   E           K DISK
     F@CPISYS   UF   E           K DISK
     FSGSYSV    IF   E             DISK
     FSEGS1L01  IF   E           K DISK
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     DERRDS            DS
     D ERRTXT                  1    385
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*---------------------------------------------------------------------
     D Cmd             S           4096
     D CmdLen          S             15  5 INZ(4096)
     D*---------------------------------------------------------------------
     c     *ENTRY        Plist
     c                   Parm                    METODO            6
     c                   Parm                    WWIRRN           15 0
     C*
     c                   If        METODO = 'ONLINE'
     c     WWIRRN        Chain     R1                                 99
     C                   Else
     c     KAF011        Chain     REPRAFED                           99
     c                   EndIf
     C*
+----C                   DoW       *IN99=*Off
|    c* ... Proceso de Aprobadas
|+---c                   If        AFESTA = 'A' and AFFBAJ = 0 AND afiopt='A'
||   C                   ExSr      AltaCredito
||   C                   ExSr      ProcMinuta
||   c                   ExSr      MarcarComoProc
||   c                   ExSr      CorrerBolsa
||   c                   GoTo      NextRecord
|+---c                   EndIf
|    c* ... Proceso de Rechazadas
|+---c                   If        AFESTA = 'R' and AFFBAJ = 0 AND afiopt='B'
||   c                   ExSr      ActScoring
|+---C                   EndIf
|+---c                   If        AFESTA = 'R' and AFFBAJ = 0 AND afiopt='A'
||   c                   ExSr      EliminaCred
||   c                   ExSr      MarcarComoProc
|+---C                   EndIf
|    c*
|    c     NextRecord    Tag
|+-- c                   If        METODO = 'ONLINE'
||   c     WWIRRN        ReadE     R1                                     99
|>-- C                   Else
||   c     KAF011        ReadE     REPRAFED                               99
|+-- c                   EndIf
+----C                   EndDo
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* CorrerBolsa: Corre la bolsa para cobrar deudas que tuviera pendiente
     c*---------------------------------------------------------------------
     c     CorrerBolsa   BegSr
     c*
     c                   Z-Add     JVISUC        WWISUC
     c                   Z-Add     JVICCL        WWICAH           11 0
     c                   Z-Add     AASFEI        WWFACR            8 0
     c*
     c                   Call      'PRFD04R5'
     C                   Parm                    WWISUC
     C                   Parm                    WWICAH
     C                   Parm                    WWFACR
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* ActScoring: Vuelve a Marcar Scoring con la operación original
     c*---------------------------------------------------------------------
     c     ActScoring    BegSr
     C*
     C     KSC040        Chain     R4                                 99
+----C                   If        *In99 = *Off  AND  AFINCV > 0
|    c                   Z-Add     AFISUC        SCISUC
|    c                   Z-Add     AFINCR        SCINCR
|    c                   Z-Add     AFIDEG        SCIDEG
|    C*
|    C                   Update    R4
|    C*
+----C                   EndIf
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* EliminaCred: Elimina Operación de PRCRED si fue rechazada
     c*---------------------------------------------------------------------
     c     EliminaCred   BegSr
     C*
     c                   Move      AFISUC        WWISUC
     c                   Move      AFINCR        WWINCR
     c                   Move      AFIDEG        WWIDEG
     C     KJV010        Chain     REPRCRED                           99
     c  N99              Delete    REPRCRED
     C*
     C     @PJOBN        Chain     @CPIUSD                            99
     C                   Z-Add     AFISUC        @CISUC
     C                   Z-Add     AFINCR        @CINCR
     C                   Z-Add     AFIDEG        @CIDEG
     C                   Update    @CPIUSRR
     c                   Call      'PRJV03SB'
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* AltaCredito: Alta Credito Aprobado
     c*---------------------------------------------------------------------
     c     AltaCredito   BegSr
     c* ... Actualizar PRMOVI
     c                   Move      AFISUC        WWISUC
     c                   Move      AFINCR        WWINCR
     c                   Move      AFIDEG        WWIDEG
     c                   ExSr      ActPRMOVI
     c* ... Actualizar PRMOVI
     C* ... Act. tipo de Cobro a 1
     C     KJV010        Chain     REPRCRED                           99
+----c                   If        *IN99 = *Off
|    c                   Z-Add     1             JVICOB
|+---c                   If        JVIECT = *BLANKS
||   C                   Move      JVISUC        JVIECT
|+---C                   EndIf
|    c                   Move      JVIECT        WWIECT            1 0
|    c                   Z-Add     WWIECT        WWISAL
|    c                   Update    REPRCRED
+----c                   Else
|    c                   LeaveSr
+----C                   EndIf
     c* ... Activación de la operación
     C     @PJOBN        Chain     @CPIUSRR                           99
     C                   Z-Add     AFISUC        @CISUC
     C                   Z-Add     AFINCR        @CINCR
     C                   Z-Add     AFIDEG        @CIDEG
     C                   Z-Add     WW$INE        @C$INE
     C                   Z-Add     *ZERO         @CICUO
     C                   Update    @CPIUSRR
     C                   Move      *BLANK        WWERRO            1
     C                   Move      'R'           W2FLAG            1
     c* ... Poner Usuario de Alta para contabilizar
     C     @PJOBN        CHAIN     @CPISYS                            99
     C                   MOVE      @ZUSER        AUUSER           10
+----C                   IF        AFDF03 <> *BLANKS
|    C                   MOVEL     AFDF03        @ZUSER
+----C                   ENDIF
     C                   UPDATE    @CPISYSR
     c*
     C                   Call      'SBPRMOVI'
     C                   Parm                    W2FLAG
     C                   Parm                    WWERRO
     c* ... Poner Usuario en CPISYS original
     C     @PJOBN        CHAIN     @CPISYS                            99
     C                   MOVEL     AUUSER        @ZUSER
     C                   UPDATE    @CPISYSR
     c* ... Poner el dinero en la caja de ahorro del cliente
     c                   Z-Add     210           WWIMCA            3 0
     c                   Z-Add     WW$INE        WW$IMP           15 2
     c                   Z-Add     JVICCL        WWICCL
     c                   ExSr      ImpactarCAH
     c*
     c* ... Marcar en BASCOR, excepto p/Linea 1 (PC P/TODOS)
+----c                   If        JVILCR <> 1
|    c     KSC010        Chain     REBASCOR                           99
|+---C                   DoW       *In99 = *Off
||+--c                   If            AFIBCF=SCIBCF
|||  c                             AND AFIRED=SCIRED
|||  c                   Z-Add     AFISUC        SCISUC
|||  c                   Z-Add     AFINCR        SCINCR
|||  c                   Z-Add     AFIDEG        SCIDEG
|||  c                   Update    REBASCOR
|||  C                   Leave
||+--C                   EndIf
||   c     KSC010        ReadE     REBASCOR                               99
|+---C                   EndDo
+----C                   EndIf
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* MarcarComoProc: Marcar Registro como ya procesado
     c*---------------------------------------------------------------------
     c     MarcarComoProcBegSr
     C*
     c                   Z-Add     AASFEI        AFFBAJ
     c                   Time                    AFHBAJ
     C                   Move      @PUSER        AFIUSB
 +-- c                   If        METODO = 'ONLINE'
 |   c                   Update    R1
 >-- C                   Else
 |   c                   Update    REPRAFED
 +-- c                   EndIf
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ProcMinuta : Procesar Minuta de Prestamo
     c*---------------------------------------------------------------------
     c     ProcMinuta    BegSr
     c*
     C     KMC010        Chain     REPRMFED                           99
+----c                   DoW       *IN99  = *Off
|+---c                   Select
||   C*-----> PC Para Todos
|+---c                   When      MCIAPC=  1 and MCITRN=439
||   c                   ExSr      Ap01Trn439
|+---c                   When      MCIAPC=  2 and MCITRN=439
||   c                   ExSr      Ap02Trn439
||   C*-----> Finaciaciones con fines determinados
|+---c** NO VA MAS.. **  When      MCIAPC=  1 and MCITRN=210
     c** SE CAMBIAN  **            and JVILCR=16
||   c** A 339 - 439 **  ExSr      Ap01Trn210
||   C*----->
|+---c                   When      MCIAPC=  2 and MCITRN=339
||   c                   ExSr      Ap02Trn339
||   C*-----> Canc. de Prestamos o Cuotas Atrasadas
||   C*                  'Debitar Importe Canc. Prestamos o Cuota Prestamos
|+---c                   When      MCIAPC=  2 and MCITRN=20
||   c                   ExSr      Ap02Trn020
||   C*                  'Cancelacion de Prestamos
|+---c                   When      MCIAPC= 20 and MCITRN=4
||   c                   ExSr      Ap20Trn004
||   C*                  'Cancelacion de Cuotas
|+---c                   When      MCIAPC= 20 and MCITRN=2
||   c                   ExSr      Ap20Trn002
||   C*-----> Apl. 02 Trn 392 -> Saca Dinero de la Cta P/Apl. al Pago de Tarjeta
|+---c                   When      MCIAPC= 2 and MCITRN=392
||   c                   ExSr      Ap02Trn392
||   C*-----> Apl. 02 Trn 491 -> Saca Dinero de la Cta P/Apl. al Pago de Tarjeta
|+---c                   When      MCIAPC= 2 and MCITRN=491
||   c                   ExSr      Ap02Trn491
||   C* ... Reemplazado por Códs 491, 147 y 77 por que este cod. de caja generab
||   c*     diferencias contables, los cód anteriores se insertan en las rutinas
||   c*     Ap08Trn066 y Ap03Trn06x
||   c*                  ExSr      Ap02Trn392
||   C*                  'Cobro Resumén Tarjeta Sol
|+---c                   When      MCIAPC= 8 and MCITRN=066
||   c                   ExSr      Ap08Trn066
||   C*                  'Cobro Resumén Carta Franca
|+---c                   When      MCIAPC= 3 and MCITRN=60
||   c                   Z-Add     60            WWITRN            3 0
||   c                   ExSr      Ap03Trn06x
||   C*                  'Cobro resumen Arg./Mast./Gold
|+---c                   When      MCIAPC= 3 and MCITRN=61
||   c                   Z-Add     61            WWITRN            3 0
||   c                   ExSr      Ap03Trn06x
||   C*                  'Cobro resumen Visa
|+---c                   When      MCIAPC= 3 and MCITRN=62
||   c                   Z-Add     62            WWITRN            3 0
||   c                   ExSr      Ap03Trn06x
|+---C                   EndSl
|    C     KMC010        ReadE     REPRMFED                               99
+----C                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap01Trn439: PC Para todos
     c*---------------------------------------------------------------------
     c     Ap01Trn439    BegSr
     c*
     c                   Z-Add     439           WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactarCC
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap01Trn210: Fines determinados
     c*---------------------------------------------------------------------
     c     Ap01Trn210    BegSr
     c*
     C*                  Z-ADD     MCISUC        WWISUC            5 0
     C                   Z-ADD     MCEDSU        WWISUC            5 0
     C                   Z-ADD     MCICCL        WWICCC           11 0
     c     KCCCTCT       Chain     RECCCTCT                           99
+----c                   If        *In99 = *On
|    C                   LeaveSr
+----C                   EndIf
     c     MCISUC        Chain     REBASUCU                           99
+----c                   If        *In99 = *On
|    C                   LeaveSr
+----C                   EndIf
     C*
     C*                  Z-ADD     MCISUC        PAISUC            5 0
     C                   Z-ADD     MCICCL        PAICCC           11 0
     C                   MOVE      AASFEI        PAFING            8 0
     C                   TIME                    PAHALT            6 0
     C                   Z-ADD     BMIMON        PAIMON            9 0
     C                   Z-ADD     BMITCU        PAITCU            2 0
     C                   Z-ADD     210           PAIMCC            3 0
     C*                  Z-ADD     MCEDSU        PAISAL            5 0
     C                   Z-ADD     MCEDSU        PAISUC            5 0
     C                   Z-ADD     WWISAL        PAISAL            5 0
     C                   Z-ADD     556           PAICAJ            5 0
     C                   Z-ADD     MC$IMP        PA$IMP           15 2
     C                   Z-ADD     AFICHE        PAICHE            7 0
     C                   Z-ADD     AASFEI        PAFASI            8 0
     C                   Z-ADD     AASFEN        WXFASI            8 0
     C                   Z-ADD     *ZERO         PAIASC            1 0
     C                   Z-ADD     *ZERO         PAFASC            8 0
     C                   Z-ADD     BMISCT        PAISCT            4 0
     C                   Z-ADD     AVIRES        PAIRES            1 0
     C                   MOVE      'S'           PAIGAS            1
     C                   MOVE      @PUSER        PAIUSP           10
     C                   MOVE      *BLANKS       PAIUSA           10
     C                   MOVE      *BLANKS       PACERT            1
     C                   MOVE      *BLANKS       PAMAUT            1
     C*
     c                   Call      'SBCCMOVI'
     C                   PARM                    PAISUC
     C                   PARM                    PAICCC
     C                   PARM                    PAFING
     C                   PARM                    PAHALT
     C                   PARM                    PAIMON
     C                   PARM                    PAITCU
     C                   PARM                    PAIMCC
     C                   PARM                    PAISAL
     C                   PARM                    PAICAJ
     C                   PARM                    PA$IMP
     C                   PARM                    PAICHE
     C                   PARM                    WXFASI
     C                   PARM                    PAIASC
     C                   PARM                    PAFASC
     C                   PARM                    PAISCT
     C                   PARM                    PAIRES
     C                   PARM                    PAIGAS
     C                   PARM                    PAIUSP           10
     C                   PARM                    PAIUSA           10
     C                   PARM                    PACERT            1
     C                   PARM                    PAMAUT            1
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap02Trn339: PC Para todos
     c*---------------------------------------------------------------------
     c     Ap02Trn339    BegSr
     c*
     c                   Z-Add     339           WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactarCAH
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap02Trn439: PC Para todos
     c*---------------------------------------------------------------------
     c     Ap02Trn439    BegSr
     c*
     c                   Z-Add     439           WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactCAHCom
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap02Trn020: Replicar accionar de transaccion 02-020 de Caja
     c*---------------------------------------------------------------------
     c     Ap02Trn020    BegSr
     c*
     c                   Z-Add     20            WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactarCAH
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap20Trn002: Replicar accionar de transaccion 20-002 de Caja
     c*---------------------------------------------------------------------
     c     Ap20Trn002    BegSr
     C*
     c*
+----c                   If        MCINCV = *ZERO
|    c                   LeaveSr
+----c                   EndIf
     c*
     c* ... Actualizar PRMOVI
     c                   Z-Add     MCEDSU        WWISUC
     c                   Z-Add     MCINCV        WWINCR
     c                   Z-Add     MCIDEV        WWIDEG
     c                   Z-Add     MCICUO        WWICUO
     c                   ExSr      SumPrmoviCta
+----c                   If        MC$IMP <> WW$INE
|    C                   ExSr      SumPrmovi60Cta
|+---c                   If        MC$IMP =  WW$INE
||   c                   ExSr      ActPRMOVICta
|+---c                   else
||   c                   ExSr      InfDifXMail
|+---c                   EndIf
+----c                   EndIf
     c*
     C     @PJOBN        Chain     @CPIUSD                            99
     C                   Z-Add     MCEDSU        @CISUC
     C                   Z-Add     MCINCV        @CINCR
     C                   Z-Add     MCIDEV        @CIDEG
     C                   Z-Add     MCICUO        @CICUO
     C                   Z-Add     MC$IMP        @C$INE
     C                   Update    @CPIUSRR
     c*
     c* ... Poner Usuario de Alta para contabilizar
     C     @PJOBN        CHAIN     @CPISYS                            99
     C                   MOVE      @ZUSER        AUUSER           10
+----C                   IF        AFDF03 <> *BLANKS
|    C                   MOVEL     AFDF03        @ZUSER
+----C                   ENDIF
     C                   UPDATE    @CPISYSR
     c*
     c* ... Actualiza Tipo de Cobro
     C     KJV010        Chain     REPRCRED                           99
+----c                   If        *IN99 = *Off
|    c                   Z-Add     1             JVICOB
|    c                   Update    REPRCRED
+----C                   EndIf
     c*
     C                   Move      *BLANK        WWERRO            1
     C                   Move      'C'           W2FLAG            1
     C                   Call      'SBPRMOVI'
     C                   Parm                    W2FLAG
     C                   Parm                    WWERRO
     c* ... Poner Usuario Original en CPISYS
     C     @PJOBN        CHAIN     @CPISYS                            99
     C                   MOVEL     AUUSER        @ZUSER
     C                   UPDATE    @CPISYSR
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* InfDifXMail: Informa diferencias por Mail
     c*---------------------------------------------------------------------
     c     InfDifXMail   BegSr
     c*
      /free
        eval Cmd='SNDMAIL RECP(PRHA04CL)                           '+
                 'SUBJECT(''Diferencia en Ap:20 Trn:002'')         '+
                 'MESG(''Suc:'+%EditW(WWISUC:'     ')               +
                 '-Op:' + %EditW (WWINCR:'               ')         +
                 '-Des:'+ %EditW (WWIDEG:'    ')                    +
                 '-Cta:'+ %EditW (WWICUO:'   ')                     +
                 '-Imp Cupon :'+ %EditW (WW$INE:'            ,   ') +
                 '-Imp Minuta:'+ %EditW (MC$IMP:'            ,   ') +
                 ''')                                              ';
      /end-free
     c*
     c                   Call      'QCMDEXC'
     c                   Parm                    Cmd
     c                   Parm                    CmdLen
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap20Trn004: Replicar accionar de transaccion 20-004 de Caja
     c*---------------------------------------------------------------------
     c     Ap20Trn004    BegSr
     c*
+----c                   If        MCINCV = *ZERO
|    c                   LeaveSr
+----c                   EndIf
     c*
     c                   Move      MCEDSU        WWISUC
     c                   Move      MCINCV        WWINCR
     c                   Move      MCIDEV        WWIDEG
     c                   ExSr      SumPrmovi
+----c                   If        MC$IMP <> WW$INE
|    C                   ExSr      SumPrmovi60
|+---c                   If        MC$IMP =  WW$INE
||   c                   Move      MCEDSU        WWISUC
||   c                   Move      MCINCV        WWINCR
||   c                   Move      MCIDEV        WWIDEG
||   c                   ExSr      ActPRMOVI
|+---c                   Else
||   c* ... Genera Cupones de Cancelación
||   c                   Call      'PRFD04R4'
||   c                   Parm                    MCEDSU
||   c                   Parm                    MCINCV
||   c                   Parm                    MCIDEV
|+---C                   EndIf
+----C                   EndIf
     c* ... Actualiza Tipo de Cobro
     C     KJV010        Chain     REPRCRED                           99
+----c                   If        *IN99 = *Off
|    c                   Z-Add     1             JVICOB
|    c                   Update    REPRCRED
+----C                   EndIf
     c* ... Llama a Host de Cancelación Total
     C                   Z-ADD     *ZERO         xx$INE           15 2
     C*
     C                   Call      'PRFD04R3'
     c                   Parm                    WWISAL
     C                   Parm                    MCEDSU
     C                   Parm                    MCINCV
     C                   Parm                    MCIDEV
     C                   Parm                    xx$INE
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap02Trn392: Replicar accionar de transaccion 2-392 de Caja
     c*---------------------------------------------------------------------
     c     Ap02Trn392    BegSr
     c*
     c                   Z-Add     392           WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactarCAH
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap02Trn491: Replicar accionar de transaccion 2-491 de Caja
     c*---------------------------------------------------------------------
     c     Ap02Trn491    BegSr
     c*
     c                   Z-Add     491           WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactarCAH
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap08Trn066: Replicar accionar de transaccion 08-066 de Caja
     c*---------------------------------------------------------------------
     c     Ap08Trn066    BegSr
     c* --> Si ya existe el 491 en el ACMOVD, saltear este paso
     c                   Z-Add     JVICCL        WWICAH           11 0
     c                   Z-Add     491           WWIMCA
     c                   Z-Add     MC$IMP        WW$IMP
     c*
     c     KGD110        Chain     REACMOVD                           99
     c*
     c* --> Sacar el dinero de la caja de ahorro con cód. 491
+----c                   If        *In99 = *On
|    c                   ExSr      ImpactarCAH
|    c* --> aqui llamar al equivalente del setsol780rg para la 8-66
|    c*                  Call      'PRFD04R1'
|    C*                  Parm                    MCISUC
|    C*                  Parm                    MCINCR
|    C*                  Parm                    MCIDEG
|    C*                  Parm                    MC$IMP
|    C*                  Parm                    AFINDO
+----c                   EndIf
     c
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Ap03Trn06X: Replicar accionar de transaccion 03-06x de Caja
     c*---------------------------------------------------------------------
     c     Ap03Trn06X    BegSr
     c*
     c* --> Determinar código a utilizar para extraer el dinero
     c                   Z-Add     147           WWIMCA
+----c                   If        WWITRN = 61
|    c                   Z-Add     77            WWIMCA
+----C                   Endif
     c*
     C                   Z-Add     MCICCL        WWINCT
     c     WWINCT        Chain     RETCPADR                           99
+----c                   If        *in99 = *Off and
|    c                             (TJIGRC='06' or TJIGRC='07')
|    c                   Z-Add     491           WWIMCA
+----c                   EndIf
     c* --> Si el cód es 491
+----c                   If        WWIMCA=491
|    c*        --> Si ya existe el 491 en el ACMOVD, saltear este paso
|    c                   Z-Add     JVICCL        WWICAH           11 0
|    c                   Z-Add     491           WWIMCA
|    c                   Z-Add     MC$IMP        WW$IMP
|    c*
|    c     KGD110        KList
|    c                   KFld                    JVISUC
|    c                   KFld                    WWICAH
|    c                   KFld                    WWIMCA
|    c                   KFld                    WW$IMP
|    c     KGD110        Chain     REACMOVD                           99
|    c*
|    c* --> Sacar el dinero de la caja de ahorro con cód. 491
|    c  n99              LeaveSr
+----c                   EndIf
     c* --> Sacar el dinero de la caja de ahorro con el cód. que correspond
     c                   Z-Add     MC$IMP        WW$IMP
     c                   ExSr      ImpactarCAH
     c* --> aqui llamar al equivalente del TCMELO sii wwimca <> 491
     c                   If        WWIMCA <> 491
     c                   Call      'PRFD04R2'
     C                   Parm                    WWITRN
     C                   Parm                    MCISUC
     C                   Parm                    MCICCL
     C                   Parm                    MC$IMP
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* *InzSr: Inicializacion
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     c*
     c     *Like         Define    MIISUC        WWISUC
     c     *Like         Define    MIISUC        WWISAL
     c     *Like         Define    MIISUC        WXISAL
     c     *Like         Define    MIINCR        WWINCR
     c     *Like         Define    MIIDEG        WWIDEG
     c     *Like         Define    MIICUO        WWICUO
     c     *Like         Define    JVICCL        WWICCL
     c     *Like         Define    TJINCT        WWINCT
     c*
     c     KAF011        KList
     c                   KFld                    AASFEI
     c     KMC010        KList
     c                   KFld                    AASFEI
     c                   KFld                    AFISUC
     c                   KFld                    AFINCR
     c                   KFld                    AFIDEG
     c     KMI013        KList
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c     KMI014        KList
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c                   KFld                    WWICUO
     c     KM2010        KList
     c                   KFld                    AASFEI
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c     KM2011        KList
     c                   KFld                    AASFEI
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c     KMI094        KList
     c                   KFld                    AASFEI
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c     KMI095        KList
     c                   KFld                    AASFEI
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c                   KFld                    WWICUO
     c     KJV010        KList
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     C*
     c     KSC010        KList
     C                   KFld                    AFINDO
     c     KSC040        KList
     C                   KFld                    AFEDSU
     C                   KFld                    AFINCV
     C                   KFld                    AFIDEV
     C*
     c     KACCTAC       KList
     C                   KFld                    JVISUC
     C                   KFld                    WWICAH
     c     KACCOME       KList
     C                   KFld                    MCEDSU
     C                   KFld                    WWICAH
     c     KCCCTCT       KList
     C                   KFld                    WWISUC
     C                   KFld                    WWICCC
     C*
     c*
     c     1             Chain     RESGSYSV
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ImpactarCAH: Impactar Caja de Ahorro
     c*---------------------------------------------------------------------
     c     ImpactarCAH   BegSr
     c*
     c                   Z-Add     *ZERO         WXISAL
     c                   Z-Add     WWICCL        WWICAH           11 0
     c     KACCTAC       Chain     REACCTAC                           99
+----c                   If        *In99 = *On
|    C                   LeaveSr
+----C                   EndIf
     C* ... Caja 556 de Manera que durante el proceso no cobre cód. asociados
     c                   Z-Add     556           WWICAJ            5 0
     c                   Z-Add     *ZERO         WWIASC            1 0
     C*
     c                   Time                    WWHORA            6 0
     C*
     c                   Z-Add     AFICHE        WWICHE            7 0
+----c                   If        WWIMCA = 491
     c                   Z-Add     WWISAL        WXISAL
     c                   Z-Add     *ZERO         WWISAL
|    c                   Z-Add     MCICCL        WWICHE
|    c     WWICHE        CHAIN     SEGS1L01                           98
|+---c                   If        *In98 = *Off
||   C     7             SUBST     S1IHOS:2      WWIHOS            7
||   C                   MOVE      WWIHOS        WWIHO1            7 0
||   C                   Z-ADD     WWIHO1        WWICHE
|+---C                   EndIf
+----C                   EndIf
     C*
     C                   Z-ADD     *ZERO         WWFASC            8 0
     c                   Z-Add     *ZERO         WWIASC            1 0
     C*
     c                   Call      'SBACMOVI'
     C                   Parm                    JVISUC
     C                   Parm                    WWICAH
     C                   Parm                    AASFEI
     C                   Parm                    WWHORA
     C                   Parm                    JVIMON
     C                   Parm                    WWIMCA
     C                   Parm                    WWISAL
     C                   Parm                    WWICAJ
     C                   Parm                    WW$IMP
     C                   Parm                    AASFEI
     C                   Parm                    WWIASC
     C                   Parm                    WWFASC
     C                   Parm                    @PUSER
     C                   Parm                    @PUSER
     C                   Parm                    WWICHE
     c
     c                   Z-Add     WXISAL        WWISAL
     c                   Z-Add     *ZERO         WXISAL
     C*
+----c                   If        WWIMCA = 491
|    C     @KEY01        KLIST
|    C                   KFLD                    WWISUC            5 0
|    C                   KFLD                    WWICAH           11 0
|    C                   KFLD                    WWIMCA            3 0
|||| C                   Z-ADD     *ZEROS        WWBAND            1 0
|    C     @KEY01        CHAIN     ACMOVB08                           51
|+---C     *IN51         DOWEQ     *OFF
||+--C     INFACR        IFLT      30000000
|||  C     IN$IMP        SUB       WW$IMP        WX$IMP           15 2
|||+-C     WX$IMP        IFLE      *ZEROS
|||| C                   DELETE    REACMOVB
|||| C                   Z-ADD     1             WWBAND
|||+-C                   ELSE
|||| C                   Z-ADD     WX$IMP        IN$IMP
|||| C                   UPDATE    REACMOVB
|||| C                   Z-ADD     1             WWBAND
|||+-C                   END
||+--C                   END
||   C     @KEY01        READE     ACMOVB08                               51
|+---C                   ENDDO
+----C                   ENDIF
||+--C     WWBAND        IFEQ      *ZEROS
||+--C                   CALL      'PRFD04R6'
||+--C                   PARM                    AFINDO
||+--C                   PARM                    WW$IMP
||+--C                   ENDIF
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ImpactCAHCom: Impactar Caja de Ahorro del Comercio
     c*---------------------------------------------------------------------
     c     ImpactCAHCom  BegSr
     c*
     c                   Z-Add     *ZERO         WXISAL
     c                   Z-Add     MCICCL        WWICAH           11 0
     c     KACCOME       Chain     REACCTAC                           99
+----c                   If        *In99 = *On
|    C                   LeaveSr
+----C                   EndIf
     C* ... Caja 556 de Manera que durante el proceso no cobre cód. asociados
     c                   Z-Add     556           WWICAJ            5 0
     c                   Z-Add     *ZERO         WWIASC            1 0
     C*
     c                   Time                    WWHORA            6 0
     C*
     c                   Z-Add     AFINCR        WWICHE            7 0
     C*
     C                   Z-ADD     *ZERO         WWFASC            8 0
     c                   Z-Add     *ZERO         WWIASC            1 0
     C*
     c                   Call      'SBACMOVI'
     C                   Parm                    MCEDSU
     C                   Parm                    WWICAH
     C                   Parm                    AASFEI
     C                   Parm                    WWHORA
     C                   Parm                    JVIMON
     C                   Parm                    WWIMCA
     C                   Parm                    WWISAL
     C                   Parm                    WWICAJ
     C                   Parm                    WW$IMP
     C                   Parm                    AASFEI
     C                   Parm                    WWIASC
     C                   Parm                    WWFASC
     C                   Parm                    @PUSER
     C                   Parm                    @PUSER
     C                   Parm                    WWICHE
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ImpactarCC : Impactar Cuenta Corriente
     c*---------------------------------------------------------------------
     c     ImpactarCC    BegSr
     C*
     c                   Z-Add     WWIMCA        PAIMCC            3 0
     c*                  Z-Add     MCISUC        PAISUC            5 0
     c                   Z-Add     MCEDSU        PAISUC            5 0
     c                   Z-Add     MCICCL        PAICCC           11 0
     c                   Z-Add     AFINCR        PAICHE            7 0
     c                   Z-Add     WW$IMP        PA$IMP           15 2
     c                   Move      *Blanks       PAIERR           40
     c                   Z-Add     AASFEI        PAFASI            8 0
     C*
     c                   Call      'PRHA04R2'
     C                   Parm                    PAIMCC
     C                   Parm                    PAISUC
     C                   Parm                    PAICCC
     C                   Parm                    PAICHE
     C                   Parm                    PA$IMP
     C                   Parm                    PAIERR
     C                   Parm                    PAFASI
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ActPRMOVI: Actualiza PRMOVI desde PRTMOD60
     c*---------------------------------------------------------------------
     c     ActPRMOVI     BegSr
     C*
     c     KM2010        Chain     REPRMO60                           99
+----C                   If        *IN99 = *Off
|    c*
|    c     KMI013        Chain     REPRMOVI                           99
|+---C                   DoW       *IN99 = *Off
||   c                   Delete    REPRMOVI
||   c     KMI013        ReadE     REPRMOVI                               99
|+---c                   EndDo
|    C*
|    c     KM2010        Chain     REPRMO60                           99
|+---C                   DoW       *IN99 = *Off
||   C                   Z-ADD     M2ISUC        MIISUC
||   C                   Z-ADD     M2INCR        MIINCR
||   C                   Z-ADD     M2IDEG        MIIDEG
||   C                   Z-ADD     M2ICUO        MIICUO
||   C                   Z-ADD     M2IPOS        MIIPOS
||   C                   Z-ADD     M2IMPR        MIIMPR
||   C                   Z-ADD     M2FASI        MIFASI
||   C                   Z-ADD     M2$IMP        MI$IMP
||   C                   Z-ADD     M2IMON        MIIMON
||   C                   Move      M2IASK        MIIASK
||   C                   Write     REPRMOVI
||   c     KM2010        ReadE     REPRMO60                               99
|+---c                   EndDo
+----C                   EndIf
     C*
     c                   Z-Add     *ZERO         WW$INE           15 2
     c     KMI013        Chain     REPRMOVI                           99
+----C                   DoW       *IN99 = *Off
|+---C                   Select
|+---C                   When      MIIASK='1'
||   C                   Add       MI$IMP        WW$INE
|+---C                   When      MIIASK='2'
||   C                   Sub       MI$IMP        WW$INE
|+---C                   ENDSL
|    c     KMI013        ReadE     REPRMOVI                               99
+----c                   EndDo
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ActPRMOVICta: Actualiza PRMOVI desde PRTMOD60  a nivel de cuota
     c*---------------------------------------------------------------------
     c     ActPRMOVICta  BegSr
     C*
     c     KM2011        Chain     REPRMO60                           99
+----C                   If        *IN99 = *Off
|    c*
|    c     KMI014        Chain     REPRMOVI                           99
|+---C                   DoW       *IN99 = *Off
||+--c                   If        M2ICUO = WWICUO
|||  c                   Delete    REPRMOVI
||+--c                   EndIf
||   c     KMI014        ReadE     REPRMOVI                               99
|+---c                   EndDo
|    C*
|    c     KM2011        Chain     REPRMO60                           99
|+---C                   DoW       *IN99 = *Off
||+--c                   If        M2ICUO = WWICUO
|||  C                   Z-ADD     M2ISUC        MIISUC
|||  C                   Z-ADD     M2INCR        MIINCR
|||  C                   Z-ADD     M2IDEG        MIIDEG
|||  C                   Z-ADD     M2ICUO        MIICUO
|||  C                   Z-ADD     M2IPOS        MIIPOS
|||  C                   Z-ADD     M2IMPR        MIIMPR
|||  C                   Z-ADD     M2FASI        MIFASI
|||  C                   Z-ADD     M2$IMP        MI$IMP
|||  C                   Z-ADD     M2IMON        MIIMON
|||  C                   Move      M2IASK        MIIASK
|||  C                   Write     REPRMOVI
||+--c                   EndIf
||   c     KM2011        ReadE     REPRMO60                               99
|+---c                   EndDo
+----C                   EndIf
     C*
     c                   Z-Add     *ZERO         WW$INE           15 2
     c     KMI013        Chain     REPRMOVI                           99
+----C                   DoW       *IN99 = *Off
|+---C                   Select
|+---C                   When      MIIASK='1'
||   C                   Add       MI$IMP        WW$INE
|+---C                   When      MIIASK='2'
||   C                   Sub       MI$IMP        WW$INE
|+---C                   ENDSL
|    c     KMI013        ReadE     REPRMOVI                               99
+----c                   EndDo
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* SumPrmovi: Sumariza PRMOVI
     c*---------------------------------------------------------------------
     c     SumPrmovi     BegSr
     c*
     c                   Z-Add     *ZERO         WW$INE           15 2
     c     KMI094        Chain     R9                                 99
+----C                   DoW       *IN99 = *Off
|+---C                   Select
|+---C                   When      MIIASK='1'
||   C                   Add       MI$IMP        WW$INE
|+---C                   When      MIIASK='2'
||   C                   Sub       MI$IMP        WW$INE
|+---C                   ENDSL
|    c     KMI094        ReadE     R9                                     99
+----c                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* SumPrmoviCta: Sumariza PRMOVI para Cuota
     c*---------------------------------------------------------------------
     c     SumPrmoviCta  BegSr
     c*
     c                   Z-Add     *ZERO         WW$INE           15 2
     c     KMI095        Chain     R9                                 99
+----C                   DoW       *IN99 = *Off
|+---C                   Select
|+---C                   When      MIIASK='1'
||   C                   Add       MI$IMP        WW$INE
|+---C                   When      MIIASK='2'
||   C                   Sub       MI$IMP        WW$INE
|+---C                   ENDSL
|    c     KMI095        ReadE     R9                                     99
+----c                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* SumPrmovi60: Sumariza PRMOVI60
     c*---------------------------------------------------------------------
     c     SumPrmovi60   BegSr
     c*
     c                   Z-Add     *ZERO         WW$INE           15 2
     c     KM2010        Chain     REPRMO60                           99
+----C                   DoW       *IN99 = *Off
|+---C                   Select
|+---C                   When      M2IASK='1'
||   C                   Add       M2$IMP        WW$INE
|+---C                   When      M2IASK='2'
||   C                   Sub       M2$IMP        WW$INE
|+---C                   ENDSL
|    c     KM2010        ReadE     REPRMO60                               99
+----c                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* SumPrmovi60Cta: Sumariza PRMOVI60 a nivel de cuota
     c*---------------------------------------------------------------------
     c     SumPrmovi60CtaBegSr
     c*
     c                   Z-Add     *ZERO         WW$INE           15 2
     c     KM2011        Chain     REPRMO60                           99
+----C                   DoW       *IN99 = *Off
|+---c                   If        M2ICUO=WWICUO
||+--C                   Select
||+--C                   When      M2IASK='1'
|||  C                   Add       M2$IMP        WW$INE
||+--C                   When      M2IASK='2'
|||  C                   Sub       M2$IMP        WW$INE
||+--C                   ENDSL
|+---c                   EndIf
|    c     KM2011        ReadE     REPRMO60                               99
+----c                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
