*SRCMBRTXT:Interbanking-Transferencias-Validador M
     H DEBUG
     H DECEDIT(',') DATEDIT(*YMD/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Gen. de Arch. Txt Transf. p/Interbanking       *
     H*                                                               *
     H*                                                               *
     H*  PROGRAM NO: PRFD01MA                                         *
     H*                                                               *
     H*  DATE:    07/06/2012                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H* Nota: En el archivo vienen los campos MAC1 y MAC2, el MAC1    *
     H* No hay que controlarlo, pues lo controla la RED.              *
     H* El campo MAC2 es "Doble" pues puede ser un "MAC2" o "MAC3"    *
     H* Esto se entiende con la siguiente explicación:                *
     H* La transf. pasa por varios estados, estos cambios estan en    *
     H* un "vector" de estados, campo ITT160, Cuando pasa del estado  *
     H* "00" Ingresada al estado "10" Enviada al Bco Deb la red calc  *
     H* el MAC1, El Banco de Debito lo recibe y calcula el MAC2 pon   *
     H* niendolo en el MAC2, cuando el Banco de Credito la recibe     *
     H* y la impacta (Estado "60") Calcula el MAC3, a este valor lo   *
     H* pone en el campo MAC2 siii en ese como el dise¦o tenia solo   *
     H* dos campos reusaron MAC2 PARA MAC3. Entonces hay que controlar*
     H* El MAC2 o MAC3 en función del estado en que se encuentra la   *
     H* Transferencia, para ello, hay que analizar el vector de estado*
     H* el cual esta formado por cod estado (2A) fecha (6,0) y Hora   *
     H* (6,0) el cual se repite 10 veces y esta guardado como una     *
     H* tira en el campo ITT160 para saber si corresponde calcular    *
     H* MAC2 (MAC de Debito) o MAC3 (MAC de credito)                  *
     H*                                                               *
     H* Acerca de las claves:                                         *
     H*                                                               *
     H* Como Norma geneal la clave de calculo del MAC se toma como    *
     H*                                                               *
     H* BXXXBYYY siendo x e y los nros de los bancos intervinientes   *
     H* siendo x siempre el nro de banco menor e y el banco mayor     *
     H* Si una transf va del nuestro (309) al bco 20 => B020B309      *
     H* Si una transf va del 20 AL NUESTRO           => B020B309      *
     H* La letra B va en MAYUSCULA                                    *
     H*                                                               *
     H* CORREO RECIBIDO DE INTERBANKING                              *
      * Durante la operatoria del dia:
      *
      * 1) Al momento que el cliente envía la transferencia a la red,
      * debe ingresar la clave pactada con el Banco débito,
      * con esta clave se genera el campo de control MAC1.
      * 2) El Banco de débito recibe esta tef y la trata
      * (puede ejecutarla o rechazarla), en este se ingresa
      * la clave pactada con el banco de crédito y se calcula el MAC2
      * (MAC generado por el Banco de débito).
      * Si la rechaza (estado 70) el que se va a informar en el archivo
      * nocturno es el MAC2. Si la acepta pueden pasar dos cosas:
      *          a) Si el Banco de crédito NO trata créditos,
      * la transferencia queda en estado 60 (ejecutada) y el MAC que se
      * va a informar en el archivo nocturno es el MAC2.
      *          b) Si el Banco de crédito trata créditos, la transferencia
      *    queda en estado 40 ó 50. Cuando el Banco de crédito trata
      *    la transferencia, se calcula el MAC3
      *    (en base a la clave pactada entre bancos),
      *    si la acepta queda en estado 60 y si la rechaza en estado 80
      *    (rechazada por Banco de crédito) y en el archivo de final de
      *    dia se informa el MAC3 (MAC generado el Banco de crédito).
      *
      *
      *    En resumen, para los controles que tenés que realizar sobre
      *    el archivo de transferencias en lo que respecta al MAC,  sería:
      *
      *            A ) Si el estado es 70 ó 40 ó 50 entonces el MAC  a
      *    verificar es el MAC2 (porque no intervino el Banco de crédito)
      *            B ) Si el estado es 80 entonces se verifica el MAC3
      *    (intervino el Banco de crédito y rechazó)
      *         C ) Si el estado es 60 y NO paso por un estado 40 ó 50
      *    (lo podes verificar en el vector de estado), se verifica
      *    el MAC2 (no intervino el Banco de crédito)
      *            D) Si el estado 60 y paso por un estado 40 ó 50
      *    (lo podes verificar en el vector de estado),
      *    se verifica el MAC3 (intervino el Banco de crédito)
      *
      *    Para los casos C y D,(no te lo comente por teléfono),
      *    pero en vez de validar el vector, hay un campo del archivo que podría
      *    utilizar (RIEBCO). Si este campo está en blanco quiere decir que no l
      *    trato el crédito (o sea caso C). Y si NO esta en blanco seria el caso
      *
      *    Te paso las clave usadas:
      *    Banco 018 = B018B309
      *    Banco 020 = B020B309
      *    Banco 309 = B309B309
     H*****************************************************************
     FIBTRAN01  UF   E           K DISK
     F@CPISYS   IF A E           K DISK
     F@CPIUSD   IF A E           K DISK
     FSGSYSV    IF   E             DISK
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
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
     D*----------------------------------------------------------------*
     D* DS Para pasar parámetros a cálculo MAC2 (Modo Debito)
     D*----------------------------------------------------------------*
     DDSDEB            DS
     D DBPARM                  1   1024
     D DBPAR1                  1    294
     D DBBADE                  1      3  0
     D DBFESO                  4     11  0
     D DBNUAB                 12     18
     D DBIMPO                 19     35  2
     D DBNOSO                 36     50
     D DBTICU                 51     52  0
     D DBNUCT                 53     69
     D DBBACR                 70     72  0
     D DBNOEN                 73     87
     D DBTICR                 88     89  0
     D DBCTCR                 90    106
     D DBOPA1                107    108  0
     D DBOPA2                109    110  0
     D DBOPA3                111    112  0
     D DBOBS1                113    172
     D DBOBS2                173    272
     D DBNURE                273    279  0
     D DBNUEN                280    282  0
     D DBNUTR                283    289  0
     D DBOPD1                290    291  0
     D DBOPD2                292    293  0
     D DBESTA                294    295
     D DBFINR                296    296
     D*----------------------------------------------------------------*
     D* DS Para Clave
     D*----------------------------------------------------------------*
     DDSKEY            DS
     D ENCKEY                  1      8
     D  FILL1                  1      1
     D  BCO01                  2      4
     D  FILL2                  5      5
     D  BCO02                  6      8

     D*----------------------------------------------------------------*
     D* DS Para pasar parámetros a cálculo MAC3 (Modo Crédito)
     D*----------------------------------------------------------------*
     DDSCRE            DS
     D CRPARM                  1   1024
     D CRPAR1                  1    305
     D CRBADE                  1      3  0
     D CRFESO                  4     11  0
     D CRNUAB                 12     18
     D CRIMPO                 19     35  2
     D CRNOSO                 36     50
     D CRTICU                 51     52  0
     D CRNUCT                 53     69
     D CRBACR                 70     72  0
     D CRNOEN                 73     87
     D CRTICR                 88     89  0
     D CRCTCR                 90    106
     D CROPA1                107    108  0
     D CROPA2                109    110  0
     D CROPA3                111    112  0
     D CROBS1                113    172
     D CROBS2                173    272
     D CRNURE                273    279  0
     D CRNUEN                280    282  0
     D CRNUTR                283    289  0
     D CROPD1                290    291  0
     D CROPD2                292    293  0
     D CRESTA                294    295
     D CROPC1                296    297  0
     D CROPC2                298    299  0
     D CRFINR                300    304
     D*----------------------------------------------------------------*
     D I                              2  0
     D Estado                         2
     D*----------------------------------------------------------------*
     C     KIT010        Chain     REIBTRAN                           25
     C                   DoW       *IN25 = *Off
     c                   If        ITNGT2 <> *Blanks
     c                   ExSr      AnzVectorSts
     C                   ExSr      BuildKey
     c                   ExSr      CalcMACCode
     c                   ExSr      CheckCode
     c                   EndIf
     C     KIT010        ReadE     REIBTRAN                               25
     C                   EndDo
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* CheckCode: Chequea MAC2/3 si corresponde
     c*---------------------------------------------------------------------
     c     CheckCode     BegSr
     c*
     c                   If        MACaChequear <> *Blank
     c                   Eval      ITNT21=MACCAL
     c                   Eval      ITDACO=*BLANKS
     c                   If        MACCAL <> ITNGT2
     c                   Eval      ITDACO='MAC2 Erroneo'
     c                   Else
     c                   Select
     c                   When      IT$A03=0
     c                   Eval      ITDACO='Rec.Inicial por la Red'
     c                   When      IT$A03=10
     c                   Eval      ITDACO='Envíada a Bco.Deb.    '
     c                   When      IT$A03=20
     c                   Eval      ITDACO='Demorada P/Bco.Deb.   '
     c                   When      IT$A03=30
     c                   Eval      ITDACO='Rev.Deb.Exc.Riesgo Red'
     c                   When      IT$A03=40
     c                   Eval      ITDACO='Valor al Cobro        '
     c                   When      IT$A03=50
     c                   Eval      ITDACO='Envíada a Bco.Cred.   '
     c                   When      IT$A03=60
     c                   Eval      ITDACO='Ejecutada             '
     c                   When      IT$A03=70
     c                   Eval      ITDACO='Rechazo Bco.Deb.      '
     c                   When      IT$A03=80
     c                   Eval      ITDACO='Rechazo Bco.Cred.     '
     c                   When      IT$A03=15
     c                   Eval      ITDACO='TFE Pend. Habilitación'
     c                   When      IT$A03=90
     c                   Eval      ITDACO='Rechazo por Inhabilit.'
     c                   EndSl
     c                   EndIf
     c                   Update    reibtran
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* BuildKey:Armar Clave
     c*---------------------------------------------------------------------
     c     BuildKey      BegSr
     c*
     c                   Move      'B'           FILL1
     c                   Move      'B'           FILL2
     c                   if        ITINBA < ITIBAN
     c                   Move      ITINBA        Bco01
     c                   Move      ITIBAN        Bco02
     c                   Else
     c                   Move      ITIBAN        Bco01
     c                   Move      ITINBA        Bco02
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* AnzVectorSts: Analizar el Vector de Estado
     c*---------------------------------------------------------------------
     c     AnzVectorSts  BegSr
     C*
     c                   Move      *Off          TratadoPorBcoC    1
     c                   Move      *Blank        MACaChequear      1
     c*                  For       I=0 to 10
     c*                  Eval      Estado=%subst(ITT160:(i*14)+1:2)
     c*                  If        Estado=*BLANKS
     c*                  Leave
     c*                  EndIf
     c*                  If        Estado='40' or  Estado ='50'
     c*                  Move      *On           TratadoPorBcoC    1
     c*                  EndIf
     c*                  EndFor
     c*
     c*                  If        ITCOTR='40' or ITCOTR='50' or ITCOTR='70'
     c*                  Move      '2'           MACaChequear
     C*                  LeaveSr
     c*                  EndIf
     c*                  If        ITCOTR='80'
     c*                  Move      '3'           MACaChequear
     C*                  LeaveSr
     c*                  EndIf
     c*                  If        ITCOTR='60'
     c*                  If        TratadoPorBcoC=*On
     c*                  Move      '3'           MACaChequear
     c*                  Else
     c*                  Move      '2'           MACaChequear
     C*                  EndIf
     C*                  LeaveSr
     c*                  EndIf
     c*
     c*
     c                   If        ITCOTR='40' or ITCOTR='50' or ITCOTR='70'
     c                   Move      '2'           MACaChequear
     c                   LeaveSr
     c                   EndIf
     c                   If        ITCOTR='80'
     c                   Move      '3'           MACaChequear
     c                   LeaveSr
     c                   EndIf
     c                   If        ITCOTR='60'
     c                   If        ITINI5 = *Blanks
     c                   Move      '2'           MACaChequear
     c                   Else
     c                   Move      '3'           MACaChequear
     c                   EndIf
     c                   EndIf
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* CalcMACCode: Calcula los códigos MAC
     c*---------------------------------------------------------------------
     c     CalcMACCode   BegSr
     C*
     c                   Select
     c                   When      MACaChequear = '2'
     c                   ExSr      MoverValDB
     c                   ExSr      ChkMAC2
     c                   When      MACaChequear = '3'
     c                   ExSr      MoverValCR
     c                   ExSr      ChkMAC3
     c                   Other
     c                   Eval      ITDACO='No hay que chequear MAC'
     c                   EndSl
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* MoverValDB: Mover Valores a las estructuras de Debito
     c*---------------------------------------------------------------------
     c     MoverValDB    BegSr
     c*
     c                   Move      *Blanks       DBPARM
     C                   Move      ITINBA        DBBADE
     C                   MoveL     ITFING        DBFESO
     C                   MoveL     ITNENT        DBNUAB
     C                   Z-Add     IT$IMP        DBIMPO
     C                   Eval      DBNOSO=%Subst(itnya1: 6: 15)
     C                   MoveL     ITTCDB        DBTICU
     C                   MoveL     ITDF01        DBNUCT
     C                   Z-Add     ITIBAN        DBBACR
     C                   Eval      DBNOEN=%Subst(itnmcr: 6: 15)
     C                   MoveL     ITTCCR        DBTICR
     C                   MoveL     ITdf02        DBCTCR
     C                   Z-Add     ITIR02        DBOPA1
     C                   Z-Add     ITIR04        DBOPA2
     C                   MoveL     ITIR05        DBOPA3
     C                   MoveL     ITDTLI        DBOBS1
     C                   MoveL     ITCBA1        DBOBS2
     C                   Move      ITIRFR        DBNURE
     C                   MoveL     ITILOT        DBNUEN
     C                   MoveL     ITICHE        DBNUTR
     C                   MoveL     ITIPRA        DBOPD1
     C                   Move      ITIPRB        DBOPD2
     C                   Move      ITCOTR        DBESTA
     c                   Move      ITCOTR        WWAUX             2 0
     C                   Z-Add     WWAUX         IT$A03
     c                   If        ITCOTR = '40' or ITCOTR='50'
     c                   Move      '60'          DBESTA
     c                   Z-Add     60            IT$A03
     C                   EndIf
     C                   Move      '.'           DBFINR
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* MoverValCR: Mover Valores a las estructuras de Crédito
     c*---------------------------------------------------------------------
     c     MoverValCR    BegSr
     c*
     c*
     c                   Move      *Blanks       CRPARM
     C                   Move      ITINBA        CRBADE
     C                   MoveL     ITFING        CRFESO
     C                   MoveL     ITNENT        CRNUAB
     C                   Z-Add     IT$IMP        CRIMPO
     C                   Eval      CRNOSO=%Subst(itnya1: 6: 15)
     C                   MoveL     ITTCDB        CRTICU
     C                   MoveL     ITDF01        CRNUCT
     C                   Z-Add     ITIBAN        CRBACR
     C                   Eval      CRNOEN=%Subst(itnmcr: 6: 15)
     C                   MoveL     ITTCCR        CRTICR
     C                   MoveL     ITdf02        CRCTCR
     C                   Z-Add     ITIR02        CROPA1
     C                   Z-Add     ITIR04        CROPA2
     C                   MoveL     ITIR05        CROPA3
     C                   MoveL     ITDTLI        CROBS1
     C                   MoveL     ITCBA1        CROBS2
     C                   Move      ITIRFR        CRNURE
     C                   MoveL     ITILOT        CRNUEN
     C                   MoveL     ITICHE        CRNUTR
     C                   MoveL     ITIPRA        CROPD1
     C                   Move      ITIPRB        CROPD2
     C                   Move      ITCOTR        CRESTA
     C                   Z-Add     ITITD1        CROPC1
     C                   Z-Add     ITITD2        CROPC2
     C                   Move      '12.BC'       CRFINR
     c*
     c                   Move      ITCOTR        WWAUX             2 0
     C                   Z-Add     WWAUX         IT$A03
     c                   If        ITCOTR = '40' or ITCOTR='50'
     c                   Move      '60'          DBESTA
     c                   Z-Add     60            IT$A03
     C                   EndIf
     C                   Move      '.'           DBFINR
     c*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* ChkMAC2: MAC Como Bco Debito
     c*---------------------------------------------------------------------
     c     ChkMAC2       BegSr
     c*
     c                   Z-Add     7             OPERAC            1 0
     c                   Z-Add     2             SHIFTK            1 0
     c                   Z-Add     37            BLOQUE            3 0
     c*
     C                   Move      *BLANKS       MACCAL           12
     c*
     C                   CALL      'DNDESC'
     C                   PARM                    DBPARM
     C                   PARM                    ENCKEY
     C                   PARM                    OPERAC
     C                   PARM                    SHIFTK
     C                   PARM                    BLOQUE
     C                   PARM                    MACCAL
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* ChkMAC3: MAC Como Bco Crédito
     c*---------------------------------------------------------------------
     c     ChkMAC3       BegSr
     c*
     c                   Z-Add     7             OPERAC            1 0
     c                   Z-Add     2             SHIFTK            1 0
     c                   Z-Add     38            BLOQUE            3 0
     c*
     C                   Move      *BLANKS       MACCAL           12
     c*
     C                   CALL      'DNDESC'
     C                   PARM                    CRPARM
     C                   PARM                    ENCKEY
     C                   PARM                    OPERAC
     C                   PARM                    SHIFTK
     C                   PARM                    BLOQUE
     C                   PARM                    MACCAL
     c                   EndSr
     c*---------------------------------------------------------------------
     c* *INZSR: Rutina de Inicialización
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     C*
     c     1             Chain     SGSYSV
     C*
     C     KIT010        KList
     C                   KFld                    AASFEI
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* DspErr: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DspErr        BEGSR
     C*
     C                   CALL      'BAER01RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ReadParms:Leer Parámetros
     C*---------------------------------------------------------------------
     C     ReadParams    BegSr
     C*
     C* Lee registro de Usuario
     C     @PJOBN        Chain(N)  @CPIUSD                            80
     C   80              Z-Add     @PJOBN        @ZJOBN
     C   80              Write     @CPIUSRR
     C* Lee registro de sistema
     C     @PJOBN        Chain(N)  @CPISYS                            90
     C   90              Z-Add     @PJOBN        @ZJOBN
     C   90              Write     @CPISYSR
     C*
     C                   EndSr
