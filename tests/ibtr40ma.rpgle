*SRCMBRTXT:Interbanking-Transferencias-Consulta In
     H DEBUG DATEDIT(*YMD)
     H DFTACTGRP(*NO)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME : SIDEBA - PRESTAMOS                             *
     H*                                                               *
     H*  PROGRAM NAME: LILR00MA                                       *
     H*                                                               *
     H*  DESCRIPTION : Consulta de Embargos Soj                       *
     H*                                                               *
     H*  PROGRAM NO  :                                                *
     H*                                                               *
     H*  DATE        : 03/08/2012                                     *
     H*                                                               *
     H*  AUTHOR      : Ottonello, Santiago                            *
     H*                                                               *
     H*---------------------------------------------------------------*
     F*
     FSGUSUA    IF   E           K DISK
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     FSGSYSV    IF   E             DISK
     D*----------------------------------------------------------------*
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D*----------------------------------------------------------------*
     DMYPSDS          SDS
     D @PRC_NAM          *PROC
     D @PGM_STS          *STATUS
     D @PRV_STS               16     20S 0
     D @LIN_NUM               21     28
     D @ROU_NAM          *ROUTINE
     D @PAR_CNT          *PARMS
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D @PGM_LIB               81     90
     D @EXC_DTA               91    170
     D @EXC_IDE              171    174
     D @DAT_RUN              191    198
     D @DAT_YEA              199    200S 0
     D @LAS_FIL              201    208
     D @FIL_INF              209    243
     D @JOB_NAM              244    253
     D @USR_NAM              254    263
     D @JOB_NUM              264    269S 0
     D @JOB_NUM_C            264    269
     D @PJOBN                264    269  0
     D @JOB_DTE              270    275S 0
     D @RUN_DTE              276    281S 0
     D @RUN_TIM              282    287S 0
     D @CRT_DTE              288    293
     D @CRT_TIM              294    299
     D @CPL_LVL              300    303
     D @SRC_FIL              304    313
     D @SRC_MBR              324    333
     D @PRC_PGM              334    343
     D @PRC_MOD              344    353
     D*----------------------------------------------------------------*
     D MSGDS           DS
     D  MSGTXT                 1    255
     D  WWNCU1                 1     55
     D  WWNCU2                56    110
     D  WWNCU3               111    165
     D  WWNCU4               166    220
     D  WWNCB1               221    275
     D  WWNCB2               276    330
     D  WWNCB3               331    385
     D  WWNCB4               386    440
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     D query           S           4096A
     D rc              S              7a
     D filename        S            255a
     D cmd             S           8096a
     I*----------------------------------------------------------------*
     c                   ExSr      PedirFecha
     c                   DoW       @FN(03)=*Off
     C                   If        @CFDES <> *ZERO
     c                   ExSr      DspMovs
     c                   ExSr      WrkMovs
     C                   EndIf
     c                   ExSr      PedirFecha
     c                   EndDo
     c*
     C                   EXSR      ENDPGM
     C*--------------------------------------------------------------
     C     ExpPcDbf      BegSr
     c*
     C                   Time                    HORANU            6 0
     c                   Z-Add     *DATE         FECHNU            8 0
     c                   Move      HORANU        HORACH            6
     c                   Move      FECHNU        FECHCH            8
     c                   Eval      filename='/home/tmp/IBTR40MA_'+
     c                             FECHCH+'_'+HORACH+'.xls'
     c*
      /free
        query=' SELECT                                                      '+
              '       *                                                     '+
              '   FROM                                                      '+
              '    IBTRAN                                                   '+
              '   WHERE                                                     '+
              '    ITFECH='+%editw(@CFDES:'        ');
      /end-free
     c                   Eval      Cmd='EXPPCDBF SQLSTM('''+%trim(query)+''') '+
     c                                 '     OUTPAT('''+%trim(filename)+''')'
     c*
     c                   Eval      rc=Shell(Cmd)
     c*
     c                   Eval      Cmd='SNDMAIL RECP('''+CVIQOU+''') '+
     c                                 'SUBJECT(''MOVS DEL DIA INTERBANKING'')'+
     c                                 ' MESG(''VEA LA PLANILLA ADJUNTA'') '+
     c                                 'FILE('''+%trim(FILENAME)+''') '
     c*
     c                   Eval      rc=Shell(Cmd)
     c*
     c                   Eval      Cmd='RMVLNK OBJLNK('''+filename+''') '
     c                   Eval      rc=Shell(Cmd)
     c*
     c                   Eval      MSGTXT='Se envío el correo al'+
     c                             'correo electrónico asignada a su '+
     c                             'perfil de usuario'
     c                   ExSr      DspMsg
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     C     WrkMovs       BegSr
     c*
     c                   If        @FN(06)=*On
     c     @USR_NAM      Chain     RESGUSUA                           99
     C                   If        *IN99 = *Off
     c                   If        CVIQOU <> *Blanks
     c                   ExSr      ExpPcDbf
     c                   Else
     c                   Eval      MSGTXT='No tiene una dirección de '+
     c                             'correo electrónico asignada a su '+
     c                             'perfil de usuario'
     c                   ExSr      DspMsg
     C                   EndIf
     c                   EndIf
     c                   EndIf
     c*
     c                   EndSr
     C*--------------------------------------------------------------
     C     DspMovs       BegSr
     C*--------------------------------------------------------------
     c*
      /free
        query=' SELECT                                                      '+
              '    ITFECH as "Fecha Ingreso ",                              '+
              '    ITINBA as "Cód.Bco.Debito",                              '+
              '    ITFING as "Fecha de Compensación",                       '+
              '    ITICHE as "Nro. de Transferencia",                       '+
              '    ITNENT as "Nro. de Abonado",                             '+
              '    ITITPR as "Tipo de Operación",                           '+
              '    IT$IMP as "Importe",                                     '+
              '    ITDBSU as "Suc. de Débito",                              '+
              '    ITNYA1 as "Nombre de Cuenta Solicitante",                '+
              '    ITNMCD as "Número de Cuenta",                            '+
              '    ITTCDB as "Tipo de Cta.Déb.",                            '+
              '    ITCLDB as "Nro. de Cta. Corto",                          '+
              '    ITDF01 as "Nro. de Cta. Débito",                         '+
              '    ITFEMI as "Fecha de Envío a Bco.Déb.",                   '+
              '    ITHEMI as "Hora  de Envío a Bco.Déb.",                   '+
              '    ITIPRA as "Operador Déb. 1",                             '+
              '    ITIPRB as "Operador Déb. 2",                             '+
              '    ITICME as "Cód.Rech.BcoDeb.",                            '+
              '    ITIBAN as "Banco Crédito",                               '+
              '    ITCRSU as "Suc. Bco Crédito",                            '+
              '    ITNMCR as "Nombre de Cta.Crédito",                       '+
              '    ITTCCR as "Tipo de Cta.Crédito",                         '+
              '    ITDF02 as "Cta. Crédito",                                '+
              '    ITFACR as "Fecha de Acreditación",                       '+
              '    ITHTOM as "Hora de Acreditación",                        '+
              '    ITITD1 as "Id.Op.Crédito 1",                             '+
              '    ITITD2 as "Id.Op.Crédito 2",                             '+
              '    ITICMI as "Cód. de Rech. Crédito",                       '+
              '    ITIR01 as "Id Operador PC  ",                            '+
              '    ITIR02 as "Id Firmante 1 PC",                            '+
              '    ITIR04 as "Id Firmante 2 PC",                            '+
              '    ITIR05 as "Id Firmante 3 PC",                            '+
              '    ITFAU1 as "Fecha de Aut.",                               '+
              '    ITHAU1 as "Hora de Aut.",                                '+
              '    ITCOTR as "Código de Estado",                            '+
              '    ITFUAS as "Fecha Ult. Cbio Estado",                      '+
              '    ITDTLI as "Observaciones 1",                             '+
              '    ITCBA1 as "Observaciones 2",                             '+
              '    ITNGT1 as "MAC1",                                        '+
              '    ITNGT2 as "MAC2",                                        '+
              '    ITIRFR as "Referencia",                                  '+
              '    ITILOT as "Nro de Envío",                                '+
              '    ITINI1 as "Pedido de Consolidacion     ",                '+
              '    ITINI2 as "Misma Titularidad           ",                '+
              '    ITINI3 as "No Existe pago previo acept ",                '+
              '    ITINI4 as "Supera Riesgo Abonado       ",                '+
              '    ITINI5 as "Supera Riesgo Banco         ",                '+
              '    ITT160 as "Tabla de Estados            ",                '+
              '    ITINI6 as "Cuenta Especial             ",                '+
              '    ITCUI0 as "Cuit Originante             ",                '+
              '    ITCUI1 as "Cuit Credito                ",                '+
              '    ITINU1 as "Offering en Banco Debito",                    '+
              '    ITINU2 as "Offering en Banco Credito "                   '+
              '   FROM                                                      '+
              '    IBTRAN                                                   '+
              '   WHERE                                                     '+
              '    ITFECH='+%editw(@CFDES:'        ');
      /end-free
     c*
     c                   Eval      PGMTIT=' INTERBANKING-Transf.del Día  '
     c                   Eval      HEDLI1=*Blanks
     c                   Eval      HEDLI2='Fecha :'+%editw(
     c                             @CFDES:'    -  -  ')
     c                   Eval      HEDLI3=*Blanks
     c                   Eval      FUNKEY='F6=Envía a Lotus como Excel'
     c                   Call      'BADSSQMB'
     c                   Parm                    PGMNME           10
     c                   Parm                    PGMTIT           30
     c                   Parm                    HEDLI1           78
     c                   Parm                    HEDLI2           78
     c                   Parm                    HEDLI3           78
     c                   Parm                    FUNKEY           64
     c                   Parm                    query          4096
     c*
     c                   ExSr      ReadParms
     c*
     C                   EndSr
     C*--------------------------------------------------------------
     C     PedirFecha    BegSr
     C*--------------------------------------------------------------
     C                   Call      'BA0001RS'
     c                   ExSr      ReadParms
     c*
     C                   EndSr
     C*--------------------------------------------------------------
     C*ReadParms: Leer Parametros
     C*--------------------------------------------------------------
     C     ReadParms     BEGSR
     C* Lee registro de Usuario
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C* Lee registro de sistema
     C     @PJOBN        CHAIN(N)  @CPISYS                            90
     C                   ENDSR
     C*--------------------------------------------------------------
     C*ENDPGM: Salir del Programa
     C*--------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*DSPMSG: Mostrar Errores
     C*-------------------------------------------------------------------------
     C     DSPMSG        BEGSR
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
     c                   ExSr      ReadParms
     C*
     C                   ENDSR
     C*--------------------------------------------------------------
     C**INZSR: Inicializacion
     C*--------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     KSCTM1        KLIST
     C                   KFLD                    @PJOBN
     C*
     C     1             CHAIN     SGSYSV
     C*
     c                   MoveL(P)  'IBTR40MA'    PGMNME           10
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     P Shell           B                   EXPORT
     D  Shell          PI             7A
     D   Command                   1024A   VALUE
     D CommLen         S             15  5

     C                   Call      'QCMDEXC'
     c                   PARM                    Command
     C                   PARM      1024          CommLen
     C                   Return    'CPF0000'
     c     *PSSR         BEGSR
     C                   Return    @EXC_TYP+@EXC_NUM
     c                   ENDSR
     c
     P Shell           E
