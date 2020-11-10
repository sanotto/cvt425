*SRCMBRTXT:Cod. ACCODI/CCCODI para SWITCH         
     H*CODIGOS DE MOVIMIENTOS AC Y CC PARA SWITCH
     H*GENERA ARCHIVO PLANO XXCOD.TXT EN LA SCLT000
     H*PR00525|PR00586
     H* 05-03-2015
     H*
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     FACCODI    IF   E           K DISK
     FCCCODI    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     FXXCOD     O    F   50        DISK    USROPN
     D*-------------------------------------------------------------------------
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
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D**********************************************************************
     C*-------------------------------------------------------------------------
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D Symbols         C                   '|°¬!"#$%&/()=?\¡¿*+~[]{}_-;,:.<>'
     D SymBlanks       C                   '                                '
     D Acentos         C                   'ñÑáéíóúäëïöüãõàèìòùâêîôû@'
     D AceBlanks       C                   'nNAEIOUAEIOUAOAEIOUAEIOU '
     D Apos            C                   ''''
     D APosBlank       C                   ' '
     D*
     D*-------------------------------------------------------------------------
     D RC              S              7A
     C*-------------------------------------------------------------------------
     c     1             Chain     RESGSYSV
     C                   Open      XXCOD
     C* ... Procesamos Caja de Ahorro
     C     *LoVal        SetLL     REACCODI
     c                   Read      REACCODI                               99
     c                   DoW       *In99 = *Off
     C                   MOVEL(P)  FXNCOD        MOVDSC           30
     c                   ExSr      FixDsc
     c                   EXCEPT    DETRCA
     c                   Read      REACCODI                               99
     c                   EndDo
     C* ... Procesamos Cuentas Corrientes
     C     *LoVal        SetLL     RECCCODI
     c                   Read      RECCCODI                               99
     c                   DoW       *In99 = *Off
     C                   MOVEL(P)  BLNCOD        MOVDSC           30
     c                   ExSr      FixDsc
     c                   EXCEPT    DETRCC
     c                   Read      RECCCODI                               99
     c                   EndDo
     c*
     C                   Close     XXCOD
     c                   ExSr      ExportFile
     c*
     c                   Seton                                        LR
     c                   Return
     C*-------------------------------------------------------------------------
     C* ExportFile: Exportar el Archivo
     C*-------------------------------------------------------------------------
     c     ExportFile    BegSr
     c*
     c                   Move      AASFEI        WWMMDD            4
     c                   Move      *Blanks       WWFINA           11
     c                   Eval      WWFINA='COD'+WWMMDD
     c                   Eval      rc=Shell('DEL ''/QDLS/SCLT000/'+WWFINA+''' ')
     C*
     c                   Eval      rc=Shell('CPYTOPCD FROMFILE(QTEMP/XXCOD)'+
     c                                      '         TOFLR(''SCLT000'')   '+
     c                                      '         TODOC('+WWFINA+')     ')
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR: Rutina Inicial
     C*-------------------------------------------------------------------------
     c     *INZSR        BegSr
     c*
     c                   Eval      RC=Shell('DLTF QTEMP/XXCOD')
     C*
     c                   Eval      RC=Shell('CRTPF FILE(QTEMP/XXCOD)   '+
     C                                      '      RCDLEN(50)          ')
     C*
     c                   Eval      RC=Shell('OVRDBF FILE(XXCOD)        '+
     C                                      '       TOFILE(QTEMP/XXCOD)')
     c*
     c                   EndSr
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
     OXXCOD     E            DETRCC
     o                                            2 'CC'
     O                       BLIMCC               5
     O                       MOVDSC              35
     OXXCOD     E            DETRCA
     o                                            2 'CA'
     O                       FXIMCA               5
     O                       MOVDSC              35
     P*---------------------------------------------------------------------
     P* PROCEDIMIENTOS DEBEN DECLARARSE DESPUES DE LA HOJA O, SI ESTA EXISTE
     P*---------------------------------------------------------------------
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
     C*=========================================================================
