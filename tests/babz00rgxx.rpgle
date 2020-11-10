*SRCMBRTXT:Crea archivo de comisiones para imprimi
     HDFTACTGRP(*NO)
     FBACOMI    IF   E           K DISK
     FBATEMP    O    E             DISK    USROPN
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D*-------------------------------------------------------------------------
     D*>DATASTRUCTURE : MYPSDS
     D*>DESCRIPTION   : Data structure containing program status information
     D*>USE           : Retrieve program status information
     D*>RELATEDFUNCT  : sys_cmd
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
     D @CRT_DTE              288    293
     D @CRT_TIM              294    299
     D @CPL_LVL              300    303
     D @SRC_FIL              304    313
     D @SRC_MBR              324    333
     D @PRC_PGM              334    343
     D @PRC_MOD              344    353
     D*-------------------------------------------------------------------------
     D LINEA                        120    DIM(45)
     D ENTRY           DS
     D  ENTTXT                 1     60
     D  DESC                   1     30
     D  SIGNO                 32     32
     D  MONTO                 33     49
     D  PORMO                 43     49
     D  PORSI                 50     50
     C*-------------------------------------------------------------------------
     D L               S            255a
     C*-------------------------------------------------------------------------
     C                   Z-ADD     1             X                 3 0
     C     KBACOMI       CHAIN     REBACOMI                           99
     C                   DOW       *IN99 = *OFF
     C                   MOVE      BZNCOD        DESC
     C                   IF        BZ$IMP > 0
     C                   EVAL      MONTO = %EDITW(BZ$IMP:'           0 ,  ')
     C                   EVAL      PORSI = '$'
     C                   ELSE
     C                   EVAL      PORMO = %EDITW(BZPIMA:' 0 ,  ')
     C                   EVAL      PORSI = '%'
     C                   ENDIF
     C                   EVAL      L=%TRIM(L)+'-'+
     c                             %TRIM(DESC)+':'+%trim(monto)+PORSI
     c                   IF        %LEN(%trim(L)) > 120
     c                   EVAL      LINEA(X)=%SUBST(L:1:120)
     c                   EVAL      L=%SUBST(L:121)
     C                   ADD       1             X
     c                   ENDIF
     C     KBACOMI       READE     REBACOMI                               99
     C                   ENDDO
     c                   EVAL      LINEA(X)=%SUBST(L:1:120)
     C     1             DO        45            X
     C                   MOVEL     LINEA(X)      B9BLK0
     C                   WRITE     REBATEMP
     C                   ENDDO
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C                   CLOSE     BATEMP
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAISUB
     C*
     C     *LIKE         DEFINE    BZISUB        PAISUB
     C*
     C     KBACOMI       KLIST
     C                   KFLD                    PAISUB
     C*
     C                   CallP     Shell('DLTF QTEMP/BATEMP')
     C                   CallP     Shell('CRTDUPOBJ OBJ(BATEMP)         ' +
     C                                   '          FROMLIB(*LIBL)      ' +
     C                                   '          OBJTYPE(*FILE )     ' +
     C                                   '          TOLIB(QTEMP)        ' )
     C*
     C                   OPEN      BATEMP
     C*
     C                   ENDSR
     C*=========================================================================
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
