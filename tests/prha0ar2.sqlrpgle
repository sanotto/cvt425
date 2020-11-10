*SRCMBRTXT:Ces.Hab.-Validación Emp.8     -Valida  
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME  : SIDEBA - PRESTAMOS                            *
     H*                                                               *
     H*  PROGRAM NAME : PRHA03R1                                      *
     H*                                                               *
     H*  PROGRAM TITLE: Ces.Hab.-Valida                               *
     H*                                                               *
     H*  DATE         : 21/10/2003                                    *
     H*                                                               *
     H*  AUTHOR       : Ottonello, Santiago                           *
     H*                                                               *
     H*  DESCRIPTION  :                                               *
     H*---------------------------------------------------------------*
     FPRHAMV03  UF A E           K DISK
     FPRCRED    IF   E           K DISK
     FPRCUOT01  IF   E           K DISK
     FSGSYSV    IF   E             DISK
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
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     C*-------------------------------------------------------------------------
     C                   EXSR      DLTREM
     C                   EXSR      OPNCUR
     C                   EXSR      FETNEX
     C                   DOW       SQLCOD  = *ZERO
     C                   MOVE      DOCU          WWINDO
     C     WKEY01        CHAIN     REPRHAMV                           99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      PRCCRE
     C                   ELSE
     C                   EXSR      NOENVI
     C                   ENDIF
     C                   EXSR      FETNEX
     C                   ENDDO
     C                   EXSR      CLOCUR
     C*
     C                   EXSR      SNDPAS
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* PRCCRE : Proceso Créditos
     C*-------------------------------------------------------------------------
     C     PRCCRE        BEGSR
     C                   EVAL      WW$SA1=0
     C                   EVAL      WW$SA2=0
     C     WKEY01        CHAIN     REPRHAMV                           99
     C                   EVAL      WW$SA1=WW$IMP
     C                   DOW       *IN99 = *OFF
     C                   IF        MV$IMP <= WW$SA1
     C                   EVAL      WW$SA1=WW$SA1 - MV$IMP
     C                   ENDIF
     C     WKEY01        READE     REPRHAMV                               99
     C                   ENDDO
     C                   IF        WW$SA1 > 0
     C                   MOVE      '2'           METIDE            1
     C                   EVAL      WW$SA2=WW$IMP
     C     WKEY01        SETGT     REPRHAMV
     C     WKEY01        READPE    REPRHAMV                               99
     C                   DOW       *IN99 = *OFF
     C                   IF        MV$IMP <= WW$SA2
     C                   EVAL      WW$SA2=WW$SA2 - MV$IMP
     C                   ENDIF
     C     WKEY01        READPE    REPRHAMV                               99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   SELECT
     C                   WHEN      WW$SA1 = 0
     C                   EXSR      MAYMEN
     C                   WHEN      WW$SA2 = 0
     C                   EXSR      MENMAY
     C                   WHEN      (WW$SA1 <> 0 AND WW$SA2 <> 0) AND
     C                              WW$SA1 <= WW$SA2
     C                   EXSR      MAYMEN
     C                   WHEN      (WW$SA1 <> 0 AND WW$SA2 <> 0) AND
     C                              WW$SA1 > WW$SA2
     C                   EXSR      MENMAY
     C                   OTHER
     C                   EXSR      MAYMEN
     C                   ENDSL
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     MAYMEN        BEGSR
     C                   EVAL      *IN40=*OFF
     C     WKEY01        CHAIN     REPRHAMV                           99
     C                   DOW       *IN99 = *OFF
     C                   EVAL      MV$IMO=*ZERO
     C                   EVAL      MV$ICU=*ZERO
     C                   EVAL      MV$IDP=*ZERO
     C                   EVAL      MV$IPR=*ZERO
     C  N40              EVAL      MV$IMO=WW$IMP
     C  N40              EVAL      *IN40=*ON
     C                   IF        MV$IMP <= WW$IMP
     C*
     C                   IF        MVEMSU <> 3
     C                   EVAL      MVCREC = 1
     C                   EVAL      MV$ICU=MV$IMP
     C                   ELSE
     C                   EVAL      MV$SBL=MV$IMP
     C                   EVAL      MVCREC = 2
     C                   ENDIF
     C*
     C                   EVAL      WW$IMP=WW$IMP - MV$IMP
     C                   ELSE
     C                   IF        WW$IMP > 0
     C                   EVAL      MVCREC = 1
     C                   EVAL      MV$ICU=WW$IMP
     C                   EVAL      WW$IMP=WW$IMP - MV$ICU
     C                   ELSE
     C                   EVAL      MVCREC = 2
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     AASFEI        MVFALT
     C                   TIME                    MVHALT
     C                   MOVE      @USR_NAM      MVIUSR
     C                   MOVE      @PRC_NAM      MVIPGM
     C                   Z-ADD     AASFEI        MVFPRC
     C                   UPDATE    REPRHAMV
     C     WKEY01        READE     REPRHAMV                               99
     C                   ENDDO
     C*
     C                   IF        WW$IMP > 0
     C     WKEY01        CHAIN     REPRHAMV                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        MVEMSU <> 3
     C                   EVAL      MV$IDP=WW$IMP
     C                   UPDATE    REPRHAMV
     C                   LEAVE
     C                   ENDIF
     C     WKEY01        READE     REPRHAMV                               99
     C                   ENDDO
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     MENMAY        BEGSR
     C                   EVAL      *IN40=*OFF
     C     WKEY01        SETGT     REPRHAMV
     C     WKEY01        READPE    REPRHAMV                               99
     C                   DOW       *IN99 = *OFF
     C                   EVAL      MV$IMO=*ZERO
     C                   EVAL      MV$ICU=*ZERO
     C                   EVAL      MV$IDP=*ZERO
     C                   EVAL      MV$IPR=*ZERO
     C  N40              EVAL      MV$IMO=WW$IMP
     C  N40              EVAL      MV$IDP=WW$SAL
     C  N40              EVAL      *IN40=*ON
     C                   IF        MV$IMP <= WW$IMP
     C*
     C                   IF        MVEMSU <> 3
     C                   EVAL      MVCREC = 1
     C                   EVAL      MV$ICU=MV$IMP
     C                   ELSE
     C                   EVAL      MV$SBL=MV$IMP
     C                   EVAL      MVCREC = 0
     C                   ENDIF
     C*
     C                   EVAL      WW$IMP=WW$IMP - MV$IMP
     C                   ELSE
     C                   IF        WW$IMP > 0
     C                   EVAL      MVCREC = 1
     C                   EVAL      MV$ICU=WW$IMP
     C                   EVAL      WW$IMP=WW$IMP - MV$ICU
     C                   ELSE
     C                   EVAL      MVCREC = 2
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     AASFEI        MVFALT
     C                   TIME                    MVHALT
     C                   MOVE      @USR_NAM      MVIUSR
     C                   MOVE      @PRC_NAM      MVIPGM
     C                   Z-ADD     AASFEI        MVFPRC
     C                   UPDATE    REPRHAMV
     C     WKEY01        READPE    REPRHAMV                               99
     C                   ENDDO
     C                   IF        WW$IMP > 0
     C     WKEY01        CHAIN     REPRHAMV                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        MVEMSU <> 3
     C                   EVAL      MV$IDP=WW$IMP
     C                   UPDATE    REPRHAMV
     C                   LEAVE
     C                   ENDIF
     C     WKEY01        READE     REPRHAMV                               99
     C                   ENDDO
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     SNDPAS        BEGSR
     C*-------------------------------------------------------------------------
     C* ... Segunda Pasada, Marco como no informado y act. inf de Log
     C     WKEY11        SETLL     REPRHAMV
     C     WKEY11        READE     REPRHAMV                               25
     C                   DOW       *IN25=*OFF
     C                   IF        MVCREC=0
     C                   EVAL      MVCREC=3
     C                   ENDIF
     C                   Z-ADD     AASFEI        MVFALT
     C                   TIME                    MVHALT
     C                   MOVE      @USR_NAM      MVIUSR
     C                   MOVE      @PRC_NAM      MVIPGM
     C                   Z-ADD     AASFEI        MVFPRC
     C                   UPDATE    REPRHAMV
     C     WKEY11        READE     REPRHAMV                               25
     C                   ENDDO
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* *INZSR : Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    PAFAAM            6
     C                   PARM                    PAIEMP            5
     C                   PARM                    PADESC           30
     C* ... Defino Variable Auxiliares
     C                   MOVE      *BLANKS       DOCU              8 0
     C                   MOVE      *BLANKS       NOMB             35
     C     *LIKE         DEFINE    MVISUC        WWISUC
     C     *LIKE         DEFINE    MVINCR        WWINCR
     C     *LIKE         DEFINE    MVIDEG        WWIDEG
     C     *LIKE         DEFINE    MVICUO        WWICUO
     C     *LIKE         DEFINE    MV$IMP        WW$IMP
     C     *LIKE         DEFINE    MVIBCF        WWIBCF
     C     *LIKE         DEFINE    MVIRED        WWIRED
     C     *LIKE         DEFINE    MVIMDS        WWIMDS
     C     *LIKE         DEFINE    MV$IMP        WW$SAL
     C     *LIKE         DEFINE    MV$IMP        WW$SA1
     C     *LIKE         DEFINE    MV$IMP        WW$SA2
     C* ... Clave para PRHAMV03
     C     WKEY01        KLIST
     C                   KFLD                    WWFAAM
     C                   KFLD                    WWIEMP
     C                   KFLD                    WWINDO
     C     WKEY03        KLIST
     C                   KFLD                    WWFAAM
     C                   KFLD                    WWIEMP
     C                   KFLD                    WWINDO
     C                   KFLD                    WWIBCF
     C                   KFLD                    WWIRED
     C                   KFLD                    WWIMDS
     C     WKEY06        KLIST
     C                   KFLD                    WWFAAM
     C                   KFLD                    WWIEMP
     C                   KFLD                    WWINDO
     C* ... Clave para PRCUOT01
     C     WKEY04        KLIST
     C                   KFLD                    MVISUC
     C                   KFLD                    MVINCR
     C                   KFLD                    MVIDEG
     C                   KFLD                    MVICUO
     C     WKEY05        KLIST
     C                   KFLD                    MVISUC
     C                   KFLD                    MVINCR
     C                   KFLD                    MVIDEG
     C* ... Clave para PRHAMV03 2 niveles
     C     WKEY11        KLIST
     C                   KFLD                    WWFAAM
     C                   KFLD                    WWIEMP
     C* ... Clave para PRCUOT01
     C     WKEY02        KLIST
     C                   KFLD                    MVISUC
     C                   KFLD                    MVINCR
     C                   KFLD                    MVIDEG
     C                   KFLD                    MVICUO
     C* ... Convertir valores
     C                   MOVE      PAFAAM        WWFAAM            6 0
     C                   MOVE      PAIEMP        WWIEMP            5 0
     C                   Z-ADD     *ZERO         WWINDO           15 0
     C* ... Recupera Fecha de Sistema
     C     1             CHAIN     SGSYSV                             80
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* DLTREM : Elimino Registros tipo M (Inf. por Gov pero no env por nos)
     C*-------------------------------------------------------------------------
     C     DLTREM        BEGSR
     C/EXEC SQL
     C+   SET OPTION COMMIT=*NONE
     C/END-EXEC
     C/EXEC SQL
     C+ DELETE FROM PRHAMV WHERE MVTIRE ='M' AND MVFAAM=:WWFAAM
     C+ AND MVIEMP=:WWIEMP
     C/END-EXEC
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ENDPGM : Finalizar el Programa
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C                   EXSR      CLOCUR
     C                   EVAL      *INLR=*ON
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* NOENVI : Da de Alta en PRHAMV con Tipo Registro M a Informado No enviado
     C*-------------------------------------------------------------------------
     C     NOENVI        BEGSR
     C                   CLEAR                   REPRHAMV
     C                   MOVE      PAFAAM        MVFAAM
     C                   MOVE      PAIEMP        MVIEMP
     C                   MOVE      'M'           MVTIRE
     C                   MOVE      DOCU          MVINDO
     C                   MOVEL(P)  NOMB          MVNYAP
     C                   Z-ADD     05            MVCREC
     C                   Z-ADD     WW$IMP        MV$IMO
     C                   WRITE     REPRHAMV
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* OPNCUR : Abrir Cursor
     C*-------------------------------------------------------------------------
     C     OPNCUR        BEGSR
     C                   EVAL      SQLCOD=*ZERO
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT CAST(DOCU   AS DEC(8, 0)) , NOMB  ,
     C+ sum(IMPORTE) FROM PRHAVM GROUP BY DOCU  , NOMB
     C/END-EXEC
     C                   IF        SQLCOD <> 0
     C                   EVAL      PADESC='DCLCUR SQLCOD='+%CHAR(SQLCOD)
     C                   EXSR      ENDPGM
     C                   ENDIF
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C                   IF        SQLCOD <> 0
     C                   EVAL      PADESC='DCLCUR SQLCOD='+%CHAR(SQLCOD)
     C                   EXSR      ENDPGM
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CLOCUR : Cerrar el Cursor
     C*-------------------------------------------------------------------------
     C     CLOCUR        BEGSR
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FETNEX : Leer Proxima Fila del Cursor
     C*-------------------------------------------------------------------------
     C     FETNEX        BEGSR
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :DOCU, :NOMB, :WW$IMP
     C/END-EXEC
     C                   ENDSR
