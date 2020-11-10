*SRCMBRTXT:Ces.Hab.-Validación Gobierno  -Valida  
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
     FPRHADO    IF   E           K DISK
     FBASCAR01  IF   E           K DISK
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
+----C                   DOW       SQLCOD  = *ZERO
|    C     WKEY01        CHAIN     REPRHAMV                           99
|+---C     *IN99         IFEQ      *OFF
||   C                   EXSR      PRCCRE
|+---C                   ELSE
||   C                   EXSR      NOENVI
|+---C                   ENDIF
|    C                   EXSR      FETNEX
+----C                   ENDDO
     C                   EXSR      CLOCUR
     C*
     C                   EXSR      SNDPAS
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* PRCCRE : Proceso Créditos
     C*-------------------------------------------------------------------------
     C     PRCCRE        BEGSR
     C                   Z-ADD     WW$BAN        WW$IMO
|||| C                   EXSR      SAVIMO
     C* ... Buscar si el importe corresponde a una cta exacta y de igual area
     C                   EVAL      WW$SA1=WW$BAN
     C     WKEY01        CHAIN     REPRHAMV                           99
+----C                   DOW       *IN99 = *OFF
|+---C                   IF        MVCREC=0  OR MVCREC=2
||+--C                   IF        MVEMSU <> 3
|||+-C                   IF        MV$IMP = WW$SA1
|||| C                   IF        MVIBCF = %XLATE(lo:up:AREA)
|||| C                   EVAL      *IN40=*OFF
|||| C                   EXSR      ANACUO
|||| C                   LEAVESR
|||| C                   ENDIF
|||+-C                   ELSE
|||+-C                   ENDIF
||+--C                   ENDIF
||+--C                   ENDIF
||   C     WKEY01        READE     REPRHAMV                               99
|+---C                   ENDDO
|    C* ... Buscar si el importe corresponde a una cta exacta <> area
|    C                   EVAL      WW$SA1=WW$BAN
|    C     WKEY01        CHAIN     REPRHAMV                           99
|+---C                   DOW       *IN99 = *OFF
||+--C                   IF        MVCREC=0  OR MVCREC=2
|||+-C                   IF        MVEMSU <> 3
|||| C                   IF        MV$IMP = WW$SA1
|||| C                   EVAL      *IN40=*OFF
|||| C                   EXSR      ANACUO
|||| C                   LEAVESR
|||| C                   ENDIF
|||+-C                   ENDIF
||+--C                   ENDIF
||   C     WKEY01        READE     REPRHAMV                               99
|+---C                   ENDDO
|    C* ... Aplicar de Mayor a Memor y viceversa
|    C                   EVAL      WW$SA1=0
|    C                   EVAL      WW$SA2=0
|    C                   EVAL      WW$SA1=WW$BAN
|    C     WKEY01        CHAIN     REPRHAMV                           99
|+---C                   DOW       *IN99 = *OFF
||+--C                   IF        MVCREC=0  OR MVCREC=2
|||+-C                   IF        MVEMSU <> 3
|||| C                   IF        MV$IMP <= WW$SA1
|||| C                   EVAL      WW$SA1=WW$SA1 - MV$IMP
|||| C                   ENDIF
|||+-C                   ENDIF
||+--C                   ENDIF
||   C     WKEY01        READE     REPRHAMV                               99
|+---C                   ENDDO
|+---C                   IF        WW$SA1 <> 0
||   C                   EVAL      WW$SA2=WW$BAN
||   C     WKEY01        SETGT     REPRHAMV
||   C     WKEY01        READPE    REPRHAMV                               99
||+--C                   DOW       *IN99 = *OFF
|||+-C                   IF        MVCREC=0  OR MVCREC = 2
|||| C                   IF        MVEMSU <> 3
|||| C                   IF        MV$IMP <= WW$SA2
|||| C                   EVAL      WW$SA2=WW$SA2 - MV$IMP
|||| C                   ENDIF
|||| C                   ENDIF
|||+-C                   ENDIF
|||  C     WKEY01        READPE    REPRHAMV                               99
||+--C                   ENDDO
|+---C                   ENDIF
|    C*
|+---C                   SELECT
|+---C                   WHEN      WW$SA1 = 0
||   C                   EXSR      MAYMEN
|+---C                   WHEN      WW$SA2 = 0
||   C                   EXSR      MENMAY
|+---C                   WHEN      (WW$SA1 <> 0 AND WW$SA2 <> 0) AND
||   C                              WW$SA1 <= WW$SA2
||   C                   EXSR      MAYMEN
|+---C                   WHEN      (WW$SA1 <> 0 AND WW$SA2 <> 0) AND
||   C                              WW$SA1 > WW$SA2
||   C                   EXSR      MENMAY
|+---C                   OTHER
||   C                   EXSR      MAYMEN
|+---C                   ENDSL
|    C*
|    C                   EXSR      SAVIDP
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------
|    C     SAVIMO        BEGSR
||   C     WKEY01        CHAIN     REPRHAMV                           99
||+--C                   DOW       *IN99 = *OFF
|||+-C                   IF        MVEMSU =  1 AND MV$IMO=0
|||| C                   EVAL      MV$IMO=WW$IMO
|||| C                   UPDATE    REPRHAMV
|||| C                   LEAVE
|||+-C                   ENDIF
|||  C     WKEY01        READE     REPRHAMV                               99
||+--C                   ENDDO
|    C                   ENDSR
|    C*----------------------------------------------------------------
|    C     SAVIDP        BEGSR
|+---C                   IF        WW$BAN > 0
||   C     WKEY01        CHAIN     REPRHAMV                           99
||+--C                   DOW       *IN99 = *OFF
|||+-C                   IF        MVEMSU =  1 AND MV$IDP=0
|||| C                   EVAL      MV$IDP=WW$BAN
|||| C                   UPDATE    REPRHAMV
|||| C                   LEAVE
|||+-C                   ENDIF
|||  C     WKEY01        READE     REPRHAMV                               99
||+--C                   ENDDO
|+---C                   ENDIF
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C*
|    C*-------------------------------------------------------------------------
|    C     MAYMEN        BEGSR
|    C                   EVAL      *IN40=*OFF
|    C     WKEY01        CHAIN     REPRHAMV                           99
|+---C                   DOW       *IN99 = *OFF
||   C                   EXSR      ANACUO
||   C     WKEY01        READE     REPRHAMV                               99
|+---C                   ENDDO
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C*
|    C*-------------------------------------------------------------------------
|    C     MENMAY        BEGSR
|    C                   EVAL      *IN40=*OFF
|    C     WKEY01        SETGT     REPRHAMV
|    C     WKEY01        READPE    REPRHAMV                               99
|+---C                   DOW       *IN99 = *OFF
||   C                   EXSR      ANACUO
||   C     WKEY01        READPE    REPRHAMV                               99
|+---C                   ENDDO
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* ANACUO: Analizar Cuotas
|    C*-------------------------------------------------------------------------
|    C     ANACUO        BEGSR
|+---C                   IF        MVCREC = 0 OR MVCREC = 2
||+--C                   IF        MVEMSU <> 3
|||+-C                   IF        WW$BAN > 0
|||| C                   EVAL      MVCREC = 1
|||| C                   IF        MV$IMP <= WW$BAN
|||| C                   EVAL      MV$ICU=MV$IMP
|||| C                   EVAL      WWAREA =  %XLATE(lo:up:AREA)
|||| C                   MOVEL(P)  WWAREA        MVDF08
|||| C                   IF        WWAREA <> MVIBCF
|||| C     WWAREA        CHAIN     REBASCAR                           99
|||| C  N99              EVAL      MVITII=MVITIN
|||| C  N99              EVAL      MVINUI=MVININ
|||| C  N99              EVAL      MVITIN=ARITIN
|||| C  N99              EVAL      MVININ=ARININ
|||| C                   ENDIF
|||| C                   ELSE
|||| C                   EVAL      MV$ICU=WW$BAN
|||| C                   EVAL      WWAREA =  %XLATE(lo:up:AREA)
|||| C                   MOVEL(P)  WWAREA        MVDF08
|||| C                   IF        WWAREA <> MVIBCF
|||| C     WWAREA        CHAIN     REBASCAR                           99
|||| C  N99              EVAL      MVITII=MVITIN
|||| C  N99              EVAL      MVINUI=MVININ
|||| C  N99              EVAL      MVITIN=ARITIN
|||| C  N99              EVAL      MVININ=ARININ
|||| C                   ENDIF
|||| C                   ENDIF
|||| C                   EVAL      WW$BAN=WW$BAN - MV$IMP
|||+-C                   ELSE
|||| C                   EVAL      MVCREC = 2
|||+-C                   ENDIF
||+--C                   ELSE
|||  C                   EVAL      MVCREC = 2
||+--C                   ENDIF
||   C                   EXSR      MARVAL
|+---C                   ENDIF
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* MARVAL: Marva PRHAMV como validado
|    C*-------------------------------------------------------------------------
|    C     MARVAL        BEGSR
|    C*
|    C                   Z-ADD     AASFEI        MVFALT
|    C                   TIME                    MVHALT
|    C                   MOVE      @USR_NAM      MVIUSR
|    C                   MOVE      @PRC_NAM      MVIPGM
|    C                   Z-ADD     AASFEI        MVFPRC
|    C                   UPDATE    REPRHAMV
|    C*
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C     SNDPAS        BEGSR
|    C*-------------------------------------------------------------------------
|    C* ... Segunda Pasada, Marco como no informado y act. inf de Log
|    C     WKEY11        SETLL     REPRHAMV
|    C     WKEY11        READE     REPRHAMV                               25
|+---C                   DOW       *IN25=*OFF
||+--C                   IF        MVCREC=0
|||  C                   EVAL      MVCREC=3
||+--C                   ENDIF
||   C                   Z-ADD     AASFEI        MVFALT
||   C                   TIME                    MVHALT
||   C                   MOVE      @USR_NAM      MVIUSR
||   C                   MOVE      @PRC_NAM      MVIPGM
||   C                   Z-ADD     AASFEI        MVFPRC
||   C                   UPDATE    REPRHAMV
||   C     WKEY11        READE     REPRHAMV                               25
|+---C                   ENDDO
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* *INZSR : Inicialización
|    C*-------------------------------------------------------------------------
|    C     *INZSR        BEGSR
|    C     *ENTRY        PLIST
|    C                   PARM                    PAFAAM            6
|    C                   PARM                    PAIEMP            5
|    C                   PARM                    PADESC           30
|    C* ... Defino Variable Auxiliares
|    C                   MOVE      *BLANKS       DOCU              8 0
|    C                   MOVE      *BLANKS       NOMB             25
|    C                   MOVE      *BLANKS       TRAB              1
|    C                   MOVE      *BLANKS       SEXO              1
|    C                   MOVE      *BLANKS       WWAREA            6
|    C     *LIKE         DEFINE    MVISUC        WWISUC
|    C     *LIKE         DEFINE    MVINCR        WWINCR
|    C     *LIKE         DEFINE    MVIDEG        WWIDEG
|    C     *LIKE         DEFINE    MVICUO        WWICUO
|    C     *LIKE         DEFINE    MV$IMP        WW$BAN
|    C     *LIKE         DEFINE    MV$IMP        WW$CRE
|    C     *LIKE         DEFINE    MVIBCF        WWIBCF
|    C     *LIKE         DEFINE    MVIBCF        AREA
|    C     *LIKE         DEFINE    MVIBCF        AREAUP
|    C     *LIKE         DEFINE    MVIRED        WWIRED
|    C     *LIKE         DEFINE    MVIMDS        WWIMDS
|    C     *LIKE         DEFINE    MV$IMP        WW$SAL
|    C     *LIKE         DEFINE    MV$IMP        WW$SA1
|    C     *LIKE         DEFINE    MV$IMP        WW$SA2
|    C     *LIKE         DEFINE    MV$IMO        WW$IMO
|    C* ... Clave para PRHAMV03
|    C     WKEY01        KLIST
|    C                   KFLD                    WWFAAM
|    C                   KFLD                    WWIEMP
|    C                   KFLD                    WWINDO
|    C     WKEY03        KLIST
|    C                   KFLD                    WWFAAM
|    C                   KFLD                    WWIEMP
|    C                   KFLD                    WWINDO
|    C                   KFLD                    WWIBCF
|    C                   KFLD                    WWIRED
|    C                   KFLD                    WWIMDS
|    C     WKEY06        KLIST
|    C                   KFLD                    WWFAAM
|    C                   KFLD                    WWIEMP
|    C                   KFLD                    WWINDO
|    C* ... Clave para PRCUOT01
|    C     WKEY04        KLIST
|    C                   KFLD                    MVISUC
|    C                   KFLD                    MVINCR
|    C                   KFLD                    MVIDEG
|    C                   KFLD                    MVICUO
|    C     WKEY05        KLIST
|    C                   KFLD                    MVISUC
|    C                   KFLD                    MVINCR
|    C                   KFLD                    MVIDEG
|    C* ... Clave para PRHAMV03 2 niveles
|    C     WKEY11        KLIST
|    C                   KFLD                    WWFAAM
|    C                   KFLD                    WWIEMP
|    C* ... Clave para PRCUOT01
|    C     WKEY02        KLIST
|    C                   KFLD                    MVISUC
|    C                   KFLD                    MVINCR
|    C                   KFLD                    MVIDEG
|    C                   KFLD                    MVICUO
|    C     KEYDO         KLIST
|    C                   KFLD                    DDINDP
|    C                   KFLD                    DDIBCF
|    C                   KFLD                    DDISEX
|    C*                  KFLD                    DDIRED
|    C* ... Convertir valores
|    C                   MOVE      PAFAAM        WWFAAM            6 0
|    C                   MOVE      PAIEMP        WWIEMP            5 0
|    C                   Z-ADD     *ZERO         WWINDO           15 0
|    C* ... Recupera Fecha de Sistema
|    C     1             CHAIN     SGSYSV                             80
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* DLTREM : Elimino Registros tipo M (Inf. por Gov pero no env por nos)
|    C*-------------------------------------------------------------------------
|    C     DLTREM        BEGSR
|    C/EXEC SQL
|    C+   SET OPTION COMMIT=*NONE
|    C/END-EXEC
|    C/EXEC SQL
|    C+ DELETE FROM PRHAMV WHERE MVTIRE ='M' AND MVFAAM=:WWFAAM
|    C+ AND MVIEMP=:WWIEMP
|    C/END-EXEC
|    C/EXEC SQL
|    C+ UPDATE SDBFIL02/PRHAMV SET MVCREC = 0, MV$IMO = 0, MV$ICU = 0,
|    C+ MV$IDP = 0, MV$SBL = 0 WHERE MVFAAM=:WWFAAM AND MVIEMP=:WWIEMP
|    C/END-EXEC
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* ENDPGM : Finalizar el Programa
|    C*-------------------------------------------------------------------------
|    C     ENDPGM        BEGSR
|    C                   EXSR      CLOCUR
|    C                   EVAL      *INLR=*ON
|    C                   RETURN
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* NOENVI : Da de Alta en PRHAMV con Tipo Registro M a Informado No enviado
|    C*-------------------------------------------------------------------------
|    C     NOENVI        BEGSR
|    C*                  CLEAR                   REPRHAMV
|    C*                  MOVE      PAFAAM        MVFAAM
|    C*                  MOVE      PAIEMP        MVIEMP
|    C*                  MOVE      'M'           MVTIRE
|    C*                  MOVE      WWINDO        MVINDO
|    C*                  MOVEL(P)  NOMB          MVNYAP
|    C*                  Z-ADD     05            MVCREC
|    C*                  Z-ADD     WW$BAN        MV$IMO
|    C*                  ADD       WW$CRE        MV$IMO
|    C*                  WRITE     REPRHAMV
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* OPNCUR : Abrir Cursor
|    C*-------------------------------------------------------------------------
|    C     OPNCUR        BEGSR
|    C                   EVAL      SQLCOD=*ZERO
|    C/EXEC SQL
|    C+ DECLARE C1 CURSOR FOR SELECT AREA, DOCU , NOMB, TRAB, SEXO,
|    C+ SUM(COD570+COD571), SUM(COD754+COD755) FROM PRHAVU GROUP BY AREA,
|    C+ DOCU, NOMB, SEXO, TRAB
|    C+ ORDER BY
|    C+ SUM(COD570+COD571) DESC , SUM(COD754+COD755) DESC
|    C/END-EXEC
|+---C                   IF        SQLCOD <> 0
||   C                   EVAL      PADESC='DCLCUR SQLCOD='+%CHAR(SQLCOD)
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
|    C/EXEC SQL
|    C+ OPEN C1
|    C/END-EXEC
|+---C                   IF        SQLCOD <> 0
||   C                   EVAL      PADESC='DCLCUR SQLCOD='+%CHAR(SQLCOD)
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* CLOCUR : Cerrar el Cursor
|    C*-------------------------------------------------------------------------
|    C     CLOCUR        BEGSR
|    C/EXEC SQL
|    C+ CLOSE C1
|    C/END-EXEC
|    C                   ENDSR
|    C*-------------------------------------------------------------------------
|    C* FETNEX : Leer Proxima Fila del Cursor
|    C*-------------------------------------------------------------------------
|    C     FETNEX        BEGSR
|    C/EXEC SQL
|    C+ FETCH NEXT FROM C1 INTO :AREA, :DOCU, :NOMB, :TRAB, :SEXO,
|    C+ :WW$BAN, :WW$CRE
|    C/END-EXEC
|    C
|    C* ... Si es un doc duplicado renormalizar a 90
|    C                   Z-ADD     DOCU          DDINDP
|    C                   EVAL      DDIBCF =  %XLATE(lo:up:AREA)
|    C                   MOVE      SEXO          DDISEX
|    C                   MOVE      TRAB          DDIRED
|    C     KEYDO         CHAIN     REPRHADO                           99
|+---C                   IF        *IN99 = *OFF
||   C                   Z-ADD     DDINDO        WWINDO
|+---C                   ELSE
||   C                   Z-ADD     DOCU          WWINDO
|+---C                   ENDIF
|    C*
|    C                   ENDSR
