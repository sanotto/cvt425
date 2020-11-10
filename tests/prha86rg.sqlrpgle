*SRCMBRTXT:null                                   
     FPRHAMV04  IF   E           K DISK
     FPRCRED    IF   E           K DISK
     FPRCUOT01  IF   E           K DISK
     FPRCUOT06  IF   E           K DISK    RENAME(REPRCUOT:R6)
     FPRMOPP    UF   E           K DISK
     FPRTMOD51  UF   E           K DISK
     FESTAH6    O    E           K DISK
     FACCTAC07  IF   E           K DISK
     FPRHATB    IF   E           K DISK
     FACMOVB    O    E           K DISK
     FCCMOVB    O    E           K DISK
     FSGSYSV    IF   E             DISK
     C*----------------------------------------------------------------
     C                   MOVE      *BLANKS       WWERROR          30
     C                   EXSR      OPENC1
     C                   EXSR      FETCHC1
     C                   DOW       SQLCOD = *ZERO
     C                   EVAL      WWERROR='CORREGIDO OK'
     C                   EXSR      FIXCRED
     C                   EXSR      FETCHC1
     C                   ENDDO
     C                   EXSR      CLOSEC1
     C                   EXSR      ENDPGM
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     FIXCRED       BEGSR
     C* ... Ver si el credito aun existe
     C     KeyPRCRED     Chain     REPRCRED                           99
     C                   If        *IN99 =*ON
     C                   Eval      WWERROR='Credito No Existe'
     C                   Goto      Error
     C                   Endif
     C                   If        JVFEPA <> *ZERO
     C                   Eval      WWERROR='Credito Cancelado'
     C                   EXSR      REINT
     C                   Goto      Error
     C                   Endif
     C* ... Ver si el credito fue mandado en periodo 2007/08
     C                   Z-ADD     200708        WWFAAM
     C     KeyPRHAMV     Chain     REPRHAMV                           99
     C                   IF        *IN99=*ON
     C                   Eval      WWERROR='Sin cuota en Periodo 08'
     C                   Goto      Error
     C                   Endif
     C                   MOVE      MVICUO        WKICUO
     C* ... Ver si la cuota no fue pagada por caja aunque no sea renovacion
     C     KeyPRCUOT     Chain     PRCUOT1R                           99
     C                   IF        *IN99=*ON
     C                   Eval      WWERROR='Cuota No Encontrada    '
     C                   Goto      Error
     C                   Endif
     C                   IF        KGFEPA <> *ZERO
     C                   EXSR      FNDCUO
     C                   IF        WWERROR='Sin cuotas libres      '
     C                   Goto      Error
     C                   Endif
     C                   Endif
     C*
     C                   MOVE      *ON           FIXOK             1
     C     KeyPRMOPP     Chain     REPRMOPP                           99
     C                   DoW       *IN99=*OFF
     C                   Exsr      FixPRTMOD50
     C                   IF        UPDOK=*OFF
     C                   MOVE      *OFF          FIXOK
     C                   ENDIF
     C                   MOVE      WKICUO        P2ICUO
     C                   Update    REPRMOPP
     C     KeyPRMOPP     ReadE     REPRMOPP                               99
     C                   EndDo
     C                   If        FIXOK = *OFF
     C                   Eval      WWERROR='No Se Actualizo correct'
     C                   EndIf
     C     Error         Tag
     C*
     C                   MOVE      WWISUC        E6ISUC
     C                   MOVE      WWINCR        E6INCR
     C                   MOVE      WWIDEG        E6IDEG
     C                   MOVE      WWICUO        E6ICUO
     C                   MOVE      WWERROR       E6NYAP
     C                   MOVE      WKICUO        E6QCUO
     C                   WRITE     REESTAH6
     C                   ENDSR
     C*----------------------------------------------------------------
     C* REINT : REINTEGRA
     C*----------------------------------------------------------------
     C     REINT         BEGSR
     C                   Z-ADD     *ZERO         WW$REI           15 2
     C/EXEC SQL
     C+  SELECT SUM(P2$IMP) INTO :WW$REI FROM        PRMOPP
     C+ WHERE
     C+     P2ISUC=:WWISUC AND P2INCR=:WWINCR  AND
     C+     P2IDEG=:WWIDEG AND P2ICUO=:WWICUO  AND P2IMPR=9
     C/END-EXEC
     C                   IF        SQLCOD = *ZERO
     C                   Eval      WWERROR='Cred No Existe-REINTEGRADO'
     C     KeyACCTAC     CHAIN     REACCTAC
     C     KeyPRHATB     CHAIN     REPRHATB
     C                   Z-ADD     WWISUC        INISUC
     C                   Z-ADD     FUICAH        INICAH
     C                   Z-ADD     FUIMON        INIMON
     C                   MOVE      FUIGRC        INIGRC
     C                   Z-ADD     WWIRRN        INICHE
     C                   Z-ADD     TBIMVR        INIMCA
     C                   Z-ADD     1             INIPOS
     C                   Z-ADD     1             INRECI
     C                   Z-ADD     WW$REI        IN$IMP
     C                   Z-ADD     AASFEI        INFACR
     C                   MOVE      *BLANKS       INDTXT
     C                   MOVEL     'PRHA'        INIAYN
     C                   WRITE     REACMOVB
     C*
     C                   Z-ADD     WWISUC        C1ISUC
     C                   Z-ADD     FUICAH        C1ICCC
     C                   Z-ADD     1             C1IMON
     C                   MOVE      '33'          C1IGRC
     C                   Z-ADD     WWIRRN        C1ICHE
     C                   Z-ADD     TBIMSR        C1IMCC
     C                   Z-ADD     1             C1IPOS
     C                   Z-ADD     1             C1RECI
     C                   Z-ADD     WW$REI        C1$IMP
     C                   Z-ADD     AASFEI        C1FACR
     C                   MOVE      *BLANKS       C1DTXT
     C                   MOVEL     'PRHA'        C1IAYN
     C                   WRITE     RECCMOVB
     C*
     C                   ENDIF
     C                   ENDSR
     C*----------------------------------------------------------------
     C* FNDCUO: BUSCA UNA CUOTA IMPAGA
     C*----------------------------------------------------------------
     C     FNDCUO        BEGSR
     C     KeyPRCRED     Chain     R6                                 99
     C                   IF        *IN99
     C                   Eval      WWERROR='Sin cuotas libres      '
     C                   ELSE
     C                   Eval      WWERROR='OK Con cuota <>PRHAMV  '
     C                   EVAL      WKICUO=KGICUO
     C                   ENDIF
     C                   ENDSR
     C*----------------------------------------------------------------
     C* FixPRTMOD50
     C*----------------------------------------------------------------
     C     FixPRTMOD50   BEGSR
     C     KeyPRTMOD51   Chain     REPRTM50                           99
     C                   MOVE      *OFF          UPDOK             1
     C                   DoW       *IN99=*OFF
     C     G8FASI        IFEQ      P2FECH
     C     G8$IMP        ANDEQ     P2$IMP
     C                   MOVE      WKICUO        G8ICUO
     C                   Update    REPRTM50
     C                   MOVE      *ON           UPDOK             1
     C                   ENDIF
     C     KeyPRTMOD51   ReadE     REPRTM50                               99
     C                   EndDo
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     KeyPRCRED     KLIST
     C                   Kfld                    WWISUC
     C                   Kfld                    WWINCR
     C                   Kfld                    WWIDEG
     C     KeyPRCUOT     KLIST
     C                   Kfld                    WWISUC
     C                   Kfld                    WWINCR
     C                   Kfld                    WWIDEG
     C                   Kfld                    MVICUO
     C     KeyPRHAMV     KLIST
     C                   Kfld                    WWFAAM
     C                   Kfld                    WWIEMP
     C                   Kfld                    WWISUC
     C                   Kfld                    WWINCR
     C                   Kfld                    WWIDEG
     C     KeyPRMOPP     KLIST
     C                   Kfld                    WWISUC
     C                   Kfld                    WWINCR
     C                   Kfld                    WWIDEG
     C                   Kfld                    WWICUO
     C     KeyPRTMOD51   KLIST
     C                   Kfld                    P2ISUC
     C                   Kfld                    P2INCR
     C                   Kfld                    P2IDEG
     C                   Kfld                    P2ICUO
     C                   Kfld                    P2IMPR
     C                   Kfld                    WWIMOC
     C     KeyACCTAC     KLIST
     C                   Kfld                    WWISUC
     C                   Kfld                    WWICCL
     C*
     C     KeyPRHATB     KLIST
     C                   Kfld                    WWITIN
     C                   Kfld                    WWININ
     C*
     C     *LIKE         DEFINE    KGISUC        WWISUC
     C     *LIKE         DEFINE    KGINCR        WWINCR
     C     *LIKE         DEFINE    KGIDEG        WWIDEG
     C     *LIKE         DEFINE    KGICUO        WWICUO
     C     *LIKE         DEFINE    MVIRRN        WWIRRN
     C     *LIKE         DEFINE    MVFAAM        WWFAAM
     C     *LIKE         DEFINE    MVICUO        WKICUO
     C     *LIKE         DEFINE    MVIEMP        WWIEMP
     C     *LIKE         DEFINE    MVICCL        WWICCL
     C     *LIKE         DEFINE    MVITIN        WWITIN
     C     *LIKE         DEFINE    MVININ        WWININ
     C     *LIKE         DEFINE    G8IMOC        WWIMOC
     C*
     C                   MOVE      '1'           WWIMOC
     C*
     C     1             CHAIN     SGSYSV
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     OPENC1        BEGSR
     C/EXEC SQL
     C+ DELETE FROM        ESTAH6
     C/END-EXEC
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR
     C+ SELECT DISTINCT
     C+ MVIRRN, MVIEMP, MVISUC, MVINCR, MVIDEG, MVICUO, MVICCL, MVITIN,
     C+ MVININ
     C+ FROM        PRHAMV
     C+ LEFT JOIN        PRMOPP
     C+  ON P2ISUC=MVISUC AND P2INCR=MVINCR  AND
     C+     P2IDEG=MVIDEG AND P2ICUO=MVICUO  AND P2IMPR=9
     C+ WHERE
     C+  MVCREC=1 AND MV$ICU=MV$IMP AND MVFBAJ = 20070706 AND MVIEMP=1
     C+ AND MVFAAM=200707 AND P2ISUC IS NOT NULL
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     CLOSEC1       BEGSR
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     FETCHC1       BEGSR
     C/EXEC SQL
     C+ FETCH C1 INTO :WWIRRN, :WWIEMP, :WWISUC, :WWINCR, :WWIDEG, :WWICUO
     C+ , :WWICCL, :WWITIN, :WWININ
     C/END-EXEC
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     ENDPGM        BEGSR
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
