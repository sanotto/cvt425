*SRCMBRTXT:OTTONELLO: CORRECCION DE PR DE MUTUALIZ
     FPRCRED    UF   E           K DISK
     FBASCOR01  UF   E           K DISK
     C                   EXSR      BORRASC$CUO
     C                   EXSR      OPNC1
     C                   EXSR      FETC1
     C                   DOW       SQLCOD=*ZERO
     C                   EXSR      ACTPRCRED
     C                   EXSR      ACTBASCOR
     C                   EXSR      FETC1
     C                   ENDDO
     C                   EXSR      CLOC1
     C                   EXSR      ENDPGM
     C*---------------------------------------------------------------------
     C* ACTUALIZAR BASCOR
     C*---------------------------------------------------------------------
     C     ACTBASCOR     BEGSR
     C                   Z-ADD     UNINDO        SCINDO
     C     UNINDO        CHAIN     REBASCOR                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        SCIBCF = UNIBCF AND UNIRED=SCIRED
     C                   Z-ADD     UNFALT        SC$CUO
     C                   Z-ADD     UN$A03        SC$A03
     C                   Z-ADD     UN$A06        SC$A06
     C                   Z-ADD     UNISUC        SCISUC
     C                   Z-ADD     UNINCR        SCINCR
     C                   Z-ADD     UNIDEG        SCIDEG
     C                   UPDATE    REBASCOR
     C                   ENDIF
     C     SCINDO        READE     REBASCOR                               99
     C                   ENDDO
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ACTUALIZAR PRCRED
     C*---------------------------------------------------------------------
     C     ACTPRCRED     BEGSR
     C*
     C                   Z-ADD     UNISUC        WWISUC            5 0
     C                   Z-ADD     UNIDEG        WWIDEG            4 0
     C*
     C     KPRCRED       KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    UNINCR
     C                   KFLD                    WWIDEG
     C*
     C     KPRCRED       CHAIN     REPRCRED                           99
     C                   IF        *IN99= *OFF AND JVILCR=8
     C                   Z-ADD     UN$A06        JVISOL
     C                   UPDATE    REPRCRED
     C                   ENDIF
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* FIN DE PROGRAMA
     C*---------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ABRE C1
     C*---------------------------------------------------------------------
     C     OPNC1         BEGSR
     C*
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT *  FROM PRODIM/UNIFNBR
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* CIERRA C1
     C*---------------------------------------------------------------------
     C     CLOC1         BEGSR
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* LEE    C1
     C*---------------------------------------------------------------------
     C     FETC1         BEGSR
     C*
     C                   Z-ADD     *ZERO         UNICUI           12 0
     C                   Z-ADD     *ZERO         UNINDO           15 0
     C                   MOVE      *BLANKS       UNIRED            1
     C                   MOVE      *BLANKS       UNISEX            1
     C                   MOVE      *BLANKS       UNNYAP           30
     C                   MOVE      *BLANKS       UNIBCF            6
     C                   Z-ADD     *ZERO         UN$A06           15 2
     C                   Z-ADD     *ZERO         UN$INP           15 2
     C                   Z-ADD     *ZERO         UNQCUO            3 0
     C                   Z-ADD     *ZERO         UNFICC            8 0
     C                   Z-ADD     *ZERO         UNFVCU            8 0
     C                   Z-ADD     *ZERO         UNFALT            8 0
     C                   Z-ADD     *ZERO         UNILCR            4 0
     C                   Z-ADD     *ZERO         UNISUC            9 0
     C                   Z-ADD     *ZERO         UNINCR           15 0
     C                   Z-ADD     *ZERO         UNIDEG            5 0
     C                   Z-ADD     *ZERO         UNILEG            6 0
     C                   Z-ADD     *ZERO         UNINDO01         15 0
     C                   Z-ADD     *ZERO         UN$A03           15 2
     C*
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :UNICUI, :UNINDO, :UNIRED, :UNISEX,
     C+ :UNNYAP, :UNIBCF, :UN$A06, :UN$INP, :UNQCUO, :UNFICC, :UNFVCU,
     C+ :UNFALT, :UNILCR, :UNISUC, :UNINCR, :UNIDEG, :UNILEG, :UNINDO01,
     C+ :UN$A03
     C/END-EXEC
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* BORRA SC$CUO
     C*---------------------------------------------------------------------
     C     BORRASC$CUO   BEGSR
     C/EXEC SQL
     C+ SET OPTION COMMIT=*NONE
     C/END-EXEC
     C/EXEC SQL
     C+ UPDATE SDBFIL/BASCOR SET SC$CUO= 0
     C/END-EXEC
     C                   ENDSR
