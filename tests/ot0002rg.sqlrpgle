*SRCMBRTXT:OTTONELLO: CORRECCION DE CUOTAS PR 9760
     FPRCUOT01  UF   E           K DISK
     C                   EXSR      OPNC1
     C                   EXSR      FETC1
     C                   DOW       SQLCOD=*ZERO
     C                   EXSR      ACTPRCUOT
     C                   EXSR      FETC1
     C                   ENDDO
     C                   EXSR      CLOC1
     C                   EXSR      ENDPGM
     C*---------------------------------------------------------------------
     C* ACTUALIZAR PRCRED
     C*---------------------------------------------------------------------
     C     ACTPRCUOT     BEGSR
     C*
     C                   Z-ADD     *ZERO         WWISUC            5 0
     C                   Z-ADD     97608         WWINCR           15 0
     C                   Z-ADD     *ZERO         WWIDEG            4 0
     C*
     C     KPRCUOT       KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWINCR
     C                   KFLD                    WWIDEG
     C                   KFLD                    WWICUO
     C*
     C     KPRCUOT       CHAIN     PRCUOT1R                           99
     C                   IF        *IN99= *OFF
     C                   Z-ADD     WWFICC        KGFICC
     C                   Z-ADD     WWFVCU        KGFVCU
     C                   UPDATE    PRCUOT1R
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
     C+ DECLARE C1 CURSOR FOR SELECT KGICUO, KGFICC, KGFVCU FROM
     C+ PRODIM/FIXPRCUOT
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
     C                   Z-ADD     *ZERO         WWICUO            3 0
     C                   Z-ADD     *ZERO         WWFICC            8 0
     C                   Z-ADD     *ZERO         WWFVCU            8 0
     C*
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :WWICUO, :WWFICC, :WWFVCU
     C/END-EXEC
     C                   ENDSR
