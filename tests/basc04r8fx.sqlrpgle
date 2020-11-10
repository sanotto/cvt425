*SRCMBRTXT:null                                   
     FPRCRED    IF   E           K DISK
     C*---------------------------------------------------------------------
     C*
     C                   EXSR      CLRBASCOR
     C                   EXSR      OPNLINEA8
     C                   EXSR      FETLINEA8
     C                   DOW       SQLCOD = *ZERO
     C                   EXSR      PRCCRE
     C                   EXSR      FETLINEA8
     C                   ENDDO
     C                   EXSR      CLOLINEA8
     C                   EXSR      ENDPGM
     C*
     C*---------------------------------------------------------------------
     C* PRCCRE: PROCESAR CREDITO
     C*---------------------------------------------------------------------
     C     PRCCRE        BEGSR
     C*
     C     KPRCRED       CHAIN     REPRCRED                           99
     C* ... Marcar Nro de Certificado en Nro de Solicitud
     C*                  UPDATE    REPRCRED
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* OPENLINEA8: ABRE CURSOR LINEA 8
     C*---------------------------------------------------------------------
     C     OPNLINEA8     BEGSR
     C*
     C/EXEC SQL
     C+ DECLARE C8 CURSOR FOR SELECT JVISUC, JVINCR, JVIDEG FROM PRCRED
     C+ WHERE JVILCR=8 AND JVIESA='1'
     C/END-EXEC
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* CLOLINEA8: CIERRA CURSOR LINEA 8
     C*---------------------------------------------------------------------
     C     CLOLINEA8     BEGSR
     C*
     C/EXEC SQL
     C+ CLOSE C8
     C/END-EXEC
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* FETLINEA8: LEE    CURSOR LINEA 8
     C*---------------------------------------------------------------------
     C     FETLINEA8     BEGSR
     C*
     C/EXEC SQL
     C+ FETCH C8 INTO :C8ISUC, :C8INCR, :C8IDEG
     C/END-EXEC
     C*
     C                   ENDSR
     C*
     C*---------------------------------------------------------------------
     C     CLRBASCOR     BEGSR
     C*
     C/EXEC SQL
     C+ UPDATE BASCOR SET SCDCAR = '', SC$CUO = 0, SC$A03 = 0,
     C+ SC$A04 = 0, SC$A05 = 0, SC$A06 = 0, SCIPLA=0
     C/END-EXEC
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ENDPGM: FIN DE PROGRAMA
     C*---------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C*---------------------------------------------------------------------
     C* *INZSR: INICIALIZACION
     C*---------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *LIKE         DEFINE    JVISUC        C8ISUC
     C     *LIKE         DEFINE    JVINCR        C8INCR
     C     *LIKE         DEFINE    JVIDEG        C8IDEG
     C*
     C     KPRCRED       KLIST
     C                   KFLD                    C8ISUC
     C                   KFLD                    C8INCR
     C                   KFLD                    C8IDEG
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
