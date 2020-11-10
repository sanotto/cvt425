*SRCMBRTXT:Corrección archivo OJMOFI              
     FOJMOFI    UP   E             DISK
     C                   ADD       1             WWIRRN
     c                   Z-Add     WWIRRN        OJIRRN
     C                   MOVE      'AF'          OJCOTR
     C     OJITRG        IFEQ      '3'
     C                   EXSR      LEVEMB
     C                   ENDIF
     C                   UPDATE    REOJMOFI
     C*-------------------------------------------------------------------------
     C* LEVEMB : LEVANTAR UN EMBARGO
     C*-------------------------------------------------------------------------
     C     LEVEMB        BEGSR
     C*
     C                   Z-Add     OJINUC        WWINUC           15 0
     C*
     C/EXEC SQL
     C+ UPDATE OJMOFI SET OJIETI='L' WHERE OJINUI = :WWINUC
     C/END-EXEC
     C*
     C                   IF        SQLCOD<>*ZERO
     C                   Z-ADD     1             X                15 0
     C                   ELSE
     C                   Z-ADD     1             X                15 0
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* *INZSR : RUTINA DE INICIALIZACION
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C                   Z-ADD     *ZERO         WWIRRN           15 0
     C*
     C                   ENDSR
