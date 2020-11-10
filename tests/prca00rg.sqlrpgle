*SRCMBRTXT:Prestamos-Cupones Aut.-Procesador de Fi
     FPRCAPA    IP   E           K DISK
     FSGSYSV    IF   E             DISK
     FQSYSPRT   O    F  132        PRINTER
     D*-------------------------------------------------------------------------
     D StrJOIN         S           4096
     D StrWHERE        S           4096
     D StrQUERY        S           8192
     D ETAPA           S             20
     D I               S              4P 0
     D J               S              4P 0
     D K               S              4P 0
     D L               S              4P 0
     D RENGLON         S             80
     D ISUC            S              5P 0
     D INCR            S             15P 0
     D IDEG            S              4P 0
     D ICUO            S              3P 0
     D FVCU            S              8P 0
     D COUNT           S              8P 0
     D*-------------------------------------------------------------------------
     IREPRCAPA      01
     I                                          CAFHAS        L9
     I                                          CAINOP        L8
     C   L8              EXSR      P8IOPL
     C                   EXSR      P7ACCI
     CL8                 EXSR      U8IOPL
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     1             CHAIN     SGSYSV                             99
     C                   MOVE      AASFEI        FECCOR            8
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     P8IOPL        BEGSR
     C*
     C                   EVAL      strJOIN=*BLANKS
     C                   EVAL      strWHERE=*BLANKS
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     P7ACCI        BEGSR
     C*
     C                   IF        CAACCI='J'
     C                   EVAL      strJOIN=%TRIM(strJoin)+' '+%trim(CARENG)
     C                   ELSE
     C                   EVAL      strWHERE=%TRIM(strWHERE)+' '+%trim(CARENG)
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     U8IOPL        BEGSR
     C*
     C                   EVAL      strQUERY='SELECT KGISUC, KGINCR, '+
     C                             'KGIDEG, KGICUO, KGFVCU FROM '+
     C                             'PRCUOT LEFT JOIN PRCRED ON '+
     C                             'JVISUC=KGISUC AND JVINCR=KGINCR '+
     C                             'AND JVIDEG=KGIDEG ' +
     C                             %TRIM(strJOIN)+' '+
     C                             'WHERE KGFEPA=0 AND JVFEPA=0 '+
     C                             'AND KGFVCU <= ' + FECCOR +' AND '+
     C                             %TRIM(strWHERE)
     C*
     C                   EXSR      PrintHeader
     C                   EXSR      ProcessRecords
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     PrintHeader   BEGSR
     C*
     C                   EXCEPT    HEADER
     C                   EVAL      J=1
     C                   EVAL      K=%LEN(RENGLON)
     C                   DOW       J <  %LEN(%TRIM(strQUERY))
     C                   EVAL      L=J+K-1
     C                   IF        L > %LEN(%TRIM(strQUERY))
     C                   EVAL      L=  %LEN(%TRIM(strQUERY))-J+1
     C                   ENDIF
     C                   EVAL      RENGLON=%SUBST(strQUERY:J:L)
     C                   EVAL      J=J+K
     C                   EXCEPT    LINEA
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     ProcessRecordsBEGSR
     C*
     C/EXEC SQL
     C+  PREPARE S1 FROM :StrQUERY
     C/END-EXEC
     C                   IF        SQLCOD<> 0
     C                   EVAL      ETAPA='PREPARE STATEMENT'
     C                   EXCEPT    SQErro
     C                   ELSE
     C/EXEC SQL
     C+  DECLARE C1 CURSOR FOR S1
     C/END-EXEC
     C                   IF        SQLCOD<> 0
     C                   EVAL      ETAPA='DECLARE CURSOR'
     C                   EXCEPT    SQErro
     C                   ELSE
     C/EXEC SQL
     C+  OPEN  C1
     C/END-EXEC
     C                   IF        SQLCOD<> 0
     C                   EVAL      ETAPA='OPEN'
     C                   EXCEPT    SQErro
     C                   ELSE
     C/EXEC SQL
     C+  FETCH C1 INTO  :ISUC, :INCR, :IDEG, :ICUO, :FVCU
     C/END-EXEC
     C                   EVAL      COUNT=0
     C                   IF        SQLCOD <> 0
     C                   EXCEPT    SQNORO
     C                   ENDIF
     C                   DOW       SQLCOD =  0
     C                   EXSR      ProcesaCuota
     C/EXEC SQL
     C+  FETCH C1 INTO  :ISUC, :INCR, :IDEG, :ICUO, :FVCU
     C/END-EXEC
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C/EXEC SQL
     C+  CLOSE C1
     C/END-EXEC
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     ProcesaCuota  BEGSR
     C                   EXCEPT    CUOTA
     C                   EVAL      COUNT=COUNT+1
     C                   ENDSR
     OQSYSPRT   E            HEADER         2
     O                                           +1 'REGLA NRO:'
     O                       CAINOP        J     +1
     O                                           +1 'VALIDA HASTA:'
     O                       CAFHAS              +1 '    /  /  '
     O          E            LINEA          1
     O                       RENGLON             85
     O          E            SQERRO      2  2
     O                                           +1 'ERROR EN LA SENTENCIA'
     O                                           +1 'SQL DE LA REGLA NRO:'
     O                       CAINOP        J     +1
     O                                           +1 'ERROR:'
     O                       SQLCOD        J     +1
     O                                           +1 'EN:'
     O                       ETAPA               +1
     O          E            CUOTA          1
     O                       ISUC          J     +1
     O                       INCR          Z     +1
     O                       IDEG          Z     +1
     O                       ICUO          Z     +1
     O                       FVCU                +1 '    /  /  '
     O          E            SQNORO      2  2
     O                                           +1 'NO HAY FILAS PARA ESTA'
     O                                           +1 'REGLA ...'
     O          E            SQNORO      2  2
     O                                           +1 'TOTAL DE FILAS PARA '
     O                                           +1 'REGLA:'
     O                       CAINOP        J     +1
     O                                           +1 ':'
     O                       COUNT         J     +1
