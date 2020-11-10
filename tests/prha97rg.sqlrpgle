*SRCMBRTXT:null                                   
     FPRHAMV06  UF   E           K DISK
     C                   Z-ADD     *ZERO         IRRN             15 0
     C                   Z-ADD     *ZERO         FACR              8 0
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT MVIRRN, MVFACR FROM SDBFIL/PRHAMV WHERE
     C+ MVIEMP=2
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :IRRN, :FACR
     C/END-EXEC
     C                   DOW       SQLCOD = 0
     C     IRRN          CHAIN     REPRHAMV                           99
     C                   IF        *IN99= *OFF
     C                   Z-ADD     FACR          MVFACR
     C                   UPDATE    REPRHAMV
     C                   ENDIF
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :IRRN, :FACR
     C/END-EXEC
     C                   ENDDO
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   RETURN
