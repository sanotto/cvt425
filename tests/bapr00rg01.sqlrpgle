*SRCMBRTXT:RECUPERA DESC. DEBITO DIRECTO          
     C     *ENTRY        PLIST
     C                   PARM                    TIPCTA            2 0
     C                   PARM                    PAICCC           11 0
     C                   PARM                    PAICHE           07 0
     C                   PARM                    TRADSC           21
     C*
     C                   Z-ADD     PAICCC        AUXCTA            9 0
     C                   MOVEL(P)  AUXCTA        NROCTA           21
     C                   MOVE (P)  PAICHE        TRANUM            8
     C                   MOVEL     '%'           TRANUM            8
     C                   MOVE      *BLANKS       TRADSC
     C*
     C/EXEC SQL
     C+  DECLARE C1 CURSOR FOR
     C+  SELECT
     C+  SUBSTR(TRIM(SENEMP)||'-'||TRIM(SPDBPR.PRDTEM) , 1, 20) FROM
     C+  SDBFIL/SPDBPR
     C+  LEFT JOIN SDBFIL/SPEMPR ON   SPEMPR.SEIDEM= SPDBPR.PRIDEM
     C+  WHERE
     C+  SPDBPR.PRICUE = :TIPCTA  AND PRNCTA = :NROCTA
     C+  AND
     C+
     C+  RTRIM(CAST ( SPDBPR.PRTRNU AS CHAR(15))) LIKE :TRANUM
     C/END-EXEC
     C*
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C                   IF        SQLCOD = *ZERO
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :TRADSC
     C/END-EXEC
     C                   IF        SQLCOD <> *ZERO
     C                   MOVE      *BLANKS       TRADSC
     C                   ENDIF
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDIF
     C                   EVAL      *INLR=*ON
     C                   RETURN
