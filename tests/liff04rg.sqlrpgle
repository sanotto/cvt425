*SRCMBRTXT:LINK-Rec. de Caidas-Verif LINKNOR CORRI
     C     *ENTRY        PLIST
     C                   PARM                    PGMNME           10
     C                   PARM                    JOBNME           10
     C                   PARM                    USRNME           10
     C                   PARM                    JOBNBR            6
     C                   PARM                    STSFLG            4
     C                   PARM                    RUNFLG            1
     C*
     C                   MOVE      *BLANKS       JOBNME
     C                   MOVE      *BLANKS       USRNME
     C                   MOVE      *BLANKS       JOBNBR
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR
     C+ SELECT SUBSTR(WRKFIL, 4, 10),
     C+ SUBSTR(WRKFIL, 17, 10),
     C+ SUBSTR(WRKFIL, 29, 6),
     C+ SUBSTR(WRKFIL, 111, 4)
     C+  FROM QTEMP/WRKFIL WHERE WRKFIL LIKE
     C+ :PGMNME
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH C1 INTO :JOBNME, :USRNME, :JOBNBR, :STSFLG
     C/END-EXEC
     C                   MOVE      *OFF          RUNFLG
     C                   IF        SQLCOD  =  *ZERO
     C                   MOVE      *ON           RUNFLG
     C                   ENDIF
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   SETON                                        LR
     C                   RETURN
