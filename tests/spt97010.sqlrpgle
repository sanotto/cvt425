*SRCMBRTXT:VER SI EXISTE UN ARC EN IFS (HOME)     
     D nombre          S             29A
     D ext             S              3A
     D FILE            S             31A
     C     *ENTRY        PLIST
     C                   PARM                    WWIMGN           29
     C                   PARM                    EXT               3
     C*
     C                   Z-ADD     *zero         c                15 0
     C                   EVAL      FILE='%' + %TRIM(WWIMGN) + '%'
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT substr(linea, 1, 29), substr(linea,
     C+ 31, 3) FROM qtemp/linea WHERE linea like :FILE
     C/END-EXEC
     C/EXEC SQL
     c+ open c1
     C/END-EXEC
     C/EXEC SQL
     c+ fetch c1 into :nombre, :ext
     C/END-EXEC
     c                   if        sqlcod <> *zero
     c                   move      *blanks       ext
     c                   endif
     C/EXEC SQL
     c+ close c1
     C/END-EXEC
     C                   SETON                                        LR
