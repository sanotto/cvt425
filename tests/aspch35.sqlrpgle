*SRCMBRTXT:ORDEN DE PAGO .-CALCULO DE TOTAL ANTERI
     C     *ENTRY        PLIST
     C                   PARM                    CTACTE           15 0
     C                   PARM                    WRKPER            6 0
     C                   PARM                    TOTANT           15 2
     C                   PARM                    DTALIB           10
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR
     C+
     C+ SELECT  CAST(SUM(c.IM220 - c.IM2201) AS DEC(15, 2))
     C+ FROM mod220  a left join mod221 b on
     C+                                a.id220 = b.cb221
     C+             left join mod220 c on b.it221=c.id220
     C+ WHERE int(a.ac220/100)=:WRKPER and
     C+       a.cc220= :CTACTE         and
     C+       a.TC220='ORDPAG'         and
     C+       a.sr220 <> 'BAJA  '      and
     C+       b.Tc221 = 'PRVCBV'
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH C1 INTO :TOTANT
     C/END-EXEC
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   EVAL               *INLR=*ON
     C                   RETURN
