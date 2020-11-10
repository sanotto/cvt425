*SRCMBRTXT:VARIOS: Crea Salvado en varios SAVF de 
     FQSYSPRT   O    F  132        PRINTER
     D SBSDS           DS                  OCCURS(99)
     D  SBSNME                        2
     D  SBSSZE                       15  0
     D  SBSBNO                        2  0
     D*-------------------------------------------------------------------------
     D DSSZE           S              2  0
     D DSPTRTOP        S              2  0
     D DSPTRBOT        S              2  0
     D SZEACU          S             15  0
     D SAVFMAX         S             15  0
     D SAVFCTR         S              2  0 INZ(1)
     D FROM            S              3    INZ('TOP')
     D SVFNME          S             10
     D OBJLST          S           2048
     D CMDSTR          S           9086
     D PALIST          S           2048
     C*-------------------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    PAIPAD           15
     C                   PARM                    PAIUSR           10
     C                   PARM                    PAIPAS           10
     C                   PARM                    PADTLB           10
     C                   PARM                    PABKLB           10
     C                   PARM                    PARMLB           10
     C                   PARM                    SVFMAX           10 0
     C*
     C                   Z-ADD     SVFMAX        SAVFMAX
     C*
     C                   ExSr      FillDS
     C                   Z-ADD     *ZERO         SZEACU
     C                   EVAL      OBJLST = ''
     C                   DoW       DSPTRTOP <  DSPTRBOT
     C*
     c                   if        savfctr > 99
     c                   leave
     c                   endif
     C                   IF        FROM='TOP'
     C     DSPTRTOP      OCCUR     SBSDS
     C                   ELSE
     C     DSPTRBOT      OCCUR     SBSDS
     C                   ENDIF
     C*
     C                   IF        FROM='TOP'
     C                   IF        (SZEACU + SBSSZE < SAVFMAX)
     C                   MOVE      SAVFCTR       SBSBNO
     C                   ADD       SBSSZE        SZEACU
     C                   EVAL      OBJLST = %TRIM(OBJLST) + ' ' + SBSNME+'*'
     C                   EXCEPT    DETA
     C                   ADD       1             DSPTRTOP
     C                   ELSE
     C                   MOVE      'BOT'         FROM
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C*
     C                   IF        FROM='BOT'
     C                   IF        (SZEACU + SBSSZE < SAVFMAX)
     C                   MOVE      SAVFCTR       SBSBNO
     C                   ADD       SBSSZE        SZEACU
     C                   EVAL      OBJLST = %TRIM(OBJLST) + ' ' + SBSNME+'*'
     C                   EXCEPT    DETA
     C                   SUB       1             DSPTRBOT
     C                   ELSE
     C                   MOVEL     'SDBFSAVF'    SVFNME
     C                   MOVE      SAVFCTR       SVFNME
     C*
     C                   EVAL      PALIST= %TRIM(PAIPAD) +','+
     c                                     %TRIM(PAIUSR) +','+
     c                                     %TRIM(PAIPAS) +','+
     c                                     %TRIM(PADTLB) +','+
     c                                     %TRIM(PABKLB) +','+
     c                                     %TRIM(PARMLB) +','+
     c                                     %TRIM(SVFNME) +','+
     C                                     '1,'       +
     C                                     %TRIM(OBJLST)+','
     C                   EVAL      CMDSTR='SBMJOB CMD( '                    +
     C                                    'CALL VSOT03R1 ('''+
     c                                    %TRIM(PALIST) +
     C                                                               ''')'  +
     C                                    ') '                              +
     C                                    'JOB('                            +
     C                                    %TRIM(SVFNME)                     +
     C                                    ')'
     C                   Z-ADD     9086          CMDLEN           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    CMDLEN
     C*
     C*
     C                   MOVE      'TOP'         FROM
     C                   Z-ADD     *ZERO         SZEACU
     C                   ADD       1             SAVFCTR
     C                   EVAL      OBJLST = ''
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C*
     C                   EndDo
     C* ... Envio de ACHISA
     C                   MOVEL     'SVFACHIS'    SVFNME
     C                   EVAL      PALIST= %TRIM(PAIPAD) +','+
     c                                     %TRIM(PAIUSR) +','+
     c                                     %TRIM(PAIPAS) +','+
     c                                     %TRIM(PADTLB) +','+
     c                                     %TRIM(PABKLB) +','+
     c                                     %TRIM(PARMLB) +','+
     c                                     %TRIM(SVFNME) +','+
     C                                     '0,'       +
     C                                     'ACHISA*'+','
     C                   EVAL      CMDSTR='SBMJOB CMD( '                    +
     C                                    'CALL VSOT03R1 ('''+
     c                                    %TRIM(PALIST) +
     C                                                               ''')'  +
     C                                    ') '                              +
     C                                    'JOB('                            +
     C                                    %TRIM(SVFNME)                     +
     C                                    ')'
     C*
     C                   Z-ADD     9086          CMDLEN           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDSTR
     C                   PARM                    CMDLEN
     C*
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C     FillDs        BegSr
     C*
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR
     C+  SELECT SUBSTR(MBFILE, 1, 2) SBS ,
     C+  CAST(
     C+  SUM(  (MBDSSZ+ MBISIZ ) /1024
     C+       ) AS DEC(15, 0)) SIZEKB
     C+  FROM
     C+    SDBFIL/SGVERV
     C+  WHERE
     C+    MBFILE <> 'ACHISA'
     C+  GROUP BY
     C+    SUBSTR(MBFILE, 1, 2) ORDER BY
     C+    CAST(
     C+     SUM(  (MBDSSZ+ MBISIZ ) /1024
     C+    ) AS DEC(15, 0)) DESC
     C/END-EXEC
     C*
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C*
     C                   Z-ADD     1             DSSZE
     C     DSSZE         OCCUR     SBSDS
     C/EXEC SQL
     C+ FETCH C1 INTO  :SBSNME, :SBSSZE
     C/END-EXEC
     C*
     C                   DOW       SQLCOD =  0
     C                   ADD       1             DSSZE
     C     DSSZE         OCCUR     SBSDS
     C/EXEC SQL
     C+ FETCH C1 INTO  :SBSNME, :SBSSZE
     C/END-EXEC
     C                   ENDDO
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C*
     C                   SUB       1             DSSZE
     C                   Z-ADD     DSSZE         DSPTRBOT
     C                   Z-ADD     1             DSPTRTOP
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     OQSYSPRT   E            DETA
     O                       SBSNME           +   1
     O                       SBSSZE        J  +   1
     O                       SBSBNO        J  +   1
