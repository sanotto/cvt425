*SRCMBRTXT:VARIOS: Reemplaza Ñ                    
     FSRCFILE   UF   E             DISK    USROPN RENAME(APGSKSRC:R1)
     D cmd             S           1024A
     c                   open      SRCFILE
     C                   READ      R1                                     99
     C                   DOW       *IN99 = *OFF
     C                   IF        %SCAN('#':SRCDTA) > 0
     C                   EVAL      SRCDTA=%XLATE('#':'Ñ':SRCDTA)
     C                   UPDATE    R1
     C                   ENDIF
     C                   READ      R1                                     99
     C                   ENDDO
     C                   Eval      cmd='DLTOVR SRCFILE'
     C                   call      'QCMDEXC'
     C                   parm                    cmd
     C                   parm      1024          cmdlen           15 5
     C                   CLOSE     SRCFILE
     C                   SETON                                        LR
     C                   RETURN
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRCLIB           10
     C                   PARM                    SRCFIL           10
     C                   PARM                    SRCMBR           10
     C*
     C                   Eval      cmd='OVRDBF ' +
     c                                      'SRCFILE '+
     C                                      %trim(SRCLIB )+'/'+
     C                                      %trim(SRCFIL )+' '+
     C                                      %trim(SRCMBR )
     C*
     C                   call      'QCMDEXC'
     C                   parm                    cmd
     C                   parm      1024          cmdlen           15 5
     C*
     C*
     C                   ENDSR
