*SRCMBRTXT:null                                   
     D Llamada         S          32575
     D FILENAME        S            255
     D DESDE           S             15  0

     C                   EVAL      LlamadA= 'CALL MINIUNZIP ('
     C                   EVAL      Llamada= %TRIM(Llamada)+
     C                             ''''+
     C                             %TRIM(ZIPFILE) +
     C                             ''''
     C                   EVAL      Llamada= %TRIM(Llamada)+
     c                             ' ''-d'' ' +
     C                             ''''+
     C                             %TRIM(DESTDIR) +
     C                             ''''
     C                   EVAL      Llamada= %TRIM(Llamada)+ ')'
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    Llamada
     C                   PARM      32575         LARGO            15 5
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ZIPFILE         255
     C                   PARM                    DESTDIR         255
     C*
     C                   MOVE      *BLANKS       BASEDIR         255
     C*
     C                   ENDSR
