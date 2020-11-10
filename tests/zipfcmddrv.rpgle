*SRCMBRTXT:null                                   
     DCANFILESDS       DS
     D CANCHR                  1      2
     D CANNUM                  1      2B 0
     D Llamada         S          32575
     D FILENAME        S            255
     D DESDE           S             15  0

     C                   EVAL      LlamadA= 'CALL MINIZIPOTO (''-B'' ' +
     C                             ''''+
     C                             %TRIM(BASEDIR) +
     C                             ''''
     C                   EVAL      Llamada= %TRIM(Llamada)+ ' '+
     C                             ''''+
     C                             %TRIM(ZIPFILE) +
     C                             ''''
     C     1             DO        CANNUM        X                 2 0
     C                   EVAL      DESDE = ((X-1)*255)+3
     C                   EVAL      FILENAME=%SUBST(FILES:DESDE:255)
     C                   EVAL      Llamada= %TRIM(Llamada)+ ' '+
     C                             ''''+
     C                             %TRIM(FILENAME) +
     C                             ''''
     C                   ENDDO
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
     C*                  PARM                    BASEDIR         255
     C                   PARM                    ZIPFILE         255
     C                   PARM                    FILES          5100
     C*
     C                   MOVEL     FILES         CANCHR
     C                   MOVE      CANNUM        NUMFILES         15 0
     C                   MOVE      *BLANKS       BASEDIR         255
     C*
     C                   ENDSR
