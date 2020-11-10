*SRCMBRTXT:RECUPERA CAMPO DE LA CPI               
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: BAGETCPI                                       *
     H*                                                               *
     H*  PROGRAM NO: Obtiene un campo de CPI                          *
     H*                                                               *
     H*  DATE:   23/09/2005                                           *
     H*                                                               *
     H*  AUTHOR: Santiago Ottonello                                   *
     H*                                                               *
     H*****************************************************************
     F@CPISYS   IF   E           K DISK
     D*-------------------------------------------------------------------------
     D*  Function key selections from program interface
     D*-------------------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
     I*-------------------------------------------------------------------------
     D QryString       S           1000A
     C     *ENTRY        PLIST
     C                   PARM                    FLDNAM            6
     C                   PARM                    VALUE           255
     C                   PARM                    FKEY              2
     C                   MOVE      @PJOBN        JOBNUM            6
     C                   MOVE      *ZEROS        FKEY              2
     C     @PJOBN        CHAIN     @CPISYS                            99
     C     1             DO        24            X                 2 0
     C                   IF        %SUBST(@ZFNKY:X:1)=*ON
     C                   MOVE      X             FKEY
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C
     C                   EVAL      QryString = 'Select cast('+%trim(fldnam)+
     C                             ' as char(255)) ' +
     C                             ' from @cpiUSd where @zjobn='+jobnum
     C
     C/EXEC SQL
     C+ PREPARE S1  FROM :QryString
     c/END-EXEC
     C/EXEC SQL
     C+ DECLARE C1  CURSOR FOR S1
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH C1 INTO :VALUE
     C/END-EXEC
     C                   IF        SQLCOD <> *ZERO
     C                   EVAL      VALUE=*BLANKS
     C                   ENDIF
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   SETON                                        LR
