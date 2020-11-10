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
     D*-------------------------------------------------------------------------
     D*  Function key selections from program interface
     D*-------------------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
     I*-------------------------------------------------------------------------
     D QryString       S           1000A
     C     *ENTRY        PLIST
     C                   PARM                    SETLST          255
     C                   MOVE      @PJOBN        JOBNUM            6
     C
     C                   EVAL      QryString = 'UPDATE @CPIUSD SET '+
     C                             %TRIM(SETLST)     +
     C                             ' WHERE @zjobn='+jobnum
     C
     C/EXEC SQL
     C+ SET OPTION COMMIT=*NONE
     c/END-EXEC
     C/EXEC SQL
     C+ PREPARE S1  FROM :QryString
     c/END-EXEC
     C/EXEC SQL
     C+ EXECUTE S1
     c/END-EXEC
     C                   SETON                                        LR
