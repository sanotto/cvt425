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
     C     *ENTRY        PLIST
     C                   PARM                    QryString      2048
     C                   PARM                    VALUE           255
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
