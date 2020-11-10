*SRCMBRTXT:RECUPERA NRO DE JOB                    
**free
ctl-opt dftname(SBGMSNRG)
        option(*SRCSTMT)
        main(Main);

dcl-proc Main;
    dcl-pi *N extpgm;
         FileName   char(10) const;
         FieldName  char(10) const;
    end-pi;

    dcl-s MaxNbr Packed(15:0);
    dcl-s Value  Packed(15:0);
    dcl-s Query  Char(1500);

    Query ='SELECT MAX('+%trim(FieldName)+
                      ') + 1 AS NEXTVAL '+
                      ' FROM '+%TRIM(FileName);

    exec sql set option commit=*None;
    exec sql PREPARE S1  FROM :Query ;
    exec sql DECLARE C1  CURSOR FOR S1 ;
    exec sql OPEN C1;
    exec sql FETCH C1 INTO :VALUE ;

    If SQLCOD <> *ZERO;
       Value=*Zero;
    EndIf;

    exec sql CLOSE C1;
    exec sql SELECT MAX(IFNULL(WNIULN, 0)) INTO :MaxNbr
             FROM BANUME
             WHERE  WNIPF1='BAGETJOB' AND
                    WNIPF2=:FileName  AND
                    WNIPF3=:FieldName;

     if SQLCOD <> *Zero;

        if MaxNbr >= Value;
           Value=MaxNbr+1;
        EndIf;

        exec sql INSERT INTO BANUME
                             (WNIPF1,
                              WNIPF2,
                              WNIPF3,
                              WNIULN)
                        VALUES(
                               'BAGETJOB' ,
                               :FileName  ,
                               :FieldName ,
                               :Value);
     Else;
        if MaxNbr >= Value;
        Value=MaxNbr+1;
        EndIf;
        exec sql UPDATE BANUME set WNIULN=:Value
                        WHERE  WNIPF1='BAGETJOB' AND
                        WNIPF2=:FileName  AND
                        WNIPF3=:FieldName;
     EndIf;
end-proc;
