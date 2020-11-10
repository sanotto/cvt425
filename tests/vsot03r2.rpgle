*SRCMBRTXT:VARIOS: Envia un savfile a otro equipo 
/*    /COPY LE00525/TO10US,TOS1RR1RI                         */
PARSE ARG BCKSVR','USRNME','USRPWD','BKPLIB','RMTLIB','SVFNME
/* CALL IMPRIMIR_PARAMETROS */
CALL CREAR_LOG
CALL ENVIAR_SAVF
CALL VERIFICAR_ENVIO

EXIT
/*-------------------------------------------------------------------*/
IMPRIMIR_PARAMETROS:
/*-------------------------------------------------------------------*/
SAY "BCKSVR:" BCKSVR
SAY "USRNME:" USRNME
SAY "USRPWD:" USRPWD
SAY "BKPLIB:" BKPLIB
SAY "RMTLIB:" RMTLIB
SAY "SVFNME:" SVFNME
RETURN
/*-------------------------------------------------------------------*/
CREAR_LOG:
/*-------------------------------------------------------------------*/
"CHKOBJ     OBJ(&BKPLIB) OBJTYPE(*LIB)"
IF RC="CPF9801" THEN DO
ADDRESS EXECSQL
EXECSQL 'SET OPTION COMMIT=*NONE'
EXECSQL "CREATE TABLE QGPL/ENVIOLOG                  ",
        " (BKPLIB CHAR (10 ) NOT NULL WITH DEFAULT,  ",
        "  SVFNME CHAR (10 ) NOT NULL WITH DEFAULT,  ",
        "  OBJLST CHAR (255) NOT NULL WITH DEFAULT,  ",
        "  RMTLIB CHAR (10 ) NOT NULL WITH DEFAULT,  ",
        "  SAVBEG TIMESTAMP                       ,  ",
        "  SAVEND TIMESTAMP                       ,  ",
        "  TFRBEG TIMESTAMP                       ,  ",
        "  TFREND TIMESTAMP                       ,  ",
        "  TFROKY CHAR(10)   NOT NULL WITH DEFAULT)  "
END/*IF*/
RETURN
/*-------------------------------------------------------------------*/
INSERTAR_LOG:
/*-------------------------------------------------------------------*/
ADDRESS EXECSQL
EXECSQL "SET OPTION COMMIT=*NONE"
EXECSQL "DELETE FROM QGPL/ENVIOLOG WHERE                         ",
        "                                BKPLIB = '"BKPLIB"' AND ",
        "                                SVFNME = '"SVFNME"'"

EXECSQL "INSERT INTO QGPL/ENVIOLOG (BKPLIB,                   ",
        "                           SVFNME,                   ",
        "                           OBJLST,                   ",
        "                           RMTLIB                    ",
        "                          ) VALUES(                  ",
        "                          '"BKPLIB"',                ",
        "                          '"SVFNME"',                ",
        "                          '"OBJLST"',                ",
        "                          '"RMTLIB"'                 ",
        "                         )                           "
RETURN
/*-------------------------------------------------------------------*/
ENVIAR_SAVF:
/*-------------------------------------------------------------------*/
"DLTOVR *ALL       "
"DLTF  QTEMP/RESULT"
"CRTPF QTEMP/RESULT RCDLEN(200)"
"DLTF  QTEMP/SCRIPT"
"CRTPF QTEMP/SCRIPT RCDLEN(200)"
"OVRDBF STDOUT QTEMP/SCRIPT SECURE(*YES)"
 SAY USRNME USRPWD
 SAY "mkdir " RMTLIB
 SAY "cd " RMTLIB
 SAY "QUOTE RCMD CRTSAVF " RMTLIB"/"SVFNME
 SAY "bin"
 say "put " BKPLIB"/"SVFNME" "RMTLIB"/"SVFNME
 say "quit"
"DLTOVR *ALL"
"OVRDBF INPUT  QTEMP/SCRIPT"
"OVRDBF STDOUT QTEMP/RESULT"
CALL ANOTAR_COMIENZO_ENVIO
"FTP RMTSYS(&BCKSVR)"
CALL ANOTAR_FIN_ENVIO
"DLTOVR *ALL"
RETURN
/*-------------------------------------------------------------------*/
ANOTAR_COMIENZO_ENVIO:
/*-------------------------------------------------------------------*/
ADDRESS EXECSQL
EXECSQL "SET OPTION COMMIT=*NONE"
EXECSQL "UPDATE QGPL/ENVIOLOG SET  TFRBEG = CURRENT_TIMESTAMP      ",
        "                          WHERE                           ",
        "                                BKPLIB = '"BKPLIB"' AND   ",
        "                                SVFNME = '"SVFNME"'"
RETURN
/*-------------------------------------------------------------------*/
ANOTAR_FIN_ENVIO:
/*-------------------------------------------------------------------*/
ADDRESS EXECSQL
EXECSQL "SET OPTION COMMIT=*NONE"
EXECSQL "UPDATE QGPL/ENVIOLOG SET  TFREND = CURRENT_TIMESTAMP      ",
        "                          WHERE                           ",
        "                                BKPLIB = '"BKPLIB"' AND   ",
        "                                SVFNME = '"SVFNME"'"
RETURN
/*-------------------------------------------------------------------*/
/*VERIFICAR SI LA TRANSMISION SE COMPLETO OK                         */
/*-------------------------------------------------------------------*/
VERIFICAR_ENVIO:
CADENA=""
TFROKY=0
ADDRESS EXECSQL
EXECSQL 'SET OPTION COMMIT=*NONE'
EXECSQL "DECLARE C2 CURSOR FOR SELECT COUNT(*) FROM QTEMP/RESULT     ",
        "WHERE RESULT LIKE '%226%'                "
EXECSQL "OPEN C2 "
EXECSQL "FETCH C2 INTO :TFROKY"
EXECSQL "CLOSE C2 "
ADDRESS EXECSQL
EXECSQL "SET OPTION COMMIT=*NONE"
IF TFROKY >= 1 THEN DO
EXECSQL "UPDATE QGPL/ENVIOLOG SET  TFROKY = '1'                    ",
        "                          WHERE                           ",
        "                                BKPLIB = '"BKPLIB"' AND   ",
        "                                SVFNME = '"SVFNME"'"
END/*IF*/
RETURN
