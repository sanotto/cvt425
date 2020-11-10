*SRCMBRTXT:VARIOS: Salva y envia a otra maq una li
/*    /COPY LE00525/TO10US,TOS1RR1RI                         */
PARSE ARG BCKSVR','USRNME','USRPWD','DTALIB',',
          BKPLIB','RMTLIB','SVFNME','OMIFLG','OBJLST','REST
/*CALL IMPRIMIR_PARAMETROS */
CALL CREAR_LOG
CALL INSERTAR_LOG
CALL VERIFICAR_OBJETOS
CALL HACER_SALVADO
CALL ENVIAR_SAVF
CALL VERIFICAR_ENVIO

EXIT
/*-------------------------------------------------------------------*/
IMPRIMIR_PARAMETROS:
/*-------------------------------------------------------------------*/
SAY "BCKSVR:" BCKSVR
SAY "USRNME:" USRNME
SAY "USRPWD:" USRPWD
SAY "DTALIB:" DTALIB
SAY "BKPLIB:" BKPLIB
SAY "RMTLIB:" RMTLIB
SAY "SVFNME:" SVFNME
SAY "OMIFLG:" OMIFLG
SAY "OBJLST:" OBJLST
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
VERIFICAR_OBJETOS:
/*-------------------------------------------------------------------*/
ADDRESS COMMAND
"CHKOBJ     OBJ(&BKPLIB) OBJTYPE(*LIB)"
IF RC="CPF9801" THEN DO
   "CRTLIB &BKPLIB"
END/*IF*/
"CHKOBJ     OBJ(&BKPLIB/&SVFNME) OBJTYPE(*FILE)"
IF RC="CPF9801" THEN DO
   "CRTSAVF FILE(&BKPLIB/&SVFNME) "
END/*IF*/
   "CLRSAVF FILE(&BKPLIB/&SVFNME) "
RETURN
/*-------------------------------------------------------------------*/
HACER_SALVADO:
/*-------------------------------------------------------------------*/
CALL ANOTAR_COMIENZO_SALVADO

IF OMIFLG = '1' THEN DO
   "SAVOBJ OBJ("OBJLST") LIB(&DTALIB) DEV(*SAVF)                    ",
   "                   SAVF(&BKPLIB/&SVFNME)                        ",
   "                   OMITOBJ((SDBFIL/ACHISA*) (SDBFIL/PRTMOD5*))  ",
   "                   SAVACT(*LIB)                                 ",
   "                   ACCPTH(*YES)                                 "
END
ELSE DO
   "SAVOBJ OBJ(&OBJLST) LIB(&DTALIB) DEV(*SAVF)  ",
   "                   SAVF(&BKPLIB/&SVFNME)     ",
   "                   SAVACT(*LIB)              ",
   "                   ACCPTH(*YES)              "
END

CALL ANOTAR_FIN_SALVADO
RETURN
/*-------------------------------------------------------------------*/
ANOTAR_COMIENZO_SALVADO:
/*-------------------------------------------------------------------*/
ADDRESS EXECSQL
EXECSQL "SET OPTION COMMIT=*NONE"
EXECSQL "UPDATE QGPL/ENVIOLOG SET  SAVBEG = CURRENT_TIMESTAMP      ",
        "                          WHERE                           ",
        "                                BKPLIB = '"BKPLIB"' AND   ",
        "                                SVFNME = '"SVFNME"'"
RETURN
/*-------------------------------------------------------------------*/
ANOTAR_FIN_SALVADO:
/*-------------------------------------------------------------------*/
ADDRESS EXECSQL
EXECSQL "SET OPTION COMMIT=*NONE"
EXECSQL "UPDATE QGPL/ENVIOLOG SET  SAVEND = CURRENT_TIMESTAMP      ",
        "                          WHERE                           ",
        "                                BKPLIB = '"BKPLIB"' AND   ",
        "                                SVFNME = '"SVFNME"'"
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
/*
 RCMD= "SBMJOB CMD(RSTOBJ OBJ(*ALL)      ",
       "       SAVLIB("DTALIB")          ",
       "       DEV(*SAVF)                ",
       "       SAVF("RMTLIB"/"SVFNME"))  ",
       "       JOB(RSTSAVF)              "

 SAY "QUOTE RCMD " RCMD
 */
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
