*SRCMBRTXT:xxxxx                                  
/*    /COPY LE00525/TO10US,TOS1RR1RI                         */

CALL START
CALL CONNECTION
EXIT
/*-------------------------------------------------------------------*/
START:
/*-------------------------------------------------------------------*/
ADDRESS COMMAND
'RTVJOBA USER(&USUARIO)'
SYSNME="192.168.1.7"
CALL RET_FECHA
USUARIO = 'PR00127'
CLAVE = 'xcvbn8'
RETURN
/*-------------------------------------------------------------------*/
CONNECTION:
/*-------------------------------------------------------------------*/
"DLTOVR *ALL       "
"DLTF  QTEMP/RESULT"
"CRTPF QTEMP/RESULT RCDLEN(200)"
"DLTF  QTEMP/SCRIPT"
"CRTPF QTEMP/SCRIPT RCDLEN(200)"
"OVRDBF STDOUT QTEMP/SCRIPT SECURE(*YES)"
 SAY USUARIO CLAVE
 INS= 'CD ' || sol_pago
 INS=TRANSLATE(INS," ",".")
 SAY STRIP(INS)
 SAY "LS *.* (DISK"
 SAY "QUIT"
"DLTOVR *ALL"
"OVRDBF INPUT  QTEMP/SCRIPT"
"OVRDBF STDOUT QTEMP/RESULT"
"FTP RMTSYS(&SYSNME) PORT(28)"
"DLTOVR *ALL"
RETURN
/*-------------------------------------------------------------------*/
/*RECUPERO FECHA DE HOY                                              */
/*-------------------------------------------------------------------*/
RET_FECHA:
ADDRESS EXECSQL
EXECSQL 'SET OPTION COMMIT=*NONE'
EXECSQL 'DECLARE C2 CURSOR FOR SELECT AASFEI FROM SGSYSV'
EXECSQL 'OPEN C2'
EXECSQL 'FETCH C2 INTO :WWFING'
EXECSQL 'CLOSE C2'
RETURN
/*ENDREXSRC*/
