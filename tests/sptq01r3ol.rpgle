*SRCMBRTXT:Imágenes de cheques - Recupera arch. vi

/*    /COPY LE00525/TO10US,TOS1RR1RI                         */

CALL START
CALL CONNECTION
EXIT
/*-------------------------------------------------------------------*/
START:
/*-------------------------------------------------------------------*/
ADDRESS COMMAND
SYSNME="192.168.1.5"
USUARIO='anonymous'
CLAVE='anonymous'
CALL RET_FECHA
RETURN
/*-------------------------------------------------------------------*/
CONNECTION:
/*-------------------------------------------------------------------*/
"DLTOVR *ALL       "
"DLTF  QTEMP/RESULT"
"CRTPF QTEMP/RESULT RCDLEN(300)"
"DLTF  QTEMP/SCRIPT"
"CRTPF QTEMP/SCRIPT RCDLEN(300)"
"OVRDBF STDOUT QTEMP/SCRIPT SECURE(*YES)"
 SAY USUARIO CLAVE
 SAY "NAMEFMT 1"
 SAY "BIN"
 CALL INSERT_ARCH
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
/*-------------------------------------------------------------------*/
/*ABRIR CURSOR PARA DATOS                                            */
/*-------------------------------------------------------------------*/
ABRIR_CURSOR:
STMNT1="SELECT TQDACL, TQDTEM, TQFALT FROM SPCHIM",
       "WHERE TQFING ="||WWFING||" ORDER BY TQDTEM"
ADDRESS EXECSQL
EXECSQL 'SET OPTION COMMIT=*NONE'
EXECSQL 'PREPARE S1 FROM :STMNT1'
EXECSQL 'DECLARE C1 CURSOR FOR S1'
EXECSQL "OPEN C1"
RETURN
/*-------------------------------------------------------------------*/
/*FETCH DEL CURSOR                                                   */
/*-------------------------------------------------------------------*/
FETCH_CURSOR:
ADDRESS EXECSQL
EXECSQL 'FETCH C1 INTO :WWDACL, :WWDTEM, :WWFALT'
RETURN
/*-------------------------------------------------------------------*/
/*CERRAR CURSOR                                                      */
/*-------------------------------------------------------------------*/
CERRAR_CURSOR:
ADDRESS EXECSQL
EXECSQL "CLOSE C1"
RETURN
/*-------------------------------------------------------------------*/
/*INSERTA EN ARCHIVO DE SCRIPT                                       */
/*-------------------------------------------------------------------*/
INSERT_ARCH:
CALL ABRIR_CURSOR
CALL FETCH_CURSOR
ADDRESS EXECSQL
WWFTEM = WWFING
INS= 'CD ' || WWFING
INS=TRANSLATE(INS," ",".")
SAY STRIP(INS)
DO WHILE SQLCODE = 0
IF WWFTEM <> WWFING THEN DO
    WWFTEM = WWFING
    SAY 'CD ..'
    INS= 'CD ' || WWFING
    SAY INS
  END
GETSS= 'get '||STRIP(WWDACL)||' /home/CAMARA/ENTCAM/'||STRIP(WWDTEM)||'/',
       ||STRIP(WWDACL)||' (REPLACE'
SAY GETSS
CALL FETCH_CURSOR
END
EXECSQL "CLOSE C1"
RETURN
/*ENDREXSRC*/
