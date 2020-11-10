*SRCMBRTXT:Link-ODE-Procesa Archivo de vuelta     
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

dcl-ds pgm_stat PSDS qualified;
        exception_type   char(3)        pos(40);
        job_number_char  char(6)        pos(264);
        job_number_num   packed(6:0)    pos(264);
        job_cur_usr      char(10)       pos(358);
        exception_number char(4)        pos(43);
end-ds;

dcl-pr exeCmd char(7);
    cmd_str   char(4096) const;
end-pr;


//==============================================================================
// Procesos o Funsiones.
//==============================================================================
dcl-proc main;
// Variables
    dcl-s cmd                char(4096);
    dcl-s rc                 char(7);
    dcl-s nombre_archivo     char(255);
    dcl-s carpeta            char(255);
    dcl-s carpeta_destino    char(255);
    dcl-s full_path          char(255);
    dcl-s path_destino       char(255);
    dcl-s fecha              packed(8:0);
    dcl-s fecha_deb_cred     packed(8:0);
    dcl-s hora               packed(6:0);
    dcl-s importar_ok        ind;
    dcl-s cuenta_ok          ind;
    dcl-s usuario            char(10);
    dcl-s FileName           char(10) inz('LIMODE');
    dcl-s FieldName          char(10) inz('U1IJOB');
    dcl-s codigo_error       char(1);
    dcl-s campo1_st          char(8);

// Variables para el archivo de respuesta
    dcl-s lnk_ide            packed(8:0);
    dcl-s lnk_pin            packed(8:0);
    dcl-s lnk_fecha_creacion packed(8:0);
    dcl-s lnk_hora_creacion  packed(6:0);
    dcl-s lnk_fecha_ven      packed(8:0);
    dcl-s lnk_cod_res        packed(1:0);
    dcl-s lnk_error          packed(3:0);
    dcl-s lnk_error_des      char(50);
    dcl-s estado_sideba      char(1);
    dcl-s str_pin            char(8);

// Variables para calcular la fecha vencimiento
    dcl-s dias               packed(15:0)INZ(*zeros);
    dcl-s sucursal           packed(5:0) INZ(*zeros);
    dcl-s fecha_vencimiento  packed(8:0);
    dcl-s modo               char(2) INZ('IN');

// Variables para el archivo temporal
    dcl-s CAMPO1             char(710)INZ (' ');

// Llamada al programa 'SGAUTAC1'
   dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
        path_archivo      char(250) CONST ;
   end-pr;

   // Llamada al programa para obetener el proximo dia habil del mes
   dcl-pr callSBBABFE2 extpgm('SBBABFE2');
        fecha     packed(8:0)CONST;
        dias      packed(15:0)CONST;
        sucursal  packed(5:0) CONST;
    end-pr;

   // Llamada al programa para obetener el proximo dia habil del mes
   dcl-pr callSBBAINFE extpgm('SBBAINFE');
        fecha     packed(8:0)CONST;
        modo      char(2)    CONST;
    end-pr;

   dcl-pr mostrar_mensaje extpgm('BAER00RS');
       renglon_1               char(55) CONST ;
       renglon_2               char(55) CONST ;
       renglon_3               char(55) CONST ;
       renglon_4               char(55) CONST ;
       renglon_5               char(55) CONST ;
       renglon_6               char(55) CONST ;
       renglon_7               char(55) CONST ;
       renglon_8               char(55) CONST ;
   end-pr;


//************************DESCRIPCION DEL PROGRAMA****************************//
EXSR inicializo_variables;
EXSR creo_tablas_temporales;
EXSR dir_list_OPEN;
EXSR dir_list_fetch;
DOW SQLcode = *zero;
 EXSR armo_direccion;
 EXSR limpia_tablas_temporales;
 EXSR importar_archivo;
 IF importar_ok;
  EXSR procesa_datos_de_respuesta;
  EXSR escribo_liserj;
  EXSR mover_archivo_procesados;
 ENDIF;
 EXSR dir_list_fetch;
ENDDO;
EXSR dir_list_CLOSE;
EXSR fin_de_programa;


//******************************************************************************
  //  SUBRUTINAS
//******************************************************************************

//==============================================================================
  // Asigno valor a variables necesarias
//==============================================================================
    BEGSR inicializo_variables;
     EXEC SQL set option commit=*None;
     EXEC SQL select aasfei into :fecha from sgsysv;
     EXEC SQL select current_user into :usuario from sysibm.sysdummy1;

      //Recupero directorio origen - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :carpeta from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/DIRORIRET');
     //Recupero directorio destino - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :carpeta_destino from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/DIRDESRET');



      hora   = %dec(%time():*HMS);
      fecha_deb_cred = fecha;

    EXSR calculo_fecha_vencimiento ;

    ENDSR;

//==============================================================================
  // Calculo fecha vencimiento
//==============================================================================
    BEGSR calculo_fecha_vencimiento;
     fecha_vencimiento = fecha;
     dias = *zeros;
     modo = 'IN';
     //Invierto Fecha a DDMMAAAA
     callSBBAINFE(fecha_vencimiento:
                  modo);
     //Busco dia habil anterior.
     callSBBABFE2(fecha_vencimiento:
                  dias:
                  sucursal);
     //Invierto Fecha a AAAAMMDD
     modo = 'NI';
     callSBBAINFE(fecha_vencimiento:
                  modo);
    ENDSR;
//==============================================================================
  // Limpio tablas temporales a trabajar
//==============================================================================
   BEGSR importar_archivo;
     importar_ok = *off;
     rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '  TOFILE(QTEMP/ODELNK)                                  ' +
                '  MBROPT(*REPLACE)                                      ' +
                '  RCDDLM(*CRLF)                                           ' +
                '  FLDDLM('','')                                         ' );

     IF rc = 'CPF0000' ;
      importar_ok = *on;
     ELSE;
     codigo_error = '2';
     EXSR fin_de_programa;
     ENDIF;
   ENDSR;


//==============================================================================
  // Asigo valores de las variables del archivo temporal e hago insert a LIMODE
//==============================================================================
  BEGSR procesa_datos_de_respuesta;

    EXSR temp_ODELNK_OPEN;
    EXSR temp_ODELNK_fetch;

    DOW SQLCODE = *zero;

    campo1_st = %subst(%trim(CAMPO1):1:8) ;
    //pregunto si el registro es el de detalle
    IF campo1_st = 'ORDEXTRA';

    //Asigno variables.

    EXSR asignar_valor_a_variables;

    //UPDATE en LIMODE
    EXSR actualizar_LIMODE;

    campo1_st = *blanks;
    ENDIF;

    EXSR temp_ODELNK_fetch;
    ENDDO;

    EXSR temp_ODELNK_CLOSE;

  ENDSR ;

//=========================================================================
  // Escribo LIMODE
//=========================================================================
   BEGSR actualizar_LIMODE ;
        estado_sideba='4';
        if lnk_cod_res <> 1;
            estado_sideba='3';
            lnk_fecha_ven = fecha_vencimiento;
        endif;
        exec sql UPDATE LIMODE SET
                            TGOPIN = :lnk_pin,
                            TGOFEC = :lnk_fecha_creacion,
                            TGOHOR = :lnk_hora_creacion,
                            TGOFVT = :lnk_fecha_ven,
                            TGOCDR = :lnk_error,
                            TGORES = :lnk_cod_res,
                            TGOERD = :lnk_error_des,
                            TGSSTA = :estado_sideba

                WHERE TGOIDE = :lnk_ide and
                      TGOFVT = 0 and
                      TGSSTA = 2 ;
   IF SQLCODE = *ZEROS;

   ENDIF ;

   ENDSR ;

//=========================================================================
  // Asigno valor a la variables restantes para escribir el LIMODE
//=========================================================================
   BEGSR asignar_valor_a_variables ;

   str_pin = %SUBST(%TRIM(CAMPO1):28:8) ;

   if str_pin  = *blanks ;

   lnk_pin = 0;
   lnk_fecha_creacion = 0;
   lnk_hora_creacion = 0;
   lnk_fecha_ven = 0;

   else;

   lnk_pin  = %DEC(%SUBST(%TRIM(CAMPO1):28:8):8:0);
   lnk_fecha_creacion  = %DEC(%SUBST(%TRIM(CAMPO1):66:6):8:0) + 20000000;
   lnk_hora_creacion  = %DEC(%SUBST(%TRIM(CAMPO1):72:6):6:0);
   lnk_fecha_ven  = %DEC(%SUBST(%TRIM(CAMPO1):78:6):8:0) + 20000000 ;

   endif;

   lnk_ide =  %DEC(%SUBST(%TRIM(CAMPO1):138:8):8:0);
   lnk_cod_res  = %DEC(%SUBST(%TRIM(CAMPO1):84:1):1:0);
   lnk_error  = %DEC(%SUBST(%TRIM(CAMPO1):85:3):3:0);
   lnk_error_des  = %SUBST(%TRIM(CAMPO1):88:50);

   ENDSR;



//=========================================================================
  // Declaro y abro CURSOR para traer todos los campos del temporal
//=========================================================================
 BEGSR temp_ODELNK_OPEN;
   EXEC SQL DECLARE ode CURSOR FOR
      SELECT CAMPO1

       FROM QTEMP/ODELNK;
   EXEC SQL OPEN ode;
   ENDSR ;

//=========================================================================
  // Ejecuto consulta para traer los datos del temporal
//=========================================================================
   BEGSR temp_ODELNK_fetch ;
   EXEC SQL FETCH ode into :CAMPO1;
   ENDSR ;

//=========================================================================
  // Cierro curso ODE
//=========================================================================
   BEGSR temp_ODELNK_CLOSE;
   EXEC SQL CLOSE ode;
   ENDSR ;

//==============================================================================
  // Limpio las tablas temporales
//==============================================================================
    BEGSR creo_tablas_temporales;
      EXEC SQL DROP TABLE QTEMP/ODELNK;

     EXEC SQL
            CREATE TABLE QTEMP/ODELNK (
                CAMPO1                 CHAR (710));
    ENDSR;

//==============================================================================
  // Declaro los CURSOR es y hago OPEN al CURSOR
//==============================================================================
    BEGSR dir_list_OPEN;
     //Consulta para saber si exite archivos de extracto
      EXEC SQL DECLARE C1 CURSOR FOR
      SELECT
            CAST (FILENAME as char(255)) AS FILNAM
      FROM  TABLE(IFSDIR(:carpeta)) AS T
      WHERE UPPER(CAST (FILENAME as char(255))) like '%EODE%'
      ORDER BY CAST (FILENAME as char(255)) ASC;

      EXEC SQL OPEN C1;
    ENDSR;

//==============================================================================
  //  ejecuto contulsta y guardo nombre archivo
//==============================================================================
   BEGSR dir_list_fetch;
    EXEC SQL FETCH C1 into :nombre_archivo;
   ENDSR;

//==============================================================================
  //  Armo direccion del archivo a importar y direccion de destino
//==============================================================================
    BEGSR armo_direccion;
     path_destino = %Trim(carpeta_destino)  +
                    '/'                     +
                    %Trim(nombre_archivo)   +
                    '_'                     +
                    %char(fecha)            +
                    '_'                     +
                    %char(hora);

      full_path = %trim(carpeta)+ '/' + %trim(nombre_archivo);
    ENDSR;

//==============================================================================
  // Limpio tablas temporales a trabajar
//==============================================================================
    BEGSR limpia_tablas_temporales;
     EXEC SQL DELETE FROM QTEMP/ODELNK;
    ENDSR;

//==============================================================================
  // Escribo LISERJ
//==============================================================================
    BEGSR escribo_liserj;
       EXEC SQL INSERT INTO LISERJ
                           (RJDACO,
                            RJFALT,
                            RJIUSR,
                            RJHORA)
                     VALUES(
                           :nombre_archivo,
                           :fecha,
                           :usuario,
                           :hora);
    ENDSR;

//==============================================================================
  // Muevo archivo a procesados
//==============================================================================
    BEGSR mover_archivo_procesados;
     ejecutar_programa_SGAUTAC4(%trim(full_path))   ;

     cmd= 'MOV  OBJ('''  +%Trim(full_path)   +''')     '+
          '     TOOBJ('''+%Trim(path_destino)+''')     ';
     rc=exeCmd(cmd);
      mostrar_mensaje('Se Proceso Archivo':
                    '':
                    nombre_archivo:
                    '':
                    '':
                    '':
                    '':
                    '');
    ENDSR;

//==============================================================================
  // Muevo archivo a procesados
//==============================================================================
    BEGSR dir_list_CLOSE;
        EXEC SQL CLOSE C1;
    ENDSR;

//==============================================================================
  // Fin de programa
//==============================================================================
   BEGSR fin_de_programa;
    //Muestro pantalla error si no se pudo recuperar el numero rrn
    IF codigo_error  = '1';
      //Envio mail por error al recuperar numero rrn.
      cmd = 'SNDMAIL  RECP('''+usuario+''') '                          +
            'SUBJECT(''ODE: Error al insertar registro'') '            +
            'MESG(''Se produjo un error al recuperar el nro rrn'       +
            '. VerIFique y reintente'') '                              ;
      exeCmd(cmd);
      RETURN;
    ENDIF;

    //Muestro pantalla error si no se pudo insertar el contenido al temp
    IF codigo_error  = '2';
      //Envio mail por error al recuperar numero rrn.
      cmd = 'SNDMAIL  RECP('''+usuario+''') '                          +
            'SUBJECT(''ODE: Error al insertar registro'') '            +
            'MESG(''Se produjo un error al quere hacer CPYFRMIMPF '    +
            'al archivo temporal. VerIFique y reintente'') '           ;
      exeCmd(cmd);
      RETURN;
    ENDIF;

   RETURN;
   ENDSR;
end-proc;

//******************************************************************************
  //OTROS PROCEDIMIENTOS Y FUNCIONES
//******************************************************************************
//==============================================================================
  // exeCmd: Ejecuta un comando del sistema operativo y retorn el cpf resultante
//==============================================================================
dcl-proc exeCmd;

    dcl-pi *n char(7);                  // <= CPF Resultante
        cmd_str char(4096) const;       // => String con un comando CL
    end-pi;

    dcl-pr QcmdExc extpgm('QCMDEXC');
        cmdstr  char(4096) const;
        cmdlen  packed(15:5) const;
    end-pr;

    QcmdExc(cmd_str:4096);
    return 'CPF0000';

    begsr *pssr;
        return pgm_stat.exception_type  +
               pgm_stat.exception_number;
    endsr;

end-proc;
//******************************************************************************
