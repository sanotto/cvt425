*SRCMBRTXT:Trae presentaciones: TEI y SUELDOS-CUBI
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*NO)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

/include sdb01.src/qrpgsrc,ligdprot ;
/include sdb01.src/qrpgsrc,ligdifs ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

//... Llamada al programa 'BAER00RS'
   dcl-pr ejecutar_programa_BAER00RS extpgm('BAER00RS');
     PANCO1     char(55) const;
     PANCO2     char(55) const;
     PANCO3     char(55) const;
     PANCO4     char(55) const;
     PANCO5     char(55) const;
     PANCO6     char(55) const;
     PANCO7     char(55) const;
     PANCO8     char(55) const;
   end-pr;

dcl-proc main;


// *entry plist
    dcl-pi *n;
         operacion   char(10) ;
    end-pi;

//... Variables
    dcl-s cmd                     char(4096);
    dcl-s rc                      char(7);
    dcl-s comando_scp             char(4096);
    dcl-s path_archivo            char(250);
    dcl-s nombre_archivo          char(255);
    dcl-s usuario                 char(10);
    dcl-s descripcion_cmd         char(49);
    dcl-s nombre_sistema          char(8) inz('CUBIX');
    dcl-s hora                    packed(6:0);
    dcl-s aasfei                  packed(8:0);
    dcl-s count                   packed(2:0);
    dcl-s count1                  packed(2:0);
    dcl-s host_name               char(250);
    dcl-s url                     char(50);
    dcl-s usuario_cert            char(10);
    dcl-s carpeta                 char(30);
    dcl-s error_archivo_procesado char(1);
    dcl-s error_conexion_servidor char(1) inz('0');

//... Llamada al programa 'SGAUTAC4'
   dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
          path_archivo      char(250) const;
   end-pr;

//=======================Aca empieza el programa==============================//
    exsr consultas_parametros;
    exsr recupero_nombre_maquina_sideba;
    exsr declara_cursores_para_consultas;
    exsr traer_archivos_del_servidor;
    exsr mover_archivos_a_carpetas;
    exsr fin_de_programa;

//==============================================================================
// Consultas para parametros necesarios.
//==============================================================================
    begsr consultas_parametros;
    exec sql set option commit=*None;
    exec sql select current_user into :usuario from sysibm.sysdummy1;
    exec sql select aasfei into :aasfei from sgsysv;
    endsr;

//==============================================================================
// Recupero nombre servido donde se esta ejecutando el pgm (desa, prod, test)
//==============================================================================
    begsr recupero_nombre_maquina_sideba;

    //Recuper nombre del servidor donde se esta ejecutando el programa
         exec sql SELECT
         HOST_NAME INTO :host_name
         FROM TABLE(QSYS2.SYSTEM_STATUS(RESET_STATISTICS=> 'NO')) X;
    Select;
    When %TRIM(host_name)  = 'DESA';

       url = 'cubixqa@cubix.qa.bancorioja.com.ar';
       usuario_cert = 'PR00684';
       carpeta = '/home2/cubixqa/';

    When %TRIM(host_name)  = 'TEST';

       url = 'cubixtest@cubix.test.bancorioja.com.ar';
       usuario_cert = 'SV000CT';
       carpeta = '/home2/cubixtest/';

    When %TRIM(host_name)  = 'PROD';

       url = 'cubixbrioja@cubix.bancorioja.com.ar';
       usuario_cert = 'SV000CP';
       carpeta = '/home/cubixbrioja/';

    ENDSL;

//...Me posiciono en el directorio raiz;
    cmd = 'CHGCURDIR DIR(''/home'')';
    exeCmd(cmd);

    ENDSR;

//==============================================================================
// Declaro los cursores para realizar las consultas.
//==============================================================================

    begsr declara_cursores_para_consultas;

//...Consulta para saber si exite la carpeta 'Presentaciones' en el dir TMP
        exec sql DECLARE C1 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/tmp')) AS T
         WHERE CAST (FILENAME as char(255)) like '%Presentaciones%';

//...Consulta para listar todo el contenido de la carpeta Presentaciones.
        exec sql DECLARE C2 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/tmp/Presentaciones/' || :AASFEI)) AS T;

//...Consulta para saber si exite la carpeta 'acreditaciones' en el dir TMP
        exec sql DECLARE C3 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home')) AS T
         WHERE UPPER(CAST(FILENAME as char(255))) like
               UPPER('%acreditaciones%');


//...Consulta para saber si exite la carpeta 'Archivos_Recibidos' en TMP
        exec sql DECLARE C4 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/TEI')) AS T
         WHERE UPPER(CAST (FILENAME as char(255))) like
               UPPER('%Archivos_Recibidos%');

//...Consulta a liserj
        exec sql DECLARE C5 CURSOR FOR
        SELECT * FROM LISERJ WHERE
        SUBSTR(RJDACO, 1, 25)
         =
        SUBSTR(:nombre_archivo, 1, 25);

    endsr;

//==============================================================================
// Ejecuto comando para traer los archivos del servidor
//==============================================================================

    begsr traer_archivos_del_servidor;

//...Verifico que exista la carpeta Presentaciones en tmp
    exec sql open C1;
    exec sql FETCH C1;
    If SQLCOD <> *ZERO;

      cmd = 'mkdir -p /home/tmp/Presentaciones';
      ejecutar_programa_SGAUTAC4(cmd);
      descripcion_cmd = 'Creo carpeta Presentaciones';
      exsr Insertar_datos_SGPCLG;

    endif;
    exec sql close C1;

//...Cambio de grupo a la carpeta presentaciones
    ejecutar_programa_SGAUTAC4(' chown GR00001 -R  /home/tmp/Presentaciones');

   Select;
//================================SUELDO======================================//
    When %TRIM(operacion)  = 'SUELDO';

//...Traigo archivos del servidor y ejecuto comando con un sbmjob
    comando_scp = 'scp -P 998 -r '+ %TRIM(url)+':'              +
        %trim(carpeta)+'files_cubix/enviados/sueldos/'+%char(aasfei)+'/ ' +
             '/home/tmp/Presentaciones > /home/tmp/prueba2.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';
    rc  =  ejecutar_sbmjob(cmd:usuario_cert);

//...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;
    exeCmd('DLYJOB DLY(30)');

//...Escribo archivo SGPCLG para tener un historial
    descripcion_cmd = 'Traer archivos SUELDOS del servido CUBIX';
    exsr Insertar_datos_SGPCLG;

//=================================TEI========================================//
    When %TRIM(operacion)  = 'TEI';

//...Traigo archivos del servidor y ejecuto comando con un sbmjob
    comando_scp = 'scp -P 998 -r '+ %TRIM(url)+':'                      +
         %trim(carpeta)+'files_cubix/enviados/tei/'+%char(aasfei)+'/ '   +
             '/home/tmp/Presentaciones > /home/tmp/prueba2.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';
    rc  =  ejecutar_sbmjob(cmd:usuario_cert);

//...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;
    exeCmd('DLYJOB DLY(30)');

//...Escribo archivo SGPCLG para tener un historial
    descripcion_cmd = 'Traer archivos de TEI del servido CUBIX';
    exsr Insertar_datos_SGPCLG;

    ENDSL;

//...Cambio grupo para la carpeta fecha y el contenido de la misma
   ejecutar_programa_SGAUTAC4(' chown GR00001 -R /home/tmp/Presentaciones/' +
                                   %char(aasfei) +'/' );

   ejecutar_programa_SGAUTAC4(' chown GR00001 -R /home/tmp/Presentaciones/' +
                                   %char(aasfei) +'/*' );


    endsr;


//==============================================================================
// Muevo archivo a sus carpetas correspondientes
//==============================================================================
    begsr mover_archivos_a_carpetas;

    Select;
//...Pregunto si existe la carpeta acreditaciones
    When %TRIM(operacion)  = 'SUELDO';
    exec sql open C3;
    exec sql FETCH C3;
    If SQLCOD <> *ZERO;
      cmd = 'mkdir -p /home/ACREDITACIONES';
      ejecutar_programa_SGAUTAC4(cmd);

//...Escribo archivo SGPCLG
      descripcion_cmd = 'Creo carpeta acreditaciones';
      exsr Insertar_datos_SGPCLG;

    endif;
    exec sql close C3;

//...Pregunto si existe la carpeta TEI
    When %TRIM(operacion)  = 'TEI';
    exec sql open C4;
    exec sql FETCH C4;
    If SQLCOD <> *ZERO;
      cmd = 'mkdir -p /home/TEI/Archivos_Recibidos';
      ejecutar_programa_SGAUTAC4(cmd);

//...Escribo archivo SGPCLG
      descripcion_cmd = 'Creo carpeta Archivo recibidos en carpeta TEI';
      exsr Insertar_datos_SGPCLG;
    ENDIF;
    exec sql close C4;
    ENDSL;

//============================================================================//

//...Muevo archivos de SUELDOS y TEI de la carpeta presentaciones.
    exec sql open C2;
    exec sql FETCH C2 INTO :nombre_archivo;

    count  = 0;
    count1 = 0;
    DoW SQLCOD = *ZERO;

//...Verifico que el achivo a mover no esta ya procesado.
    error_archivo_procesado = '0';
    exsr verifico_LISERJ;
//...Si error es = 1 leo el siguiente archivo y vuelvo el bucle
    if error_archivo_procesado = '1';
           exec sql fetch C2 INTO :nombre_archivo;
           iter;
    endif;

         Select;
//================================SUELDO======================================//
         When %subst(nombre_archivo :1 :5) = 'ACSUE';
         count = count + 1;

//...Muevo archivo SUELDO de presentaciones a acreditaciones.
         cmd = 'mv /home/tmp/Presentaciones/' + %char(aasfei) + '/' +
                %trim(nombre_archivo) + ' /home/acreditaciones';
         ejecutar_programa_SGAUTAC4(cmd);

//...Escribo archivo SGPCLG
         descripcion_cmd = 'Mover archivo ' + %char(count) +
                           ' de SUELDO a acreditaciones ';
         exsr Insertar_datos_SGPCLG;

//...Cambio grupo del archivo en la carpeta acreditaciones.
         ejecutar_programa_SGAUTAC4(' chown GR00001 -r /home/acreditaciones/' +
                                   nombre_archivo );

//================================TEI=========================================//
         When %subst(nombre_archivo :1 :5) = 'IMTEI';
         count1 = count1 + 1;

//...Muevo archivo TEI de presentaciones a acreditaciones.
         cmd = 'mv /home/tmp/Presentaciones/' + %char(aasfei) + '/' +
                %trim(nombre_archivo) + ' /home/TEI/Archivos_Recibidos';
         ejecutar_programa_SGAUTAC4(cmd);

//...Escribo SGPCLG
         descripcion_cmd = 'Mover archivo ' + %char(count1) +
                           ' de TEI a Archivos Recibidos' ;
         exsr Insertar_datos_SGPCLG;

//...Cambio grupo para el archivo TEI en carpeta TEI
 ejecutar_programa_SGAUTAC4(' chown GR00001 -r /home/TEI/Archivos_Recibidos/' +
                                   nombre_archivo );

        ENDSL;
    exec sql fetch C2 INTO :nombre_archivo;

    ENDDO;
    exec sql close C2;

//...Elimino la carpeta presentaciones
    ejecutar_programa_SGAUTAC4(' rm -r /home/tmp/Presentaciones');

    endsr;


//==============================================================================
// Inserto registro en SGPCLG  cada vez que ejecuto un escrip
//==============================================================================
    begsr Insertar_datos_SGPCLG;

    hora   = %dec(%time():*HMS);
      exec sql INSERT INTO SGPCLG
                           (LGISYS,
                            LGFECH,
                            LGHORA,
                            LGIUSR,
                            LGDPL1,
                            LGDPL2)

                     VALUES(
                           :nombre_sistema,
                           :aasfei,
                           :hora,
                           :usuario,
                           :nombre_archivo,
                           :descripcion_cmd);
    endsr;

//==============================================================================
// Verifico si el archivo ya esta procesado
//==============================================================================
    begsr verifico_LISERJ;
    exec sql open C5;
    exec sql FETCH C5;
    IF SQLCOD = *ZERO;
     error_archivo_procesado = '1';
    ENDIF;
     exec sql close C5;
    endsr;

//==============================================================================
// Fin de programa
//==============================================================================
    begsr fin_de_programa;

//...Muestro pantalla error si no se pudo ejecutar el comando al servidor
     if error_conexion_servidor = '1';
     PANTALLA(
     '             ERROR CONEXION CON SERVIDOR                          ':
     '                                                                  ':
     'No se puedo realizar la comunicacion con el servidor.             ':
     '                                                                  ':
     '                                                                  ':
     '                                                                  ':
     'Verifique y reintente                                             ':
     '                                                                  ');
     return;
     ENDIF;

     return;

    endsr;
//==============================================================================
end-proc;

//==============================================================================
   //  Funcion para ejectuar comandos con sbmjob
//==============================================================================

dcl-proc ejecutar_sbmjob;

    dcl-pi *n char(7);
              cmd             char(4096);
              usuario_cert    char(10);
    end-pi;



    dcl-s sbmjob  char(MAX_FILE_SIZE);
    dcl-s rc                 char(7);

    sbmjob = 'SBMJOB CMD('+%trim(cmd)+') JOB(PRUEBA) USER('
                          +%trim(usuario_cert)+') OUTQ(*DEV)'
                          +' DSPSBMJOB(*NO) MSGQ(*NONE)';
    rc = exeCmd(sbmjob);

    return rc;

end-proc;

//==============================================================================
   //  Funcion para llamar a pantalla que muestra los mensajes
//==============================================================================

dcl-proc pantalla;

    dcl-pi *n;
              PANCO1     char(55) const;
              PANCO2     char(55) const;
              PANCO3     char(55) const;
              PANCO4     char(55) const;
              PANCO5     char(55) const;
              PANCO6     char(55) const;
              PANCO7     char(55) const;
              PANCO8     char(55) const;
    end-pi;

   ejecutar_programa_BAER00RS(PANCO1:
                              PANCO2:
                              PANCO3:
                              PANCO4:
                              PANCO5:
                              PANCO6:
                              PANCO7:
                              PANCO8);
end-proc;
/copy sdb01.src/qrpgsrc,ligdimpl ;
