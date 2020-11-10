*SRCMBRTXT:Envia y procesa archivos respuesta - CU
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

dcl-proc main;

// *entry plist
    dcl-pi *n;
         operacion   char(10) ;
    end-pi;

//... Variables
    dcl-s cmd                char(4096);
    dcl-s rc                 char(7);
    dcl-s comando_scp        char(4096);
    dcl-s comando_ssh        char(4096);
    dcl-s archivo_qbitemp    char(255);
    dcl-s archivo_qbitit     char(255);
    dcl-s path_archivo       char(250);
    dcl-s nombre_archivo     char(255);
    dcl-s usuario            char(10);
    dcl-s descripcion_cmd    char(49);
    dcl-s nombre_sistema     char(8) inz('CUBIX');
    dcl-s hora               packed(6:0);
    dcl-s aasfei             packed(8:0);
    dcl-s host_name          char(250);
    dcl-s url                char(50);
    dcl-s usuario_cert       char(10);
    dcl-s carpeta            char(30);
    dcl-s error_archivo_procesado char(1);
    dcl-s error_conexion_servidor char(1) inz('0');

//... Llamada al programa 'SGAUTAC4'
   dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
          path_archivo      char(250) const;
   end-pr;

//=======================Aca empieza el programa==============================//
    exsr consultas_parametros;
    exsr recupero_nombre_maquina_sideba;
    exsr ejecutar_comando_inicial;
    exsr declara_cursores_para_consultas;
//...Pregunto si es producto SUELDO o TEI
   Select;
    When %TRIM(operacion)  = 'SUELDO';
    exsr enviar_archivo_respuesta_SUELDO_servidor_web;

    When %TRIM(operacion)  = 'TEI';
    exsr enviar_archivo_respuesta_TEI_servidor_web;
    ENDSL;
    exsr fin_de_programa;

//==============================================================================
// Consultas para parametros necesarios.
//==============================================================================
    begsr consultas_parametros;
     exec sql set option commit=*None;
     exec sql select aasfei into :aasfei from sgsysv;
     exec sql select current_user into :usuario from sysibm.sysdummy1;
    endsr;

//==============================================================================
// Consultas para parametros necesarios.
//==============================================================================
    begsr recupero_nombre_maquina_sideba;
//...Recuper nombre del servidor donde se esta ejecutando el programa
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
//...Me posiciono en el directorio raiz /home
     cmd = 'CHGCURDIR DIR(''/home'')';
     exeCmd(cmd);

    endsr;

//==============================================================================
// Declaro los cursores para realizar las consultas.
//==============================================================================

  begsr declara_cursores_para_consultas;

//...Consulta para verificar si existe respuesta para SUELDO
        exec sql DECLARE C2 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/acreditaciones')) AS T
         WHERE CAST (FILENAME as char(255)) like '%RESUE%'
         ORDER BY 1;

//...Consulta a liserj
        exec sql DECLARE C3 CURSOR FOR

        SELECT * FROM LISERJ WHERE RJDACO = :nombre_archivo;

//...Consulta para verificar si existe respuesta para TEI
        exec sql DECLARE C4 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/TEI/Archivos_Respuesta')) AS T
         WHERE CAST (FILENAME as char(255)) like '%RETEI%'
         ORDER BY 1;

//...Consulta a liserj
        exec sql DECLARE C5 CURSOR FOR
        SELECT * FROM LISERJ WHERE
        RJDACO
         =
        :nombre_archivo;

 endsr;
//==============================================================================
// Ejecuto comando scp para enviar los archivos respuesta SUELDO
//==============================================================================
    begsr enviar_archivo_respuesta_SUELDO_servidor_web;

//...Pregunto si hay achivos con etiqueta RESUE
    exec sql open C2;
    exec sql FETCH C2 INTO :nombre_archivo;

    dow SQLCOD = *ZERO;

//...Verifico que el achivo a enviar no este en liserj, si esta leo el sig y
//...muevo a historico.
    error_archivo_procesado = '0';
    exsr verifico_LISERJ;
//...Si error es = 1 leo el siguiente archivo y vuelvo el bucle
    if error_archivo_procesado = '1';
//...Muevo archivo a HISTORICOS
    hora   = %dec(%time():*HMS);
    cmd = 'mv -i /home/acreditaciones/' +
                %trim(nombre_archivo) + ' /home/acreditaciones/HISTORICO/'+
                %trim(nombre_archivo) +'_'+%char(aasfei)+'_'+ %char(hora);
    ejecutar_programa_SGAUTAC4(cmd);

//...Leo el siguiente archivo
           exec sql fetch C2 INTO :nombre_archivo;
           iter;
    endif;

//============================================================================//

//...Pregunto si el achivo ya esta cargado en liserj
    exec sql open C3;
    exec sql FETCH C3;

    If SQLCOD <> *ZERO;

//...Envio archivo respuesta y ejecuto comando con un sbmjob
    path_archivo = '/home/acreditaciones/'+ %trim(nombre_archivo);
    comando_scp = 'scp -P 998 '+ %TRIM(path_archivo) +
    ' '+%TRIM(url)+':'+%trim(carpeta)+'files_cubix/pendientes/sueldos/'+
          ' > /home/tmp/prueba.txt 2>&1' ;
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';
    rc = ejecutar_sbmjob(cmd:usuario_cert);

//...Valido si tiro error la ejecucion del comando
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;
    exeCmd('DLYJOB DLY(60)');

//...Escribo archivo SGPCLG para tener un historial
    descripcion_cmd = 'Envio Archivo respuesta SUELDO al servidor';
    exsr Insertar_datos_SGPCLG;

//...Ejecuto script en servidor CUBIX para procesar la respuesta.
    exsr ejecutar_script_php_en_servidor;
    exeCmd('DLYJOB DLY(20)');

//...Cambio grupo para el archivo en la carpeta acreditaciones
    ejecutar_programa_SGAUTAC4(' chown GR00001 -R  /home/acreditaciones/'+
                               %trim(nombre_archivo) );

//...Muevo archivo a HISTORICOS
    cmd = 'mv -i /home/acreditaciones/' +
                %trim(nombre_archivo) + ' /home/acreditaciones/HISTORICO/'+
                %trim(nombre_archivo) +'_'+ %char(aasfei);
    ejecutar_programa_SGAUTAC4(cmd);

//...Escribo nombre achivo en liserj
    exsr escribo_liserj;

    endif;

    exec sql close C3;
    exec sql fetch C2 INTO :nombre_archivo;

    ENDDO;

    exec sql close C2;

    endsr;

//==============================================================================
// Ejecuto comando scp para enviar los archivos respuesta TEI
//==============================================================================
    begsr enviar_archivo_respuesta_TEI_servidor_web;

    exec sql open C4;
    exec sql FETCH C4 INTO :nombre_archivo;

//...Pregunto si hay achivos con etiqueta RETEI
    dow SQLCOD = *ZERO;

//...Verifico que el achivo a enviar en liserj.
    error_archivo_procesado = '0';
    exsr verifico_LISERJ;
//...Si error es = 1 leo el siguiente archivo y vuelvo el bucle
    if error_archivo_procesado = '1';
//...Muevo archivo
    hora   = %dec(%time():*HMS);
    cmd = 'mv -i /home/TEI/Archivos_Respuesta/' +
                %trim(nombre_archivo) + ' /home/TEI/Archivos_Procesados/'+
                %trim(nombre_archivo) +'_'+%char(aasfei)+'_'+%char(hora);
    ejecutar_programa_SGAUTAC4(cmd);
           exec sql fetch C4 INTO :nombre_archivo;
           iter;
    endif;

//============================================================================//

    exec sql open C3;
    exec sql FETCH C3;
//...Pregunto si el achivo ya esta cargado en liserj
    If SQLCOD <> *ZERO;

//...Envio archivo con las instituciones y ejecuto comando con un sbmjob
    path_archivo = '/home/TEI/Archivos_Respuesta/'+ %trim(nombre_archivo);
    comando_scp = 'scp -P 998 '+ %TRIM(path_archivo) +
    ' '+ %TRIM(url)+':'+%trim(carpeta)+'files_cubix/pendientes/tei/' +
          ' > /home/tmp/prueba.txt 2>&1' ;
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';

//...Valido si tiro error en ejecucion de comando
    rc = ejecutar_sbmjob(cmd:usuario_cert);
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;
    exeCmd('DLYJOB DLY(40)');

//...Escribo archivo SGPCLG para historico
    descripcion_cmd = 'Envio Archivo respuesta TEI al servidor';
    exsr Insertar_datos_SGPCLG;

//...Ejecuto script para enviar al servidor
    exsr ejecutar_script_php_en_servidor;
    exeCmd('DLYJOB DLY(20)');

//...Escribo nombre archivo procesado en liserj
    exsr escribo_liserj;

  ejecutar_programa_SGAUTAC4(' chown GR00001 -R  /home/TEI/Archivos_Respuesta/'+
                               %trim(nombre_archivo) );

//...Muevo archivo
    cmd = 'mv -i /home/TEI/Archivos_Respuesta/' +
                %trim(nombre_archivo) + ' /home/TEI/Archivos_Procesados/'+
                %trim(nombre_archivo) +'_'+ %char(aasfei);
    ejecutar_programa_SGAUTAC4(cmd);

    endif;

    exec sql close C3;
    exec sql fetch C4 INTO :nombre_archivo;

    ENDDO;

    exec sql close C4;

    endsr;

//==============================================================================
// Ejecuto los script necesarios en el servidor de CUBIX
//==============================================================================

    begsr ejecutar_script_php_en_servidor;

    if %subst(nombre_archivo :1 :5) = 'RESUE';

//...Ejecuto script para procesar la respuesta de SUELDOS.
    comando_ssh = 'ssh -p 998 ' + %TRIM(url)          +
           ' "php '+%trim(carpeta)+'cubix/bin/procesarRespuestaSueldo.php" ' +
                  '> /home/tmp/prueba.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_ssh)+''')';
    ejecutar_sbmjob(cmd:usuario_cert);
//...exeCmd(cmd);
    descripcion_cmd = 'Ejecutar script para procesar respuesta SUELDO';
    exsr Insertar_datos_SGPCLG;

    ELSE;

//...Ejecuto script para procesar la respuesta de TEI.
    comando_ssh = 'ssh -p 998 '+ %TRIM(url)      +
            ' "php '+%trim(carpeta)+'cubix/bin/procesarRespuestaTEI.php" ' +
                  '> /home/tmp/prueba.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_ssh)+''')';
    ejecutar_sbmjob(cmd:usuario_cert);

    descripcion_cmd = 'Ejecutar script para procesar respuesta TEI';
    exsr Insertar_datos_SGPCLG;

    ENDIF;

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
     //...Envio mail por error en conexion con el servidor.
     cmd = 'SNDMAIL  RECP(''QBQZ00MB'') '                            +
                'SUBJECT(''Cubix: Error en conexion - Servidor'') '   +
                'MESG(''No se puedo enviar el achivo de respuesta '   +
                'al servidor. Verifique y reintente'') ';

     exeCmd(cmd);
     return;
     ENDIF;

        return;
    endsr;

//==============================================================================
// Ejecuto comando para levantar cubix
//==============================================================================

    begsr ejecutar_comando_inicial;

    comando_scp = 'scp -P 998 /home/tmp/prueba.txt' +
    ' '+%TRIM(url)+':'+%trim(carpeta)+'files_cubix/datos'+
          ' > /home/tmp/prueba.txt 2>&1' ;
    cmd = 'QSH CMD('''+%trim(comando_ssh)+''')';


    rc = ejecutar_sbmjob(cmd:usuario_cert);

    exeCmd('DLYJOB DLY(30)');

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
// Escribo LISERJ
//==============================================================================

    begsr escribo_liserj;

    hora   = %dec(%time():*HMS);

       exec sql INSERT INTO LISERJ
                           (RJDACO,
                            RJFALT,
                            RJIUSR,
                            RJHORA)

                     VALUES(
                           :nombre_archivo,
                           :aasfei,
                           :usuario,
                           :hora);
    endsr;

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
/copy sdb01.src/qrpgsrc,ligdimpl ;
