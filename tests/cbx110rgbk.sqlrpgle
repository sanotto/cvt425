*SRCMBRTXT:Procesa base inicial empleados, emp y t
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
    dcl-s error_conexion_servidor char(1) inz('0');

    //... Llamada al programa 'SGAUTAC4'
   dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
          path_archivo      char(250) const;
   end-pr;


//Aca empieza el programa
exec sql set option commit=*None;
exec sql select aasfei into :aasfei from sgsysv;
exec sql select current_user into :usuario from sysibm.sysdummy1;
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

    exsr declara_cursores_para_consultas;

    cmd = 'CHGCURDIR DIR(''/home'')';

    exeCmd(cmd);
    exsr enviar_archivo_titulares_empresas_a_servidor_web;
    exsr enviar_archivo_padron_empleados_a_servidor_web;
    exsr fin_de_programa;

//==============================================================================
// Declaro los cursores para realizar las consultas.
//==============================================================================

    begsr declara_cursores_para_consultas;

//Consulta para saber si exite archivos de empleados
        exec sql DECLARE C2 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/acreditaciones')) AS T
         WHERE CAST (FILENAME as char(255)) like '%APSUE%'
         ORDER BY CAST (FILENAME as char(255)) ASC;

//Consulta a liserj
        exec sql DECLARE C3 CURSOR FOR

        SELECT * FROM LISERJ WHERE RJDACO = :nombre_archivo;


//Consulta para saber si exite de titulares y empresas
        exec sql DECLARE C4 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/acreditaciones')) AS T
         WHERE CAST (FILENAME as char(255)) like '%TISUE%' OR
               CAST (FILENAME as char(255)) like '%EMSUE%'
         ORDER BY CAST (FILENAME as char(255)) ASC;

 endsr;
//==============================================================================
// Ejecuto comando scp para enviar los archivo de padron de empleados al serv
//==============================================================================

    begsr enviar_archivo_padron_empleados_a_servidor_web;

    exec sql open C2;
    exec sql FETCH C2 INTO :nombre_archivo;

    // Pregunto si hay achivos con etiqueta APSUE
    dow SQLCOD = *ZERO;

    //exec sql open C3;
    //exec sql FETCH C3;

    // Pregunto si el achivo ya esta cargado en liserj
    //If SQLCOD <> *ZERO;

    //Envio archivo con las instituciones
    path_archivo = '/home/acreditaciones/'+ %trim(nombre_archivo);
    comando_scp = 'scp -P 998 '+ %TRIM(path_archivo) +
        ' ' + %TRIM(url)+ ':'+%trim(carpeta)+'/files_cubix/datos' +
            ' > /home/tmp/prueba.txt 2>&1' ;
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';
    RC = ejecutar_sbmjob(cmd:usuario_cert);

//...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;

    exeCmd('DLYJOB DLY(40)');

    descripcion_cmd = 'Envio Archivo de empleados al servidor';
    exsr Insertar_datos_SGPCLG;



    //Ejecuto el script en el servidor CUBIX
    exsr ejecutar_script_padron_empleados_php_en_servidor;

    //Muevo el archivo a hisotorico
    cmd = 'mv /home/acreditaciones/' +
    %trim(nombre_archivo) + ' /home/acreditaciones/HISTORICO/' +
    %trim(nombre_archivo) +'_'+ %char(aasfei);
    ejecutar_programa_SGAUTAC4(cmd);

    //Registro el archivo en LISERJ
    exsr escribo_liserj;

    //endif;

    //exec sql close C3;
    exec sql fetch C2 INTO :nombre_archivo;

    ENDDO;

    exec sql close C2;

    endsr;

//==============================================================================
// Ejecuto los script de padron de empleados en el servidor de CUBIX
//==============================================================================

    begsr ejecutar_script_padron_empleados_php_en_servidor;

    //Ejecuto script para insertar las instituciones.
    comando_ssh =  'ssh -p 998 ' + %TRIM(url)                    +
                ' "php '+%trim(carpeta)+'cubix/bin/agregaEmpleados.php" '   +
                  '> /home/tmp/prueba.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_ssh)+''')';
    rc = ejecutar_sbmjob(cmd:usuario_cert);

    //...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;

    descripcion_cmd = 'Ejecutar script para cargar los empleados';
    exsr Insertar_datos_SGPCLG;

    endsr;

//==============================================================================
// Ejecuto comando scp para enviar los archivos qbiemp y qbitit al servidor web
//==============================================================================

    begsr enviar_archivo_titulares_empresas_a_servidor_web;

    exec sql open C4;
    exec sql FETCH C4 INTO :nombre_archivo;

    // Pregunto si hay achivos con etiqueta TISUE O EMSUE
    dow SQLCOD = *ZERO;

    //exec sql open C3;
    //exec sql FETCH C3;

    // Pregunto si el achivo ya esta cargado en liserj
    //If SQLCOD <> *ZERO;

    if %subst(nombre_archivo :1 :5) = 'EMSUE';
    //Envio archivo con las instituciones
    path_archivo = '/home/acreditaciones/' + %trim(nombre_archivo);
    comando_scp = 'scp -P 998 '+ %TRIM(path_archivo) +'' +
         ' ' + %TRIM(url) + ':'+%trim(carpeta)+'files_cubix/datos' +
          ' > /home/tmp/prueba.txt 2>&1' ;
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';
    rc = ejecutar_sbmjob(cmd:usuario_cert);

   //...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;

    exeCmd('DLYJOB DLY(40)');

    descripcion_cmd ='Envio archivo '+%trim(nombre_archivo)+' a servidor CUBIX';
    exsr Insertar_datos_SGPCLG;

    //Ejecuto script en servidor para procesar archivo
    exsr ejecutar_script_titutalres_empresas_php_en_servidor;

    //Muevo el archivo a hisotorico
    cmd = 'mv /home/acreditaciones/' +
          %trim(nombre_archivo) + ' /home/acreditaciones/HISTORICO/'+
          %trim(nombre_archivo) +'_'+ %char(aasfei);
    ejecutar_programa_SGAUTAC4(cmd);

    //Registro el archivo en LISERJ
    exsr escribo_liserj;

    ELSE;

    //Envio archivo con los titulares
    path_archivo = '/home/acreditaciones/' + %trim(nombre_archivo);
    comando_scp = 'scp -P 998 '+ %TRIM(path_archivo) +'' +
        ' ' + %TRIM(url) + ':'+%trim(carpeta)+'files_cubix/datos' +
           ' > /home/tmp/prueba2.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_scp)+''')';
    rc = ejecutar_sbmjob(cmd:usuario_cert);

    //...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;

    exeCmd('DLYJOB DLY(40)');

    descripcion_cmd ='Envio archivo '+%trim(nombre_archivo)+' a servidor CUBIX';
    exsr Insertar_datos_SGPCLG;

    //Ejecuto script para procesar archivo
    exsr ejecutar_script_titutalres_empresas_php_en_servidor;

    //Muevo el archivo a hisotorico
    cmd = 'mv /home/acreditaciones/' +
                %trim(nombre_archivo) + ' /home/acreditaciones/HISTORICO/'+
                %trim(nombre_archivo) +'_'+ %char(aasfei);
    ejecutar_programa_SGAUTAC4(cmd);

    //Registro el archivo en LISERJ
    exsr escribo_liserj;

    ENDIF;

   // endif;

   // exec sql close C3;
    exec sql fetch C4 INTO :nombre_archivo;

    ENDDO;

     exec sql close C4;

    endsr;

//==============================================================================
// Ejecuto los script para procesar archivos titulares y empresas
//==============================================================================

    begsr ejecutar_script_titutalres_empresas_php_en_servidor;

    if %subst(nombre_archivo :1 :5) = 'EMSUE';

    //Ejecuto script para insertar las instituciones.
    comando_ssh = 'ssh -p 998 ' +  %TRIM(url)    +
               ' "php '+%trim(carpeta)+'cubix/bin/agregaInstitucion.php" ' +
                  '> /home/tmp/prueba.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_ssh)+''')';
    rc = ejecutar_sbmjob(cmd:usuario_cert);

    //...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;

    descripcion_cmd = 'Ejecuto script agregaInstitucion.php';
    exsr Insertar_datos_SGPCLG;

    else;

    //Ejecuto script para insertar los titulares.

     //Ejecuto script para insertar las instituciones.
    comando_ssh = 'ssh -p 998 ' + %TRIM(url)    +
                  ' "php '+%trim(carpeta)+'cubix/bin/procesaFirmantes.php" ' +
                  '> /home/tmp/prueba.txt 2>&1';
    cmd = 'QSH CMD('''+%trim(comando_ssh)+''')';
    rc =  ejecutar_sbmjob(cmd:usuario_cert);

   //...Muevo 1 a error_conexion_servidor si no se ejecuto de manera correcta
    IF rc <> 'CPF0000';
       error_conexion_servidor = '1';
       exsr fin_de_programa;
    ENDIF;

    descripcion_cmd = 'Ejecuto script procesaFirmantes.php';
    exsr Insertar_datos_SGPCLG;

    endif;

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
   RC =  exeCmd(sbmjob);

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
