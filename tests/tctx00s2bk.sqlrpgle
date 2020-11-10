*SRCMBRTXT:Generar Archiv .CSV - CORREO MASIVO TAS
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

//==============================================================================
//Declaración de prototipos, importadas de los miembros declarados.
//==============================================================================

//Max Buffer
dcl-c MAX_FILE_SIZE 65535;

/include sdb01.src/qrpgsrc,ligdifs ;
/include sdb01.src/qrpgsrc,ligdprot ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

    //Entry plist
    dcl-ds c1_ds qualified;
        filename                varchar(255);
    end-ds;

    dcl-s import_ok             ind;
    dcl-s path_to_out           varchar(255);
    dcl-s path_to_dir           varchar(255);
    dcl-s cmd                   varchar(4096);
    dcl-s rc                    char(7);
    dcl-s fd                    int(10);
    dcl-s fqn                   varchar(255) inz;
    dcl-s aasfei                packed(8:0);
    dcl-s nombre_liserj         char(30);
    dcl-s nombre_archivo        char(30);
    dcl-s verificar_correo      char(50);
    dcl-s verificar_correo_2    char(50);
    dcl-s cantidad_correos      packed(2:0);
    dcl-s exitencia_correos     packed(2:0);

    dcl-ds archivocsv           qualified;
            ape_nombre          char(30);
            correo              char(50);
    end-ds;

    path_to_dir = '/home/Tarjetas_de_Creditos/';

    exec sql SET OPTION COMMIT = *NONE;
    exec sql SELECT AASFEI into :aasfei FROM SGSYSV;

    exsr crear_tabla_trabajo;
    exsr verificar_archivo_procesado;
    exsr verificar_si_existen_correos_a_informar;
    exsr declarar_cursor;
    exsr abrir_archivo_csv;
    wrtToJobLog('Comienza Generación de archivo:'+fqn);
    exsr escribir_cabezera_archivo;
    exec sql open cursor_archivo;
    exec sql fetch cursor_archivo into :archivocsv;
    dow sqlcod = *Zero;
        if (verificar_correo_2 = archivocsv.correo);
            exec sql fetch cursor_archivo into :archivocsv;
        else;
        exsr escribir_detalles_archivo;
        exec sql fetch cursor_archivo into :archivocsv;
        endif;
    enddo;
    exec sql close cursor_archivo;
    exsr cerrar_archivo;
    exsr guardar_liserj;

    return;

//==============================================================================

    begsr crear_tabla_trabajo;

    exec sql DROP TABLE QTEMP.TCALCO; //Temporal para correos.

    exec sql CREATE TABLE QTEMP.TCALCO (correo CHAR(50)); //Creo Tabla temporal.

    exec sql DELETE FROM QTEMP.TCALCO; //Vaciar Tabla.

    endsr;

//==============================================================================

    begsr verificar_archivo_procesado;

    nombre_archivo = 'GCAX00309_' + %char(aasfei) +'_CORREO.csv';

    exec sql
            SELECT RJDACO
                INTO :nombre_liserj
            FROM LISERJ
            WHERE RJDACO = :nombre_archivo;

        if nombre_archivo = nombre_liserj;

        //...Imprimo mensaje de error en pantalla de sideba.

        die('El Archivo '+%Trim(nombre_archivo)+', ya fué ' +
            'procesado anteriormente por favor verifique y' +
            ' reintente nuevamente.                       ');

        endif;

    endsr;

//==============================================================================

    begsr verificar_si_existen_correos_a_informar;

        exec sql SELECT
                    COUNT(*)
                 into: exitencia_correos
                 FROM TCPRT0
                 WHERE TXFASI = 0;

        if exitencia_correos = 0;

        //Imprimo mensaje de error en pantalla de sideba.

        die('No existen correos a informar.');

        endif;


    endsr;

//==============================================================================

    begsr declarar_cursor;

        exec sql declare cursor_archivo cursor for
                    SELECT
                        TXNYAP,
                        TXCOEL
                    FROM TCPRT0
                    WHERE TXFASI = 0;

    endsr;

//==============================================================================

    begsr abrir_archivo_csv;

        fqn = %Trim(path_to_dir) + %Trim(nombre_archivo);

        fd = ifs_stmf_opnwrt(fqn);
        if (fd < 0);
           die('No se pudo abrir archivo:'+fqn);
        endif;

    endsr;

//==============================================================================

    begsr escribir_cabezera_archivo;

    ifs_wrtln(fd:'Nombre y Apellido;'               +
                 'Direccion de correo electronico'  );

    endsr;

//==============================================================================

    begsr escribir_detalles_archivo;

        exec sql SELECT
                    COUNT(*)
                    TXCOEL
                 INTO :cantidad_correos, :verificar_correo
                 FROM TCPRT0
                 WHERE
                 TXCOEL = :archivocsv.correo AND TXFASI=0 AND
                 :archivocsv.correo NOT IN (SELECT correo FROM QTEMP.TCALCO)
                 ORDER BY TXCOEL;

        if (cantidad_correos = 1);

        ifs_wrtln(fd:''+%TRIM(archivocsv.ape_nombre)  +';' +
                     ''+%TRIM(archivocsv.correo)          );

        exec sql UPDATE TCPRT0 SET TXFASI = :aasfei
             WHERE TXNYAP = :archivocsv.ape_nombre AND
                   TXFASI = 0;

        exec sql INSERT INTO QTEMP.TCALCO(
                            correo)

                 VALUES(:archivocsv.correo);

        endif;

        if (cantidad_correos > 1);

        exec sql SELECT
                    TXCOEL
                 INTO :verificar_correo_2
                 FROM TCPRT0
                 WHERE
                 TXCOEL = :archivocsv.correo AND TXFASI=0
                 LIMIT 1;

        if (archivocsv.correo = verificar_correo_2);

        ifs_wrtln(fd:''+%TRIM(archivocsv.ape_nombre)  +';' +
                     ''+%TRIM(archivocsv.correo)          );

        exec sql UPDATE TCPRT0 SET TXFASI = :aasfei
             WHERE TXNYAP = :archivocsv.ape_nombre AND
                   TXFASI = 0;

        exec sql INSERT INTO QTEMP.TCALCO(
                            correo)

                 VALUES(:archivocsv.correo);

        endif;

        endif;

    endsr;

//==============================================================================

    begsr cerrar_archivo;

        ifs_stmf_close(fd);

    endsr;

//==============================================================================

    begsr guardar_liserj;

        exec sql
            INSERT INTO LISERJ
             (
              RJDACO,
              RJCAN1,
              RJ$TOT,
              RJFALT,
              RJIUSR,
              RJHORA,
              RJITAC
             )
             values
             (
              TRIM(:nombre_archivo),
              0,
              0,
              (SELECT AASFEI FROM SGSYSV LIMIT 1),
              CURRENT_USER,
              (SELECT HOUR(NOW())*10000+MINUTE(NOW())*100 +SECOND(NOW())
               FROM  SYSIBM/SYSDUMMY1 ),
              0
             );

    endsr;

//==============================================================================

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
