*SRCMBRTXT:Manejador Peditutarjeta                
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

    //Llamada al Programa LITA20H8
    dcl-pr LITA20H8 extpgm('LITA20H8');
        WWNTAR             packed(16:0);
    end-pr;

    //Llamada al Programa LITA20H7
    dcl-pr LITA20H7 extpgm('LITA20H7');
        WWNTAR             packed(16:0);
    end-pr;

    //Llamada al Programa LITA20H6
    dcl-pr LITA20H6 extpgm('LITA20H6');
        WWITAR             packed(16:0);
    end-pr;

    //Llamada al Programa SGAUTAC4
    dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
       path_archivo      char(250) const;
    end-pr;

    dcl-s dest_name             varchar(255);
    dcl-s full_path             varchar(255);
    dcl-s sufijo                varchar(255);

    dcl-s import_ok             ind;
    dcl-s path_to_out           varchar(255);
    dcl-s path_to_dir           varchar(255);
    dcl-s cmd                   varchar(4096);
    dcl-s rc                    char(7);
    dcl-s fd                    int(10);
    dcl-s fqn                   varchar(255) inz;
    dcl-s path_archivo          char(250);

    dcl-s arc_no_clientes       char(255);
    dcl-s arc_tds_exisntentes   char(255);
    dcl-s arc_tds_altas         char(255);

    // Varibles para archivo de No Clientes.
    dcl-s documento_cliente     packed(15:0);

    // Variables para archivo TDS Existentes.
    dcl-s numero_tar            char(16);
    dcl-s suc_tar               char(5);
    dcl-s cuenta_tar            char(9);
    dcl-s dni_cuenta            packed(15:0);
    dcl-s est_tarjeta           char(1);
    dcl-s numero_tarjeta        packed(16:0);
    dcl-s suc_tarjeta           packed(1:0);
    dcl-s cuenta_tarjeta        packed(9:0);

    // Variable Global para Documento.
    dcl-s doc_global            char(15);
    dcl-s nombre_liserj         char(30);

    //Variables Altas TDS.
    dcl-s cuenta_td             char(9);
    dcl-s grupo_td              char(2);
    dcl-s sub_grupo_td          char(2);
    dcl-s WWNTAR                packed(16:0);
    dcl-s WWITAR                packed(16:0);
    dcl-s aasfei                packed(8:0);

    //Cursor Tarjetas Validas
    dcl-s td_dni                packed(15:0);
    dcl-s td_cuenta             char(9);
    dcl-s td_baja               packed(8:0);
    dcl-s td_grupo              char(1);
    dcl-s td_suc                packed(5:0);
    dcl-s td_tdo                packed(2:0);

    dcl-ds no_clientes qualified;
            marca_temporal   varchar(50);
            apellido         varchar(50);
            nombre           varchar(50);
            dni              packed(15:0);
            correo           varchar(50);
            telefono         varchar(15);
            empresa          varchar(15);
            calle            varchar(30);
            numeracion       varchar(15);
            barrio           varchar(30);
            codigo_postal    varchar(4);
            ciudad           varchar(50);
            localidad        varchar(50);
    end-ds;

    dcl-ds clientes qualified;
            marca_temporal   varchar(50);
            apellido         varchar(50);
            nombre           varchar(50);
            dni              packed(15:0);
            correo           varchar(50);
            telefono         varchar(15);
            empresa          varchar(15);
            calle            varchar(30);
            numeracion       varchar(15);
            barrio           varchar(30);
            codigo_postal    varchar(4);
            ciudad           varchar(50);
            localidad        varchar(50);
    end-ds;

    //variables para formatear Strings
    dcl-c up              'ABCDEFGHIJKLMNNOPQRSTUVWXYZ'      ;
    dcl-c lo              'abcdefghijklmn¦opqrstuvwxyz'      ;
    dcl-c emp             '                           '      ;
    dcl-c Symbols         '|°¬!"#$%&/()=?\¡¿*+~¢]{}_-;,:.<>' ;
    dcl-c SymBlanks       '                                ' ;
    dcl-c Acentos         'ñÑáéíóúäëïöüãõàèìòùâêîôû@'        ;
    dcl-c AceBlanks       'nNAEIOUAEIOUAOAEIOUAEIOU '        ;
    dcl-c AceEmpty        '                         '        ;
    dcl-c Apos            ''''                               ;
    dcl-c APosBlank       ' '                                ;

    path_to_dir = '/home/PR00705/';
    path_to_out = '/home/PR00705/Historicos/';

    arc_no_clientes = 'PediTuTarjeta-No-Clientes';
    arc_tds_exisntentes = 'PediTuTarjeta-TDS-Existentes';
    arc_tds_altas = 'PediTuTarjeta-Alta-TDS';

    exec sql SET OPTION COMMIT = *NONE;

    exsr crea_sufijo_de_archivo_ya_procesado;
    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    dow  sqlcod = *zero;
         exsr verificar_archivo_procesado;
         exsr armar_paths;
         exsr limpiar_tablas_de_trabajo;
         exsr importar_archivo_csv;
         if import_ok;
            exsr declarar_cursor;
            exsr verificar_si_es_cliente;
            exsr verificar_si_posee_tds;
            //exsr actulizar_datos;
            exsr generar_alta_tds;
            exsr gener_informe_alta_tds;
            //exsr guardar_liserj;
            //exsr mover_archivo_a_ya_procesado;
         endif;
         exsr dir_list_fetch;
    enddo;
    exsr dir_list_close;
    return;

//==============================================================================

    begsr crea_sufijo_de_archivo_ya_procesado;

    exec sql SELECT
                    TRANSLATE(
                            CAST ( timestamp(now()) as char(19))
                                   , '__','.-')
                    INTO :sufijo
                 FROM SYSIBM/SYSDUMMY1;

    endsr;

//==============================================================================

    begsr crear_tablas_de_trabajo;

        exec sql DROP TABLE QTEMP.ACTDCO;

        exec sql CREATE TABLE QTEMP.ACTDCO (
                        marca_temporal      CHAR(50),
                        correo              CHAR(50),
                        apellido            CHAR(50),
                        nombre              CHAR(50),
                        dni                 CHAR(50),
                        calle               CHAR(50),
                        numeracion          CHAR(50),
                        barrio              CHAR(50),
                        telefono            CHAR(50),
                        empresa             CHAR(50),
                        codigo_postal       CHAR(50),
                        ciudad              CHAR(50),
                        localidad           CHAR(50));

    endsr;

//==============================================================================

    begsr dir_list_open;

        exec sql DECLARE C1 CURSOR FOR
                SELECT
                    CAST (FILENAME as char(255)) AS FILNAM
                FROM TABLE(IFSDIR(:path_to_dir)) AS T
                WHERE
                    UPPER(FILENAME) LIKE '%TARJETA%';

        exec sql OPEN C1;

    endsr;

//==============================================================================

    begsr verificar_archivo_procesado;

    exec sql
            SELECT RJDACO
                INTO :nombre_liserj
            FROM LISERJ
            WHERE RJDACO = :c1_ds.filename;

        if c1_ds.filename = nombre_liserj;

        exsr dir_list_close;

        //...Imprimo mensaje de error en pantalla de sideba

        die('El Archivo '+%Trim(c1_ds.filename)+', ya fué ' +
            'procesado anteriormente por favor verifique y' +
            ' reintente nuevamente.                       ');

        endif;

    endsr;

//==============================================================================

    begsr dir_list_fetch;

        exec sql FETCH C1 into :c1_ds;

    endsr;

//==============================================================================

    begsr armar_paths;

        dest_name = %Trim(path_to_out)+
                    %Trim(c1_ds.filename)+
                    '_'+
                    %Trim(sufijo);

        full_path = %trim(path_to_dir)+%trim(c1_ds.filename);

    endsr;

//==============================================================================

    begsr limpiar_tablas_de_trabajo;

        exec sql DELETE FROM QTEMP.ACTDCO;

    endsr;

//==============================================================================

    begsr importar_archivo_csv;

        import_ok = *off;

        //Cambio ccsid a 1208 del archivo .csv
        cmd =       'CHGATR OBJ('''+%Trim(full_path)+''') ' +
                    'ATR(*CCSID) VALUE(1208)';
        rc = execmd(cmd);

        rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '           TOFILE(QTEMP/ACTDCO)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*CRLF)                                   ' +
                '           FLDDLM('','')                                   ' );

        //Eliminar cabeceras.
        if rc = 'CPF0000' ;

            exec sql
                DELETE FROM QTEMP.ACTDCO WHERE RRN(ACTDCO) = 1;

            import_ok = *on;

        endif;

    endsr;

//==============================================================================

    begsr declarar_cursor;


    //Cursor Clientes
    exec sql declare cursor_clientes cursor for
            SELECT DISTINCT
            marca_temporal,
            apellido,
            nombre,
            CAST(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            dni, '.', ''), ' ', ''), 'M', ''), 'F', ''), 'LE', ''), 'LC',
            ''), 'f', ''), 'm', ''), 'él', ''), 'cha', '')
            AS DEC(15, 0)) as dni,
            correo,
            CASE
                WHEN Substring(telefono, 1, 5) = '38015' THEN
                Substring(telefono, 1, 3) || '-' || Substring(telefono, 6)
                WHEN Substring(telefono, 1, 6) = '380415' THEN
                Substring(telefono, 1, 3) || '-' || Substring(telefono, 4, 1) ||
                Substring(telefono, 7)
                WHEN Substring(telefono, 1, 6) = '038015' THEN
                Substring(telefono, 2, 3) || '-' || Substring(telefono, 7)
                WHEN Substring(telefono, 1, 3) = '380' THEN
                Substring(telefono, 1, 3) || '-' ||Substring(telefono, 4)
                WHEN Substring(telefono, 1, 4) = '0380' THEN
                Substring(telefono, 2, 3) || '-' || Substring(telefono, 5)
                WHEN Substring(telefono, 1, 6) = '382515' OR
                Substring(telefono, 1, 6) = '382615' OR
                Substring(telefono, 1, 6) = '382715' THEN
                Substring(telefono, 1, 4) || '-' || Substring(telefono, 7)
                WHEN Substring(telefono, 1, 1) = '4' AND codigo_postal = 5300
                THEN '380-' || telefono
                WHEN Substring(telefono, 1, 3) = '382' THEN
                Substring(telefono, 1, 4) || '-' ||Substring(telefono, 5)
                WHEN Substring(telefono, 1, 4) = '0382' THEN
                Substring(telefono, 2, 4) || '-' ||Substring(telefono, 6)
            ELSE telefono END AS telefono,
            empresa,
            calle,
            numeracion,
            barrio,
            codigo_postal,
            ciudad,
            localidad
            FROM QTEMP.ACTDCO
            INNER JOIN BAPFIS ON A#INDO =
            CAST(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            dni, '.', ''), ' ', ''), 'M', ''), 'F', ''), 'LE', ''), 'LC',
            ''), 'f', ''), 'm', ''), 'él', ''), 'cha', '') AS DEC(15, 0));

    //Cursor No Clientes
    exec sql declare cursor_no_clientes cursor for
            SELECT
            marca_temporal,
            apellido,
            nombre,
            CAST(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            dni, '.', ''), ' ', ''), 'M', ''), 'F', ''), 'LE', ''), 'LC',
            ''), 'f', ''), 'm', ''), 'él', ''), 'cha', '')
            AS DEC(15, 0)) as dni,
            correo,
            CASE
                WHEN Substring(telefono, 1, 5) = '38015' THEN
                Substring(telefono, 1, 3) || '-' || Substring(telefono, 6)
                WHEN Substring(telefono, 1, 6) = '380415' THEN
                Substring(telefono, 1, 3) || '-' || Substring(telefono, 4, 1) ||
                Substring(telefono, 7)
                WHEN Substring(telefono, 1, 6) = '038015' THEN
                Substring(telefono, 2, 3) || '-' || Substring(telefono, 7)
                WHEN Substring(telefono, 1, 3) = '380' THEN
                Substring(telefono, 1, 3) || '-' ||Substring(telefono, 4)
                WHEN Substring(telefono, 1, 4) = '0380' THEN
                Substring(telefono, 2, 3) || '-' || Substring(telefono, 5)
                WHEN Substring(telefono, 1, 6) = '382515' OR
                Substring(telefono, 1, 6) = '382615' OR
                Substring(telefono, 1, 6) = '382715' THEN
                Substring(telefono, 1, 4) || '-' || Substring(telefono, 7)
                WHEN Substring(telefono, 1, 1) = '4' AND codigo_postal = 5300
                THEN '380-' || telefono
                WHEN Substring(telefono, 1, 3) = '382' THEN
                Substring(telefono, 1, 4) || '-' ||Substring(telefono, 5)
                WHEN Substring(telefono, 1, 4) = '0382' THEN
                Substring(telefono, 2, 4) || '-' ||Substring(telefono, 6)
            ELSE telefono END AS telefono,
            empresa,
            calle,
            numeracion,
            barrio,
            codigo_postal,
            ciudad,
            localidad
            FROM QTEMP.ACTDCO
            LEFT JOIN BAPFIS ON A#INDO =
            CAST(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            dni, '.', ''), ' ', ''), 'M', ''), 'F', ''), 'LE', ''), 'LC',
            ''), 'f', ''), 'm', ''), 'él', ''), 'cha', '') AS DEC(15, 0))
            WHERE
            A#INDO IS NULL;

    //Cursor Tarjetas Validas
    exec sql declare tarjeta_valida cursor for
                    SELECT DISTINCT
                            TAINDO,
                            TAICCL,
                            TAFBAJ,
                            TAIETA,
                            FUIGRC,
                            FUISGC,
                            FUISUC,
                            OTITDO
                     FROM LIKMTR
                     LEFT JOIN BADCCL ON  OTICCL = TAICCL AND
                                          OTISUC = TAISUC AND
                                          OTITDO = TAITDO AND
                                          OTINDO = TAINDO
                     INNER JOIN ACCTAC ON FUICAH = TAICCL AND
                                          FUISUC = OTISUC
                     WHERE
                     FUFBAJ = 0 AND
                     OTINDO = :clientes.dni;

    endsr;

//==============================================================================

    begsr verificar_si_es_cliente;

         exec sql open cursor_no_clientes;
         exec sql fetch  cursor_no_clientes into :no_clientes;
         exsr abrir_archivo_no_clientes;
         exsr escribir_cabezera_archivo_no_clientes;
             dow sqlcod = *Zero;
                 exsr escribir_detalles_archivo_no_cliente;
                 exec sql fetch  cursor_no_clientes into :no_clientes;
             enddo;
         exec sql close cursor_no_clientes;


    endsr;

//==============================================================================

    begsr abrir_archivo_no_clientes;

        ejecutar_programa_SGAUTAC4(' chown GR00001 -R '+%trim(path_to_out)+'');

        fqn = %Trim(path_to_out) + %Trim(arc_no_clientes) + '_' + %Trim(sufijo)+
              '.csv';

        fd = ifs_stmf_opnwrt(fqn);
        if (fd < 0);
           exec sql close cursor_no_clientes;
           exsr cerrar_archivo_no_clientes;
           exsr dir_list_close;
           die('No se pudo abrir archivo:'+fqn);
        endif;

    endsr;

//==============================================================================

    begsr escribir_cabezera_archivo_no_clientes;

    ifs_wrtln(fd:'Marca temporal,'                  +
                 'Dirección de correo electrónico,' +
                 'Apellido,'                        +
                 'Nombre,'                          +
                 'DNI,'                             +
                 'Calle,'                           +
                 'Numeración,'                      +
                 'Barrio,'                          +
                 'Teléfono,'                        +
                 'Código Postal,'                   +
                 'Ciudad,'                          +
                 'Localidad,'                       +
                 'Observación'                     );

    endsr;

//==============================================================================

    begsr escribir_detalles_archivo_no_cliente;

    doc_global = %char(no_clientes.dni);

    ifs_wrtln(fd:''+%TRIM(no_clientes.marca_temporal)  +',' +
                 ''+%TRIM(no_clientes.correo)          +',' +
                 ''+%TRIM(no_clientes.apellido)        +',' +
                 ''+%TRIM(no_clientes.nombre)          +',' +
                 ''+%TRIM(doc_global)                  +',' +
                 ''+%TRIM(no_clientes.calle)           +',' +
                 ''+%TRIM(no_clientes.numeracion)      +',' +
                 ''+%TRIM(no_clientes.barrio)          +',' +
                 ''+%TRIM(no_clientes.telefono)        +',' +
                 ''+%TRIM(no_clientes.codigo_postal)   +',' +
                 ''+%TRIM(no_clientes.ciudad)          +',' +
                 ''+%TRIM(no_clientes.localidad)       +',' +
                 'No es Cliente'                        );

    endsr;

//==============================================================================

    begsr cerrar_archivo_no_clientes;

        ifs_stmf_close(fd);

    endsr;

//==============================================================================

    begsr verificar_si_posee_tds;

         exec sql open cursor_clientes;
         exec sql fetch  cursor_clientes into :clientes;
         exsr abrir_archivo_tds_existentes;
         exsr escribir_cabezera_archivo_tds_existentes;
         dow sqlcod = *Zero;
                 exsr escribir_detalles_tds_existentes;
                 exec sql fetch  cursor_clientes into :clientes;
         enddo;
         exec sql close cursor_clientes;
         exsr cerrar_archivo_tds_existentes;

    endsr;

//==============================================================================

    begsr abrir_archivo_tds_existentes;

        ejecutar_programa_SGAUTAC4(' chown GR00001 -R '+%trim(path_to_out)+'');

        fqn = %Trim(path_to_out) + %Trim(arc_tds_exisntentes) + '_' +
              %Trim(sufijo) + '.csv';

        fd = ifs_stmf_opnwrt(fqn);
        if (fd < 0);
           exec sql close cursor_clientes;
           exsr cerrar_archivo_tds_existentes;
           exsr dir_list_close;
           die('No se pudo abrir archivo:'+fqn);
        endif;

    endsr;

//==============================================================================

    begsr escribir_cabezera_archivo_tds_existentes;

    ifs_wrtln(fd:'Marca temporal,'                  +
                 'Dirección de correo electrónico,' +
                 'Apellido,'                        +
                 'Nombre,'                          +
                 'DNI,'                             +
                 'Calle,'                           +
                 'Numeración,'                      +
                 'Barrio,'                          +
                 'Teléfono,'                        +
                 'Código Postal,'                   +
                 'Ciudad,'                          +
                 'Localidad,'                       +
                 'Estado Tarjeta,'                  +
                 'Numero Tarjeta,'                  +
                 'Sucursal,'                        +
                 'Cuenta'                          );

    endsr;

//==============================================================================

    begsr escribir_detalles_tds_existentes;

    exec sql
            SELECT
                OTINDO,
                TAIETA,
                TANTAR,
                TAISUC,
                TAICCL
                INTO :dni_cuenta, :est_tarjeta, :numero_tarjeta, :suc_tarjeta,
                :cuenta_tarjeta
            FROM BADCCL
            INNER JOIN ACCTAC ON FUICAH = OTICCL AND FUISUC = OTISUC
            INNER JOIN LIKMTR ON TAICCL = FUICAH AND TAISUC = FUISUC
            WHERE OTINDO = :clientes.dni AND
                  FUFBAJ = 0 AND
                  TAFBAJ = 0;

    numero_tar   = %char(numero_tarjeta);
    suc_tar      = %char(suc_tarjeta);
    cuenta_tar   = %char(cuenta_tarjeta);

    if (clientes.dni = dni_cuenta);

    doc_global = %char(clientes.dni);

    ifs_wrtln(fd:''+%TRIM(clientes.marca_temporal)   +',' +
                 ''+%TRIM(clientes.correo)           +',' +
                 ''+%TRIM(clientes.apellido)         +',' +
                 ''+%TRIM(clientes.nombre)           +',' +
                 ''+%TRIM(doc_global)                +',' +
                 ''+%TRIM(clientes.calle)            +',' +
                 ''+%TRIM(clientes.numeracion)       +',' +
                 ''+%TRIM(clientes.barrio)           +',' +
                 ''+%TRIM(clientes.telefono)         +',' +
                 ''+%TRIM(clientes.codigo_postal)    +',' +
                 ''+%TRIM(clientes.ciudad)           +',' +
                 ''+%TRIM(clientes.localidad)        +',' +
                 ''+%TRIM(est_tarjeta)               +',' +
                 ''+%TRIM(numero_tar)                +',' +
                 ''+%TRIM(suc_tar)                   +',' +
                 ''+%TRIM(cuenta_tar)                +'');

    endif;

    endsr;

//==============================================================================

    begsr actulizar_datos;

       exec sql open cursor_clientes;
       exec sql fetch cursor_clientes into :clientes;
       exsr limpiar_caracteres;

       exec sql SELECT
                    A#INDO
                INTO :documento_cliente
                FROM BAPFIS
                WHERE A#INDO = :clientes.dni;

       if (clientes.dni = documento_cliente);

       dow sqlcod = *Zero;

       //Actualizar Telefono en BAPFIS.
            exec sql UPDATE BAPFIS

                   SET A#ITEL = :clientes.telefono

                   WHERE

                   A#INDO = :clientes.dni AND
                   (A#ITEL = '' OR A#ITEL <> :clientes.telefono);

        //Actualizar Calle - Barrio, Numero, Codigo Postal.

            exec sql UPDATE BADIPF

                    SET AENCAL = UPPER(TRIM(:clientes.calle)) || ' ' ||
                                 UPPER(TRIM(:clientes.barrio)),
                        AEIPUE = clientes.numeracion,
                        AEICPO = :clientes.codigo_postal,
                        AEITDM = 5

                    WHERE
                    AEINDO = :clientes.dni AND
                    AEITDM <> 5;

        //Actualizar Correo y Empresa Telefonica.

            exec sql UPDATE BAPFDA

                    SET QQCOEL = :clientes.correo,
                        QQTCEL = CASE
                                  WHEN :clientes.empresa = 'Movistar' THEN 01
                                  WHEN :clientes.empresa = 'Claro'    THEN 02
                                  WHEN :clientes.empresa = 'Personal' THEN 03
                                ELSE 0 END

                    WHERE
                    QQINDO = :clientes.dni AND
                    (QQCOEL LIKE '%nocorreo%' OR QQCOEL LIKE '%NOCORREO%'  OR
                     QQCOEL LIKE '%NOMAIL%'   OR QQCOEL LIKE '%SINCORREO%' OR
                     QQCOEL = ' ' OR QQCOEL <> :clientes.correo);

            exec sql fetch cursor_clientes into :clientes;

        enddo;

        endif;

        exec sql close cursor_clientes;

    endsr;

//==============================================================================

    begsr limpiar_caracteres;

        clientes.apellido = %XLATE(Symbols:SymBlanks:clientes.apellido);
        clientes.apellido = %XLATE(Acentos:AceBlanks:clientes.apellido);
        clientes.apellido = %XLATE(Apos:AposBlank:clientes.apellido);
        clientes.apellido = %XLATE(lo:up:clientes.apellido);

        clientes.nombre = %XLATE(Symbols:SymBlanks:clientes.nombre);
        clientes.nombre = %XLATE(Acentos:AceBlanks:clientes.nombre);
        clientes.nombre = %XLATE(Apos:AposBlank:clientes.nombre);
        clientes.nombre = %XLATE(lo:up:clientes.nombre);

        clientes.empresa = %XLATE(Symbols:SymBlanks:clientes.empresa);
        clientes.empresa = %XLATE(Acentos:AceBlanks:clientes.empresa);
        clientes.empresa = %XLATE(Apos:AposBlank:clientes.empresa);
        clientes.empresa = %XLATE(lo:up:clientes.empresa);

        clientes.numeracion = %XLATE(Symbols:SymBlanks:clientes.numeracion);
        clientes.numeracion = %XLATE(Acentos:AceBlanks:clientes.numeracion);
        clientes.numeracion = %XLATE(Apos:AposBlank:clientes.numeracion);
        clientes.numeracion = %XLATE(lo:emp:clientes.numeracion);

        clientes.calle = %XLATE(Symbols:SymBlanks:clientes.calle);
        clientes.calle = %XLATE(Acentos:AceBlanks:clientes.calle);
        clientes.calle = %XLATE(Apos:AposBlank:clientes.calle);
        clientes.calle = %XLATE(lo:up:clientes.calle);

        clientes.barrio = %XLATE(Symbols:SymBlanks:clientes.barrio);
        clientes.barrio = %XLATE(Acentos:AceBlanks:clientes.barrio);
        clientes.barrio = %XLATE(Apos:AposBlank:clientes.barrio);
        clientes.barrio = %XLATE(lo:up:clientes.barrio);

        clientes.ciudad = %XLATE(Symbols:SymBlanks:clientes.ciudad);
        clientes.ciudad = %XLATE(Acentos:AceBlanks:clientes.ciudad);
        clientes.ciudad = %XLATE(Apos:AposBlank:clientes.ciudad);
        clientes.ciudad = %XLATE(lo:up:clientes.ciudad);

        clientes.localidad = %XLATE(Symbols:SymBlanks:clientes.localidad);
        clientes.localidad = %XLATE(Acentos:AceBlanks:clientes.localidad);
        clientes.localidad = %XLATE(Apos:AposBlank:clientes.localidad);
        clientes.localidad = %XLATE(lo:up:clientes.localidad);

    endsr;

//==============================================================================

    begsr cerrar_archivo_tds_existentes;

        ifs_stmf_close(fd);

    endsr;

//==============================================================================

    begsr generar_alta_tds;

    //Recupera Fecha Actual.
    exec sql SELECT
                AASFEI
             INTO :aasfei
             FROM SGSYSV;

    //Abrir Cursor.
    exec sql open cursor_clientes;
    exec sql fetch cursor_clientes into :clientes;

    Dow sqlcod = *Zero;

    //Consulto si ya tiene una tarjeta en tramite.
    exec sql open tarjeta_valida;
    exec sql fetch tarjeta_valida INTO :td_dni, :td_cuenta, :td_baja, :td_grupo,
                                     :grupo_td, :sub_grupo_td, :td_suc, :td_tdo;

    if sqlcod = *Zero;
        if (td_baja = 0);
        exec sql fetch cursor_clientes into :clientes;
        exec sql close tarjeta_valida;
        ITER;
        endif;
    endif;



    if (grupo_td = '04' AND (sub_grupo_td = 'JU' OR sub_grupo_td = ''));

        WWNTAR = *Zeros;
        LITA20H8(WWNTAR);

        //Alta en LIKMTR.

        exec sql INSERT INTO LIKMTR(
                    TAISUC,
                    TANTAR,
                    TAISUB,
                    TAICCL,
                    TAFALT,
                    TAIBAC,
                    TAFEBL,
                    TACATC,
                    TAILDB,
                    TATCPR,
                    TAIGRC,
                    TAISGC,
                    TAINCT,
                    TAIETA,
                    TAFVTO,
                    TAICAU,
                    TAICSC,
                    TANCCO,
                    TANYAP,
                    TAITDO,
                    TAINDO,
                    TAISDE,
                    TADF06,
                    TADF07,
                    TADF08)

                 SELECT DISTINCT
                    :td_suc,
                    :WWNTAR,
                    'AC',
                    :td_cuenta,
                    :aasfei,
                    0,
                    0,
                    20,
                    20,
                    11,
                    :grupo_td,
                    :sub_grupo_td,
                    :td_cuenta,
                    'G',
                    430,
                    'S',
                    'AC',
                    :td_cuenta,
                    UPPER(TRIM(:clientes.apellido)) || ', ' ||
                    UPPER(TRIM(:clientes.nombre)),
                    :td_tdo,
                    :clientes.dni,
                    0,
                    3,
                    :aasfei,
                    5010432222
                 FROM BADCCL
                 WHERE
                 OTICCL = :td_cuenta AND OTINDO = :clientes.dni;

        //Alta en LIKMAS.

        exec sql INSERT INTO LIKMAS (
                KMIFEC,
                KMCPIN,
                KMNSE1,
                KMACTI,
                KMNTAR,
                KM$PAT,
                KMIABN,
                KMFISO,
                KMCOTR,
                KMDPIS,
                KMIABP,
                KMICBP,
                KMFNA1,
                KMFUAS,
                KMICSL,
                KMILO2,
                KMFULB,
                KMFULT,
                KMNPOI,
                KMPPRP,
                KMIFA1,
                KMIFEA,
                KMIUF1,
                KMNRES,
                KMCTAD,
                KMICDT,
                KMCDIS,
                KMACTL,
                KMCATC,
                KMTXTF,
                KMNCDB,
                KMMONE,
                KM$CGP,
                KMBCOC,
                KMDAFJ,
                KMFENA,
                KMIACD,
                KMIASU,
                KMICAA,
                KMIF45,
                KMINUP,
                KMIPRE,
                KMIPT1,
                KMISER,
                KMITEL,
                KMITPR,
                KMLPLA,
                KMIF56,
                KMINU1,
                KMIQUE,
                KMISMO,
                KMNCBA,
                KMNCUI,
                KMNPIS,
                KMOBSE,
                KMTCAS,
                KMACT2,
                KMFDPA,
                KMFHPA,
                KMCC01,
                KMCC02,
                KMNDOA,
                KMPDES,
                KMCC03,
                KMCC04,
                KMQSI1,
                KMQSI2,
                KMRDEP,
                KMSUBT,
                KMIRFR,
                KMDDOB,
                KMENDB,
                KMEFOR,
                KMIFAS,
                KMIPAO,
                KMIHOL,
                KMIRA1,
                KMNEXJ,
                KMNFAN,
                KMNPCR,
                KMCC05,
                KMQSI3,
                KMQSI4,
                KMCC06,
                KMCC07,
                KMQSI5,
                KMFCMD,
                KMCC08,
                KMCC09,
                KMQCA2,
                KMQCA3,
                KMABAN,
                KMAFB1,
                KM$GA1,
                KMCBIT,
                KMCDPI,
                KMASCR,
                KMCLCR,
                KMCLDB,
                KMCREL,
                KMCC10,
                KMCO01,
                KMAALF,
                KMEXCA,
                KM$MCA,
                KMIGRP,
                KMACCI,
                KMIFEU,
                KMIFPH,
                KMIC02,
                KMACPA,
                KMAMON,
                KMAREF,
                KM$STA,
                KMICPR,
                KMIECJ,
                KMCFOR,
                KMBLK0,
                KMCBA1,
                KMALCR,
                KMNCCR,
                KMT224,
                KMBLK1,
                KMADIR,
                KMNKEY)

        SELECT DISTINCT
                 120005,
                 '0309',
                 999,
                 003,
                 0,
                 0,
                 :aasfei,
                 23000000,
                 '',
                 '',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 SUBSTRING(:WWNTAR, 1, 15),
                 501043,
                 OTICCL,
                 OTISUC,
                 1,
                 '1',
                 '11',
                 SUBSTRING(OTICCL, 1, 2) || '000' || SUBSTRING(OTICCL, 3),
                 SUBSTRING(OTICCL, 1, 2) || '000' || SUBSTRING(OTICCL, 3),
                 'DNI',
                 0,
                 '...............',
                 '...............',
                 0,
                 0,
                 'P',
                 'P',
                 SUBSTRING(UPPER(TRIM(:clientes.calle)) || ' ' ||
                 UPPER(TRIM(:clientes.barrio)), 1, 45),
                 '',
                 00,
                 '',
                 UPPER(TRIM(:clientes.localidad)),
                 UPPER(TRIM(:clientes.codigo_postal)),
                 '12',
                 :clientes.telefono,
                 '...................',
                 '',
                 '',
                 '',
                 '....................',
                 '...............',
                 '99',
                 '...............',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 CASE
                    WHEN OTITDO = 24 THEN 'CI '
                    WHEN OTITDO = 89 THEN 'LE '
                    WHEN OTITDO = 90 THEN 'LC '
                    WHEN OTITDO = 96 THEN 'DNI'
                    WHEN OTITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN OTITDO = 24 THEN 'CI '
                    WHEN OTITDO = 89 THEN 'LE '
                    WHEN OTITDO = 90 THEN 'LC '
                    WHEN OTITDO = 96 THEN 'DNI'
                    WHEN OTITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 OTINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 A#ICUI,
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '....................',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 SUBSTRING(:WWNTAR, 1, 15),
                 '0',
                 :clientes.dni,
                 0,
                 0,
                 20,
                 20,
                 0,
                 'P',
                 048,
                 0,
                 '1',
                 01,
                 0,
                 0,
                 '0',
                 0,
                 0,
                 0,
                 'N',
                 '00000000',
                 '00000000',
                 '................',
                 '',
                 11,
                 '1',
                 '',
                 '',
                 '',
                 '',
                 '...' || '999003' || :aasfei || '23000000' || '999003' ||
                 :aasfei || '23000000' ||
                 '......................................' ||
                 '......................................' ||
                 '......................................' ||
                 '......................................' ||
                 '.........................',
                 '..................................................' ||
                 '..................................................' ||
                 '..................................................' ||
                 '..................................................',
                 '*',
                 '*'
            FROM BADCCL
            INNER JOIN BAPFIS ON A#INDO = OTINDO AND A#ITDO = OTITDO
            INNER JOIN ACCTAC ON FUICAH = OTICCL AND FUISUC = OTISUC
            INNER JOIN LIKMTR ON TAICCL = FUICAH AND TAISUC = OTISUC
            WHERE
            OTINDO = :clientes.dni AND TAFALT = :aasfei;

            exec sql close tarjeta_valida;

    endif;

    if (grupo_td = '04' AND sub_grupo_td = 'AN' AND
        td_baja > 0 AND
       (td_grupo = 'G' OR td_grupo = 'R' OR
        td_grupo = 'E' OR td_grupo = 'I'));

        WWNTAR = *Zeros;
        LITA20H7(WWNTAR);

        //Alta en LIKMTR.

        exec sql INSERT INTO LIKMTR(
                    TAISUC,
                    TANTAR,
                    TAISUB,
                    TAICCL,
                    TAFALT,
                    TAIBAC,
                    TAFEBL,
                    TACATC,
                    TAILDB,
                    TATCPR,
                    TAIGRC,
                    TAISGC,
                    TAINCT,
                    TAIETA,
                    TAFVTO,
                    TAICAU,
                    TAICSC,
                    TANCCO,
                    TANYAP,
                    TAITDO,
                    TAINDO,
                    TAISDE,
                    TADF01,
                    TADF06,
                    TADF07,
                    TADF08)

                 SELECT DISTINCT
                    :td_suc,
                    :WWNTAR,
                    'AC',
                    :td_cuenta,
                    :aasfei,
                    0,
                    0,
                    20,
                    20,
                    11,
                    :grupo_td,
                    :sub_grupo_td,
                    :td_cuenta,
                    'G',
                    430,
                    'S',
                    'AC',
                    :td_cuenta,
                    UPPER(TRIM(:clientes.apellido)) || ', ' ||
                    UPPER(TRIM(:clientes.nombre)),
                    :td_tdo,
                    :td_dni,
                    0,
                    8888,
                    3,
                    :aasfei,
                    5010433090
                 FROM BADCCL
                 WHERE
                 OTICCL = :td_cuenta AND OTINDO = :clientes.dni;

              //Alta en LIKMAS.

              exec sql INSERT INTO LIKMAS (
                KMIFEC,
                KMCPIN,
                KMNSE1,
                KMACTI,
                KMNTAR,
                KM$PAT,
                KMIABN,
                KMFISO,
                KMCOTR,
                KMDPIS,
                KMIABP,
                KMICBP,
                KMFNA1,
                KMFUAS,
                KMICSL,
                KMILO2,
                KMFULB,
                KMFULT,
                KMNPOI,
                KMPPRP,
                KMIFA1,
                KMIFEA,
                KMIUF1,
                KMNRES,
                KMCTAD,
                KMICDT,
                KMCDIS,
                KMACTL,
                KMCATC,
                KMTXTF,
                KMNCDB,
                KMMONE,
                KM$CGP,
                KMBCOC,
                KMDAFJ,
                KMFENA,
                KMIACD,
                KMIASU,
                KMICAA,
                KMIF45,
                KMINUP,
                KMIPRE,
                KMIPT1,
                KMISER,
                KMITEL,
                KMITPR,
                KMLPLA,
                KMIF56,
                KMINU1,
                KMIQUE,
                KMISMO,
                KMNCBA,
                KMNCUI,
                KMNPIS,
                KMOBSE,
                KMTCAS,
                KMACT2,
                KMFDPA,
                KMFHPA,
                KMCC01,
                KMCC02,
                KMNDOA,
                KMPDES,
                KMCC03,
                KMCC04,
                KMQSI1,
                KMQSI2,
                KMRDEP,
                KMSUBT,
                KMIRFR,
                KMDDOB,
                KMENDB,
                KMEFOR,
                KMIFAS,
                KMIPAO,
                KMIHOL,
                KMIRA1,
                KMNEXJ,
                KMNFAN,
                KMNPCR,
                KMCC05,
                KMQSI3,
                KMQSI4,
                KMCC06,
                KMCC07,
                KMQSI5,
                KMFCMD,
                KMCC08,
                KMCC09,
                KMQCA2,
                KMQCA3,
                KMABAN,
                KMAFB1,
                KM$GA1,
                KMCBIT,
                KMCDPI,
                KMASCR,
                KMCLCR,
                KMCLDB,
                KMCREL,
                KMCC10,
                KMCO01,
                KMAALF,
                KMEXCA,
                KM$MCA,
                KMIGRP,
                KMACCI,
                KMIFEU,
                KMIFPH,
                KMIC02,
                KMACPA,
                KMAMON,
                KMAREF,
                KM$STA,
                KMICPR,
                KMIECJ,
                KMCFOR,
                KMBLK0,
                KMCBA1,
                KMALCR,
                KMNCCR,
                KMT224,
                KMBLK1,
                KMADIR,
                KMNKEY)

        SELECT DISTINCT
                 120001,
                 '0309',
                 999,
                 003,
                 0,
                 0,
                 :aasfei,
                 23000000,
                 '',
                 '',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 SUBSTRING(:WWNTAR, 1, 15),
                 501043,
                 OTICCL,
                 OTISUC,
                 1,
                 '1',
                 '11',
                 SUBSTRING(OTICCL, 1, 2) || '000' || SUBSTRING(OTICCL, 3),
                 SUBSTRING(OTICCL, 1, 2) || '000' || SUBSTRING(OTICCL, 3),
                 'DNI',
                 0,
                 '...............',
                 '...............',
                 0,
                 0,
                 'P',
                 'P',
                 SUBSTRING(UPPER(TRIM(:clientes.calle)) || ' ' ||
                 UPPER(TRIM(:clientes.barrio)), 1, 45),
                 '',
                 00,
                 '',
                 UPPER(TRIM(:clientes.localidad)),
                 UPPER(TRIM(:clientes.codigo_postal)),
                 '12',
                 :clientes.telefono,
                 '...................',
                 '',
                 '',
                 '',
                 '....................',
                 '...............',
                 '99',
                 '...............',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 CASE
                    WHEN OTITDO = 24 THEN 'CI '
                    WHEN OTITDO = 89 THEN 'LE '
                    WHEN OTITDO = 90 THEN 'LC '
                    WHEN OTITDO = 96 THEN 'DNI'
                    WHEN OTITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN OTITDO = 24 THEN 'CI '
                    WHEN OTITDO = 89 THEN 'LE '
                    WHEN OTITDO = 90 THEN 'LC '
                    WHEN OTITDO = 96 THEN 'DNI'
                    WHEN OTITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 OTINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 A#ICUI,
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '....................',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 SUBSTRING(:WWNTAR, 1, 15),
                 '0',
                 :clientes.dni,
                 0,
                 0,
                 20,
                 20,
                 0,
                 'P',
                 048,
                 0,
                 '1',
                 01,
                 0,
                 0,
                 '0',
                 0,
                 0,
                 0,
                 'N',
                 '00000000',
                 '00000000',
                 '................',
                 '',
                 11,
                 '1',
                 '',
                 '',
                 '',
                 '',
                 '...' || '999003' || :aasfei || '23000000' || '999003' ||
                 :aasfei || '23000000' ||
                 '......................................' ||
                 '......................................' ||
                 '......................................' ||
                 '......................................' ||
                 '.........................',
                 '..................................................' ||
                 '..................................................' ||
                 '..................................................' ||
                 '..................................................',
                 '*',
                 '*'
            FROM BADCCL
            INNER JOIN ACCTAC ON FUICAH = OTICCL AND FUISUC = OTISUC
            INNER JOIN BAPFIS ON A#INDO = OTINDO AND A#ITDO = OTITDO
            INNER JOIN LIKMTR ON TAICCL = FUICAH AND TAISUC = OTISUC
            WHERE
            OTINDO = :clientes.dni AND TAFALT = :aasfei;

            exec sql close tarjeta_valida;

    endif;

    if (grupo_td <> '04' AND td_baja > 0 AND
       (td_grupo = 'G' OR td_grupo = 'R' OR
        td_grupo = 'E' OR td_grupo = 'I'));

      WWITAR = *Zeros;
      LITA20H6(WWITAR);

      //Alta en LIKMTR.

      exec sql INSERT INTO LIKMTR(
                      TAISUC,
                      TANTAR,
                      TAISUB,
                      TAICCL,
                      TAFALT,
                      TAIBAC,
                      TAFEBL,
                      TACATC,
                      TAILDB,
                      TATCPR,
                      TAIGRC,
                      TAISGC,
                      TAINCT,
                      TAIETA,
                      TAFVTO,
                      TAICAU,
                      TAICSC,
                      TANCCO,
                      TANYAP,
                      TAITDO,
                      TAINDO,
                      TAISDE,
                      TADF06,
                      TADF07,
                      TADF08)

                   SELECT DISTINCT
                      :td_suc,
                      :WWITAR,
                      'AC',
                      :td_cuenta,
                      :aasfei,
                      0,
                      0,
                      20,
                      20,
                      11,
                      :grupo_td,
                      :sub_grupo_td,
                      :td_cuenta,
                      'G',
                      430,
                      'S',
                      'AC',
                      :td_cuenta,
                      UPPER(TRIM(:clientes.apellido)) || ', ' ||
                      UPPER(TRIM(:clientes.nombre)),
                      :td_tdo,
                      :clientes.dni,
                      0,
                      3,
                      :aasfei,
                      5010430000
                 FROM BADCCL
                 WHERE
                 OTICCL = :td_cuenta AND OTINDO = :clientes.dni;

        //Alta en LIKMAS.

        exec sql INSERT INTO LIKMAS (
                KMIFEC,
                KMCPIN,
                KMNSE1,
                KMACTI,
                KMNTAR,
                KM$PAT,
                KMIABN,
                KMFISO,
                KMCOTR,
                KMDPIS,
                KMIABP,
                KMICBP,
                KMFNA1,
                KMFUAS,
                KMICSL,
                KMILO2,
                KMFULB,
                KMFULT,
                KMNPOI,
                KMPPRP,
                KMIFA1,
                KMIFEA,
                KMIUF1,
                KMNRES,
                KMCTAD,
                KMICDT,
                KMCDIS,
                KMACTL,
                KMCATC,
                KMTXTF,
                KMNCDB,
                KMMONE,
                KM$CGP,
                KMBCOC,
                KMDAFJ,
                KMFENA,
                KMIACD,
                KMIASU,
                KMICAA,
                KMIF45,
                KMINUP,
                KMIPRE,
                KMIPT1,
                KMISER,
                KMITEL,
                KMITPR,
                KMLPLA,
                KMIF56,
                KMINU1,
                KMIQUE,
                KMISMO,
                KMNCBA,
                KMNCUI,
                KMNPIS,
                KMOBSE,
                KMTCAS,
                KMACT2,
                KMFDPA,
                KMFHPA,
                KMCC01,
                KMCC02,
                KMNDOA,
                KMPDES,
                KMCC03,
                KMCC04,
                KMQSI1,
                KMQSI2,
                KMRDEP,
                KMSUBT,
                KMIRFR,
                KMDDOB,
                KMENDB,
                KMEFOR,
                KMIFAS,
                KMIPAO,
                KMIHOL,
                KMIRA1,
                KMNEXJ,
                KMNFAN,
                KMNPCR,
                KMCC05,
                KMQSI3,
                KMQSI4,
                KMCC06,
                KMCC07,
                KMQSI5,
                KMFCMD,
                KMCC08,
                KMCC09,
                KMQCA2,
                KMQCA3,
                KMABAN,
                KMAFB1,
                KM$GA1,
                KMCBIT,
                KMCDPI,
                KMASCR,
                KMCLCR,
                KMCLDB,
                KMCREL,
                KMCC10,
                KMCO01,
                KMAALF,
                KMEXCA,
                KM$MCA,
                KMIGRP,
                KMACCI,
                KMIFEU,
                KMIFPH,
                KMIC02,
                KMACPA,
                KMAMON,
                KMAREF,
                KM$STA,
                KMICPR,
                KMIECJ,
                KMCFOR,
                KMBLK0,
                KMCBA1,
                KMALCR,
                KMNCCR,
                KMT224,
                KMBLK1,
                KMADIR,
                KMNKEY)

        SELECT DISTINCT
                 120005,
                 '0309',
                 999,
                 003,
                 0,
                 0,
                 :aasfei,
                 23000000,
                 '',
                 '',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 SUBSTRING(:WWITAR, 1, 15),
                 501043,
                 OTICCL,
                 OTISUC,
                 1,
                 '1',
                 '11',
                 SUBSTRING(OTICCL, 1, 2) || '000' || SUBSTRING(OTICCL, 3),
                 SUBSTRING(OTICCL, 1, 2) || '000' || SUBSTRING(OTICCL, 3),
                 'DNI',
                 0,
                 '...............',
                 '...............',
                 0,
                 0,
                 'P',
                 'P',
                 SUBSTRING(UPPER(TRIM(:clientes.calle)) || ' ' ||
                 UPPER(TRIM(:clientes.barrio)), 1, 45),
                 '',
                 00,
                 '',
                 UPPER(TRIM(:clientes.localidad)),
                 UPPER(TRIM(:clientes.codigo_postal)),
                 '12',
                 :clientes.telefono,
                 '...................',
                 '',
                 '',
                 '',
                 '....................',
                 '...............',
                 '99',
                 '...............',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 CASE
                    WHEN OTITDO = 24 THEN 'CI '
                    WHEN OTITDO = 89 THEN 'LE '
                    WHEN OTITDO = 90 THEN 'LC '
                    WHEN OTITDO = 96 THEN 'DNI'
                    WHEN OTITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN OTITDO = 24 THEN 'CI '
                    WHEN OTITDO = 89 THEN 'LE '
                    WHEN OTITDO = 90 THEN 'LC '
                    WHEN OTITDO = 96 THEN 'DNI'
                    WHEN OTITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 OTINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 A#ICUI,
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '....................',
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 999,
                 003,
                 :aasfei,
                 23000000,
                 SUBSTRING(:WWITAR, 1, 15),
                 '0',
                 :clientes.dni,
                 0,
                 0,
                 20,
                 20,
                 0,
                 'P',
                 048,
                 0,
                 '1',
                 01,
                 0,
                 0,
                 '0',
                 0,
                 0,
                 0,
                 'N',
                 '00000000',
                 '00000000',
                 '................',
                 '',
                 11,
                 '1',
                 '',
                 '',
                 '',
                 '',
                 '...' || '999003' || :aasfei || '23000000' || '999003' ||
                 :aasfei || '23000000' ||
                 '......................................' ||
                 '......................................' ||
                 '......................................' ||
                 '......................................' ||
                 '.........................',
                 '..................................................' ||
                 '..................................................' ||
                 '..................................................' ||
                 '..................................................',
                 '*',
                 '*'
            FROM BADCCL
            INNER JOIN ACCTAC ON FUICAH = OTICCL AND FUISUC = OTISUC
            INNER JOIN BAPFIS ON A#INDO = OTINDO AND A#ITDO = OTITDO
            INNER JOIN LIKMTR ON TAICCL = FUICAH AND TAISUC = OTISUC
            WHERE
            OTINDO = :clientes.dni AND TAFALT = :aasfei;

            exec sql close tarjeta_valida;

    endif;

    exec sql fetch cursor_clientes into :clientes;

    enddo;

    exec sql close cursor_clientes;

    endsr;

//==============================================================================

    begsr gener_informe_alta_tds;

    exec sql open cursor_clientes;
    exec sql fetch  cursor_clientes into :clientes;
    exsr abrir_archivo_tds_altas;
    exsr escribir_cabezera_archivo_tds_altas;
    dow sqlcod = *Zero;
            exsr escribir_detalles_tds_altas;
            exec sql fetch  cursor_clientes into :clientes;
    enddo;
    exec sql close cursor_clientes;
    exsr cerrar_archivo_tds_altas;

    endsr;

//==============================================================================

    begsr abrir_archivo_tds_altas;

    ejecutar_programa_SGAUTAC4(' chown GR00001 -R '+%trim(path_to_out)+'');

    fqn = %Trim(path_to_out) + %Trim(arc_tds_altas) + '_' + %Trim(sufijo) +
          '.csv';

        fd = ifs_stmf_opnwrt(fqn);
        if (fd < 0);
           exec sql close cursor_clientes;
           exsr cerrar_archivo_tds_altas;
           exsr dir_list_close;
           die('No se pudo abrir archivo:'+fqn);
        endif;

    endsr;

//==============================================================================

    begsr escribir_cabezera_archivo_tds_altas;

    ifs_wrtln(fd:'Marca temporal,'                  +
                 'Dirección de correo electrónico,' +
                 'Apellido,'                        +
                 'Nombre,'                          +
                 'DNI,'                             +
                 'Calle,'                           +
                 'Numeración,'                      +
                 'Barrio,'                          +
                 'Teléfono,'                        +
                 'Código Postal,'                   +
                 'Ciudad,'                          +
                 'Localidad,'                       +
                 'Estado Tarjeta,'                  +
                 'Numero Tarjeta,'                  +
                 'Sucursal,'                        +
                 'Cuenta'                           +
                 'Tarjeta'                          +
                 'Nro Tarjeta'                      +
                 'Sucursal'                         +
                 'Cuenta'                           +
                 'Observacion'                      );

    endsr;

//==============================================================================

    begsr escribir_detalles_tds_altas;

    exec sql
            SELECT DISTINCT
                OTINDO,
                TAIETA,
                TANTAR,
                TAISUC,
                TAICCL
                INTO :dni_cuenta, :est_tarjeta, :numero_tarjeta, :suc_tarjeta,
                :cuenta_tarjeta
            FROM BADCCL
            INNER JOIN ACCTAC ON FUICAH = OTICCL AND FUISUC = OTISUC
            INNER JOIN LIKMTR ON TAICCL = FUICAH AND TAISUC = FUISUC
            WHERE OTINDO = :clientes.dni AND
                  TAFALT = :aasfei AND
                  FUFBAJ = 0 AND
                  TAFBAJ = 0;

    numero_tar   = %char(numero_tarjeta);
    suc_tar      = %char(suc_tarjeta);
    cuenta_tar   = %char(cuenta_tarjeta);

    if (clientes.dni = dni_cuenta);

    doc_global = %char(clientes.dni);

    ifs_wrtln(fd:''+%TRIM(clientes.marca_temporal)   +',' +
                 ''+%TRIM(clientes.correo)           +',' +
                 ''+%TRIM(clientes.apellido)         +',' +
                 ''+%TRIM(clientes.nombre)           +',' +
                 ''+%TRIM(doc_global)                +',' +
                 ''+%TRIM(clientes.calle)            +',' +
                 ''+%TRIM(clientes.numeracion)       +',' +
                 ''+%TRIM(clientes.barrio)           +',' +
                 ''+%TRIM(clientes.telefono)         +',' +
                 ''+%TRIM(clientes.codigo_postal)    +',' +
                 ''+%TRIM(clientes.ciudad)           +',' +
                 ''+%TRIM(clientes.localidad)        +',' +
                 ''+%TRIM(est_tarjeta)               +',' +
                 ''+%TRIM(numero_tar)                +',' +
                 ''+%TRIM(suc_tar)                   +',' +
                 ''+%TRIM(cuenta_tar)                +',' +
                 'Se Solicita TDS'                   );

    endif;

    endsr;

//==============================================================================

    begsr cerrar_archivo_tds_altas;

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
              :c1_ds.filename || '_' ||
              (SELECT AASFEI FROM SGSYSV LIMIT 1),
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

    begsr mover_archivo_a_ya_procesado;

    ejecutar_programa_SGAUTAC4(' chown GR00001 -R '+%trim(path_to_dir)+'');

    cmd= 'MOV   OBJ('''+%Trim(full_path)+''')                      '+
         '    TOOBJ('''+%Trim(dest_name)+''')                      ';
    rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr dir_list_close;

        exec sql CLOSE C1;

    endsr;

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
