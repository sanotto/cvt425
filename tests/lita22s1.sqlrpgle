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

    //Llamada al Programa LITA20H8.
    dcl-pr LITA20H8 extpgm('LITA20H8');
        WWNTAR                  packed(16:0);
    end-pr;

    //Llamada al Programa LITA20H7.
    dcl-pr LITA20H7 extpgm('LITA20H7');
        WWNTAR                  packed(16:0);
    end-pr;

    //Llamada al Programa LITA20H6.
    dcl-pr LITA20H6 extpgm('LITA20H6');
        WWITAR                  packed(16:0);
    end-pr;

    //Llama al Programa SBBABFER.
    dcl-pr SBBABFER extpgm('SBBABFER');
        PAFECH                  packed(8:0);
        PADIAS                  packed(15:0);
        PAISUC                  packed(5:0);
    end-pr;

    //Llamada al Programa SBBAINFE.
    dcl-pr SBBAINFE extpgm('SBBAINFE');
        PAFECH                  packed(8:0);
        PACINV                  char(2);
    end-pr;

    //Llamada al Programa SBBACFMA.
    dcl-pr SBBACFMA extpgm('SBBACFMA');
        PAFECH                  packed(8:0);
        PAIMES                  packed(2:0);
    end-pr;

    //Llamada al Programa SBBAFECH.
    dcl-pr SBBAFECH extpgm('SBBAFECH');
        PAFECH                  packed(8:0);
        PADIAS                  packed(15:0);
        PADSEM                  char(2);
        PADIRR                  char(1);
    end-pr;

    //Llamada al Programa SGAUTAC4
    dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
       path_archivo      char(250) const;
    end-pr;

    dcl-s import_ok              ind;
    dcl-s dest_name              varchar(255);
    dcl-s full_path              varchar(255);
    dcl-s sufijo                 varchar(255);
    dcl-s path_to_out            varchar(255);
    dcl-s path_to_dir            varchar(255);
    dcl-s cmd                    varchar(4096);
    dcl-s rc                     char(7);
    dcl-s fd                     int(10);
    dcl-s fqn                    varchar(255) inz;
    dcl-s aasfei                 packed(8:0);
    dcl-s cuatro_meses           packed(8:0);
    dcl-s informe                char(30);
    dcl-s doc_global             char(15);
    dcl-s WWNTAR                 packed(16:0);
    dcl-s WWITAR                 packed(16:0);
    dcl-s cantidad_tarjetas      packed(2:0);
    dcl-s inf_no_clientes        char(255);
    dcl-s consulta_no_clientes   char(7500);
    dcl-s inf_clientes_tds       char(255);
    dcl-s consulta_clientes_tds  char(7500);
    dcl-s inf_alta_tds           char(255);
    dcl-s consulta_alta_tds      char(8500);
    dcl-s verificador            packed(2:0);
    dcl-s ultimo_movimiento      char(8);
    dcl-s documento_cliente      packed(15:0);
    dcl-s PAFECH                 packed(8:0);
    dcl-s PADIAS                 packed(15:0);
    dcl-s PAISUC                 packed(5:0);
    dcl-s PACINV                 char(2);
    dcl-s PAIMES                 packed(2:0);
    dcl-s PADSEM                 char(2);
    dcl-s PADIRR                 char(1);
    dcl-s nombre_liserj          varchar(255);
    dcl-s path_archivo           char(255);

    //Cursor No Clientes.
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

    //Cursor Clientes.
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

    //Fechas TD
    dcl-ds fechas_td qualified;
            fecha_td         packed(8:0);
    end-ds;

    //Clientes Sin Tarjetas.
    dcl-s tipo_doc           char(2);
    dcl-s num_doc            packed(15:0);
    dcl-s cuenta_cliente     char(11);
    dcl-s grupo_cliente      char(2);
    dcl-s sub_grupo_cliente  char(2);
    dcl-s sucursal_cliente   char(1);
    dcl-s nombe_apellido     char(30);

    //Clientes Con Tarjetas.
    dcl-s td_dni             packed(15:0);
    dcl-s td_cuenta          char(9);
    dcl-s td_baja            packed(8:0);
    dcl-s td_alta            packed(8:0);
    dcl-s td_estado          char(1);
    dcl-s td_suc             char(1);
    dcl-s td_tdo             packed(2:0);
    dcl-s cuenta_td          char(9);
    dcl-s grupo_td           char(2);
    dcl-s sub_grupo_td       char(2);

    //Informe Clientes con Tarjetas.
    dcl-s documento                 char(15);
    dcl-s sucursal_tarjeta          char(1);
    dcl-s numero_tarjeta            char(16);
    dcl-s cuenta_tarjeta            char(9);
    dcl-s estado_tarjeta            char(1);
    dcl-s fecha_alta                char(8);
    dcl-s fecha_recepcion_central   char(8);
    dcl-s fecha_recepcion_sucursal  char(8);
    dcl-s fecha_envio_interior      char(8);
    dcl-s entrega_oca               char(10);
    dcl-s devolucion_oca            char(10);
    dcl-s sucursal_devolucion       char(1);
    dcl-s modo_entrega              char(30);
    dcl-s modo_alta                 char(30);
    dcl-s cantidad_dev              char(10);

    //Cliente con cuentas dadas de baja.
    dcl-s baja_tipo_dni             char(2);
    dcl-s baja_dni                  packed(15:0);
    dcl-s baja_cuenta               char(11);
    dcl-s baja_sucursal             char(1);
    dcl-s baja_grupo                char(2);
    dcl-s baja_sub_grupo            char(2);
    dcl-s baja_apenom               char(30);

    //Recuperar Totales.
    dcl-s Total_Archivo             packed(10:0);
    dcl-s Total_Procesado           packed(10:0);
    dcl-s Total_Existentes          packed(10:0);
    dcl-s Total_Nuevas              packed(10:0);
    dcl-s Total_No_Clientes         packed(10:0);
    dcl-s Diferencia                packed(10:0);

    //Formatear Strings.
    dcl-c up              'AABCDEFGHIJKLMNNOOPQRSTUUVWXYZ'     ;
    dcl-c lo              'Áabcdefghijklmn¦oópqrstuÜvwxyz'     ;
    dcl-c emp             '                              '     ;
    dcl-c Symbols         '|°¬!"#Ü$%&/()=?\¡¿*+~¢]{}_-;,:.<>?-';
    dcl-c SymBlanks       '                                   ';
    dcl-c Acentos         'ñÑáéíóúäëïöüãõàèìòùâêîôû@Ó'         ;
    dcl-c AceBlanks       'nNAEIOUAEIOUAOAEIOUAEIOU O'         ;
    dcl-c AceEmpty        '                         '          ;
    dcl-c Apos            ''''                                 ;
    dcl-c APosBlank       ' '                                  ;

    path_to_dir = '/home/Tarjetas_de_Debitos/RECIBIDOS/';
    path_to_out = '/home/Tarjetas_de_Debitos/HISTORICO/';

    exec sql SET OPTION COMMIT = *NONE;

    exsr crea_sufijo_de_archivo_ya_procesado;
    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    dow sqlcod = *Zero;
         exsr verificar_archivo_procesado;
         exsr armar_paths;
         exsr limpiar_tablas_de_trabajo;
         exsr importar_archivo_csv;
         if import_ok;
            exsr consultas_auxiliares;
            exsr declarar_cursores;
            exsr generar_informe_no_clientes;
            exsr generar_informe_tds_existentes;
            exsr actulizar_datos;
            exsr generar_alta_tds;
            exsr generar_informe_alta_tds;
            exsr informar_personas_sin_altas;
            exsr guardar_liserj;
            exsr mover_archivo_a_ya_procesado;
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

        exec sql DROP TABLE QTEMP.ACTDCO; //Temporal Para Importar Formulario.
        exec sql DROP TABLE QTEMP.ACTDNC; //Temporal Para No Clientes.
        exec sql DROP TABLE QTEMP.ACTDEX; //Temporal Para TDS Existentes.
        exec sql DROP TABLE QTEMP.ACTDNT; //Temporal Para Nuevos Clientes.
        exec sql DROP TABLE QTEMP.ACTDSA; //Clientes sin altas.

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

        exec sql CREATE TABLE QTEMP.ACTDNC (
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
                        localidad           CHAR(50),
                        observacion         CHAR(50));

         exec sql CREATE TABLE QTEMP.ACTDEX (
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
                        localidad           CHAR(50),
                        fecha_alta          CHAR(50),
                        estado_tarjeta      CHAR(50),
                        numero_tarjeta      CHAR(50),
                        sucursal            CHAR(50),
                        cuenta_tarjeta      CHAR(50),
                        modo_alta           CHAR(50),
                        modo_entrega        CHAR(50),
                        suc_devolucion      CHAR(50),
                        fecha_central       CHAR(50),
                        fecha_sucursal      CHAR(50),
                        envio_interior      CHAR(50),
                        ultimo_uso_td       CHAR(50),
                        entrega_oca         CHAR(50),
                        devolucion_oca      CHAR(50),
                        cantidad_dev        CHAR(50));

         exec sql CREATE TABLE QTEMP.ACTDNT (
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
                        localidad           CHAR(50),
                        fecha_alta          CHAR(50),
                        estado_tarjeta      CHAR(50),
                        numero_tarjeta      CHAR(50),
                        sucursal            CHAR(50),
                        cuenta_tarjeta      CHAR(50),
                        entrega             CHAR(50),
                        suc_devolucion      CHAR(50),
                        duplicados          CHAR(50),
                        dup                 CHAR(50));

         exec sql CREATE TABLE QTEMP.ACTDSA (
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
                        localidad           CHAR(50),
                        observacion         CHAR(50));

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

    begsr dir_list_fetch;

        exec sql FETCH C1 into :c1_ds;

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

    begsr armar_paths;

        dest_name = %Trim(path_to_out)    +
                    %Trim(c1_ds.filename) +
                    '_'                   +
                    %Trim(sufijo)         ;

        full_path = %trim(path_to_dir)+%trim(c1_ds.filename);

    endsr;

//==============================================================================

    begsr limpiar_tablas_de_trabajo;

        exec sql DELETE FROM QTEMP.ACTDCO;
        exec sql DELETE FROM QTEMP.ACTDNC;
        exec sql DELETE FROM QTEMP.ACTDEX;
        exec sql DELETE FROM QTEMP.ACTDNT;
        exec sql DELETE FROM QTEMP.ACTDSA;

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

    begsr consultas_auxiliares;

        //Recuperar Fecha Actual.
        exec sql SELECT AASFEI INTO :aasfei FROM SGSYSV;

        //Recupero Cuarto Mes hacia atras.
        PAFECH = aasfei;
        PAIMES = *Zeros;

        SBBACFMA(PAFECH
                :PAIMES);

        PACINV = 'IN';

        SBBAINFE(PAFECH
                :PACINV);

        PADIAS = *Zeros;
        PADSEM = '';
        PADIRR = '';

        SBBAFECH(PAFECH
                :PADIAS
                :PADSEM
                :PADIRR);

        PADIAS = PADIAS - 120;
        PAFECH = *ZEROS;
        PAISUC = *ZEROS;

        SBBABFER(PAFECH
                :PADIAS
                :PAISUC);

        PACINV = 'NI';

        SBBAINFE(PAFECH
                :PACINV);

        cuatro_meses = PAFECH;

        //Limpiar DNI QTEMP.ACTDCO
        exec sql UPDATE QTEMP.ACTDCO
                    SET dni = CAST(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            REPLACE(REPLACE(
            dni, '.', ''), ' ', ''), 'M', ''), 'F', ''), 'LE', ''), 'LC',
            ''), 'f', ''), 'm', ''), 'él', ''), 'cha', '')
            AS DEC(15, 0));

        //Limpiar Caso errone que cargo Correo en Apellido.
        exec sql UPDATE QTEMP.ACTDCO
                    SET apellido = 'LUNA'
                 WHERE dni = 8017995;

    endsr;

//==============================================================================

    begsr declarar_cursores;

        //Cursor Para Clientes del Banco.
        exec sql declare cursor_clientes cursor for
            SELECT
            marca_temporal,
            apellido,
            nombre,
            dni,
            correo,
            CASE
                WHEN Substring(telefono, 1, 4) = '+549' THEN
                Substring(telefono, 5, 3) || '-' || Substring(telefono, 8)
                WHEN Substring(telefono, 1, 3) = '+54' THEN
                Substring(telefono, 4, 3) || '-' || Substring(telefono, 7)
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
                WHEN Substring(telefono, 1, 1) = '4' AND codigo_postal = 5360
                THEN '3825-' || telefono
                WHEN Substring(telefono, 1, 2) = '15' AND codigo_postal = 5300
                THEN '380-' || Substring(telefono, 3)
                WHEN Substring(telefono, 1, 2) = '15' AND codigo_postal = 5360
                THEN '3825-' || Substring(telefono, 4)
            ELSE telefono END AS telefono,
            empresa,
            calle,
            numeracion,
            barrio,
            codigo_postal,
            ciudad,
            localidad
            FROM QTEMP.ACTDCO
            INNER JOIN BAPFIS ON A#INDO = dni
            GROUP BY
            dni,
            marca_temporal,
            apellido,
            nombre,
            correo,
            telefono,
            empresa,
            calle,
            numeracion,
            barrio,
            codigo_postal,
            ciudad, localidad;

        //Cursor Para no clientes
        exec sql declare cursor_no_clientes cursor for
            SELECT
            marca_temporal,
            apellido,
            nombre,
            dni,
            correo,
            telefono,
            empresa,
            calle,
            numeracion,
            barrio,
            codigo_postal,
            ciudad,
            localidad
            FROM QTEMP.ACTDCO
            LEFT JOIN BAPFIS ON A#INDO = dni
            WHERE A#INDO IS NULL;

    endsr;

//==============================================================================

    begsr generar_alta_tds;

        //Alta de TDS a Clientes sin Tarjeta.
        exec sql open cursor_clientes;
        exec sql fetch cursor_clientes into :clientes;
        Dow sqlcod = *Zero;
                exsr limpiar_caracteres;
                exsr recupera_clientes_sin_tds;
                exsr alta_clientes_sin_tds;
            exec sql fetch cursor_clientes into :clientes;
        enddo;
        exec sql close cursor_clientes;

        //Alta de TDS a Clientes Con Tarjetas Dadas de Baja.
        exec sql open cursor_clientes;
        exec sql fetch cursor_clientes into :clientes;
        Dow sqlcod = *Zero;
            exsr limpiar_caracteres;
            exsr verificar_cantidad_de_tarjetas_por_cliente;
            exsr recuperar_tarjetas_validas;
            exsr alta_clientes_tds;
            exec sql fetch cursor_clientes into :clientes;
        enddo;
        exec sql close cursor_clientes;

    endsr;

//==============================================================================

    begsr recupera_clientes_sin_tds;

        exec sql SELECT DISTINCT
                    OTITDO,
                    OTINDO,
                    OTICCL,
                    FUISUC,
                    FUIGRC,
                    FUISGC,
                    A#NYAP
                    INTO :tipo_doc, :num_doc, :cuenta_cliente,
                         :sucursal_cliente, :grupo_cliente,
                         :sub_grupo_cliente, :nombe_apellido
                 FROM BADCCL
                 INNER JOIN BAPFIS ON A#ITDO = OTITDO AND A#INDO = OTINDO
                 INNER JOIN ACCTAC ON FUICAH = OTICCL AND FUISUC = OTISUC
                 LEFT  JOIN LIKMTR ON TAICCL = FUICAH AND TAISUC = OTISUC
                 WHERE
                 OTINDO = :clientes.dni AND
                 OTITTL = 1 AND
                 FUFBAJ = 0 AND
                 (:clientes.dni NOT IN (SELECT TAINDO FROM LIKMTR));

    endsr;

//==============================================================================

    begsr alta_clientes_sin_tds;

        if (grupo_cliente     = '04' AND cuenta_cliente <> '' AND
           (sub_grupo_cliente = 'JU' OR sub_grupo_cliente = ''));

           //Recupero Ultimo Numero de Tarjeta.
           WWNTAR = *Zeros;
           LITA20H8(WWNTAR);

           //Insertar en el Maestro de TDS.
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
                    TAFEIG,
                    TAISDE,
                    TADF03,
                    TADF06,
                    TADF07,
                    TADF08)

                 SELECT DISTINCT
                    :sucursal_cliente,
                    :WWNTAR,
                    'AC',
                    :cuenta_cliente,
                    :aasfei,
                    0,
                    0,
                    20,
                    20,
                    11,
                    :grupo_cliente,
                    :sub_grupo_cliente,
                    :cuenta_cliente,
                    'G',
                    430,
                    'S',
                    'AC',
                    :cuenta_cliente,
                    SUBSTRING(UPPER(TRIM(:clientes.apellido)) || ' ' ||
                    UPPER(TRIM(:clientes.nombre)), 1, 30),
                    :tipo_doc,
                    :clientes.dni,
                    :aasfei,
                    0,
                    current_user || '   ' || '00000',
                    3 || '        ' || 'P',
                    0 || '           ' || :aasfei,
                    5010432222
                    FROM BADCCL
                    WHERE
                    OTICCL = :cuenta_cliente AND OTINDO = :clientes.dni;

        //Insertar en LIKMAS.
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
                 TAICCL,
                 TAISUC,
                 1,
                 '1',
                 '11',
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
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
                 CASE
                    WHEN :clientes.numeracion = ''     THEN '99999'
                    WHEN :clientes.numeracion = ' '    THEN '99999'
                    WHEN :clientes.numeracion = '0'    THEN '99999'
                    WHEN :clientes.numeracion = '00'   THEN '99999'
                    WHEN :clientes.numeracion = '000'  THEN '99999'
                    WHEN :clientes.numeracion = '0000' THEN '99999'
                    WHEN SUBSTRING(:clientes.numeracion, 1, 5) = '' THEN '99999'
                 ELSE TRIM(SUBSTRING(:clientes.numeracion, 1, 5)) END,
                 '00',
                 '',
                 SUBSTRING(UPPER(TRIM(:clientes.localidad)), 1, 20),
                 CASE
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '560'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '150'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 30'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '530'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '54'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = ''
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '536'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 3'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 36'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '35'
                    THEN '          05300'
                 ELSE
                 '          0' || UPPER(TRIM(:clientes.codigo_postal)) END,
                 '12',
                 '    ' || SUBSTRING(:clientes.telefono, 1, 11),
                 '....................',
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
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 TAINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 SUBSTRING(A#ICUI, 1, 2) || 0 || SUBSTRING(A#ICUI, 3),
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '          ....................',
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
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................',
                 '*',
                 '*'
            FROM LIKMTR
            INNER JOIN BAPFIS ON A#INDO = TAINDO AND A#ITDO = TAITDO
            WHERE
            TAINDO = :clientes.dni AND
            TAICCL = :cuenta_cliente AND
            TADF06 = '3' || '        ' || 'P' AND
            (TAFALT = :aasfei AND TAFBAJ = 0);

        elseif (grupo_cliente = '04' AND sub_grupo_cliente = 'AN' AND
                cuenta_cliente <> '');

        //Recupero Ultimo Numero de Tarjeta.
        WWNTAR = *Zeros;
        LITA20H7(WWNTAR);

        //Insertar en el Maestro de TDS.
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
                    TAFEIG,
                    TAISDE,
                    TADF01,
                    TADF03,
                    TADF06,
                    TADF07,
                    TADF08)

                 SELECT DISTINCT
                    :sucursal_cliente,
                    :WWNTAR,
                    'AC',
                    :cuenta_cliente,
                    :aasfei,
                    0,
                    0,
                    20,
                    20,
                    11,
                    :grupo_cliente,
                    :sub_grupo_cliente,
                    :cuenta_cliente,
                    'G',
                    430,
                    'S',
                    'AC',
                    :cuenta_cliente,
                    SUBSTRING(UPPER(TRIM(:clientes.apellido)) || ' ' ||
                    UPPER(TRIM(:clientes.nombre)), 1, 30),
                    :tipo_doc,
                    :clientes.dni,
                    :aasfei,
                    0,
                    '8888',
                    current_user || '   ' || '00000',
                    3 || '        ' || 'P',
                    0 || '           ' || :aasfei,
                    5010433090
                 FROM BADCCL
                 WHERE
                 OTICCL = :cuenta_cliente AND OTINDO = :clientes.dni;

        //Insertar en LIKMAS.
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
                 TAICCL,
                 TAISUC,
                 8888,
                 '1',
                 '11',
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
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
                 CASE
                    WHEN :clientes.numeracion = ''     THEN '99999'
                    WHEN :clientes.numeracion = ' '    THEN '99999'
                    WHEN :clientes.numeracion = '0'    THEN '99999'
                    WHEN :clientes.numeracion = '00'   THEN '99999'
                    WHEN :clientes.numeracion = '000'  THEN '99999'
                    WHEN :clientes.numeracion = '0000' THEN '99999'
                    WHEN SUBSTRING(:clientes.numeracion, 1, 5) = '' THEN '99999'
                 ELSE TRIM(SUBSTRING(:clientes.numeracion, 1, 5)) END,
                 '00',
                 '',
                 SUBSTRING(UPPER(TRIM(:clientes.localidad)), 1, 20),
                 CASE
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '560'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '150'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 30'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '530'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '54'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = ''
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '536'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 3'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 36'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '35'
                    THEN '          05300'
                 ELSE
                 '          0' || UPPER(TRIM(:clientes.codigo_postal)) END,
                 '12',
                 '    ' || SUBSTRING(:clientes.telefono, 1, 11),
                 '....................',
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
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 TAINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 SUBSTRING(A#ICUI, 1, 2) || 0 || SUBSTRING(A#ICUI, 3),
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '          ....................',
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
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................',
                 '*',
                 '*'
            FROM LIKMTR
            INNER JOIN BAPFIS ON A#INDO = TAINDO AND A#ITDO = TAITDO
            WHERE
            TAINDO = :clientes.dni AND
            TAICCL = :cuenta_cliente AND
            TADF06 = '3' || '        ' || 'P' AND
            (TAFALT = :aasfei AND TAFBAJ = 0);

        elseif (grupo_cliente <> '04' AND cuenta_cliente <> '');

        //Recupero Ultimo Numero de Tarjeta.
        WWITAR = *Zeros;
        LITA20H6(WWITAR);

        //Insertar en el Maestro de TDS.
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
                        TAFEIG,
                        TAISDE,
                        TADF03,
                        TADF06,
                        TADF07,
                        TADF08)

                     SELECT DISTINCT
                        :sucursal_cliente,
                        :WWITAR,
                        'AC',
                        :cuenta_cliente,
                        :aasfei,
                        0,
                        0,
                        20,
                        20,
                        11,
                        :grupo_cliente,
                        :sub_grupo_cliente,
                        :cuenta_cliente,
                        'G',
                        430,
                        'S',
                        'AC',
                        :cuenta_cliente,
                        SUBSTRING(UPPER(TRIM(:clientes.apellido)) || ' ' ||
                        UPPER(TRIM(:clientes.nombre)), 1, 30),
                        :tipo_doc,
                        :clientes.dni,
                        :aasfei,
                        0,
                        current_user || '   ' || '00000',
                        3 || '        ' || 'P',
                        0 || '           ' || :aasfei,
                        5010430000
                   FROM BADCCL
                   WHERE
                   OTICCL = :cuenta_cliente AND OTINDO = :clientes.dni;

        //Insertar en LIKMAS.
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
                 SUBSTRING(:WWITAR, 1, 15),
                 501043,
                 TAICCL,
                 TAISUC,
                 1,
                 '1',
                 '11',
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
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
                 CASE
                    WHEN :clientes.numeracion = ''     THEN '99999'
                    WHEN :clientes.numeracion = ' '    THEN '99999'
                    WHEN :clientes.numeracion = '0'    THEN '99999'
                    WHEN :clientes.numeracion = '00'   THEN '99999'
                    WHEN :clientes.numeracion = '000'  THEN '99999'
                    WHEN :clientes.numeracion = '0000' THEN '99999'
                    WHEN SUBSTRING(:clientes.numeracion, 1, 5) = '' THEN '99999'
                 ELSE TRIM(SUBSTRING(:clientes.numeracion, 1, 5)) END,
                 '00',
                 '',
                 SUBSTRING(UPPER(TRIM(:clientes.localidad)), 1, 20),
                 CASE
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '560'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '150'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 30'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '530'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '54'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = ''
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '536'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 3'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 36'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '35'
                    THEN '          05300'
                 ELSE
                 '          0' || UPPER(TRIM(:clientes.codigo_postal)) END,
                 '12',
                 '    ' || SUBSTRING(:clientes.telefono, 1, 11),
                 '....................',
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
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 TAINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 SUBSTRING(A#ICUI, 1, 2) || 0 || SUBSTRING(A#ICUI, 3),
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '          ....................',
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
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................',
                 '*',
                 '*'
            FROM LIKMTR
            INNER JOIN BAPFIS ON A#INDO = TAINDO AND A#ITDO = TAITDO
            WHERE
            TAINDO = :clientes.dni AND
            TAICCL = :cuenta_cliente AND
            TADF06 = '3' || '        ' || 'P' AND
            (TAFALT = :aasfei AND TAFBAJ = 0);

        endif;

    endsr;

//==============================================================================

    begsr verificar_cantidad_de_tarjetas_por_cliente;

        exec sql SELECT COUNT(*) AS Cantidad
                 INTO :cantidad_tarjetas
                 FROM LIKMTR
                 WHERE TAINDO = :clientes.dni;

    endsr;

//==============================================================================

    begsr recuperar_tarjetas_validas;

    if (cantidad_tarjetas > 1);

    exec sql DROP TABLE QTEMP.TDCUAL; //Temporal para fechas.

    exec sql CREATE TABLE QTEMP.TDCUAL (fecha CHAR(50)); //Creo Tabla temporal.

    exec sql DELETE FROM QTEMP.TDCUAL; //Vaciar Tabla.

    exec sql INSERT INTO QTEMP.TDCUAL (
                                 fecha)
                    SELECT
                        TAFBAJ
                    FROM LIKMTR
                    INNER JOIN BADCCL ON OTICCL = TAICCL AND
                                         OTISUC = TAISUC AND
                                         OTINDO = TAINDO
                    INNER JOIN ACCTAC ON FUICAH = OTICCL AND
                                         FUISUC = OTISUC
                    WHERE
                    OTITTL = 1 AND
                    FUFBAJ = 0 AND TAINDO = :clientes.dni;

    exec sql declare fechas_tarjetas cursor for
                        SELECT
                            fecha
                        FROM QTEMP.TDCUAL;

    exec sql open fechas_tarjetas;
    exec sql fetch fechas_tarjetas into :fechas_td;
        Dow sqlcod = *Zero;
            if (fechas_td.fecha_td = 0);
            exec sql close fechas_tarjetas;
            endif;
        exec sql fetch fechas_tarjetas into :fechas_td;
        enddo;
    exec sql close fechas_tarjetas;

    if (fechas_td.fecha_td > 0);

        exec sql SELECT DISTINCT
                       TAINDO,
                       TAICCL,
                       TAFBAJ,
                       TAIETA,
                       TAFALT,
                       FUIGRC,
                       FUISGC,
                       FUISUC,
                       OTITDO
                 INTO :td_dni, :td_cuenta, :td_baja, :td_estado,
                      :td_alta,
                      :grupo_td, :sub_grupo_td, :td_suc,  :td_tdo
                 FROM LIKMTR
                 INNER JOIN BADCCL ON  OTICCL = TAICCL AND
                                       OTISUC = TAISUC AND
                                       OTINDO = TAINDO
                 INNER JOIN ACCTAC ON  FUICAH = OTICCL AND
                                       FUISUC = OTISUC
                 WHERE
                 OTITTL = 1 AND
                 FUFBAJ = 0 AND TAINDO = :clientes.dni;

    endif;

    elseif (cantidad_tarjetas = 1);

        exec sql SELECT DISTINCT
                       TAINDO,
                       TAICCL,
                       TAFBAJ,
                       TAIETA,
                       FUIGRC,
                       FUISGC,
                       FUISUC,
                       OTITDO
                 INTO :td_dni, :td_cuenta, :td_baja, :td_estado,
                      :grupo_td, :sub_grupo_td, :td_suc,  :td_tdo
                 FROM LIKMTR
                 INNER JOIN BADCCL ON  OTICCL = TAICCL AND
                                       OTISUC = TAISUC AND
                                       OTINDO = TAINDO
                 INNER JOIN ACCTAC ON  FUICAH = OTICCL AND
                                       FUISUC = OTISUC
                 WHERE
                 OTITTL = 1 AND
                 FUFBAJ = 0 AND TAINDO = :clientes.dni;

    endif;

    endsr;

//==============================================================================

    begsr alta_clientes_tds;

        if (grupo_td = '04' AND td_baja > 0 AND td_cuenta <> '' AND
           (sub_grupo_td = 'JU' OR sub_grupo_td = ''));

        //Recupero Ultimo Numero de Tarjeta.
        WWNTAR = *Zeros;
        LITA20H8(WWNTAR);

        //Insertar en el Maestro de TDS.
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
                    TAFEIG,
                    TAISDE,
                    TADF03,
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
                    SUBSTRING(UPPER(TRIM(:clientes.apellido)) || ' ' ||
                    UPPER(TRIM(:clientes.nombre)), 1, 30),
                    :td_tdo,
                    :clientes.dni,
                    :aasfei,
                    0,
                    current_user || '   ' || '00000',
                    3 || '        ' || 'P',
                    0 || '           ' ||:aasfei,
                    5010432222
                 FROM BADCCL
                 WHERE
                 OTICCL = :td_cuenta AND OTINDO = :clientes.dni;

        //Insertar en LIKMAS.
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
                 TAICCL,
                 TAISUC,
                 1,
                 '1',
                 '11',
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
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
                 CASE
                    WHEN :clientes.numeracion = ''     THEN '99999'
                    WHEN :clientes.numeracion = ' '    THEN '99999'
                    WHEN :clientes.numeracion = '0'    THEN '99999'
                    WHEN :clientes.numeracion = '00'   THEN '99999'
                    WHEN :clientes.numeracion = '000'  THEN '99999'
                    WHEN :clientes.numeracion = '0000' THEN '99999'
                    WHEN SUBSTRING(:clientes.numeracion, 1, 5) = '' THEN '99999'
                 ELSE TRIM(SUBSTRING(:clientes.numeracion, 1, 5)) END,
                 '00',
                 '',
                 SUBSTRING(UPPER(TRIM(:clientes.localidad)), 1, 20),
                 CASE
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '560'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '150'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 30'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '530'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '54'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = ''
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '536'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 3'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 36'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '35'
                    THEN '          05300'
                 ELSE
                 '          0' || UPPER(TRIM(:clientes.codigo_postal)) END,
                 '12',
                 '    ' || SUBSTRING(:clientes.telefono, 1, 11),
                 '....................',
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
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 TAINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 SUBSTRING(A#ICUI, 1, 2) || 0 || SUBSTRING(A#ICUI, 3),
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '          ....................',
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
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................',
                 '*',
                 '*'
            FROM LIKMTR
            INNER JOIN BAPFIS ON A#INDO = TAINDO AND A#ITDO = TAITDO
            WHERE
            TAINDO = :clientes.dni AND
            TAICCL = :td_cuenta AND
            TADF06 = '3' || '        ' || 'P' AND
            (TAFALT = :aasfei AND TAFBAJ = 0);

        elseif (grupo_td = '04' AND sub_grupo_td = 'AN' AND td_baja > 0 AND
                td_cuenta <> '');

        //Recupero Ultimo Numero de Tarjeta.
        WWNTAR = *Zeros;
        LITA20H7(WWNTAR);

        //Insertar en el Maestro de TDS.
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
                    TAFEIG,
                    TAISDE,
                    TADF01,
                    TADF03,
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
                    SUBSTRING(UPPER(TRIM(:clientes.apellido)) || ' ' ||
                    UPPER(TRIM(:clientes.nombre)), 1, 30),
                    :td_tdo,
                    :td_dni,
                    :aasfei,
                    0,
                    '8888',
                    current_user || '   ' || '00000',
                    3 || '        ' || 'P',
                    0 || '           ' ||:aasfei,
                    5010433090
                 FROM BADCCL
                 WHERE
                 OTICCL = :td_cuenta AND OTINDO = :clientes.dni;

        //Insertar en LIKMAS.
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
                 TAICCL,
                 TAISUC,
                 8888,
                 '1',
                 '11',
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
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
                 CASE
                    WHEN :clientes.numeracion = ''     THEN '99999'
                    WHEN :clientes.numeracion = ' '    THEN '99999'
                    WHEN :clientes.numeracion = '0'    THEN '99999'
                    WHEN :clientes.numeracion = '00'   THEN '99999'
                    WHEN :clientes.numeracion = '000'  THEN '99999'
                    WHEN :clientes.numeracion = '0000' THEN '99999'
                    WHEN SUBSTRING(:clientes.numeracion, 1, 5) = '' THEN '99999'
                 ELSE TRIM(SUBSTRING(:clientes.numeracion, 1, 5)) END,
                 '00',
                 '',
                 SUBSTRING(UPPER(TRIM(:clientes.localidad)), 1, 20),
                 CASE
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '560'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '150'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 30'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '530'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '54'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = ''
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '536'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 3'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 36'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '35'
                    THEN '          05300'
                 ELSE
                 '          0' || UPPER(TRIM(:clientes.codigo_postal)) END,
                 '12',
                 '    ' || SUBSTRING(:clientes.telefono, 1, 11),
                 '....................',
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
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 TAINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 SUBSTRING(A#ICUI, 1, 2) || 0 || SUBSTRING(A#ICUI, 3),
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '          ....................',
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
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................',
                 '*',
                 '*'
            FROM LIKMTR
            INNER JOIN BAPFIS ON A#INDO = TAINDO AND A#ITDO = TAITDO
            WHERE
            TAINDO = :clientes.dni AND
            TAICCL = :td_cuenta AND
            TADF06 = '3' || '        ' || 'P' AND
            (TAFALT = :aasfei AND TAFBAJ = 0);

        elseif (grupo_td <> '04' AND td_baja > 0 AND td_cuenta <> '');

        //Recupero Ultimo Numero de Tarjeta.
        WWITAR = *Zeros;
        LITA20H6(WWITAR);

        //Insertar en el Maestro de TDS.
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
                      TAFEIG,
                      TAISDE,
                      TADF03,
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
                      SUBSTRING(UPPER(TRIM(:clientes.apellido)) || ' ' ||
                    UPPER(TRIM(:clientes.nombre)), 1, 30),
                      :td_tdo,
                      :clientes.dni,
                      :aasfei,
                      0,
                      current_user || '   ' || '00000',
                      3 || '        ' || 'P',
                      0 || '           ' ||:aasfei,
                      5010430000
                 FROM BADCCL
                 WHERE
                 OTICCL = :td_cuenta AND OTINDO = :clientes.dni;

        //Insertar en LIKMAS.
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
                 SUBSTRING(:WWITAR, 1, 15),
                 501043,
                 TAICCL,
                 TAISUC,
                 1,
                 '1',
                 '11',
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
                 CASE
                    WHEN TAICCL > 99999999
                    THEN '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 3)
                 ELSE '000' || TAISUC || TAIGRC || '000' ||
                    SUBSTRING(TAICCL, 2) END,
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
                 CASE
                    WHEN :clientes.numeracion = ''     THEN '99999'
                    WHEN :clientes.numeracion = ' '    THEN '99999'
                    WHEN :clientes.numeracion = '0'    THEN '99999'
                    WHEN :clientes.numeracion = '00'   THEN '99999'
                    WHEN :clientes.numeracion = '000'  THEN '99999'
                    WHEN :clientes.numeracion = '0000' THEN '99999'
                    WHEN SUBSTRING(:clientes.numeracion, 1, 5) = '' THEN '99999'
                 ELSE TRIM(SUBSTRING(:clientes.numeracion, 1, 5)) END,
                 '00',
                 '',
                 SUBSTRING(UPPER(TRIM(:clientes.localidad)), 1, 20),
                 CASE
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '560'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '150'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 30'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '530'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '54'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = ''
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '536'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 3'
                    THEN '          05300'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '5 36'
                    THEN '          05360'
                    WHEN UPPER(TRIM(:clientes.codigo_postal)) = '35'
                    THEN '          05300'
                 ELSE
                 '          0' || UPPER(TRIM(:clientes.codigo_postal)) END,
                 '12',
                 '    ' || SUBSTRING(:clientes.telefono, 1, 11),
                 '....................',
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
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 CASE
                    WHEN TAITDO = 24 THEN 'CI '
                    WHEN TAITDO = 89 THEN 'LE '
                    WHEN TAITDO = 90 THEN 'LC '
                    WHEN TAITDO = 96 THEN 'DNI'
                    WHEN TAITDO = 94 THEN 'PAS'
                 ELSE '' END,
                 TAINDO,
                 SUBSTRING(UPPER(TRIM(:clientes.apellido)), 1, 15),
                 SUBSTRING(UPPER(TRIM(:clientes.nombre)), 1, 15),
                 A#ISEX,
                 SUBSTRING(A#ICUI, 1, 2) || 0 || SUBSTRING(A#ICUI, 3),
                 '....................',
                 A#FNAC,
                 'S',
                 'ARGENTINA',
                 '          ....................',
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
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................'      ||
                 '..................................................',
                 '*',
                 '*'
            FROM LIKMTR
            INNER JOIN BAPFIS ON A#INDO = TAINDO AND A#ITDO = TAITDO
            WHERE
            TAINDO = :clientes.dni AND
            TAICCL = :td_cuenta AND
            TADF06 = '3' || '        ' || 'P' AND
            (TAFALT = :aasfei AND TAFBAJ = 0);

        endif;

    endsr;

//==============================================================================

    begsr generar_informe_no_clientes;

        exec sql open cursor_no_clientes;
        exec sql fetch cursor_no_clientes into :no_clientes;
        dow sqlcod = *Zero;
            exsr insertar_no_clientes;
            exec sql fetch  cursor_no_clientes into :no_clientes;
        enddo;
        exec sql close cursor_no_clientes;

    endsr;

//==============================================================================

    begsr insertar_no_clientes;

        exec sql INSERT INTO QTEMP.ACTDNC(
                    marca_temporal,
                    correo,
                    apellido,
                    nombre,
                    dni,
                    calle,
                    numeracion,
                    barrio,
                    telefono,
                    empresa,
                    codigo_postal,
                    ciudad,
                    localidad,
                    observacion)

                 VALUES (
                    :no_clientes.marca_temporal,
                    :no_clientes.correo,
                    :no_clientes.apellido,
                    :no_clientes.nombre,
                    :no_clientes.dni,
                    :no_clientes.calle,
                    :no_clientes.numeracion,
                    :no_clientes.barrio,
                    :no_clientes.telefono,
                    :no_clientes.empresa,
                    :no_clientes.codigo_postal,
                    :no_clientes.ciudad,
                    :no_clientes.localidad,
                    'No es Cliente');

    endsr;

//==============================================================================

    begsr informe_no_clientes;

    ejecutar_programa_SGAUTAC4(' chown GR00001 -R '+%trim(path_to_dir)+'');

    inf_no_clientes = %Trim(path_to_out) + 'PediTuTarjeta-No-Clientes' + '_' +
                      %Trim(sufijo)      + '.xls';

    consulta_no_clientes = 'SELECT                                         '+
                           'CAST(marca_temporal AS CHAR(25)) AS Marca_Temp,'+
                           'CAST(correo AS CHAR(50))         AS Correo,    '+
                           'CAST(apellido AS CHAR(30))       AS Aepllido,  '+
                           'CAST(nombre AS CHAR(30))         AS Nombre,    '+
                           'CAST(dni AS CHAR(15))            AS Dni,       '+
                           'CAST(calle AS CHAR(30))          AS Calle,     '+
                           'CAST(numeracion AS CHAR(15))     AS Numeracion,'+
                           'CAST(barrio AS CHAR(30))         AS Barrio,    '+
                           'CAST(telefono AS CHAR(15))       AS Telefono,  '+
                           'CAST(empresa AS CHAR(15))        AS Empresa,   '+
                           'CAST(codigo_postal AS CHAR(5))   AS Cod_Postal,'+
                           'CAST(ciudad AS CHAR(25))         AS Ciudad,    '+
                           'CAST(localidad AS CHAR(25))      AS Localidad, '+
                           'CAST(observacion AS CHAR(15))    AS Obs        '+
                           'FROM QTEMP.ACTDNC                              ';

    cmd =   'EXPPCDBF SQLSTM('''+%Trim(consulta_no_clientes)+''')  '+
            'OUTPAT('''+%Trim(inf_no_clientes)+''')                ';

    rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr generar_informe_tds_existentes;

        exec sql open cursor_clientes;
        exec sql fetch  cursor_clientes into :clientes;
        dow sqlcod = *Zero;
             exsr valido_cliente_con_tds;
             if (verificador > 0);
                exsr recuperar_clientes_con_tds;
                exsr recuperar_ultimo_movimiento;
                exsr insertar_clientes_con_tds;
             endif;
             exec sql fetch  cursor_clientes into :clientes;
        enddo;
        exec sql close cursor_clientes;
        exsr informar_clintes_con_tds;

    endsr;

//==============================================================================

    begsr valido_cliente_con_tds;

        exec sql SELECT
                    COUNT(*)
                 INTO :verificador
                 FROM LIKMTR
                 WHERE
                 TAINDO = :clientes.dni AND
                 TAFBAJ = 0;

    endsr;

//==============================================================================

    begsr recuperar_clientes_con_tds;

        exec sql
            SELECT DISTINCT
                TAINDO,
                TAISUC,
                TANTAR,
                TAFALT,
                TAICCL,
                TAIETA,
                TAF001,
                TAF002,
                TAF003,
                SUBSTRING(TADF06, 2, 8),
                SUBSTRING(TADF06, 11, 9),
                TAISDE,
                CASE
                    WHEN SUBSTRING(TADF06, 1, 1) = '1' THEN 'Delivery'
                    WHEN SUBSTRING(TADF06, 1, 1) = '2' THEN 'Delivery'
                    WHEN SUBSTRING(TADF06, 1, 1) = '4' THEN 'P.Puerta'
                    WHEN SUBSTRING(TADF06, 1, 1) IN ('', '3') THEN 'BRSAU'
                ELSE '' END,
                CASE
                    WHEN SUBSTRING(TADF06, 10, 1) = ''  THEN 'ALTA.ADM'
                    WHEN SUBSTRING(TADF06, 10, 1) = 'A' THEN 'AM.ANSES'
                    WHEN SUBSTRING(TADF06, 10, 1) = 'S' THEN 'AM.SIDEBA'
                    WHEN SUBSTRING(TADF06, 10, 1) = 'P' THEN 'AM.PTT'
                ELSE '' END,
                SUBSTRING(TADF06, 19, 1)
                INTO :documento, :sucursal_tarjeta, :numero_tarjeta,
                     :fecha_alta, :cuenta_tarjeta, :estado_tarjeta,
                     :fecha_recepcion_central, :fecha_recepcion_sucursal,
                     :fecha_envio_interior, :entrega_oca, :devolucion_oca,
                     :sucursal_devolucion, :modo_entrega, :modo_alta,
                     :cantidad_dev
            FROM LIKMTR
            WHERE TAINDO = :clientes.dni AND
                  TAFBAJ = 0;

    endsr;

//==============================================================================

    begsr recuperar_ultimo_movimiento;

    ultimo_movimiento = ' ';

    if (estado_tarjeta = 'E');

        exec sql SELECT
                    GCFING
                 INTO :ultimo_movimiento
                 FROM ACMOVD
                 WHERE
                 GCICAH = :cuenta_tarjeta AND
                 GCISUC = :sucursal_tarjeta AND
                 GCHALT BETWEEN :cuatro_meses AND :aasfei AND
                 (GCIMCA = 48 OR GCIMCA = 8)
                 ORDER BY GCFING DESC
                 LIMIT 1;

       if (ultimo_movimiento = ' ');

       exec sql SELECT
                   GDFALT
                INTO :ultimo_movimiento
                FROM ACMOVH
                WHERE
                GDICAH = :cuenta_tarjeta AND
                GDISUC = :sucursal_tarjeta AND
                GDFALT BETWEEN :cuatro_meses AND :aasfei AND
                (GDIMCA = 48 OR GDIMCA = 8)
                ORDER BY GDFALT DESC
                LIMIT 1;

       endif;

    endif;

    endsr;

//==============================================================================

    begsr insertar_clientes_con_tds;

        exec sql INSERT INTO QTEMP.ACTDEX(
                    marca_temporal,
                    correo,
                    apellido,
                    nombre,
                    dni,
                    calle,
                    numeracion,
                    barrio,
                    telefono,
                    empresa,
                    codigo_postal,
                    ciudad,
                    localidad,
                    fecha_alta,
                    estado_tarjeta,
                    numero_tarjeta,
                    sucursal,
                    cuenta_tarjeta,
                    modo_alta,
                    modo_entrega,
                    suc_devolucion,
                    fecha_central,
                    fecha_sucursal,
                    envio_interior,
                    ultimo_uso_td,
                    entrega_oca,
                    devolucion_oca,
                    cantidad_dev)

                 VALUES(
                    :clientes.marca_temporal,
                    :clientes.correo,
                    :clientes.apellido,
                    :clientes.nombre,
                    :clientes.dni,
                    :clientes.calle,
                    :clientes.numeracion,
                    :clientes.barrio,
                    :clientes.telefono,
                    :clientes.empresa,
                    :clientes.codigo_postal,
                    :clientes.ciudad,
                    :clientes.localidad,
                    :fecha_alta,
                    :estado_tarjeta,
                    :numero_tarjeta,
                    :sucursal_tarjeta,
                    :cuenta_tarjeta,
                    :modo_alta,
                    :modo_entrega,
                    :sucursal_devolucion,
                    :fecha_recepcion_central,
                    :fecha_recepcion_sucursal,
                    :fecha_envio_interior,
                    :ultimo_movimiento,
                    :entrega_oca,
                    :devolucion_oca,
                    :cantidad_dev);

    endsr;

//==============================================================================

    begsr informar_clintes_con_tds;

    ejecutar_programa_SGAUTAC4(' chown GR00001 -R '+%trim(path_to_dir)+'');

    inf_clientes_tds = %Trim(path_to_out) + 'PediTuTarjeta-TDS-Existentes' +
                       '_' + %Trim(sufijo) + '.xls';

    consulta_clientes_tds = 'SELECT                                         '+
                            'CAST(marca_temporal AS CHAR(25)) AS Marca_Temp,'+
                            'CAST(correo AS CHAR(50))         AS Correo,    '+
                            'CAST(apellido AS CHAR(30))       AS Aepllido,  '+
                            'CAST(nombre AS CHAR(30))         AS Nombre,    '+
                            'CAST(dni AS CHAR(15))            AS Dni,       '+
                            'CAST(calle AS CHAR(30))          AS Calle,     '+
                            'CAST(numeracion AS CHAR(15))     AS Numeracion,'+
                            'CAST(barrio AS CHAR(30))         AS Barrio,    '+
                            'CAST(telefono AS CHAR(15))       AS Telefono,  '+
                            'CAST(empresa AS CHAR(15))        AS Empresa,   '+
                            'CAST(codigo_postal AS CHAR(5))   AS Cod_Postal,'+
                            'CAST(ciudad AS CHAR(25))         AS Ciudad,    '+
                            'CAST(localidad AS CHAR(25))      AS Localidad, '+
                            'CAST(fecha_alta AS CHAR(8))      AS Fecha_Alta,'+
                            'CAST(estado_tarjeta AS CHAR(1))  AS Estado,    '+
                            'CAST(numero_tarjeta AS CHAR(16)) AS Tarjeta,   '+
                            'CAST(sucursal AS CHAR(1))        AS Sucursal,  '+
                            'CAST(cuenta_tarjeta AS CHAR(9))  AS Cuenta,    '+
                            'CAST(modo_alta AS CHAR(10))      AS ModoAlta,  '+
                            'CAST(modo_entrega AS CHAR(10))   AS ModEntrega,'+
                            'CAST(suc_devolucion AS CHAR(1))  AS SucursalDv,'+
                            'CAST(fecha_central AS CHAR(8))   AS Recepcion, '+
                            'CAST(fecha_sucursal AS CHAR(8))  AS Rec_Sucu,  '+
                            'CAST(envio_interior AS CHAR(8))  AS Envio_Inte,'+
                            'CAST(ultimo_uso_td AS CHAR(8))   AS Ultimo_Uso,'+
                            'CAST(entrega_oca AS CHAR(8))     AS EntregaOCA,'+
                            'CAST(devolucion_oca AS CHAR(10)) AS FecDevOCA, '+
                            'CAST(cantidad_dev AS CHAR(10))   AS CantDevOCA '+
                            'FROM QTEMP.ACTDEX                              ';

    cmd =   'EXPPCDBF SQLSTM('''+%Trim(consulta_clientes_tds)+''')  '+
            'OUTPAT('''+%Trim(inf_clientes_tds)+''')                ';

    rc=exeCmd(cmd);

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

                    SET QQCOEL = :clientes.correo

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
      clientes.numeracion = %XLATE(lo:up:clientes.numeracion);
      clientes.numeracion = %XLATE(up:emp:clientes.numeracion);

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

      clientes.codigo_postal = %XLATE(Symbols:SymBlanks:clientes.codigo_postal);
      clientes.codigo_postal = %XLATE(Acentos:AceBlanks:clientes.codigo_postal);
      clientes.codigo_postal = %XLATE(Apos:AposBlank:clientes.codigo_postal);
      clientes.codigo_postal = %XLATE(lo:up:clientes.codigo_postal);
      clientes.codigo_postal = %XLATE(up:emp:clientes.codigo_postal);

    endsr;

//==============================================================================

    begsr generar_informe_alta_tds;

        exec sql open cursor_clientes;
        exec sql fetch  cursor_clientes into :clientes;
        dow sqlcod = *Zero;
             exsr insertar_tds_dados_de_alta;
             exec sql fetch  cursor_clientes into :clientes;
        enddo;
        exec sql close cursor_clientes;
        exsr cambiar_estado_duplicados;
        exsr informar_tds_dadas_de_alta;

    endsr;

//==============================================================================

    begsr cambiar_estado_duplicados;

        //Coloco en Blanco las altas sin duplicados.
        exec sql UPDATE QTEMP.ACTDNT
                             SET dup = ''
                 WHERE DUPLICADOS = '1';

        exec sql open cursor_clientes;
        exec sql fetch  cursor_clientes into :clientes;

        //Coloco en Blanco a uno de los duplicados en las altas.
        dow sqlcod = *Zero;
        exec sql UPDATE QTEMP.ACTDNT
                             SET dup = ''
                 WHERE dni = :clientes.dni AND
                       MARCA_TEMPORAL = (SELECT MARCA_TEMPORAL
                                         FROM QTEMP.ACTDNT
                                         WHERE dni = :clientes.dni
                                         LIMIT 1);
        exec sql fetch  cursor_clientes into :clientes;
        enddo;
        exec sql close cursor_clientes;

     endsr;

//==============================================================================

    begsr insertar_tds_dados_de_alta;

        exec sql INSERT INTO QTEMP.ACTDNT(
                    marca_temporal,
                    correo,
                    apellido,
                    nombre,
                    dni,
                    calle,
                    numeracion,
                    barrio,
                    telefono,
                    empresa,
                    codigo_postal,
                    ciudad,
                    localidad,
                    fecha_alta,
                    estado_tarjeta,
                    numero_tarjeta,
                    sucursal,
                    cuenta_tarjeta,
                    entrega,
                    suc_devolucion,
                    duplicados,
                    dup)

                 SELECT DISTINCT
                    :clientes.marca_temporal,
                    :clientes.correo,
                    :clientes.apellido,
                    :clientes.nombre,
                    :clientes.dni,
                    :clientes.calle,
                    :clientes.numeracion,
                    :clientes.barrio,
                    :clientes.telefono,
                    :clientes.empresa,
                    :clientes.codigo_postal,
                    :clientes.ciudad,
                    :clientes.localidad,
                    TAFALT,
                    TAIETA,
                    TANTAR,
                    TAISUC,
                    TAICCL,
                    'BR SAU - Pedi Tu Tarjeta',
                    TAISDE,
                    (SELECT COUNT(*) FROM QTEMP.ACTDCO
                    WHERE :clientes.dni = CAST(dni AS dec(15))),
                    'S'
                 FROM LIKMTR
                 WHERE
                 TAINDO = :clientes.dni AND
                 TADF03 = CURRENT_USER || '   ' || '00000' AND
                 TADF06 = '3' || '        ' || 'P' AND
                 TAFALT = :aasfei;

    endsr;

//==============================================================================

    begsr informar_tds_dadas_de_alta;

    inf_alta_tds = %Trim(path_to_out) + 'PediTuTarjeta-Altas-TDS' +
                   '_' + %Trim(sufijo) + '.xls';

    consulta_alta_tds = 'SELECT                                         '+
                        'CAST(marca_temporal AS CHAR(25)) AS Marca_Temp,'+
                        'CAST(correo AS CHAR(50))         AS Correo,    '+
                        'CAST(apellido AS CHAR(30))       AS Aepllido,  '+
                        'CAST(nombre AS CHAR(30))         AS Nombre,    '+
                        'CAST(dni AS CHAR(15))            AS Dni,       '+
                        'CAST(calle AS CHAR(30))          AS Calle,     '+
                        'CAST(numeracion AS CHAR(15))     AS Numeracion,'+
                        'CAST(barrio AS CHAR(30))         AS Barrio,    '+
                        'CAST(telefono AS CHAR(15))       AS Telefono,  '+
                        'CAST(empresa AS CHAR(15))        AS Empresa,   '+
                        'CAST(codigo_postal AS CHAR(5))   AS Cod_Postal,'+
                        'CAST(ciudad AS CHAR(25))         AS Ciudad,    '+
                        'CAST(localidad AS CHAR(25))      AS Localidad, '+
                        'CAST(fecha_alta AS CHAR(8))      AS Fecha_Alta,'+
                        'CAST(estado_tarjeta AS CHAR(1))  AS Estado,    '+
                        'CAST(numero_tarjeta AS CHAR(16)) AS Tarjeta,   '+
                        'CAST(sucursal AS CHAR(1))        AS Sucursal,  '+
                        'CAST(cuenta_tarjeta AS CHAR(9))  AS Cuenta,    '+
                        'CAST(entrega AS CHAR(30))        AS Entrega,   '+
                        'CAST(suc_devolucion AS CHAR(1))  AS SucursalDv,'+
                        'CAST(duplicados AS CHAR(5))      AS Cant_Dup,  '+
                        'CAST(dup AS CHAR(5))             AS Duplicados '+
                        'FROM QTEMP.ACTDNT                              ';

    cmd =   'EXPPCDBF SQLSTM('''+%Trim(consulta_alta_tds)+''')  '+
            'OUTPAT('''+%Trim(inf_alta_tds)+''')                ';

    rc=exeCmd(cmd);

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
              TRIM(:c1_ds.filename) || '_' || :aasfei,
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

    begsr informar_personas_sin_altas;

        exec sql INSERT INTO QTEMP.ACTDSA (
                        marca_temporal,
                        correo        ,
                        apellido      ,
                        nombre        ,
                        dni           ,
                        calle         ,
                        numeracion    ,
                        barrio        ,
                        telefono      ,
                        empresa       ,
                        codigo_postal ,
                        ciudad        ,
                        localidad     ,
                        observacion)

                        SELECT
                        marca_temporal,
                        correo        ,
                        apellido      ,
                        nombre        ,
                        dni           ,
                        calle         ,
                        numeracion    ,
                        barrio        ,
                        telefono      ,
                        empresa       ,
                        codigo_postal ,
                        ciudad        ,
                        localidad     ,
                        'NO ALTA PARA ESTE CLIENTE'
                        FROM QTEMP.ACTDCO
                        WHERE
                        DNI NOT IN (SELECT DNI FROM QTEMP.ACTDEX) AND
                        DNI NOT IN (SELECT DNI FROM QTEMP.ACTDNC) AND
                        DNI NOT IN (SELECT DNI FROM QTEMP.ACTDNT);

         exec sql SELECT COUNT(*) INTO :Total_Archivo FROM QTEMP.ACTDCO;
         exec sql SELECT COUNT(*) INTO :Total_Existentes FROM QTEMP.ACTDEX;
         exec sql SELECT COUNT(*) INTO :Total_Nuevas FROM QTEMP.ACTDNT;
         exec sql SELECT COUNT(*) INTO :Total_No_Clientes FROM QTEMP.ACTDNC;

         Total_Procesado = Total_Existentes + Total_Nuevas + Total_No_Clientes;

         Diferencia = Total_Archivo - Total_Procesado;

         if (Total_Procesado < Total_Archivo);

            exec sql INSERT INTO QTEMP.ACTDNC (
                        marca_temporal,
                        correo        ,
                        apellido      ,
                        nombre        ,
                        dni           ,
                        calle         ,
                        numeracion    ,
                        barrio        ,
                        telefono      ,
                        empresa       ,
                        codigo_postal ,
                        ciudad        ,
                        localidad     ,
                        observacion)

                        SELECT
                        marca_temporal,
                        correo        ,
                        apellido      ,
                        nombre        ,
                        dni           ,
                        calle         ,
                        numeracion    ,
                        barrio        ,
                        telefono      ,
                        empresa       ,
                        codigo_postal ,
                        ciudad        ,
                        localidad     ,
                        'CUENTA DE BAJA'
                        FROM QTEMP.ACTDSA
                        LIMIT :diferencia;
         endif;

         exsr informe_no_clientes;

    endsr;

//==============================================================================

    begsr dir_list_close;

        exec sql CLOSE C1;

    endsr;

//==============================================================================

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
