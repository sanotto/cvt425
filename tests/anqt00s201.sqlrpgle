*SRCMBRTXT:Importación de Fellecidos Reg. Civil In
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

dcl-f BAPFIS05 keyed;
dcl-f BAPSIN01 keyed usage(*update:*output);

dcl-ds BAPFIS5 likerec(reBAPFIS);
dcl-ds BAPSIN1 likerec(reBAPSIN);

//==============================================================================
//Declaración de prototipos, importadas de los miembros declarados.
//==============================================================================

/include sdb01.src/qrpgsrc,ligdifs ;
/include sdb01.src/qrpgsrc,ligdprot ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

    //Llamada al Programa BAER00RS.
    dcl-pr BAER00RS extpgm('BAER00RS');
        MSJ1                  char(55);
        MSJ2                  char(55);
        MSJ3                  char(55);
        MSJ4                  char(55);
        MSJ5                  char(55);
        MSJ6                  char(55);
        MSJ7                  char(55);
        MSJ8                  char(55);
    end-pr;

    dcl-ds c1_ds qualified;
        filename               varchar(255);
    end-ds;

    dcl-ds fallecidos qualified;
        marca_temporal         varchar(35);
        apellido               varchar(50);
        nombre                 varchar(50);
        dni                    packed(15:0);
        fec_fall               varchar(15);
        sexo                   varchar(15);
    end-ds;

    dcl-s dest_name            varchar(255);
    dcl-s sufijo               varchar(255);

    dcl-s cmd                  varchar(4096);
    dcl-s full_path            varchar(255);
    dcl-s rc                   char(7);
    dcl-s import_ok            ind;
    dcl-s nombre_liserj        varchar(30);
    dcl-s aasfei               varchar(8);
    dcl-s fecha_fall           varchar(8);

    dcl-s consulta             varchar(8000);
    dcl-s informe_xls          varchar(255);

    dcl-s persona_bapfis       char(1);
    dcl-s persona_bapsin       char(1);

    dcl-s MSJ1                 char(55);
    dcl-s MSJ2                 char(55);
    dcl-s MSJ3                 char(55);
    dcl-s MSJ4                 char(55);
    dcl-s MSJ5                 char(55);
    dcl-s MSJ6                 char(55);
    dcl-s MSJ7                 char(55);
    dcl-s MSJ8                 char(55);

    dcl-s path_to_dir char(255);
    dcl-s path_to_out char(255);

    path_to_dir = '/home/ANSES/IMP/';
    path_to_out = '/home/ANSES/HISTORICOS/';

    exec sql SET OPTION COMMIT = *NONE;

    exsr crea_sufijo_de_archivo_ya_procesado;
    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    dow sqlcode = *zero;
        exsr verificar_archivo_procesado;
        exsr armar_paths;
        exsr importar_archivo;
        if import_ok;
            exsr declarar_cursor_fallecidos;
            exsr operaciones_correspondientes;
            exsr generar_informe;
            exsr guardar_log_en_liserj;
            exsr enviar_correo;
            exsr mover_archivo_a_ya_procesado;
        endif;
        exsr dir_list_fetch;
    enddo;
    exsr dir_list_close;

//==============================================================================

    begsr crea_sufijo_de_archivo_ya_procesado;

        exec sql SELECT AASFEI INTO :aasfei FROM SGSYSV;

        exec sql SELECT
                    TRANSLATE(
                            CAST ( timestamp(now()) as char(19))
                                   , '__','.-')
                    INTO :sufijo
                 FROM SYSIBM/SYSDUMMY1 ;

    endsr;

//==============================================================================

    begsr crear_tablas_de_trabajo;

        //Eliminamos Tablas Existentes.
        exec sql DROP TABLE QTEMP.FALRGC;
        exec sql DROP TABLE QTEMP.FALDUP;
        exec sql DROP TABLE QTEMP.FALNEW;
        exec sql DROP TABLE QTEMP.FALACC;

        //Creamos Nueva Tabla.
        exec sql
                 CREATE TABLE QTEMP.FALRGC (
                    marca_temporal      CHAR(35),
                    usuario             CHAR(60),
                    apellido            CHAR(50),
                    nombre              CHAR(50),
                    dni                 CHAR(15),
                    fecha_fallecimiento CHAR(15),
                    sexo                CHAR(15),
                    delegacion          CHAR(60));

        exec sql
                 CREATE TABLE QTEMP.FALDUP (
                    marca_temporal      CHAR(35),
                    apellido            CHAR(50),
                    nombre              CHAR(50),
                    dni                 CHAR(15),
                    fecha_fallecimiento CHAR(15),
                    sexo                CHAR(15),
                    inconsistencia      CHAR(50),
                    observacion         CHAR(50));

        exec sql
                 CREATE TABLE QTEMP.FALNEW (
                    marca_temporal      CHAR(35),
                    apellido            CHAR(50),
                    nombre              CHAR(50),
                    dni                 CHAR(15),
                    fecha_fallecimiento CHAR(15),
                    sexo                CHAR(15),
                    inconsistencia      CHAR(50),
                    observacion         CHAR(50));

        exec sql
                 CREATE TABLE QTEMP.FALACC (
                    marca_temporal      CHAR(35),
                    apellido            CHAR(50),
                    nombre              CHAR(50),
                    dni                 CHAR(15),
                    fecha_fallecimiento CHAR(15),
                    sexo                CHAR(15),
                    inconsistencia      CHAR(30),
                    observacion         CHAR(35),
                    subsistema          CHAR(20),
                    nro_sucursal        CHAR(10),
                    nro_cuenta          CHAR(15),
                    titular             CHAR(5),
                    grupo_cuenta        CHAR(10),
                    subgrupo_cuenta     CHAR(10),
                    saldo_operativo     CHAR(15),
                    fecha_baja          CHAR(25),
                    codigo_bloqueo      CHAR(25),
                    estado_cuenta       CHAR(30));

        //Vaciamos Tabla.
        exec sql DELETE FROM QTEMP.FALRGC;
        exec sql DELETE FROM QTEMP.FALDUP;
        exec sql DELETE FROM QTEMP.FALNEW;
        exec sql DELETE FROM QTEMP.FALACC;

    endsr;

//==============================================================================

    begsr dir_list_open;

        exec sql DECLARE C1 CURSOR FOR
                SELECT
                    CAST (FILENAME as char(255)) AS FILNAM
                FROM TABLE(IFSDIR(:path_to_dir)) AS T
                WHERE
                    UPPER(FILENAME) LIKE '%FALLEREGISTRO%';

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

        if sqlcod = *zero;

        exec sql DROP TABLE QTEMP.FALRGC;

        exsr dir_list_close;

        MSJ4='  El Archivo '+%Trim(c1_ds.filename)+', ya fué   ';
        MSJ5='  procesado anteriormente por favor verifique y  ';
        MSJ6='  reintente nuevamente.                          ';

        BAER00RS(MSJ1
                :MSJ2
                :MSJ3
                :MSJ4
                :MSJ5
                :MSJ6
                :MSJ7
                :MSJ8);

        return;

        endif;

    endsr;

//==============================================================================

    begsr armar_paths;

        dest_name = %Trim(path_to_out)+
                    %Trim(c1_ds.filename)+
                    '_' +
                    ''+%Trim(sufijo);

        full_path = %trim(path_to_dir)+%trim(c1_ds.filename);

    endsr;

//==============================================================================

    begsr importar_archivo;

        import_ok = *off;

        //Cambio ccsid a 1208 del archivo .csv
        cmd =       'CHGATR OBJ('''+%Trim(full_path)+''') ' +
                    'ATR(*CCSID) VALUE(1208)';
        rc = execmd(cmd);

        rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '           TOFILE(QTEMP/FALRGC)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*LF)                                     ' +
                '           FLDDLM('','')                                   ' );

        //Eliminar cabeceras.
        if rc = 'CPF0000' ;

            exec sql
                DELETE FROM QTEMP.FALRGC WHERE RRN(FALRGC) = 1;

            import_ok = *on;

        endif;

    endsr;

//==============================================================================

    begsr declarar_cursor_fallecidos;

        exec sql DECLARE fallecidos CURSOR FOR
                SELECT
                    marca_temporal,
                    apellido,
                    nombre,
                    dni,
                    fecha_fallecimiento,
                    sexo
                FROM QTEMP.FALRGC;

    endsr;

//==============================================================================

    begsr operaciones_correspondientes;

        exec sql open fallecidos;
        exec sql fetch fallecidos into :fallecidos;
        Dow sqlcod = *Zero;
            exsr verificar_si_es_cliente;
            if (persona_bapfis = '2' AND persona_bapsin = '1');
                //Cliente pero no esta en fallecidos.
                exsr dar_alta_en_bapsin_y_temporal;
            else;
                //No es cliente o es cliente y esta en fallecidos.
                exsr dar_alta_en_temporal;
            endif;
            exec sql fetch fallecidos into :fallecidos;
        enddo;
        exec sql close fallecidos;
        exsr obtener_cuentas_clientes;
        exsr guardar_historicos_bloqueos_ac_cc;
        exsr bloquear_ac_cc;
        exsr informar_cuentas_bloquedas;

    endsr;

//==============================================================================

    begsr verificar_si_es_cliente;

        //Verificar Persona BAPFIS.
        chain (fallecidos.dni) BAPFIS05 BAPFIS5;
            if not %found();
                persona_bapfis = '1';
            else;
                persona_bapfis = '2';
            endif;

        //Verificar Persona BAPSIN.
        chain (fallecidos.dni) BAPSIN01 BAPSIN1;
            if not %found();
                persona_bapsin = '1';
            else;
                persona_bapsin = '2';
            endif;
    endsr;

//==============================================================================

    begsr dar_alta_en_bapsin_y_temporal;

        //Recuperar Fecha de Falecimiento.
        fecha_fall = %Scanrpl('-':'':%Subst(fallecidos.fec_fall:1:10));

        //Alta en Bapsin.
        QTITDO = BAPFIS5.A#ITDO;
        QTINDO = BAPFIS5.A#INDO;
        QTFALT = %Dec(aasfei:8:0);
        QTNYAP = BAPFIS5.A#NYAP;
        QTFBAJ = %Dec(fecha_fall:8:0);
        QTDAV1 = c1_ds.filename;
        QTDAM2 = 'REGISTRO CIVIL INTERIOR' + ' ' + aasfei;

        write REBAPSIN;

        //Alta en Temporal.
        exec sql INSERT INTO QTEMP.FALNEW(
                            marca_temporal,
                            apellido,
                            nombre,
                            dni,
                            fecha_fallecimiento,
                            sexo,
                            inconsistencia,
                            observacion)

        VALUES (:fallecidos.marca_temporal,
                :fallecidos.apellido,
                :fallecidos.nombre,
                :fallecidos.dni,
                :fallecidos.fec_fall,
                :fallecidos.sexo,
                'No existe Inconsistencia',
                'Alta en Fallecidos');

    endsr;

//==============================================================================

    begsr dar_alta_en_temporal;

        if (persona_bapfis = '1' AND persona_bapsin = '1');

        //Alta en Temporal para no Clientes.
        exec sql INSERT INTO QTEMP.FALDUP(
                            marca_temporal,
                            apellido,
                            nombre,
                            dni,
                            fecha_fallecimiento,
                            sexo,
                            inconsistencia,
                            observacion)

        VALUES (:fallecidos.marca_temporal,
                :fallecidos.apellido,
                :fallecidos.nombre,
                :fallecidos.dni,
                :fallecidos.fec_fall,
                :fallecidos.sexo,
                '-',
                'No es cliente del Banco');

        elseif (persona_bapfis = '2' AND persona_bapsin = '2');

        //Recuperar Fecha de Falecimiento.
        fecha_fall = %Scanrpl('-':'':%Subst(fallecidos.fec_fall:1:10));

        //Alta en Temporal Clientes Cargados en Bapsin.
        exec sql INSERT INTO QTEMP.FALDUP(
                            marca_temporal,
                            apellido,
                            nombre,
                            dni,
                            fecha_fallecimiento,
                            sexo,
                            inconsistencia,
                            observacion)

        VALUES (:fallecidos.marca_temporal,
                :fallecidos.apellido,
                :fallecidos.nombre,
                :fallecidos.dni,
                :fallecidos.fec_fall,
                :fallecidos.sexo,
                CASE
                    WHEN :BAPSIN1.QTFBAJ = CAST(:fecha_fall AS DEC (8, 0)) THEN
                    'No existe Inconsistencia'
                ELSE 'Existe Inconsistencia' END,
                'Existe en Base de Fallecidos');

         endif;

    endsr;

//==============================================================================

    begsr obtener_cuentas_clientes;

        //Informe Fallecidos Final.
        exec sql INSERT INTO QTEMP.FALACC(
                        marca_temporal,
                        apellido,
                        nombre,
                        dni,
                        fecha_fallecimiento,
                        sexo,
                        inconsistencia,
                        observacion,
                        subsistema,
                        nro_sucursal,
                        nro_cuenta,
                        titular,
                        grupo_cuenta,
                        subgrupo_cuenta,
                        saldo_operativo,
                        fecha_baja,
                        codigo_bloqueo,
                        estado_cuenta)
        SELECT
            marca_temporal,
            apellido,
            nombre,
            dni,
            fecha_fallecimiento,
            sexo,
            inconsistencia,
            observacion,
            CASE
                WHEN OTICCL = FUICAH THEN 'AC'
                WHEN OTICCL = BMICCC THEN 'CC'
            ELSE 'No posee cuenta' END,
            OTISUC,
            OTICCL,
            OTITTL,
            CASE
                WHEN OTICCL = FUICAH THEN FUIGRC
                WHEN OTICCL = BMICCC THEN BMIGRC
            ELSE '-' END,
            CASE
                WHEN OTICCL = FUICAH THEN FUISGC
                WHEN OTICCL = BMICCC THEN BMISGC
            ELSE '-' END,
            CASE
                WHEN OTICCL = FUICAH THEN FU$SOP
                WHEN OTICCL = BMICCC THEN BM$SOP
            ELSE ' ' END,
            CASE
                WHEN OTICCL = FUICAH THEN FUFBAJ
                WHEN OTICCL = BMICCC THEN BMFBAJ
            ELSE ' ' END,
            CASE
                WHEN FUIBAC = 0 THEN '0'
                WHEN FUIBAC = 2 THEN '2'
                WHEN FUIBAC = 3 THEN '3'
                WHEN FUIBAC = 4 THEN '4'
                WHEN FUIBAC = 7 THEN '7'
                WHEN BMIBCC = 0 THEN '0'
                WHEN BMIBCC = 1 THEN '1'
                WHEN BMIBCC = 2 THEN '2'
                WHEN BMIBCC = 4 THEN '4'
                WHEN BMIBCC = 7 THEN '7'
                WHEN BMIBCC = 9 THEN '9'
            ELSE '-' END,
            CASE
                WHEN FUIBAC = 0 THEN 'Sin bloqueo'
                WHEN FUIBAC = 2 THEN 'Bloqueo a debitos'
                WHEN FUIBAC = 3 THEN 'Bloqueo a creditos'
                WHEN FUIBAC = 4 THEN 'Bloqueo a creditos y debitos'
                WHEN FUIBAC = 7 THEN 'Orden de Embargo AFIP'
                WHEN BMIBCC = 0 THEN 'Sin bloqueo'
                WHEN BMIBCC = 1 THEN 'Bloqueo a debitos'
                WHEN BMIBCC = 2 THEN 'Bloqueo a creditos'
                WHEN BMIBCC = 4 THEN 'Bloqueo a creditos y debitos'
                WHEN BMIBCC = 7 THEN 'Orden de Embargo AFIP'
                WHEN BMIBCC = 9 THEN 'Bloqueo a Debito p/Sueldo'
            ELSE '-' END

    FROM QTEMP.FALNEW

    LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
    LEFT JOIN ACCTAC ON OTICCL = FUICAH
    LEFT JOIN CCCTCT ON OTICCL = BMICCC
    LEFT JOIN BAPSIN ON QTINDO = CAST(dni AS DEC(15, 0))
    LEFT JOIN ACBLOQ ON FVIBAC = FUIBAC
    LEFT JOIN CCBLOQ ON BFIBCC = BMIBCC

    WHERE
    OTITTL IN (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
               13, 14, 15, 16, 17, 18, 19, 20)
    OR (OTITTL IS NULL)

    UNION ALL

        SELECT
            marca_temporal,
            apellido,
            nombre,
            dni,
            fecha_fallecimiento,
            sexo,
            inconsistencia,
            observacion,
            CASE
                WHEN OTICCL = FUICAH THEN 'AC'
                WHEN OTICCL = BMICCC THEN 'CC'
            ELSE 'No posee cuenta' END,
            OTISUC,
            OTICCL,
            OTITTL,
            CASE
                WHEN OTICCL = FUICAH THEN FUIGRC
                WHEN OTICCL = BMICCC THEN BMIGRC
            ELSE '-' END,
            CASE
                WHEN OTICCL = FUICAH THEN FUISGC
                WHEN OTICCL = BMICCC THEN BMISGC
            ELSE '-' END,
            CASE
                WHEN OTICCL = FUICAH THEN FU$SOP
                WHEN OTICCL = BMICCC THEN BM$SOP
            ELSE ' ' END,
            CASE
                WHEN OTICCL = FUICAH THEN FUFBAJ
                WHEN OTICCL = BMICCC THEN BMFBAJ
            ELSE ' ' END,
            CASE
                WHEN FUIBAC = 0 THEN '0'
                WHEN FUIBAC = 2 THEN '2'
                WHEN FUIBAC = 3 THEN '3'
                WHEN FUIBAC = 4 THEN '4'
                WHEN FUIBAC = 7 THEN '7'
                WHEN BMIBCC = 0 THEN '0'
                WHEN BMIBCC = 1 THEN '1'
                WHEN BMIBCC = 2 THEN '2'
                WHEN BMIBCC = 4 THEN '4'
                WHEN BMIBCC = 7 THEN '7'
                WHEN BMIBCC = 9 THEN '9'
            ELSE '-' END,
            CASE
                WHEN FUIBAC = 0 THEN 'Sin bloqueo'
                WHEN FUIBAC = 2 THEN 'Bloqueo a debitos'
                WHEN FUIBAC = 3 THEN 'Bloqueo a creditos'
                WHEN FUIBAC = 4 THEN 'Bloqueo a creditos y debitos'
                WHEN FUIBAC = 7 THEN 'Orden de Embargo AFIP'
                WHEN BMIBCC = 0 THEN 'Sin bloqueo'
                WHEN BMIBCC = 1 THEN 'Bloqueo a debitos'
                WHEN BMIBCC = 2 THEN 'Bloqueo a creditos'
                WHEN BMIBCC = 4 THEN 'Bloqueo a creditos y debitos'
                WHEN BMIBCC = 7 THEN 'Orden de Embargo AFIP'
                WHEN BMIBCC = 9 THEN 'Bloqueo a Debito p/Sueldo'
            ELSE '-' END

    FROM QTEMP.FALDUP

    LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
    LEFT JOIN ACCTAC ON OTICCL = FUICAH
    LEFT JOIN CCCTCT ON OTICCL = BMICCC
    LEFT JOIN BAPSIN ON QTINDO = CAST(dni AS DEC(15, 0))
    LEFT JOIN ACBLOQ ON FVIBAC = FUIBAC
    LEFT JOIN CCBLOQ ON BFIBCC = BMIBCC

    WHERE
    OTITTL IN (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
               13, 14, 15, 16, 17, 18, 19, 20)
    OR (OTITTL IS NULL);

    endsr;

//==============================================================================

   begsr guardar_historicos_bloqueos_ac_cc;

   //Historicos AC bloqueo 4
   exec sql

       INSERT INTO ACCTAB
           (F1ISUC,
            F1ICAH,
            F1BLQA,
            F1IBAC,
            F1THEL,
            F1IUSR,
            F1HORA,
            F1FALT,
            F1ITRL)

       SELECT DISTINCT
           nro_sucursal,
           nro_cuenta,
           FUIBAC,
           4,
           'REGISTRO CIVIL INTERIOR',
           CURRENT_USER,
           (SELECT
         REPLACE(SUBSTRING(CAST(timestamp(now()) as char(19)), 12, 19), '.', '')
               FROM SYSIBM.SYSDUMMY1),
           (SELECT AASFEI FROM SGSYSV),
           (SELECT
                SUBSTRING(JOB_NAME, 16, 24)
            FROM SYSIBM.SYSDUMMY1)

       FROM QTEMP.FALACC

       LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
       LEFT JOIN ACCTAC ON nro_cuenta= FUICAH

       WHERE

       OTITTL IN  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                     13, 14, 15, 16, 17, 18, 19) AND
       FUFBAJ = 0     AND
       FUISGC <> 'IN' AND
       FUIGRC = '04'  AND
       FUIBAC <> 7    AND
       FUIBAC <> 4    AND
       FUIOFI <> 3;

    //Historicos AC bloqueo 2
    exec sql

        INSERT INTO ACCTAB
            (F1ISUC,
             F1ICAH,
             F1BLQA,
             F1IBAC,
             F1THEL,
             F1IUSR,
             F1HORA,
             F1FALT,
             F1ITRL)

        SELECT DISTINCT
            nro_sucursal,
            nro_cuenta,
            FUIBAC,
            2,
            'REGISTRO CIVIL INTERIOR',
            CURRENT_USER,
            (SELECT
         REPLACE(SUBSTRING(CAST(timestamp(now()) as char(19)), 12, 19), '.', '')
            FROM SYSIBM.SYSDUMMY1),
            (SELECT AASFEI FROM SGSYSV),
            (SELECT
                SUBSTRING(JOB_NAME, 16, 24)
             FROM SYSIBM.SYSDUMMY1)

        FROM QTEMP.FALACC

        LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
        LEFT JOIN ACCTAC ON nro_cuenta= FUICAH

        WHERE

        OTITTL IN  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                     13, 14, 15, 16, 17, 18, 19) AND
        FUFBAJ = 0     AND
        FUISGC <> 'IN' AND
        FUIGRC <> '04' AND
        FUIBAC <> 7    AND
        FUIBAC <> 4    AND
        FUIBAC <> 2    AND
        FUIOFI <> 3;

    //Historicos CC bloqueo 2
    exec sql

        INSERT INTO CCCTCB
            (B1ISUC,
             B1ICCC,
             B1BLQA,
             B1IBCC,
             B1THEL,
             B1IUSR,
             B1HORA,
             B1FALT,
             B1ITRL)

        SELECT DISTINCT
            nro_sucursal,
            nro_cuenta,
            BMIBCC,
            1,
            'REGISTRO CIVIL INTERIOR',
            CURRENT_USER,
            (SELECT
         REPLACE(SUBSTRING(CAST(timestamp(now()) as char(19)), 12, 19), '.', '')
            FROM SYSIBM.SYSDUMMY1),
            (SELECT AASFEI FROM SGSYSV),
            (SELECT
                SUBSTRING(JOB_NAME, 16, 24)
             FROM SYSIBM.SYSDUMMY1)

        FROM QTEMP.FALACC

        LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
        LEFT JOIN CCCTCT ON nro_cuenta= BMICCC

        WHERE

        OTITTL IN  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                    13, 14, 15, 16, 17, 18, 19) AND
        BMFBAJ = 0     AND
        BMISGC <> 'IN' AND
        BMIBCC <> 4    AND
        BMIBCC <> 7    AND
        BMIBCC <> 1    AND
        BMIOFI <> 3    AND
        BMIGRC <> 10;

    endsr;

//==============================================================================

    begsr bloquear_ac_cc;

    //Bloquear AC a bloqueo 4

    exec sql

    UPDATE ACCTAC

    SET FUIBAC = 4

    WHERE

    FUICAH IN
            (SELECT DISTINCT
                   nro_cuenta
            FROM QTEMP.FALACC

            LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
            LEFT JOIN ACCTAC ON FUICAH = nro_cuenta

            WHERE

            OTITTL IN  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                     13, 14, 15, 16, 17, 18, 19) AND
            FUFBAJ = 0     AND
            FUISGC <> 'IN' AND
            FUIGRC = '04'  AND
            FUIBAC <> 4    AND
            FUIBAC <> 7    AND
            FUIOFI <> 3);

    //Bloquear AC a bloqueo 2

    exec sql

    UPDATE ACCTAC

    SET FUIBAC = 2

    WHERE

    FUICAH IN
            (SELECT DISTINCT
                   nro_cuenta
            FROM QTEMP.FALACC

            LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
            LEFT JOIN ACCTAC ON FUICAH = nro_cuenta

            WHERE

            OTITTL IN  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                     13, 14, 15, 16, 17, 18, 19) AND
            FUFBAJ = 0     AND
            FUISGC <> 'IN' AND
            FUIGRC <> '04' AND
            FUIBAC <> 4    AND
            FUIBAC <> 7    AND
            FUIBAC <> 2    AND
            FUIOFI <> 3);

    //Bloquear CC a bloqueo 2

    exec sql

    UPDATE CCCTCT

    SET BMIBCC = 1

    WHERE

    BMICCC IN
            (SELECT DISTINCT
                   nro_cuenta
            FROM QTEMP.FALACC

            LEFT JOIN BADCCL ON OTINDO = CAST(dni AS DEC(15, 0))
            LEFT JOIN CCCTCT ON BMICCC = nro_cuenta

            WHERE

            OTITTL IN  (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                     13, 14, 15, 16, 17, 18, 19) AND
            BMFBAJ = 0     AND
            BMISGC <> 'IN' AND
            BMIBCC <> 4    AND
            BMIBCC <> 7    AND
            BMIBCC <> 1    AND
            BMIOFI <> 3    AND
            BMIGRC <> 10);

    endsr;

//==============================================================================

    begsr informar_cuentas_bloquedas;

        exec sql DELETE FROM QTEMP.FALACC;
        exsr obtener_cuentas_clientes;

    endsr;

//==============================================================================

    begsr generar_informe;

    informe_xls = '/home/ANSES/HISTORICOS/'+
                  'Fallecidos_Reg_Civil_Interior' +
                  '_' +
                  ''+ %Trim(sufijo) +'' +
                  '.xls';

    consulta = 'SELECT                            '+
                 'marca_temporal  AS M_TEMPORAL, '+
                 'apellido        AS APELLIDO,   '+
                 'nombre          AS NOMBRE,     '+
                 'dni             AS DNI,        '+
                 'fecha_fallecimiento AS FECHA,  '+
                 'sexo            AS SEXO,       '+
                 'inconsistencia  AS INCONSISTE, '+
                 'observacion     AS OBSERVACIO, '+
                 'subsistema      AS SUBSISTEMA, '+
                 'nro_sucursal    AS SUCURSAL,   '+
                 'nro_cuenta      AS CUENTA,     '+
                 'titular         AS TITUAL,     '+
                 'grupo_cuenta    AS GRUPO,      '+
                 'subgrupo_cuenta AS SUBGRUPO,   '+
                 'saldo_operativo AS SALDO,      '+
                 'fecha_baja      AS FECHA_BAJA, '+
                 'codigo_bloqueo  AS CODIGO,     '+
                 'estado_cuenta   AS ESTADO      '+
               'FROM QTEMP.FALACC                 ';

    cmd =   'EXPPCDBF SQLSTM('''+consulta+''')      '+
            'OUTPAT('''+informe_xls+''')            ';

    rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr guardar_log_en_liserj;

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
              :c1_ds.filename,
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

    begsr enviar_correo;

        cmd = 'SNDMAIL                                                       '+
              '   RECP(''BAQT20C1'')                                         '+
              '   SUBJECT(''Informe de Fallecidos Registro Civil Interior'') '+
              '   MESG(''Se Adjunta Informe de Reg. Civil del Interior'')    '+
              '   FILE('''+%trim(informe_xls)+''')                           ';
        rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr mover_archivo_a_ya_procesado;

         cmd= 'MOV   OBJ('''+%Trim(full_path)+''')'+
              '    TOOBJ('''+%Trim(dest_name)+''')';
         rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr dir_list_close;

        exec sql CLOSE C1;

    endsr;

//==============================================================================

end-proc;

/copy sdb01.src/qrpgsrc,ligdimpl ;
