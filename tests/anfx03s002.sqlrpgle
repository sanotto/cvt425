*SRCMBRTXT:Retenciones Masivas - ANSES.           
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

dcl-f BAEXEP03 keyed;
dcl-f ANLIHA10 keyed;

dcl-ds BAEXEP3 likerec(reBAEXEP) ;
dcl-ds ANLIHA0 likerec(reANLIHA) ;

//==============================================================================
//Declaración de prototipos, importadas de los miembros declarados.
//==============================================================================

/include sdb01.src/qrpgsrc,ligdifs ;
/include sdb01.src/qrpgsrc,ligdprot ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

    //*Entry plist.
    dcl-ds c1_ds qualified;
        filename               varchar(255);
    end-ds;

    //Llamada al Programa SBBAINFE.
    dcl-pr SBBAINFE extpgm('SBBAINFE');
        PAFECH                  packed(8:0);
        PACINV                  char(2);
    end-pr;

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

    dcl-ds maestro_reten qualified;
       nro_beneficio        char(50);
       apellido_nombre      char(50);
       nro_documento        char(50);
       cuil                 char(50);
       ag_pagador           char(50);
       liquidacion          char(50);
       importe              char(50);
    end-ds;

    dcl-s dest_name            varchar(255);
    dcl-s sufijo               varchar(255);

    dcl-s cmd                  varchar(4096);
    dcl-s full_path            varchar(255);
    dcl-s rc                   char(7);
    dcl-s import_ok            ind;
    dcl-s nombre_liserj        varchar(30);
    dcl-s aasfei               char(8);
    dcl-s aasfen               char(8);

    dcl-s retencion            char(1);
    dcl-s maestro_retenciones  char(1);
    dcl-s maestro_anses        char(1);
    dcl-s ex_caja              packed(2);
    dcl-s tipo_beneficio       packed(1);
    dcl-s nro_ben              packed(7);
    dcl-s coparticipe          packed(1);
    dcl-s dg                   packed(1);
    dcl-s imp                  char(12);
    dcl-s periodo              packed(6);

    dcl-s MSJ1                 char(55);
    dcl-s MSJ2                 char(55);
    dcl-s MSJ3                 char(55);
    dcl-s MSJ4                 char(55);
    dcl-s MSJ5                 char(55);
    dcl-s MSJ6                 char(55);
    dcl-s MSJ7                 char(55);
    dcl-s MSJ8                 char(55);

    dcl-s PAFECH               packed(8:0);
    dcl-s PACINV               char(2);

    dcl-s consulta             varchar(8000);
    dcl-s informe_xls          varchar(255);

    dcl-s path_to_dir char(255);
    dcl-s path_to_out char(255);

    path_to_dir = '/home/ANSES/IMP/';
    path_to_out = '/home/ANSES/HISTORICOS/';

    exec sql SET OPTION COMMIT = *NONE;
    exec sql SELECT AASFEI, AASFEN INTO :aasfei, :aasfen FROM SGSYSV;

    exsr crea_sufijo_de_archivo_ya_procesado;
    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    dow sqlcode = *zero;
        exsr verificar_archivo_procesado;
        exsr vaciar_tablas_de_trabajo;
        exsr armar_paths;
        exsr importar_archivo;
        if import_ok;
            exsr declar_cursor;
            exsr realizar_operaciones_correspondientes;
            exsr generar_informe;
            exsr guardar_log_en_liserj;
            exsr enviar_correo;
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
                 FROM SYSIBM/SYSDUMMY1 ;

    endsr;

//==============================================================================

    begsr crear_tablas_de_trabajo;

        //Eliminamos Tablas Existentes.
        exec sql DROP TABLE QTEMP.ANMTRT;
        exec sql DROP TABLE QTEMP.ANMTEX;

        //Creamos Nueva Tabla.
        exec sql
                 CREATE TABLE QTEMP.ANMTRT (
                    nro_beneficio      CHAR(50),
                    apellido_nombre    CHAR(50),
                    nro_documento      CHAR(50),
                    cuil               CHAR(50),
                    ag_pagador         CHAR(50),
                    liquidacion        CHAR(50),
                    importe            CHAR(50));

        exec sql CREATE TABLE QTEMP.ANMTEX (
                    nro_beneficio      CHAR(50),
                    apellido_nombre    CHAR(50),
                    tpo_documento      CHAR(50),
                    nro_documento      CHAR(50),
                    cuil               CHAR(50),
                    ag_pagador         CHAR(50),
                    liquidacion        CHAR(50),
                    importe            CHAR(50),
                    observacion        CHAR(50));

    endsr;

//==============================================================================

    begsr dir_list_open;

        exec sql DECLARE C1 CURSOR FOR
                SELECT
                    CAST (FILENAME as char(255)) AS FILNAM
                FROM TABLE(IFSDIR(:path_to_dir)) AS T
                WHERE
                    UPPER(FILENAME) LIKE '%RETEN%';

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

            exec sql DROP TABLE QTEMP.ANMTRT;
            exec sql DROP TABLE QTEMP.ANMTEX;

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

    begsr vaciar_tablas_de_trabajo;

        //Vaciamos Tabla.
        exec sql DELETE FROM QTEMP.ANMTRT;
        exec sql DELETE FROM QTEMP.ANMTEX;

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

        rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '           TOFILE(QTEMP/ANMTRT)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*CRLF)                                   ' +
                '           FLDDLM('','')                                   ' );

        //Eliminar cabeceras.
        if rc = 'CPF0000' ;

            import_ok = *on;

        endif;

    endsr;

//==============================================================================

    begsr declar_cursor;

        exec sql DECLARE retenciones CURSOR FOR
                    SELECT
                        REPLACE(nro_beneficio, '-', '') AS nro_beneficio,
                        apellido_nombre,
                        nro_documento,
                        REPLACE(cuil, '-', '') AS cuil,
                        REPLACE(ag_pagador, '-', '') AS ag_pagador,
                        liquidacion,
                        importe
                    FROM QTEMP.ANMTRT;

    endsr;

//==============================================================================

    begsr realizar_operaciones_correspondientes;

        exec sql OPEN retenciones;
        exec sql FETCH retenciones INTO :maestro_reten;
        Dow sqlcod = *zero;
            exsr verificar_existencia_en_maestro_de_excepciones;
            exsr grabar_en_maestro_de_excepciones;
            exsr inicializar_en_maestro_anses_y_eliminar_acreditacion;
            exsr grabar_retenciones_en_maestro_de_retenciones;
            exsr grabar_retenciones_para_informe;
            exec sql FETCH retenciones INTO :maestro_reten;
        enddo;
        exec sql CLOSE retenciones;

    endsr;

//==============================================================================

    begsr verificar_existencia_en_maestro_de_excepciones;

        ex_caja = %Dec(%Subst(maestro_reten.nro_beneficio:1:2):2:0);
        tipo_beneficio = %Dec(%Subst(maestro_reten.nro_beneficio:3:1):1:0);
        nro_ben = %Dec(%Subst(maestro_reten.nro_beneficio:4:7):7:0);
        coparticipe = %Dec(%Subst(maestro_reten.nro_beneficio:11:1):1:0);
        dg = %Dec(%Subst(maestro_reten.nro_beneficio:12:1):1:0);

        periodo = %Dec(%Subst(c1_ds.filename:13:6):6:0);

        chain ('AN':%Dec(%Subst(maestro_reten.nro_beneficio:1:11):11:0))
               BAEXEP03 BAEXEP3;
        if not %found();
                maestro_retenciones = '1';
            else;
                maestro_retenciones = '2';
        endif;

    endsr;

//==============================================================================

    begsr grabar_en_maestro_de_excepciones;

        if (maestro_retenciones = '1');

        exec sql INSERT INTO BAEXEP03(
                    EXISUB,
                    EXISUC,
                    EXICAH,
                    EXICCC,
                    EXIMCC,
                    EXIMCA,
                    EXOBSO,
                    EXFALT,
                    EXHALT,
                    EXIUSR)

        VALUES('AN',
               0,
               CAST(SUBSTR(:maestro_reten.nro_beneficio, 1, 11) AS DEC(11, 0)),
               0,
               SUBSTR(:periodo, 1, 3),
               SUBSTR(:periodo, 4, 3),
               REPLACE(:c1_ds.filename, '.TXT', ''),
               CAST(:aasfei AS DEC(8, 0)),
               (SELECT HOUR(NOW())*10000+MINUTE(NOW())*100 +SECOND(NOW())
               FROM  SYSIBM/SYSDUMMY1 ),
               CURRENT_USER);

        endif;

    endsr;

//==============================================================================

    begsr inicializar_en_maestro_anses_y_eliminar_acreditacion;

        retencion = '1';

        chain (periodo:ex_caja:tipo_beneficio:nro_ben:coparticipe:dg)
               ANLIHA10 ANLIHA0;
        if not %found();
                maestro_anses = '1';
            else;
                maestro_anses = '2';
        endif;

        PAFECH = ANLIHA0.ANFDPA;

        PACINV = 'NI';

        SBBAINFE(PAFECH
                :PACINV);

        if (maestro_anses = '2' AND PAFECH > %Dec(aasfei:8:0) AND
            ANLIHA0.ANICAH <> 0);

            exec sql DELETE FROM ACMOVD
                        WHERE GCICAH = :ANLIHA0.ANICAH;

            exec sql UPDATE ANLIHA SET ANISUC = 0,
                                       ANICAH = 0
                     WHERE ANFEPR = :periodo AND
                           ANINBE = :nro_ben;

            retencion = '2';

        endif;

    endsr;

//==============================================================================

    begsr grabar_retenciones_en_maestro_de_retenciones;

    imp = %Trim(%EditW(%Dec(
          %Scanrpl(';':'':maestro_reten.importe):12:0):'        0 ,  '));

        exec sql INSERT INTO ANRETE (
                        ARFALT,
                        ARHALT,
                        ARIUSR,
                        ARIECJ,
                        ARITBE,
                        ARINBE,
                        ARIICP,
                        ARIDVA,
                        ARNYAA,
                        ARINDA,
                        ARCUIA,
                        ARICBP,
                        ARIABP,
                        ARIOLM,
                        AR$LQ2,
                        ANFEPR,
                        ANRENG)

      VALUES (
             :aasfei,
             (SELECT HOUR(NOW())*10000+MINUTE(NOW())*100 +SECOND(NOW())
             FROM SYSIBM/SYSDUMMY1 ),
             CURRENT_USER,
             :ex_caja,
             :tipo_beneficio,
             :nro_ben,
             :coparticipe,
             :dg,
             SUBSTR(:maestro_reten.apellido_nombre, 1, 27),
             CAST(SUBSTR(:maestro_reten.nro_documento, 3, 8) AS DEC(8, 0)),
             CAST(:maestro_reten.cuil AS DEC (11, 0)),
             CAST(SUBSTR(:maestro_reten.ag_pagador, 1, 3) AS DEC(3, 0)),
             CAST(SUBSTR(:maestro_reten.ag_pagador, 4, 6) AS DEC(3, 0)),
             SUBSTR(:maestro_reten.liquidacion, 1, 1),
             :imp,
             :periodo,
             REPLACE(:c1_ds.filename, '.TXT', '')
             );

    endsr;

//==============================================================================

    begsr grabar_retenciones_para_informe;

        exec sql INSERT INTO QTEMP.ANMTEX(
                        nro_beneficio,
                        apellido_nombre,
                        tpo_documento,
                        nro_documento,
                        cuil,
                        ag_pagador,
                        liquidacion,
                        importe,
                        observacion)

                 VALUES(
                        :maestro_reten.nro_beneficio,
                        :maestro_reten.apellido_nombre,
                        SUBSTR(:maestro_reten.nro_documento, 1, 3),
                        REPLACE(:maestro_reten.nro_documento, 'DU', ''),
                        :maestro_reten.cuil,
                        :maestro_reten.ag_pagador,
                        :maestro_reten.liquidacion,
                        :imp,
                        CASE
                            WHEN :retencion = '2' THEN 'Se Aplico Retencion'
                        ELSE 'No se Aplico Retencion' END);

    endsr;

//==============================================================================

    begsr generar_informe;

        informe_xls = '/home/ANSES/HISTORICOS/'  +
                      %Subst(c1_ds.filename:1:25)+
                      '.xls';

        consulta = 'SELECT                                          '+
                    'CAST(nro_beneficio AS CHAR(20)) AS NRO_BENEF,  '+
                    'CAST(apellido_nombre AS CHAR(35)) AS APE_NOM,  '+
                    'CAST(tpo_documento AS CHAR(10)) AS TPO_DOC,    '+
                    'CAST(nro_documento AS CHAR(15)) AS NRO_DOC,    '+
                    'CAST(cuil AS CHAR(15)) AS CUIL,                '+
                    'CAST(ag_pagador AS CHAR(10)) AS AG_PAGADOR,    '+
                    'CAST(liquidacion AS CHAR(10)) AS LIQ,          '+
                    'CAST(importe AS CHAR(15)) AS IMPORTE,          '+
                    'CAST(observacion AS CHAR(35)) AS OBS           '+
                   'FROM QTEMP.ANMTEX';

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
              '   SUBJECT('''+%Subst(c1_ds.filename:1:25)+''')               '+
              '   MESG(''Se proceso archivo de retenciones'')                '+
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
