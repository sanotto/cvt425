*SRCMBRTXT:Padron IFE - ANSES.                    
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

dcl-f ANLIIF03 keyed;

dcl-ds ANLIIF3 likerec(reANLIIF);

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

    dcl-s dest_name            varchar(255);
    dcl-s sufijo               varchar(255);

    dcl-s cmd                  varchar(4096);
    dcl-s full_path            varchar(255);
    dcl-s rc                   char(7);
    dcl-s import_ok            ind;
    dcl-s nombre_liserj        varchar(30);
    dcl-s aasfei               char(8);
    dcl-s MSJ1                 char(55);
    dcl-s MSJ2                 char(55);
    dcl-s MSJ3                 char(55);
    dcl-s MSJ4                 char(55);
    dcl-s MSJ5                 char(55);
    dcl-s MSJ6                 char(55);
    dcl-s MSJ7                 char(55);
    dcl-s MSJ8                 char(55);
    dcl-s beneficio            char(1);
    dcl-s per_padron       char(30);

    dcl-ds contacto_ife qualified;
        nro_beneficio          varchar(11);
        cuil                   varchar(11);
        telefono               varchar(14);
        email                  varchar(50);
        marca_celular          varchar(1);
        banco                  varchar(3);
        agencia                varchar(3);
    end-ds;

    dcl-ds padron_ife qualified;
        cod_banco                   varchar(3);
        cod_agencia                 varchar(3);
        beneficio                   varchar(11);
        ex_caja                     varchar(2);
        tipo_benef                  varchar(1);
        cod_banco_benf              varchar(7);
        coparticipe                 varchar(1);
        benef_ppal                  varchar(11);
        doc_benefcio                varchar(11);
        tipo_doc                    varchar(1);
        numero_documento            varchar(8);
        pcia_emision_docum          varchar(2);
        doc_apoderado               varchar(11);
        tipo_doc_apoder             varchar(1);
        numero_documento_apoder     varchar(8);
        pcia_emision_docum_apoder   varchar(2);
        fecha_liquid                varchar(6);
        ape_nom_benef               varchar(27);
        ape_nom_apode               varchar(27);
        grupo_pago                  varchar(2);
        cuil_benef                  varchar(11);
        domicilio_benef             varchar(62);
        calle                       varchar(25);
        numero                      varchar(5);
        piso                        varchar(2);
        departamento                varchar(3);
        cod_postal                  varchar(4);
        localidad                   varchar(20);
        cod_provincia               varchar(2);
        domicilio_rural             varchar(1);
        fecha_nac_benef             varchar(8);
        nacionalidad_benef          varchar(1);
        sexo_benef                  varchar(1);
        estado_civil                varchar(1);
        tipo_apoderado              varchar(1);
        cuil_apoderado              varchar(11);
        domicilio_apoderado         varchar(62);
        calle_apoder                varchar(25);
        numero_apoder               varchar(5);
        piso_apoder                 varchar(2);
        departamento_apoder         varchar(3);
        cod_postal_apoder           varchar(4);
        localidad_apoder            varchar(20);
        cod_provincia_apoder        varchar(2);
        domicilio_rural_apoder      varchar(1);
        digito_verificador          varchar(1);
        alta_beneficio              varchar(1);
        reactivacion_beneficio      varchar(1);
        cambio_banco_benef          varchar(1);
        cambio_datos_apoderado      varchar(1);
        cambio_domicilio_benef      varchar(1);
        cambio_docum_benef          varchar(1);
        cambio_cuil_benef           varchar(1);
        cambio_ape_nom_benef        varchar(1);
        cambio_sexo_benef           varchar(1);
        cambio_fecha_nac_benef      varchar(1);
        cambio_nacionalidad         varchar(1);
        cambio_estado_civil         varchar(1);
        alta_anses                  varchar(1);
        reserva                     varchar(4);
        cod_sistema                 varchar(2);
        cod_pais_nac                varchar(3);
        reserva_anses               varchar(7);
    end-ds;

    dcl-s path_to_dir char(255);
    dcl-s path_to_out char(255);

    path_to_dir = '/home/ANSES/IMP/';
    path_to_out = '/home/ANSES/HISTORICOS/';

    exec sql SET OPTION COMMIT = *NONE;
    exec sql SELECT AASFEI INTO :aasfei FROM SGSYSV;

    exsr crea_sufijo_de_archivo_ya_procesado;
    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    dow sqlcode = *zero;
        exsr verificar_archivo_procesado;
        exsr armar_paths;
        exsr importar_archivo;
        if import_ok;
            exsr declar_cursor_padron_ife_anses;
            exsr operaciones_correspondientes;
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
        exec sql DROP TABLE QTEMP.ANPIFE;
        exec sql DROP TABLE QTEMP.ANCIFE;

        //Creamos Nueva Tabla.
        exec sql
                 CREATE TABLE QTEMP.ANPIFE (
                    pife      CHAR(300));

        exec sql
                 CREATE TABLE QTEMP.ANCIFE (
                    cife      CHAR(100));

        //Vaciamos Tabla.
        exec sql DELETE FROM QTEMP.ANPIFE;
        exec sql DELETE FROM QTEMP.ANCIFE;

    endsr;

//==============================================================================

    begsr dir_list_open;

        exec sql DECLARE C1 CURSOR FOR
                SELECT
                    CAST (FILENAME as char(255)) AS FILNAM
                FROM TABLE(IFSDIR(:path_to_dir)) AS T
                WHERE
                    UPPER(FILENAME) LIKE '%PIFE%' OR
                    UPPER(FILENAME) LIKE '%BCOSPRO%'
                ORDER BY FILENAME DESC;

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

            exec sql DROP TABLE QTEMP.ANPIFE;

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

        if (%Subst(c1_ds.filename:1:4) = 'PIFE');

        rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '           TOFILE(QTEMP/ANPIFE)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*CRLF)                                   ' +
                '           FLDDLM('';'')                                   ' );

        else;

                rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''') ' +
                '           TOFILE(QTEMP/ANCIFE)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*CRLF)                                   ' +
                '           FLDDLM('';'')                                   ' );

        endif;

        //Eliminar cabeceras.
        if rc = 'CPF0000' ;

            import_ok = *on;

        endif;

    endsr;

//==============================================================================

    begsr declar_cursor_padron_ife_anses;

    if (%Subst(c1_ds.filename:1:4) = 'PIFE');

        exec sql DECLARE padron cursor for
                    SELECT
                          SUBSTR(pife, 1, 3)    AS COD_BANCO,
                          SUBSTR(pife, 4, 3)    AS COD_AGENCIA,
                          SUBSTR(pife, 7, 11)   AS BENEFICIO,
                          SUBSTR(pife, 7, 2)    AS EX_CAJA,
                          SUBSTR(pife, 9, 1)    AS TIPO_BENEF,
                          SUBSTR(pife, 10, 7)   AS COD_BANCO,
                          SUBSTR(pife, 17, 1)   AS COPARTICIPE,
                          SUBSTR(pife, 18, 11)  AS BENEF_PPAL,
                          SUBSTR(pife, 29, 11)  AS DOC_BENEFCIO,
                          SUBSTR(pife, 29, 1)   AS TIPO_DOC,
                          SUBSTR(pife, 30, 8)   AS NUMERO_DOCUMENTO,
                          SUBSTR(pife, 38, 2)   AS PCIA_EMISION_DOCUM,
                          SUBSTR(pife, 40, 11)  AS DOC_APODERADO,
                          SUBSTR(pife, 40, 1)   AS TIPO_DOC,
                          SUBSTR(pife, 41, 8)   AS NUMERO_DOCUMENTO,
                          SUBSTR(pife, 49, 2)   AS PCIA_EMISION_DOCUM,
                          SUBSTR(pife, 51, 6)   AS FECHA_LIQUID,
                          SUBSTR(pife, 57, 27)  AS APE_NOM_BENEF,
                          SUBSTR(pife, 84, 27)  AS APE_NOM_APODE,
                          SUBSTR(pife, 111, 2)  AS GRUPO_PAGO,
                          SUBSTR(pife, 113, 11) AS CUIL_BENEF,
                          SUBSTR(pife, 124, 62) AS DOMICILIO_BENEF,
                          SUBSTR(pife, 124, 25) AS CALLE,
                          SUBSTR(pife, 149, 5)  AS NUMERO,
                          SUBSTR(pife, 154, 2)  AS PISO,
                          SUBSTR(pife, 156, 3)  AS DEPARTAMENTO,
                          SUBSTR(pife, 159, 4)  AS COD_POSTAL,
                          SUBSTR(pife, 163, 20) AS LOCALIDAD,
                          SUBSTR(pife, 183, 2)  AS COD_PROVINCIA,
                          SUBSTR(pife, 185, 1)  AS DOMICILIO_RURAL,
                          SUBSTR(pife, 186, 8)  AS fecha_nac_benef,
                          SUBSTR(pife, 194, 1)  AS NACIONALIDAD_BENEF,
                          SUBSTR(pife, 195, 1)  AS SEXO_BENEF,
                          SUBSTR(pife, 196, 1)  AS ESTADO_CIVIL,
                          SUBSTR(pife, 197, 1)  AS TIPO_APODERADO,
                          SUBSTR(pife, 198, 11) AS CUIL_APODERADO,
                          SUBSTR(pife, 209, 62) AS DOMICILIO_APODERADO,
                          SUBSTR(pife, 209, 25) AS CALLE,
                          SUBSTR(pife, 234, 5)  AS NUMERO,
                          SUBSTR(pife, 239, 2)  AS PISO,
                          SUBSTR(pife, 241, 3)  AS DEPARTAMENTO,
                          SUBSTR(pife, 244, 4)  AS COD_POSTAL,
                          SUBSTR(pife, 248, 20) AS LOCALIDAD,
                          SUBSTR(pife, 268, 2)  AS COD_PROVINCIA,
                          SUBSTR(pife, 270, 1)  AS DOMICILIO_RURAL,
                          SUBSTR(pife, 271, 1)  AS DIGITO_VERIFICADOR,
                          SUBSTR(pife, 272, 1)  AS ALTA_BENEFICIO,
                          SUBSTR(pife, 273, 1)  AS REACTIVACION_BENEFICIO,
                          SUBSTR(pife, 274, 1)  AS CAMBIO_BANCO_BENEF,
                          SUBSTR(pife, 275, 1)  AS CAMBIO_DATOS_APODERADO,
                          SUBSTR(pife, 276, 1)  AS CAMBIO_DOMICILIO_BENEF,
                          SUBSTR(pife, 277, 1)  AS CAMBIO_DOCUM_BENEF,
                          SUBSTR(pife, 278, 1)  AS CAMBIO_CUIL_BENEF,
                          SUBSTR(pife, 279, 1)  AS CAMBIO_APE_NOM_BENEF,
                          SUBSTR(pife, 280, 1)  AS CAMBIO_SEXO_BENEF,
                          SUBSTR(pife, 281, 1)  AS CAMBIO_FECHA_NAC_BENEF,
                          SUBSTR(pife, 282, 1)  AS CAMBIO_NACIONALIDAD,
                          SUBSTR(pife, 283, 1)  AS CAMBIO_ESTADO_CIVIL,
                          SUBSTR(pife, 284, 1)  AS ALTA_ANSES,
                          SUBSTR(pife, 285, 4)  AS RESERVA,
                          SUBSTR(pife, 289, 2)  AS COD_SISTEMA,
                          SUBSTR(pife, 291, 3)  AS COD_PAIS_NAC,
                          SUBSTR(pife, 294, 7)  AS RESERVA_ANSES
                    FROM QTEMP.ANPIFE;

        else;

        exec sql DECLARE contacto cursor for
                    SELECT
                          SUBSTR(cife, 1, 11)  AS NRO_BENEFICIO,
                          SUBSTR(cife, 12, 11) AS CUIL,
                          CASE
                               WHEN SUBSTR(cife, 23, 5) = '00380' THEN
                               SUBSTR(cife, 25, 3) || SUBSTR(cife, 30, 7)
                               WHEN SUBSTR(cife, 23, 5) = '00264' THEN
                               SUBSTR(cife, 25, 3) || SUBSTR(cife, 30, 7)
                               WHEN SUBSTR(cife, 23, 5) = '00011' THEN
                               SUBSTR(cife, 26, 2) || SUBSTR(cife, 29, 8)
                          ELSE SUBSTR(cife, 24, 4) || SUBSTR(cife, 31, 6) END
                          AS TELEFONO,
                          SUBSTR(cife, 37, 50) AS EMAIL,
                          SUBSTR(cife, 87, 1)  AS MARCA_CELULAR,
                          SUBSTR(cife, 88, 3)  AS BANCO,
                          SUBSTR(cife, 91, 3)  AS AGENCIA
                    FROM QTEMP.ANCIFE;

        endif;

    endsr;

//==============================================================================

    begsr operaciones_correspondientes;

    if (%Subst(c1_ds.filename:1:4) = 'PIFE');

        exec sql OPEN padron;
        exec sql FETCH padron INTO :padron_ife;
        Dow sqlcod = *Zero;
            exsr verificar_existencia_en_padron;
            if (beneficio = '1');
                exsr informar_en_padron_anliif;
            endif;
            exec sql FETCH padron INTO :padron_ife;
        enddo;
        exec sql CLOSE padron;

    else;

        exec sql OPEN contacto;
        exec sql FETCH contacto INTO :contacto_ife;
        Dow sqlcod = *Zero;
            exsr actualizar_datos_contacto;
            exec sql FETCH contacto INTO :contacto_ife;
        enddo;
        exec sql CLOSE contacto;

    endif;

    endsr;

//==============================================================================

    begsr verificar_existencia_en_padron;

        //Verificar Beneficio ANLIIF.
        chain (%Dec(padron_ife.numero_documento:8:0):%Trim(c1_ds.filename))
               ANLIIF03 ANLIIF3;
        if not %found();
            beneficio = '1';
        else;
            beneficio = '2';
        endif;

        per_padron = %Subst(c1_ds.filename:9:4) + %Subst(c1_ds.filename:7:2);

    endsr;

//==============================================================================

    begsr informar_en_padron_anliif;

        exec sql INSERT INTO ANLIIF(IFFPRE,
                                    IFFALT,
                                    IFHALT,
                                    IFIUSR,
                                    IFNCB1,
                                    IFINDO,
                                    IFNCAL,
                                    IFIPUE,
                                    IFICPO,
                                    IFNLOC,
                                    IFFNAC,
                                    IFCUIA,
                                    IFISEX,
                                    IFRENG,
                                    IFICBP,
                                    IFIABP,
                                    IFICPR,
                                    IFIECJ,
                                    IFITBE,
                                    IFINBE,
                                    IFIDVA)
                 VALUES(CAST(:aasfei AS DEC(8, 0)),
                        CAST(:aasfei AS DEC(8, 0)),
                        (SELECT HOUR(NOW())*10000+MINUTE(NOW())*100
                        + SECOND(NOW()) FROM  SYSIBM/SYSDUMMY1 ),
                        CURRENT_USER,
                        SUBSTR(:padron_ife.ape_nom_benef, 1, 55),
                        CAST(:padron_ife.numero_documento AS DEC(11, 0)),
                        SUBSTR(:padron_ife.calle, 1, 30),
                        CASE
                            WHEN :padron_ife.numero = 'S/N'    THEN 999999
                            WHEN :padron_ife.numero = ''       THEN 999999
                            WHEN :padron_ife.numero LIKE '%/%' THEN
                            CAST(REPLACE(:padron_ife.numero, '/', '')
                            AS DEC(6, 0))
                        ELSE CAST(:padron_ife.numero AS DEC(6, 0)) END,
                        CAST(:padron_ife.cod_postal AS DEC(5, 0)),
                        SUBSTR(:padron_ife.localidad, 1, 30),
                        CAST(:padron_ife.fecha_nac_benef AS DEC(8, 0)),
                        CAST(:padron_ife.cuil_benef AS DEC(11, 0)),
                        :padron_ife.sexo_benef,
                        :c1_ds.filename,
                        CAST(:padron_ife.cod_banco AS DEC(3, 0)),
                        CAST(:padron_ife.cod_agencia AS DEC(3, 0)),
                        '00' || :padron_ife.cod_provincia,
                        CAST(:padron_ife.ex_caja AS DEC(2, 0)),
                        CAST(:padron_ife.tipo_benef AS DEC(1, 0)),
                        CAST(:padron_ife.cod_banco_benf AS DEC(7, 0)),
                        CAST(:padron_ife.coparticipe AS DEC(1, 0))
                        );

    endsr;

//==============================================================================

    begsr actualizar_datos_contacto;

    //Actualizar Datos de Contacto - ANLIIF.
    if (contacto_ife.telefono = '');
        exec sql UPDATE ANLIIF
                      SET IFCOEL = :contacto_ife.email
                 WHERE IFCUIA = CAST(:contacto_ife.cuil AS DEC(11, 0)) AND
                       IFFALT = CAST(:aasfei AS DEC(8, 0));
    else;
        exec sql UPDATE ANLIIF
                      SET IFCOEL = :contacto_ife.email,
                          IFITEL = CAST(:contacto_ife.telefono AS DEC(15, 0))
                 WHERE IFCUIA = CAST(:contacto_ife.cuil AS DEC(11, 0)) AND
                       IFFALT = CAST(:aasfei AS DEC(8, 0));
    endif;

  //Actualizar Teléfono.
  //if (contacto_ife.telefono <> '');
  //    exec sql UPDATE BAPFIS
  //                  SET A#ITEL = CAST(:contacto_ife.telefono AS DEC(15, 0)),
  //                      A#FMOD = CAST(:aasfei AS DEC(8, 0))
  //             WHERE A#ICUI = CAST(:contacto_ife.cuil AS DEC(11, 0));
  //endif;

  //Actualizar Correo - BAPFDA.
  //if (contacto_ife.email <> '');
  //    exec sql UPDATE BAPFDA
  //                  SET QQCOEL = :contacto_ife.email,
  //                      QQIUSR = CURRENT_USER
  //             WHERE
  //             QQINDO = SUBSTR(:contacto_ife.cuil, 3, 8) AND
  //             (QQCOEL LIKE '%nocorreo%'  OR QQCOEL LIKE '%NOCORREO%'      OR
  //              QQCOEL LIKE '%NOMAIL%'    OR QQCOEL LIKE '%SINCORREO%'     OR
  //              QQCOEL LIKE '%mail@mai%'  OR QQCOEL LIKE '%MAIL@MAI%'      OR
  //              QQCOEL LIKE '%NO@NO%'     OR QQCOEL LIKE '%nm@nm%'         OR
  //              QQCOEL LIKE '%N@N.COM%'   OR QQCOEL LIKE '%NC@NC.COM%'     OR
  //              QQCOEL LIKE '%nm@nnm%'    OR QQCOEL LIKE '%MAIL@MIAL.COM%' OR
  //              QQCOEL LIKE '%nc@nc.com%' OR QQCOEL LIKE '%n@gmail.com%'   OR
  //              QQCOEL LIKE '%NM@NM.COM%' OR QQCOEL LIKE '%notiene%'       OR
  //              QQCOEL = 'N@GMAIL.COM'    OR QQCOEL = 'nm@gmail.com'       OR
  //              QQCOEL LIKE '%n@n.com%'   OR QQCOEL LIKE '%noposee%'       OR
  //              QQCOEL = '');
  //endif;

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

        if (%Subst(c1_ds.filename:1:8) = 'BCOSPROV');

        cmd = 'SNDMAIL                                                 '+
              '   RECP(''BAQT20C1'')                                   '+
              '   SUBJECT(''Padrón IFE-ANSES'')                        '+
              '   MESG(''Se procesarón los archivos correspondientes al'+
                       ' periodo '+%Trim(per_padron)+'.'')';
        rc=exeCmd(cmd);

        endif;

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
