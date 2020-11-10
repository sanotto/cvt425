*SRCMBRTXT:Manejador de Proc. de Adhesion de Servi
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

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

    //*Entry plist.

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
    dcl-s cabecera             varchar(25);
    dcl-s aasfei               packed(8:0);
    dcl-s cuenta_cliente       char(9);
    dcl-s grupo_cliente        char(2);
    dcl-s sub_grupo_cliente    char(2);
    dcl-s MSJ1                 char(55);
    dcl-s MSJ2                 char(55);
    dcl-s MSJ3                 char(55);
    dcl-s MSJ4                 char(55);
    dcl-s MSJ5                 char(55);
    dcl-s MSJ6                 char(55);
    dcl-s MSJ7                 char(55);
    dcl-s MSJ8                 char(55);

    dcl-ds cuentas_bajas       qualified;
           cuentas             varchar(11);
           fecha_baja          packed(8:0);
           hora_baja           packed(6:0);
           usr_baja            varchar(10);
    end-ds;

    dcl-ds correo_electronico  qualified;
           cuentas             varchar(11);
           correo              varchar(50);
           t_doc               varchar(2);
           n_doc               varchar(15);
    end-ds;

    dcl-ds clientes_alta_ca_cc qualified;
           fecha_alta          packed(8:0);
           fecha_procesamiento packed(8:0);
           red                 char(10);
           canal               char(10);
           servicio            char(10);
           tipo_extracto       char(10);
           codigo_subsistema   char(10);
           sucursal            char(1);
           cuenta              char(9);
    end-ds;

    dcl-s path_to_dir char(255);
    dcl-s path_to_out char(255);

    path_to_dir = '/home/LINK/';
    path_to_out = '/home/LINK/Procesados/';

    exsr crea_sufijo_de_archivo_ya_procesado;
    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    dow sqlcode = *zero;
        exsr verificar_archivo_procesado;
        exsr armar_paths;
        exsr limpia_tablas_de_trabajo;
        exsr importar_archivo_lnk;
        if import_ok;
            exsr insertar_cuentas_maestro_adhsesion;
            exsr actualizar_bajas_maestro_adhesion;
            exsr generar_alta_cc_ca;
            exsr actulizar_correo_electronico;
            exsr guardar_log_en_liserj;
            exsr mover_archivo_a_ya_procesado;
        endif;
        exsr dir_list_fetch;
    enddo;
    exsr dir_list_close;

    return;

//==============================================================================

    begsr armar_paths;

        dest_name = %Trim(path_to_out)+
                    %Trim(c1_ds.filename)+
                    '_' +
                    ''+%Trim(sufijo);

        full_path = %trim(path_to_dir)+%trim(c1_ds.filename);

    endsr;

//==============================================================================

    begsr dir_list_open;

        exec sql SET OPTION COMMIT = *NONE;

        exec sql DECLARE C1 CURSOR FOR
                SELECT
                    CAST (FILENAME as char(255)) AS FILNAM
                FROM TABLE(IFSDIR(:path_to_dir)) AS T
                WHERE
                    UPPER(FILENAME) LIKE '%EGDE%';

        exec sql OPEN C1;

    endsr;

//==============================================================================

    begsr verificar_archivo_procesado;

        exec sql
            SELECT RJDACO
                INTO :nombre_liserj
            FROM LISERJ
            WHERE RJDACO = :c1_ds.filename;

        if sqlcod = *zero;

        exec sql DROP TABLE QTEMP.GDERMC;

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

    begsr dir_list_fetch;

        exec sql FETCH C1 into :c1_ds;

    endsr;

//==============================================================================

    begsr dir_list_close;

        exec sql CLOSE C1;

    endsr;

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

        //Crear una tablas temporales para GDE: Resumen de Cuentas

        exec sql DROP TABLE QTEMP.GDERMC;

        exec sql
                 CREATE TABLE QTEMP.GDERMC (
                    arch_adh_serv      CHAR(121));

    endsr;

//==============================================================================

    begsr limpia_tablas_de_trabajo;

        exec sql DELETE FROM QTEMP.GDERMC;

    endsr;

//==============================================================================

    begsr insertar_cuentas_maestro_adhsesion;

        exec sql
                INSERT INTO LIGDEA (
                    LGFING,
                    LGFPRE,
                    LGAPRO,
                    LGCLTR,
                    LGIPRO,
                    LGILDI,
                    LGISUB,
                    LGISUC,
                    LGICCL,
                    LGCOEL,
                    LGFALT,
                    LGHALT,
                    LGIUSR,
                    LGACCL,
                    LGRENG)

               SELECT
                    (SELECT AASFEI FROM SGSYSV LIMIT 1),
                    SUBSTRING(:cabecera, 18, 8),
                    'LNK',
                    SUBSTRING(arch_adh_serv, 101, 2),
                    'RC',
                    SUBSTRING(:cabecera, 8, 6),
                    CASE
                        WHEN SUBSTRING(arch_adh_serv, 14, 11) = FUICAH
                        THEN 'AC'
                        WHEN SUBSTRING(arch_adh_serv, 14, 11) = BMICCC
                        THEN 'CC'
                    ELSE '-' END,
                    CASE
                        WHEN SUBSTRING(arch_adh_serv, 14, 11) = FUICAH
                        THEN FUISUC
                        WHEN SUBSTRING(arch_adh_serv, 14, 11) = BMICCC
                        THEN BMISUC
                    ELSE ' ' END,
                    CASE
                        WHEN SUBSTRING(arch_adh_serv, 14, 11) = FUICAH
                        THEN FUICCL
                        WHEN SUBSTRING(arch_adh_serv, 14, 11) = BMICCC
                        THEN BMICCL
                    ELSE ' ' END,
                    SUBSTRING(arch_adh_serv, 33, 50),
                    SUBSTRING(arch_adh_serv, 83, 8),
                    SUBSTRING(arch_adh_serv, 91, 6),
                    CURRENT_USER,
                    SUBSTRING(arch_adh_serv, 97, 4),
                    :c1_ds.filename

                FROM QTEMP.GDERMC

                LEFT JOIN ACCTAC ON FUICAH = SUBSTRING(arch_adh_serv, 14, 11)
                                AND FUFBAJ = 0
                LEFT JOIN CCCTCT ON BMICCC = SUBSTRING(arch_adh_serv, 14, 11)
                                AND BMFBAJ = 0
                LEFT JOIN LIGDEA ON LGICCL = SUBSTRING(arch_adh_serv, 14, 11)

                WHERE

                CASE WHEN LGICCL IS NOT NULL THEN
                'YA_EXISTE'
                ELSE
                'NO_EXISTE'
                END = 'NO_EXISTE';

    endsr;

//==============================================================================

    begsr actualizar_bajas_maestro_adhesion;

        exec sql
                declare cursor_cuentas_bajas cursor for
                SELECT
                    LGICCL,
                    (SELECT AASFEI FROM SGSYSV LIMIT 1),
                    (SELECT HOUR(NOW())*10000+MINUTE(NOW())*100+SECOND(NOW())
                    FROM SYSIBM/SYSDUMMY1 ),
                    CURRENT_USER
                FROM LIGDEA
                WHERE NOT EXISTS(
                               SELECT *
                               FROM QTEMP.GDERMC
                               WHERE SUBSTRING(arch_adh_serv, 14, 11) = LGICCL);

                exec sql open cursor_cuentas_bajas;
                exec sql fetch  cursor_cuentas_bajas into :cuentas_bajas;

                dow sqlcod = *Zero;

                exec sql
                        UPDATE LIGDEA

                            SET LGFBAJ = :cuentas_bajas.fecha_baja,
                                LGHBAJ = :cuentas_bajas.hora_baja,
                                LGIUSB = :cuentas_bajas.usr_baja
                        WHERE
                        LGICCL IN :cuentas_bajas.cuentas;

                exec sql fetch cursor_cuentas_bajas into :cuentas_bajas;

                enddo;

                exec sql close cursor_cuentas_bajas;

    endsr;

//==============================================================================

    begsr generar_alta_cc_ca;

        //Recupera fecha actual.
        exec sql SELECT
                    AASFEI
                 INTO :aasfei
                 FROM SGSYSV;

        //Declaro cursor.
        exec sql
                declare cursor_clientes_alta_ca_cc cursor for
                SELECT
                    LGFING,
                    LGFPRE,
                    LGAPRO,
                    LGCLTR,
                    LGIPRO,
                    LGILDI,
                    LGISUB,
                    LGISUC,
                    LGICCL
                FROM LIGDEA
                WHERE
                LGFING = :aasfei;

        exec sql open cursor_clientes_alta_ca_cc;
        exec sql fetch cursor_clientes_alta_ca_cc into :clientes_alta_ca_cc;

        dow sqlcod = *Zero;

            if (clientes_alta_ca_cc.codigo_subsistema = 'CC');

                exec sql UPDATE CCCTCT
                            SET BMICRE = 3
                         WHERE
                         BMICCC = :clientes_alta_ca_cc.cuenta AND
                         BMFBAJ = 0;

            else;

            exec sql SELECT
                        FUICAH,
                        FUIGRC,
                        FUISGC
                     INTO :cuenta_cliente, :grupo_cliente, :sub_grupo_cliente
                     FROM ACCTAC
                     WHERE
                     FUICAH = :clientes_alta_ca_cc.cuenta AND
                     FUFBAJ = 0;

                if (sub_grupo_cliente <> 'GU');

                exec sql UPDATE ACCTAC
                            SET FUICRE = CASE
                                            WHEN FUISGC = 'MN' THEN 5
                                            WHEN FUISGC = 'AD' THEN 3
                                            WHEN FUISGC = 'CE' THEN 5
                                            WHEN FUIGRC = '01' THEN 5
                                            WHEN FUIGRC = '04' THEN 6
                                            WHEN FUIGRC = '03' THEN 3
                                            WHEN FUIGRC = '05' THEN 5
                                            WHEN FUIGRC NOT IN ('01', '03',
                                                                '04', '05',
                                                                '70', '98',
                                                                '99')
                                            THEN 6 ELSE 0 END

                         WHERE
                         FUICAH = :clientes_alta_ca_cc.cuenta AND
                         FUFBAJ = 0;

                endif;

            endif;

        exec sql fetch cursor_clientes_alta_ca_cc into :clientes_alta_ca_cc;

        enddo;

        exec sql close cursor_clientes_alta_ca_cc;

    endsr;

//==============================================================================

    begsr actulizar_correo_electronico;

        exec sql
                declare cursor_correos_electronicos cursor for
                SELECT
                    SUBSTRING(arch_adh_serv, 14, 11),
                    SUBSTRING(arch_adh_serv, 33, 50),
                    OTITDO,
                    OTINDO
                FROM QTEMP.GDERMC
                INNER JOIN BADCCL ON OTICCL = SUBSTRING(arch_adh_serv, 14, 11)
                INNER JOIN BAPFDA ON QQITDO = OTITDO AND QQINDO = OTINDO;

        exec sql open cursor_correos_electronicos;
        exec sql fetch cursor_correos_electronicos into :correo_electronico;

        dow sqlcod = *Zero;

         exec sql
                UPDATE BAPFDA

                    SET QQCOEL = :correo_electronico.correo

                    WHERE
                    QQINDO = :correo_electronico.n_doc AND
                    (QQCOEL LIKE '%nocorreo%' OR QQCOEL LIKE '%NOCORREO%'  OR
                     QQCOEL LIKE '%NOMAIL%'   OR QQCOEL LIKE '%SINCORREO%' OR
                     QQCOEL = ' ');

        exec sql fetch cursor_correos_electronicos into :correo_electronico;

        enddo;

        exec sql close cursor_correos_electronicos;

    endsr;

//==============================================================================

    begsr importar_archivo_lnk;

        import_ok = *off;

        rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '           TOFILE(QTEMP/GDERMC)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*LF)                                     ' );

        //Eliminar cabeceras.
        if rc = 'CPF0000' ;

            exec sql SELECT
                        SUBSTRING(arch_adh_serv, 1, 25)
                        INTO :cabecera
                     FROM QTEMP.GDERMC
                     LIMIT 1;

            exec sql
                DELETE FROM QTEMP.GDERMC WHERE RRN(GDERMC) = 1;

            exec sql
                DELETE FROM QTEMP.GDERMC WHERE ARCH_ADH_SERV =
                (SELECT * FROM QTEMP.GDERMC
                 ORDER BY ARCH_ADH_SERV DESC
                 LIMIT 1);

            import_ok = *on;

        endif;

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

    begsr mover_archivo_a_ya_procesado;

         cmd= 'MOV   OBJ('''+%Trim(full_path)+''')                           '+
              '    TOOBJ('''+%Trim(dest_name)+''')                           ';
         rc=exeCmd(cmd);

    endsr;

end-proc;

/copy sdb01.src/qrpgsrc,ligdimpl ;
