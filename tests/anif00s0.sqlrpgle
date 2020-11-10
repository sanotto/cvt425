*SRCMBRTXT:ALTAS CUENTAS IFE                      
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

//==============================================================================
//Declaración de prototipos, importadas de los miembros declarados.
//==============================================================================

dcl-f BAPFIS usage(*output) keyed;
dcl-f BADIPF usage(*output) keyed;


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

    //Llamada al Programa SBBAINFE.
    dcl-pr SBBAINFE extpgm('SBBAINFE');
        PAFECH                  packed(8:0);
        PACINV                  char(2);
    end-pr;

    //Llamada al Programa SBCCDIGC
    dcl-pr SBCCDIGC extpgm('SBCCDIGC');
        PAISUC                  packed(5:0);
        PAICAH                  packed(11:0);
        PAIGRC                  char(2);
        PAIMON                  packed(9:0);
    end-pr;

    //Llamada al Programa SBCCDIGC
    dcl-pr SBCCDIGN extpgm('SBCCDIGN');
        PAISUC                  packed(5:0);
        PAICAH                  packed(11:0);
    end-pr;

    dcl-s PAISUC                packed(5:0);
    dcl-s PAICAH                packed(11:0);
    dcl-s PAIGRC                char(2);
    dcl-s PAIMON                packed(9:0);

    dcl-s cuenta_alta           packed(11:0);

    dcl-s import_ok             ind;
    dcl-s sufijo                varchar(255);
    dcl-s dest_name             varchar(255);
    dcl-s full_path             varchar(255);
    dcl-s path_to_out           varchar(255);
    dcl-s path_to_dir           varchar(255);
    dcl-s cmd                   varchar(4096);
    dcl-s rc                    char(7);
    dcl-s aasfei                packed(8:0);
    dcl-s nombre_liserj         char(30);
    dcl-s nombre_archivo        char(30);
    dcl-s PAFECH                packed(8:0);
    dcl-s PACINV                char(2);

    dcl-s dni                   packed(15:0);
    dcl-s correo                char(50);

    dcl-s dni_padron            packed(15:0);
    dcl-s cuil_padron           packed(11:0);
    dcl-s fecha_marca_tmp       packed(8:0);
    dcl-s fecha_nac             packed(8:0);

    dcl-s ac_documento          packed(15:0);
    dcl-s ac_sucursal           packed(5:0);
    dcl-s ac_cuenta             packed(11:0);
    dcl-s tipo                  char(1);
    dcl-s cbu                   packed(22:0);
    dcl-s acb_documento         packed(15:0);
    dcl-s acb_sucursal          packed(5:0);
    dcl-s acb_cuenta            packed(11:0);

    dcl-ds beneficio qualified;
           marca_temporal       char(50);
           correo               char(50);
           apellido             char(50);
           nombre               char(50);
           dni                  packed(15:0);
           calle                char(50);
           numeracion           char(50);
           barrio               char(50);
           telefono             char(50);
           codigo_postal        char(50);
           ciudad               char(50);
           localidad            char(50);
           fecha_nacimiento     char(50);
           archivo              char(100);
           actividad            char(50);
           cuil                 char(50);
           sexo                 char(50);
    end-ds;

        dcl-ds beneficio_error qualified;
           marca_temporal       char(50);
           correo               char(50);
           apellido             char(50);
           nombre               char(50);
           dni                  packed(15:0);
           calle                char(50);
           numeracion           char(50);
           barrio               char(50);
           telefono             char(50);
           codigo_postal        char(50);
           ciudad               char(50);
           localidad            char(50);
           fecha_nacimiento     char(50);
           archivo              char(100);
           actividad            char(50);
           cuil                 char(50);
           sexo                 char(50);
    end-ds;

    //Variables para formatear Strings
    dcl-c up              'ABCDEFGHIJKLMNNOPQRSTUVWXYZ'      ;
    dcl-c lo              'abcdefghijklmn¦opqrstuvwxyz'      ;
    dcl-c emp             '                           '      ;
    dcl-c Symbols         '|°¬!"#$%&/()=?\¡¿*+~¢]{}_-;,:.<>º';
    dcl-c SymBlanks       '                                 ';
    dcl-c Acentos         'ñÑáéíóúäëïöüãõàèìòùâêîôû@'        ;
    dcl-c AceBlanks       'nNAEIOUAEIOUAOAEIOUAEIOU '        ;
    dcl-c AceEmpty        '                         '        ;
    dcl-c Apos            ''''                               ;
    dcl-c APosBlank       ' '                                ;


    dcl-ds  DatosPersonales ;
        estaEnBapfis        ind inz(*off);
        poseeDireccion      ind inz(*off);
        estaEnPadronIFE     ind inz(*off);
        estaEnPadronANSES   ind inz(*off);
        poseeCorreo         ind inz(*off);
        esFallecido         ind inz(*off);
        poseeCuenta         ind inz(*off);
        correoExistente     char(50) inz(*blanks);
    end-ds;

    path_to_dir = '/home/ANSES/';
    path_to_out = '/home/ANSES/HISTORICOS/';

    exec sql SET OPTION COMMIT = *NONE;

    exec sql SELECT AASFEI INTO :aasfei FROM SGSYSV;

    exsr crear_tablas_de_trabajo;

    exsr dir_list_open;
    exsr dir_list_fetch;
    exsr crea_sufijo_de_archivo_ya_procesado;

    dow sqlcod = *zero;
        exsr armar_path;

        exsr limpiar_tablas_de_trabajo;
        exsr procesar_Archivo_ife;
        exsr dir_list_fetch;
    enddo;
    exsr dir_list_close;

    //==========================================================================
    begsr procesar_archivo_ife;
    //==========================================================================
        exsr importar_archivo_ife;
        exsr declarar_cursor_AACIFE;
        //exsr cargar_padron_beneficios_error;
        exsr cargar_padron_beneficios_ife;
    endsr;

    //==========================================================================
    begsr crea_sufijo_de_archivo_ya_procesado;
    //==========================================================================
        exec sql SELECT
                    TRANSLATE(
                            CAST ( timestamp(now()) as char(19))
                                   , '__','.-')
                    INTO :sufijo
                 FROM SYSIBM/SYSDUMMY1;

    endsr;

    //==========================================================================
    begsr crear_tablas_de_trabajo;
    //==========================================================================

        exec sql DROP TABLE QTEMP.AACIFE;
        exec sql CREATE TABLE QTEMP.AACIFE(
                                    marca_temporal      char(50),
                                    correo              char(50),
                                    apellido            char(50),
                                    nombre              char(50),
                                    dni                 char(50),
                                    calle               char(50),
                                    numeracion          char(50),
                                    barrio              char(50),
                                    telefono            char(50),
                                    codigo_postal       char(50),
                                    ciudad              char(50),
                                    localidad           char(50),
                                    fecha_nacimiento    char(50),
                                    archivo             char(100),
                                    actividad           char(50),
                                    cuil                char(50),
                                    sexo                char(50));

    endsr;

    //==========================================================================
    begsr dir_list_open;
    //==========================================================================
        exec sql DECLARE C1 CURSOR FOR
                SELECT
                    CAST (FILENAME as char(255)) AS FILNAM
                FROM TABLE(IFSDIR(:path_to_dir)) AS T
                WHERE
                    UPPER(FILENAME) LIKE '%IFE%';

        exec sql OPEN C1;
    endsr;

    //==========================================================================
    begsr dir_list_fetch;
    //==========================================================================
        exec sql FETCH C1 into :c1_ds;
    endsr;

    //==========================================================================
    begsr armar_path;
    //==========================================================================
        dest_name = %Trim(path_to_out)+             //+'/'+
                    %Trim(c1_ds.filename)+          //+'_'+
                    '_' +
                    ''+%Trim(sufijo)+ ''+
                    '.csv';
        full_path = %trim(path_to_dir)+%trim(c1_ds.filename);
    endsr;

    //==========================================================================
    begsr limpiar_tablas_de_trabajo;
    //==========================================================================
        exec sql DELETE FROM QTEMP.AACIFE;
    endsr;

    //==========================================================================
    begsr importar_archivo_ife;
    //==========================================================================
        import_ok = *off;

        //Cambio ccsid a 1208 del archivo .csv
        cmd =       'CHGATR OBJ('''+%Trim(full_path)+''') ' +
                    'ATR(*CCSID) VALUE(1208)';
        rc = execmd(cmd);

        rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '           TOFILE(QTEMP/AACIFE)                            ' +
                '           MBROPT(*REPLACE)                                ' +
                '           RCDDLM(*CRLF)                                   ' +
                '           FLDDLM('','')                                   ' );

        //Eliminar cabeceras.
        if rc = 'CPF0000' ;

            exec sql
                DELETE FROM QTEMP.AACIFE WHERE RRN(AACIFE) = 1;

            import_ok = *on;

        endif;
    endsr;

    //==========================================================================
    begsr declarar_cursor_AACIFE;
    //==========================================================================
        exec sql declare cursor_padron cursor for
                     SELECT
                        marca_temporal,
                        correo,
                        apellido,
                        nombre,
                        CAST(dni AS DEC(15)) as dni,
                        calle,
                        CASE
                            WHEN numeracion IS NULL THEN '0'
                        ELSE numeracion END,
                        barrio,
                        telefono,
                        codigo_postal,
                        ciudad,
                        localidad,
                        CASE
                            WHEN fecha_nacimiento IS NULL THEN '0'
                        ELSE fecha_nacimiento END,
                        CASE
                            WHEN archivo IS NULL THEN '-'
                        ELSE archivo END,
                        CASE
                            WHEN actividad IS NULL THEN '-'
                        ELSE actividad END,
                        CASE
                            WHEN cuil IS NULL THEN '0'
                        ELSE cuil END,
                        CASE
                            WHEN sexo IS NULL THEN '-'
                        ELSE sexo END
                     FROM QTEMP.AACIFE
                     WHERE actividad IS NULL;
    endsr;


    //==========================================================================
    begsr limpiar_carateres;
    //==========================================================================

        beneficio.apellido = %XLATE(Symbols:SymBlanks:beneficio.apellido);
        beneficio.apellido = %XLATE(Acentos:AceBlanks:beneficio.apellido);
        beneficio.apellido = %XLATE(Apos:AposBlank:beneficio.apellido);
        beneficio.apellido = %XLATE(lo:up:beneficio.apellido);

        beneficio.nombre = %XLATE(Symbols:SymBlanks:beneficio.nombre);
        beneficio.nombre = %XLATE(Acentos:AceBlanks:beneficio.nombre);
        beneficio.nombre = %XLATE(Apos:AposBlank:beneficio.nombre);
        beneficio.nombre = %XLATE(lo:up:beneficio.nombre);

        beneficio.calle = %XLATE(Symbols:SymBlanks:beneficio.calle);
        beneficio.calle = %XLATE(Acentos:AceBlanks:beneficio.calle);
        beneficio.calle = %XLATE(Apos:AposBlank:beneficio.calle);
        beneficio.calle = %XLATE(lo:up:beneficio.calle);

        beneficio.barrio = %XLATE(Symbols:SymBlanks:beneficio.barrio);
        beneficio.barrio = %XLATE(Acentos:AceBlanks:beneficio.barrio);
        beneficio.barrio = %XLATE(Apos:AposBlank:beneficio.barrio);
        beneficio.barrio = %XLATE(lo:up:beneficio.barrio);

        beneficio.ciudad = %XLATE(Symbols:SymBlanks:beneficio.ciudad);
        beneficio.ciudad = %XLATE(Acentos:AceBlanks:beneficio.ciudad);
        beneficio.ciudad = %XLATE(Apos:AposBlank:beneficio.ciudad);
        beneficio.ciudad = %XLATE(lo:up:beneficio.ciudad);

        beneficio.localidad = %XLATE(Symbols:SymBlanks:beneficio.localidad);
        beneficio.localidad = %XLATE(Acentos:AceBlanks:beneficio.localidad);
        beneficio.localidad = %XLATE(Apos:AposBlank:beneficio.localidad);
        beneficio.localidad = %XLATE(lo:up:beneficio.localidad);

        beneficio.sexo = %XLATE(Symbols:SymBlanks:beneficio.sexo);
        beneficio.sexo = %XLATE(Acentos:AceBlanks:beneficio.sexo);
        beneficio.sexo = %XLATE(Apos:AposBlank:beneficio.sexo);
        beneficio.sexo = %XLATE(lo:up:beneficio.sexo);

        beneficio.numeracion = %XLATE(Symbols:SymBlanks:beneficio.numeracion);
        beneficio.numeracion = %XLATE(Acentos:AceBlanks:beneficio.numeracion);
        beneficio.numeracion = %XLATE(Apos:AposBlank:beneficio.numeracion);
        beneficio.numeracion = %XLATE(lo:up:beneficio.numeracion);
        beneficio.numeracion = %XLATE(up:emp:beneficio.numeracion);

        beneficio.codigo_postal =
                              %XLATE(Symbols:SymBlanks:beneficio.codigo_postal);
        beneficio.codigo_postal =
                              %XLATE(Acentos:AceBlanks:beneficio.codigo_postal);
        beneficio.codigo_postal =
                              %XLATE(Apos:AposBlank:beneficio.codigo_postal);
        beneficio.codigo_postal = %XLATE(lo:up:beneficio.codigo_postal);
        beneficio.codigo_postal = %XLATE(up:emp:beneficio.codigo_postal);

        //Invertir Fecha Temporal.
        fecha_marca_tmp =
        %Dec(%Scanrpl('/':'':%Subst(beneficio.marca_temporal:1:10)):8:0);
        PAFECH = fecha_marca_tmp;
        PACINV = 'NI';

        SBBAINFE(PAFECH
                :PACINV);

        fecha_marca_tmp = PAFECH;

        //Ivertir Fecha Nacimiento.
        fecha_nac =
        %Dec(%Scanrpl('/':'':%Subst(beneficio.fecha_nacimiento:1:10)):8:0);
        PAFECH = fecha_nac;
        PACINV = 'NI';

        SBBAINFE(PAFECH
                :PACINV);

        fecha_nac = PAFECH;

    endsr;

    //==========================================================================
    begsr cargar_padron_beneficios_ife;
    //==========================================================================
        exec sql open cursor_padron;
        exec sql fetch cursor_padron into :beneficio;
        dow sqlcod = *Zero;
            exsr limpiar_carateres;
            exsr verificar_si_es_cliente;

            if ( estaEnBapfis or esFallecido or estaEnPadronIFE);
                //exsr agregar_a_informe_de_rechazados;
                iter;
            endif;

            exsr generar_alta_persona_fisica;
            exsr generar_alta_cuenta;
            exsr generar_cbu;
            exsr darlo_de_alta_en_padron;


            exec sql fetch cursor_padron into :beneficio;
        enddo;
        exec sql close cursor_padron;
    endsr;

    //==========================================================================
    begsr verificar_si_es_cliente;
    //==========================================================================
        Clear DatosPersonales;

        //Verificar si el beneficiario esta cargado en base de BAPFIS.
        exec sql    SELECT
                        A#INDO
                    INTO :dni
                    FROM
                        BAPFIS
                    WHERE
                        A#INDO = :beneficio.dni;

        if SQLCODE = *ZERO;
            estaEnBapfis = *on;
            poseeCuenta = *on;
        endif;

        //Verif. si el beneficiario fue cargado enteriormente en el Padrón IFE.
        exec sql    SELECT
                        IFINDO
                    INTO :dni
                    FROM ANLIIF
                    WHERE
                        IFINDO = :beneficio.dni;

        if SQLCODE =     *ZERO;
            estaEnPadronIFE = *on;
        endif;

        //Verificar si el beneficiario es Fallecido.
        exec sql    SELECT
                        QTINDO
                    INTO :dni
                    FROM BAPSIN
                    WHERE
                        QTINDO = :beneficio.dni;

        if SQLCODE = *ZERO;
            esFallecido = *on;
        endif;

        //Verificar si existen en Padrón Anses.
        exec sql    SELECT
                        APINDO,
                        APICUI

                    INTO :dni_padron,
                         :cuil_padron
                    FROM AFPADR
                    WHERE
                            APINDO = :beneficio.dni
                    AND     SUBSTRING(APNDNN, 1, 1) =
                            SUBSTRING(:beneficio.apellido, 1, 1)
                    AND     APFBAJ = 0;

        if SQLCODE = *ZERO;
            estaEnPadronANSES  = *on;
        endif;

        //Verificar si existen en BADIPF.
        exec sql    SELECT
                        AEINDO
                    INTO :dni
                    FROM BADIPF
                    WHERE
                        AEINDO = :beneficio.dni;

        if SQLCODE = *ZERO;
            poseeDireccion  = *on;
        endif;

        //Verificar si existe en BAPFDA.
        exec sql    SELECT
                        QQCOEL
                    INTO :correoExistente
                    FROM BAPFDA
                    WHERE
                        QQINDO = :beneficio.dni;
    endsr;



    //==========================================================================
    begsr generar_cbu;
    //==========================================================================
    endsr;

    //==========================================================================
    begsr darlo_de_alta_en_padron;
    //==========================================================================
    endsr;



    //==========================================================================
    begsr generar_alta_persona_fisica;
    //==========================================================================

        //Alta en Bapfis.
        A#ITDO=96;
        A#INDO=dni_padron;
        A#NYAP=%TRIM(beneficio.apellido) +' ' +%TRIM(beneficio.nombre);
        A#FNAC=19890228;
        A#ISEX='-';
        if %SUBST(beneficio.sexo: 1: 1) = 'H' ;
            A#ISEX='M';
        else;
            A#ISEX='F';
        endif;
        A#IECI=1;
        A#IPAI=80;
        A#INAC='ARGENTINA';
        A#FALT=aasfei;
        A#FECH=aasfei;
        A#IACT=203;
        A#ICIV=3;
        A#ICUI=cuil_padron;
        A#ISCT=1;
        A#IGRC='04';
        A#ITEL=%TRIM(beneficio.telefono);
        A#ISIT=1;
        A#ISIC=1;
        A#ICAR=2;
        A#LNAC=%SUBST(beneficio.localidad: 1: 30);

        write REBAPFIS;

        //Alta BADIPF si no esta cargado.
        if (not poseeDireccion);

            AEITDO=96;
            AEINDO=dni_padron;
            AEIPAI=80;
            AEICPO=%dec(%trim(beneficio.codigo_postal):5:0);
            AENCAL=%subst(%trim(beneficio.calle)
                    + ' '
                    + %trim(beneficio.barrio): 1: 30);

            if %subst(beneficio.numeracion: 1: 6) = ' ' ;
                AEIPUE=0;
            endif;

            AEIPUE=%dec(%subst(beneficio.numeracion: 1: 6):6:0);
            AEIBIS='0';
            AEIPLA=0;
            AEIPIS=0;
            AEIDPT='1';
            AEFMOD=aasfei;

            write REBADIPF;
        endif;

    //Alta BAPFDA si no existe.
    if (not poseeCorreo);
        exec sql INSERT INTO BAPFDA(
                            QQITDO,
                            QQINDO,
                            QQNAPE,
                            QQNOMB,
                            QQFALT,
                            QQCOEL)

        values(
        96,
        :dni_padron,
        SUBSTRING(TRIM(:beneficio.apellido), 1, 30),
        SUBSTRING(TRIM(:beneficio.nombre), 1, 30),
        :aasfei,
        TRIM(:beneficio.correo)
        );
    endif;

    endsr;

//==============================================================================

    begsr generar_alta_cuenta;

    //Obtengo Numero de CUenta.
    PAISUC = 0;
    PAICAH = 0;
    PAIGRC = '04';
    PAIMON = 0;

    SBCCDIGC(PAISUC
            :PAICAH
            :PAIGRC
            :PAIMON);

    SBCCDIGN(PAISUC
            :PAICAH);

    cuenta_alta = PAICAH;

    //Alta en BADCCL.
    exec sql INSERT INTO BADCCL(
                            OTISUC,
                            OTICCL,
                            OTITTL,
                            OTITDO,
                            OTINDO,
                            OTISOC)

    Values(0,
           :cuenta_alta,
           1,
           96,
           :dni_padron,
           0);

    //Alta en BAICCL
    exec sql INSERT INTO BAICCL(
                            OSISUC,
                            OSICCL,
                            OSNCCL,
                            OSITIN,
                            OSISIT,
                            OSISIC,
                            OSININ,
                            OSFALT,
                            OSICUE,
                            OSIELI,
                            OSIAMA,
                            OSECCL)

    Values(0,
           :cuenta_alta,
           TRIM(:beneficio.apellido) || ' ' || TRIM(:beneficio.nombre),
           0,
           1,
           1,
           0,
           :aasfei,
           0,
           5,
           1,
           '8');

    //Alta ACCTAC.
    exec sql INSERT INTO ACCTAC(
                            FUISUC,
                            FUICAH,
                            FUIMON,
                            FUFALT,
                            FUIOFI,
                            FUIBAC,
                            FUICRE,
                            FUISCT,
                            FUIGRC,
                            FUISGC,
                            FUICCL,
                            FUINCT,
                            FUIUCA)

    Values(0,
           :cuenta_alta,
           1,
           :aasfei,
           '1',
           0,
           6,
           1,
           '04',
           ' ',
           :cuenta_alta,
           :cuenta_alta,
           'S');

    endsr;



//==============================================================================

    //begsr cargar_padron_beneficios_error;
    //
    //    exec sql open cursor_padron_error;
    //    exec sql fetch cursor_padron_error into :beneficio_error;
    //    dow sqlcod = *Zero;
    //        exsr limpiar_carateres_inconsistentes;
    //        exsr informar_beneficios_con_inconsistencia;
    //        exec sql fetch cursor_padron_error into :beneficio_error;
    //    enddo;
    //    exec sql close cursor_padron_error;
    //
    //endsr;

//==============================================================================

    //begsr limpiar_carateres_inconsistentes;
    //
    //beneficio_error.apellido =
    //%XLATE(Symbols:SymBlanks:beneficio_error.apellido);
    //beneficio_error.apellido =
    //%XLATE(Acentos:AceBlanks:beneficio_error.apellido);
    //beneficio_error.apellido = %XLATE(Apos:AposBlank:beneficio_error.apellido)
    //beneficio_error.apellido = %XLATE(lo:up:beneficio_error.apellido);
    //
    //beneficio_error.nombre = %XLATE(Symbols:SymBlanks:beneficio_error.nombre);
    //beneficio_error.nombre = %XLATE(Acentos:AceBlanks:beneficio_error.nombre);
    //beneficio_error.nombre = %XLATE(Apos:AposBlank:beneficio_error.nombre);
    //beneficio_error.nombre = %XLATE(lo:up:beneficio_error.nombre);
    //
    //beneficio_error.calle = %XLATE(Symbols:SymBlanks:beneficio_error.calle);
    //beneficio_error.calle = %XLATE(Acentos:AceBlanks:beneficio_error.calle);
    //beneficio_error.calle = %XLATE(Apos:AposBlank:beneficio_error.calle);
    //beneficio_error.calle = %XLATE(lo:up:beneficio_error.calle);
    //
    //beneficio_error.barrio = %XLATE(Symbols:SymBlanks:beneficio_error.barrio);
    //beneficio_error.barrio = %XLATE(Acentos:AceBlanks:beneficio_error.barrio);
    //beneficio_error.barrio = %XLATE(Apos:AposBlank:beneficio_error.barrio);
    //beneficio_error.barrio = %XLATE(lo:up:beneficio_error.barrio);
    //
    //beneficio_error.ciudad = %XLATE(Symbols:SymBlanks:beneficio_error.ciudad);
    //beneficio_error.ciudad = %XLATE(Acentos:AceBlanks:beneficio_error.ciudad);
    //beneficio_error.ciudad = %XLATE(Apos:AposBlank:beneficio_error.ciudad);
    //beneficio_error.ciudad = %XLATE(lo:up:beneficio_error.ciudad);
    //
    //beneficio_error.localidad =
    //%XLATE(Symbols:SymBlanks:beneficio_error.localidad);
    //beneficio_error.localidad =
    //%XLATE(Acentos:AceBlanks:beneficio_error.localidad);
    //beneficio_error.localidad =
    //%XLATE(Apos:AposBlank:beneficio_error.localidad);
    //beneficio_error.localidad = %XLATE(lo:up:beneficio_error.localidad);
    //
    //beneficio_error.sexo = %XLATE(Symbols:SymBlanks:beneficio_error.sexo);
    //beneficio_error.sexo = %XLATE(Acentos:AceBlanks:beneficio_error.sexo);
    //beneficio_error.sexo = %XLATE(Apos:AposBlank:beneficio_error.sexo);
    //beneficio_error.sexo = %XLATE(lo:up:beneficio_error.sexo);
    //
    //beneficio_error.numeracion =
    //%XLATE(Symbols:SymBlanks:beneficio_error.numeracion);
    //beneficio_error.numeracion =
    //%XLATE(Acentos:AceBlanks:beneficio_error.numeracion);
    //beneficio_error.numeracion =
    //%XLATE(Apos:AposBlank:beneficio_error.numeracion);
    //beneficio_error.numeracion = %XLATE(lo:up:beneficio_error.numeracion);
    //beneficio_error.numeracion = %XLATE(up:emp:beneficio_error.numeracion);
    //
    //beneficio_error.codigo_postal =
    //%XLATE(Symbols:SymBlanks:beneficio_error.codigo_postal);
    //beneficio_error.codigo_postal =
    //%XLATE(Acentos:AceBlanks:beneficio_error.codigo_postal);
    //beneficio_error.codigo_postal =
    //%XLATE(Apos:AposBlank:beneficio_error.codigo_postal);
    //beneficio_error.codigo_postal = %XLATE(lo:up:beneficio_error.codigo_postal
    //beneficio_error.codigo_postal =
    //%XLATE(up:emp:beneficio_error.codigo_postal);
    //
    ////Invertir Fecha Temporal.
    //fecha_marca_tmp =
    //%Dec(%Scanrpl('/':'':%Subst(beneficio_error.marca_temporal:1:10)):8:0);
    //PAFECH = fecha_marca_tmp;
    //PACINV = 'NI';
    //
    //SBBAINFE(PAFECH
    //        :PACINV);
    //
    //fecha_marca_tmp = PAFECH;
    //
    ////Ivertir Fecha Nacimiento.
    //fecha_nac =
    //%Dec(%Scanrpl('/':'':%Subst(beneficio_error.fecha_nacimiento:1:10)):8:0);
    //PAFECH = fecha_nac;
    //PACINV = 'NI';
    //
    //SBBAINFE(PAFECH
    //        :PACINV);
    //
    //fecha_nac = PAFECH;
    //
    //endsr;

//==============================================================================

//    begsr informar_beneficios_con_inconsistencia;
//
//          exec sql INSERT INTO ANLIIF(
//                                IFFPRE,
//                                IFFALT,
//                                IFHALT,
//                                IFCOEL,
//                                IFNCB1,
//                                IFINDO,
//                                IFNCAL,
//                                IFIPUE,
//                                IFITEL,
//                                IFICPO,
//                                IFDF01,
//                                IFNLOC,
//                                IFFNAC,
//                                IFCUIA,
//                                IFISEX,
//                                IFRENG,
//                                IFCLTR,
//                                IFNCCL,
//                                IFFBAJ)
//
//          VALUES(
//          :aasfei,
//          :fecha_marca_tmp,
//          REPLACE(SUBSTRING(:beneficio_error.marca_temporal, 12, 8), ':', ''),
//          :beneficio_error.correo,
//          TRIM(:beneficio_error.apellido) || ' ' ||
//          TRIM(:beneficio_error.nombre),
//          :beneficio_error.dni,
//          SUBSTRING(TRIM(:beneficio_error.calle) || ' ' ||
//          TRIM(:beneficio_error.barrio), 1, 30),
//          CASE
//            WHEN SUBSTRING(:beneficio_error.numeracion, 1, 6) = ' ' THEN 0
//            WHEN SUBSTRING(:beneficio_error.numeracion, 1, 6) = '' THEN 0
//          ELSE CAST(REPLACE(
//          SUBSTRING(:beneficio_error.numeracion, 1, 6), ' ', '') AS DEC(6)) EN
//          TRIM(:beneficio_error.telefono),
//          CAST(TRIM(:beneficio_error.codigo_postal) AS DEC(5)),
//          SUBSTRING(:beneficio_error.ciudad, 1, 20),
//          SUBSTRING(:beneficio_error.localidad, 1, 30),
//          :fecha_nac,
//          TRIM(:beneficio_error.cuil),
//          CASE
//           WHEN SUBSTRING(:beneficio_error.sexo, 1, 1) = 'H' THEN 'M'
//           WHEN SUBSTRING(:beneficio_error.sexo, 1, 1) = 'M' THEN 'F'
//          ELSE '-' END,
//          SUBSTRING(:beneficio_error.archivo, 1, 38),
//          'WEB',
//          'Datos Inconsitentes para Alta',
//          :aasfei
//          );
//
//    endsr;

//==============================================================================

    begsr dir_list_close;

        exec sql CLOSE C1;

    endsr;

//==============================================================================

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
