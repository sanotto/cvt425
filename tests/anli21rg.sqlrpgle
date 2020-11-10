*SRCMBRTXT:AN: INFORMACIÓN OPERATORIA DIARIA BANCO
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

    //Llamada al Programa SBBAINFE.
    dcl-pr SBBAINFE extpgm('SBBAINFE');
        PAFECH                  packed(8:0);
        PACINV                  char(2);
    end-pr;

    //... Llamada al programa 'SBBABFE2'
    dcl-pr SBBABFE2 extpgm('SBBABFE2');
        PAFECH             packed(8:0);
        PADIAS             packed(15:0);
        PAISUC             packed(5:0);
    end-pr;

    //... Llamada al programa 'SBBAVFE1'
    dcl-pr SBBAVFE1 extpgm('SBBAVFE1');
        PAFECH             packed(8:0);
        PAISUC             packed(5:0);
        PAMODO             char(2);
    end-pr;

    //... Llamada al programa 'SBBABFE1'
    dcl-pr SBBABFE1 extpgm('SBBABFE1');
        PAFECH             packed(8:0);
        PADIAS             packed(15:0);
        PAMODO             char(2);
    end-pr;

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
    dcl-s fecha_contabilidad    packed(8:0);
    dcl-s fecha_movimientos     packed(8:0);
    dcl-s dia_habil_anterior    packed(8:0);
    dcl-s PAFECH                packed(8:0);
    dcl-s PADIAS                packed(15:0);
    dcl-s PAISUC                packed(5:0);
    dcl-s PACINV                char(2);
    dcl-s PAMODO                char(2);
    dcl-s directorio            char(255);
    dcl-s dest_name             char(255);
    dcl-s consulta              char(255);
    dcl-s fecha                 char(8);
    dcl-s dia_habil             packed(8:0);

    dcl-ds codigo_agencia qualified;
           cod_agencia          packed(3:0);
    end-ds;

    dcl-s prev_cant             packed(10:0);
    dcl-s prev_imp              packed(15:2);
    dcl-s plan_can              packed(10:0);
    dcl-s plan_imp              packed(15:2);

    path_to_dir = '/home/ANSES/';
    path_to_out = '/home/ANSES/HISTORICOS/';

    exec sql SET OPTION COMMIT = *NONE;

    exsr crear_tablas_de_trabajo;
    exsr optener_dia_habil_anterior;
    if dia_habil = aasfei;
        exsr verificar_archivo_procesado;
        exsr declarar_cursor_codigo_agencia;
        exec sql open cursor_agencia;
        exec sql fetch cursor_agencia into :codigo_agencia;
        Dow sqlcod = *Zero;
            exsr recuperar_totales;
            exsr insertar_tado_en_tabla_temporal;
            exec sql fetch cursor_agencia into :codigo_agencia;
        enddo;
        exec sql close cursor_agencia;
        exsr generar_informe;
        wrtToJobLog('Comienza Generación de archivo:'+directorio);
        exsr guardar_liserj;
        exsr enviar_mail;
    else;
        wrtToJobLog('El informe no se ha generado, no es un dia habil.');
    endif;

//==============================================================================

    begsr crear_tablas_de_trabajo;

        //Creo tabla temporal para llevar la operatoria diaria.

        exec sql DROP TABLE QTEMP.ANOPBC;

        exec sql CREATE TABLE QTEMP.ANOPBC(
                            fecha           char(5),
                            sucursal        char(8),
                            prev_can        char(20),
                            prev_imp        char(20),
                            plan_can        char(20),
                            plan_imp        char(20));

        exec sql DELETE FROM QTEMP.ANOPBC;

    endsr;

//==============================================================================

    begsr optener_dia_habil_anterior;

    exec sql SELECT
                AASFEI
             INTO :aasfei
             FROM SGSYSV;

    exec sql SELECT
                F#FPCO
             INTO :fecha_contabilidad
             FROM COTARJ;

    PAFECH = fecha_contabilidad;
    PACINV = 'IN';

    //Invertir Fecha.
    SBBAINFE(PAFECH
            :PACINV);

    //Obtener Dia habil Anterior.
    //SBBABFE2(PAFECH
    //        :PADIAS
    //        :PAISUC);

    fecha_contabilidad = PAFECH;

    //Obenter Fecha Para ACMOVH.
    PACINV = 'NI';

    SBBAINFE(PAFECH
            :PACINV);

    fecha_movimientos = PAFECH;

    //Obenter nombre del archivo.

    fecha = %Char(fecha_contabilidad);

    fecha = %Editc( %Dec ( %Trim ( fecha ) :8:0) : 'X' );

    nombre_archivo = '309_INFO_Ventanilla_' +
                     %Subst(fecha:1:2) +
                     '-' +
                     %Subst(fecha:3:2) +
                     '.xls';

    //Obtener dia habil, el archivo se genera de Lunes a Viernes.

    PAFECH = aasfei;
    PAISUC = 0;
    PAMODO = 'I';

    SBBAVFE1(PAFECH
            :PAISUC
            :PAMODO);

    if PAMODO <> '';

       PADIAS = 0;
       PAMODO = 'IN';

       SBBABFE1(PAFECH
               :PADIAS
               :PAMODO);

    endif;

    dia_habil = PAFECH;

    endsr;

//==============================================================================

    begsr verificar_archivo_procesado;

        exec sql
            SELECT RJDACO
                INTO :nombre_liserj
            FROM LISERJ
            WHERE RJDACO = :nombre_archivo;

        if nombre_archivo = nombre_liserj;

        //...Imprimo mensaje de error en pantalla de sideba

        die('El Archivo '+%Trim(nombre_archivo)+', ya fué ' +
            'procesado anteriormente por favor verifique y' +
            ' reintente nuevamente.                       ');

        endif;

    endsr;

//==============================================================================

    begsr declarar_cursor_codigo_agencia;

        exec sql DECLARE cursor_agencia CURSOR FOR

                 SELECT DISTINCT
                            ANIABP
                 FROM ANLIHA
                 WHERE
                 ANFDPA = :fecha_contabilidad AND
                 ANIGRP > 0                   AND
                 ANICAH > 0;

    endsr;

//==============================================================================

    begsr recuperar_totales;

    //Inizializo Totales en Zero.
    prev_cant = 0;
    prev_imp  = 0;
    plan_can  = 0;
    plan_imp  = 0;

    //Consulta 1° Parte Informe.

        exec sql SELECT DISTINCT
                            COUNT(*),
                            SUM(GD$IMP)
                 INTO :prev_cant, :prev_imp
                 FROM ANLIHA
                 INNER JOIN ACMOVH ON GDICAH = ANICAH AND
                                      GDISUC = ANISUC AND
                                      GDIMCA = 1      AND
                                      GDFASI = :fecha_movimientos
                 WHERE
                 ANFDPA = :fecha_contabilidad AND
                 ANIGRP > 0        AND
                 ANICAH > 0        AND
                 ANIABP = :codigo_agencia.cod_agencia AND
                 ANIECJ NOT IN (77, 76, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                                97, 98);

    //Consulta 2° Parte Informe.

        exec sql SELECT DISTINCT
                            COUNT(*),
                            SUM(GD$IMP)
                 INTO :plan_can, :plan_imp
                 FROM ANLIHA
                 INNER JOIN ACMOVH ON GDICAH = ANICAH AND
                                      GDISUC = ANISUC AND
                                      GDIMCA = 1      AND
                                      GDFASI = :fecha_movimientos
                 WHERE
                 ANFDPA = :fecha_contabilidad AND
                 ANIGRP > 0        AND
                 ANICAH > 0        AND
                 ANIABP = :codigo_agencia.cod_agencia AND
                 ANIECJ IN (77, 76, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 97,
                            98);

    endsr;

//==============================================================================

    begsr insertar_tado_en_tabla_temporal;

        exec sql INSERT INTO QTEMP.ANOPBC(
                            fecha,
                            sucursal,
                            prev_can,
                            prev_imp,
                            plan_can,
                            plan_imp)

                 VALUES (SUBSTRING(:fecha, 1, 2) || '/' ||
                         SUBSTRING(:fecha, 3, 2),
                         '309' || '-' || :codigo_agencia.cod_agencia,
                         :prev_cant,
                         :prev_imp,
                         :plan_can,
                         :plan_imp);

    endsr;

//==============================================================================

    begsr generar_informe;

        directorio = %Trim(path_to_dir) + %Trim(nombre_archivo);

        consulta = 'SELECT * FROM QTEMP.ANOPBC';

        cmd =   'EXPPCDBF SQLSTM('''+%Trim(consulta)+''')  '+
                'OUTPAT('''+%Trim(directorio)+''')         ';

        rc=exeCmd(cmd);

        //Realizar copia en historicos.
        dest_name = %Trim(path_to_out) + %Subst(%Trim(nombre_archivo):1:25) +
                    '_' + %Editw(aasfei:'        ') + '.xls';

        cmd =   'EXPPCDBF SQLSTM('''+%Trim(consulta)+''')  '+
                'OUTPAT('''+%Trim(dest_name)+''')          ';

        rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr guardar_liserj;

        //Guardo nombre del archivo en Liserj.

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

    begsr enviar_mail;

        cmd =   'SNDMAIL                                         '+
              '   RECP(''ANLI19CL'')                             '+
              '   SUBJECT(''Información operatoria Diaria del dia'+
              ' '+%Trim(fecha)+''')     '+
              '   MESG(''Se proceso archivo del dia'+
              ' '+%Trim(fecha)+'. Por fav'+
              'or enviar al Personal de ANSES. El Archivo se encue'+
              'ntra en: '+%Trim(directorio)+'.'')                 ';
        rc=exeCmd(cmd);

    endsr;

//==============================================================================

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
