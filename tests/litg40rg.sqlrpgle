*SRCMBRTXT:Link-ODE-Generador TXT                 
**free
ctl-opt dftname(LITG50RG)
        option(*SRCSTMT)
        dftactgrp(*NO)
        main(main);

dcl-c MAX_FILE_SIZE 65535;

//Declaración de prototipos, importadas de los miembros declarados
/include desa00525/qrpgsrc,ligdprot ;
/include desa00525/qrpgsrc,ligdifs ;

//Procedimiento main, declarado como punto de entrada
//en ctl-opt
dcl-proc main;

    //*entry plist
    dcl-pi *n;
        fecha_a_informar        packed(8:0);
        error                   char(50);
    end-pi;

    //Declaración de Estructuras de archivo LIMODE
    dcl-ds limode EXTNAME('LIMODE')  end-ds;

    dcl-c up              'ABCDEFGHIJKLMNOPQRSTUVWXYZ'        ;
    dcl-c lo              'abcdefghijklmnopqrstuvwxyz'        ;
    dcl-c Symbols         '|°¬!"#$%&/()=?\¡¿*+~¢]{}_-;,:.<>'  ;
    dcl-c SymBlanks       '                                '  ;
    dcl-c Acentos         'ñÑáéíóúäëïöüãõàèìòùâêîôû@'         ;
    dcl-c AceBlanks       'nNAEIOUAEIOUAOAEIOUAEIOU '         ;
    dcl-c Apos            ''''                                ;
    dcl-c APosBlank       ' '                                 ;

    dcl-s fqn                   varchar(255) inz; //Fully quallified name
    dcl-s fd                    int(10);
    dcl-s nuevo_lote            packed(4:0);
    dcl-s lote4                 packed(4:0);
    dcl-s nombre_archivo        varchar(50);
    dcl-s linea                 char(700);
    dcl-s crlf                  char(2) inz(x'0d25');
    dcl-s filler_678            char(678) inz(*blanks);
    dcl-s filler_682            char(682) inz(*blanks);
    dcl-s filler_474            char(474) inz(*blanks);
    dcl-s cantidad              packed(10:0);
    dcl-s nombre_empresa        char(50) INZ('BANCO RIOJA SAU');

    dcl-s ode_imp_10            packed(10:0);
    dcl-s ode_ide_20            packed(20:0);
    dcl-s ode_tdo_2             packed(2:0);
    dcl-s ode_ndo_8             char(8);
    dcl-s ode_tct_2             char(2);
    dcl-s ode_pbf_19            char(19);

    dcl-pr mostrar_mensaje extpgm('BAER00RS');
        renglon_1               char(55) CONST ;
        renglon_2               char(55) CONST ;
        renglon_3               char(55) CONST ;
        renglon_4               char(55) CONST ;
        renglon_5               char(55) CONST ;
        renglon_6               char(55) CONST ;
        renglon_7               char(55) CONST ;
        renglon_8               char(55) CONST ;
    end-pr;

    exec sql set option commit=*none;
    exsr determinar_nuevo_nro_de_lote;
    exsr determinar_nombre_de_archivo;
    exsr abrir_archivo_ifs;
    exsr abrir_cursor;
    exsr escribir_cabecera;
    exsr escribir_detalle;
    exsr escribir_pie;
    exsr marcar_lote_como_generado;

    exsr endpgm;

    //--------------------------------------------------------------------------
    begsr escribir_cabecera;
    //--------------------------------------------------------------------------
        linea='CABECERA0309'
                + %editc(fecha_a_informar:'X')
                + '00'
                + filler_678
                ;
        ifs_wrtln_fix(fd:linea);
    endsr;

    //--------------------------------------------------------------------------
    begsr escribir_detalle;
    //--------------------------------------------------------------------------
        cantidad=0;
        exec sql FETCH C1 into :limode;
        dow SQLCODE = *zero;
            cantidad=cantidad +1;
            exsr escribir_linea;
            exec sql FETCH C1 into :limode;
        enddo;
    endsr;
    //--------------------------------------------------------------------------
    begsr escribir_linea;
    //--------------------------------------------------------------------------
        ode_imp_10 = TGOIMP*100; //Dos posiciones decimales
        ode_ide_20 = TGOIDE;
        ode_tdo_2  = TGOTDO;
        ode_ndo_8  = %subst(%editc(TGONDO:'X'):8:8);
        ode_tct_2  = %SUBST(TGTPBF:1:2);
        ode_pbf_19 = %subst(TGTPBF:3);

        TGOTXT = %XLATE(Symbols:SymBlanks:TGOTXT);
        TGOTXT = %XLATE(Acentos:AceBlanks:TGOTXT);
        TGOTXT = %XLATE(Apos:AposBlank:TGOTXT);
        TGOTXT = %XLATE(lo:up:TGOTXT);

        if TGOTXT = *blanks;
            TGOTXT = 'ODE GEN POR BR SAU';
        endif;

        linea='ORDEXTRA'
               + %subst(%editc(TGNCUI:'X'):2:11)
               + nombre_empresa
               + %editc(ode_imp_10:'X')
               + TGOTXT
               + %editc(ode_tdo_2:'X')
               + ode_ndo_8
               + ode_tct_2
               + ode_pbf_19
               + '0'+TGCCON
               + TGCOEL
               + TGCTEL
               + '0'+%editc(TGCCIT:'X')
               + %editc(TGOIDE:'X')
               + %editc(TGNTAR:'X')+'   '
               + filler_474
               ;
        ifs_wrtln_fix(fd:linea);
    endsr;

    //--------------------------------------------------------------------------
    begsr escribir_pie;
    //--------------------------------------------------------------------------
        linea='TRAILER '
               + %editc(cantidad:'X')
               + filler_682
               ;

        ifs_wrtln_fix(fd:linea);
    endsr;


    //--------------------------------------------------------------------------
    begsr marcar_lote_como_generado;
    //--------------------------------------------------------------------------
        exec sql
            UPDATE LIMODE SET
                TGELOT= :nuevo_lote,
                TGEFIL= :nombre_archivo,
                TGEFEC= (SELECT AASFEI FROM SGSYSV LIMIT 1),
                TGEHOR= (SELECT
                            REPLACE( CAST( TIME(NOW()) as CHAR(8)), ':', '')
                         FROM SYSIBM/SYSDUMMY1)   ,
                TGEUSR= CURRENT_USER,
                TGSSTA='2'
            WHERE
                    TGAFEC = :fecha_a_informar
                AND TGELOT = 0
                AND TGSSTA = '0';

    endsr;

    //--------------------------------------------------------------------------
    begsr determinar_nombre_de_archivo;
    //--------------------------------------------------------------------------
        //RODE.Eeeeennn.FPaammdd.DATOS
        //Donde:
        //(1) R = Fijo, indica que se trata de un archivo REFRESH.
        //ODE = Fijo "ODE" (Órdenes de extracción).
        //E = Fijo "E"
        //eeee = Numero de Entidad según RED LINK S.A.
        //nnnn = Indica el número de envió p/ la misma fecha comenzando p/0000.
        //FP = Fijo "FP" (Fecha de Proceso)
        //aa = indica el a¦o de proceso del archivo.
        //mm = indica el mes de proceso del archivo.
        //dd = indica el día de proceso del archivo.
        //DATOS = extensión del archivo.
        lote4= nuevo_lote -1;
        if lote4 < *zero;
            lote4 = *zero;
        endif;
        nombre_archivo='RODE.E0309'
                        + %editc(lote4:'X')
                        +'.FP'
                        + %subst(%editc(fecha_a_informar:'X'):3:6)
                        +'.DATOS';

    endsr;

    //--------------------------------------------------------------------------
    begsr determinar_nuevo_nro_de_lote;
    //--------------------------------------------------------------------------
        exec sql
            SELECT
                MAX(IFNULL(TGELOT, 0))+1 into :nuevo_lote
            FROM LIMODE01
            WHERE
                    TGAFEC = :fecha_a_informar;

        if sqlcode <> *zero;
            error = 'no se encontraron registros para la fecha';
            exsr endpgm;
        endif;
    endsr;
    //--------------------------------------------------------------------------
    begsr abrir_archivo_ifs;
    //--------------------------------------------------------------------------
        exec sql select
               TRIM(DPVALU)
               into :fqn from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/DIRENV');
        fqn = fqn + '/' + nombre_archivo;
        fd = ifs_stmf_opnwrt(fqn);
        if (fd < 0);
           error = 'No se pudo abrir archivo:'+fqn;
           exsr endpgm;
        endif;
    endsr;

    //--------------------------------------------------------------------------
    begsr cerrar_archivo_ifs;
    //--------------------------------------------------------------------------
        ifs_stmf_close(fd);
    endsr;

    //-------------------------------------------------------------------------
    begsr abrir_cursor;
    //-------------------------------------------------------------------------
        exec sql DECLARE C1 CURSOR  FOR
            SELECT
                *
            FROM LIMODE01
            WHERE
                    TGAFEC = :fecha_a_informar
                AND TGELOT = 0
                AND TGSSTA = '0'
            ;
        exec sql open C1;

        if sqlcode <> *zero;
            exsr endpgm;
        endif;
    endsr;

    //-------------------------------------------------------------------------
    begsr cerrar_cursor;
    //-------------------------------------------------------------------------
        exec sql CLOSE C1;
    endsr;

    //-------------------------------------------------------------------------
    begsr endpgm;
    //-------------------------------------------------------------------------
        exsr cerrar_cursor;
        exsr cerrar_archivo_ifs;

        if error = *blanks;
            error='Se ha creado el archivo:'+
                    nombre_archivo;
        endif;
        return;
    endsr;
end-proc;


/copy desa00525/qrpgsrc,ligdimpl ;

