*SRCMBRTXT:Link-ODE-Marca ODE Como Cobrada        
**free
ctl-opt dftname(LITG60RG)
        option(*SRCSTMT)
        dftactgrp(*NO)
        main(main);

dcl-c MAX_FILE_SIZE 65535;

//Declaración de prototipos, importadas de los miembros declarados

//Procedimiento main, declarado como punto de entrada
//en ctl-opt
dcl-proc main;

    //*entry plist
    dcl-pi *n;
        documento_numero        packed(8:0);
        importe                 packed(15:2);
        numero_secuencia        packed(7:0);
        fecha_ingreso           packed(8:0);
        hora_ingreso            packed(6:0);
        tipo_transaccion        packed(4:0);
    end-pi;

    dcl-ds limode EXTNAME('LIMODE')  end-ds;

    dcl-s   ode_encontrada      ind;

    exec sql set option commit = *none;

    exsr crear_tabla_para_reporte;
    if tipo_transaccion = 200 or
       tipo_transaccion = 210 or
       tipo_transaccion = 220 or
       tipo_transaccion = 230 ;
       exsr encontrar_ode_impaga_para_importe;
       if ode_encontrada;
          exsr marcar_ode_como_cobrada;
       endif;
    else;
       exsr encontrar_ode_pagada_para_importe;
       if ode_encontrada;
          exsr marcar_ode_como_reversada;
       endif;
    endif;
    exsr agregar_fila_a_reporte;
    exsr finalizar_programa;

    //--------------------------------------------------------------------------
    begsr crear_tabla_para_reporte;
    //--------------------------------------------------------------------------
        //La primera vez se crea en qtemp, las sucesivas fallará pero la tabla
        //acumulará registros para ser enviados.
        exec sql
        CREATE TABLE QTEMP/ODEREP (
        TRANS_FECHA      FOR COLUMN TRNFEC DEC  ( 8, 0) NOT NULL WITH DEFAULT,
        TRANS_HORA       FOR COLUMN TRNHOR DEC  ( 6, 0) NOT NULL WITH DEFAULT,
        TRANS_SEQ        FOR COLUMN TRNSEQ DEC  ( 7, 0) NOT NULL WITH DEFAULT,
        TRANS_IMPORTE    FOR COLUMN TRNIMP DEC  (15, 2) NOT NULL WITH DEFAULT,
        TRANS_MASIVA     FOR COLUMN TRNMAS CHAR ( 1   ) NOT NULL WITH DEFAULT,
        TRANS_RRN        FOR COLUMN TRNRRR DEC  (15, 0) NOT NULL WITH DEFAULT,
        ODE_DNI          FOR COLUMN ODEDNI DEC  ( 8, 0) NOT NULL WITH DEFAULT,
        ODE_NOMBRE       FOR COLUMN ODENYA CHAR (50   ) NOT NULL WITH DEFAULT
        );

    endsr;

    //--------------------------------------------------------------------------
    begsr encontrar_ode_impaga_para_importe;
    //--------------------------------------------------------------------------
        ode_encontrada = *off;

        clear limode;

        exec sql SELECT
                    * INTO :limode
                FROM LIMODE
                WHERE
                        TGONDO  =   :documento_numero
                    AND TGOIMP  =   :importe
                    AND TGSSTA  IN  ('2', '4')
                    AND TGSSTA  NOT IN  ('0', '1', '3', '5', '9')
                LIMIT 1;
        if sqlcode = *zero;
            ode_encontrada = *on;
        endif;

    endsr;
    //--------------------------------------------------------------------------
    begsr encontrar_ode_pagada_para_importe;
    //--------------------------------------------------------------------------
        ode_encontrada = *off;

        clear limode;

        exec sql SELECT
                    * INTO :limode
                FROM LIMODE
                WHERE
                        TGONDO  =   :documento_numero
                    AND TGOIMP  =   :importe
                    AND TGSSTA  IN  ('5')
                LIMIT 1;
        if sqlcode = *zero;
            ode_encontrada = *on;
        endif;

    endsr;

    //--------------------------------------------------------------------------
    begsr marcar_ode_como_cobrada;

        exec sql    UPDATE LIMODE SET
                        TGSSTA  =   '5',
                        TGFEPA  =   :fecha_ingreso,
                        TGHORA  =   :hora_ingreso,
                        TGICHE  =   :numero_secuencia
                    WHERE
                        TGIRRN  =   :tgirrn;
    endsr;
    //--------------------------------------------------------------------------
    begsr marcar_ode_como_reversada;

        exec sql    UPDATE LIMODE SET
                        TGSSTA  =   '4',
                        TGFEPA  =   0,
                        TGHORA  =   0,
                        TGICHE  =   0,
                        TGfd04  =   'COBRO REV.X LNK'
                    WHERE
                        TGIRRN  =   :tgirrn;
    endsr;

    //--------------------------------------------------------------------------
    begsr agregar_fila_a_reporte;
    //--------------------------------------------------------------------------
        exec sql    INSERT INTO ODEREP
                    VALUES (
                                :fecha_ingreso,
                                :hora_ingreso,
                                :numero_secuencia,
                                :importe,
                                :ode_encontrada,
                                :tgirrn,
                                :tgondo,
                                :tgoncl
                            );

    endsr;

    //--------------------------------------------------------------------------
    begsr finalizar_programa;
    //--------------------------------------------------------------------------
        return;
    endsr;


end-proc;
