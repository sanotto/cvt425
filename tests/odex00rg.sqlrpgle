*SRCMBRTXT:ODE - Programa de carga adm de excepcio
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*NO)
            main(main);

//DECLARO HOJA F PARA REALIZAR CHAIN
dcl-f @CPIUSD   KEYED;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

//... Variables
    dcl-s count              packed(1:0);
    dcl-s nro_job            packed(6:0);
    dcl-s fecha              packed(8:0);
    dcl-s usuario            char(10);
    dcl-s hora               packed(6:0);

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

    //Nombrar indicadores
   dcl-ds datos qualified;
         doc                 packed(15:0);
         nombre_apellido     char(50);
   end-ds;


//Aca empieza el programa

    exsr inicializar_variables;
    exsr consulta_baexep;
    exsr cargar_excepcion;
    exsr fin_de_programa;

//==============================================================================
// Pregnto si ya existe en archivo BAEXEP
//==============================================================================
 begsr inicializar_variables;

exec sql set option commit=*None;
EXEC SQL select aasfei into :fecha from sgsysv;
EXEC SQL select current_user into :usuario from sysibm.sysdummy1;
exec sql SELECT SUBSTRING(JOB_NAME, 01, 06) INTO :nro_job FROM SYSIBM.SYSDUMMY1;

    //RECUERO VALORES DE CPISYS
    chain (nro_job)  @CPIUSD;
    datos.doc             = @CINDO ;
    datos.nombre_apellido = @CNYAP ;
    hora   = %dec(%time():*HMS);
 endsr;


//==============================================================================
// Pregnto si ya existe en archivo BAEXEP
//==============================================================================
 begsr consulta_baexep;
   exec sql
           SELECT
            COUNT(*)
            INTO :count
           FROM
               BAEXEP
           WHERE
            EXISUB = 'OD'                 AND
            EXICAH = :datos.doc           AND
            EXICCC = 0
           LIMIT 1;

   if count <> 0 ;
     mostrar_mensaje(
     '               EXCEPCIONES - ODE                                  ':
     '                                                                  ':
     'Ya existe la persona que quiere cargar como excepcion             ':
     '                                                                  ':
     '                                                                  ':
     '                                                                  ':
     '                                                                  ':
     'INTRO=Aceptar                                                     ');
     exsr fin_de_programa;
   endif;

 endsr;


//==============================================================================
// Cargo excepcion para la persona seleccionada
//==============================================================================

 begsr cargar_excepcion;

   exec sql
             INSERT INTO BAEXEP
             VALUES('OD',
                     0,
                     :datos.doc,
                     0,
                     0,
                     0,
                     ' ',
                     :fecha,
                     :hora,
                     :usuario);

   mostrar_mensaje(
     '               EXCEPCIONES - ODE                                  ':
     '                                                                  ':
     'Se cargo la siguiente persona como excepcion:                     ':
     '   DNI: '    + %char(datos.doc)                                    :
     '   NOMBRE: ' + datos.nombre_apellido                               :
     '                                                                  ':
     '                                                                  ':
     'INTRO=Aceptar                                                     ');

 endsr;


//==============================================================================
// Fin de programa
//==============================================================================

    begsr fin_de_programa;
        return;
    endsr;

end-proc;
