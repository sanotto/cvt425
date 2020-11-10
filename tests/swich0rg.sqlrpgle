*SRCMBRTXT:Crea archivos csv empleado-emp-tit-SUEL
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*NO)
            main(main);

dcl-c MAX_RETRIES 3;
dcl-c MAX_FILE_SIZE 65535;

/include sdb01.src/qrpgsrc,ligdprot ;
/include sdb01.src/qrpgsrc,ligdifs ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

//... Variables
    dcl-s cmd                char(4096);
    dcl-s rc                 char(7);
    dcl-s connme             char(15);
    dcl-s referencia         char(15);
    dcl-s corriendo          packed(2:0);
    dcl-s cant_antes         packed(8:0);
    dcl-s cant_despues       packed(8:0);
    dcl-s respuesta          packed(8:0);

//Aca empieza el programa
exec sql set option commit=*None;
    exsr consultas;
    exsr guardo_variables;
    exsr fin_de_programa;



//==============================================================================
// Declaro las consultas para guardar los datos.
//==============================================================================

begsr consultas;

  exec sql DELETE FROM SGPCLG WHERE LGISYS = 'SWITCH';
//Consulta para saber si el swich esta funcionando
  exec sql SELECT 'H2H'||COIBCF INTO :connme
         FROM LICONN LIMIT 1;

  exec sql SELECT count(*) into :corriendo
         FROM TABLE(QSYS2.ACTIVE_JOB_INFO( JOB_NAME_FILTER => TRIM(:connme),
                    SUBSYSTEM_LIST_FILTER => 'QBATCH')) X ;

//Consulta para saber si entran operaciones
  exec sql SELECT COUNT(*) INTO :cant_antes FROM LILOGF;

  exeCmd('DLYJOB DLY(10)');

  exec sql SELECT COUNT(*) INTO :cant_despues FROM LILOGF;

endsr;

//==============================================================================
// Guardo datos
//==============================================================================

    begsr guardo_variables;

   //corriendo
    if corriendo  = 0;
       //Servicio no activo
       referencia = 'servicio';
       respuesta  = 0;
    else;
       //Servicio activo
       referencia = 'servicio';
       respuesta  = 1;
    endif;
   exsr Insertar_datos_SGPCLG;

    //Verificar si estan entrando movimientos
    if cant_antes < cant_despues;
       //Entrada de movimientos
       referencia = 'movimientos';
       respuesta  = 0;
    else;
       //Sin entradas de movimientos.
       referencia = 'movimientos';
       respuesta  = 1;
    endif;
    exsr Insertar_datos_SGPCLG;

    endsr;

//==============================================================================
// Inserto registro en SGPCLG  cada vez que ejecuto un escrip
//==============================================================================
    begsr Insertar_datos_SGPCLG;

      exec sql INSERT INTO SGPCLG
                           (LGISYS,
                            LGFECH,
                            LGREFD)

                     VALUES(
                           'SWITCH',
                           :respuesta,
                           :referencia);
    endsr;

//==============================================================================
// Fin de programa
//==============================================================================

    begsr fin_de_programa;
        return;
    endsr;

end-proc;
/copy sdb01.src/qrpgsrc,ligdimpl ;
