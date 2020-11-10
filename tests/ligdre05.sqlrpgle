*SRCMBRTXT:Recuperar Nombre GDE AC-CC             
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

    //Entry plist
    dcl-pi *n;
        fqn                  char(255);
        ts                   char(2);
        msj                  char(1);
    end-pi;

    dcl-s import_ok             ind;
    dcl-s path_to_out           varchar(255);
    dcl-s path_to_dir           varchar(255);
    dcl-s cmd                   varchar(4096);
    dcl-s rc                    char(7);
    dcl-s aasfei                char(8);
    dcl-s nombre_liserj         char(30);
    dcl-s nombre_archivo        char(30);

    exec sql SET OPTION COMMIT = *NONE;

    exsr recuperar_fecha_archivo;
    exsr recuerar_nombre_archivo_gde;
    exsr verificar_archivo_procesado;

//==============================================================================

    begsr recuperar_fecha_archivo;

        exec sql SELECT AASFEI INTO :aasfei FROM SGSYSV;

    endsr;

//==============================================================================

    begsr recuerar_nombre_archivo_gde;

        if (ts = 'CA');

        fqn = 'RGDE0309.RCCAE001.FP'+%Subst(aasfei:3:6)+'.DATOS';

        else;

        fqn = 'RGDE0309.RCCCE001.FP'+%Subst(aasfei:3:6)+'.DATOS';

        endif;

    endsr;

//==============================================================================

    begsr verificar_archivo_procesado;

    nombre_archivo = %Subst(fqn:1:30);

    exec sql
            SELECT RJDACO
            INTO :nombre_liserj
            FROM LISERJ
            WHERE RJDACO = :nombre_archivo;

        if sqlcod = *zero;

            msj = '1';

        else;

        //exsr enviar_correo;
        exsr guardar_en_liserj;

        endif;

    endsr;

//==============================================================================

    begsr enviar_correo;

        cmd = 'SNDMAIL                                               '+
              '   RECP(''LISERVCX'')                                 '+
              '   SUBJECT(''Generación GDE Resumen de Cuenta AC-CC'')'+
              '   MESG(''Se genero el archivo de Resumen de Cuenta'')';
        rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr guardar_en_liserj;

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

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
