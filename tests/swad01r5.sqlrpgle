*SRCMBRTXT:Descomprime zip y mueve a historico    
**free
ctl-opt dftname(SWAD01R5)
        option(*SRCSTMT)
        dftactgrp(*NO)
        main(main);

dcl-ds pgm_stat PSDS qualified;
        exception_type   char(3)        pos(40);
        job_number_char  char(6)        pos(264);
        job_number_num   packed(6:0)    pos(264);
        job_cur_usr      char(10)       pos(358);
        exception_number char(4)        pos(43);
end-ds;

dcl-pr exeCmd char(7);
    cmd_str   char(4096) const;
end-pr;

//*****************************************************************************
// Procedimiento Main: PRINCIPAL
//*****************************************************************************
dcl-proc Main;
    //ENTRY PLIST
    //dcl-pi *n;
    //    dir_archivo        char(255); //dir del archivo a trabajar
    //    dir_unzip          char(255); //dir donde descomprimir zip
    //    dir_destino        char(255); //dir donde mover zip una vez trabajado
    //    error              char(50);
    //end-pi;

    //Declaro Variables
    dcl-s error_ValidarArchivo    packed(1:0) inz(0);
    dcl-s aasfei                  packed(8:0);
    dcl-s aasfei_aaaammdd         char(10);
    dcl-s hora                    packed(6:0);
    dcl-s usuario                 char(10);
    dcl-s comando                 char(250) inz(*blanks);
    dcl-s rc                      char(7);
    dcl-s cmd                     varchar(4096);
    dcl-s sufijo                  char(4);
    dcl-s nombre_archivo          char(255);
    dcl-s nom_cbug_orix_desx      char(255);
    dcl-s dir_unzip               char(255);
    dcl-s dir_destino_procesado   char(255);
    dcl-s dir_destino_rechazado   char(255);



   dcl-pr CallSGAUTAC4 extpgm('SGAUTAC4');
       comando        char(250);
   end-pr;

//*****************************************************************************
// Ciclo de programa
//*****************************************************************************
EXSR Inicializacion;
EXSR OpenCursors;
EXSR ReadC1;
dow  sqlcode = *Zero;
    EXSR ValidarArchivo;
    if  error_ValidarArchivo = 0;
        if  sufijo = '.zip';
            EXSR DescomprimirArchivo;
            EXSR MoverArchivoProcesado;
        endif;
    else;
        EXSR RechazarArchivo;
    endif;
    EXSR ReadC1;
enddo;
EXSR FinPrograma;

//*****************************************************************************
//                  SUBRUTINAS
//*****************************************************************************
// Inicializacion: Inicializa programa
//*****************************************************************************
BEGSR Inicializacion;

     exec sql set option commit=*None;
     exec sql select aasfei into :aasfei from sgsysv limit 1;
     aasfei_aaaammdd =  %trim(%editw(aasfei:'        '));
     exec sql select replace(char(current_time), ':', '') into :hora
              from sysibm.sysdummy1;
     exec sql select replace(char(current_time), ':', '') into :hora
              from sysibm.sysdummy1;
     exec sql select current_user into :usuario from sysibm.sysdummy1;
     dir_unzip = '/home/LINKBEE/Extracts/';
     dir_destino_procesado='/home/LINKBEE/Procesados/';
     dir_destino_rechazado='/home/LINKBEE/Rechazados/';

ENDSR;
//*****************************************************************************
// OpenCursors
//*****************************************************************************
BEGSR OpenCursors;

    exec sql DECLARE C1 CURSOR FOR
        SELECT CAST (FILENAME as char(255)) AS FILNAM
        FROM TABLE(IFSDIR('/home/LINKBEE/Extracts/')) AS T;

    exec sql OPEN C1;

ENDSR;
//*****************************************************************************
// ReadC1
//*****************************************************************************
BEGSR ReadC1;

     exec sql FETCH C1 into :nombre_archivo;

ENDSR;
//*****************************************************************************
// ValidarArchivo:
//*****************************************************************************
BEGSR ValidarArchivo;

    error_ValidarArchivo = 0;
    sufijo =  %subst(nombre_archivo : %len(%trim(nombre_archivo))-3 : 4);

    if %check('0123456789' : sufijo) > 0;
            error_ValidarArchivo = 1;
    endif;
    if sufijo = '.zip' ;
            error_ValidarArchivo = 0;
    endif;


ENDSR;
//*****************************************************************************
// DescomprimirArchivo
//*****************************************************************************
BEGSR DescomprimirArchivo;

//CALL PGM(SGAUTAC4) PARM('/QOpenSys/pkgs/bin/unzip
//                        ''/home/PR00685/prueba_zip/f.zip''
//                     -d ''/home/PR00685/prueba_zip/historico''  ')

    comando = '/QOpenSys/pkgs/bin/unzip '+
              ''''''+%trim(dir_unzip)+%trim(nombre_archivo)+''''''+
              ' -d '''''+%trim(dir_unzip)+''''' ';
    CallSGAUTAC4(comando);

    exec sql
        SELECT
              CAST (FILENAME as char(255)) AS FILNAM
              INTO :nom_cbug_orix_desx
        FROM TABLE(IFSDIR('/home/LINKBEE/Extracts/$DATA4/EXT0309/')) AS T
        LIMIT 1;

    comando = 'mv ''/home/LINKBEE/Extracts/$DATA4/EXT0309/'+
                  %trim(nom_cbug_orix_desx) +''''+
                  ' ''/home/LINKBEE/Extracts/'+
                  %trim(nom_cbug_orix_desx) +'''';
    CallSGAUTAC4(comando);

     cmd = 'RMDIR DIR(''/home/LINKBEE/Extracts/$DATA4/'') SUBTREE(*ALL)';
     rc = exeCmd(cmd);

ENDSR;
//*****************************************************************************
// RechazarArchivo
//*****************************************************************************
BEGSR RechazarArchivo;

   comando = 'mv '+%trim(dir_unzip)+%trim(nombre_archivo)+' '+
                   %trim(dir_destino_rechazado)+
                   %trim(nombre_archivo)+'_'+
                   %trim(aasfei_aaaammdd)+'_'+
                   %trim(%char(hora))+'_'+
                   %trim(usuario);

   CallSGAUTAC4(comando);

ENDSR;
//*****************************************************************************
// MoverArchivoProcesado
//*****************************************************************************
BEGSR MoverArchivoProcesado;

   comando = 'mv '+%trim(dir_unzip)+%trim(nombre_archivo)+' '+
                   %trim(dir_destino_procesado)+'/'+
                   %trim(nombre_archivo)+'_'+
                   %trim(aasfei_aaaammdd)+'_'+
                   %trim(%char(hora))+'_'+
                   %trim(usuario);

   CallSGAUTAC4(comando);

ENDSR;
//*****************************************************************************
// FinPrograma: Termina el programa
//*****************************************************************************
BEGSR FinPrograma;

    exec sql close C1;
    return;

ENDSR;
end-proc;

//*****************************************************************************
//         PROCEDIMIENTOS ADICIONALES
//*****************************************************************************
// exeCmd: Ejecuta un comando del sistema operativo
//         y retorn el cpf resultante.
//*****************************************************************************
dcl-proc exeCmd;

    dcl-pi *n char(7);                  // <= CPF Resultante
        cmd_str char(4096) const;       // => String con un comando CL
    end-pi;

    dcl-pr QcmdExc extpgm('QCMDEXC');
        cmdstr  char(4096) const;
        cmdlen  packed(15:5) const;
    end-pr;

    QcmdExc(cmd_str:4096);
    return 'CPF0000';

    begsr *pssr;
        return pgm_stat.exception_type  +
               pgm_stat.exception_number;
    endsr;

end-proc;
//*****************************************************************************
