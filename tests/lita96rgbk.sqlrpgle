*SRCMBRTXT:Genera reportes p/OCA TDSO305          
**free
ctl-opt dftname(LITA96RG)
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
    //    nombre_archivo        char(30);
    //    path_archivo          char(250);
    //    error                 char(1);
    //end-pi;

    //Declaro Variables
    dcl-s error                   packed(1:0) inz(1);
    dcl-s error_Inicializacion    packed(1:0) inz(0);
    dcl-s error_VerificarArchivo  packed(1:0) inz(0);
    dcl-s error_OpenCursors       packed(1:0) inz(0);
    dcl-s error_ReadC1            packed(1:0) inz(0);
    dcl-s error_WrtARCHIVO        packed(1:0) inz(0);
    dcl-s error_GenerarInforme    packed(1:0) inz(0);
    dcl-s error_EnviarMail        packed(1:0) inz(0);
    dcl-s subject                 char(256) inz(*blanks);
    dcl-s mesg                    char(256) inz(*blanks);
    dcl-s file                    char(256) inz(*blanks);

    dcl-s aasfei                  packed(8:0);
    dcl-s aasfei_aaaammdd         char(10);
    dcl-s hora                    packed(6:0);
    dcl-s path_full               char(250) inz(*blanks);
    dcl-s path_full_historico     char(250) inz(*blanks);
    dcl-s comando                 char(250) inz(*blanks);
    dcl-s rc                      char(7);
    dcl-s registro                char(256);
    dcl-s cmd                     varchar(4096);

    dcl-s wwdf06                  char(9);
    dcl-s wwncal                  char(30);
    dcl-s wwipue                  packed(6:0);
    dcl-s wwnloc                  char(30);
    dcl-s wwicpo                  packed(5:0);

    dcl-s bandera_1               packed(2:0);
    dcl-s bandera_2               packed(2:0);


    dcl-ds ds_reg qualified;
        aeitdm packed(2:0);
        taindo packed(15:0);
        tantar packed(16:0);
        tanyap char(30);
        aencal char(30);
        aeipue packed(6:0);
        aeicpo packed(5:0);
        alnloc char(30);
        oxnloc char(30);
    end-ds;


    dcl-pr CallSGAUTAC4 extpgm('SGAUTAC4');
        comando        char(250);
    end-pr;

//*****************************************************************************
// Ciclo de programa
//*****************************************************************************
EXSR Inicializacion;
EXSR GenerarInforme;
EXSR EnviarMail;
EXSR FinPrograma;

//*****************************************************************************
//                  SUBRUTINAS
//*****************************************************************************
// Inicializacion: Inicializa programa
//*****************************************************************************
BEGSR Inicializacion;

     exec sql set option commit=*None;
     exec sql select aasfei into :aasfei from sgsysv limit 1;
     aasfei_aaaammdd =  %subst(%char(aasfei): 1: 4)+
                        %subst(%char(aasfei): 5: 2)+
                        %subst(%char(aasfei): 7: 2);
     exec sql select replace(char(current_time), ':', '') into :hora
              from sysibm.sysdummy1;
     path_full_historico = '/home/Tarjetas_de_Debitos/HISTORICO/TDSO305_' +
                           %trim(aasfei_aaaammdd)+'.xls';

    if sqlcode <> 0;
        error_Inicializacion = 1;
        EXSR FinPrograma;
    endif;

ENDSR;
//*****************************************************************************
// GenerarInforme
//*****************************************************************************
BEGSR GenerarInforme;

     cmd= 'EXPPCDBF SQLSTM('''+
           'SELECT '+
           'SUBSTR(WWTEMP, 95, 3) as SUCURSAL       ,'+
           'SUBSTR(WWTEMP, 3, 16) as NRO_TARJETA    ,'+
           'SUBSTR(WWTEMP, 61, 30) as TITULAR       ,'+
           'SUBSTR(WWTEMP, 103, 24) as DIRECCION    ,'+
           'SUBSTR(WWTEMP, 127, 6) as NUMERO        ,'+
           'SUBSTR(WWTEMP, 143, 20) as LOCALIDAD    ,'+
           'SUBSTR(WWTEMP, 138, 5) as CODIGO_POSTAL '+
           'FROM QTEMP/WWTEMP '+
           'WHERE SUBSTR(WWTEMP, 1, 1)=''''2'''' AND '+
           'SUBSTR(WWTEMP, 227, 5)=''''00501'''''+
           ''') OUTPAT('''+%trim(path_full_historico)+''')';
     rc = exeCmd(cmd);

     cmd = 'CALL SGAUTACL ('''+path_full_historico+''')';
     rc = exeCmd(cmd);

ENDSR;
//*****************************************************************************
// EnviarMail
//*****************************************************************************
BEGSR EnviarMail;

    // Genero Correo electrónico
    subject = 'Informe de TD entregadas OCA ('+
                        %subst(%char(aasfei): 7: 2)+'/'+
                        %subst(%char(aasfei): 5: 2)+'/'+
                        %subst(%char(aasfei): 1: 4)+')';
    mesg = 'Se adjunta listado de TD del dia: ('+
                        %subst(%char(aasfei): 7: 2)+'/'+
                        %subst(%char(aasfei): 5: 2)+'/'+
                        %subst(%char(aasfei): 1: 4)+')';
    file = %trim(path_full_historico);

     cmd = 'SNDMAIL RECP(LITA96RG) '+
           'SUBJECT('''+%trim(subject)+''') '+
           'MESG('''+%trim(mesg)+''') '+
           'FILE('''+%trim(file)+''')';
     rc = exeCmd(cmd);

     if rc <> 'CPF0000';
        error_EnviarMail = 1;
     endif;

ENDSR;
//*****************************************************************************
// FinPrograma: Termina el programa
//*****************************************************************************
BEGSR FinPrograma;

    // Analizo posibles errores criticos
    error=0;
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
