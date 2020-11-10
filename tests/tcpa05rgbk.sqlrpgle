*SRCMBRTXT:TC: Envía informe TSB por correo       
**free
ctl-opt dftname(TCPA05RG)
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
    dcl-pi *n;
        ac_det        char(250);
        cc_det        char(250);
        ac_acu        char(250);
        cc_acu        char(250);
        cc_control    char(250);
        cc_excepci    char(250);
        cc_negativ    char(250);
        cc_ctrl_com   char(250);
        nro_camp      packed(10:0);
        acredita      char(1);
    end-pi;

    //Declaro Variables
    dcl-s subject                 char(256) inz(*blanks);
    dcl-s mesg                    char(256) inz(*blanks);
    dcl-s file                    char(256) inz(*blanks);

    dcl-s new_dir                 char(256) inz(*blanks);
    dcl-s new_dir_zip             char(256) inz(*blanks);
    dcl-s zip_dir                 char(256) inz(*blanks);
    dcl-s zip_dir_final           char(256) inz(*blanks);
    dcl-s n1                      char(256) inz(*blanks);
    dcl-s n2                      char(256) inz(*blanks);
    dcl-s n3                      char(256) inz(*blanks);
    dcl-s n4                      char(256) inz(*blanks);
    dcl-s n5                      char(256) inz(*blanks);
    dcl-s n6                      char(256) inz(*blanks);
    dcl-s n7                      char(256) inz(*blanks);
    dcl-s n8                      char(256) inz(*blanks);

    dcl-s nro_movimientos         packed(10:0) inz(*zeros);
    dcl-s nro_cuentas             packed(10:0) inz(*zeros);
    dcl-s devolucion              packed(15:2) inz(*zeros);
    dcl-s devolucion_tope         packed(15:2) inz(*zeros);
    dcl-s nombre_camp             char(30) inz(*blanks);

    dcl-s aasfei                  packed(8:0);
    dcl-s aasfei_aaaammdd         char(10);
    dcl-s hora                    packed(6:0);
    dcl-s comando                 char(250) inz(*blanks);
    dcl-s rc                      char(7);
    dcl-s cmd                     varchar(4096);

   dcl-pr CallSGAUTAC4 extpgm('SGAUTAC4');
       comando        char(250);
   end-pr;
//*****************************************************************************
// Ciclo de programa
//*****************************************************************************
EXSR Inicializacion;
EXSR GenerarAdjunto;
EXSR GenerarMail;
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

ENDSR;
//*****************************************************************************
// GenerarAdjunto
//*****************************************************************************
BEGSR GenerarAdjunto;

  new_dir     = '/home/Tarjetas_de_Creditos/TSB/'+%char(aasfei)+'_'+%char(hora)+
                '_'+%trim(%char(nro_camp));
  new_dir_zip = '/home/Tarjetas_de_Creditos/TSB/'+%char(aasfei)+'_'+%char(hora)+
                '_'+%trim(%char(nro_camp))+'_zip';
  zip_dir       = %trim(new_dir_zip)+'/'+%char(aasfei)+'_'+%char(hora)+
                  '_'+%trim(%char(nro_camp))+'.zip';
  zip_dir_final = %trim(new_dir)    +'/'+%char(aasfei)+'_'+%char(hora)+
                  '_'+%trim(%char(nro_camp))+'.zip';

  cmd = 'MKDIR ('''+%trim(new_dir)+''') ';
  rc=exeCmd(cmd);
  cmd = 'MKDIR ('''+%trim(new_dir_zip)+''') ';
  rc=exeCmd(cmd);

   exec sql select substr(:ac_det, 28, 222) into :n1 from sysibm.sysdummy1;
   exec sql select substr(:cc_det, 28, 222) into :n2 from sysibm.sysdummy1;
   exec sql select substr(:ac_acu, 28, 222) into :n3 from sysibm.sysdummy1;
   exec sql select substr(:cc_acu, 28, 222) into :n4 from sysibm.sysdummy1;
   exec sql select substr(:cc_control, 28, 222) into :n5 from sysibm.sysdummy1;
   exec sql select substr(:cc_excepci, 28, 222) into :n6 from sysibm.sysdummy1;
   exec sql select substr(:cc_negativ, 28, 222) into :n7 from sysibm.sysdummy1;
   exec sql select substr(:cc_ctrl_com, 28, 222) into :n8 from sysibm.sysdummy1;


   comando = 'mv '+%trim(ac_det)+' '+%trim(new_dir)+'/'+n1;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(cc_det)+' '+%trim(new_dir)+'/'+n2;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(ac_acu)+' '+%trim(new_dir)+'/'+n3;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(cc_acu)+' '+%trim(new_dir)+'/'+n4;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(cc_control)+' '+%trim(new_dir)+'/'+n5;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(cc_excepci)+' '+%trim(new_dir)+'/'+n6;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(cc_negativ)+' '+%trim(new_dir)+'/'+n7;
   CallSGAUTAC4(comando);
   comando = 'mv '+%trim(cc_ctrl_com)+' '+%trim(new_dir)+'/'+n8;
   CallSGAUTAC4(comando);

   cmd = 'ZIPF ('''+%trim(zip_dir)+''')'+
         ' FILES('''+%trim(new_dir)+''')';
   rc=exeCmd(cmd);

   comando = 'mv '+%trim(zip_dir)+' '+%trim(zip_dir_final);
   CallSGAUTAC4(comando);
   comando = 'rmdir ' +%trim(new_dir_zip);
   CallSGAUTAC4(comando);

ENDSR;
//*****************************************************************************
// GenerarMail
//*****************************************************************************
BEGSR GenerarMail;

    exec sql
    select cpdav1 into :nombre_camp from licamp where cpisfo=:nro_camp limit 1;

    subject = 'TE SUPER BANCO: '+%trim(nombre_camp);

    exec sql
      WITH rs AS (
      SELECT COUNT(*) AS nro_mov FROM QTEMP/ACINT1
      union all
      SELECT COUNT(*) AS nro_mov FROM QTEMP/CCINT1
      )
      SELECT SUM(nro_mov) INTO :nro_movimientos FROM rs;

    exec sql
      WITH rs AS (
      SELECT COUNT(*) AS nro_ctas FROM QTEMP/ACINTE
      union all
      SELECT COUNT(*) AS nro_ctas FROM QTEMP/CCINTE
      )
      SELECT SUM(nro_ctas) INTO :nro_cuentas FROM rs;

    exec sql
      WITH rs AS (
      SELECT SUM(IN$IMP) AS devolucion FROM QTEMP/ACINT1
      union all
      SELECT SUM(IN$IMP) AS devolucion FROM QTEMP/CCINT1
      )
      SELECT SUM(CAST(devolucion AS DECIMAL(15, 2))) INTO :devolucion FROM rs;

    exec sql SELECT SUM(E6$IMP) INTO :devolucion_tope FROM QTEMP/ESTAH6;

    if acredita = 'N';
        mesg = %trim(mesg) + 'Se recibió:' + X'0D'+ X'25';
    else;
        mesg = %trim(mesg) + 'Se acreditó:' + X'0D'+ X'25';
    endif;

    mesg = %trim(mesg) +
           'Campa¦a: '     + %trim(nombre_camp) + ' ('
                           + %trim(%char(nro_camp)) + ')'
                                                    + X'0D'+ X'25' +
           'Movimientos: ' + %char(nro_movimientos) + X'0D'+ X'25' +
           'Cuentas: '     + %char(nro_cuentas)     + X'0D'+ X'25' +
           'Devolución: '  + %char(devolucion)      + X'0D'+ X'25' +
    'Devolución con tope: '+ %char(devolucion_tope) + X'0D'+ X'25' ;

    file = zip_dir_final;

ENDSR;
//*****************************************************************************
// EnviarMail
//*****************************************************************************
BEGSR EnviarMail;

     cmd = 'SNDMAIL RECP(TCPA05RG) '+
           'SUBJECT('''+%trim(subject)+''') '+
           'MESG('''+%trim(mesg)+''') '+
           'FILE('''+%trim(file)+''')';
     rc = exeCmd(cmd);

     if rc <> 'CPF0000';

     endif;

ENDSR;
//*****************************************************************************
// FinPrograma: Termina el programa
//*****************************************************************************
BEGSR FinPrograma;
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
