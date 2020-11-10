*SRCMBRTXT:Verifica el funcionamiento del Switch/R
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);


  dcl-ds pgm_stat PSDS ;
        exception_type   char(3)        pos(40);
        job_number_char  char(6)        pos(264);
        job_number_num   packed(6:0)    pos(264);
        job_cur_usr      char(10)       pos(358);
        exception_number char(4)        pos(43);
  end-ds;


//Llamada al Programa BAER00RS.
dcl-pr BAER00RS extpgm('BAER01RS');
    MSJ1                  char(55) const;
    MSJ2                  char(55) const;
    MSJ3                  char(55) const;
    MSJ4                  char(55) const;
    MSJ5                  char(55) const;
    MSJ6                  char(55) const;
    MSJ7                  char(55) const;
    MSJ8                  char(55) const;
end-pr;

//==============================================================================
// Procesos o Funciones.
//==============================================================================
dcl-proc main;

  dcl-pi *n;
      continuar                char(1);
  end-pi;


  dcl-s connme                 char(10);
  dcl-s corriendo              packed(15:0) inz(0);
  dcl-s cant_antes             packed(15:0) inz(0);
  dcl-s cant_despues           packed(15:0) inz(0);
  dcl-s totori                 packed(15:0) inz(0);
  dcl-s totfor                 packed(15:0) inz(0);
  dcl-s totgra                 packed(15:0) inz(0);
  dcl-s porori                 packed(15:0) inz(0);
  dcl-s porfor                 packed(15:0) inz(0);
  dcl-s strerr                 char(55) inz(*blanks);
  dcl-s f10                    char(1) inz(*zero);

  continuar = *on;
  exsr verificar_si_la_conn_estar_corriendo;
  exsr verificar_si_estan_entrando_movs;
  exsr verificar_si_no_hay_microcortes;
  exsr pedir_autorizacion_si_corresponde;

  //----------------------------------------------------------------------------
  begsr verificar_si_la_conn_estar_corriendo;
  //----------------------------------------------------------------------------
    exec sql SELECT 'H2H'||COIBCF INTO :connme FROM LICONN LIMIT 1;
    exec sql SELECT
                    count(*) into :corriendo
            FROM TABLE(QSYS2.ACTIVE_JOB_INFO(
                JOB_NAME_FILTER => ':connme',
                SUBSYSTEM_LIST_FILTER => 'QBATCH')) X ;

  endsr;

  //----------------------------------------------------------------------------
  begsr verificar_si_estan_entrando_movs;
  //----------------------------------------------------------------------------
    exec sql SELECT COUNT(*) INTO :cant_antes FROM LILOGF;
    exec sql CALL QCMDEXC('DLYJOB DLY(6)');
    exec sql SELECT COUNT(*) INTO :cant_despues FROM LILOGF;
  endsr;


  //----------------------------------------------------------------------------
  begsr verificar_si_no_hay_microcortes;
  //----------------------------------------------------------------------------
    exec sql DROP TABLE QTEMP/TOP1000;

    exec sql CREATE TABLE QTEMP/TOP1000  AS (
                    SELECT  * FROM LILOGF LIMIT 1000  ) WITH DATA;

    exec sql SELECT
                count(*)  INTO :totori
            FROM QTEMP/TOP1000
            WHERE LRTIPT = 'O' ;

    exec sql SELECT
                count(*)  INTO :totfor
            FROM QTEMP/TOP1000
            WHERE LRTIPT = 'F' ;

    exec sql SELECT
                count(*)  INTO :totgra
            FROM QTEMP/TOP1000
            WHERE LRTIPT IN ('O' , 'F');
    porori = totori * 100 / totgra;
    porfor = totfor * 100 / totgra;

  endsr;

  //----------------------------------------------------------------------------
  begsr pedir_autorizacion_si_corresponde;
  //----------------------------------------------------------------------------
    if corriendo = 0;
        strerr = 'RTB No está corriendo, verifíque...';
    endif;
    if corriendo = 1  and (cant_antes = cant_despues);
       strerr = 'Al parecer no están entrando mensajes, verifíque...';
    endif;
    if porfor > 40 ;
       strerr = 'Muchos forzados, verifíque...';
    endif;
    if strerr <> *blanks;
        continuar = *off;
        BAER00RS('Atención...':'':'':'':strerr:'':'':
          'F10=Autoriza proceso F3=Cancela');
        exec sql SELECT
                    substr(@ZFNKY, 10, 1) into :f10
                    FROM @cpisys
                    WHERE @ZJOBN = cast( :job_number_char as dec(6, 0));
        if f10=*on;
            continuar = *on;
        endif;
    endif;
  endsr;

end-proc;
