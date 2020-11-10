*SRCMBRTXT:OJ-Import Arch. COELSA- Proc. de Remane
**free
ctl-opt dftname(OJIM12RG)
        option(*SRCSTMT)
        dftactgrp(*NO)
        main(main);


dcl-c MAX_RETRIES 3;
dcl-c MAX_RETRIES2 2;
dcl-c MAX_FILE_SIZE 65535;

//Declaración de prototipos, importadas de los miembros declarados
/include *libl/qrpgsrc,ligdprot ;
/include *libl/qrpgsrc,ligdifs ;

//Procedimiento main, declarado como punto de entrada
//en ctl-opt
dcl-proc main;


    //*entry plist
    //dcl-pi *n;
      //   carpeta          char(255) ;
   // end-pi;

    dcl-s notify_address    char(255)
                            inz('NICOLAS.MARTIN@BANCORIOJA.COM.AR');
    dcl-s count             packed(1:0);
    dcl-s nombre_archivo    char(255);
    dcl-s nombre_arch_full  char(255);
    dcl-s aasfei            packed(8:0);
    dcl-s chsfei            char(8);
    dcl-s rc                char(7);
    dcl-s cmd1              char(4096);
    dcl-s cmd2              char(4096);
    dcl-s cmd3              char(4096);
    dcl-s cmd4              char(4096);
    dcl-s fd                int(10);
    dcl-s cr                int(10);
    dcl-s line              varchar(MAX_FILE_SIZE);
    dcl-s cols              varchar(MAX_FILE_SIZE) dim(27);
    dcl-s col               varchar(MAX_FILE_SIZE);
    dcl-s i                 int(10);
    dcl-s j                 int(10);
    dcl-s ch                char(4096);

    //Campos
    dcl-s  carpeta          char(255) inz('/home/SOJ/Conciliacion/');
    dcl-s nro_oficio_conci  packed(15:0);
    dcl-s codigo_error      char(1);
    dcl-s hora              packed(6:0);
    dcl-s nro_caja          packed(5:0) inz(0);
    dcl-s WWIASC            packed(1:0) inz(1);
    dcl-s WWUSUA            char(10) inz(' ');
    dcl-s hora_debito_AC    packed(6:0);
    dcl-s hora_debito_CC    packed(6:0);
    dcl-s hora_credito      packed(6:0);

// Variables para cuentas de grupo 05
    dcl-s  WWISUC      packed(5:0) INZ (0);
    dcl-s  WWCODI      packed(3:0) INZ (789);
    dcl-s  WWIMON      packed(9:0) inz(1);



//... Entrada de parametros para datos ojmofi
    dcl-ds datos_ojmofi qualified;
       nro_iche         packed(8:0);
       nro_oficio       packed(11:0);
       fecha            packed(8:0);
    end-ds;


//... Entrada de parametros para datos ojmofi
    dcl-ds datos_bawssj qualified;
       nro_cuenta_05    packed(15:0);
       sucursal         packed(5:0);
       sub_cuenta       char(2);
       nro_cuenta       packed(16:0);
       monto            packed(15:2);
       codigo           packed(3:0);
       nro_iche         packed(8:0);
       moneda           packed(9:0);
       hora             packed(6:0);
    end-ds;


 //... Llamada al programa 'SBACMOVB'
   dcl-pr callSRSBAC extpgm('SBACMOVB');
        sucursal          packed(5:0) const;
        nro_cuenta        packed(11:0) const;
        fecha             packed(8:0) const;
        hora              packed(6:0) const;
        moneda            packed(9:0) const;
        codigo            packed(3:0) const;
        sucursal          packed(5:0) const;
        caja              packed(5:0) const;
        monto             packed(15:2) const;
        fecha             packed(8:0) const;
        asc               packed(1:0) const;
        fecha             packed(8:0) const;
        usuario           char(10) const;
        usuario           char(10) const;
        nro_iche          packed(7:0) const;
        codigo            packed(3:0) const;
    end-pr;


//... Llamada al programa 'SRSBCC'
   dcl-pr callSRSBCC extpgm('SBCCMOVB');
        sucursal          packed(5:0) const;
        nro_cuenta        packed(11:0) const;
        fecha             packed(8:0) const;
        hora              packed(6:0) const;
        moneda            packed(9:0) const;
        codigo            packed(3:0) const;
        sucursal          packed(5:0) const;
        caja              packed(5:0) const;
        monto             packed(15:2) const;
        fecha             packed(8:0) const;
        asc               packed(1:0) const;
        fecha             packed(8:0) const;
        usuario           char(10) const;
        usuario           char(10) const;
        nro_iche          packed(7:0) const;
        codigo            packed(3:0) const;
    end-pr;



    exec sql set option commit=*None;
    exec sql select aasfei into :aasfei from sgsysv;
    exsr declara_cursor;
    exsr Crear_nombre_archivo;
    exsr Esperar_archivo;
    exsr Procesar_conciliacion;
    exsr Avisar_fin_proceso;
    exsr Fin_de_programa;

//------------------------------------------------------------------------------

//=========================================================================
   // Subrutina que crea el nombre del archivo.
//=========================================================================

    begsr Crear_nombre_archivo;
        exec sql select aasfei into :aasfei from sgsysv;
        chsfei = %editw(aasfei:'        ');
        nombre_archivo =    %subst(chsfei :7 :2)    +
                            %subst(chsfei :5 :2)    +
                            %subst(chsfei :3 :2)    +
                            '_SOJ_CONCILIACION.309';
        nombre_arch_full = %trim(carpeta)           +
                           %trim(nombre_archivo)   ;
    endsr;




//=========================================================================
   //  Subrutina que espera a que el archivo este disponible
//=========================================================================

    begsr Esperar_archivo;
        cmd1 = 'DSPLNK OBJ('''                  +
                    %trim(nombre_arch_full)     +
                        ''') OUTPUT(*PRINT)'    ;

        cmd2 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '             +
               ' SUBJECT(''SOJ Archivo Conciliacion no encontrado'') '  +
               ' BODY(''Se esperaba encontrar el archivo:'              +
                             %trim(nombre_archivo)                      +
               ' en la carpeta:'                                        +
                        %trim(carpeta)                                  +
               ' '') '                                                  ;

      dou count >= MAX_RETRIES;
            rc = exeCmd(cmd1);
            if rc = 'CPF0000';
                leavesr;
            endif;

//...Enviar mail

            exeCmd(cmd2);
            exeCmd('DLYJOB DLY(5)');
            count +=1;

      enddo;

        cmd2 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '             +
               ' SUBJECT(''SOJ Archivo Conciliacion CANCELACION'') '    +
               ' BODY(''Se esperaba encontrar el archivo:'              +
                             %trim(nombre_archivo)                      +
               ' en la carpeta:'                                        +
                        %trim(carpeta)                                  +
               ' . Se CANCELA despues de agotar los reintentos'') '     ;

        exeCmd(cmd2);


//...Imprimo mensaje de error en pantalla de sideba
        die('No encontre el archivo:'           +
                        %trim(carpeta)          +
                        %trim(nombre_archivo)   +
                    ' para procesar           ');
    endsr;


//=========================================================================
   //  Procesa el archivo (Embargo)
//=========================================================================
 begsr Procesar_conciliacion;

    hora   = %dec(%time():*HMS);
    EXSR leer_Conciliacion;

//Aca obtengo los datos del ojmofi para comparar
    exec sql open ojm_conciliacion;
    exec sql fetch ojm_conciliacion into :datos_ojmofi;
    DoW sqlcod = *Zero;
    codigo_error = '2';


//For para recorrer el array cargado del archivo de conciliacion

for i = 1 to %size(%trim(cols));
     nro_oficio_conci = 0;

     if cols(i) = ' ';

        leave;

     endif;

     nro_oficio_conci = %dec(%subst(%trim(cols(i)) :13 :15):15:0);

    if nro_oficio_conci = datos_ojmofi.nro_oficio;
         codigo_error = '0';
        leave;
    endif;
endfor;


//Pregunto si el nro oficio no fui informado en archivo conciliacion
if codigo_error = '2';

//Select para extraer los datos en bawssj
  exec sql open bawssj_conciliacion;
  exec sql FETCH bawssj_conciliacion into :datos_bawssj;
   DoW SQLCOD = *ZERO;

  if codigo_error = '2';
//... Sub Rutina para realizar la reversa
    exsr realizarReversa;

  endif;
   exec sql fetch bawssj_conciliacion into :datos_bawssj;
 enddo;
 exec sql close bawssj_conciliacion;

endif;

    exec sql fetch ojm_conciliacion into :datos_ojmofi;
EndDo;

    exec sql close ojm_conciliacion;


 endsr;


//=========================================================================
   // Abre el archivo
//=========================================================================
    begsr abrir_archivo;
        fd = ifs_stmf_opnread(%trim(nombre_arch_full));
        if (fd < 0);
           die('No se pudo abrir archivo:'+nombre_arch_full);
        endif;
    endsr;


//=========================================================================
   //  Cierra el archivo
//=========================================================================

    begsr cerrar_archivo;
        ifs_stmf_close(fd);
    endsr;



//==============================================================================
   //  Avisar si ocurrurio un error en el intenro de reversar una cuenta
//==============================================================================

    begsr Avisar_error_proceso;

  if codigo_error = '3';

    cmd4 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '                      +
               ' SUBJECT(''Aviso de error de proceso SOJ'') '                +
       ' BODY(''Ocurrio un error al querer recuperar los datos del archivo ' +
        'BAWSSJ.'                                                            +
               ' '') '                                                       ;


  endif;

  if codigo_error = '4';
   cmd4 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '                       +
               ' SUBJECT(''Aviso de error de proceso SOJ'') '                +
       ' BODY(''Ocurrio un error al querer realizar el contrasiento de la'   +
       ' cuenta'                                                             +
               ' '') '                                                       ;


   endif;

    if codigo_error = '5';


     cmd4 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '                     +
               ' SUBJECT(''Aviso de error de proceso SOJ'') '                +
       ' BODY(''Ocurrio un error al querer realizar el contrasiento de la'   +
       '  cuenta grupo 05 subgrupo OJ: '                            +
               ' '') '                                                       ;




    endif;

  exeCmd(cmd4);


 endsr;


//=================================================
   // Subrutina para leer el archivo conciliacion.
//=================================================

    begsr leer_Conciliacion;

     EXSR abrir_archivo;
     j = 1;
     clear cols;

//...Lee la cabecera
              ifs_readln_dos(fd: line);

//... Proceso las lineas del archivo conciliacion
            dow  ifs_readln_dos(fd: line) > 0;
              if  (%subst(%trim(line) :1 :3) <> 'FIN');
              col = %trim(line);
              cols(j) = col;
              j += 1;
              col = ' ';
             endif;
            enddo;



     EXSR cerrar_archivo;

    endsr;



//=================================================
   // Subrutina que realiza la reversa
//=================================================

    begsr realizarReversa;

//realizo reversa a la cuenta debitada
      if datos_bawssj.sub_cuenta = 'AC';
         hora_debito_AC = 0;
         EXSR recuperoHoraAcDebito;

         if codigo_error = '2';
         EXSR llamarSRSBAC;

         endif;

      else;
         hora_debito_CC = 0;
         EXSR recuperoHoraCcDebito;
         if codigo_error = '2';
         EXSR llamarSRSBCC;
         endif;

      endif;

//reaizo la reversa de la cuenta de grupo 05 sub grupo OJ
      hora_credito = 0;
      EXSR recuperoHoraCredito;

      if codigo_error = '2';
      EXSR  llamarSRSBAC05;

      EXSR borrarojmofi;
      EXSR borrarojmres;
      endif;


      if codigo_error <> '2' and
         codigo_error <> '0';

      EXSR Avisar_error_proceso;

      endif;

    endsr;



//=================================================
   //  Envia los parametros para el programa SRSBAC
//=================================================-
  BEGSR llamarSRSBAC05 ;

     callSRSBAC( WWISUC:
                 datos_bawssj.nro_cuenta_05:
                 aasfei:
                 hora_credito:
                 WWIMON:
                 WWCODI:
                 WWISUC:
                 nro_caja:
                 datos_bawssj.monto:
                 aasfei:
                 WWIASC:
                 aasfei:
                 WWUSUA:
                 WWUSUA:
                 datos_bawssj.nro_iche:
                 WWCODI);

  ENDSR;


//=================================================
   //  Envia los parametros para el programa SRSBAC
//=================================================-
  BEGSR llamarSRSBAC ;

     callSRSBAC( datos_bawssj.sucursal:
                 datos_bawssj.nro_cuenta:
                 aasfei:
                 hora_debito_AC:
                 datos_bawssj.moneda:
                 datos_bawssj.codigo:
                 datos_bawssj.sucursal:
                 nro_caja:
                 datos_bawssj.monto:
                 aasfei:
                 WWIASC:
                 aasfei:
                 WWUSUA:
                 WWUSUA:
                 datos_bawssj.nro_iche:
                 datos_bawssj.codigo);

  ENDSR;


//=================================================
   //  Envia los parametros para el programa SRSBCC
//=================================================-
  BEGSR llamarSRSBCC ;

     callSRSBCC( datos_bawssj.sucursal:
                 datos_bawssj.nro_cuenta:
                 aasfei:
                 hora_debito_CC:
                 datos_bawssj.moneda:
                 datos_bawssj.codigo:
                 datos_bawssj.sucursal:
                 nro_caja:
                 datos_bawssj.monto:
                 aasfei:
                 WWIASC:
                 aasfei:
                 WWUSUA:
                 WWUSUA:
                 datos_bawssj.nro_iche:
                 datos_bawssj.codigo);

  ENDSR;


//=================================================
   // Subrutina que termina el programa
//=================================================

    begsr Fin_de_programa;
        return;
    endsr;


//======================================================
   //  Avisar a los responsables que el proceso termino
//======================================================

    begsr Avisar_fin_proceso;

    cmd3 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '                      +
               ' SUBJECT(''Aviso de fin de proceso Conciliacion'') '         +
       ' BODY(''Se realizo el procesamiento del archivo Conciliacion de SOJ' +
               ' '') '                                                       ;

    exeCmd(cmd3);


    endsr;





//=========================================================
   //  Extrae la hora debito AC
//=========================================================

    BEGSR  recuperoHoraAcDebito;

     exec sql open acmovd_hora_debito;
     exec sql FETCH acmovd_hora_debito into :hora_debito_AC;

     If SQLCOD <> *ZERO;
      codigo_error = '4';
     endif;

     exec sql close acmovd_hora_debito;

    ENDSR;


//=========================================================
   //  Extrae  la hora debito CC
//=========================================================

    BEGSR  recuperoHoraCcDebito;

     exec sql open ccmoct_hora_debito;
     exec sql FETCH ccmoct_hora_debito into :hora_debito_CC;

     If SQLCOD <> *ZERO;
      codigo_error = '4';
     endif;

     exec sql close ccmoct_hora_debito;

    ENDSR;


 //=========================================================
   //  Extrae hora credito
//=========================================================

    BEGSR  recuperoHoraCredito;

     exec sql open acmovd_hora_credito;
     exec sql FETCH acmovd_hora_credito into :hora_credito;

     If SQLCOD <> *ZERO;
      codigo_error = '5';
     endif;

     exec sql close acmovd_hora_credito;

    ENDSR;



BEGSR borrarOjmofi;

//...borrar ojmofi

 exec sql  DELETE
             FROM   OJMOFI
          WHERE
                 OJIRRN = :datos_ojmofi.nro_iche
           AND   OJCOTR = 'AF'
           AND   OJINUI = :datos_ojmofi.nro_oficio;


ENDSR;


BEGSR borrarOjmres;

//...borrar ojmofi

 exec sql  DELETE
             FROM   OJMRES
             WHERE
              MRINUI = :datos_ojmofi.nro_oficio ;


ENDSR;


//=================================================
   //  Declaro cursores para las consultas.
//=================================================

  BEGSR declara_cursor;


//... hora del CREDITO 05

 exec sql declare acmovd_hora_credito cursor for
     SELECT GCHALT as hora_debito_AC
    FROM   acmovd
    WHERE
              GCISUC = :WWISUC
        AND   GCICAH = :datos_bawssj.nro_cuenta_05
        AND   GCFING = :aasfei
        AND   GC$IMP = :datos_bawssj.monto
        AND   GCIMCA = 789
        AND   GCICHE = :datos_bawssj.nro_iche;


//... hora del debito AC

 exec sql declare acmovd_hora_debito cursor for
     SELECT GCHALT as hora_debito_AC
    FROM   acmovd
    WHERE
              GCISUC = :datos_bawssj.sucursal
        AND   GCICAH = :datos_bawssj.nro_cuenta
        AND   GCFING = :aasfei
        AND   GC$IMP = :datos_bawssj.monto
        AND   GCIMCA = 788
        AND     GCICHE = :datos_bawssj.nro_iche;


//... hora del debito CC

 exec sql declare ccmoct_hora_debito cursor for
     SELECT CFHALT as hora_debito_CC
    FROM   ccmoct
    WHERE
              CFISUC = :datos_bawssj.sucursal
        AND   CFICCC = :datos_bawssj.nro_cuenta
        AND   CFFING = :aasfei
        AND   CF$IMP = :datos_bawssj.monto
        AND   CFIMCC = 788
        AND   CFICHE = :datos_bawssj.nro_iche;


//... Select al ojmofi para saber si esta en el archivo conciliacion

 exec sql declare ojm_conciliacion cursor for
     SELECT OJIRRN as nro_iche,
            OJINUI as nro_oficio,
            OJFALT as fecha
    FROM   OJMOFI
    WHERE
           OJFALT = :aasfei
       AND OJITRG = 1
       AND OJIAFO = 1;


//... Select al bawssj para realizar la reversa

 exec sql declare bawssj_conciliacion cursor for
     SELECT U1INDO as nro_cuenta_05,
            U1ISUC as sucu,
            U1ISUB as sub_cuenta,
            U1ITAR as nro_cuenta,
            U1$SOP as monto,
            U1QCUO as codigo,
            U1FVGD as nro_iche,
            U1IMON as moneda,
            U1IUTI as hora

    FROM   BAWSSJ
    WHERE
           U1FALT = :aasfei
       AND U1FVGD = :datos_ojmofi.nro_iche;


 endsr;

end-proc;
/copy desa00525/qrpgsrc,ligdimpl ;
