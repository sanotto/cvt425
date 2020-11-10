*SRCMBRTXT:Link-ODE-Procesa reintegro para ODE ven
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
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

//==============================================================================
// Procesos o Funsiones.
//==============================================================================
dcl-proc main;
//Indicadores
    dcl-s vencidos_ok        ind;
// Variables
    dcl-s cmd                char(4096);
    dcl-s rc                 char(7);
    dcl-s nombre_archivo     char(255);
    dcl-s carpeta            char(255);
    dcl-s carpeta_destino    char(255);
    dcl-s full_path          char(255);
    dcl-s path_destino       char(255);
    dcl-s fecha              packed(8:0);
    dcl-s hora               packed(6:0);
    dcl-s usuario            char(10);
    dcl-s FileName           char(10)                 INZ('LIMODE');
    dcl-s FieldName          char(10)                 INZ('U1IJOB');
    dcl-s nro_rrn            packed(15:0)  ;
    dcl-s id_ode             packed(8:0)  ;
    dcl-s codigo_error       char(1)                  INZ('0');
    dcl-s saldo_fijo         packed (15:2);
    dcl-s nro_caja           packed(5:0)              INZ(556);
    dcl-s suc_cta_pozo       packed(5:0);
    dcl-s nro_cta_pozo       packed(9:0);
    dcl-s consulta           char(65535);
// Variables para calcular la fecha vencimiento
    dcl-s dias               packed(15:0) INZ(*zeros);
    dcl-s modo               char(2) INZ(*blanks);
    dcl-s fecha_vencimiento  packed(8:0);
// variables para la seccion ODE
    dcl-s tipo_documento     packed(2:0) ;
    dcl-s nro_documento      packed(15:0);
    dcl-s cuit_cliente       char(15)    ;
    dcl-s nombre_cliente     char(30) ;
// variavles para la seccion CUENTA
    dcl-s tipo_cuenta        char(2)     ;
    dcl-s sucursal_cuenta    packed(5:0) ;
    dcl-s nro_cuenta         packed(9:0) ;
// Variables para la seccion CONTACTOS
    dcl-s mail               char(50)    ;
    dcl-s nro_cel            char(15)    ;
// Variables para seccion DIRECCION
    dcl-s codigo_postal       packed(5:0);
    dcl-s calle               char(30);
    dcl-s barrio              char(30);
    dcl-s nro_puerta          packed(6:0);
//... Entrada de parametros codigos de debito y credito para AC y CC
    dcl-ds codigos qualified;
       ac_deb           packed(3:0);
       ac_cred          packed(3:0);
       cc_deb           packed(3:0);
       cc_cred          packed(3:0);
    end-ds;
//... Parametros dinamicos - BADIPA
    dcl-ds parametros qualified;
       ac_credi         char(3);
       cc_debi          char(3);
       sucursal         char(5);
       cuenta           char(9);
       tar_virt         char(16);
    end-ds;

// Llamada al programa 'SGAUTAC1'
   dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
        path_archivo      char(250) CONST ;
   end-pr;

// Llamada al programa 'BAGETJOB'
   dcl-pr callBAGETJOB extpgm('BAGETJOB');
        file      char(10) CONST;
        field     char(10) CONST;
    end-pr;

// Llamada al programa para obetener el proximo dia habil del mes
   dcl-pr callSBBABFE1 extpgm('SBBABFE1');
        fecha     packed(8:0)CONST;
        dias      packed(15:0)CONST;
        modo      char(2)    CONST;
    end-pr;

// Llamada al programa para debitar/acreditar caja de ahorro (SBACMOVB)
   dcl-pr callSBACMOVB extpgm('SBACMOVB');
        sucursal      packed(5:0) CONST;
        cuenta        packed(11:0)CONST;
        fecha_alta    packed(8:0) CONST;
        hora          packed(6:0) CONST;
        moneda        packed(9:0) CONST;
        codigo        packed(3:0) CONST;
        sucursal_alta packed(5:0) CONST;
        nro_caja      packed(5:0) CONST;
        importe       packed(15:2)CONST;
        fecha_asiento packed(8:0) CONST;
        asiento_contr packed(1:0) CONST;
        fec_asi_contr packed(8:0) CONST;
        usuario1      char(8)     CONST;
        usaurio2      char(10)    CONST;
        nro_cheque    packed(7:0) CONST;
        codigo        packed(3:0) CONST;
    end-pr;

// Llamada al programa para debitar/acreditar cuenta corriente  (SBCCMOVB)
   dcl-pr callSBCCMOVB extpgm('SBCCMOVB');
        sucursal      packed(5:0) CONST;
        cuenta        packed(11:0)CONST;
        fecha_alta    packed(8:0) CONST;
        hora          packed(6:0) CONST;
        moneda        packed(9:0) CONST;
        codigo        packed(3:0) CONST;
        sucursal_alta packed(5:0) CONST;
        nro_caja      packed(5:0) CONST;
        importe       packed(15:2)CONST;
        fecha_asiento packed(8:0) CONST;
        asiento_contr packed(1:0) CONST;
        fec_asi_contr packed(8:0) CONST;
        usuario1      char(8)     CONST;
        usaurio2      char(10)    CONST;
        nro_cheque    packed(7:0) CONST;
        codigo        packed(3:0) CONST;
    end-pr;


//************************DESCRIPCION DEL PROGRAMA****************************//
EXSR inicializo_variables;
EXSR creo_tablas_temporales;
EXSR limpia_tablas_temporales;
EXSR ode_ven_OPEN;
EXSR ode_ven_fetch;
  DOW SQLcode = *zero;
     EXSR calculo_fecha_vencimiento;
     //Pregunto si la fecha vencimiento es menor a fecha actual
     IF fecha_vencimiento < fecha;
        vencidos_ok = *off;
        nro_rrn = id_ode;
        EXSR realiza_reintegro;
        EXSR actualizo_LIMODE;
        EXSR escribo_temporal;
     ENDIF ;
     EXSR ode_ven_fetch;
  ENDDO;
EXSR ode_ven_CLOSE;
EXSR genero_reporte;
EXSR envio_reporte;
EXSR mover_archivo_procesados;
EXSR fin_de_programa;


//******************************************************************************
  //                               SUBRUTINAS
//******************************************************************************

//==============================================================================
  // Asigno valor a variables necesarias
//==============================================================================
    BEGSR inicializo_variables;
     EXEC SQL set option commit=*None;
     EXEC SQL select aasfei into :fecha from sgsysv;
     EXEC SQL select current_user into :usuario from sysibm.sysdummy1;
     //Recupero codigo credito para cuenta del el cliente - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.ac_credi from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/CODCRECLT');
     //Recupero codigo debito para cuenta pozo del banco - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.cc_debi from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/CODDEBPOZ');
     //Recupero sucurasl de cuenta pozo del banco para credito - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.sucursal from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/SUCPOZCRED');
     //Recupero cuenta de pozo del banco para credito - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.cuenta from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/CTAPOZCRED');
     //Recupero directorio origen - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :carpeta from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/DIRORIFOR');
     //Recupero directorio destino - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :carpeta_destino from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/DIRDESFOR');

      //asigo parametros de BADIPA a variales
      codigos.ac_cred   =%DEC(parametros.ac_credi   :3:0);
      codigos.cc_deb    =%DEC(parametros.cc_debi    :3:0);
      suc_cta_pozo      =%DEC(parametros.sucursal   :5:0);
      nro_cta_pozo      =%DEC(parametros.cuenta     :9:0);
      hora   = %dec(%time():*HMS);
      nombre_archivo = 'REPORTE_ODE_VENCIDAS_' + %char(fecha) + '.xls';
      path_destino = %Trim(carpeta_destino) +'/'+ %trim(nombre_archivo);
      full_path = '/home/tmp/' + %trim(nombre_archivo);
      vencidos_ok = *on;

    ENDSR;

//==============================================================================
  // Calculo fecha vencimiento
//==============================================================================
    BEGSR calculo_fecha_vencimiento;

     //Valido feriados, sabado o domigos
     modo = 'IN';
     dias = *zeros;
     callSBBABFE1(fecha_vencimiento:
                  dias:
                  modo);
    ENDSR;

//==============================================================================
  // Genero reporte de las ODE reintegradas al cliente.
//==============================================================================
  BEGSR genero_reporte;
  //Preparo consulta para generar archivo XLSX.
    IF vencidos_ok;
       LEAVESR;
    ENDIF;

    cmd = '';
    consulta = 'SELECT'             +
               ' *'                 +
               ' FROM  ODEVEN    '  ;

    cmd = 'EXPPCDBF SQLSTM('''+%trim(consulta)+''') '       +
          'OUTPAT('''+%trim(full_path) +''')'               ;

    rc =  exeCmd(cmd);

  ENDSR ;

//==============================================================
   //  Modifica registro ODE con estado de reintegro
//==============================================================

BEGSR actualizo_LIMODE;
    exec sql UPDATE LIMODE set TGSSTA = '9'
                        WHERE  TGOIDE = :id_ode;
ENDSR;

//=========================================================================
  // Envio Reporte
//=========================================================================
   BEGSR envio_reporte;

     SELECT ;
     //Si hay vencimientos envio reporte
     WHEN vencidos_ok = *off;
        cmd  = 'SNDMAIL  RECP('''+%trim(usuario)+''') '                   +
               'SUBJECT(''ODE: Reporte de reintegro'') '                  +
               'MESG(''Se realizaron reintegros para el dia de la fecha.' +
                     ' Se adjunta reporte para su control.'') '           +
               'FILE('''+ %trim(full_path) +''')' ;

     //Si no hay vencimientos envio mail notificando
     WHEN vencidos_ok;
        cmd  ='SNDMAIL  RECP('''+%trim(usuario)+''') '                        +
              'SUBJECT(''ODE: Reporte de reintegro'') '                       +
              'MESG(''No se realizaron reintegros para el dia de la fecha.'')';
     ENDSL;
   //Envio mail
   rc = exeCmd(cmd);
   ENDSR;

//=========================================================================
  // Escribo temporal
//=========================================================================
   BEGSR escribo_temporal ;

        exec sql INSERT INTO ODEVEN
                           (FECHA        ,
                            ID_IDE       ,
                            TIP_DOCUMENTO,
                            NRO_DOCUMENTO,
                            NRO_CUIT     ,
                            NOMBRE       ,
                            MAIL         ,
                            CELULAR      ,
                            CODIGO_POSTAL,
                            CALLE        ,
                            BARRIO       ,
                            NRO_PUERTA   ,
                            SUC_CREDITO  ,
                            CTA_CREDITO  ,
                            SUC_DEBITO   ,
                            CTA_DEBITO   ,
                            IMPORTE      ,
                            NRO_CHEQUE   )

                     VALUES(
                           :fecha,
                           :id_ode          ,
                           :tipo_documento  ,
                           :nro_documento   ,
                           :cuit_cliente    ,
                           :nombre_cliente  ,
                           :mail            ,
                           :nro_cel         ,
                           :codigo_postal   ,
                           :calle           ,
                           :barrio          ,
                           :nro_puerta      ,
                           :sucursal_cuenta ,
                           :nro_cuenta      ,
                           :suc_cta_pozo    ,
                           :nro_cta_pozo    ,
                           :saldo_fijo      ,
                           :nro_rrn         );

   IF SQLCODE <> *ZEROS;
   codigo_error = '3';
   EXSR fin_de_programa;
   ENDIF ;

   ENDSR ;

//=========================================================================
  // Realizo debitos y creditos a las cuentas correspondientes
//=========================================================================
   BEGSR realiza_reintegro;

      //Debito a cuenta cliente
      callSBACMOVB(sucursal_cuenta: //Sucural cuenta cliente
                   nro_cuenta:      //Cuenta Cliente 'AC'
                   fecha:           //Fecha alta
                   hora:            //Hora alta
                   1:               //Moneda
                   codigos.ac_cred: //Codigo Credito Cuenta Cliente
                   *zeros:          //Sucursal Alta
                   nro_caja:        //Nro de caja (556)
                   saldo_fijo:      //Importe a debitar
                   fecha:          //Fecha Asiento
                   *zeros:          //Movimiento Contrasentado
                   *zeros:          //Fecha Movimiento Contrasentado
                   *blanks:         //Usuario
                   *blanks:         //Usuario
                   nro_rrn:         //Nro de Cheque
                   codigos.ac_cred);//Codigo

     //Credito a cuenta de fondos del banco
     callSBCCMOVB (suc_cta_pozo:     //Sucursal Cuenta Banco
                   nro_cta_pozo:     //Nro cuenta del Banco 'CC'
                   fecha:            //Fecha Alta
                   hora:             //Hora alta
                   1:                //Moneda
                   codigos.cc_deb:   //Codigo Debito
                   *ZEROS:           //Sucursal Alta
                   nro_caja:         //Nro de caja (556)
                   saldo_fijo:       //Importe a acreditar
                   fecha:            //Fecha Asiento
                   *ZEROS:           //Asiento contrasentado
                   *ZEROS:           //Fecha asiento contrasenadd
                   *BLANKS:          //Usuario
                   *BLANKS:          //Usuario
                   nro_rrn:          //Nro de cheque
                   codigos.cc_deb);  //Codigo
   ENDSR ;

//==============================================================================
  // Limpio las tablas temporales
//==============================================================================
    BEGSR creo_tablas_temporales;
     EXEC SQL DROP TABLE QTEMP/ODEVEN;
     EXEC SQL
            CREATE TABLE QTEMP/ODEVEN (
                FECHA                          DECIMAL(8, 0),
                ID_IDE                         DECIMAL(8, 0),
                TIP_DOCUMENTO                  DECIMAL(2, 0),
                NRO_DOCUMENTO                  DECIMAL(15, 0),
                NRO_CUIT                       CHAR (15),
                NOMBRE                         CHAR (30),
                MAIL                           CHAR(50),
                CELULAR                        CHAR(15),
                CODIGO_POSTAL                  DECIMAL(5, 0),
                CALLE                          CHAR(30),
                BARRIO                         CHAR(30),
                NRO_PUERTA                     DECIMAL(6, 0),
                SUC_CREDITO                    DECIMAL(5, 0),
                CTA_CREDITO                    DECIMAL(9, 0),
                SUC_DEBITO                     DECIMAL(5, 0),
                CTA_DEBITO                     DECIMAL(9, 0),
                IMPORTE                        DECIMAL(15, 2),
                NRO_CHEQUE                     DECIMAL(15, 0));
    ENDSR;

//==============================================================================
  // Declaro los CURSOR es y hago OPEN al CURSOR
//==============================================================================
    BEGSR ode_ven_OPEN;
     //Consulta para traer las ODE Vencidas
      EXEC SQL DECLARE ode_ven CURSOR FOR
      SELECT
            TGOIDE, --ODE-Nro de Sol.= ID. ODE
            TGOTDO, --ODE-Tipo de Documento
            TGONDO, --ODE-Número de documento
            TGOCUI, --ODE-Número de CUIT del clie
            TGONCL, --ODE-Nombre
            TGOIMP, --ODE-Importe
            TGTSUB, --Cuenta-Tipo
            TGTSUC, --Cuenta-Sucursal
            TGTCCL, --Cuenta-Nro de Cta Clte
            TGCOEL, --Correo Electronico
            TGCTEL, --Celular
            TGICPO, --Codigo postal
            TGNCAL, --Calle
            TGDAIB, --Barrio
            TGIPUE, --Nro Puerta
            TGOFVT  --Fecha Vencimiento
      FROM LIMODE
      WHERE
            TGOFVT > 0
        AND TGOFVT < :fecha
        AND TGSSTA NOT IN ('5', '1', '9')
        AND TGTCCL > 0
        AND TGOIMP > 0;

      EXEC SQL OPEN ode_ven;
    ENDSR;

//==============================================================================
  //  ejecuto contulsta y guardo nombre archivo
//==============================================================================
   BEGSR ode_ven_fetch;
    EXEC SQL FETCH ode_ven into :id_ode,
                                :tipo_documento ,
                                :nro_documento  ,
                                :cuit_cliente   ,
                                :nombre_cliente ,
                                :saldo_fijo     ,
                                :tipo_cuenta    ,
                                :sucursal_cuenta,
                                :nro_cuenta     ,
                                :mail,
                                :nro_cel,
                                :codigo_postal,
                                :calle,
                                :barrio,
                                :nro_puerta,
                                :fecha_vencimiento;
   ENDSR;



//==============================================================================
  // Limpio tablas temporales a trabajar
//==============================================================================
    BEGSR limpia_tablas_temporales;
     EXEC SQL DELETE FROM QTEMP/ODEFOR;
    ENDSR;

//==============================================================================
  // Muevo archivo a procesados
//==============================================================================
    BEGSR mover_archivo_procesados;
     //Si no hay vencimientos, salgo
     IF vencidos_ok;
        LEAVESR;
     ENDIF;
     ejecutar_programa_SGAUTAC4(%trim(full_path))      ;

     cmd= 'MOV  OBJ('''  +%Trim(full_path)   +''')     '+
          '     TOOBJ('''+%Trim(path_destino)+''')     ';
     rc=exeCmd(cmd);
    ENDSR;

//==============================================================================
  // Cierro Cursor
//==============================================================================
    BEGSR ode_ven_CLOSE;
        EXEC SQL CLOSE ode_ven;
    ENDSR;

//==============================================================================
  // Fin de programa
//==============================================================================
   BEGSR fin_de_programa;
    //Envio mail por error al recuperar numero rrn.
    IF codigo_error  = '1';
      cmd = 'SNDMAIL  RECP('''+usuario+''') '                          +
            'SUBJECT(''ODE: Error al insertar registro'') '            +
            'MESG(''Se produjo un error al recuperar el nro rrn'       +
            '. VerIFique y reintente'') '                              ;
      exeCmd(cmd);
      RETURN;
    ENDIF;

    //Envio mail por error si no se pudo insertar en temporal
    IF codigo_error  = '3';
      cmd = 'SNDMAIL  RECP('''+usuario+''') '                          +
            'SUBJECT(''ODE: Error al insertar registro en ODEVEN'') '  +
            'MESG(''Se produjo un error al quere hacer insert en '     +
            'el archivo ODEVEN. Verifique y reintente'') '             ;
      exeCmd(cmd);
      RETURN;
    ENDIF;

   RETURN;
   ENDSR;
end-proc;

//******************************************************************************
  //OTROS PROCEDIMIENTOS Y FUNCIONES
//******************************************************************************
//==============================================================================
  // exeCmd: Ejecuta un comando del sistema operativo y retorn el cpf resultante
//==============================================================================
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
//******************************************************************************
