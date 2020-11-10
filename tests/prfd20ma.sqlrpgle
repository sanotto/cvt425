*SRCMBRTXT:Consulta Web Service Federal           
**free
ctl-opt dftname(PRFD20MA)
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

dcl-ds fed_params DTAARA(*AUTO : *USRCTL : 'QGPL/FEDPARAMS') qualified;
    url              char(100);
      //localhost http://39.0.71.47:8084/federal/consulta
      //homo      http://181.15.143.226:9091/tfr/servlet/awslote
      //prod      http://181.15.143.225:9091/tfr/servlet/awslote
    timeout          char(100);
end-ds;

dcl-pr exeCmd char(7);
    cmd_str   char(4096) const;
end-pr;

//**************************************************
// Procedimiento Main: PRINCIPAL
//**************************************************
dcl-proc Main;
    //ENTRY PLIST
    dcl-pi *n;
        id_prestamo     packed(15:0);
        aasfei          packed(8:0);
        error           char(1);
        mensaje         char(216);
    end-pi;
    //Declaro Variables
    dcl-s parm_alta char(221)    ccsid (1208) inz(*blanks);
    dcl-s parm_baja char(221)    ccsid (1208) inz(*blanks);
    dcl-s parm_request char(4096) ccsid (1208) inz(*blanks);
    dcl-s HEADER        char(250) INZ(*BLANKS);
   //connectionTimeout
    dcl-s ds_baja       char(221);
    dcl-s ds_alta       char(221);
    dcl-s msg1          char(54);
    dcl-s msg2          char(54);
    dcl-s msg3          char(54);
    dcl-s msg4          char(54);
    dcl-s rc            char(7);
    dcl-s metodo        char(6);
    dcl-s dependiente   packed(1: 0);
    dcl-s dsply_msj     char(52);

    dcl-ds c1_ds qualified;
        reg             char(4096) ccsid(65535);
    end-ds;

    dcl-ds ds_clave_baja qualified;
        sucursal             packed(5:0);
        prestamo             packed(15:0);
        desglose             packed(4:0);
    end-ds;
    dcl-ds ds_response_baja qualified;
        secuencia                   char(6) pos(1);
        nbr                         char(3) pos(7);
        tipo_registro               char(1) pos(10);
        codigo_descripcion          char(3) pos(11);
        sexo                        char(1) pos(14);
        nro_documento               char(8) pos(15);
        apellido_nombre             char(30) pos(23);
        relacion_trabajo            char(2) pos(53);
        area_gobierno_recibo        char(5) pos(55);
        codigo_operacion            char(14) pos(60);
        codigo_operacion_renovada   char(14) pos(74);
        fecha                       char(10) pos(88);
        importe_operacion           char(9) pos(98);
        cantidad_cuotas             char(3) pos(107);
        importe_cuota               char(9) pos(110);
        nro_proceso_federal         char(6) pos(119);
        nro_autorizacion_federal    char(9) pos(125);
        fecha_autorizacion          char(16) pos(134);
        codigo_aprobado             char(3) pos(150);
        fecha_rellenar              char(10) pos(153);
        importe_rellenar            char(9) pos(163);
        filler                      char(50) pos(172);
    end-ds;
    dcl-ds ds_response_alta qualified;
        secuencia                   char(6) pos(1);
        nbr                         char(3) pos(7);
        tipo_registro               char(1) pos(10);
        codigo_descripcion          char(3) pos(11);
        sexo                        char(1) pos(14);
        nro_documento               char(8) pos(15);
        apellido_nombre             char(30) pos(23);
        relacion_trabajo            char(2) pos(53);
        area_gobierno_recibo        char(5) pos(55);
        codigo_operacion            char(14) pos(60);
        codigo_operacion_renovada   char(14) pos(74);
        fecha                       char(10) pos(88);
        importe_operacion           char(9) pos(98);
        cantidad_cuotas             char(3) pos(107);
        importe_cuota               char(9) pos(110);
        nro_proceso_federal         char(6) pos(119);
        nro_autorizacion_federal    char(9) pos(125);
        fecha_autorizacion          char(16) pos(134);
        codigo_aprobado             char(3) pos(150);
        fecha_rellenar              char(10) pos(153);
        importe_rellenar            char(9) pos(163);
        filler                      char(50) pos(172);
    end-ds;
    dcl-pr CallPRFD04MA extpgm('PRFD04MA');
        metodo        char(6);
        id_prestamo   packed(15:0);
    end-pr;
    error = '1';
    HEADER = '<httpHeader '+
                'connectionTimeout="'+ %trim(fed_params.timeout)+ '" '+
                'readTimeout="'+ %trim(fed_params.timeout) +'">'+
                '<header name="content-type" value="text/plain" />'+
             '</httpHeader>';

//**************************************************
// Ciclo de programa
//**************************************************
UNLOCK *DTAARA;
EXSR InicializacionSR;
EXSR ProgramaPrincipalSR;
EXSR FinProgramaSR;


//**************************************************
//                  SUBRUTINAS
//**************************************************
// InicializacionSR: Inicializa programa
//**************************************************
BEGSR InicializacionSR;

ENDSR;
//**************************************************
// ProgramaPrincipalSR
//**************************************************
BEGSR ProgramaPrincipalSR;

    exec sql set option commit=*None;

    //* Genero registro ALTA
    exec sql
SELECT
      LPAD(AFISEQ, 6, '0')||
      'NBR'||
      LPAD(AFIOPT, 1, ' ')||
      LPAD(AFIMDS, 3, '0')||
      LPAD(AFISEX, 1, ' ')||
      LPAD(AFINDO, 8, '0')||
      AFNYAP||
      CASE WHEN AFIRED LIKE '%A%'
           THEN '10'
           WHEN AFIRED LIKE '%B%'
           THEN '11'
           WHEN AFIRED LIKE '%C%'
           THEN '12'
           WHEN AFIRED LIKE '%D%'
           THEN '13'
           WHEN AFIRED LIKE '%E%'
           THEN '14'
           WHEN AFIRED LIKE '%F%'
           THEN '15'
           WHEN AFIRED LIKE '%G%'
           THEN '16'
           WHEN AFIRED LIKE '%H%'
           THEN '17'
           WHEN AFIRED LIKE '%I%'
           THEN '18'
           WHEN AFIRED LIKE '%J%'
           THEN '19'
           WHEN AFIRED LIKE '%K%'
           THEN '20'
           WHEN AFIRED LIKE '%L%'
           THEN '21'
           WHEN AFIRED LIKE '%M%'
           THEN '22'
           WHEN AFIRED LIKE '%N%'
           THEN '23'
           WHEN AFIRED LIKE '%O%'
           THEN '24'
           WHEN AFIRED LIKE '%P%'
           THEN '25'
           WHEN AFIRED LIKE '%Q%'
           THEN '26'
           WHEN AFIRED LIKE '%R%'
           THEN '27'
           WHEN AFIRED LIKE '%S%'
           THEN '28'
           WHEN AFIRED LIKE '%T%'
           THEN '29'
           WHEN AFIRED LIKE '%U%'
           THEN '30'
           WHEN AFIRED LIKE '%V%'
           THEN '31'
           WHEN AFIRED LIKE '%W%'
           THEN '32'
           WHEN AFIRED LIKE '%X%'
           THEN '33'
           WHEN AFIRED LIKE '%Y%'
           THEN '34'
           WHEN AFIRED LIKE '%Z%'
           THEN '35'
           WHEN AFIRED LIKE '%0%'
           THEN '01'
           ELSE LPAD(AFIRED, 2, '0')
      END ||
      LPAD(SUBSTR(AFIBCF, 1, 5), 5, ' ')||
      LPAD(AFISUC, 2, '0')||LPAD(AFINCR, 10, '0')||LPAD(AFIDEG, 2, '0')||
      CASE WHEN AFINCV<>0
           THEN LPAD(AFEDSU, 2, '0') ||
                LPAD(AFINCV, 10, '0')||
                LPAD(AFIDEV, 2, '0')
           ELSE '              '
      END||
      CASE WHEN AFFECH>0
           THEN
           SUBSTR(AFFECH, 7, 2)||'/'||SUBSTR(AFFECH, 5, 2)||'/'
             ||SUBSTR(AFFECH, 1, 4)
           ELSE '          '
      END||
      LPAD(replace(AF$IMP ,',','.'), 9, '0')||
      LPAD(AFQCUO, 3, '0')||
      LPAD(replace(AF$CUO,',','.'), 9, '0')||
      RPAD(TRIM(SUBSTR(AFIPRC, 5, 6)), 6, '0')||
      LPAD(AFINCE, 9, '0')||
      CASE WHEN AFFAU1<>''
           THEN SUBSTR(AFFAU1, 7, 2)||'/'||SUBSTR(AFFAU1, 5, 2)||'/'
                ||SUBSTR(AFFAU1, 1, 4)
           ELSE '  /  /    '
      END||
      ' '||
      CASE WHEN AFHAU1<>''
           THEN SUBSTR(AFHAU1, 1, 2)||':'||SUBSTR(AFHAU1, 3, 2)
           ELSE '00:00'
      END||
      '     /  /    000000.00                                                  '
INTO :parm_alta
FROM PRAFED
WHERE    AFIRRN= :id_prestamo
     AND AFFECH= :aasfei
     AND AFIOPT='A'
LIMIT 1;

    if sqlcode <> *zero;
        dsply_msj = 'Err al generar param alta. sql: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        EXSR FinProgramaSR;
    endif;

    exec sql
            SELECT
                  AFISUC,AFINCR,AFIDEG
            INTO :ds_clave_baja
            FROM PRAFED
            WHERE    AFIRRN= :id_prestamo
                 AND AFFECH= :aasfei
                 AND AFIOPT='A'
            LIMIT 1;

    //* Genero registro BAJA (si la hay)
    exec sql
SELECT
      LPAD(AFISEQ, 6, '0')||
      'NBR'||
      LPAD(AFIOPT, 1, ' ')||
      LPAD(AFIMDS, 3, '0')||
      LPAD(AFISEX, 1, ' ')||
      LPAD(AFINDO, 8, '0')||
      AFNYAP||
      CASE WHEN AFIRED LIKE '%A%'
           THEN '10'
           WHEN AFIRED LIKE '%B%'
           THEN '11'
           WHEN AFIRED LIKE '%C%'
           THEN '12'
           WHEN AFIRED LIKE '%D%'
           THEN '13'
           WHEN AFIRED LIKE '%E%'
           THEN '14'
           WHEN AFIRED LIKE '%F%'
           THEN '15'
           WHEN AFIRED LIKE '%G%'
           THEN '16'
           WHEN AFIRED LIKE '%H%'
           THEN '17'
           WHEN AFIRED LIKE '%I%'
           THEN '18'
           WHEN AFIRED LIKE '%J%'
           THEN '19'
           WHEN AFIRED LIKE '%K%'
           THEN '20'
           WHEN AFIRED LIKE '%L%'
           THEN '21'
           WHEN AFIRED LIKE '%M%'
           THEN '22'
           WHEN AFIRED LIKE '%N%'
           THEN '23'
           WHEN AFIRED LIKE '%O%'
           THEN '24'
           WHEN AFIRED LIKE '%P%'
           THEN '25'
           WHEN AFIRED LIKE '%Q%'
           THEN '26'
           WHEN AFIRED LIKE '%R%'
           THEN '27'
           WHEN AFIRED LIKE '%S%'
           THEN '28'
           WHEN AFIRED LIKE '%T%'
           THEN '29'
           WHEN AFIRED LIKE '%U%'
           THEN '30'
           WHEN AFIRED LIKE '%V%'
           THEN '31'
           WHEN AFIRED LIKE '%W%'
           THEN '32'
           WHEN AFIRED LIKE '%X%'
           THEN '33'
           WHEN AFIRED LIKE '%Y%'
           THEN '34'
           WHEN AFIRED LIKE '%Z%'
           THEN '35'
           WHEN AFIRED LIKE '%0%'
           THEN '01'
           ELSE LPAD(AFIRED, 2, '0')
      END ||
      LPAD(SUBSTR(AFIBCF, 1, 5), 5, ' ')||
      LPAD(AFISUC, 2, '0')||LPAD(AFINCR, 10, '0')||LPAD(AFIDEG, 2, '0')||
      CASE WHEN AFINCV<>0
           THEN LPAD(AFEDSU, 2, '0') ||
                LPAD(AFINCV, 10, '0')||
                LPAD(AFIDEV, 2, '0')
           ELSE '00000000000000'
      END||
      CASE WHEN AFFECH>0
           THEN
           SUBSTR(AFFECH, 7, 2)||'/'||SUBSTR(AFFECH, 5, 2)||'/'
             ||SUBSTR(AFFECH, 1, 4)
           ELSE '          '
      END||
      LPAD(replace(AF$IMP ,',','.'), 9, '0')||
      LPAD(AFQCUO, 3, '0')||
      LPAD(replace(AF$CUO,',','.'), 9, '0')||
      RPAD(TRIM(SUBSTR(AFIPRC, 5, 6)), 6, '0')||
      LPAD(AFINCE, 9, '0')||
      CASE WHEN AFFAU1<>''
           THEN SUBSTR(AFFAU1, 7, 2)||'/'||SUBSTR(AFFAU1, 5, 2)||'/'
                ||SUBSTR(AFFAU1, 1, 4)
           ELSE '  /  /    '
      END||
      ' '||
      CASE WHEN AFHAU1<>''
           THEN SUBSTR(AFHAU1, 1, 2)||':'||SUBSTR(AFHAU1, 3, 2)
           ELSE '00:00'
      END||
      '     /  /    000000.00                                                  '
INTO :parm_baja
FROM PRAFED
WHERE    AFFECH= :aasfei
     AND AFIOPT='B'
     AND AFEDSU= :ds_clave_baja.sucursal
     AND AFINCV= :ds_clave_baja.prestamo
     AND AFIDEV= :ds_clave_baja.desglose
LIMIT 1;

    if sqlcode <> *zero and sqlcode <> 100;
        dsply_msj = 'Err al generar param baja. sql: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        EXSR FinProgramaSR;
    endif;

   //* ARMO EL PARM_REQUEST
   if parm_baja <> '';
        parm_request = parm_baja + X'0A' + parm_alta;
   else;
        parm_request =  parm_alta;
   endif;

    rc = exeCmd( 'CHGJOB CCSID(37)');
   // rc = exeCmd( 'CHGJOB CCSID(65535)');

    exec sql DROP TABLE QTEMP/REGISTRO ;
    exec sql CREATE TABLE QTEMP/REGISTRO
             (
              dependiente    decimal(1, 0) ,
              baja           varchar (4096) CCSID 1208,
              alta           varchar (4096) CCSID 1208);
    exec sql
      INSERT INTO QTEMP/REGISTRO
      SELECT
      CASE WHEN SUBSTR(responseMsg, 10, 1)='B'
           THEN 1
           ELSE 0
      END,
      CASE WHEN SUBSTR(responseMsg, 10, 1)='B'
           THEN SUBSTR(responseMsg, 1, 221)
           ELSE LPAD('', 221, ' ')
      END,
      CASE WHEN SUBSTR(responseMsg, 10, 1)='B'
                THEN SUBSTR(responseMsg, 224, 221)
           WHEN SUBSTR(responseMsg, 10, 1)='A'
                THEN SUBSTR(responseMsg, 1, 221)
           ELSE LPAD('', 221, ' ')
      END
             FROM
             table(
             SYSTOOLS.HTTPCLOBVERBOSE(
             CAST ( :fed_params.url AS VARCHAR(2048)),
             'POST',
             CAST( :HEADER AS CLOB(10K) CCSID 1208),
             CAST(:parm_request as CLOB(2G) CCSID 1208)
             )) as table;

    if sqlcode <> *zero;
        dsply_msj = 'Err al invocar web_service. sql: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        EXSR FinProgramaSR;
    endif;

    exec sql SELECT dependiente INTO :dependiente FROM QTEMP.REGISTRO;
    if sqlcode <> *zero;
        dsply_msj = 'Err al insert ds_dependiente: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        EXSR FinProgramaSR;
    endif;
    exec sql SELECT baja INTO :ds_baja FROM QTEMP.REGISTRO;
    ds_response_baja = ds_baja;
    if sqlcode <> *zero;
        dsply_msj = 'Err al insert ds_baja: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        EXSR FinProgramaSR;
    endif;
    exec sql SELECT alta INTO :ds_alta FROM QTEMP.REGISTRO;
    ds_response_alta = ds_alta;
    if sqlcode <> *zero;
        dsply_msj = 'Err al insert ds_alta: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        EXSR FinProgramaSR;
    endif;


    if dependiente = 1 ;
        EXSR ProcesaBajaSR;
    endif;
    EXSR ProcesaAltaSR;

    // LLamada a PRFD04MA
    metodo = 'ONLINE';
    CallPRFD04MA( metodo : id_prestamo);

    exec sql
    UPDATE BANUME SET WNIULN=
           (SELECT CASE WHEN (WNIULN - 1)<0
                   THEN 0
                   ELSE WNIULN - (1 + :dependiente)
                   END
            FROM BANUME
            WHERE WNIPF1='PRFD00RG  ' AND
                  WNIPF2='AFISEQ    ' AND
                  WNIPF3= :AASFEI
            LIMIT 1)
    WHERE WNIPF1='PRFD00RG  ' AND
          WNIPF2='AFISEQ    ' AND
          WNIPF3= :AASFEI ;

    //Armo mensaje de respuesta
    if (ds_response_alta.codigo_aprobado='APR');
         msg1 = 'El prestamo fue otorgado. Autorización nro: '+
                 %trim(%char(ds_response_alta.nro_autorizacion_federal));
         msg2 = 'Estado: ' + %trim(%char(ds_response_alta.filler));
         msg3 = 'Fecha y hora: ' +
                 %trim(%char(ds_response_alta.fecha_autorizacion));
         mensaje=msg1+msg2+msg3;
    else;
         msg1  = 'Prestamo rechazado.                                   ';
         msg2 = 'Estado: ' + ds_response_alta.filler;
         msg3 = 'Fecha y hora: ' +
                 %trim(%char(ds_response_alta.fecha_autorizacion));
         mensaje=msg1+msg2+msg3;
         exec sql
         UPDATE @CPIUSD SET @CITIP=0 WHERE @ZJOBN= :pgm_stat.job_number_char;
    endif;
    error = '0';
ENDSR;
//**************************************************
// FinProgramaSR: Termina el programa
//**************************************************
BEGSR FinProgramaSR;
    return;
ENDSR;
//**************************************************
// ProcesaBajaSR: Procesa una baja de prestamo
//**************************************************
BEGSR ProcesaBajaSR;
      if sqlcode <> *zero
         and (ds_response_baja.codigo_aprobado <> 'APR' or
              ds_response_baja.codigo_aprobado <> 'RCH');
           dsply_msj = 'Error al proc baja: ' + %trim(%char(sqlcode));
           dsply (dsply_msj);
           msg4 = 'filler: ' + ds_response_baja.filler
                 +'. sql:'+%trim(%char(sqlcode));
           EXSR FinProgramaSR;
      endif;
    exec sql
    UPDATE PRAFED SET
    AFFECH = SUBSTR(:ds_response_baja.fecha, 7, 4)||
             SUBSTR(:ds_response_baja.fecha, 4, 2)||
             SUBSTR(:ds_response_baja.fecha, 1, 2),
    AFISEQ = '0',
    AFFACR = (SELECT AASFEI
            FROM SGSYSV LIMIT 1)   ,
    AFHSUP = (SELECT CAST(CONCAT(CONCAT(SUBSTR(CAST(NOW() AS CHAR(50)), 12, 2),
             SUBSTR(CAST(NOW() AS CHAR(50)), 15, 2)),
             SUBSTR(CAST(NOW() AS CHAR(50)), 18, 2)) AS DECIMAL(6, 0))
             FROM SYSIBM.SYSDUMMY1),
    AFIUSA = (SELECT CURRENT_USER
             FROM SYSIBM.SYSDUMMY1),
    AFFAU1 = SUBSTR(:ds_response_baja.fecha_autorizacion, 7, 4)||
             SUBSTR(:ds_response_baja.fecha_autorizacion, 4, 2)||
             SUBSTR(:ds_response_baja.fecha_autorizacion, 1, 2),
    AFHAU1 = SUBSTR(:ds_response_baja.fecha_autorizacion, 12, 2)||
             SUBSTR(:ds_response_baja.fecha_autorizacion, 15, 2)||
             '00',
    AFFE01 = CAST(
             CASE WHEN   LENGTH(RTRIM(TRANSLATE(SUBSTR
         (:ds_response_baja.fecha_autorizacion, 7, 4), '*', ' 0123456789'))) = 0
                     AND LENGTH(RTRIM(TRANSLATE(SUBSTR
         (:ds_response_baja.fecha_autorizacion, 4, 2), '*', ' 0123456789'))) = 0
                     AND LENGTH(RTRIM(TRANSLATE(SUBSTR
         (:ds_response_baja.fecha_autorizacion, 1, 2), '*', ' 0123456789'))) = 0
             THEN  (
                    SUBSTR(:ds_response_baja.fecha_autorizacion, 7, 4)||
                    SUBSTR(:ds_response_baja.fecha_autorizacion, 4, 2)||
                    SUBSTR(:ds_response_baja.fecha_autorizacion, 1, 2)
                   )
             ELSE 0
             END
             AS DECIMAL(8, 0)),
    AFFTRA = (SELECT AASFEI
            FROM SGSYSV LIMIT 1)   ,
    AFHTRA = (SELECT CAST(CONCAT(CONCAT(SUBSTR(CAST(NOW() AS CHAR(50)), 12, 2),
             SUBSTR(CAST(NOW() AS CHAR(50)), 15, 2)),
             SUBSTR(CAST(NOW() AS CHAR(50)), 18, 2)) AS DECIMAL(6, 0))
             FROM SYSIBM.SYSDUMMY1),
    AFIUSP = (SELECT CURRENT_USER
              FROM SYSIBM.SYSDUMMY1),
    AFINCE = :ds_response_baja.nro_autorizacion_federal ,
    AFIPRC = :ds_response_baja.nro_proceso_federal  ,
    AFICHE = CAST(SUBSTR(:ds_response_baja.nro_autorizacion_federal, 1, 7)
              AS DECIMAL(7, 0)),
    AFESTA = CASE WHEN :ds_response_baja.codigo_aprobado = 'APR'
                  THEN 'A'
                  ELSE 'R'
                  END,
    AFAMRC = :ds_response_baja.filler,
    AF$A03 = :ds_response_baja.importe_rellenar,
    AFDF05 = 'FEDERAL_ONLINE',
    AFFBAJ = 0

    WHERE    AFIRRN= :id_prestamo -1
         AND AFFECH= :aasfei
         AND AFIOPT='B';
    if sqlcode <> *zero;
        dsply_msj = 'Error al proc baja: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        msg4 = 'filler: ' + ds_response_baja.filler
              +'. sql:'+%trim(%char(sqlcode));
        EXSR FinProgramaSR;
    endif;
ENDSR;
//**************************************************
// ProcesaAltaSR: Procesa una alta de prestamo
//**************************************************
BEGSR ProcesaAltaSR;
      if sqlcode <> *zero
         and (ds_response_alta.codigo_aprobado <> 'APR' or
              ds_response_alta.codigo_aprobado <> 'RCH');
           dsply_msj = 'Error al proc alta: ' + %trim(%char(sqlcode));
           dsply (dsply_msj);
           msg4 = 'filler: ' + ds_response_alta.filler
                 +'. sql:'+%trim(%char(sqlcode));
           EXSR FinProgramaSR;
      endif;
    exec sql
    UPDATE PRAFED SET
    AFFECH = SUBSTR(:ds_response_alta.fecha, 7, 4)||
             SUBSTR(:ds_response_alta.fecha, 4, 2)||
             SUBSTR(:ds_response_alta.fecha, 1, 2),
    AFISEQ = '0',
    AFFACR = (SELECT AASFEI
            FROM SGSYSV LIMIT 1)   ,
    AFHSUP = (SELECT CAST(CONCAT(CONCAT(SUBSTR(CAST(NOW() AS CHAR(50)), 12, 2),
             SUBSTR(CAST(NOW() AS CHAR(50)), 15, 2)),
             SUBSTR(CAST(NOW() AS CHAR(50)), 18, 2)) AS DECIMAL(6, 0))
             FROM SYSIBM.SYSDUMMY1),
    AFIUSA = (SELECT CURRENT_USER
             FROM SYSIBM.SYSDUMMY1),
    AFFAU1 = SUBSTR(:ds_response_alta.fecha_autorizacion, 7, 4)||
             SUBSTR(:ds_response_alta.fecha_autorizacion, 4, 2)||
             SUBSTR(:ds_response_alta.fecha_autorizacion, 1, 2),
    AFHAU1 = SUBSTR(:ds_response_alta.fecha_autorizacion, 12, 2)||
             SUBSTR(:ds_response_alta.fecha_autorizacion, 15, 2)||
             '00',
    AFFE01 = CAST(
             CASE WHEN   LENGTH(RTRIM(TRANSLATE(SUBSTR
         (:ds_response_alta.fecha_autorizacion, 7, 4), '*', ' 0123456789'))) = 0
                     AND LENGTH(RTRIM(TRANSLATE(SUBSTR
         (:ds_response_alta.fecha_autorizacion, 4, 2), '*', ' 0123456789'))) = 0
                     AND LENGTH(RTRIM(TRANSLATE(SUBSTR
         (:ds_response_alta.fecha_autorizacion, 1, 2), '*', ' 0123456789'))) = 0
             THEN  (
                    SUBSTR(:ds_response_alta.fecha_autorizacion, 7, 4)||
                    SUBSTR(:ds_response_alta.fecha_autorizacion, 4, 2)||
                    SUBSTR(:ds_response_alta.fecha_autorizacion, 1, 2)
                   )
             ELSE 0
             END
             AS DECIMAL(8, 0)),
    AFFTRA = (SELECT AASFEI
            FROM SGSYSV LIMIT 1)   ,
    AFHTRA = (SELECT CAST(CONCAT(CONCAT(SUBSTR(CAST(NOW() AS CHAR(50)), 12, 2),
             SUBSTR(CAST(NOW() AS CHAR(50)), 15, 2)),
             SUBSTR(CAST(NOW() AS CHAR(50)), 18, 2)) AS DECIMAL(6, 0))
             FROM SYSIBM.SYSDUMMY1),
    AFIUSP = (SELECT CURRENT_USER
              FROM SYSIBM.SYSDUMMY1),
    AFINCE = :ds_response_alta.nro_autorizacion_federal ,
    AFIPRC = :ds_response_alta.nro_proceso_federal  ,
    AFICHE = CAST(SUBSTR(:ds_response_alta.nro_autorizacion_federal, 1, 7)
              AS DECIMAL(7, 0)),
    AFESTA = CASE WHEN :ds_response_alta.codigo_aprobado = 'APR'
                  THEN 'A'
                  ELSE 'R'
                  END,
    AFAMRC = :ds_response_alta.filler,
    AF$A03 = :ds_response_alta.importe_rellenar,
    AFDF05 = 'FEDERAL_ONLINE',
    AFFBAJ = 0

    WHERE    AFIRRN= :id_prestamo
         AND AFFECH= :aasfei;
    if sqlcode <> *zero;
        dsply_msj = 'Error al proc alta: ' + %trim(%char(sqlcode));
        dsply (dsply_msj);
        msg4 = 'filler: ' + ds_response_alta.filler
              +'. sql:'+%trim(%char(sqlcode));
        EXSR FinProgramaSR;
    endif;
ENDSR;
//**************************************************
// Fin Procedimiento Main:
//**************************************************
end-proc;

//**************************************************
//         PROCEDIMIENTOS ADICIONALES
//**************************************************
// exeCmd: Ejecuta un comando del sistema operativo
//         y retorn el cpf resultante.
//**************************************************
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
//**************************************************
