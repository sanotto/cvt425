*SRCMBRTXT:Conc.Clearing Debitos TDs -MA255       
**free
ctl-opt dftname(TCT200RG)
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
        parefm                char(9);
        archivo               char(30);
        error                 char(1);
    end-pi;

    //Declaro Variables
    dcl-s aasfei                  packed(8:0);
    dcl-s aasfei_aaaammdd         char(10);
    dcl-s hora                    packed(6:0);
    dcl-s bandera                 packed(1:0);
    dcl-s iche_banume             packed(15:0) inz(*zeros);
    dcl-s etiqueta                char(30);
    dcl-s path_full               char(250) inz(*blanks);
    dcl-s path_full_historico     char(250) inz(*blanks);
    dcl-s path_reporte            char(250) inz(*blanks);
    dcl-s rc                      char(7);
    dcl-s nro_registros           packed(10: 0);
    dcl-s cmd                     varchar(4096);

    dcl-s resultado_operacion     char(20);
    dcl-s nro_iche                packed(7:0);
    dcl-s nro_comprobante         packed( 7:0);

    dcl-s codigo_proceso_ac    packed( 3:0);
    dcl-s sucursal_ac          packed( 5:0);
    dcl-s moneda_ac            packed( 9:0);
    dcl-s numero_cuenta_ac     packed(11:0);
    dcl-s codigo_movimiento_ac packed( 3:0);
    dcl-s importe              packed(15:2);
    dcl-s nro_comprobante_ac   packed( 7:0);
    dcl-s codigo_caja_ac       packed( 5:0);
    dcl-s hora_operacion_ac    packed( 6:0);
    dcl-s fecha_operacion_ac   packed( 8:0);
    dcl-s error_operacion_ac   char(40)    ;
    dcl-s codigo_proceso_cc    packed( 3:0);
    dcl-s sucursal_cc           packed( 5:0);
    dcl-s moneda_cc            packed( 9:0);
    dcl-s numero_cuenta_cc     packed(11:0);
    dcl-s codigo_movimiento_cc packed( 3:0);
    dcl-s nro_comprobante_cc   packed( 7:0);
    dcl-s codigo_caja_cc       packed( 5:0);
    dcl-s hora_operacion_cc    packed( 6:0);
    dcl-s fecha_operacion_cc   packed( 8:0);
    dcl-s error_operacion_cc   char(40)    ;

    dcl-ds ds_reg1 qualified;
        unif_otor                         packed(3:0)    pos(1);
        tipo_registro                     packed(1:0)    pos(4);
        cod_archivo                       char(8)        pos(5);
        fecha_proceso                     packed(8:0)    pos(13);
        filler                            char(130)      pos(21);
    end-ds;

    dcl-ds ds_reg2 qualified;
        unif_otor2                        packed(3:0)    ;//pos(1);
        tipo_registro                     packed(1:0)    ;//pos(4);
        sucur_otor                        packed(3:0)    ;//pos(5);
        nro_tarjeta                       char(19)       ;//pos(8);
        fec_presen                        packed(8:0)    ;//pos(27);
        fec_clearing                      packed(8:0)    ;//pos(35);
        fec_oper                          packed(8:0)    ;//pos(43);
        hora_operacion                    packed(6:0)    ;//pos(51);
        codigo_mov                        packed(3:0)    ;//pos(57);
        importe_total                     packed(13:2)   ;//pos(60);
        signo_oper                        char(1)        ;//pos(73);
        codigo_moneda                     char(3)        ;//pos(74);
        oddigo_moneda_orig                packed(3:0)    ;//pos(77);
        cod_terminal                      packed(9:0)    ;//pos(80);
        lote                              packed(9:0)    ;//pos(89);
        cupon_pos                         packed(9:0)    ;//pos(98);
        trace_number                      char(6)        ;//pos(107);
        nombre_fantasia                   char(22)       ;//pos(113);
        filler                            char(16)       ;//pos(135);
        registro                          char(150)      ;//pos(1);
    end-ds;

    dcl-ds ds_cuenta_acreditar qualified;
        nro_tarjeta                  packed(16:0) inz(0);
        subsistema                   char(2)      inz(*blanks);
        sucursal                     packed(5:0)  inz(*blanks);
        cuenta                       packed(9:0)  inz(*blanks);
        moneda                       packed(9:0)  inz(*blanks);
    end-ds;



// Dcl-DS FECIDS qualified;
// ACA PUEDO IGUALAR FECINV con un char de 8 y ya se cargan el resto de variable
//      FECINV                   Char(8) Pos(1);
//      A#O                      Char(4) Pos(1);
//      MES                      Char(2) Pos(5);
//      DIA                      Char(2) Pos(7);
//   End-DS;


    dcl-pr CallAC0223R4 extpgm('AC0223R4');
        codigo_proceso_ac    packed( 3:0); //IDAC: Para cobrar codigos asociados
        sucursal_ac          packed( 5:0); //ISAL: sucursal de la cuenta a op.
        moneda_ac            packed( 9:0); //IMON: moneda de la cuenta a operar
        numero_cuenta_ac     packed(11:0); //ICAH: nro cuenta de la cuenta a op.
        codigo_movimiento_ac packed( 3:0); //IMCA: De la operacion
        importe              packed(15:2); //$IMP: importe de la operacion
        nro_comprobante_ac   packed( 7:0); //ICHE: nro de comprobante de la op.
        codigo_caja_ac       packed( 5:0); //ICAJ: codigo de caja de la op.
        hora_operacion_ac    packed( 6:0); //HORA: hora de la operacion
        fecha_operacion_ac   packed( 8:0); //FCOM: fecha de la operacion
        error_operacion_ac   char(40)    ; //DERR: error
    end-pr;

    dcl-pr CallCC0225R1 extpgm('CC0225R1');
        codigo_proceso_cc    packed( 3:0); //IDAC: Para cobrar codigos asociados
        sucursal_cc          packed( 5:0); //ISAL: sucursal de la cuenta a op.
        moneda_cc            packed( 9:0); //IMON: moneda de la cuenta a operar
        numero_cuenta_cc     packed(11:0); //ICAH: nro cuenta de la cuenta a op.
        codigo_movimiento_cc packed( 3:0); //IMCA: De la operacion
        importe              packed(15:2); //$IMP: importe de la operacion
        nro_comprobante_cc   packed( 7:0); //ICHE: nro de comprobante de la op.
        codigo_caja_cc       packed( 5:0); //ICAJ: codigo de caja de la op.
        hora_operacion_cc    packed( 6:0); //HORA: hora de la operacion
        fecha_operacion_cc   packed( 8:0); //FCOM: fecha de la operacion
        error_operacion_cc   char(40)    ; //DERR: error
    end-pr;
//*****************************************************************************
// Ciclo de programa
//*****************************************************************************
EXSR Inicializacion;
EXSR OpenCursors;
EXSR ReadC1;
EXSR ValidaArchivo;
EXSR ReadC2;
dow  SQLCOD = *Zero;
    EXSR WrtTC255D;
    EXSR LimpiarVariables;
    EXSR ReadC2;
enddo;
EXSR GenerarInforme;
EXSR EnviarMail;
EXSR MoverHistorico;
EXSR FinPrograma;

//*****************************************************************************
//                  SUBRUTINAS
//*****************************************************************************
// Inicializacion: Inicializa programa
//*****************************************************************************
BEGSR Inicializacion;

     error = '1';
     exec sql set option commit=*None;
     exec sql select aasfei into :aasfei from sgsysv limit 1;
     aasfei_aaaammdd =  %subst(%char(aasfei): 7: 2)+'/'+
                        %subst(%char(aasfei): 5: 2)+'/'+
                        %subst(%char(aasfei): 1: 4);
     exec sql select replace(char(current_time), ':', '') into :hora
              from sysibm.sysdummy1;
     exec sql DELETE QTEMP/WWTEMP;
     exec sql DROP TABLE QTEMP/WWTEMP;
     exec sql CREATE TABLE QTEMP/WWTEMP (REGISTRO CHAR(150));
     path_full = '/home/Tarjetas_de_Creditos/PADRON_PROC/'+%trim(parefm);
     cmd = 'CPYFRMIMPF FROMSTMF('''+%trim(path_full)+''') '+
           'TOFILE(QTEMP/WWTEMP) '+
           'RCDDLM(*CRLF) STRDLM(*NONE) RMVBLANK(*NONE) FLDDLM('';'')';
     rc = exeCmd(cmd);

ENDSR;
//*****************************************************************************
// ValidaArchivo
//*****************************************************************************
BEGSR ValidaArchivo;
    etiqueta = %trim(parefm) + '_' + %trim(%char(ds_reg1.fecha_proceso));
    exec sql
            SELECT 1 into :bandera
            FROM LISERJ
            WHERE RJDACO LIKE :etiqueta
            LIMIT 1;
    if bandera = 1;
        error = '0';
        archivo = etiqueta;
        EXSR FinPrograma;
    endif;
    if ds_reg1.unif_otor  <> 161
    or ds_reg1.tipo_registro <> 0
or ds_reg1.fecha_proceso = 0
    and (ds_reg1.cod_archivo <> 'MA255D  '
        or ds_reg1.cod_archivo <> 'MA255DA ');
        error = '1';
        EXSR FinPrograma;
    endif;

ENDSR;
//*****************************************************************************
// OpenCursors
//*****************************************************************************
BEGSR OpenCursors;

    exec sql
    DECLARE C1 CURSOR FOR
    SELECT
        CAST(SUBSTR(REGISTRO, 1, 3)  AS DECIMAL(3, 0))    UNIF_OTOR,
        CAST(SUBSTR(REGISTRO, 4, 1)  AS DECIMAL(1, 0))    TIPO_REGISTRO,
        SUBSTR(REGISTRO, 5, 8)                            COD_ARCHIVO,
        CAST(SUBSTR(REGISTRO, 13, 8)  AS DECIMAL(8, 0))   FECHA_PROCESO,
        SUBSTR(REGISTRO, 21, 130)                         FILLER
    FROM QTEMP/WWTEMP
    WHERE CAST(SUBSTR(REGISTRO, 4, 1)  AS DECIMAL(1, 0)) = 0;

    exec sql OPEN C1;

    exec sql
    DECLARE C2 CURSOR FOR
    SELECT
        CAST(SUBSTR(REGISTRO, 1, 3)  AS DECIMAL(3, 0))    UNIF_OTOR2,
        CAST(SUBSTR(REGISTRO, 4, 1)  AS DECIMAL(1, 0))    TIPO_REGISTRO,
        CAST(SUBSTR(REGISTRO, 5, 3)  AS DECIMAL(3, 0))    SUCUR_OTOR,
        SUBSTR(REGISTRO, 8, 19)                           NRO_TARJETA,
        CAST(SUBSTR(REGISTRO, 27, 8)  AS DECIMAL(8, 0))   FEC_PRESEN,
        CAST(SUBSTR(REGISTRO, 35, 8)  AS DECIMAL(8, 0))   FEC_CLEARING,
        CAST(SUBSTR(REGISTRO, 43, 8)  AS DECIMAL(8, 0))   FEC_OPER,
        CAST(SUBSTR(REGISTRO, 51, 6)  AS DECIMAL(6, 0))   HORA_OPERACION,
        CAST(SUBSTR(REGISTRO, 57, 3)  AS DECIMAL(3, 0))   CODIGO_MOV,
        CAST(SUBSTR(REGISTRO, 60, 13)  AS DECIMAL(13, 2)) IMPORTE_TOTAL,
        SUBSTR(REGISTRO, 73, 1)                           SIGNO_OPER    ,
        SUBSTR(REGISTRO, 74, 3)                           CODIGO_MONEDA    ,
        CAST(SUBSTR(REGISTRO, 77, 3)  AS DECIMAL(3, 0))   ODDIGO_MONEDA_ORIG,
        CAST(SUBSTR(REGISTRO, 80, 9)  AS DECIMAL(9, 0))   COD_TERMINAL    ,
        CAST(SUBSTR(REGISTRO, 89, 9)  AS DECIMAL(9, 0))   LOTE    ,
        CAST(SUBSTR(REGISTRO, 98, 9)  AS DECIMAL(9, 0))   CUPON_POS    ,
        SUBSTR(REGISTRO, 107, 6)                          TRACE_NUMBER    ,
        SUBSTR(REGISTRO, 113, 22)                         NOMBRE_FANTASIA    ,
        SUBSTR(REGISTRO, 135, 16)                         FILLER,
        REGISTRO
    FROM QTEMP/WWTEMP
    WHERE    CAST(SUBSTR(REGISTRO, 4, 1)  AS DECIMAL(1, 0)) = 1;
      // AND CAST(SUBSTR(REGISTRO, 57, 3) AS DECIMAL(3, 0)) IN (821, 822)

    exec sql OPEN C2;

ENDSR;
//*****************************************************************************
// ReadC1
//*****************************************************************************
BEGSR ReadC1;
             exec sql FETCH C1 into :ds_reg1;
ENDSR;
//*****************************************************************************
// ReadC2
//*****************************************************************************
BEGSR ReadC2;
             exec sql FETCH C2 into :ds_reg2;
ENDSR;
//*****************************************************************************
// LimpiarVariables
//*****************************************************************************
BEGSR LimpiarVariables;

    ds_cuenta_acreditar.nro_tarjeta    =    *zeros ;
    ds_cuenta_acreditar.subsistema     =    *blanks;
    ds_cuenta_acreditar.sucursal       =    *zeros ;
    ds_cuenta_acreditar.cuenta         =    *zeros ;
    ds_cuenta_acreditar.moneda         =    *zeros ;

ENDSR;
//*****************************************************************************
// WrtTC255D: Write TC255D - CONCILIACION CON CLEARING DEBITOS CON TDs
//*****************************************************************************
BEGSR WrtTC255D;

    resultado_operacion = 'No impactado';

    if (ds_reg2.codigo_mov = 821 or ds_reg2.codigo_mov=822 or
        ds_reg2.codigo_mov = 710);
        exec sql
            SELECT
                    TANTAR,
                    TAISUB,
                    OTISUC,
                    OTICCL,
                    CASE WHEN FUICAH IS NOT NULL
                             THEN FUIMON
                         WHEN BMICCC IS NOT NULL
                             THEN BMIMON
                    END AS MONEDA
            INTO :ds_cuenta_acreditar
            FROM LIKMTR
            INNER JOIN BADCCL ON otisuc=taisuc and oticcl=taiccl and otittl= 1
            LEFT JOIN ACCTAC ON
                                 TAISUB='AC'
                             AND TAISUC=FUISUC
                             AND TAICCL=FUICCL
                                AND FUFBAJ=0
                                AND FUIBAC=0
                             AND FUISGC<> 'IN'
               LEFT JOIN CCCTCT ON
                                  TAISUB='CC'
                              AND TAISUC=BMISUC
                              AND TAICCL=BMICCC
                              AND BMFBAJ=0
                              AND BMIBCC=0
                              AND BMISGC<> 'IN'
            WHERE
                     (FUICCL IS NOT NULL OR BMICCC IS NOT NULL)
                 AND (TAFBAJ=0)
                 AND (TANTAR = CAST(SUBSTR(:ds_reg2.nro_tarjeta, 1, 16)
                               AS DECIMAL(16, 0)))
            ORDER BY TAIAST;

        if ds_cuenta_acreditar.nro_tarjeta > 0
           and ds_cuenta_acreditar.cuenta > 0;

            if ds_cuenta_acreditar.subsistema = 'AC';

                codigo_proceso_ac    = 203;
                sucursal_ac          = ds_cuenta_acreditar.sucursal;
                moneda_ac            = ds_cuenta_acreditar.moneda;
                numero_cuenta_ac     = ds_cuenta_acreditar.cuenta;
                codigo_movimiento_ac = ds_reg2.codigo_mov;
                if ds_reg2.codigo_mov = 710;
                    codigo_movimiento_ac = 408;
                    ds_reg2.codigo_mov   = 408;
                endif;
                importe              = ds_reg2.importe_total/100;
                codigo_caja_ac       = 556;
                hora_operacion_ac    = hora;
                fecha_operacion_ac   = aasfei;

                if ds_reg2.cupon_pos = 0;
                    EXSR GeneroComprobante;
                endif;
                nro_comprobante_ac   = ds_reg2.cupon_pos;
                nro_iche             = nro_comprobante_ac;

                CallAC0223R4(codigo_proceso_ac   :
                             sucursal_ac         :
                             moneda_ac           :
                             numero_cuenta_ac    :
                             codigo_movimiento_ac:
                             importe             :
                             nro_comprobante_ac  :
                             codigo_caja_ac      :
                             hora_operacion_ac   :
                             fecha_operacion_ac  :
                             error_operacion_ac
                             );
                if error_operacion_ac = *blanks;
                    resultado_operacion = 'Impactado';
                endif;
                nro_iche = nro_comprobante_ac;

            endif;

            if ds_cuenta_acreditar.subsistema = 'CC';

                EXSR GeneroComprobante;
                codigo_proceso_cc    = 5;
                sucursal_cc          = ds_cuenta_acreditar.sucursal;
                moneda_cc            = ds_cuenta_acreditar.moneda;
                numero_cuenta_cc     = ds_cuenta_acreditar.cuenta;
                codigo_movimiento_cc = ds_reg2.codigo_mov;
                if ds_reg2.codigo_mov = 710;
                    codigo_movimiento_cc = 408;
                    ds_reg2.codigo_mov   = 408;
                endif;
                importe              = ds_reg2.importe_total/100;
                codigo_caja_cc       = 556;
                hora_operacion_cc    = hora;
                fecha_operacion_cc   = aasfei;

                if ds_reg2.cupon_pos = 0;
                    EXSR GeneroComprobante;
                endif;
                nro_comprobante_cc   = ds_reg2.cupon_pos;
                nro_iche             = nro_comprobante_cc;

                CallCC0225R1(codigo_proceso_cc   :
                             sucursal_cc         :
                             moneda_cc           :
                             numero_cuenta_cc    :
                             codigo_movimiento_cc:
                             importe             :
                             nro_comprobante_cc  :
                             codigo_caja_cc      :
                             hora_operacion_cc   :
                             fecha_operacion_cc  :
                             error_operacion_cc
                             );
                if error_operacion_cc = *blanks;
                    resultado_operacion = 'Impactado';
                endif;
                nro_iche = nro_comprobante_cc;

            endif;

        endif;

    endif;

    exec sql
    INSERT INTO TC255D
    (
         T2ICEN,
         T2FING,
         T2FAAL,
         T2FPRE,
         T2FASI,
         T2FALT,
         T2HALT,
         T2ITAR,
         T2IMIM,
         T2IVIN,
         T2IDIG,
         T2ISUB,
         T2ISUC,
         T2ICCL,
         T2IMON,
         T2IMCA,
         T2$IMP,
         T2ITER,
         T2ILCB,
         T2ILCE,
         T2NDNN,
         T2DF04,
         T2DF01,
         T2ICHE,
         T2BLK0
    ) VALUES (
         :ds_reg1.unif_otor,
         :aasfei,
         :ds_reg1.fecha_proceso,
         :ds_reg2.fec_presen,
         :ds_reg2.fec_clearing,
         :ds_reg2.fec_oper,
         :ds_reg2.hora_operacion,
         SUBSTR(VARCHAR(:ds_reg2.nro_tarjeta), 1, 16),
         SUBSTR(VARCHAR(:ds_reg2.nro_tarjeta), 16, 1),
         SUBSTR(VARCHAR(:ds_reg2.nro_tarjeta), 17, 1),
         SUBSTR(VARCHAR(:ds_reg2.nro_tarjeta), 18, 1),
         :ds_cuenta_acreditar.subsistema,
         :ds_cuenta_acreditar.sucursal,
         :ds_cuenta_acreditar.cuenta,
         :ds_cuenta_acreditar.moneda,
         :ds_reg2.codigo_mov,
         :importe,
         :ds_reg2.cod_terminal    ,
         :ds_reg2.lote    ,
         :ds_reg2.cupon_pos    ,
         :ds_reg2.nombre_fantasia,
         TRIM(:parefm),
         :resultado_operacion,
         :nro_iche,
         :ds_reg2.registro
    );

ENDSR;
//*****************************************************************************
// GeneroComprobante
//*****************************************************************************
BEGSR GeneroComprobante;

    exec sql
    SELECT WNIULN into :iche_banume
    FROM BANUME WHERE WNIPF1 LIKE '%TC255D%' AND WNIPF2 LIKE '%ICHE%';
    if iche_banume=0;
        exec sql
            INSERT INTO BANUME(
                WNIPF1, --CHAR(10)-Primer prefijo para numerador
                WNIPF2, --CHAR(10)-Segundo prefijo para numerador
                WNIPF3, --CHAR(10)-Tercer prefijo para numerador
                WNIPF4, --CHAR(10)-Cuarto prefijo para numerador
                WNIPF5, --CHAR(10)-Quinto prefijo para numerador
                WNIULN, --DECIMAL(15, 0)-Ultimo numero utilizado
                WNDULN --CHAR(30)-Descripcion del uso del numera
                ) VALUES(
                'TC255D    ', --CHAR(10)-Primer prefijo para numerador
                'ICHE      ', --CHAR(10)-Segundo prefijo para numerador
                '', --CHAR(10)-Tercer prefijo para numerador
                '', --CHAR(10)-Cuarto prefijo para numerador
                '', --CHAR(10)-Quinto prefijo para numerador
                1, --DECIMAL(15, 0)-Ultimo numero utilizado
                'Nro_ICHE p/TC255D en cero' --CHAR(30)-Descri. del uso del num
                );

        ds_reg2.cupon_pos=1;
    else;
        exec sql
        UPDATE BANUME SET WNIULN = (SELECT WNIULN
                                    FROM BANUME
                                    WHERE WNIPF1 LIKE '%TC255D%' AND
                                          WNIPF2 LIKE '%ICHE%') + 1
        WHERE WNIPF1 LIKE '%TC255D%' AND
        WNIPF2 LIKE '%ICHE%';

        ds_reg2.cupon_pos=iche_banume+1;
    endif;

ENDSR;
//*****************************************************************************
// GenerarInforme
//*****************************************************************************
BEGSR GenerarInforme;

     path_reporte = '/home/Tarjetas_de_Creditos/PADRON_HIST/'+
                             %trim(etiqueta)+'.xls';

     cmd= 'EXPPCDBF SQLSTM('''+
           'select '+
           'T2ICEN as Cod_Ent    ,'+
           'T2FING as Fec_SIDEBA ,'+
           'T2FAAL as Fec_Arch   ,'+
           'T2FPRE as Fec_Pres   ,'+
           'T2FASI as Fec_Clear  ,'+
           'T2FALT as Fec_Oper   ,'+
           'T2HALT as Hora_Oper  ,'+
           'T2ITAR as Nro_TDs    ,'+
           'T2IMIM as Miembro    ,'+
           'T2IVIN as Version    ,'+
           'T2IDIG as DV         ,'+
           'T2ISUB as Subs       ,'+
           'T2ISUC as Cod_Suc    ,'+
           'T2ICCL as Nro_Cta    ,'+
           'T2IMON as Moneda     ,'+
           'T2IMCA as Cod_Mov    ,'+
           'T2$IMP as Importe    ,'+
           'T2ICHE as Nro_Cbte   ,'+
           'T2DF01 as Observacion,'+
           'T2ITER as Terminal   ,'+
           'T2ILCB as Lote       ,'+
           'T2ILCE as Cupon      ,'+
           'T2NDNN as Comercio '+
           'from TC255D '+
           'where    T2FING = '+%trim(%char(aasfei))+
           ' and T2FAAL = '+%trim(%char(ds_reg1.fecha_proceso))+
           ' and T2ICEN = 161 '+
           ' and TRIM(T2DF04) LIKE '''''+%trim(parefm)+''''''+
           ''') OUTPAT('''+%trim(path_reporte)+''')';
     rc = exeCmd(cmd);

     cmd = 'CALL SGAUTACL ('''+path_reporte+''')';
     rc = exeCmd(cmd);
ENDSR;
//*****************************************************************************
// EnviarMail
//*****************************************************************************
BEGSR EnviarMail;

     cmd = 'SNDMAIL RECP(LIS700CL) '+
           'SUBJECT(''Operaciones de Clearing del día ('+
                    %trim(aasfei_aaaammdd)+')'')'+
           ' MESG(''Se proceso archivo '+%trim(parefm)+' del dia '+
                  %trim(aasfei_aaaammdd)+'. Se adjunta informe.'')'+
           ' FILE('''+%trim(path_reporte)+''')';
     rc = exeCmd(cmd);

     if rc = 'CPF0000';
        error = '0';
        archivo = etiqueta;
     endif;

ENDSR;
//*****************************************************************************
// MoverHistorico
//*****************************************************************************
BEGSR MoverHistorico;

ENDSR;
//*****************************************************************************
// FinPrograma: Termina el programa
//*****************************************************************************
BEGSR FinPrograma;
    exec sql close C1;
    exec sql close C2;
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
