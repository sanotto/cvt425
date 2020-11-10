*SRCMBRTXT:TD: Acusa TD como devuelta por OCA     
**free
ctl-opt dftname(TCPA01MA)
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
    dcl-s error_ErrorValidarTD      packed(1:0) inz(0);
    dcl-s error_EnviarMail        packed(1:0) inz(0);
    dcl-s error_RealizarDevolucion     packed(1:0) inz(0);

    dcl-s subject                 char(256) inz(*blanks);
    dcl-s mesg                    char(256) inz(*blanks);
    dcl-s file                    char(256) inz(*blanks);

    dcl-s aasfei                  packed(8:0);
    dcl-s aasfei_aaaammdd         char(8);
    dcl-s hora                    packed(6:0);
    dcl-s path_full               char(250) inz(*blanks);
    dcl-s path_full_historico     char(250) inz(*blanks);
    dcl-s comando                 char(250) inz(*blanks);
    dcl-s rc                      char(7);
    dcl-s registro                char(256);
    dcl-s cmd                     varchar(4096);


    dcl-s job                     packed(16:0);
    dcl-s bandera_validar_td      packed(1:0);
    dcl-s bandera_salida          packed(1:0);
    dcl-s bandera_devuelta        packed(1:0);




    dcl-ds teclas_funcion qualified;
        f1     char(1);
        f2     char(1);
        f3     char(1);
        f4     char(1);
        f5     char(1);
        f6     char(1);
        f7     char(1);
        f8     char(1);
        f9     char(1);
        f10    char(1);
        f11    char(1);
        f12    char(1);
        f13    char(1);
        f14    char(1);
        f15    char(1);
        f16    char(1);
        f17    char(1);
        f18    char(1);
        f19    char(1);
        f20    char(1);
        f21    char(1);
        f22    char(1);
        f23    char(1);
        f24    char(1);
    end-ds;

    dcl-ds tarjeta_debito qualified;
        tantar         packed(16:0);
        version        packed(1:0);
        dig_ver        packed(1:0);
        nombre         char(30);
        venc           packed(4:0);
    end-ds;



   dcl-pr CallSGAUTAC4 extpgm('SGAUTAC4');
       comando        char(250);
   end-pr;

//*****************************************************************************
// Ciclo de programa
//*****************************************************************************
EXSR Inicializacion;
dow bandera_salida=0;
    EXSR CallLITA00RS;
    EXSR VerificarSalida;
    if bandera_salida=0;
        EXSR ValidoTD;
        if  bandera_validar_td = 0 and bandera_devuelta= 0;
            EXSR WrtLIKMTH;
            EXSR RealizarDevolucion;
        else;
            EXSR ErrorValidarTD;
        endif;
    endif;
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
    aasfei_aaaammdd =  %subst(%char(aasfei): 1: 4)+
                       %subst(%char(aasfei): 5: 2)+
                       %subst(%char(aasfei): 7: 2);
    exec sql select replace(char(current_time), ':', '') into :hora
             from sysibm.sysdummy1;

    job= %dec(pgm_stat.job_number_char:6:0);
    if sqlcode <> 0;
        error_Inicializacion = 1;
        EXSR FinPrograma;
    endif;

ENDSR;
//*****************************************************************************
// CallLITA00RS
//*****************************************************************************
BEGSR CallLITA00RS;

     cmd = 'CALL LITA00RS';
     rc = exeCmd(cmd);

    if rc <> 'CPF0000';
        error_OpenCursors = 1;
        EXSR FinPrograma;
    endif;

ENDSR;
//*****************************************************************************
// ValidoTD
//*****************************************************************************
BEGSR ValidoTD;
    bandera_validar_td=1;
    bandera_devuelta=0;
    exec sql DECLARE C1 CURSOR FOR
             SELECT
                   @CITAR,
                   @CIPLA,
                   @CIDIG,
                   @CNGRU,
                   @CIPAI
             FROM @CPIUSD
             WHERE @ZJOBN = :job ;
    exec sql OPEN C1;
    exec sql FETCH C1 into :tarjeta_debito;
    exec sql close C1;

    if sqlcode=0 and tarjeta_debito.tantar>0;
     exec sql
             SELECT 0 into :bandera_validar_td
             FROM LIKMTR
             WHERE    TANTAR=:tarjeta_debito.tantar
                  AND TAIETA IN ('R','G');

     exec sql
             SELECT 1 into :bandera_devuelta
             FROM LIKMTR
             WHERE    TANTAR=:tarjeta_debito.tantar
                  AND TAIETA='R'
                  AND SUBSTR(TADF06, 11, 8) = :aasfei;
    endif;

ENDSR;
//*****************************************************************************
// ErrorValidarTD
//*****************************************************************************
BEGSR ErrorValidarTD;

    if bandera_devuelta = 1;
        MessageBox('La TD ya fue DEVUELTA al Banco.');
    else;
        MessageBox('Estado incorrecto de la TD para realizar devolucion.');
    endif;
    error_ErrorValidarTD = 1;

ENDSR;
//*****************************************************************************
// RealizarDevolucion
//*****************************************************************************
BEGSR RealizarDevolucion;

    exec sql
       UPDATE LIKMTR
       SET
         TADF06=SUBSTR(TADF06, 1, 10)||
         :aasfei_aaaammdd||
          VARCHAR(
            CAST(CASE
                  WHEN SUBSTR(TADF06, 19, 1)=' ' THEN '0'
                  ELSE SUBSTR(TADF06, 19, 1)
                 END
            AS DECIMAL(1, 0)) + 1
          )||
         SUBSTR(TADF06, 20, 1),
         TAIETA=CASE
                  WHEN TAIETA='G' THEN 'R'
                  ELSE TAIETA
                END,
         TAF001=CASE
                  WHEN TAIETA='G' THEN :aasfei
                  ELSE TAF001
                END
       WHERE TANTAR=:tarjeta_debito.tantar;

     if sqlcode = 0;
        MessageBox('            Devolucion correcta.                       '+
                   '                                                       '+
                   'TD: '+%char(tarjeta_debito.tantar)  + ' ' +
                          %char(tarjeta_debito.version) + ' ' +
                          %char(tarjeta_debito.dig_ver) +
                   '                               '+
                   'Titular: '+tarjeta_debito.nombre+'               '+
                   '                                                       '+
                   '                                                       '+
                   '                    Presione Intro para continuar...   ');
     else;
        MessageBox('Error al realizar Devolucion.');
        error_RealizarDevolucion = 1;
     endif;

ENDSR;
//*****************************************************************************
// WrtLIKMTH
//*****************************************************************************
BEGSR WrtLIKMTH;

    exec sql
        INSERT INTO LIKMTH
        SELECT
                TAISUC,--DECIMAL(5,0)-Número de sucursal
                TANTAR,--DECIMAL(16,0)-Número de Tarjeta
                TAISUB,--CHAR(2)-Código de subsistema
                TAICCL,--DECIMAL(9,0)-Número cuenta cliente
                TAFALT,--DECIMAL(8,0)-Fecha de alta
                TAIBAC,--DECIMAL(2,0)-Código de bloqueo para caja de
                TAFEBL,--DECIMAL(8,0)-Fecha de Bloqueo
                TACATC,--NUMERIC(2,0)-Categoría de comisión
                TAILDB,--NUMERIC(2,0)-Código límite débito
                TATCPR,--NUMERIC(2,0)-Tipo de cuenta principal
                TAIGRC,--CHAR(2)-Grupo de cartera
                TAISGC,--CHAR(2)-Sub-grupo de cartera
                TAINCT,--DECIMAL(11,0)-Número de cuenta
                TAFENT,--DECIMAL(8,0)-Fecha de entrega
                TAFBAJ,--DECIMAL(8,0)-Fecha de baja
                TAIETA,--CHAR(1)-Estado tarjeta Débito NBLR
                TAFVTO,--NUMERIC(4,0)-Fecha de Venc.  MM/AA
                TAIUTI,--DECIMAL(6,0)-Número de usuario tarjeta inte
                TAICAU,--CHAR(1)-Costo asume usuario
                TAICSC,--CHAR(2)-Cód. subsistema cobro
                TANCCO,--DECIMAL(11,0)-Número de cuenta cobro
                TAFAAL,--DECIMAL(8,0)-Fecha de Certif.de Superv.
                TAECON,--DECIMAL(1,0)-Reimpresión de Pin
                TAEMPR,--DECIMAL(1,0)-Reimpresión de Plástico
                TAIACF,--DECIMAL(1,0)-Emisión Tarjeta Adicional
                TAIACR,--DECIMAL(1,0)-Otros
                TAIMIM,--NUMERIC(1,0)-Miembro
                TAIVIN,--NUMERIC(1,0)-Versión
                TAIDIG,--DECIMAL(1,0)-Dígito Verificador
                TAFMOD,--DECIMAL(8,0)-Fecha entrega nueva Versión
                TANYAP,--CHAR(30)-Apellido y nombre
                TAITDO,--DECIMAL(2,0)-Tipo de documento
                TAINDO,--DECIMAL(15,0)-Número de documento
                TAFEIG,--DECIMAL(8,0)-Fecha de Vinculacion Cuenta
                TAFACR,--DECIMAL(8,0)-Fecha de Solicitud Nueva Versi
                TAF001,--DECIMAL(8,0)-Fecha de Recepcion TD
                TAF002,--DECIMAL(8,0)-Fecha de Recepcion TD Interior
                TAF003,--DECIMAL(8,0)-Fecha de Envio al Interior
                TAISDE,--DECIMAL(5,0)-Sucursal Envio Interior
              0, --DECIMAL(8,0)-Fecha de desvinculacion cuenta
                TAIAST,--DECIMAL(2,0)-Nro.Orden Cuenta
              (SELECT AASFEI FROM SGSYSV LIMIT 1),--FECHA INGRESO
              (SELECT  REPLACE(VARCHAR(CURRENT_TIME), ':', '')
               FROM SYSIBM.SYSDUMMY1),--HORA
              (SELECT CURRENT_USER FROM SYSIBM.SYSDUMMY1),--USUARIO
              'LITA02F2',--PROGRAMA INGRESO
              (SELECT
                 SUBSTR(JOB_NAME, LOCATE_IN_STRING(JOB_NAME, '/', -1) +1)
                 FROM SYSIBM.SYSDUMMY1),
              'DEV_OCA',
              'DEVOLUCION OCA DE LA TD',
                TADF01,--CHAR(20)-Descripción de campo
                TADF02,--CHAR(20)-Descripción de campo
                TADF03,--CHAR(20)-Descripción de campo
                TADF04,--CHAR(20)-Descripción de campo
                TADF05,--CHAR(20)-Descripción de campo
                TADF06,--CHAR(20)-Descripción de campo
                TADF07,--CHAR(20)-Descripción de campo
                TADF08--CHAR(20)-Descripción de campo
        FROM LIKMTR
        WHERE TANTAR=:tarjeta_debito.tantar;

ENDSR;
//*****************************************************************************
// VerificarSalida:
//*****************************************************************************
BEGSR VerificarSalida;

    bandera_salida =0;
    exec sql DECLARE C2 CURSOR FOR
            SELECT
                 substr(@ZFNKY, 1 , 1),
                 substr(@ZFNKY, 2 , 1),
                 substr(@ZFNKY, 3 , 1),
                 substr(@ZFNKY, 4 , 1),
                 substr(@ZFNKY, 5 , 1),
                 substr(@ZFNKY, 6 , 1),
                 substr(@ZFNKY, 7 , 1),
                 substr(@ZFNKY, 8 , 1),
                 substr(@ZFNKY, 9 , 1),
                 substr(@ZFNKY, 10, 1),
                 substr(@ZFNKY, 11, 1),
                 substr(@ZFNKY, 12, 1),
                 substr(@ZFNKY, 13, 1),
                 substr(@ZFNKY, 14, 1),
                 substr(@ZFNKY, 15, 1),
                 substr(@ZFNKY, 16, 1),
                 substr(@ZFNKY, 17, 1),
                 substr(@ZFNKY, 18, 1),
                 substr(@ZFNKY, 19, 1),
                 substr(@ZFNKY, 20, 1),
                 substr(@ZFNKY, 21, 1),
                 substr(@ZFNKY, 22, 1),
                 substr(@ZFNKY, 23, 1),
                 substr(@ZFNKY, 24, 1)
            FROM @CPISYS WHERE @ZJOBN=:job;

    exec sql OPEN C2;
    exec sql FETCH C2 into :teclas_funcion;
    exec sql close C2;

    if teclas_funcion.f3='1' or teclas_funcion.f12='1' ;
        bandera_salida=1;
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
//=================================================
// PROC: MessageBox (Muestra msg en pantalla)
//=================================================
dcl-proc MessageBox;
    dcl-pi *n ;
        mensaje    char(440) const;
    end-pi;

    dcl-pr dspmsg extpgm('BAER00RS');
            linea_1         char(55) ;
            linea_2         char(55) ;
            linea_3         char(55) ;
            linea_4         char(55) ;
            linea_5         char(55) ;
            linea_6         char(55) ;
            linea_7         char(55) ;
            linea_8         char(55) ;
    end-pr;

    dcl-ds msg_ds;
       msgstr           char(440);
       linea1           char(55) overlay(msgstr:*next);
       linea2           char(55) overlay(msgstr:*next);
       linea3           char(55) overlay(msgstr:*next);
       linea4           char(55) overlay(msgstr:*next);
       linea5           char(55) overlay(msgstr:*next);
       linea6           char(55) overlay(msgstr:*next);
       linea7           char(55) overlay(msgstr:*next);
       linea8           char(55) overlay(msgstr:*next);
    end-ds;
    msgstr=mensaje;
    dspmsg (linea1:
            linea2:
            linea3:
            linea4:
            linea5:
            linea6:
            linea7:
            linea8);
end-proc;
//*****************************************************************************
