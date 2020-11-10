*SRCMBRTXT:Link-ODE-Procesa Arch CSV De Formulario
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
    dcl-s importar_ok        ind;
    dcl-s cuenta_ok          ind;
    dcl-s ode_activa_ok      ind;
    dcl-s ode_celular_ok     ind;
    dcl-s ode_LIKMTR_ok      ind;
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
    dcl-s codigo_error       char(1)                  INZ('0');
    dcl-s saldo_fijo         packed (15:2);
    dcl-s saldo_sin_dec      packed(15:0);
    dcl-s nro_caja           packed(5:0)              INZ(556);
    dcl-s suc_cta_cred       packed(5:0);
    dcl-s nro_cta_cred       packed(9:0);
    dcl-s estado_ODE_activa  char(1);
    dcl-s error              char(50);
    dcl-s cant_procesados    packed(15:0);
    dcl-s nro_cel_val        char(15) ;
    dcl-s likmtr_val         packed(1:0) ;
    // Variables para calcular la fecha vencimiento
    dcl-s dias               packed(15:0) INZ(*zeros);
    dcl-s modo               char(2) INZ(*blanks);
    dcl-s fecha_vencimiento  packed(8:0);
    //variables para formatear Strings
    dcl-c up              'ABCDEFGHIJKLMNOPQRSTUVWXYZ'        ;
    dcl-c lo              'abcdefghijklmnopqrstuvwxyz'        ;
    dcl-c Symbols         '|°¬!"#$%&/()=?\¡¿*+~¢]{}_-;,:.<>'  ;
    dcl-c SymBlanks       '                                '  ;
    dcl-c Acentos         'ñÑáéíóúäëïöüãõàèìòùâêîôû@'         ;
    dcl-c AceBlanks       'nNAEIOUAEIOUAOAEIOUAEIOU '         ;
    dcl-c Apos            ''''                                ;
    dcl-c APosBlank       ' '                                 ;
// Variables para validar cuenta.
    dcl-s sucursal           packed(5:0);
    dcl-s cuenta             packed(9:0);
    dcl-s fecha_baja         packed(8:0);
    dcl-s cod_bloq           packed(2:0);
    dcl-s inmov              char(1);
    dcl-s grupo              char(2);
    dcl-s saldo_op           packed(15:2);
    dcl-s saldo_mayor        packed(15:2);
    dcl-s grupo_deb          char(2);
    dcl-s moneda_deb         packed(9:0);
    dcl-s estado_ODE         char(1);
    dcl-s desc_error         char(50);
// variables para la seccion EMPRESA
    dcl-s tipo_inscrip       packed(2:0)             INZ(80);
    dcl-s nro_inscrip        packed(15:0)            INZ(30671859339);
    dcl-s cuit_empresa       packed(12:0)            INZ(30671859339);
    dcl-s tarjeta_virt       packed(16:0);
// variables para la seccion ODE
    dcl-s tipo_documento     packed(2:0) ;
    dcl-s tipo_documento_c   packed(2:0) ;
    dcl-s nro_documento      packed(15:0);
    dcl-s cuit_cliente       char(15)    ;
    dcl-s nombre_cliente     char(30) ;
    dcl-s fecha_creacion     packed(8:0) ;
    dcl-s hora_creacion      packed(6:0) ;
    dcl-s saldo_total_deb    packed(15:2);
// variavles para la seccion CUENTA
    dcl-s tipo_cuenta        char(2)     ;
    dcl-s sucursal_cuenta    packed(5:0) ;
    dcl-s nro_cuenta         packed(9:0) ;
    dcl-s cuenta_link        char(19);
// Variables para la seccion CONTACTOS
    dcl-s tipo_contacto      char(1)     ;
    dcl-s mail               char(50)    ;
    dcl-s empresa_tel        packed(1:0) ;
    dcl-s nro_cel            char(15)    ;
// Variables para la seccion de ALTA
    dcl-s url_foto_selfi      char(255)  ;
    dcl-s url_foto_frente_dni char(255)  ;
    dcl-s url_foto_atras_dni  char(255)  ;
// Variables para seccion DIRECCION
    dcl-s codigo_postal       packed(5:0);
    dcl-s calle               char(30);
    dcl-s barrio              char(30);
    dcl-s nro_puerta          packed(6:0);
// Variables para el archivo temporal
    dcl-s CAMPO1             char(200)INZ('    ');
    dcl-s CAMPO2             char(200)INZ('    ');
    dcl-s CAMPO3             char(200)INZ('    ');
    dcl-s CAMPO4             char(200)INZ('    ');
    dcl-s CAMPO5             char(200)INZ('    ');
    dcl-s CAMPO6             char(200)INZ('    ');
    dcl-s CAMPO7             char(200)INZ('    ');
    dcl-s CAMPO8             char(200)INZ('    ');
    dcl-s CAMPO9             char(200)INZ('    ');
    dcl-s CAMPO10            char(200)INZ('    ');
    dcl-s CAMPO11            char(200)INZ('    ');
    dcl-s CAMPO12            char(200)INZ('    ');
    dcl-s CAMPO13            char(200)INZ('    ');
    dcl-s CAMPO14            char(200)INZ('    ');
    dcl-s CAMPO15            char(200)INZ('    ');
//... Entrada de parametros codigos de debito y credito para AC y CC
    dcl-ds codigos qualified;
       ac_deb           packed(3:0);
       ac_cred          packed(3:0);
       cc_deb           packed(3:0);
       cc_cred          packed(3:0);
    end-ds;
//... Parametros dinamicos - BADIPA
    dcl-ds parametros qualified;
       ac_debi          char(3);
       cc_credi         char(3);
       sucursal         char(5);
       cuenta           char(9);
       cuenta_link      char(19);
       tar_virt         char(16);
       saldo_fijo       char(15);
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

// Llamada al programa para debitar/acreditar cuenta corriente  (SBCCMOVI)
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

// Llamada al programa 'LITG40RG'
   dcl-pr call_LITG40RG extpgm('LITG40RG');
        fecha                   packed(8:0) CONST;
        errores                 char(50);
    end-pr;

    dcl-pr mostrar_mensaje extpgm('BAER00RS');
        renglon_1               char(55) CONST ;
        renglon_2               char(55) CONST ;
        renglon_3               char(55) CONST ;
        renglon_4               char(55) CONST ;
        renglon_5               char(55) CONST ;
        renglon_6               char(55) CONST ;
        renglon_7               char(55) CONST ;
        renglon_8               char(55) CONST ;
    end-pr;
//************************DESCRIPCION DEL PROGRAMA****************************//
EXSR inicializo_variables;
EXSR creo_tablas_temporales;
EXSR dir_list_OPEN;
EXSR dir_list_fetch;
cant_procesados = *zero;
  DOW SQLcode = *zero;
     EXSR armo_direccion;
     EXSR limpia_tablas_temporales;
     EXSR importar_archivo;
     IF importar_ok;
        cant_procesados = cant_procesados + 1;
        EXSR insertar_datos;
        EXSR escribo_liserj;
        EXSR mover_archivo_procesados;
     ENDIF;
     EXSR dir_list_fetch;
  ENDDO;
EXSR dir_list_CLOSE;

if cant_procesados > *zero;
    call_LITG40RG(fecha: error);
    mostrar_mensaje('Archivo ODE':
                    '':
                    error:
                    '':
                    '':
                    '':
                    '':
                    '');
else;
        mostrar_mensaje('AVISO:':
                    '':
                    'No se encontraror archivos para procesar':
                    '':
                    '':
                    '':
                    '':
                    '');

endif;

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
     //Recupero codigo debito para cuenta del el cliente - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.ac_debi from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/CODDEBCLT');
     //Recupero codigo credito para cuenta pozo del banco - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.cc_credi from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/CODCREDPOZ');
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
     //Recupero cuenta de pozo del banco en formato LINK - BADIPA.
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.cuenta_link from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/CTALINKFOR');
     //Recupero nro de tarjeta virutal - BADIPA.
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.tar_virt from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/NROTARVIRT');
     //Recupero saldo fijo para extraer - BADIPA
     EXEC SQL select
               TRIM(DPVALU)
               into :parametros.saldo_fijo from BADIPA
               WHERE UPPER(TRIM(DPCLAV)) = UPPER('ODE/SALFIJEXTR');
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
      codigos.ac_deb   =%DEC(parametros.ac_debi   :3:0);
      codigos.cc_cred  =%DEC(parametros.cc_credi  :3:0);
      suc_cta_cred     =%DEC(parametros.sucursal  :5:0);
      nro_cta_cred     =%DEC(parametros.cuenta    :9:0);
      cuenta_link      = parametros.cuenta_link;
      tarjeta_virt     =%DEC(parametros.tar_virt  :16:0);
      saldo_fijo       =%DEC(parametros.saldo_fijo:15:2);

      hora   = %dec(%time():*HMS);
      saldo_op = 0;
      estado_ODE = '0';
      desc_error = ' ' ;
    ENDSR;
//==============================================================================
  // Limpio tablas temporales a trabajar
//==============================================================================
   BEGSR importar_archivo;
     importar_ok = *off;

     //Cambio ccsid a 1208 del archivo .csv
     cmd =       'CHGATR OBJ('''+%Trim(full_path)+''') ' +
                  'ATR(*CCSID) VALUE(1208)';
     rc = execmd(cmd);

     rc = exeCmd( 'CPYFRMIMPF FROMSTMF('''+%Trim(full_path)+''')         ' +
                '  TOFILE(QTEMP/ODEFOR)                                  ' +
                '  MBROPT(*REPLACE)                                      ' +
                '  RCDDLM(*LF)                                           ' +
                '  FLDDLM('','')                                         ' );

     IF rc = 'CPF0000' ;
      importar_ok = *on;
     ELSE;
     codigo_error = '2';
     EXSR fin_de_programa;
     ENDIF;
   ENDSR;


//==============================================================================
  // Asigo valores de las variables del archivo temporal e hago insert a LIMODE
//==============================================================================
  BEGSR insertar_datos;

   EXSR temp_odeFOR_OPEN;
   EXSR temp_odeFOR_fetch;

    DOW SQLCODE = *zero;
      //pregunto si el primer registro es la cabecera, si lo es salgo y leo sig
        IF %trim(CAMPO1) = 'Marca temporal';
           EXSR temp_odeFOR_fetch;
           ITER;
        ENDIF;

      EXSR inicializo_variables;
      EXSR recupero_nro_rrn;
     //Asigno variables.
      EXSR asignar_valor_a_variables;
     //pregunto si tiene una ODE activa.
      EXSR valido_ODE_activa;
        IF ode_activa_ok;
         EXSR valido_LIKMTR;
          IF ode_likmtr_ok;
               EXSR valido_ODE_celular;
                 IF ode_celular_ok;
                    //Obtengo y valido cuenta cliente.
                    EXSR obtener_nro_cta;
                    //Calculo saldo a debitar y realizo debito y credito
                    IF cuenta_ok;
                        EXSR calcular_saldo_debito;
                        EXSR realizar_deb_cred;
                    ENDIF;
               ENDIF;
           ENDIF;
        ENDIF;
      //Insert en LIMODE
      EXSR escribir_LIMODE;
      EXSR temp_odeFOR_fetch;
      ENDDO;

   EXSR temp_odeFOR_CLOSE;

  ENDSR ;

//=========================================================================
  // Valido el nro de celular del cliente.
//=========================================================================
   BEGSR valido_ODE_celular;

       ode_celular_ok = *on;
       nro_cel_val = ' ';
       EXEC SQL
              SELECT
                IFNULL( TGCTEL, '0')
                INTO :nro_cel_val
              FROM
                 LIMODE
              WHERE
                 TGONDO = :nro_documento AND
                 TGSSTA = '5'
              ORDER BY TGAFEC ASC
              LIMIT 1;

       if %trim(nro_cel_val) <> *blanks;
          if %trim(nro_cel_val) <> %trim(nro_cel);
               ode_celular_ok = *off;
          endif;
       endif;

       IF ode_celular_ok = *off;
          sucursal_cuenta = 0;
          nro_cuenta = 0;
          tipo_cuenta = ' ';
          estado_ODE = '1';
          saldo_op = 0;
          saldo_fijo= 0;
          desc_error = 'El cel es distinto a los ya cargados en el sistema';
       ENDIF;
   ENDSR ;

//=========================================================================
  // Valido que el cliente tenga para el perido actual una tar gen
//=========================================================================
   BEGSR valido_LIKMTR;

   ode_LIKMTR_ok = *on;
   EXEC SQL
          SELECT
            COUNT(*)
            INTO :likmtr_val
           FROM
               LIKMTR
           WHERE
               TAIETA <> 'E'               AND
              (TAFALT >= 20200101          OR
               TAFACR >= 20200101)         AND
               TAFBAJ = 0                  AND
               TAITDO = :tipo_documento_c  AND
               TAINDO = :nro_documento
           LIMIT 1    ;

   if likmtr_val = 0;
       ode_LIKMTR_ok = *off;
       sucursal_cuenta = 0;
       nro_cuenta = 0;
       tipo_cuenta = ' ';
       estado_ODE = '1';
       saldo_op = 0;
       saldo_fijo= 0;
       desc_error = 'El cliente ya tiene tarjeta de débito habilitada';
   endif;

   ENDSR ;

//=========================================================================
  // Valido si tiene una ODE activa
//=========================================================================
   BEGSR valido_ODE_activa ;

   ode_activa_ok = *on;
   EXSR valida_limode_OPEN;
   EXSR valida_limode_fetch;
   DOW SQLCODE = *ZEROS;
     EXSR calculo_fecha_vencimiento;
     SELECT;
     WHEN estado_ode_activa = '0' OR
          estado_ode_activa = '2';
        ode_activa_ok = *off;
     WHEN estado_ode_activa = '4';
        IF fecha_vencimiento >= fecha;
           ode_activa_ok = *off;
        ENDIF;
     ENDSL;

   IF ode_activa_ok = *off;
      sucursal_cuenta = 0;
      nro_cuenta = 0;
      tipo_cuenta = ' ';
      estado_ODE = '1';
      saldo_op = 0;
      saldo_fijo= 0;
      desc_error = 'El Cliente ya tiene una ODE ACTIVA';
      LEAVE;
   ENDIF;

   EXSR valida_limode_fetch;
   ENDDO;
   EXSR valida_limode_CLOSE;
   ENDSR ;

//==============================================================================
  // Calculo fecha vencimiento
//==============================================================================
 BEGSR calculo_fecha_vencimiento;
     //Valido feriados, sabado o domigos
        dias = *zeros;
        modo = 'IN';
        callSBBABFE1(fecha_vencimiento:
                     dias:
                     modo);
  ENDSR;

//=========================================================================
  // Escribo LIMODE
//=========================================================================
   BEGSR escribir_LIMODE ;

        exec sql INSERT INTO LIMODE
                           (TGIRRN, --Número de registro
                            TGITIN, --Empresa-Tipo de Inscripción
                            TGININ, --Empresa-Número de Inscripcó
                            TGNTAR, --Empresa-Tarjeta o Tarjeta V
                            TGNCUI, --Empresa-CUIT de La empresa
                            TGOIDE, --ODE-Nro de Sol.= ID. ODE
                            TGOTDO, --ODE-Tipo de Documento
                            TGONDO, --ODE-Número de documento
                            TGOCUI, --ODE-Número de CUIT del clie
                            TGONCL, --ODE-Nombre
                            TGOIMP, --ODE-Importe
                            TGOSOP, --ODE-Saldo Operativo antes d
                            TGOFEC, --ODE-Fecha de Creación
                            TGOHOR, --ODE-Hora de Creación
                            TGTSUB, --Cuenta-Tipo
                            TGTSUC, --Cuenta-Sucursal
                            TGTCCL, --Cuenta-Nro de Cta Clte
                            TGTPBF, --Cuenta-Formato PBF
                            TGCCON, --Contacto-Tipo
                            TGCOEL, --Contacto-Correo Electrónico
                            TGCCIT, --Contacto-Cia de telefonía m
                            TGCTEL, --Contacto-Nro de Celular
                            TGAFEC, --Alta-Fecha
                            TGAHOR, --Alta-Hora
                            TGAUSR, --Alta-Usuario
                            TGAIM1, --Alta-URL Selfie
                            TGAIM2, --Alta-URL Frente DNI
                            TGAIM3, --Alta-URL Back DNI
                            TGICPO, --Dire-Cod.Postal
                            TGNCAL, --Calle
                            TGDAIB, --Barrio
                            TGIPUE, --Nro de puerta
                            TGSSTA, --Estado de la ODE
                            TGSDES) --Descripcion Error

                     VALUES(
                           :nro_rrn             ,
                           :tipo_inscrip        ,
                           :nro_inscrip         ,
                           :tarjeta_virt        ,
                           :cuit_empresa        ,
                           :nro_rrn             ,
                           :tipo_documento      ,
                           :nro_documento       ,
                           :cuit_cliente        ,
                           :nombre_cliente      ,
                           :saldo_fijo          ,
                           :saldo_op            ,
                           :fecha_creacion      ,
                           :hora_creacion       ,
                           :tipo_cuenta         ,
                           :sucursal_cuenta     ,
                           :nro_cuenta          ,
                           :cuenta_link         ,
                           :tipo_contacto       ,
                           :mail                ,
                           :empresa_tel         ,
                           :nro_cel             ,
                           :fecha               ,
                           :hora                ,
                           :usuario             ,
                           :url_foto_selfi      ,
                           :url_foto_frente_dni ,
                           :url_foto_atras_dni  ,
                           :codigo_postal       ,
                           :calle               ,
                           :barrio              ,
                           :nro_puerta          ,
                           :estado_ODE          ,
                           :desc_error          );

   IF SQLCODE <> *ZEROS;
   codigo_error = '3';
   EXSR fin_de_programa;
   ENDIF ;

   ENDSR ;

//=========================================================================
  // Asigno valor a la variables restantes para escribir el LIMODE
//=========================================================================
   BEGSR asignar_valor_a_variables ;

  //Seccion ODE
      //para consulta de cuenta
      SELECT;
      WHEN %TRIM(CAMPO2) = 'DNI';
         tipo_documento_c = 96 ;
      WHEN %TRIM(CAMPO2) = 'PAS';
         tipo_documento_c = 94;
      WHEN %TRIM(CAMPO2) = 'LC';
         tipo_documento_c = 90;
      WHEN %TRIM(CAMPO2) = 'LE';
         tipo_documento_c = 89;
      ENDSL;

      //para cargar en limode
      SELECT;
      WHEN %TRIM(CAMPO2) = 'DNI';
         tipo_documento = 01 ;
      WHEN %TRIM(CAMPO2) = 'PAS';
         tipo_documento = 04;
      WHEN %TRIM(CAMPO2) = 'LC';
         tipo_documento = 03;
      WHEN %TRIM(CAMPO2) = 'LE';
         tipo_documento = 02;
      ENDSL;
      nro_documento  = %DEC(%TRIM(CAMPO3):15:0);
      cuit_cliente   = %TRIM(CAMPO4);
      //formateo nombre por caracteres especiales.
      nombre_cliente = %TRIM(CAMPO5) +' '+  %TRIM(CAMPO6);
      nombre_cliente = %XLATE(Symbols:SymBlanks:nombre_cliente);
      nombre_cliente = %XLATE(Acentos:AceBlanks:nombre_cliente);
      nombre_cliente = %XLATE(Apos:AposBlank:nombre_cliente);
      nombre_cliente = %XLATE(lo:up:nombre_cliente);
      fecha_creacion = %DEC(%SUBST(CAMPO1:1:4) +
                            %SUBST(CAMPO1:6:2) +
                            %SUBST(CAMPO1:9:2):8:0);
      hora_creacion  = %DEC(%ScanRpl(':':'':%SUBST(CAMPO1:12:8)):6:0);
   //Seccion CONTACTOS
      mail  = %TRIM(CAMPO11);
      SELECT;
      WHEN %TRIM(CAMPO12) = 'Movistar';
         empresa_tel = 1;
      WHEN %TRIM(CAMPO12) = 'Personal';
         empresa_tel = 2;
      WHEN %TRIM(CAMPO12) = 'Claro';
         empresa_tel = 3;
      ENDSL;
      nro_cel = %TRIM(CAMPO13);
      tipo_contacto = '1' ;

    //Seccion ALTA
      url_foto_selfi      = %TRIM(CAMPO14);
      url_foto_frente_dni = %TRIM(CAMPO15);
      url_foto_atras_dni  = ' ';

    //Seccion Direccion
      //formateo
      calle         = %TRIM(CAMPO7);
      calle         = %XLATE(Symbols:SymBlanks:calle);
      calle         = %XLATE(Acentos:AceBlanks:calle);
      calle         = %XLATE(Apos:AposBlank:calle);
      calle         = %XLATE(lo:up:calle);
      nro_puerta    = %DEC(%TRIM(%SUBST(CAMPO8:1:6)):6:0);
      //formateo
      barrio        = %TRIM(CAMPO9);
      barrio        = %XLATE(Symbols:SymBlanks:barrio);
      barrio        = %XLATE(Acentos:AceBlanks:barrio);
      barrio        = %XLATE(Apos:AposBlank:barrio);
      barrio        = %XLATE(lo:up:barrio);
      codigo_postal = %DEC(%TRIM(%SUBST(CAMPO10:1:5)):5:0);
      ENDSR;

//=========================================================================
  // Realizo debitos y creditos a las cuentas correspondientes
//=========================================================================
   BEGSR realizar_deb_cred;

      //Debito a cuenta cliente
      callSBACMOVB(sucursal_cuenta: //Sucural cuenta cliente
                   nro_cuenta:      //Cuenta Cliente 'AC'
                   fecha:           //Fecha alta
                   hora:            //Hora alta
                   moneda_deb:      //Moneda
                   codigos.ac_deb:  //Codigo debito Cuenta Cliente
                   *zeros:          //Sucursal Alta
                   nro_caja:        //Nro de caja (556)
                   saldo_total_deb: //Importe a debitar
                   fecha:           //Fecha Asiento
                   *zeros:          //Movimiento Contrasentado
                   *zeros:          //Fecha Movimiento Contrasentado
                   *blanks:         //Usuario
                   *blanks:         //Usuario
                   nro_rrn:         //Nro de Cheque
                   codigos.ac_deb); //Codigo

     //Credito a cuenta de fondos del banco
     callSBCCMOVB (suc_cta_cred:     //Sucursal Cuenta Banco
                   nro_cta_cred:     //Nro cuenta del Banco 'CC'
                   fecha:            //Fecha Alta
                   hora:             //Hora alta
                   1:                //Moneda
                   codigos.cc_cred:  //Codigo Credito
                   *ZEROS:           //Sucursal Alta
                   nro_caja:         //Nro de caja (556)
                   saldo_fijo:       //Importe a acreditar
                   fecha:            //Fecha Asiento
                   *ZEROS:           //Asiento contrasentado
                   *ZEROS:           //Fecha asiento contrasenadd
                   *BLANKS:          //Usuario
                   *BLANKS:          //Usuario
                   nro_rrn:          //Nro de cheque
                   codigos.cc_cred  );

   ENDSR ;

//=========================================================================
  // Calculo el saldo a debitar
//=========================================================================
   BEGSR calcular_saldo_debito ;
      SELECT;
      //Saldo sufuciente para monto extraccion fijo
      WHEN saldo_op >= saldo_fijo;
        saldo_total_deb = saldo_fijo;
      //Saldo insuficiente para monto de extraccion fijo
      WHEN saldo_op < saldo_fijo;
        saldo_sin_dec = saldo_op / 1000;
        saldo_total_deb = saldo_sin_dec * 1000;
        saldo_fijo = saldo_total_deb;
      ENDSL;
   ENDSR ;

//=========================================================================
  // Obtengo numero de cuenta
//=========================================================================

  BEGSR obtener_nro_cta;

  cuenta_ok = *off;
  saldo_mayor = 0;
  EXSR cuenta_badccl_OPEN;
  EXSR cuenta_badccl_fetch;

  DOW SQLCODE = *ZERO;
  cuenta_ok = *on;

  SELECT;
  WHEN fecha_baja <> 0;
  cuenta_ok = *off;
  EXSR cuenta_badccl_fetch;
  ITER;

  WHEN cod_bloq <> 0;
  cuenta_ok = *off;
  EXSR cuenta_badccl_fetch;
  ITER;

  WHEN inmov = 'I';
  cuenta_ok = *off;
  EXSR cuenta_badccl_fetch;
  ITER;

  WHEN saldo_op < 1000;
  cuenta_ok = *off;
  EXSR cuenta_badccl_fetch;
  ITER;

  ENDSL;

  //Si la cuenta no tiene ningun error, asigno y guardo saldo mayor
  saldo_mayor = saldo_op;
  sucursal_cuenta = sucursal;
  nro_cuenta = cuenta;
  tipo_cuenta = 'AC';
  grupo_deb = grupo;
  desc_error = ' ';
  estado_ODE = '0';
  LEAVE;
  EXSR cuenta_badccl_fetch;
  ENDDO;

  //Si no encuentra cuenta valida o no posee, carga error
  IF cuenta_ok = *off;
      sucursal_cuenta = 0;
      nro_cuenta = 0;
      tipo_cuenta = ' ';
      estado_ODE = '1';
      saldo_op = 0;
      saldo_fijo = 0;
      desc_error = 'El Cliente no tiene Cuenta o no posee una valida';
  ENDIF;

  EXSR cuenta_badccl_CLOSE;

  ENDSR;

//=========================================================================
  // Declaro cursos y hago open para consultar la cuentas del cliente.
//=========================================================================
 BEGSR cuenta_badccl_OPEN;
   EXEC SQL DECLARE cue CURSOR FOR
           SELECT
           OTISUC,
           OTICCL,
           FUFBAJ,
           FUIBAC,
           FUIINM,
           FUIGRC,
           FU$SOP,
           FUIMON
        FROM BADCCL
        INNER JOIN ACCTAC ON OTICCL = FUICAH AND
                             OTISUC = FUISUC
        INNER JOIN BAICCL ON OTICCL = OSICCL AND
                             OTISUC = OSISUC AND
                             OSITIN = 0      AND
                             OSININ = 0
        WHERE OTITDO = :tipo_documento_c AND
              OTINDO = :nro_documento    AND
              OTITTL = 1                 AND
              FUIMON = 1                 AND
              FUISGC <> 'CE' ;
       EXEC SQL OPEN cue;
   ENDSR ;


//=========================================================================
  // Ejecuto consulta para traer las cuentas del cliente
//=========================================================================
   BEGSR cuenta_badccl_fetch ;
   EXEC SQL FETCH cue into :sucursal, :cuenta, :fecha_baja,
                           :cod_bloq, :inmov,  :grupo, :saldo_op,
                           :moneda_deb;
   ENDSR ;

//=========================================================================
  // Cierro curso ODE
//=========================================================================
   BEGSR cuenta_badccl_CLOSE;
   EXEC SQL CLOSE cue;
   ENDSR ;

//=========================================================================
  // Declaro cursos y hago open para consulta LIMODE ode activa
//=========================================================================
 BEGSR valida_limode_OPEN;
   EXEC SQL DECLARE ode_act CURSOR FOR
           SELECT
           TGSSTA,
           TGOFVT
        FROM LIMODE
        WHERE TGONDO = :nro_documento;
       EXEC SQL OPEN ode_act;
   ENDSR ;


//=========================================================================
  // Ejecuto consulta para traer las cuentas del cliente
//=========================================================================
   BEGSR valida_limode_fetch ;
   EXEC SQL FETCH ode_act into :estado_ode_activa,
                               :fecha_vencimiento;
   ENDSR ;

//=========================================================================
  // Cierro curso ODE
//=========================================================================
   BEGSR valida_limode_CLOSE;
   EXEC SQL CLOSE ode_act;
   ENDSR ;
//=========================================================================
  // Declaro y abro CURSOR para traer todos los campos del temporal
//=========================================================================
 BEGSR temp_odeFOR_OPEN;
   EXEC SQL DECLARE ode CURSOR FOR
      SELECT CAMPO1,
             CAMPO2,
             CAMPO3,
             CAMPO4,
             CAMPO5,
             CAMPO6,
             CAMPO7,
             CAMPO8,
             CAMPO9,
             CAMPO10,
             CAMPO11,
             CAMPO12,
             CAMPO13,
             CAMPO14,
             CAMPO15
       FROM QTEMP/ODEFOR;
   EXEC SQL OPEN ode;
   ENDSR ;

//=========================================================================
  // Ejecuto consulta para traer los datos del temporal
//=========================================================================
   BEGSR temp_odeFOR_fetch ;
   EXEC SQL FETCH ode into :CAMPO1,  :CAMPO2,  :CAMPO3,  :CAMPO4,  :CAMPO5,
                           :CAMPO6,  :CAMPO7,  :CAMPO8,  :CAMPO9,  :CAMPO10,
                           :CAMPO11, :CAMPO12, :CAMPO13, :CAMPO14, :CAMPO15;
   ENDSR ;

//=========================================================================
  // Cierro curso ODE
//=========================================================================
   BEGSR temp_odeFOR_CLOSE;
   EXEC SQL CLOSE ode;
   ENDSR ;

//=========================================================================
  // Recupero nro de rrn
//=========================================================================
   BEGSR recupero_nro_rrn;

   //Llamo programa para insertar numero de rrn
    callBAGETJOB(FileName:
                 FieldName);
    //declaro CURSOR jobs
    EXEC SQL DECLARE jobs CURSOR FOR
     SELECT    WNIULN
     FROM      BANUME
     WHERE
               WNIPF1 = 'BAGETJOB'
          AND  WNIPF2 = :FileName
          AND  WNIPF3 = :FieldName;

    //Ejecuto consulta para poder recuperar numero rrn
    EXEC SQL OPEN jobs;
    EXEC SQL FETCH jobs INTO :nro_rrn;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
      EXSR fin_de_programa;
     ENDIF;

     EXEC SQL CLOSE jobs;
   ENDSR;

//==============================================================================
  // Limpio las tablas temporales
//==============================================================================
    BEGSR creo_tablas_temporales;
     EXEC SQL DROP TABLE QTEMP/ODEFOR;
     EXEC SQL
            CREATE TABLE QTEMP/ODEFOR (
                CAMPO1                         CHAR (200),
                CAMPO2                         CHAR (200),
                CAMPO3                         CHAR (200),
                CAMPO4                         CHAR (200),
                CAMPO5                         CHAR (200),
                CAMPO6                         CHAR (200),
                CAMPO7                         CHAR (200),
                CAMPO8                         CHAR (200),
                CAMPO9                         CHAR (200),
                CAMPO10                        CHAR (200),
                CAMPO11                        CHAR (200),
                CAMPO12                        CHAR (200),
                CAMPO13                        CHAR (200),
                CAMPO14                        CHAR (200),
                CAMPO15                        CHAR (200));
    ENDSR;

//==============================================================================
  // Declaro los CURSOR es y hago OPEN al CURSOR
//==============================================================================
    BEGSR dir_list_OPEN;
     //Consulta para saber si exite archivos de extracto
      EXEC SQL DECLARE C1 CURSOR FOR
      SELECT
            CAST (FILENAME as char(255)) AS FILNAM
      FROM  TABLE(IFSDIR(:carpeta)) AS T
      WHERE UPPER(CAST (FILENAME as char(255))) like '%ODE%'
      ORDER BY CAST (FILENAME as char(255)) ASC;

      EXEC SQL OPEN C1;
    ENDSR;

//==============================================================================
  //  ejecuto contulsta y guardo nombre archivo
//==============================================================================
   BEGSR dir_list_fetch;
    EXEC SQL FETCH C1 into :nombre_archivo;
   ENDSR;

//==============================================================================
  //  Armo direccion del archivo a importar y direccion de destino
//==============================================================================
    BEGSR armo_direccion;
     path_destino = %Trim(carpeta_destino)  +
                    '/'                     +
                    %Trim(nombre_archivo)   +
                    '_'                     +
                    %char(fecha)            +
                    '_'                     +
                    %char(hora);

      full_path = %trim(carpeta) +'/'+ %trim(nombre_archivo);
    ENDSR;

//==============================================================================
  // Limpio tablas temporales a trabajar
//==============================================================================
    BEGSR limpia_tablas_temporales;
     EXEC SQL DELETE FROM QTEMP/ODEFOR;
    ENDSR;

//==============================================================================
  // Escribo LISERJ
//==============================================================================
    BEGSR escribo_liserj;
       EXEC SQL INSERT INTO LISERJ
                           (RJDACO,
                            RJFALT,
                            RJIUSR,
                            RJHORA)
                     VALUES(
                           :nombre_archivo,
                           :fecha,
                           :usuario,
                           :hora);
    ENDSR;

//==============================================================================
  // Muevo archivo a procesados
//==============================================================================
    BEGSR mover_archivo_procesados;
     ejecutar_programa_SGAUTAC4(%trim(full_path))   ;

     cmd= 'MOV  OBJ('''  +%Trim(full_path)   +''')     '+
          '     TOOBJ('''+%Trim(path_destino)+''')     ';
     rc=exeCmd(cmd);
    ENDSR;

//==============================================================================
  // Muevo archivo a procesados
//==============================================================================
    BEGSR dir_list_CLOSE;
        EXEC SQL CLOSE C1;
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

    //Envio mail por si no se pudo insertar el contenido al temp
    IF codigo_error  = '2';
      cmd = 'SNDMAIL  RECP('''+usuario+''') '                          +
            'SUBJECT(''ODE: Error al insertar registro'') '            +
            'MESG(''Se produjo un error al quere hacer CPYFRMIMPF '    +
            'al archivo temporal. VerIFique y reintente'') '           ;
      exeCmd(cmd);
      RETURN;
    ENDIF;

    //Envio mail por error si no se pudo insertar en LIMODE
    IF codigo_error  = '3';
      cmd = 'SNDMAIL  RECP('''+usuario+''') '                          +
            'SUBJECT(''ODE: Error al insertar registro en LIMODE'') '  +
            'MESG(''Se produjo un error al quere hacer insert en '     +
            'el archivo LIMODE. Verifique y reintente'') '             ;
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
