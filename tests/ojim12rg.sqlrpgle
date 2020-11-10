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

    dcl-pr ojex00cl extpgm('OJEX00CL');
            chfech  char(8) const;
    end-pr;

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
    dcl-s fd                int(10);
    dcl-s cr                int(10);
    dcl-s line              varchar(MAX_FILE_SIZE);
    dcl-s cols              varchar(MAX_FILE_SIZE) dim(27);
    dcl-s col               varchar(MAX_FILE_SIZE);
    dcl-s i                 int(10);
    dcl-s j                 int(10);
    dcl-s ch                char(1);

    //Campos a mandar al WSSOJ0R2
    dcl-s  carpeta          char(255) inz('/home/SOJ/Novedades/');
    dcl-s tipo_oficio       char(1);
    dcl-s nro_expediente    char(15);
    dcl-s CodigoJuzgado     char(5);
    dcl-s nombreJuez        char(30);
    dcl-s capital           char(15);
    dcl-s interes           char(15);
    dcl-s DependenciaAfip   char(4);
    dcl-s informacionSolic  char(50);
    dcl-s nroOficioLevant   char(11);
    dcl-s realizaTrans      char(1);
    dcl-s codigoVigencia    char(1);
    dcl-s codigoRespuesta   char(1);
    dcl-s personaJuridica   char(1);
    dcl-s tipoDocumento     char(2) inz('80');
    dcl-s origenDatos       char(1) inz('2');
    dcl-s IdOrGral          char(2);
    dcl-s codigo_error      char(1) inz('0');
    dcl-s nro_oficio        char(11);
    dcl-s nro_cuit          char(13);
    dcl-s razon_social      char(17);
    dcl-s transfBanco       char(30);
    dcl-s transfSucursal    char(20);
    dcl-s transfCuenta      char(11);
    dcl-s dniPersona        packed(15:0);
    dcl-s tipo_cuenta       char(2);
    dcl-s moneda            char(1);
    dcl-s cotizacion        char(15);
    dcl-s  nro_jobs         char(6);
    dcl-s  nro_iche         char(7);
    dcl-s  importe          char(15);
    dcl-s  legAgente        char(8);
    dcl-s  cuenta           char(11);
    dcl-s  sucursal         char(5);
    dcl-s  cuitSinGuiones   char(11);
    dcl-s  fecha_oficio     char(8);
    dcl-s  fecha_alta_int   char(8);


// Otras variable
    dcl-s  WWCAPI      packed(15:2);
    dcl-s  WWINTE      packed(15:2);
    dcl-s  FileName    char(10) inz('BAWSSJ');
    dcl-s  FieldName   char(10) inz('U1IJOB');
    dcl-s  WW$COR      packed(15:2);
    dcl-s  cuitDec     packed(11:0);
    dcl-s  hora        packed(6:0);
    dcl-s  WW$SAL      packed(15:2);
    dcl-s  WWINUC      packed(11:0);


//... Entrada de parametros para cuenta y sucursal
    dcl-ds datos_cuenta qualified;
       nro_sucursal     packed(5:0);
       nro_cuenta       packed(15:0);
       saldo_operativo  packed(15:2);
       moneda_cuenta    packed(1:0);
    end-ds;

//... Llamada al programa 'BAGETJOB'
   dcl-pr callBAGETJOB extpgm('DESA00653/BAGETJOB');
        file      char(10) const;
        field     char(10) const;
    end-pr;

//... Llamada al programa 'WSSOJ0R2'
 dcl-pr callSRWSSOJ0R2 extpgm('DESA00684/WSSOJ0R22');
      nro_jobs          char(6)  const;
      tipo_cuenta       char(2)  const;
      nro_sucursal      char(5)  const;
      nro_cuenta        char(15) const;
      nro_iche          char(7)  const;
      importe           char(15) const;
      moneda            char(1)  const;
      cotizacion        char(15) const;
      nro_cuit          char(13) const;
      razon_social      char(17) const;
      nro_oficio        char(11) const;
      tipo_oficio       char(2)  const;
      nro_expediente    char(15) const;
      CodigoJuzgado     char(5)  const;
      nombreJuez        char(30) const;
      capital           char(15) const;
      interes           char(15) const;
      DependenciaAfip   char(4)  const;
      informacionSolic  char(50) const;
      nroOficioLevant   char(11) const;
      legAgente         char(8)  const;
      realizaTrans      char(1)  const;
      codigoVigencia    char(1)  const;
      codigoRespuesta   char(1)  const;
      personaJuridica   char(1)  const;
      tipoDocumento     char(2)  const;
      origenDatos       char(1)  const;
      IdOrGral          char(1)  const;
      fecha_oficio      char(8)  const;
      fecha_alta_int    char(8)  const;
      codigo_error      char(1)  const;
      //transfBanco char(30) const;
      //transfSucursal char(20) const;
      //transfCuenta char(15) const;
    end-pr;

    exec sql set option commit=*None;
    exsr declara_cursor;
    exsr Crear_nombre_archivo;
    exsr Esperar_archivo;
    exsr Procesar_embargo;
    exsr Procesar_levantamiento;
    exsr Generar_archivo_de_respuestas;
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
                            '_SOJ_REMANENTE.309';
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
               ' SUBJECT(''SOJ Archivo Remanentes no encontrado'') '    +
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
               ' SUBJECT(''SOJ Archivo Remanentes CANCELACION'') '      +
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
 begsr Procesar_embargo;

      EXSR abrir_archivo;

//...Lee la cabecera
      ifs_readln_dos(fd: line);


    dow  ifs_readln_dos(fd: line) > 0;

      EXSR procesar_linea;


  if cols(1) <> 'Fin de Archivo' and
     cols(3) = '1'               and
     %len(cols(5)) = 11;


         codigo_error = '0';
         EXSR asignarVariables;
         EXSR validaCliente;

  if personaJuridica = 'F';

         EXSR validaCuentaF;

  else;

         EXSR validaCuentaJ;

  endif;

      if codigo_error = '0';

         EXSR verificaCotizacion;
         cotizacion = %char(WW$COR);
         cotizacion = %Editc ( %Dec ( %Trim ( cotizacion ) :15:2)  : 'X' );
         moneda     = %char(datos_cuenta.moneda_cuenta);
         moneda     = %Editc ( %Dec ( %Trim ( moneda ) :1:0)  : 'X' );
         sucursal = %char(datos_cuenta.nro_sucursal);
         cuenta   = %char(datos_cuenta.nro_cuenta);
         sucursal = %Editc ( %Dec ( %Trim ( sucursal ) :5:0)  : 'X' );
         cuenta   = %Editc ( %Dec ( %Trim ( cuenta )   :11:0) : 'X' );

         EXSR tipoCuenta;
         EXSR SRBAGET;
         EXSR recuperaNroJobs;
         EXSR recuperaNroIche;

      endif;

         if codigo_error = '0' ;

             nro_jobs = %Editc ( %Dec ( %Trim ( nro_jobs ) :6:0) : 'X' );
             nro_iche = %Editc ( %Dec ( %Trim ( nro_iche ) :7:0) : 'X' );

//...realizo el debito y creo las cuentas de grupo 05
           EXSR SRWSSOJ;

         endif;
        endif;

 enddo;

   exsr cerrar_archivo;

 endsr;


 //=========================================================================
   //  Procesa el archivo (Levantamientos)
//=========================================================================
 begsr Procesar_levantamiento;

      EXSR abrir_archivo;

//...Lee la cabecera
      ifs_readln_dos(fd: line);


    dow  ifs_readln_dos(fd: line) > 0;

      EXSR procesar_linea;


//... aca pregunto si el oficio es de tipo levantamiento (3)

  if cols(1) <> 'Fin de Archivo' and
     cols(3) = '3'               and
     %len(cols(5)) = 11 ;

       codigo_error = '0';
       EXSR asignarVariables;
       WWINUC = %dec(nroOficioLevant:11:0);

           IF WWINUC > 0;

              EXSR verificaLevantamiento;
              EXSR verificaLevantamientoDUP;

             if codigo_error = '0';

               EXSR SRBAGET;
               EXSR recuperaNroJobs;
               EXSR recuperaNroIche;

               nro_jobs = %Editc ( %Dec ( %Trim ( nro_jobs ) :6:0) : 'X' );
               nro_iche = %Editc ( %Dec ( %Trim ( nro_iche ) :7:0) : 'X' );

               EXSR escribirOjmofi;
               EXSR modificaOjmofi;

             endif;

           ENDIf;

   endif;

 enddo;

   exsr cerrar_archivo;

 endsr;




//=========================================================================
   //  Procesa linea del archivo
//=========================================================================
    begsr procesar_linea;
        j = 1;
        col='';
        clear cols;
        for i = 1 to %size(%trim(line));
            ch = %subst(line: i);
            if ch = '@';
                cols(j) = col;
                j += 1;
                col='';
                iter;
            endif;
            col += ch;
        endfor;

        cols(j) = col;
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


//=========================================================================
   //  Subrutina que llama al programa que genera el archivo de respuesta
//=========================================================================

    begsr Generar_archivo_de_respuestas;

       codigo_error = '0';

      EXSR verificar_Ojmres;

       if codigo_error = '0';

        ojex00cl(chsfei);

       else;

       //...Imprimo mensaje de error en pantalla de sideba
        die('No se encontraron registros para la fecha '           +
                       chsfei                              +
                    ' en el archivo OJMRES');

       endif;

    endsr;



//======================================================
   //  Avisar a los responsables que el proceso termino
//======================================================

    begsr Avisar_fin_proceso;

    cmd1 = 'SNDGMAIL TOUSR('+%trim(notify_address)+') '                      +
               ' SUBJECT(''Aviso de fin de proceso SOJ'') '                  +
       ' BODY(''Se realizo el procesamiento del archivo remanente de SOJ y'  +
       ' se genero el archivo de respuesta en : '                            +
       '/home/SOJ/Respuestas/.  '                                            +
       ' Dicho archivo tendra que ser enviado por camara online a COELSA '   +
               ' '') '                                                       ;

    exeCmd(cmd1);


    endsr;



//=================================================
   // Subrutina que termina el programa
//=================================================

    begsr Fin_de_programa;
        return;
    endsr;


//=================================================
   //  Declaro cursores para las consultas.
//=================================================

  BEGSR declara_cursor;


//... Select al ojmofi para saber ya existe el levantamiento

 exec sql declare ojm_levan cursor for
     SELECT OJINUI as nro_oficioLevantado
    FROM   OJMOFI
    WHERE
           OJINUI = :nro_oficio;


//... Select al cotiza

 exec sql declare cotiza cursor for
    SELECT MU$COR as cotizacion
    FROM COTIZA
    WHERE
             MU$COR > 1
         AND MUIMON = 2
         AND MUIUCO = 1

   ORDER BY MUVIGE DESC
   LIMIT 1;

//... Select al bapfis o al bapjur

 exec sql declare tipoPersona cursor for
    SELECT AÑINDO as nro_cuit
    FROM   BAPFIS
    WHERE
           AÑICUI = :cuitSinGuiones

    UNION ALL

    SELECT AOININ as nro_cuit
    FROM   BAPJUR
    WHERE
           AOININ = :cuitSinGuiones;



//... Select al ojmres para verificar existencia de registros

 exec sql declare c_ojmres cursor for
    SELECT MRINUI as nro_oficio
    FROM   OJMRES
    WHERE
           MRFCIM = :aasfei;


//... Select al ojmofi para verificar el levantamiento

 exec sql declare levantamiento cursor for
    SELECT OJINUI as nro_oficioLevantado
    FROM   OJMOFI
    WHERE
           OJINUI = :WWINUC;



//... Select al badccl con un union all al acctac y ccctct para ver si la
//    cuenta es valida para persona fisica

 exec sql declare cuentaF cursor for
    SELECT
        *
    FROM (
        SELECT
            FUISUC as nro_sucursal,
            FUICAH as nro_cuenta,
            FU$SOP as saldo_operativo,
            FUIMON as moneda
        FROM BADCCL
        INNER JOIN  ACCTAC ON
                        OTISUC = FUISUC
                    AND OTICCL = FUICAH
        WHERE
                OTINDO = :dniPersona
            AND FUFBAJ=0
            AND FUIBAC NOT IN ( 2, 4, 7)
            AND FU$SOP > 0
            AND OTITTL = 1
            AND FUIGRC='01'
            AND FUISGC IN ('CE','')

        UNION ALL

        SELECT
            BMISUC  as nro_sucursal,
            BMICCC  as nro_cuenta,
            BM$SOP  as saldo_operativo,
            BMIMON as moneda
        FROM BADCCL
        INNER JOIN  CCCTCT ON
                        OTISUC = BMISUC
                    AND OTICCL = BMICCC
        WHERE
                OTINDO = :dniPersona
            AND BMFBAJ=0
            AND BMIBCC NOT IN ( 2, 4, 7)
            AND BM$SOP > 0
            AND OTITTL = 1
            AND (
                        BMIGRC='01' AND BMISGC IN ('FP','')
                    OR  BMIGRC='03' AND BMISGC IN ('FP','CE', '')
                )
    ) AS TABLA
    ORDER BY 3 DESC
    LIMIT 1;


//... Select al baIccl con un union all al acctac y ccctct para ver si la
//    cuenta es valida para persona Juridica
 exec sql declare cuentaJ cursor for
SELECT
        *
    FROM (
        SELECT
            FUISUC as nro_sucursal,
            FUICAH as nro_cuenta,
            FU$SOP as saldo_operativo,
            FUIMON as moneda
        FROM BAICCL
        INNER JOIN  ACCTAC ON
                        OSISUC = FUISUC
                    AND OSICCL = FUICAH
        WHERE
                OSININ = :dniPersona
            AND FUFBAJ=0
            AND FUIBAC NOT IN ( 2, 4, 7)
            AND FU$SOP > 0
            AND FUIGRC='01'
            AND FUISGC IN ('CE','')

        UNION ALL

        SELECT
            BMISUC  as nro_sucursal,
            BMICCC  as nro_cuenta,
            BM$SOP  as saldo_operativo,
            BMIMON  as moneda
        FROM BAICCL
        INNER JOIN  CCCTCT ON
                        OSISUC = BMISUC
                    AND OSICCL = BMICCC
        WHERE
                OSININ = :dniPersona
            AND BMFBAJ=0
            AND BMIBCC NOT IN ( 2, 4, 7)
            AND BM$SOP > 0
            AND (
                        BMIGRC='01' AND BMISGC IN ('FP','')
                    OR  BMIGRC='03' AND BMISGC IN ('FP','CE', '')
                )
    ) AS TABLA
    ORDER BY 3 DESC
    LIMIT 1 ;


//... Select para extraer el tipo de la cuenta

 exec sql declare tipo cursor for
        SELECT

        CASE WHEN
                  (SELECT FUICAH
                   FROM ACCTAC
                   WHERE

                          FUISUC = :datos_cuenta.nro_sucursal
                      AND FUICAH = :datos_cuenta.nro_cuenta )

                      IS NOT NULL THEN 'AC'

             WHEN
                  (SELECT BMICCC
                   FROM CCCTCT
                   WHERE

                          BMISUC = :datos_cuenta.nro_sucursal
                      AND BMICCC = :datos_cuenta.nro_cuenta )

                     IS NOT NULL THEN 'CC'

                  END AS tipo_cuenta

        FROM sysibm.sysdummy1;



//... Select para extraer el numero de job o el iche

 exec sql declare jobs cursor for
       SELECT    WNIULN
       FROM      BANUME
       WHERE
                 WNIPF1 = 'BAGETJOB'
            AND  WNIPF2 = :FileName
            AND  WNIPF3 = :FieldName;


  ENDSR;



//=================================================
   //  Asigna el valor a cada variable
//=================================================

 BEGSR asignarVariables;

//... Agrego los '-' para armar el cuit
         nro_cuit       =   %subst(cols(5) :1 :2) + '-'
                          + %subst(cols(5) :3 :8) + '-'
                          + %subst(cols(5) :11: 1);

         tipo_oficio      = cols(3);
         cuitSinGuiones   = cols(5);
         razon_social     = cols(6);
         nro_expediente   = cols(7);
         nombreJuez       = cols(9) ;
         transfBanco      = cols(12);
         transfSucursal   = cols(13);
         transfCuenta     = cols(14);
         informacionSolic = cols(17);
         realizaTrans     = cols(20);
         tipoDocumento    = cols(24);
         nro_oficio      =  %Editc ( %Dec ( %Trim ( cols(1)  )  :11:0) : 'X' );
         CodigoJuzgado   =  %Editc ( %Dec ( %Trim ( cols(8)  )  :5 :0) : 'X' );
         capital         =  %Editc ( %Dec ( %Trim ( cols(10) )  :15:2) : 'X' );
         Interes         =  %Editc ( %Dec ( %Trim ( cols(11) )  :15:2) : 'X' );
         DependenciaAfip =  %Editc ( %Dec ( %Trim ( cols(15) )  :4 :0) : 'X' );
         nroOficioLevant =  %Editc ( %Dec ( %Trim ( cols(18) )  :11:0) : 'X' );
         legAgente       =  %Editc ( %Dec ( %Trim ( cols(19) )  :8 :0) : 'X' );
         IdOrGral        = %Editc  ( %Dec ( %Trim ( cols(26) )  :2:0)  : 'X' );
         fecha_oficio    = %ScanRpl('/' : '' : cols(2));
         fecha_oficio    = %Editc ( %Dec ( %Trim (fecha_oficio)  :8 :0) : 'X');
         fecha_alta_int  = %ScanRpl('/' : '' : cols(16));
         fecha_alta_int  = %Editc( %Dec ( %Trim (fecha_alta_int)  :8 :0) : 'X');

         fecha_oficio   =   %subst(fecha_oficio :5 :4)    +
                            %subst(fecha_oficio :3 :2)    +
                            %subst(fecha_oficio :1 :2);

         fecha_alta_int   =   %subst(fecha_alta_int :5 :4)    +
                              %subst(fecha_alta_int :3 :2)    +
                              %subst(fecha_alta_int :1 :2);





//... Convierto el capital y el interes para sumarlos y guardarlos en importe
         WWCAPI  = %dec(cols(10):15:2);
         WWINTE  = %dec(cols(11):15:2);
         importe = %char(WWCAPI + WWINTE);
         importe = %Editc ( %Dec ( %Trim ( importe ) :15:2) : 'X' );


//... Armo el codigo de vigencia
         codigoVigencia   = '1';
         if cols(21) <> 'Actuales y Futuras';
            codigoVigencia   = '2';
         endif;

//... Armo el codigo de respuesta
         codigoRespuesta  =  '1';
         if cols(22) <> 'Solo Clientes';
         codigoRespuesta = '2';
         endif;

//... Verifico si es persona juridica o fisica
         personaJuridica  = 'F';
         if cols(23) <> '1' ;
         personaJuridica = 'J';
         endif;

   ENDSR;



//=========================================================
   //  Verifica que el oficio exista para el levantamiento
//=========================================================

    BEGSR  verificaLevantamientoDUP;

     exec sql open ojm_levan;
     exec sql FETCH ojm_levan;

     If SQLCOD = *ZERO;
      codigo_error = '1';
     endif;

     exec sql close ojm_levan;

    ENDSR;


//=========================================================
   //  Verifica que el oficio exista para el levantamiento
//=========================================================

    BEGSR  verificaLevantamiento;

     exec sql open levantamiento;
     exec sql FETCH levantamiento;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close levantamiento;

    ENDSR;



//=================================================
   //  Recupera Nuermo de Jobs
//=================================================

    BEGSR  recuperaNroJobs;

     FileName  = 'BAWSSJ';
     FieldName = 'U1IJOB';

     exec sql open jobs;
     exec sql FETCH jobs INTO :nro_jobs;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close jobs;

    ENDSR;



//=================================================
   //  Recupera Nuermo de Jobs
//=================================================

    BEGSR  verificar_Ojmres;

     exec sql open c_ojmres;
     exec sql FETCH c_ojmres;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close c_ojmres;

    ENDSR;


//=================================================
   //  Recupera Nuermo de Iche
//=================================================

    BEGSR  recuperaNroIche;

     FileName  = 'OJMOFI';
     FieldName = 'OJIRRN';

     exec sql open jobs;
     exec sql FETCH jobs INTO :nro_iche;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close jobs;

    ENDSR;

//=================================================
 //  carga las variables llamar a BAGETJOB
//=================================================-
   BEGSR SRBAGET ;
  count = 0;
  dou count >= MAX_RETRIES2;

    FileName = '  ';
    FieldName = '  ';

    if count = 0;

     FileName = 'BAWSSJ';
     FieldName = 'U1IJOB';

    else;

     FileName = 'OJMOFI';
     FieldName = 'OJIRRN';

    endif;

     callBAGETJOB(FileName:
                  FieldName);

      count += 1;
  enddo;

   ENDSR;


//=================================================
   //  Verifica la cotizacion
//=================================================

    BEGSR  verificaCotizacion;

     exec sql open cotiza;
     exec sql FETCH cotiza INTO :WW$COR;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close cotiza;

    ENDSR;


//=================================================
   //  Verifica que sea cliente
//=================================================

    BEGSR  validaCliente;

     exec sql open tipoPersona;
     exec sql FETCH tipoPersona INTO :dniPersona;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close tipoPersona;

    ENDSR;


//=================================================
   //  Verifica que la cuenta sea valida
//=================================================

    BEGSR  validaCuentaJ;

     exec sql open cuentaJ;
     exec sql FETCH cuentaJ INTO :datos_cuenta;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close cuentaJ;

    ENDSR;

//=================================================
   //  Verifica que la cuenta sea valida
//=================================================

    BEGSR  validaCuentaF;

     exec sql open cuentaF;
     exec sql FETCH cuentaF INTO :datos_cuenta;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close cuentaF;

    ENDSR;


//=================================================
   //  Recuerpera el tipo de cuenta
//=================================================

  BEGSR  tipoCuenta;

     exec sql open tipo;
     exec sql FETCH tipo INTO :tipo_cuenta;

     If SQLCOD <> *ZERO;
      codigo_error = '1';
     endif;

     exec sql close tipo;

  ENDSR;


//==============================================================
   //  Modifica el embargo que hace referencia el levantamiento
//==============================================================

   BEGSR modificaOjmofi;
    exec sql UPDATE OJMOFI set OJDF03 = 'Oficio levantado',
                               OJDF04 = :nro_oficio       ,
                               OJDF05 = :nro_iche
                        WHERE  OJINUI= :WWINUC;

   ENDSR;


 //=================================================
   //  Escriebe en OJMOFI los levantamientos
//=================================================

   BEGSR escribirOjmofi;
    cuitDec = %dec(cuitSinGuiones:11:0);
    hora   = %dec(%time():*HMS);
    WW$SAL = WWCAPI + WWINTE;


   exec sql INSERT INTO OJMOFI
                           (OJIRRN,
                            OJCOTR,
                            OJININ,
                            OJINUI,
                            OJNTDO,
                            OJIINI,
                            OJVIGE,
                            OJITRG,
                            OJNYAP,
                            OJDDOB,
                            OJCENT,
                            OJNRSO,
                            OJ$CAP,
                            OJ$INT,
                            OJISEC,
                            OJFING,
                            OJAINF,
                            OJINUC,
                            OJINDA,
                            OJLTRA,
                            OJCVIG,
                            OJCRRQ,
                            OJIINC,
                            OJIOPT,
                            OJ$SAL,
                            OJFPRE,
                            OJITDO,
                            OJINDO,
                            OJIJOB,
                            OJIUSR,
                            OJITER,
                            OJFALT,
                            OJHALT,
                            OJIAFO)

                     VALUES(
                           :nro_iche,
                           'AF',
                           :IdOrGral,
                           :nro_oficio,
                            80,
                           :cuitDec,
                           :fecha_oficio,
                           :tipo_oficio,
                           :razon_social,
                           :nro_expediente,
                           :CodigoJuzgado,
                           :nombreJuez,
                           :capital,
                           :interes,
                           :DependenciaAfip,
                           :fecha_alta_int,
                           :informacionSolic,
                           :nroOficioLevant,
                           :legAgente,
                           :realizaTrans,
                           :codigoVigencia,
                           :codigoRespuesta,
                           :personaJuridica,
                           'S',
                           :WW$SAL,
                           :aasfei,
                           :tipoDocumento,
                           :cuitDec,
                           :nro_jobs,
                           'SOJONLINE',
                           'SOJONLINE',
                           :aasfei,
                           :hora,
                           :origenDatos);

   ENDSR;

//=================================================
   //  Envia los parametros para el programa SRWSSOJ
//=================================================-
  BEGSR SRWSSOJ ;

     callSRWSSOJ0R2( nro_jobs:
                     tipo_cuenta:
                     sucursal:
                     cuenta:
                     nro_iche:
                     importe:
                     moneda:
                     cotizacion:
                     nro_cuit:
                     razon_social:
                     nro_oficio:
                     tipo_oficio:
                     nro_expediente:
                     CodigoJuzgado:
                     nombreJuez:
                     capital:
                     interes:
                     DependenciaAfip:
                     //transfBanco:
                     //transfSucursal:
                     //transfCuenta:
                     informacionSolic:
                     nroOficioLevant:
                     legAgente:
                     realizaTrans:
                     codigoVigencia:
                     codigoRespuesta:
                     personaJuridica:
                     tipoDocumento:
                     origenDatos:
                     IdOrGral:
                     fecha_oficio:
                     fecha_alta_int:
                     codigo_error);

  ENDSR;

end-proc;
/copy desa00525/qrpgsrc,ligdimpl ;
