*SRCMBRTXT:Regimen de Reintegro IVA               
**free
ctl-opt
        option(*SRCSTMT)
        dftactgrp(*NO)
        main(main);

dcl-c MAX_FILE_SIZE 65535;

/include sdb01.src/qrpgsrc,ligdprot ;
/include sdb01.src/qrpgsrc,ligdifs ;

//==================================================
// Procedimiento Main: PRINCIPAL
//==================================================
dcl-proc Main;

    //... Llamada al programa 'SBBAVFE1'
    dcl-pr SBBAVFE1 extpgm('SBBAVFE1');
        PAFECH             packed(8:0);
        PAISUC             packed(5:0);
        PAMODO             char(2);
    end-pr;

    //... Llamada al programa 'SBBABFE1'
    dcl-pr SBBABFE1 extpgm('SBBABFE1');
        PAFECH             packed(8:0);
        PADIAS             packed(15:0);
        PAMODO             char(2);
    end-pr;

    //Declaro Variables.

    dcl-s cmd              varchar(4096);
    dcl-s rc               char(7);
    dcl-s fd               int(10);
    dcl-s fqn              varchar(255) inz;
    dcl-s nombre_archivo   char(12);
    dcl-s periodo          packed(6:0);
    dcl-s per              char(8);
    dcl-s tipo             char(1);
    dcl-s count            packed(2:0);

    //Totales Cabecera.

    dcl-s tot_ope          packed(15:0);
    dcl-s tot_imp          packed(15:2);
    dcl-s tot_imv          packed(15:2);

    dcl-s tot_op           packed(15:0);
    dcl-s tot_im           packed(15:2);
    dcl-s tot_iv           packed(15:2);

    dcl-s tot_operacion    char(15);
    dcl-s tot_importe      char(15);
    dcl-s tot_importe_dev  char(15);

    //Totales Registro.

    dcl-s tot_oper         packed(4:0);
    dcl-s tot_impr         packed(15:2);
    dcl-s tot_imvr         packed(15:2);

    dcl-s tot_operacionr   char(4);
    dcl-s tot_importer     char(15);
    dcl-s tot_importe_devr char(15);

    dcl-s dest_name        char(255);

    dcl-s nombre_liserj    char(30);
    dcl-s dia_habil        packed(8:0);
    dcl-s aasfei           packed(8:0);
    dcl-s PAFECH           packed(8:0);
    dcl-s PAISUC           packed(5:0);
    dcl-s PAMODO           char(2);
    dcl-s PADIAS           packed(15:0);

    //Declaración de Estructuras de datos

    dcl-ds fila_clientes   qualified;
            tccl           packed(11:0);
            tsuc           packed(5:0);
            tsub           char(2);
    end-ds;

    exec sql set option commit=*None;

    exsr recuperar_periodo;
    exsr recuperar_quinto_dia_habil_cada_mes;

    if dia_habil = aasfei;
      exsr verificar_archivo_procesado;
      exsr declarar_cursores;
      exec sql open cursor_clientes;
      exec sql fetch  cursor_clientes into :fila_clientes;
      if sqlcod <> *Zero;
          exsr archivo_no_generado;
      endif;
      exsr abrir_archivo;
      exsr escribir_cabecera_registro;
      wrtToJobLog('Comienza Generación de archivo:'+fqn);
          dow sqlcod = *Zero;
              exsr escribir_detalles_registro;
              exec sql fetch  cursor_clientes into :fila_clientes;
          enddo;
      exec sql close cursor_clientes;
      exsr cerrar_archivo;
      exsr guardar_liserj;
      exsr cambiar_permisos_sobre_archivo;
      exsr enviar_mail;
    endif;

//==============================================================================

    begsr recuperar_quinto_dia_habil_cada_mes;

        exec sql
            SELECT
                AASFEI
                INTO:aasfei
            FROM SGSYSV;

        PAFECH = dia_habil;
        PAISUC = 0;
        PAMODO = 'I';
        count = 1;

        dow count <= 1;

            SBBAVFE1(PAFECH
                    :PAISUC
                    :PAMODO);

            if PAMODO <> '';
            PADIAS = 0;
            PAMODO = 'IN';

            SBBABFE1(PAFECH
                    :PADIAS
                    :PAMODO);

            endif;

            count = count + 1;

        enddo;

        dia_habil = PAFECH;

    endsr;

//==============================================================================

    begsr verificar_archivo_procesado;

    nombre_archivo = 'ma550' + '_' + %editw(periodo:'      ');

    exec sql
            SELECT RJDACO
                INTO :nombre_liserj
            FROM LISERJ
            WHERE RJDACO = :nombre_archivo;

        if nombre_archivo = nombre_liserj;

        //...Imprimo mensaje de error en pantalla de sideba

        die('El Archivo '+%Trim(nombre_archivo)+', ya fué ' +
            'procesado anteriormente por favor verifique y' +
            ' reintente nuevamente.                       ');

        endif;

    endsr;

//==============================================================================

    begsr recuperar_periodo;

        exec sql
            SELECT
                SUBSTRING(AASFEI, 1, 6)
                INTO:periodo
            FROM SGSYSV;

        per = %editw(periodo:'      ') + '05';

        dia_habil = %dec(per:8:0);

        periodo = periodo - 1;

    endsr;

//==============================================================================
    begsr declarar_cursores;

//==============================================================================
// Clientes
//==============================================================================
        exec sql
                declare cursor_clientes cursor for
                    SELECT DISTINCT
                        T2ICCL,
                        T2ISUC,
                        T2ISUB
                    FROM TC255D
                    WHERE
                    SUBSTRING(T2FING, 1, 6) = :periodo  AND
                    T2FACR = 0 AND
                    T2DF04 = 'LA250DIS' AND
                    (T2BLK2 = 'IMPACTO' OR T2BLK2 = 'NO IMPAC');

    endsr;

//==============================================================================

    begsr abrir_archivo;

        fqn = '/home/Tarjetas_de_Creditos/PADRON_HIST/' + %Trim(nombre_archivo);

        fd = ifs_stmf_opnwrt(fqn);
        if (fd < 0);
           die('No se pudo abrir archivo:'+fqn);
        endif;

    endsr;

//==============================================================================

    begsr escribir_cabecera_registro;

    exec sql
            SELECT DISTINCT
                COUNT(*),
                SUM(T2$IMP),
                SUM(T2$TOT)
                INTO:tot_ope, :tot_imp, :tot_imv
            FROM TC255D
            WHERE
            SUBSTRING(T2FING, 1, 6) = :periodo  AND
            T2FACR = 0 AND
            T2DF04 = 'LA250DIS' AND
            (T2BLK2 = 'IMPACTO' OR T2BLK2 = 'NO IMPAC');

    exec sql
            SELECT DISTINCT
                COUNT(*),
                SUM(T2$IMP),
                SUM(T2$TOT)
                INTO:tot_op, :tot_im, :tot_iv
            FROM TC255D
            WHERE
            SUBSTRING(T2FING, 1, 6) = :periodo  AND
            T2FACR = 0 AND
            T2DF04 = 'LA250DIS' AND
            (T2BLK2 = 'IMPACTO' OR T2BLK2 = 'NO IMPAC');

    tot_operacion   = %char(tot_ope);
    tot_importe     = %char(tot_imp);
    tot_importe_dev = %char(tot_imv);

    tot_operacion   = %Editc ( %Dec ( %Trim ( tot_operacion ) :15:2) : 'X' );
    tot_importe     = %Editc ( %Dec ( %Trim ( tot_importe ) :15:2) : 'X' );
    tot_importe_dev = %Editc ( %Dec ( %Trim ( tot_importe_dev ) :15:2) : 'X' );

    ifs_wrtln(fd:'01'                           + //Tipo de Registro.
                 '30671859339'                  + //Cuit Ent. Fin.
                 ''+%editw(periodo:'      ')+'' + //Periodo.
                 '0'                            + //Secuencia.
                 '940'                          + //Impuesto.
                 '019'                          + //Concepto.
                 '019'                          + //Sub Concepto.
                 '2008'                         + //Formulario.
                 '00100'                        + //Versión.
                 ''+tot_operacion  +''          + //Cant. Tot. Oper.
                 ''+tot_importe    +''          + //Impt. Tot. Oper.
                 ''+tot_importe_dev+'');          //Impt. Tot. Devo.

    endsr;

//==============================================================================

    begsr escribir_detalles_registro;

    exec sql
            SELECT DISTINCT
                  COUNT(*),
                  SUM(T2$IMP),
                  SUM(T2$TOT)
            INTO :tot_oper, :tot_impr, :tot_imvr
            FROM TC255D
            WHERE SUBSTRING(T2FING, 1, 6) = :periodo  AND
                  T2ICCL = :fila_clientes.tccl AND
                  T2FACR = 0 AND
                  T2DF04 = 'LA250DIS' AND
                  (T2BLK2 = 'IMPACTO' OR T2BLK2 = 'NO IMPAC');

    tot_operacionr   = %char(tot_oper);
    tot_importer     = %char(tot_impr);
    tot_importe_devr = %char(tot_imvr);

    tot_operacionr   = %Editc( %Dec ( %Trim ( tot_operacionr ) :4:0) : 'X' );
    tot_importer     = %Editc( %Dec ( %Trim ( tot_importer ) :15:2) : 'X' );
    tot_importe_devr = %Editc( %Dec ( %Trim ( tot_importe_devr ) :15:2) : 'X' );

    if (fila_clientes.tsub = 'CC');
            tipo='1';
    else;
            tipo='2';
    endif;

    ifs_wrtln(fd:'02'                           +               //Tipo de Reg.
                 ''+%editw(bco_get_cbu( tipo:
                                        fila_clientes.tsuc:
                                        fila_clientes.tccl):
                                  '                      ')+''+ //Cbu Benef.
                 '33629749859'                  +               //Cuit Adm.
                 ''+tot_operacionr     +''      +               //Can. Tot. Ope.
                 ''+tot_importer       +''      +               //Imp. Tot. Ope.
                 ''+tot_importe_devr   +'');                    //Imp. Tot. Dev.

    exec sql UPDATE TC255D SET T2FACR = :aasfei
             WHERE SUBSTRING(T2FING, 1, 6) = :periodo  AND
                   T2ICCL = :fila_clientes.tccl AND
                   T2FACR = 0 AND
                   T2DF04 = 'LA250DIS' AND
                   (T2BLK2 = 'IMPACTO' OR T2BLK2 = 'NO IMPAC');

    endsr;

//==============================================================================

    begsr cerrar_archivo;

        ifs_stmf_close(fd);

    endsr;

//==============================================================================

    begsr guardar_liserj;

        exec sql
            INSERT INTO LISERJ
             (
              RJDACO,
              RJCAN1,
              RJ$TOT,
              RJFALT,
              RJIUSR,
              RJHORA,
              RJITAC
             )
             values
             (
              :nombre_archivo,
              0,
              0,
              (SELECT AASFEI FROM SGSYSV LIMIT 1),
              CURRENT_USER,
              (SELECT HOUR(NOW())*10000+MINUTE(NOW())*100 +SECOND(NOW())
               FROM  SYSIBM/SYSDUMMY1 ),
              0
             );

    endsr;

//==============================================================================

    begsr cambiar_permisos_sobre_archivo;

    cmd =   'CHGCURDIR DIR(''/home'')';
    rc=exeCmd(cmd);

    cmd = 'QSH CMD(''chown GR00001 '+%trim(fqn)+''')';
    rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr enviar_mail;

        cmd =   'SNDMAIL                                 '+
              '   RECP(''TCPA01C4'')                     '+
              '   SUBJECT(''Rendicion AFIP-Reintegro IVA-Per_'+
                          ''+%editw(periodo:'      ')+''')'+
              '   MESG(''El archivo fue generado, '+
            'el mismo se encuentra en, '+
            '/home/Tarjetas_de_Cretido/PADRON_HIST. '+
            'Solicitar al Personal de Operaciones el mismo. '+
            'Archivo: '+%Trim(nombre_archivo)+'. '+
            'Tot. de Ope.: '+%trim(%editw(tot_ope:'               '))+'. '+
            'Tot. de Imp.: '+%trim(%editw(tot_im:'               '))+'. '+
            'Tot. de Imp. Dev.:'+%trim(%editw(tot_iv:'               '))+'.'')';
        rc=exeCmd(cmd);

    endsr;

//==============================================================================

    begsr archivo_no_generado;

        cmd = 'SNDMAIL                                       '+
              '   RECP(''TCPA01C4'')                         '+
              '   SUBJECT(''Rendicion AFIP-Reintegro IVA-Per_'+
                         ''+%editw(periodo:'      ')+''')'+
              '   MESG(''El Archivo no fue generado. No se encontraron'+
                      ' movimientos a rendir'')';
        rc=exeCmd(cmd);

        die('El Archivo '+%Trim(nombre_archivo)+', no fué ' +
            'generado porque no se encontrarón movimientos');

    endsr;

//==============================================================================
end-proc;

/copy sdb01.src/qrpgsrc,ligdimpl ;
