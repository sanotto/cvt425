*SRCMBRTXT:Carga de base MIPYME - Archivo SEPYME  
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

//==============================================================================
//Declaración de prototipos, importadas de los miembros declarados.
//==============================================================================

//Max Buffer
dcl-c MAX_FILE_SIZE 65535;

/include sdb01.src/qrpgsrc,ligdifs ;
/include sdb01.src/qrpgsrc,ligdprot ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

    //*Entry plist.

    dcl-f bampdp usage(*output) ;

    dcl-ds rec likerec(rebampdp) ;

    dcl-s filename              varchar(255);
    dcl-s import_ok             ind;
    dcl-s sufijo                varchar(255);
    dcl-s path_to_out           varchar(255);
    dcl-s dest_name             varchar(255);
    dcl-s fd                    int(10);
    dcl-s eof                   int(10);
    dcl-s line                  varchar(MAX_FILE_SIZE);
    dcl-s cmd                   varchar(4096);
    dcl-s rc                    char(7);
    dcl-s curl                  char(2048);
    dcl-s cantidad_registros    packed(15:0);

    dcl-s cuit                  char(11);
    dcl-s fecha1                char(8);
    dcl-s fecha2                char(8);
    dcl-s fecha                 packed(8:0);
    dcl-s hora                  packed(6:0);
    dcl-s usr                   char(10);

    dcl-s url                   char(2048);
    dcl-s directorio            char(255);

    dcl-s token                 varchar(MAX_FILE_SIZE);
    dcl-s dir_tk                char(255);
    dcl-s curl_tk               char(510);
    dcl-s lnk                   char(255);
    dcl-s user                  char(255);
    dcl-s pass                  char(255);
    dcl-s grant                 char(255);
    dcl-s cl_id                 char(255);
    dcl-s cl_sc                 char(255);

    filename = '/home/Impuestos/SEPYME.txt';
    path_to_out = '/home/Impuestos/historico/';

    exec sql SET OPTION COMMIT = *NONE;

    exsr armar_patch;
    exsr verificar_estado_servicio;
    exsr solicitar_token;
    exsr procesar_token;
    exsr recuperar_padron;
    exsr mover_padron_a_historicos;
    exsr eliminar_registros_padron_pymes;
    exsr insertar_padron_pymes;
    exsr guardar_log_en_liserj;
    exsr mover_archivo_a_ya_procesados;
    return;

//==============================================================================
// Armar Patch.
//==============================================================================

    begsr armar_patch;

        exec sql SELECT
                    TRANSLATE(
                            CAST ( timestamp(now()) as char(19))
                                   , '__','.-') || '_' || CURRENT_USER
                    INTO :sufijo
                 FROM SYSIBM/SYSDUMMY1 ;

        dest_name = %Trim(path_to_out)+
                    'SEPYME.txt'+
                    '_' +
                    ''+%Trim(sufijo);

    endsr;

//==============================================================================
// Verifico Estado de Servicio.
//==============================================================================

    begsr verificar_estado_servicio;

    directorio = '/home/tmp/verificador.txt';

    url = 'curl https://padronpymes.produccion.gob.ar/Ñ/pages/login'+
          ' -I -s -k -m 20 -o '+%Trim(directorio)+'';

    cmd = 'QSH CMD('''+%trim(url)+''')';
    rc  = exeCmd(cmd);

    fd=ifs_stmf_opnread(%Trim(directorio));
    eof = ifs_readln_dos(fd: line);

     if %Trim(line) <> 'HTTP/1.1 200 OK';

      cmd =   'RMVLNK OBJLNK('''+%TRIM(directorio)+''')';
      rc=exeCmd(cmd);

      die('El servicio no esta disponible. Intente nuevamente mas tarde.');

     endif;

    cmd =   'RMVLNK OBJLNK('''+%TRIM(directorio)+''')';
    rc=exeCmd(cmd);

    endsr;

//==============================================================================
// Solicitar Token
//==============================================================================

    begsr solicitar_token;

    dir_tk = '/home/tmp/token.txt';

    lnk   = 'https://padronpymes.produccion.gob.ar/oauth/token';
    user  = 'username=30671859339';
    pass  = 'password=EX202000633805';
    grant = 'grant_type=password';
    cl_id = 'client_id=5e0a4d18afcb99016577a2a4';
    cl_sc = 'client_secret=Kxnbp9EokrbLRP0CRy1n8L4m6paLsCxPUnJwQ4nf';

    curl_tk = 'curl -X POST'+
              ' -F '+%Trim(user)+''+
              ' -F '+%Trim(pass)+''+
              ' -F '+%Trim(grant)+''+
              ' -F '+%Trim(cl_id)+''+
              ' -F '+%Trim(cl_sc)+''+
              ' '+%Trim(lnk)+' --silent --insecure -o '+%Trim(dir_tk)+'';

    cmd = 'QSH CMD('''+%trim(curl_tk)+''')';
    rc  = exeCmd(cmd);

    endsr;

//==============================================================================
// Procesar Token
//==============================================================================

    begsr procesar_token;

    fd=ifs_stmf_opnread(%Trim(dir_tk));
    eof = ifs_readln_dos(fd: line);

    token = %Subst(line:62:1132);

    cmd =   'RMVLNK OBJLNK('''+%TRIM(dir_tk)+''')';
    rc=exeCmd(cmd);

    endsr;

//==============================================================================
// Recuperar Token.
//==============================================================================

    begsr recuperar_padron;

    curl = 'curl '+
         '''''https://padronpymes.produccion.gob.ar/api/v1/certificados'''' '+
         '--silent --insecure  --header ''''Authorization: Bearer '+
             ''+%Trim(token)+''''' -o '+%trim(filename)+'';

    cmd = 'QSH CMD('''+%trim(curl)+''')';
    rc  = exeCmd(cmd);

   endsr;

//==============================================================================
// Agregar en Padron Pymes.
//==============================================================================

    begsr insertar_padron_pymes;

    exec sql
            SELECT
                (SELECT AASFEI FROM SGSYSV LIMIT 1),
                HOUR(NOW())*10000+MINUTE(NOW())*100 + SECOND(NOW()),
                CURRENT_USER
                into:fecha, :hora, :usr
            FROM SYSIBM/SYSDUMMY1;

    fd=ifs_stmf_opnread(filename);
    eof = ifs_readln_tres(fd: line);

    DoW eof > *zero;
        cuit   = %subst(line:1:11);
        fecha1 = %subst(line:13:4)+
                 %subst(line:18:2)+
                 %subst(line:21:2);

        fecha2 = %subst(line:24:4)+
                 %subst(line:29:2)+
                 %subst(line:32:2);

        clear rec;
        rec.DPICUI=%dec(cuit:11:0);
        rec.DPFIN1=%dec(fecha1:8:0);
        rec.DPFIN2=%dec(fecha2:8:0);
        rec.DPFING=fecha;
        rec.DPHORA=hora;
        rec.DPIUSR=usr;

        write REBAMPDP rec;
        eof = ifs_readln_tres(fd: line);

    EndDo;

    endsr;

//==============================================================================
// Mover a Historicos.
//==============================================================================

    begsr mover_padron_a_historicos;

        exec sql
                SELECT COUNT(*) into:cantidad_registros FROM BAMPDP;

        if cantidad_registros > 0;

        cmd = 'CPYF  FROMFILE(BAMPDP)  '+
              '      TOFILE(BAMPDH)    '+
              '      MBROPT(*ADD)      '+
              '      FMTOPT(*NOCHK)    ';

        rc  = exeCmd(cmd);

        exec sql
                UPDATE BAMPDH SET
                    DHFALT = (SELECT AASFEI FROM SGSYSV LIMIT 1)
                    WHERE DHFALT = 0;

        endif;

    endsr;

//==============================================================================
// Eliminar registros de Padron
//==============================================================================

    begsr eliminar_registros_padron_pymes;

        exec sql DELETE FROM BAMPDP;

    endsr;

//==============================================================================
// Guardar en Liserj.
//==============================================================================

    begsr guardar_log_en_liserj;

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
              SUBSTRING(:dest_name , 27, 30),
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
// Mover Archivo a Procesados.
//==============================================================================

    begsr mover_archivo_a_ya_procesados;

         cmd = 'MOV   OBJ('''+%Trim(filename)+''')  '+
               '    TOOBJ('''+%Trim(dest_name)+''') ';

         rc = exeCmd(cmd);

    endsr;

end-proc;

//==============================================================================
// ifs_wrtln: Escribe una línea terminada con lf
//==============================================================================

dcl-proc ifs_readln_tres;

  dcl-pi *n int(10);
    fd int(10) const;
    line varchar(MAX_FILE_SIZE);
  end-pi;

  dcl-c zero_d        x'25';


  dcl-s contents  varchar(MAX_FILE_SIZE) ;
  dcl-s input_char char(1);
  dcl-s ec int(10);
  dcl-s contador int(10) inz(-1);

   ec = read(fd:%addr(input_char):1);
   DoW ec > 0 and input_char <> zero_d;
       contador +=1;
       contents += input_char;
       ec = read(fd:%addr(input_char):1);
   enddo;
   line=contents;
   return contador;

end-proc;

//==============================================================================

/copy sdb01.src/qrpgsrc,ligdimpl ;
