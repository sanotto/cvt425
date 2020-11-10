*SRCMBRTXT:LINK-GDE-Implemetaciones de Funciones C
**free

//==============================================================================
// getRefreshFileName: Crea una etiqueta de archivo según formato Link
//==============================================================================
dcl-proc getRefreshFileName;


    dcl-pi *N char(29);           // <= Etiqueta creada
        p_Servicio char(2) const; // => Servicio
        p_TipoServ char(2) const; // => Tipo de Servicio
    end-pi;

    dcl-ds file_name qualified;
        label;
          Prefijo     char(004)   overlay(label)       Inz('RGDE');
          Entidad     char(004)   overlay(label:*next) Inz('0309');
          Servicio    char(002)   overlay(label:*next);
          TipoServ    char(002)   overlay(label:*next);
          Version     Zoned(03:0) overlay(label:*next);
          FechaProc   char(002)   overlay(label:*next) Inz('FP');
          AA          char(002)   overlay(label:*next);
          MM          char(002)   overlay(label:*next);
          DD          char(002)   overlay(label:*next);
          Extension   char(006)   overlay(label:*next)Inz('.DATOS');
    end-ds;

    dcl-ds date_ds qualified;
        isofmt          zoned(8:0);
            Year        char(004) overlay(isofmt);
               cc       char(002) overlay(Year);
               aa       char(002) overlay(Year:*next);
            Month       char(002) overlay(isofmt:*next);
            Day         char(002) overlay(isofmt:*next);
    end-ds;


    dcl-s vernro zoned(3:0);
    dcl-s prefix char(13);
    dcl-s label  char(29);
    dcl-s aasfei zoned(8:0);

    aasfei = bco_get_sfei();

    date_ds.isofmt = aasfei;

    file_name.Servicio = p_Servicio ;
    file_name.TipoServ = p_TipoServ;

    prefix= %subst(file_name.label:1:12)+'%';
    exec sql select
                 count(*)  into :vernro
             from liserj
             where
                     RJFALT = :aasfei
                 and RJDACO like :prefix;

    file_name.Version  = vernro;
    file_name.AA       = date_ds.AA;
    file_name.MM       = date_ds.Month;
    file_name.DD       = date_ds.Day;

    label = file_name.label;

    exec sql insert into liserj (
              RJDACO,
              RJFALT,
              RJIUSR,
              RJHORA)
          values(
              :label,
              :aasfei,
              CURRENT_USER,
              cast(varchar_format(current_timestamp,'HH24MISS')
                   as dec(6, 0))
          );
    return label;
end-proc;
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

//==============================================================================
// sndPgmMsg: Envía un Program Message (Mensaje que sale en el joblog)
//==============================================================================
dcl-proc sndPgmMsg;

    dcl-pi *n char(4);                                // <= Key del msg env.
        msg_id      char(7) const ;                   // => Id del msg
        msg_file    char(20) const options(*nopass);  // =>Bib y Msgf en una str
        msg_data    char(256) const options(*nopass); // =>Datos adicionales
        msg_type    char(10) const options(*nopass);  // =>Tipo Msg *ESC o *INQ
    end-pi;

    dcl-pr sysSndPgmMsg extpgm('QMHSNDPM');
        MessageID                   char(  7) const;
        QualMsgF                    char( 20) const;
        MsgData                     char(256) const;
        MsgDtaLen                   int(  10) const;
        MsgType                     char( 10) const;
        CallStkEnt                  char( 10) const;
        CallStkCnt                  int(  10) const;
        MessageKey                  char(  4);
        ErrorCode                   char(32766) options(*varsize);
    end-pr;

    dcl-s msg_key   char(4);
    dcl-s wwMsgLen  int(10);
    dcl-s wwMsgF    char(20);
    dcl-s wwMsgD    char(256);
    dcl-s wwTheKey  char(4);
    dcl-s wwMsgTyp  char(10) INZ('*ESCAPE');

    wwMsgF='QCPFMSG   *LIBL     ';
    wwMsgD=*BLANKS;
    if %parms  >= 2;
        wwMsgF=msg_file;
    endif;
    if %parms  >= 3;
        wwMsgD=msg_data;
    endif;
    if %parms  >= 4;
        wwMsgTyp=msg_type;
    endif;
    wwMsgLen = %len(%trimr(wwMsgD));

    SysSndPgmMsg(msg_id:
                 wwMsgF:
                 wwMsgD:
                 wwMsgLen:
                 wwMsgTyp:
                 '*CTLBDY':
                 1:
                 wwTheKey:
                 SYSERR);

    return    wwTheKey;

end-proc;
//==============================================================================
// die: Finaliza ejecución y deja mensaje en JobLog
//==============================================================================
dcl-proc die;
    dcl-pi *n;
        peMsg char(256) const;
    end-pi;


    dcl-pr sysSndPgmMsg extpgm('QMHSNDPM');
        MessageID                   char(  7) const;
        QualMsgF                    char( 20) const;
        MsgData                     char(256) const;
        MsgDtaLen                   int(  10) const;
        MsgType                     char( 10) const;
        CallStkEnt                  char( 10) const;
        CallStkCnt                  int(  10) const;
        MessageKey                  char(  4);
        ErrorCode                   char(32766) options(*varsize);
    end-pr;

    dcl-ds dsEC;
      dsECBytesP             int(3) inz;
      dsECBytesA             int(3) inz(0);
      dsECMsgID              char(6);
      dsECReserv             char(1);
      dsECMsgDta             char(239);
    end-ds;

    dcl-s wwMsgLen          int(10);
    dcl-s wwTheKey          char(4);

    wwMsgLen = %len(%trimr(peMsg));
    if wwMsgLen<1;
        return;
    endif;

    sysSndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
               peMsg: wwMsgLen: '*ESCAPE':
              '*PGMBDY': 1: wwTheKey: dsEC);

    return;
end-proc;

//==============================================================================
// die: Finaliza ejecución y deja mensaje en JobLog
//==============================================================================
dcl-proc wrtToJobLog;
    dcl-pi *n;
        peMsg char(256) const;
    end-pi;


    dcl-pr sysSndPgmMsg extpgm('QMHSNDPM');
        MessageID                   char(  7) const;
        QualMsgF                    char( 20) const;
        MsgData                     char(256) const;
        MsgDtaLen                   int(  10) const;
        MsgType                     char( 10) const;
        CallStkEnt                  char( 10) const;
        CallStkCnt                  int(  10) const;
        MessageKey                  char(  4);
        ErrorCode                   char(32766) options(*varsize);
    end-pr;

    dcl-ds dsEC;
      dsECBytesP             int(3) inz;
      dsECBytesA             int(3) inz(0);
      dsECMsgID              char(6);
      dsECReserv             char(1);
      dsECMsgDta             char(239);
    end-ds;

    dcl-s wwMsgLen          int(10);
    dcl-s wwTheKey          char(4);

    wwMsgLen = %len(%trimr(peMsg));
    if wwMsgLen<1;
        return;
    endif;

    sysSndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
               peMsg: wwMsgLen: '*INFO':
              '*PGMBDY': 1: wwTheKey: dsEC);

    return;
end-proc;

//==============================================================================
// ifs_stmf_opnwrt: Abre un archivo DE TEXTO EN UTF-8/ASCII para escritura
//==============================================================================
dcl-proc ifs_stmf_opnwrt;
  dcl-pi *n int(10);
    fqn varchar(255) const;
  end-pi;

  dcl-s flags     int(10);
  dcl-s mode      int(10);
  dcl-s fd        int(10);

  flags = O_WRONLY + O_CREAT + O_TRUNC + O_CODEPAGE ;
  mode = S_IRUSR + S_IWUSR + S_IRGRP + S_IROTH;
  fd = open(fqn: flags: mode: 01252);

  callp close(fd);

  fd = open(fqn: O_WRONLY+O_TEXTDATA);

  return fd;
end-proc;
//==============================================================================
// ifs_stmf_opnread: Abre un archivo DE TEXTO EN UTF-8/ASCII para lectura
//==============================================================================
dcl-proc ifs_stmf_opnread;
  dcl-pi *n int(10);
    fqn varchar(255) const;
  end-pi;

  dcl-s flags     int(10);
  dcl-s mode      int(10);
  dcl-s fd        int(10);

  flags = O_RDONLY + O_TEXTDATA ;
  mode = S_IRUSR + S_IWUSR + S_IRGRP + S_IROTH;

  fd = open(fqn: flags:mode:01252);

  return fd;

end-proc;

//==============================================================================
// ifs_stmf_close: Cierra un archivo DE TEXTO abierto en el IFS
//==============================================================================
dcl-proc ifs_stmf_close;
  dcl-pi *n;
    fd int(10) const;
  end-pi;
  callp close(fd);
end-proc;

//==============================================================================
// ifs_wrtln: Escribe una línea terminada con cr+lf
//==============================================================================
dcl-proc ifs_wrtln;
  dcl-pi *n;
    fd int(10) const;
    line varchar(MAX_FILE_SIZE) const;
  end-pi;

  dcl-c nl        x'0d25';
  dcl-s contents  varchar(MAX_FILE_SIZE) ;
  contents = %trimr(line)+nl;
  callp write(fd: %addr(contents)+2: %len(contents));
end-proc;

//==============================================================================
// ifs_wrtln: Escribe una línea terminada con cr
//==============================================================================
dcl-proc ifs_readln;
  dcl-pi *n int(10);
    fd int(10) const;
    line varchar(MAX_FILE_SIZE);
  end-pi;

  dcl-c zero_d        x'0d';


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
// ifs_wrtln: Escribe una línea terminada con cr + lf
//==============================================================================
dcl-proc ifs_readln_dos;
  dcl-pi *n int(10);
    fd int(10) const;
    line varchar(MAX_FILE_SIZE);
  end-pi;

  dcl-c zero_d        x'0d';


  dcl-s contents  varchar(MAX_FILE_SIZE) ;
  dcl-s input_char char(1);
  dcl-s ec int(10);
  dcl-s contador int(10) inz(-1);

   ec = read(fd:%addr(input_char):1);
   DoW ec > 0 and input_char <> zero_d;
       contador +=1;
       contents += input_char;
       ec = read(fd:%addr(input_char):1);
       if input_char = zero_d;
            ec = read(fd:%addr(input_char):1);
            input_char=zero_d;
       endif;
   enddo;
   line=contents;
   return contador;

end-proc;

dcl-proc bco_get_sfei;
    dcl-pi *n packed(8:0) end-pi;

    dcl-s aasfei zoned(8:0);

    exec sql select aasfei into :aasfei from sgsysv limit 1;

    return aasfei;
end-proc;

dcl-proc bco_get_cbu;
    dcl-pi *n packed(22:0);
        tipo    char(1);
        isuc    packed(5:0);
        icta    packed(11:0);
    end-pi;

    dcl-pr cbu000rg extpgm('CBU000RG');
        WWTIPO packed( 2:0);
        WWISUC packed( 5:0);
        WWICTA packed(11:0);
        WWBLQ1 packed( 8:0);
        WWBLQ2 packed(14:0);
        WWBLQ3 packed(22:0);
    end-pr;

    dcl-s WWTIPO packed( 2:0);
    dcl-s WWBLQ1 packed( 8:0);
    dcl-s WWBLQ2 packed(14:0);
    dcl-s WWBLQ3 packed(22:0);

    wwtipo= 1;
    if tipo='2';
        wwtipo = 2;
    endif;
    cbu000rg(   WWTIPO :
                isuc:
                icta:
                WWBLQ1:
                WWBLQ2:
                WWBLQ3);

    return WWBLQ3;
end-proc;
