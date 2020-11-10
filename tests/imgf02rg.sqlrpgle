*SRCMBRTXT:Zipea comprobantes PDF para enviadas   
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*NO)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

/include sdb01.src/qrpgsrc,ligdprot ;
/include sdb01.src/qrpgsrc,ligdifs ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

// *entry plist
    dcl-pi *n;
         nombre_archivo   char(30) ;
    end-pi;


//... Variables
    dcl-s cmd                char(4096);
    dcl-s aasfei             packed(8:0);
    dcl-s usuario            char(10);
    dcl-s rc                 char(10);
    dcl-s hora               packed(6:0);
    dcl-s error_EnviarMail   packed(1:0);
    dcl-s path_archivo       char(250);
    dcl-s etiqueta_archivo   char(26);
    dcl-s nombre_pdf         char(250);
    dcl-s subject            char(256) inz(*blanks);
    dcl-s mesg               char(256) inz(*blanks);
    dcl-s file               char(256) inz(*blanks);


//... Llamada al programa 'QCMDEXC'
   dcl-pr ejecutar_programa_IMGF01S7 extpgm('IMGF01S7');
        nombre_archivo      char(30) const;
   end-pr;

   //... Llamada al programa 'SGAUTAC4'
   dcl-pr ejecutar_programa_SGAUTAC4 extpgm('SGAUTAC4');
          path_archivo      char(250) const;
   end-pr;


//Aca empieza el programa
exec sql select current_user into :usuario from sysibm.sysdummy1;
exec sql select aasfei into :aasfei from sgsysv;

    etiqueta_archivo = %subst(nombre_archivo:1:26);
    EXSR declara_cursores_para_consultas;
    exsr mover_archivos;
    exsr zipeo_carpeta;
    EXSR fin_de_programa;


//==============================================================================
// Declaro los cursores para realizar las consultas.
//==============================================================================

    begsr declara_cursores_para_consultas;

//Consulta para saber si exite la carpeta 'trasnf'
        exec sql DECLARE C1 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/TEI')) AS T
         WHERE UPPER(CAST(FILENAME as char(255))) like
               UPPER('%FM%');


//Consulta para saber si exite la carpeta 'trasnf'
        exec sql DECLARE C2 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/TEI')) AS T
         WHERE UPPER(CAST(FILENAME as char(255))) like
               UPPER('%IMTEI%');

ENDSR;


//==============================================================================
// Mover archivos a carpeta para zipear
//==============================================================================

    begsr mover_archivos;

    exec sql open C1;
    exec sql FETCH C1 INTO :nombre_PDF;

    IF SQLCOD = *ZERO;
    cmd = 'CHGCURDIR DIR(''/home'')';
    execmd(cmd);
    ejecutar_programa_SGAUTAC4('mkdir -p /home/TEI/'+ etiqueta_archivo);
    ENDIF;

    DoW SQLCOD = *ZERO;


    ejecutar_programa_SGAUTAC4('mv /home/TEI/' + %trim(nombre_pdf) +
                      ' /home/TEI/' + etiqueta_archivo);
    exec sql fetch C1 INTO :nombre_pdf;

    ENDDO;

    exec sql close C1;

    endsr;

//==============================================================================
// Zipeo carpeta con archivos de transferencias presentacion
//==============================================================================
    begsr zipeo_carpeta;

    hora   = %dec(%time():*HMS);
    path_archivo = '/home/TEI/Archivos_Procesados/FM_US_'  + %TrIM(usuario)   +
                        '_FE_'   + %char(AASFEI)                              +
                        '_HO_'   + %char(hora)                                +
              '_TR_'   + %Editc (%dec(%subst(nombre_archivo:20:7):7:0): 'X' ) +
              '.ZIP';

    exec sql open C2;
    exec sql FETCH C2;

    IF SQLCOD = *ZERO;


    cmd = 'ZIPF ZIPFILE(''' +%Trim(path_archivo) + ''') FILES(''/home/TEI/'
                                 + etiqueta_archivo+''')';

    exeCmd(cmd);

    //envio mail
    subject = 'TEI GOBIERNO: Informe Transferencias ';
    mesg    = 'Se adjunta informes de transferencias realizadas '+
              'en la fecha '+ %char(AASFEI)+ ' para el siguiente archivo: '+
               X'0D'+ X'25'+
               X'0D'+ X'25'+
               %trim(nombre_archivo) +
               X'0D'+ X'25';
    file = path_archivo;
    exsr envio_informe;

    //Elimino la carpeta temporal
     ejecutar_programa_SGAUTAC4(' rm -r /home/TEI/'+ etiqueta_archivo + '/');

    endif;

    exec sql close C2;

    endsr;

//=========================================================================
  // Envio Informe
//=========================================================================
   BEGSR envio_informe;

    cmd = 'SNDMAIL RECP('''+%trim(usuario)+''') '+
           'SUBJECT('''+%trim(subject)+''') '+
           'MESG('''+%trim(mesg)+''') '+
           'FILE('''+%trim(file)+''')';
     rc = exeCmd(cmd);

     if rc <> 'CPF0000';
        error_EnviarMail = 1;
     endif;

   ENDSR;


//==============================================================================
// Fin de programa
//==============================================================================

    begsr fin_de_programa;
        return;
    endsr;



end-proc;
/copy sdb01.src/qrpgsrc,ligdimpl ;
