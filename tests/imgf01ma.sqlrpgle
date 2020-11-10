*SRCMBRTXT:Manejador para generar comprobante PDF 
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
    dcl-s hora               packed(6:0);
    dcl-s path_archivo       char(250);


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
    EXSR declara_cursores_para_consultas;
    exsr crear_pdf;
    EXSR fin_de_programa;


    //==========================================================================
// Declaro los cursores para realizar las consultas.
//==============================================================================

    begsr declara_cursores_para_consultas;

//Consulta para saber si exite la carpeta 'trasnf'
        exec sql DECLARE C1 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/tmp')) AS T
         WHERE UPPER (CAST(FILENAME as char(255))) like
                    UPPER('%trasnf%');


//Consulta para saber si exite contenido en la carpeta transf
        exec sql DECLARE C2 CURSOR FOR
         SELECT
               CAST (FILENAME as char(255)) AS FILNAM
         FROM  TABLE(IFSDIR('/home/tmp/transf')) AS T
         WHERE UPPER(CAST(FILENAME as char(255))) like
                    UPPER('%TEI.PDF%');


ENDSR;

//==============================================================================
// Creo tabla temporales
//==============================================================================
    begsr crear_pdf;

    exec sql open C1;
    exec sql FETCH C1;

    If SQLCOD <> *ZERO;

      cmd = 'CHGCURDIR DIR(''/home'')';
      execmd(cmd);
      ejecutar_programa_SGAUTAC4('mkdir -p /home/tmp/transf');
    endif;

    exec sql close C1;


    ejecutar_programa_IMGF01S7(nombre_archivo);

    exec sql open C2;
    exec sql FETCH C2;

    If SQLCOD = *ZERO;

    hora   = %dec(%time():*HMS);
    path_archivo = '/home/TEI/FM_US_'  + %TrIM(usuario)                       +
                        '_FE_'   + %char(AASFEI)                              +
                        '_HO_'   + %char(hora)                                +
              '_TR_'   + %Editc (%dec(%subst(nombre_archivo:20:7):7:0): 'X' ) +
                                                  '.PDF'                      ;

    ejecutar_programa_SGAUTAC4('mv /home/tmp/transf/TEI.PDF ' +
                     path_archivo);
    ENDIF;

    exec sql close C2;

    //Elimino la carpeta presentaciones
    ejecutar_programa_SGAUTAC4(' rm -r /home/tmp/transf');

    endsr;


//==============================================================================
// Fin de programa
//==============================================================================

    begsr fin_de_programa;
        return;
    endsr;



end-proc;
/copy sdb01.src/qrpgsrc,ligdimpl ;
