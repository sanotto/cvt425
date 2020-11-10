*SRCMBRTXT:TEI2019-Calcula saldo cuenta debito    
**free
ctl-opt dftactgrp(*NO)
        main(main);

dcl-c MAX_FILE_SIZE 900;

//Declaración de prototipos, importadas de los miembros declarados
/include sdb01.src/qrpgsrc,ligdprot ;
/include sdb01.src/qrpgsrc,ligdifs ;

//Procedimiento main, declarado como punto de entrada
//en ctl-opt
dcl-proc main;


// *entry plist
    dcl-pi *n;
         nombre_Archivo   char(30) ;
         importe_Debito   packed(15:2);
         error            char(1);
    end-pi;

    dcl-s archivo_Recibido  char(255);
    dcl-s carpeta_Recibido  char(255) inz ('/home/TEI/Archivos_Recibidos/');
    dcl-s fd                int(10);
    dcl-s line              varchar(MAX_FILE_SIZE);
    dcl-s importe  char (16);

    EXSR crear_Direccion_Archivo;
    EXSR procesar_Linea;
    EXSR Fin_de_programa;

//=========================================================================
   // Subrutina que crea el nombre del archivo.
//=========================================================================

BEGSR crear_Direccion_Archivo;

// Armo direccion para archivo recibido
     archivo_Recibido =  %trim(carpeta_Recibido) +
                         nombre_Archivo;
ENDSR;

//=========================================================================
   //  Procesa el archivo (TEI: Desarrollo social)
//=========================================================================
 BEGSR procesar_Linea;

  //CONSULTO ARCHIVO TEMPORAL PARA SUMAR TOTAL DEL IMPORTE
  exec sql DECLARE C1 CURSOR FOR
         SELECT
              PLISTR
         FROM QTEMP/BCPAPL;

  exec sql open  C1;
  exec sql FETCH C1 INTO :line;

    //EXSR abrir_archivo;

   DOW SQLCOD = *ZERO;

    //DOW  ifs_readln_dos(fd: line) > 0;

      importe =   %subst(line:35:13) + '.' + %subst(line:48:2) ;
      importe_Debito =  importe_Debito +  %dec(importe:15:2) ;

    //ENDDO;
     exec sql FETCH C1 INTO :line;
    ENDDO;
    exec sql close C1;
   //EXSR cerrar_archivo;

 ENDSR;

//=========================================================================
   // Abre el archivo
//=========================================================================
    BEGSR abrir_archivo;
        fd = ifs_stmf_opnread(%trim(archivo_Recibido));
        if (fd < 0);
           error = '4';
           //die('No se pudo abrir archivo:'+archivo_Recibido);
        ENDIF;
    ENDSR;

//=========================================================================
   //  Cierra el archivo
//=========================================================================
    BEGSR cerrar_archivo;
        ifs_stmf_close(fd);
    ENDSR;

//=================================================
   // Subrutina que termina el programa
//=================================================

BEGSR Fin_de_programa;
        return ;
ENDSR;

end-proc;
/copy sdb01.src/qrpgsrc,ligdimpl ;
