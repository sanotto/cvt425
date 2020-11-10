*SRCMBRTXT:Crea archivos csv empleado-emp-tit-SUEL
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


    //*Entry plist
    dcl-pi *n;
         empresa          packed(4:0);
         sub_empresa      packed(4:0);
         producto         packed(3:0);
         sub_producto     packed(3:0);
    end-pi;


//... Variables
    dcl-s cmd                char(4096);
    dcl-s comando_scp        char(4096);
    dcl-s comando_ssh        char(4096);
    dcl-s archivo_qbitemp    char(255);
    dcl-s archivo_qbitit     char(255);
    dcl-s path_archivo       char(250);
    dcl-s usuario            char(10);
    dcl-s descripcion_cmd    char(49);
    dcl-s nombre_sistema     char(8) inz('CUBIX');
    dcl-s hora               packed(6:0);
    dcl-s aasfei             packed(8:0);

//... Llamada al programa 'SGAUTACL'
   dcl-pr ejecutar_programa_SGAUTACL extpgm('SGAUTACL');
        path_archivo      char(250) const;
   end-pr;


//... Llamada al programa 'SGAUTAC1'
   dcl-pr ejecutar_programa_SGAUTAC1 extpgm('SGAUTAC1');
        path_archivo      char(250) const;
   end-pr;

//Aca empieza el programa
exec sql set option commit=*None;
exec sql select aasfei into :aasfei from sgsysv;
exec sql select current_user into :usuario from sysibm.sysdummy1;
    exsr crear_tablas;
    exsr limpia_tablas;
    exsr guardar_datos;
    exsr generar_archivo;
    exsr fin_de_programa;

//==============================================================================
// Creo tabla temporales
//==============================================================================
    begsr crear_tablas;

            exec sql DROP TABLE QTEMP/EMPSUB ;
            exec sql DROP TABLE QTEMP/EMPTIT ;

            exec sql
                CREATE TABLE QTEMP/EMPSUB (
                    empresa                          CHAR (4) ,
                    sub_empresa                      CHAR (4) ,
                    nombre_empresa                   CHAR (30),
                    cuit_empresa                     CHAR (15),
                    producto                         CHAR (4) ,
                    sub_producto                     CHAR (4) ,
                    orden_firmante                   CHAR (1)) ;

           exec sql
                CREATE TABLE QTEMP/EMPTIT (
                    empresa                          CHAR (4),
                    sub_empresa                      CHAR (4),
                    tipo_dni                         CHAR (2),
                    dni_persona                      CHAR (15),
                    nombre_persona                   CHAR (30));
    endsr;

//==============================================================================
// Limpio las tablas temporales
//==============================================================================

    begsr limpia_tablas;
      exec sql DELETE FROM QTEMP/EMPSUB ;
      exec sql DELETE FROM QTEMP/EMPTIT ;
    endsr;

//==============================================================================
// Insert de los datos a tabla temporales
//==============================================================================

    begsr guardar_datos;

       exec sql
            INSERT INTO QTEMP/EMPSUB
             SELECT
             AACDIS,
             AAIBAN,
             QCNRSO,
             QCININ,
             AAIPIS,
             AAIACT,
             CASE WHEN FUIOFI IS NOT NULL THEN FUIOFI
                  ELSE BMIOFI END
             FROM ACEMCE
             INNER JOIN QBIPRO ON AAIPIS = QPIPIS
             INNER JOIN BADCCL ON AAICCL = OTICCL AND
                                  AAISUC = OTISUC
             LEFT  JOIN ACCTAC ON OTICCL = FUICAH AND
                                  OTISUC = FUISUC
             LEFT  JOIN CCCTCT ON OTICCL = BMICCC AND
                                  OTISUC = BMISUC
             INNER JOIN QBSEMP ON AACDIS = QCCDIS
             WHERE
                   AACDIS = :empresa      AND
                   AAIBAN = :sub_empresa  AND
                   AAIPIS = :producto     AND
                   AAIACT = :sub_producto

             GROUP BY
                   AACDIS,
                   AAIBAN,
                   QCNRSO,
                   QCININ,
                   AAIPIS,
                   AAIACT,
                   FUIOFI,
                   BMIOFI;

      exec sql
            INSERT INTO QTEMP/EMPTIT
             SELECT
             AACDIS,
             AAIBAN,
             OTITDO,
             OTINDO,
             A#NYAP
             FROM ACEMCE
             INNER JOIN BADCCL ON AAICCL = OTICCL AND
                                  AAISUC = OTISUC
             INNER JOIN BAPFIS ON OTINDO = A#INDO
             WHERE
                   AACDIS = :empresa      AND
                   AAIBAN = :sub_empresa  AND
                   AAIPIS = :producto     AND
                   AAIACT = :sub_producto
             GROUP BY
                   AACDIS,
                   AAIBAN,
                   OTITDO,
                   OTINDO,
                   A#NYAP;

    endsr;

//==============================================================================
// Genero archivo .CSV
//==============================================================================

    begsr generar_archivo;

    //Genero archivo csv de empresa y producto

    path_archivo = '/home/acreditaciones/EMSUE' + %Editc (empresa: 'X')     +
                                                  %Editc (sub_empresa: 'X') +
                                                  %Editc (producto: 'X' )   +
                                                  %Editc (sub_producto: 'X')+
                                                  '000001.CSV'              ;

    ejecutar_programa_SGAUTAC1(%trim(path_archivo));
    cmd  = 'CPYTOIMPF FROMFILE(QTEMP/EMPSUB)' +
           ' TOSTMF('''+%trim(path_archivo)+''')' +
           ' STMFCCSID(*PCASCII) '             +
           ' RCDDLM(*LF)' ;
    exeCmd(cmd);
    descripcion_cmd = 'Crear archivo ' + path_archivo ;
    exsr Insertar_datos_SGPCLG;
    ejecutar_programa_SGAUTACL(%trim(path_archivo));

    //Genero archivo csv de titulares
    cmd = '';

    path_archivo = '/home/acreditaciones/TISUE' + %Editc (empresa: 'X')     +
                                                  %Editc (sub_empresa: 'X') +
                                                  %Editc (producto: 'X' )   +
                                                  %Editc (sub_producto: 'X')+
                                                  '000001.CSV'              ;
    ejecutar_programa_SGAUTAC1(%trim(path_archivo));
    cmd  = 'CPYTOIMPF FROMFILE(QTEMP/EMPTIT)' +
           ' TOSTMF('''+%trim(path_archivo)+''')' +
           ' STMFCCSID(*PCASCII) '             +
           ' RCDDLM(*LF)' ;
    exeCmd(cmd);
    descripcion_cmd = 'Crear archivo' + path_archivo;
    exsr Insertar_datos_SGPCLG;
    ejecutar_programa_SGAUTACL(%trim(path_archivo));

    endsr;


//==============================================================================
// Fin de programa
//==============================================================================

    begsr fin_de_programa;
        return;
    endsr;

//==============================================================================
// Inserto registro en SGPCLG  cada vez que ejecuto un escrip
//==============================================================================

    begsr Insertar_datos_SGPCLG;

    hora   = %dec(%time():*HMS);

      exec sql INSERT INTO SGPCLG
                           (LGISYS,
                            LGFECH,
                            LGHORA,
                            LGIUSR,
                            LGDPL1)

                     VALUES(
                           :nombre_sistema,
                           :aasfei,
                           :hora,
                           :usuario,
                           :descripcion_cmd);

    endsr;

end-proc;
/copy sdb01.src/qrpgsrc,ligdimpl ;
