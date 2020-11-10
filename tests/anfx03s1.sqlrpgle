*SRCMBRTXT:Eliminar Retenciones Anteriores - ANSES
**free
ctl-opt
            option(*SRCSTMT)
            dftactgrp(*no)
            main(main);

dcl-c MAX_FILE_SIZE 65535;

//==============================================================================
//Declaración de prototipos, importadas de los miembros declarados.
//==============================================================================

/include sdb01.src/qrpgsrc,ligdifs ;
/include sdb01.src/qrpgsrc,ligdprot ;

//==============================================================================
// Procesos o Funsiones.
//==============================================================================

dcl-proc main;

    dcl-pi *n;
        @CFEPR                packed(6);
    end-pi;

    exec sql SET OPTION COMMIT = *NONE;

    exec sql DELETE FROM BAEXEP03
                    WHERE
                    EXISUB = 'AN' AND
                    EXICAH > 0    AND
                    EXICCC = 0    AND
                    EXIMCC || LPAD(EXIMCA, 3, '0') < :@CFEPR;

//==============================================================================

end-proc;

/copy sdb01.src/qrpgsrc,ligdimpl ;
