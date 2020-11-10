*SRCMBRTXT:LINK-REFRESH DE MOVS DIARIOS PARA HOME 
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DEBUG DATEDIT(*YMD)

     FACCTAC    IF   E           K DISK
     FACMOVD02  IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FCCMOCT01  IF   E           K DISK
     FACCODI    IF   E           K DISK
     FCCCODI    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     FUMOV      O    F  300        DISK    USROPN
     FUMOVU     UF   F  300        DISK    USROPN
     D**********************************************************************
     D* HOJA D => DEFINICIONES DE: A)INTERFACES DE PROCEDIMIENTOS/FUNCIONES
     D*                            B)ESTRUCTURAS DE DATOS
     D*                            C)CONSTANTES
     D*                            D)VARIABLES
     D**********************************************************************
     D* INTERFACES DE PROCEDIMIENTOS
     D**********************************************************************
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D**********************************************************************
     D* ESTRUCTURAS DE DATOS
     D**********************************************************************
     D DSMOV1          DS
     D  V                      1    250
     D                                     DIM(10)
     D  MOVSTR                 1    250
     D CTADS           DS
     D  CTALNK                 1     19
     D  LINSUC                 1      3  0
     D  LINFI2                 4      4
     D  LINGRP                 5      6
     D  LINFIL                 7      9  0
     D  LINCAH                10     16  0
     D*-------------------------------------------------------------------------
     D* PSDS ESTRUCTURA CONTENIENDO INF. DEL ESTADO DEL PROGRAMA
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D @PRC_NAM          *PROC
     D @PGM_STS          *STATUS
     D @PRV_STS               16     20S 0
     D @LIN_NUM               21     28
     D @ROU_NAM          *ROUTINE
     D @PAR_CNT          *PARMS
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D @PGM_LIB               81     90
     D @EXC_DTA               91    170
     D @EXC_IDE              171    174
     D @DAT_RUN              191    198
     D @DAT_YEA              199    200S 0
     D @LAS_FIL              201    208
     D @FIL_INF              209    243
     D @JOB_NAM              244    253
     D @USR_NAM              254    263
     D @JOB_NUM              264    269S 0
     D @JOB_NUM_C            264    269
     D @PJOBN                264    269  0
     D @JOB_DTE              270    275S 0
     D @RUN_DTE              276    281S 0
     D @RUN_TIM              282    287S 0
     D @CRT_DTE              288    293
     D @CRT_TIM              294    299
     D @CPL_LVL              300    303
     D @SRC_FIL              304    313
     D @SRC_MBR              324    333
     D @PRC_PGM              334    343
     D @PRC_MOD              344    353
     D*-----------------------------------------------------------------
     D* ESTRUCTURA PARA SER UTILIZADA CON EL UPDATE DE 1 ARCH.DESC.X PGM
     D*-----------------------------------------------------------------
     D UMOVDS          DS
     D  DSUMOV                 1    300
     D  DSUMTR                18     25  0
     D**********************************************************************
     D* DEFINICION DE CONSTANTES
     D**********************************************************************
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D Symbols         C                   '|°¬!"#$%&/()=?\¡¿*+~[]{}_-;,:.<>'
     D SymBlanks       C                   '                                '
     D Acentos         C                   'ñÑáéíóúäëïöüãõàèìòùâêîôû@'
     D AceBlanks       C                   'nNAEIOUAEIOUAOAEIOUAEIOU '
     D Apos            C                   ''''
     D APosBlank       C                   ' '
     D**********************************************************************
     D* DEFS DE VARIABLES-EN ILE DEFINIR VARS USANDO MOVE/Z-ADD ES MAL STYLE
     D**********************************************************************
     D CMDSTR          S            255A   INZ(*BLANKS)
     D RC              S              7A
     D FILNA1          S             12A
     D FILNA2          S            255A
     D FILNA3          S            255A
     D FILNA4          S            255A
     D X               S              2S 0
     D CNTREG          S              8S 0
     D CANTRE          S              1S 0
     D NUMSEC          S              4S 0
     I*---------------------------------------------------------------------
     I* HOJA I PARA DEFINIR EL FORMATO DE ENTRAD DE UN ARCH. DESC.X PGM
     I*---------------------------------------------------------------------
     IUMOVU     AA
     I                                  1  300  LINUMOV
     I                                 18   25 0TOTLIN
     C*=====================================================================----
     C* PROGRAMA PRINCIPAL
     C*=====================================================================----
     C                   EXSR      WRTHED
     C                   EXSR      WRTMCA
     C                   EXSR      WRTMCC
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* ENDPGM: SE EJECUTA AL LLEGAR AL ULT REG DEL ARCHIVO DE INP PRIMARIO
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   EXSR      ACTHDR
     C* ... AL FINALIZAR CERRAR EL ARCHIVO UMOV TAMBIEN
     C                   CLOSE     UMOV
     C* ... CREAR UN NOMBRE 8.3 PARA LA QDLS QUE TIENE ESA LIMITACION
     C                   EVAL      FILNA1='RHBMC.TXT'
     C* ... CREAR UN NOMBRE LARGO PARA EL IFS
     C                   EVAL      FILNA2='RHBMC'+FEDDMM+'000309.DAT'
     C                   EVAL      FILNA4='RHBMC'+FEDDMM+'000309.ZIP'
     C* ... CREAR UN NOMBRE LARGO PARA EL IFS
     C                   EVAL      FILNA3='RHBMC'+FEDDMM+'000309.CTR'
     C* ... EXPORTAR EL ARCHIVO A LAS CARPETAS COMPARTIDAS
     C                   EVAL      CMDSTR='CPYTOPCD FROMFILE(QTEMP/UMOV)'+
     C                                    '         TOFLR(SCLT000)      '+
     C                                    '         TODOC('+FILNA1+')   '+
     C                                    '         REPLACE(*YES)       '
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... MOVER EL ARCHIVO AL HOME
     C                   EVAL      CMDSTR='rmvlnk ' +
     C                                    ' objlnk(''/home/LINKBEE/'+
     C                                    'REFRESH/'+%TRIM(FILNA2)+''')'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='MOVE OBJ(''/qdls/SCLT000/'    +
     C                                     FILNA1+''')'                  +
     C                                    ' TOOBJ(''/home/LINKBEE/'+
     C                                    'REFRESH/'+%TRIM(FILNA2)+''')'
     C
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... ZIPEAR EL ARCHIVO
     C                   EVAL      CMDSTR='CD DIR(''/home/LINKBEE/REFRESH'')'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='ZIPF ZIPFILE('''+
     C                                     %TRIM(FILNA4)+''') ' +
     C                                    ' FILES('''+%TRIM(FILNA2)+''')'
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... Limpiamos el archivo original
     C                   EVAL      CMDSTR='rmvlnk ' +
     C                                    ' objlnk(''/home/LINKBEE/'+
     C                                    'REFRESH/'+%TRIM(FILNA2)+''')'
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... Limpiamos el archivo umov
     C                   EVAL      CMDSTR='CLRPFM FILE(QTEMP/UMOV)'
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... Crear el Archivo de Control
     C                   OPEN      UMOV
     C                   EXCEPT    FILCNT
     C                   CLOSE     UMOV
     C
     C* ... EXPORTAR EL ARCHIVO A LAS CARPETAS COMPARTIDAS
     C                   EVAL      CMDSTR='CPYTOPCD FROMFILE(QTEMP/UMOV)'+
     C                                    ' TOFLR(SCLT000)      '+
     C                                    ' TODOC('+%TRIM(FILNA1)+')   '+
     C                                    ' REPLACE(*YES)       '
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... MOVER EL ARCHIVO AL HOME
     C                   EVAL      CMDSTR='rmvlnk ' +
     C                                    ' objlnk(''/home/LINKBEE/'+
     C                                    'REFRESH/'+%TRIM(FILNA3)+''')'
     C
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='MOVE OBJ(''/qdls/SCLT000/'    +
     C                                     %TRIM(FILNA1)+''')'     +
     C                                    ' TOOBJ(''/home/LINKBEE/'+
     C                                    'REFRESH/'+%TRIM(FILNA3)+''')'
     C
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... AHORA SI, PODEMOS SALIR
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ACTHDR: ACTUALIZA CABECERA CON TOTAL DE LINEAS
     C*-------------------------------------------------------------------------
     C     ACTHDR        BEGSR
     C*
     C                   EVAL      CMDSTR='OVRDBF FILE(UMOVU)         ' +
     C                                    '       TOFILE(QTEMP/UMOV)  '
     C                   EVAL      RC=SHELL(CMDSTR)
     C*
     C                   OPEN      UMOVU
     C     1             CHAIN     UMOVU                              99
+----C                   IF        *IN99 = *OFF
|    C* ... El reg. de cabecera VA en la cuenta
|    C*                  ADD       1             COUNT
|    C                   Z-ADD     CNTREG        COUNT
|    C*
|    C                   MOVE      LINUMOV       DSUMOV
|    C                   Z-ADD     COUNT         DSUMTR
|    C                   UPDATE    UMOVU         UMOVDS
+----C                   ENDIF
     C*
     C                   CLOSE     UMOVU
     C                   EVAL      CMDSTR='DLTOVR *ALL'
     C                   EVAL      RC=SHELL(CMDSTR)
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* *INZSR: Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C                   MOVE      *BLANKS       RUNMDE            1
     C                   MOVE      *BLANKS       TIPREG            6
     C*
     C     WKEY01        KLIST
     C                   KFLD                    GCISUC
     C                   KFLD                    GCICAH
     C*
     C     WKEY02        KLIST
     C                   KFLD                    CFISUC
     C                   KFLD                    CFICCC
     C*
     C     1             CHAIN     SGSYSV                             25
     C                   MOVE      AASFEI        FEEXT             8 0
     C                   Z-ADD     AASFEI        FECCOR            8 0
     C                   MOVEL     AASFEN        FEDDMM            4
     C*
     C*Cuento la cantidad de movimientos para agregarlos en la cabera
     C*
     C                   ADD       1             CANTRE
     C                   Z-ADD     *ZERO         CNTREG
     C                   EXSR      WRTMCA
     C                   EXSR      WRTMCC
     C*
     C                   Z-ADD     *ZERO         COUNT             7 0
     C* ... BORRAR EL ARCHIVO DE LA QTEMP, SI NO EXISTE DEVUELVE CPF3701 EN
     C*     RC
     C                   EVAL      CMDSTR='DLTF QTEMP/UMOV'
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... CREAR EL ARCHIVO EN LA QTEMP
     C                   EVAL      CMDSTR='CRTPF FILE(QTEMP/UMOV)  '+
     C                                    '      RCDLEN(300)       '
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... ABRIR EL ARCHIVO (ESTO LO CREARA)
     C                   OPEN      UMOV
     C* ... CERRAR PARA CAMBIAR EL MAXIMO DE REGISTROS
     C                   CLOSE     UMOV
     C                   EVAL      CMDSTR='CHGPF FILE(QTEMP/UMOV) ' +
     C                                    '      SIZE(*NOMAX)     '
     C
     C                   EVAL      RC=SHELL(CMDSTR)
     C* ... VOLVER A ABRIR Y DEJAR ABIERTO PARA EL PROCESO
     C                   OPEN      UMOV
     C*
     C                   EXCEPT    HEDREC
     C*
     C                   Z-ADD     *ZERO         CANTRE
     C*                  Z-ADD     *ZERO         CNTREG
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTHED: Escritura Registro Cabecera
     C*-------------------------------------------------------------------------
     C     WRTHED        BEGSR
     C*
     C*
     C                   SETOFF                                       99
     C*
     C                   Z-ADD     2             CNTHEA           15 2
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTMCA: Escribir Movimientos de CA
     C*-------------------------------------------------------------------------
     C     WRTMCA        BEGSR
     C*  ... ESTO POSICIONA EL PTRO DEL ARCHIVO ANTES DEL PRIMER REGISTRO
     C     *LOVAL        SETLL     REACMOVD
     C                   READ      REACMOVD                               99
+----C                   DOW       *IN99 = *OFF
     C     CANTRE        IFEQ      *ZERO
|    C                   EXSR      WRTDCA
     C                   ELSE
     C                   EXSR      CHKMCA
     C                   ENDIF
|    C                   READ      REACMOVD                               99
+----C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CHKMCA: Valida movimientos de CA
     C*-------------------------------------------------------------------------
     C     CHKMCA        BEGSR
     C*
     C     WKEY01        CHAIN     REACCTAC                           25
+----C     *IN25         IFEQ      *ON
|    C                   GOTO      ENDSCA
+----C                   ENDIF
+----C     FUIUCA        IFEQ      'S'
|    C     FUFBAJ        ANDEQ     *ZERO
|    C     FUIBAC        ANDEQ     *ZERO
|    C     FUIGRC        ANDNE     'IN'
     C     GCFING        ANDEQ     FECCOR
     C                   ADD       1             CNTREG
     C                   ENDIF
     C*
     C     ENDSCA        ENDSR
     C*-------------------------------------------------------------------------
     C* WRTDCA: Escribir Detalle de CA
     C*-------------------------------------------------------------------------
     C     WRTDCA        BEGSR
     C*
     C* aqui hay que verificar que tipo de cuenta se trata
     C* cuenta corriente, caja de ahorro, pesos, dolares, etc
     C*
     C*
     C     WKEY01        CHAIN     REACCTAC                           25
+----C     *IN25         IFEQ      *ON
|    C                   GOTO      ENDDCA
+----C                   ENDIF
+----C     FUIUCA        IFEQ      'S'
|    C     FUFBAJ        ANDEQ     *ZERO
|    C     FUIBAC        ANDEQ     *ZERO
|    C     FUIGRC        ANDNE     'IN'
     C     GCFING        ANDEQ     FECCOR
|    C*
|    C* GENERA TIPO DE REGISTRO CONFOR \\ DIFERI
||   C                   MOVE      'CONFOR'      TIPREG
|+---C*    GCFING        IFEQ      GCFASI
||   C*                  MOVE      'CONFOR'      TIPREG
|+---C*                  ENDIF
|+---C*    GCFING        IFLT      GCFASI
||   C*                  MOVE      'DIFERI'      TIPREG
|+---C*                  ENDIF
|    C*
|    C                   Z-ADD     *ZERO         TIPMON            2 0
|+---C     GCIMON        IFEQ      1
||   C                   Z-ADD     *ZERO         TIPMON
||   C                   Z-ADD     1             TIPCUE            1 0
|+---C                   ENDIF
|+---C     GCIMON        IFEQ      2
||   C                   Z-ADD     1             TIPMON
||   C                   Z-ADD     2             TIPCUE            1 0
|+---C                   ENDIF
|    C*
|    C* aqui se calcula el CBU
|    C*
|+---C     FUISGC        IFEQ      'CE'
||   C                   Z-ADD     3             WWTIPO            2 0
|>   C                   ELSE
||   C                   Z-ADD     2             WWTIPO
|+---C                   ENDIF
|    C                   Z-ADD     *ZEROS        WWBLQ1            8 0
|    C                   Z-ADD     *ZEROS        WWBLQ2           14 0
|    C                   Z-ADD     *ZEROS        WWBLQ3           23 0
|    C                   CALL      'CBU000RG'
|    C                   PARM                    WWTIPO
|    C                   PARM                    GCISAL
|    C                   PARM                    GCICAH
|    C                   PARM                    WWBLQ1
|    C                   PARM                    WWBLQ2
|    C                   PARM                    WWBLQ3
|    C*
|    C* AQUI VA UN CHAIN A  CCODI POR EL CAMPO FXITMO PARA DETERMINAR
|    C* MOV DE CREDITO O DEBITO
|    C*
|    C     GCIMCA        CHAIN     REACCODI                           25
|+---C     FXITMO        IFEQ      1
||   C                   MOVE      'D'           TIPMOV            1
|+---C                   ELSE
||   C                   MOVE      'C'           TIPMOV            1
|+---C                   ENDIF
     C                   MOVEL(P)  FXACOD        MOVDSC           50
|    C*
|    C                   ExSr      FixDsc
|    C*
|+---C     X             IFGT      *ZERO
||   C                   ADD       1             CNTREG
||   C                   EXCEPT    DETREC
|+---C                   ENDIF
|    C*
|    C* Nª de cuenta link
|    C*
|    C                   MOVE      *BLANKS       CTALNK
|    C                   Z-ADD     GCISUC        LINSUC
|    C                   MOVE      GCIGRC        LINGRP
|    C                   Z-ADD     GCICAH        LINCAH
|    C                   Z-ADD     *ZEROS        LINFIL
     C                   MOVE      *ZEROS        LINFI2
|    C*
|    C                   Z-ADD     GCICHE        WWICHE           13 0
|    C                   Z-ADD     GC$IMP        WW$IMP           17 2
|    C*
|    C                   ADD       1             NUMSEC
|    C*
|    C                   EXCEPT    DETREC
|    C                   ADD       1             COUNT
+----C                   ENDIF
     C*
     C     ENDDCA        ENDSR
     C*-------------------------------------------------------------------------
     C* WRTMCC: Escribir Movimientos de CC
     C*-------------------------------------------------------------------------
     C     WRTMCC        BEGSR
     C                   SETOFF                                           99
     C*  ... ESTO POSICIONA EL PTRO DEL ARCHIVO ANTES DEL PRIMER REGISTRO
     C     *LOVAL        SETLL     RECCMOCT
     C                   READ      RECCMOCT                               99
+----C                   DOW       *IN99 = *OFF
     C     CANTRE        IFEQ      *ZERO
|    C                   EXSR      WRTDCC
     C                   ELSE
     C                   EXSR      CHKMCC
     C                   ENDIF
|    C                   READ      RECCMOCT                               99
+----C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CHKMCC: Valida movimientos de CC
     C*-------------------------------------------------------------------------
     C     CHKMCC        BEGSR
     C*
     C     WKEY02        CHAIN     RECCCTCT                           25
+----C     *IN25         IFEQ      *ON
|    C                   GOTO      ENDSCC
+----C                   ENDIF
+----C     BMIUCA        IFEQ      'S'
|    C     BMFBAJ        ANDEQ     *ZERO
|    C     BMICBA        ANDEQ     *ZERO
|    C     BMIGRC        ANDNE     'IN'
     C     GCFING        ANDEQ     FECCOR
     C                   ADD       1             CNTREG
     C                   ENDIF
     C*
     C     ENDSCC        ENDSR
     C*-------------------------------------------------------------------------
     C* WRTDCC: Escribir Detalle de CC
     C*-------------------------------------------------------------------------
     C     WRTDCC        BEGSR
     C*
     C* aqui hay que verificar que tipo de cuenta se trata
     C* cuenta corriente, caja de ahorro, pesos, dolares, etc
     C*
     C*
     C                   Z-ADD     *ZERO         X                 2 0
     C*
     C     WKEY02        CHAIN     RECCCTCT                           25
+----C     *IN25         IFEQ      *ON
|    C                   GOTO      ENDDCC
+----C                   ENDIF
+----C     BMIUCA        IFEQ      'S'
|    C     BMFBAJ        ANDEQ     *ZERO
|    C     BMICBA        ANDEQ     *ZERO
|    C     BMIGRC        ANDNE     'IN'
     C     GCFING        ANDEQ     FECCOR
|    C*
|    C* GENERA TIPO DE REGISTRO CONFOR \\ DIFERI
||   C                   MOVE      'CONFOR'      TIPREG
|+---C*    C1FACR        IFEQ      GCFASI
||   C*                  MOVE      'CONFOR'      TIPREG
|+---C*                  ENDIF
|+---C*    GCFING        IFLT      GCFASI
||   C*                  MOVE      'DIFERI'      TIPREG
|+---C*                  ENDIF
|    C*
|    C                   Z-ADD     *ZERO         TIPMON            2 0
|+---C     CFIMON        IFEQ      1
||   C                   Z-ADD     *ZERO         TIPMON
||   C                   Z-ADD     0             TIPCUE            1 0
|+---C                   ENDIF
|+---C     CFIMON        IFEQ      2
||   C                   Z-ADD     1             TIPMON
||   C                   Z-ADD     4             TIPCUE            1 0
|+---C                   ENDIF
|    C*
|    C* aqui se calcula el CBU
|    C*
|    C                   Z-ADD     1             WWTIPO            2 0
|    C                   Z-ADD     *ZEROS        WWBLQ1            8 0
|    C                   Z-ADD     *ZEROS        WWBLQ2           14 0
|    C                   Z-ADD     *ZEROS        WWBLQ3           23 0
|    C                   CALL      'CBU000RG'
|    C                   PARM                    WWTIPO
|    C                   PARM                    CFISUC
|    C                   PARM                    CFICCC
|    C                   PARM                    WWBLQ1
|    C                   PARM                    WWBLQ2
|    C                   PARM                    WWBLQ3
|    C*
|    C* AQUI VA UN CHAIN A CCCODI POR EL CAMPO FXITMO PARA DETERMINAR
|    C* MOV DE CREDITO O DEBITO
|    C*
|    C     CFIMCC        CHAIN     RECCCODI                           25
|+---C     FXITMO        IFEQ      1
||   C                   MOVE      'D'           TIPMOV            1
|+---C                   ELSE
||   C                   MOVE      'C'           TIPMOV            1
|+---C                   ENDIF
     C                   MOVEL(P)  BLACOD        MOVDSC
|    C*
|    C                   ExSr      FixDsc
|    C*
|+---C     X             IFGT      *ZERO
||   C                   ADD       1             CNTREG
||   C                   EXCEPT    DETREC
|+---C                   ENDIF
|    C*
|    C* Nª de cuenta link
|    C*
|    C                   MOVE      *BLANKS       CTALNK
|    C                   Z-ADD     CFISUC        LINSUC
|    C                   MOVE      CFIGRC        LINGRP
|    C                   Z-ADD     CFICCC        LINCAH
|    C                   Z-ADD     *ZEROS        LINFIL
     C                   MOVE      *ZEROS        LINFI2
|    C*
|    C                   Z-ADD     CFICHE        WWICHE           13 0
|    C                   Z-ADD     CF$IMP        WW$IMP           17 2
|    C*
|    C                   ADD       1             NUMSEC
|    C*
|    C                   EXCEPT    DETREC
|    C                   ADD       1             COUNT
+----C                   ENDIF
     C*
     C     ENDDCC        ENDSR
     C*-------------------------------------------------------------------------
     C* FixDsc: Elimina caracteres "raros" de descripción
     C*-------------------------------------------------------------------------
     C     FixDsc        BegSr
     C*
     C                   Eval      MOVDSC = %XLATE(Symbols:SymBlanks:MOVDSC)
     C                   Eval      MOVDSC = %XLATE(Acentos:AceBlanks:MOVDSC)
     C                   Eval      MOVDSC = %XLATE(Apos:AposBlank:MOVDSC)
     C                   Eval      MOVDSC = %XLATE(lo:up:MOVDSC)
     C*
     C                   EndSr
     OUMOV      E            HEDREC
     O                                            6 'CONFOR'
     O                                            9 '309'
     O                       FEEXT               17
     O                       CNTREG              25
     O                                          155 ' '
     O                                          156 '*'
     OUMOV      E            DETREC
     O                       TIPREG               6
     O                                            9 '309'
     O                       TIPCUE              10
     O                       TIPMON              12
     O                       CTALNK              31
     O                       WWBLQ3              54
     O                       GCFING              62
     O                       GCFASI              70
     O                       TIPMOV              71
     O                       WW$IMP              88
     O                       NUMSEC              92
     O                       WWICHE             105
     O                       MOVDSC             155
     O                                          156 '*'
     OUMOV      E            FILCNT
     O                                            5 'RHBMC'
     O                       FEDDMM               9
     O                                           19 '000309.dat'
     O                                           30 ''
     O                                           33 '309'
     O                       FEEXT               41
     O                       CNTREG              49
     P*---------------------------------------------------------------------
     P* PROCEDIMIENTOS DEBEN DECLARARSE DESPUES DE LA HOJA O, SI ESTA EXISTE
     P*---------------------------------------------------------------------
     P Shell           B                   EXPORT
     D  Shell          PI             7A
     D   Command                   1024A   VALUE
     D CommLen         S             15  5

     C                   Call      'QCMDEXC'
     c                   PARM                    Command
     C                   PARM      1024          CommLen
     C                   Return    'CPF0000'
     c     *PSSR         BEGSR
     C                   Return    @EXC_TYP+@EXC_NUM
     c                   ENDSR
     c
     P Shell           E
