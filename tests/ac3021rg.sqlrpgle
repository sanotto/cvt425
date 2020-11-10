*SRCMBRTXT:Emisión de resumenes de cajas de ahorro
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: Caja de Ahorro     - Fin del dia                *
     H*                                                               *
     H*  PROGRAM NAME: Emisión de resumenes de caja de ahorros        *
     H*                                                               *
     H*  PROGRAM NO: AC3021RG                                         *
     H*                                                               *
     H*  DATE: 17/07/1992                                             *
     H*                                                               *
     H*  AUTHOR: Guillermo Pitton - Claudio Romano                    *
     H*                                                               *
     H*  *IN35 Indica que comienza el resumen de otra cuenta          *
     H*  *IN40 Indica sucursal procesable                             *
     H*  *IN50 Indica sucursal listable                               *
     H*  *IN75 Indica que no se debe listar mas movimientos por hoja  *
     H*        llena                                                  *
     H*  *IN91 Indica que hubo al menos un resumen impreso            *
     H*  *IN92 Indica que cambio la sucursal de ingreso               *
     H*                                                               *
     H*  MODIFICADO: Carolina I. Cova - PR00543 - 12.09.2011          *
     H*                                                               *
     H*****************************************************************
     H*  Arreglo de resumenes     FECHA= 01/02/2000; FECHA1=31/01/2000*
     H*****************************************************************
     H*
     H* 1) Copiar ACMOVH, ACMOVH01 y ACMOVH02 de SDBFIL a entorno.
     H* 2) Los contrasientos (iasc=4) cambiarlos por 0.
     H* 3) Desmarcar los movimientos Fecha de alta >= Fecha.
     H* 4) Desmarcar los movimientos Fecha de alta < Fecha y Fecha de
     H*    Asiento >= Fecha.
     H* 5) Copiar ACCTAC de fecha1 a entorno.
     H* 6) Copiar ACHISA de fecha1 a entorno.
     H* 7) Verificar si ACCTAC de fecha1 tiene diferencias con ACHISA
     H*    (FU$SUR y NE$SAL), con QRY (seleccionar solo Sucursal y
     H*    Cuenta todo caracter - DIGITS), en Archivo AC2401 en
     H*    entorno. Correr ACRE024RG de SCORLIB compilado en entorno,
     H*    que actualiza ACCTAC de fecha.
     H* 8) Pasar AC2401 a AC2029 (base de datos) en entorno, corriendo
     H*    el programa ACRE024R1 de SCORLIB compilado en entorno.
     H* 9) Copiar el ACCTAC arreglado en puntos anteriores a ACCTACNO
     H*    de entorno, y copiar ACCTAC de SDBFIL a entorno.
     H*10) Correr el programa ACRE024R2 que actualiza ACCTAC actual en
     H*    entorno con datos de resumen en ACCTACNO, y borra los
     H*    registros de las cuentas con resumenes correctos.
     H*11) Llenar el ACRESU en entorno con programa ACRE024R3 de
     H*    SCORLIB compilado e entorno.
     H*12) Copiar ACTARJ y SGSYSV en entorno con fechas de resumenes
     H*    (actual). Salvar los archivos en archivo AC0081BK/SDBFIL
     H*    (*SAVF).
     H*13) Correr programa AC3021CL/SCORLIB (LDA), compilado en entorno
     H*14) Controlar si estan correctos los datos de resumenes en
     H*    ACCTAC contra el ACHISA con QRY ACRE024QR/LGRLIB, si no hay
     H*    diferencias, continuar con el punto siguiente, en caso
     H*    contrario, verificar el error imprimiendo resumenes de las
     H*    cuentas condiferecias con consultas de saldos historicos
     H*    hasta determinar la diferencia, luego restaurar archivos
     H*    en entorno, desde AC0081BK/SDBFIL, corregir las diferencias
     H*    salvar nuevamente en AC0081BK y correr el punto 13 otra vez.
     H*15) Copiar ACCTAC de entorno en ACCTACNO (REPLACE), sacar
     H*    ACCTAC y ACMOVH, con sus vistas, de entorno, y correr
     H*    programa ACRE024R4 de SCORLIB compilado en entorno (arregla
     H*    ACCTAC de SDBFIL, con datos de resumenes correctos, y marca
     H*    en ACMOVH de SDBFIL los movimientos no emitidos, dejando
     H*    los archivos actualizados correctamente en SDBFIL).
     H*
     H*****************************************************************
     FACRESU    IP   E           K DISK
     FACMOVH01  UF   E           K DISK
     FACCTAC    UF   E           K DISK
     FACDBER    IF   E           K DISK
     FACLEYE    IF   E           K DISK
     FACCODI    IF   E           K DISK
     F*
     F* Desde aqui hasta el próximo espacio son entidades para busqueda
     F* de datos de titulares de cuentas corrientes
     F* La subrutina asociada es PBTITU (Proceso-busca-titular)
     F*
     FBACIVA    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     FBAPJUR    IF   E           K DISK
     FBALOCA    IF   E           K DISK
     FBADUCP01  IF   E           K DISK
     FBASUCU    IF   E           K DISK
     FBAMONE    IF   E           K DISK
 GDE FBAORFI    IF   E           K DISK
     FACTARJ    IF   E           K DISK
     FACTASU01  IF   E           K DISK
     FBADIPF04  IF   E           K DISK
     FBADIPJ04  IF   E           K DISK
     FBAMSRE    IF   E           K DISK
     FBAENTI    IF   E             DISK
     F*BATEMP    IF   E             DISK    USROPN
     FSGSYSV    IF   E             DISK
 GDE FBAHILI    O    E             DISK
     FAC3021P0  O    E             PRINTER OFLIND(*IN81)
     FAC0021P1  O    E             PRINTER OFLIND(*IN82)
     F*----------------------------------------------------------------*
     D @COD            S             39    DIM(5000)
     D @REV            S            120    DIM(46)
     D*----------------------------------------------------------------*
     D                 DS
     D  DS@COD                 1     39
     D  DSITMO                 1      1  0
     D  DSACOD                 2      8
     D  DSNCOD                 9     38
     D  DSIGRE                39     39  0
     D*----------------------------------------------------------------*
     D                 DS
     D  DSFECI                 1      8  0
     D  DSAÑOI                 1      4  0
     D  DSMESI                 5      6  0
     D  DSDIAI                 7      8  0
     D*----------------------------------------------------------------*
     D                 DS
     D  DSFECN                 1      8  0
     D  DSDIAN                 1      2  0
     D  DSMESN                 3      4  0
     D  DSAÑON                 5      8  0
GDE  D*----------------------------------------------------------------*
GDE  D                 DS
GDE  D  wkfede                 1      8  0
GDE  D  anodes                 3      4  0
GDE  D  mesdes                 5      6  0
GDE  D*----------------------------------------------------------------*
GDE  D                 DS
GDE  D  wkfeha                 1      8  0
GDE  D  anohas                 3      4  0
GDE  D  meshas                 5      6  0
GDE  D*----------------------------------------------------------------*
GDE  D CompileData     DS
GDE  D                               27a   Inz('ENERO    FEBRERO  MARZO    ')
GDE  D                               27a   Inz('ABRIL    MAYO     JUNIO    ')
GDE  D                               27a   Inz('JULIO    AGOSTO   SETIEMBRE')
GDE  D                               27a   Inz('OCTUBRE  NOVIEMBREDICIEMBRE')
GDE  D  MonthNames                    9a   Overlay(CompileData) Dim(12)
     d*
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
GDE  D WWNMES          S             27a
     D DSDES1          C                   CONST('Caja de Ahorro .:   ')
     D DSDES2          C                   CONST('Ctas.Ctes.Espec.:   ')
     I*----------------------------------------------------------------*
     IREACRESU      01
     I                                          OIISUC        L9M9
     I                                          OIIMON        L8M8
     I                                          OIICAH        L7M7
     I*----------------------------------------------------------------*
     C*
+----C     *IN99         IFEQ      *OFF
     C                   EXSR      INICIO
+----C                   END
     C*
     C   L9              EXSR      D9ISUC
     C   L8
     CAN 40              EXSR      D8IMON
     C   L7
     CAN 40              EXSR      D7ICAH
     C* ... Sucursal procesable y cta no de baja
     C   40
+----CANN41FUISUC        IFNE      W1ISUC
     C     FUIMON        ORNE      W1IMON
     C     FUIGRC        ORNE      W1IGRC
     C     FUISGC        ORNE      W1ISGC
     C                   Z-ADD     FUISUC        W1ISUC
     C                   Z-ADD     FUIMON        W1IMON
     C                   MOVE      FUIGRC        W1IGRC
     C                   MOVE      FUISGC        W1ISGC
     C                   EXSR      DBUSCA
+----C                   END
     C* ... Por cada Cta no de Baja que pertenezca a Suc. Procesable
     C   01
     CAN 40
     CANN41              EXSR      PPROCE
     C*
     CL7 40
     CAN 99
     CANN41              EXSR      T7ICAH
GDE  C*
GDE  CLR 99              EXSR      WrtEndLst
     C*----------------------------------------------------------------*
     C* D9ISUC - Busca datos de sucursal
     C*----------------------------------------------------------------*
     C     D9ISUC        BEGSR
     C*
     C                   Z-ADD     *ZERO         WWIPA1
     C                   MOVE      *ON           *IN92
     C     OIISUC        CHAIN     BASUCU                             80
     C     @KEY10        CHAIN     ACTASU01                           80
+----C     *IN80         IFEQ      *OFF
     C                   MOVE      *ON           *IN40
+----C                   ELSE
     C                   MOVE      *OFF          *IN40
+----C                   END
+----C     *IN80         IFEQ      *OFF
     C     KPOPTL        ANDEQ     'S'
     C                   MOVE      *ON           *IN50
+----C                   ELSE
     C                   MOVE      *OFF          *IN50
+----C                   END
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* D8IMON - Busca datos de moneda
     C*----------------------------------------------------------------*
     C     D8IMON        BEGSR
     C*
     C     OIIMON        CHAIN     BAMONE                             80
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* D7ICAH - Busca datos de cuenta
     C*----------------------------------------------------------------*
     C     D7ICAH        BEGSR
     C*
     C*
     C                   Z-ADD     -1            L7QMOV
     C                   MOVE      *ON           *IN35
     C     @KEY03        CHAIN     ACCTAC                             80
+----C     FUFBAJ        IFGT      0
     C                   MOVE      *ON           *IN41
     C                   GOTO      ENDD7
+----C                   ELSE
     C                   MOVE      *OFF          *IN41
+----C                   ENDIF
     C                   MOVEL     'AC'          PAISUB            2
     C                   MOVEL     FUIGRC        PAIGRC            2
     C                   MOVEL     FUISGC        PAISGC            2
     C                   Z-ADD     FUICAH        PAICAH           11 0
     C                   MOVEL     *BLANKS       PADESC
     C                   CALL      'BAPR00R5'
     C                   PARM                    PAISUB
     C                   PARM                    PAIGRC
     C                   PARM                    PAISGC
     C                   PARM                    PAICAH
     C                   PARM                    PADESC
+----C*          FUISGC    IFEQ 'CE'                       ---------.
     C*                    MOVELWWDES2    WWDESC           SC 04/09
+----C*                    ELSE                                     <
     C*                    MOVELWWDES1    WWDESC           SC 04/09
+----C*                    ENDIF                           ---------'
     C*          ......    Busca CBU
     C                   EXSR      SRCBU
     C*          ......    Busca titular
     C                   EXSR      PBTITU
     C                   Z-ADD     1             WWPAGE
     C     FUIHUR        ADD       1             W1PAGE
GDE  C* ... Recupera orden de Firmantes
GDE  C     FUIOFI        CHAIN     REBAORFI                           80
GDE  C*
GDE  C                   EXSR      WrtBegCbte
GDE  C*
     C*          ......    Comienza resumen para la cuenta
     C                   EXSR      SR0006
     C*          ......    Imprime saldo del resumen anterior
     C                   EXSR      SR0008
     C*          ......    Imprime reporte de resumenes del banco
     C                   EXSR      SR0001
     C*
     C     ENDD7         TAG
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* DBUSCA - Busca datos de RESUMEN
     C*----------------------------------------------------------------*
     C     DBUSCA        BEGSR
     C* .... Busca par.para generación de movimientos para sucursal
     C                   Z-ADD     OIISUC        WWISUC
     C     @KEY02        CHAIN     ACDBER                             80
+----C     *IN80         IFEQ      *ON
|    C     WWISUC        ORNE      BPISUC
|    C     OIIMON        ORNE      BPIMON
|    C* .... Busca par. para generación de movimientos para banco
|    C                   Z-ADD     99999         WWISUC
|    C     @KEY02        CHAIN     ACDBER                             80
+----C                   END
     C*
     C                   MOVEL     AMAMON        WWAMO1
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* PPROCE - Procesa movimientos por cuenta.
     C*----------------------------------------------------------------*
     C     PPROCE        BEGSR

     C                   Z-ADD     *ZEROS        WWTOTD
     C                   Z-ADD     *ZEROS        WWTOTC
     C     @KEY12        CHAIN     ACMOVH01                           79
     C     *IN79         DOWEQ     *OFF
     C                   Z-ADD     GDIMCA        X
     C                   MOVEL     @COD(X)       DS@COD
     C                   SELECT
     C     GDIMCA        WHENEQ    10
     C                   Z-ADD     2             TPOCTA            2 0
     C                   MOVE      *BLANKS       TRADSC           21
     C                   Z-ADD     GDFASI        PAFECH            8 0
     C                   CALL      'BAPR00RG'
     C                   PARM                    TPOCTA
     C                   PARM                    GDICAH
     C                   PARM                    GDICHE
     C                   PARM                    PAFECH
     C                   PARM                    TRADSC
     C     TRADSC        IFNE      *BLANKS
     C                   MOVEL     TRADSC        WWNCO1
     C                   MOVEL     TRADSC        WWNCOD
     C                   ELSE
     C                   MOVEL     DSNCOD        WWNCO1
     C                   MOVEL     DSNCOD        WWNCOD
     C                   ENDIF
     C     GDIMCA        WHENEQ    613
     C                   MOVE      *BLANKS       TRADS1           27
     C                   CALL      'BAPR00R1'
     C                   PARM                    GDISUC
     C                   PARM                    GDICAH
     C                   PARM                    GDFASI
     C                   PARM                    GDICHE
     C                   PARM                    GD$IMP
     C                   PARM                    TRADS1
     C     TRADS1        IFNE      *BLANKS
     C                   MOVEL     TRADS1        WWNCO1
     C                   MOVEL     TRADS1        WWNCOD
     C                   ELSE
     C                   MOVEL     DSNCOD        WWNCO1
     C                   MOVEL     DSNCOD        WWNCOD
     C                   ENDIF
     C     GDIMCA        WHENEQ    247
     C     GDIMCA        OREQ      248
     C                   Z-ADD     GD$IMP        WW$LQ2           12 2
     C*
     C                   Z-ADD     GDFASI        WWFASI            8 0
     C                   CALL      'SBBAINFE'
     C                   PARM                    WWFASI
     C                   PARM      'IN'          PACINV            2
     C*
     C                   CALL      'BAPR00R4'
     C                   PARM                    GDISUC
     C                   PARM                    GDICAH
     C                   PARM                    WWFASI
     C                   PARM                    GDICHE
     C                   PARM                    WW$LQ2
     C                   PARM                    WWNCOD
     C     WWNCOD        IFNE      *BLANKS
     C                   MOVEL     WWNCOD        WWNCO1
     C                   ELSE
     C                   MOVEL     DSNCOD        WWNCO1
     C                   MOVEL     DSNCOD        WWNCOD
     C                   ENDIF
     C     GDIMCA        WHENEQ    480
     C     GDIMCA        OREQ      481
     C     GDIMCA        OREQ      482
     C                   MOVE      *BLANKS       WWNCOD
     C                   MOVEL     'AC'          PAISUB
     C                   Z-ADD     GDICHE        PAIULN           15 0
     C                   CALL      'BAPR00R6'
     C                   PARM                    PAISUB
     C                   PARM                    GDFASI
     C                   PARM                    GDISUC
     C                   PARM                    GDICAH
     C                   PARM                    PAIULN
     C                   PARM                    GD$IMP
     C                   PARM                    WWNCOD
     C     WWNCOD        IFNE      *BLANKS
     C                   MOVEL     WWNCOD        WWNCO1
     C                   ELSE
     C                   MOVEL     DSNCOD        WWNCO1
     C                   MOVEL     DSNCOD        WWNCOD
     C                   ENDIF
     C                   OTHER
     C                   MOVEL     DSNCOD        WWNCO1
     C                   MOVEL     DSNCOD        WWNCOD
     C                   ENDSL
     C*                                                              %TRIM
     C     GDFASI        IFLE      HDFPAC
     C     GDFASI        ANDLE     OIFHAS
     C     GDIERE        ANDEQ     *ZERO
     C     DSIGRE        ANDEQ     *ZERO
     C   35
     CORN75              DO
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN75
     C     GDIASC        IFEQ      3
     C*          ......    Actualiza como emitido
     C                   EXSR      SR0003
     C                   ELSE
     C*          ......    Procesa todo
     C                   EXSR      SR0005
     C                   END
     C                   END
     C*
     C     GDIASC        IFEQ      0
     C     GDIMCA        IFEQ      550
     C                   ADD       GD$IMP        WWTOTD
     C                   END
     C     GDIMCA        IFEQ      551
     C     GDIMCA        OREQ      552
     C     GDIMCA        OREQ      553
     C                   ADD       GD$IMP        WWTOTC
     C                   END
     C                   END
     C                   END
     C     @KEY12        READE     ACMOVH01                               79
     C                   ENDDO
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* T7ICAH - Actualiza movimientos del dia e históricos.
     C*----------------------------------------------------------------*
     C     T7ICAH        BEGSR
     C*          ......    Actualiza cuenta corriente
     C                   EXSR      SR0004
     C*          ......    Finaliza impresión de resumen para la cuenta
     C                   EXSR      SR0011
GDE  C*          ......    Escribe End para cbte  para Link GDE
GDE  C                   EXSR      WrtEndCbte
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0001
     C*-------------------------------------------------------------'
     C     SR0001        BEGSR
     C* ... Por Cambio de Suc/Fin de Pagina/o Cambio de Mon => Impr Header
     C   92
     CAN 50
     COR 82
     CAN 50
     COR L8
+----CAN 50              DO
     C                   ADD       1             WWIPA1
     C                   WRITE     @HEA10
     C                   MOVE      *OFF          *IN81
     C                   MOVE      *OFF          *IN92
     C                   MOVE      *ON           *IN91
+----C                   END
     C   50              WRITE     @DET10
     C   50              MOVE      *ON           *IN91
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* PBTITU - Busca datos para personas jurídicas
     C*----------------------------------------------------------------*
     C     PBTITU        BEGSR
     C*
     C     @KEY11        CHAIN     BAICCL                             80
     C                   MOVE      OSNCCL        WWTITU
+----C     OSITIN        IFEQ      *ZERO
     C     OSININ        ANDEQ     *ZERO
     C* Busca datos personas físicas
     C                   EXSR      PPFISI
+----C                   ELSE
     C* Busca datos personas jurídicas
     C                   EXSR      PPJURI
+----C                   END
     C*
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* PPJURI - Busca datos para personas jurídicas
     C*----------------------------------------------------------------*
     C     PPJURI        BEGSR
     C*          ......    Accede a personas jurídicas
     C     @KEY07        CHAIN     BAPJUR                             80
+----C     AOICIV        IFNE      HJICIV
     C                   Z-ADD     AOICIV        HJICIV
     C     HJICIV        CHAIN     BACIVA                             80
+----C                   END
     C                   MOVE      AONRSO        WWTITU
     C                   Z-ADD     1             WWITDM
     C*          ......    Busca dirección de persona jurídica
     C     @KEY08        CHAIN     BADIPJ04                           80
     C   80@KEY07        CHAIN     BADIPJ04                           80
     C*
     C                   Z-ADD     AFIPAI        WWIPAI
     C                   Z-ADD     AFICPO        WWICPO
+----C     AFISCP        IFNE      *ZERO
     C                   Z-ADD     AFISCP        WWISCP
     C     @KEY90        CHAIN     BADUCP01                           80
     C                   MOVEL     OXNLOC        WWNLOC
+----C                   ELSE
     C     @KEY09        CHAIN     BALOCA                             80
     C                   MOVEL     ALNLOC        WWNLOC
+----C                   END
     C*
GDE  C                   MOVE      *BLANKS       WWNCAL
GDE  C                   MOVE      *BLANKS       WWIPUE
GDE  C                   MOVE      *BLANKS       WWIBIS
GDE  C                   MOVE      *BLANKS       WWIPLA
GDE  C                   MOVE      *BLANKS       WWIPIS
GDE  C*
+----C     *IN80         IFEQ      *OFF
     C                   CALL      'SBBADIRE'
     C                   PARM                    AFNCAL
     C                   PARM                    AFIPUE
     C                   PARM                    AFIBIS
     C                   PARM                    AFIPLA
     C                   PARM                    AFIPIS
     C                   PARM                    AFIDPT
     C                   PARM                    PAIRES           61
GDE  C*
GDE  C                   MOVE      AFNCAL        WWNCAL
GDE  C                   MOVE      AFIPUE        WWIPUE
GDE  C                   MOVE      AFIBIS        WWIBIS
GDE  C                   MOVE      AFIPLA        WWIPLA
GDE  C                   MOVE      AFIPIS        WWIPIS
GDE  C                   MOVE      AFIDPT        WWIDPT
+----C                   ELSE
     C                   MOVE      *BLANK        PAIRES
+----C                   END
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* PPFISI - Busca datos para personas físicas
     C*----------------------------------------------------------------*
     C     PPFISI        BEGSR
     C*          ......    Accede a personas físicas
     C     @KEY11        CHAIN     BADCCL01                           80
     C     @KEY05        CHAIN     BAPFIS                             80
+----C     AÑICIV        IFNE      HJICIV
     C                   Z-ADD     AÑICIV        HJICIV
     C     HJICIV        CHAIN     BACIVA                             80
+----C                   END
     C                   Z-ADD     1             WWITDM
     C*          ......    Si es InZano :-) Dir del Curador
+----C     AÑIGRC        IFEQ      'IZ'
     C                   Z-ADD     20            WWITTL
     C     WKEY11        CHAIN     BADCCL01                           80
     C     @KEY05        CHAIN     BAPFIS                             80
+----C                   ENDIF
     C*          ......    Busca dirección de persona físicas
     C     @KEY06        CHAIN     BADIPF04                           80
     C   80@KEY05        CHAIN     BADIPF04                           80
     C                   Z-ADD     AEIPAI        WWIPAI
     C                   Z-ADD     AEICPO        WWICPO
+----C     AEISCP        IFNE      *ZERO
     C                   Z-ADD     AEISCP        WWISCP
     C     @KEY90        CHAIN     BADUCP01                           80
     C                   MOVEL     OXNLOC        WWNLOC
+----C                   ELSE
     C     @KEY09        CHAIN     BALOCA                             80
     C                   MOVEL     ALNLOC        WWNLOC
+----C                   END
     C*
GDE  C                   MOVE      *BLANKS       WWNCAL
GDE  C                   MOVE      *BLANKS       WWIPUE
GDE  C                   MOVE      *BLANKS       WWIBIS
GDE  C                   MOVE      *BLANKS       WWIPLA
GDE  C                   MOVE      *BLANKS       WWIPIS
GDE  C*
+----C     *IN80         IFEQ      *OFF
     C                   CALL      'SBBADIRE'
     C                   PARM                    AENCAL
     C                   PARM                    AEIPUE
     C                   PARM                    AEIBIS
     C                   PARM                    AEIPLA
     C                   PARM                    AEIPIS
     C                   PARM                    AEIDPT
     C                   PARM                    PAIRES           61
GDE  C*
GDE  C                   MOVE      AENCAL        WWNCAL
GDE  C                   MOVE      AEIPUE        WWIPUE
GDE  C                   MOVE      AEIBIS        WWIBIS
GDE  C                   MOVE      AEIPLA        WWIPLA
GDE  C                   MOVE      AEIPIS        WWIPIS
GDE  C                   MOVE      AEIDPT        WWIDPT
+----C                   ELSE
     C                   MOVE      *BLANK        PAIRES
+----C                   END
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0002 -
     C*----------------------------------------------------------------*
     C     SR0002        BEGSR
     C*          ......    Actualiza movimientos emitidos
     C                   Z-ADD     1             GDIERE
     C                   UPDATE    REACMOVH
     C*          ......    Controla asientos contrasentados
+----C     GDIASC        IFNE      *ZERO
     C                   Z-SUB     GD$IMP        WW$IMP
     C                   MOVE      *ON           *IN70
+----C                   ELSE
     C                   MOVE      *OFF          *IN70
     C                   Z-ADD     GD$IMP        WW$IMP
+----C                   END
     C*          ......    Acumula en sado último resumen
+----C     DSITMO        IFEQ      1
     C                   MOVEL     *ON           *IN71
     C                   Z-ADD     GD$IMP        WW$ID1
     C                   Z-ADD     *ZERO         WW$IH1
     C                   SUB       WW$IMP        FU$SUR
+----C                   ELSE
     C                   MOVEL     *OFF          *IN71
     C                   Z-ADD     GD$IMP        WW$IH1
     C                   Z-ADD     *ZERO         WW$ID1
     C                   ADD       WW$IMP        FU$SUR
+----C                   END
     C*          ......    Mantiene saldo anterior por traslado
     C                   Z-ADD     WW$SA1        WW$AA1
     C*
     C                   Z-ADD     GDICHE        WWICH1
     C*
     C                   Z-ADD     FU$SUR        WW$SA1
     C*          ......    Imprime movimiento
     C                   EXSR      SR0007
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0003 -
     C*----------------------------------------------------------------*
     C     SR0003        BEGSR
     C*          ......    Actualiza movimientos emitidos
     C                   Z-ADD     1             GDIERE
     C                   UPDATE    REACMOVH
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0004 -
     C*----------------------------------------------------------------*
     C     SR0004        BEGSR
     C*
     C                   ADD       WWPAGE        FUIHUR
     C                   Z-ADD     HDFPAC        FUFURE
     C                   UPDATE    REACCTAC
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0005 -
     C*----------------------------------------------------------------*
     C     SR0005        BEGSR
     C*
     C                   Z-ADD     GDFASI        DSFECI
     C                   Z-ADD     DSAÑOI        DSAÑON
     C                   Z-ADD     DSMESI        DSMESN
     C                   Z-ADD     DSDIAI        DSDIAN
     C                   Z-ADD     DSFECN        WWFAS1
     C*
+----C     BPQREM        IFEQ      0
     C                   Z-ADD     29            BPQREM
+----C                   ENDIF
     C*
     C                   ADD       1             L7QMOV
     C     L7QMOV        DIV       BPQREM        WWIPAG
     C*          ......    Marca movimiento y calcúla
     C                   EXSR      SR0002
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0006 - COMIENZA RESUMEN PARA LA CUENTA
     C*----------------------------------------------------------------*
     C     SR0006        BEGSR
     C*
     C                   MOVEL     WWTITU        WWTIT1
     C                   MOVEL     PAIRES        PAIRE1           50
     C                   Z-ADD     FUINCT        WWICA1
     C                   Z-ADD     WWICPO        WWICP1
     C                   MOVEL     WWNLOC        WWNLO1
     C                   Z-ADD     W1PAGE        WWQPA1
     C*
     C   50              EXSR      BLDPID
     C   50              WRITE     @HEAD1
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0007 -
     C*----------------------------------------------------------------*
     C     SR0007        BEGSR
     C*
+----C     WWIPAG        IFEQ      WWPAGE
     C*          ......    Imprime continuación de hoja
     C                   EXSR      SR0010
     C     WWIPAG        ADD       1             WWPAGE
     C                   ADD       1             W1PAGE
     C*          ......    Imprime cabecera del resumen
     C                   EXSR      SR0006
     C*          ......    Imprime transporte de hoja
     C                   EXSR      SR0009
+----C                   ENDIF
     C   50              WRITE     @DETA1
     C*
+----C     *IN82         IFEQ      *ON
     C                   MOVEL     *OFF          *IN82
     C                   ADD       1             WWIPA1
     C                   WRITE     @HEA10
+----C                   ENDIF
     C                   WRITE     @DET11
     C*
GDE  C                   EXSR      WrtDetMov
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C*
     C*----------------------------------------------------------------*
     C     BLDPID        BEGSR
     C*
     C                   MOVEL     FUISUC        WWTMP1           16
     C                   MOVE      FUICAH        WWTMP1
     C                   MOVE      WWQPA1        WWTMP2           21
     C                   MOVEL     WWTMP1        WWTMP2           21
     C                   MOVE      WWTMP2        WWPIDE           24
     C                   MOVEL     'CPA'         WWPIDE           24
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0008 -
     C*----------------------------------------------------------------*
     C     SR0008        BEGSR
     C*
     C                   Z-ADD     FUFURE        DSFECI
     C                   Z-ADD     DSAÑOI        DSAÑON
     C                   Z-ADD     DSMESI        DSMESN
     C                   Z-ADD     DSDIAI        DSDIAN
     C                   Z-ADD     DSFECN        WWFUR1
     C                   Z-ADD     FU$SUR        WW$SA1
     C   50              WRITE     @DETA2
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0009 -
     C*----------------------------------------------------------------*
     C     SR0009        BEGSR
     C*
     C   50              WRITE     @DETA3
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0010 -
     C*----------------------------------------------------------------*
     C     SR0010        BEGSR
     C*
+----C     *IN50         IFEQ      *ON
     C                   WRITE     @DETA4
+----C                   ENDIF
+----C     WW$AA1        IFGE      *ZERO
     C     WW$AA1        ORLE      *ZERO
     C                   Z-ADD     WW$AA1        WW$TH1
     C                   Z-ADD     *ZERO         WW$TD1
     C                   MOVE      AMAMON        WWAMOH
+----C                   ELSE
+----C                   END
     C                   WRITE     @DETA5
     C                   EXSR      REVERS
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SR0011 -
     C*----------------------------------------------------------------*
     C     SR0011        BEGSR
     C*
+----C     FU$SUR        IFGE      *ZERO
     C     FU$SUR        ORLE      *ZERO
     C                   Z-ADD     FU$SUR        WW$TH1
     C                   Z-ADD     *ZERO         WW$TD1
     C                   MOVE      AMAMON        WWAMOH
+----C                   ELSE
+----C                   END
     C*
+----C     WWTOTD        IFNE      0
     C     WWTOTC        ORNE      0
     C   50WWTOTC        MULT      0.34          WW1788
     C   50              WRITE     @DETA6
+----C                   ENDIF
     C   50              WRITE     @DETA5
     C   50              EXSR      REVERS
     C*
+----C     *IN82         IFEQ      *ON
     C                   MOVEL     *OFF          *IN82
     C                   ADD       1             WWIPA1
     C                   WRITE     @HEA10
+----C                   ENDIF
     C                   WRITE     @TL710
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para inicio del programa
     C*----------------------------------------------------------------*
     C     INICIO        BEGSR
     C*
     C                   MOVE      *ON           *IN99
     C                   EXSR      SRDEFN
     C                   IN        LDA
     C     JAICTM        CHAIN     ACLEYE                             80
     C     EQIPRC        CHAIN     ACTARJ                             80
     C     1             CHAIN     SGSYSV                             80
     C     1             CHAIN     BAENTI                             80
     C                   Z-ADD     HDFPAC        DSFECI
     C                   Z-ADD     DSAÑOI        DSAÑON
     C                   Z-ADD     DSMESI        DSMESN
     C                   Z-ADD     DSDIAI        DSDIAN
     C                   Z-ADD     DSFECN        WWFPAC
     C     99999999      SUB       HDFPAC        WWSFEI
     C                   Z-ADD     OIFHAS        DSFECI
     C                   Z-ADD     DSAÑOI        DSAÑON
     C                   Z-ADD     DSMESI        DSMESN
     C                   Z-ADD     DSDIAI        DSDIAN
     C                   Z-ADD     DSFECN        WWFPC1
     C                   MOVEL     DSDES1        WWDES1           17
     C                   MOVEL     DSDES2        WWDES2           17
     C                   MOVEL     *BLANK        WWDESC           17
     C*
     C                   Z-ADD     *ZERO         FXIMCA
     C     FXIMCA        SETLL     ACCODI
     C                   READ      ACCODI                                 80
+----C     *IN80         DOWEQ     *OFF
     C                   Z-ADD     FXIMCA        X
     C                   Z-ADD     FXITMO        DSITMO
     C                   MOVEL     FXACOD        DSACOD
     C                   MOVEL     FXNCOD        DSNCOD
     C                   Z-ADD     FXIGRE        DSIGRE
     C                   MOVEL     DS@COD        @COD(X)
     C                   READ      ACCODI                                 80
+----C                   ENDDO
     C* ... Arma la planilla de Comisiones
     C*                  CALL      'BABZ00RG'
     C*                  PARM      'AC'          SBSPRM            2
     C*                  Z-ADD     *ZERO         I                 2 0
     C*                  OPEN      BATEMP
     C*                  READ      REBATEMP                               98
+----C*    *IN98         DOWEQ     *OFF
     C*                  ADD       1             I
     C*                  MOVEL     B9BLK0        @REV(I)
 +---C*    I             IFGT      46
     C*                  LEAVE
 +---C*                  ENDIF
     C*                  READ      REBATEMP                               98
+----C*                  ENDDO
     C*                  CLOSE     BATEMP
GDE  C* ... Inserta Principio de Archivo GDE LINK
GDE  C                   EXSR      WrtBegLst
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRCBU  - Busca CBU de la Cuenta
     C*----------------------------------------------------------------*
     C     SRCBU         BEGSR
     C*
     C* TIPO: 1 -> CTAS.CTES., 2-> AHORRO, 3->CTAS.CTES.ESP.(AC-CE)
     C* ISUC: SUCURSAL DE LA CUENTA
     C* ICTA: NUMERO DE CUENTA
     C* BLQ1: BLOQUE 1 DE CBU
     C* BLQ2: BLOQUE 2 DE CBU
     C*
     C                   Z-ADD     2             WWTIPO            2 0
+----C     FUISGC        IFEQ      'CE'
     C                   Z-ADD     3             WWTIPO
+----C                   ENDIF
     C                   Z-ADD     *ZEROS        WWBLQ1            8 0
     C                   Z-ADD     *ZEROS        WWBLQ2           14 0
     C                   Z-ADD     *ZEROS        WWBLQ3           22 0
     C                   CALL      'CBU000RG'
     C                   PARM                    WWTIPO
     C                   PARM                    FUISUC
     C                   PARM                    FUICAH
     C                   PARM                    WWBLQ1
     C                   PARM                    WWBLQ2
     C                   PARM                    WWBLQ3
     C*
     C                   ENDSR
     C*-------------------------------------------------------------.
     C* SRDEFN
     C*-------------------------------------------------------------'
     C     SRDEFN        BEGSR
     C*
     C     *DTAARA       DEFINE    *LDA          LDA
     C*
     C     *LIKE         DEFINE    HDFPAC        WWSFEI
     C     *LIKE         DEFINE    OSNCCL        WWTITU
     C     *LIKE         DEFINE    AEICPO        WWICPO
     C     *LIKE         DEFINE    ALNLOC        WWNLOC
     C     *LIKE         DEFINE    KPISUC        WWISUC
     C     *LIKE         DEFINE    FUIHUR        L7QMOV
     C     *LIKE         DEFINE    FUIHUR        L7QMO1
     C     *LIKE         DEFINE    AFITDM        WWITDM
     C     *LIKE         DEFINE    ALIPAI        WWIPAI
     C     *LIKE         DEFINE    OXISCP        WWISCP
     C     *LIKE         DEFINE    GD$IMP        WW$IMP
     C     *LIKE         DEFINE    FUIHUR        WWIPAG
     C     *LIKE         DEFINE    FUIHUR        WWPAGE
     C     *LIKE         DEFINE    FUIHUR        W1PAGE
     C     *LIKE         DEFINE    FUIHUR        W2PAGE
     C     *LIKE         DEFINE    OTITTL        WWITTL
     C     *LIKE         DEFINE    FUISUC        W1ISUC
     C     *LIKE         DEFINE    FUIMON        W1IMON
     C     *LIKE         DEFINE    FUIGRC        W1IGRC
     C     *LIKE         DEFINE    FUISGC        W1ISGC
     C     *LIKE         DEFINE    FXIMCA        X
GDE  C     *LIKE         DEFINE    AFNCAL        WWNCAL
GDE  C     *LIKE         DEFINE    AFIPUE        WWIPUE
GDE  C     *LIKE         DEFINE    AFIBIS        WWIBIS
GDE  C     *LIKE         DEFINE    AFIPLA        WWIPLA
GDE  C     *LIKE         DEFINE    AFIPIS        WWIPIS
GDE  C     *LIKE         DEFINE    AFIDPT        WWIDPT
     C                   Z-ADD     1             WWITTL
     C                   MOVE      'IN'          PACINV            2
     C* .... Acceso a Parámtros emisión de resúmenes cuentas corrientes
     C     @KEY02        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    OIIMON
     C                   KFLD                    FUIGRC
     C                   KFLD                    FUISGC
     C* .... Acceso a maestro de cuentas caja de ahorro
     C     @KEY03        KLIST
     C                   KFLD                    OIISUC
     C                   KFLD                    OIICAH
     C* .... Acceso a relaciones de cuentas personas físicas
     C     @KEY04        KLIST
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICAH
     C* .... Acceso a personas físicas
     C     @KEY05        KLIST
     C                   KFLD                    OTITDO
     C                   KFLD                    OTINDO
     C* .... Acceso a direcciones personas físicas
     C     @KEY06        KLIST
     C                   KFLD                    OTITDO
     C                   KFLD                    OTINDO
     C                   KFLD                    WWITDM
     C* .... Acceso a personas jurídicas
     C     @KEY07        KLIST
     C                   KFLD                    OSITIN
     C                   KFLD                    OSININ
     C* .... Acceso a direcciones personas jurídicas
     C     @KEY08        KLIST
     C                   KFLD                    OSITIN
     C                   KFLD                    OSININ
     C                   KFLD                    WWITDM
     C* .... Acceso a localidades
     C     @KEY09        KLIST
     C                   KFLD                    WWIPAI
     C                   KFLD                    WWICPO
     C* .... Acceso a sub-localidades
     C     @KEY90        KLIST
     C                   KFLD                    WWIPAI
     C                   KFLD                    WWICPO
     C                   KFLD                    WWISCP
     C* .... Acceso a emisión de resumenes.
     C     @KEY10        KLIST
     C                   KFLD                    EQIPRC
     C                   KFLD                    OIISUC
     C* .... Acceso a cuentas clientes
     C     @KEY11        KLIST
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICCL
     C* .... Acceso a cuentas clientes
     C     WKEY11        KLIST
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICCL
     C                   KFLD                    WWITTL
     C* .... Acceso a MENSAJE POR CUENTA
     C     WKEY13        KLIST
     C                   KFLD                    KMISUB
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICAH
     C     WKEY15        KLIST
     C                   KFLD                    KMISUB
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICAH
     C                   KFLD                    AASFEI
     C*
     C* .... Acceso a cuentas clientes
     C     @KEY12        KLIST
     C                   KFLD                    OIISUC
     C                   KFLD                    OIIMON
     C                   KFLD                    OIICAH
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* REVERS -
     C*----------------------------------------------------------------*
     C     REVERS        BEGSR
     C*
     C                   MOVEL     'CPR'         WWPIDE           24
     C*
     C                   WRITE     @REHD1
     C                   GOTO      NOREV
     C                   Z-ADD     1             REVCNT            2 0
     C                   MOVE      'CA'          KMISUB
     C     WKEY15        SETLL     REBAMSRE                           98
     C     WKEY13        READE     REBAMSRE                               98
     C  N98              MOVE      KMFHAS        WWFHAS            8 0
+----C     *IN98         DOWEQ     *OFF
 +---C     KMFHAS        IFGE      AASFEI
     C     KMFHAS        ANDEQ     WWFHAS
     C                   WRITE     @REDE1
     C                   ADD       1             REVCNT
+--  C     REVCNT        IFGT      6
     C                   LEAVE
+--  C                   ENDIF
+-- -C                   ENDIF
     C     WKEY13        READE     REBAMSRE                               98
+----C                   ENDDO
     C                   Z-ADD     1             REVCNT            2 0
+----C     1             DO        46            I                 2 0
     C                   MOVEL(P)  @REV(I)       REVLIN          120
     C                   WRITE     @REDE2
 +---C                   ENDDO
     C*
     C     NOREV         ENDSR
GDE  C*================================================================
 |   C* RUTINAS NUEVAS POR LINK GDE
 v   C*================================================================
     C*
     C*----------------------------------------------------------------
     C* WrtBegLst: Escribe comienzo archivo
     C*----------------------------------------------------------------
     C     WrtBegLst     BEGSR
     C* ... Carga campos iniciales para BAHILI
     C                   EVAL      HLCTRE='LNK'
     C                   EVAL      HLIPGM='LNKAC3021'
     C                   EVAL      HLIFIL='LINKGDEXML'
     C                   EVAL      HLFALT=AASFEI
     C                   EVAL      HLAFB1='A'
     C                   EVAL      HLISUC=0
     C                   EVAL      HLICCC=0
     C                   EVAL      HLIHUR=0
     C                   Z-ADD     OIFHAS        WKFEDE            8 0
     C                   Z-ADD     OIFHAS        WKFEHA            8 0
     C/FREE
        Eval HLDES3='<?xml version="1.0" encoding="utf-8"?>';
        Write REBAHILI;
        Eval HLDES3='<LISTARESUMEN>';
        Write REBAHILI;

     C/END-FREE
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WrtCabecera
     C*----------------------------------------------------------------
     C     WrtCabecera   BEGSR
     C*
     C                   EVAL      HLAFB1='A        '
     C                   MOVE      '11'          WWTICU            2
     C                   IF        FUIMON=2
     C                   MOVE      '15'          WWTICU            2
     C                   ENDIF
     C/FREE
        Eval HLDES3='  <RESUMEN>';
        Write REBAHILI;
        Eval HLDES3='    <CABECERA>';
        Write REBAHILI;
        Eval HLDES3='       <PERIODO>' +
                            %TRIM(%EDITW(AASFEI:'        ')) +
                            '</PERIODO>' ;
        Write REBAHILI;
        Eval HLDES3='       <FRECUENCIA>' +
                            'DI' +
                            '</FRECUENCIA>' ;
        Write REBAHILI;
        Eval HLDES3='       <TIPOCUENTA>'+WWTICU+'</TIPOCUENTA>';
        Write REBAHILI;
        Eval HLDES3='    </CABECERA>';
        Write REBAHILI;
     C/END-FREE
     C*
     C                   ENDSR

     C*----------------------------------------------------------------
     C* WrtEndLst: Escribe fin del archivo
     C*----------------------------------------------------------------
     C     WrtEndLst     BEGSR
     C*
     C                   EVAL      HLFALT=AASFEI
     C                   EVAL      HLAFB1='C'
     C                   EVAL      HLISUC=*HIVAL
     C                   EVAL      HLICCC=*HIVAL
     C                   EVAL      HLIHUR=*HIVAL
     C/FREE
        Eval HLDES3='</LISTARESUMEN>';
        Write REBAHILI;
     C/END-FREE
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WrtBegCbte: Escribe comienzo seccion Cbte del XML
     C*----------------------------------------------------------------
     C     WrtBegCbte    BEGSR
     C*
     C                   EVAL       HLISUC=OIISUC
     C                   EVAL       HLICCC=OIICAH
     C                   EVAL       HLIHUR=W1PAGE
     C                   Eval       HLAFB1='A'
     C*
     C                   EXSR      WrtCabecera
     C*
     C                   ExSr      BldFechas
     C/FREE
        Eval HLDES3='  <ENCABEZADO>';
        Write REBAHILI;
        Eval HLDES3='    <CLIENTE>'             +
                    %TRIM(WWTITU)               +
                        '</CLIENTE>'            ;
        Write REBAHILI;
        Eval HLDES3='    <DOMIC_CALLE_CLI>'+%TRIM(WWNCAL) +'</DOMIC_CALLE_CLI>';
        Write REBAHILI;
        Eval HLDES3='    <DOM_NRO_CLI>'+
                         %TRIM(%EDITC(WWIPUE:'Z'))+
                         '</DOM_NRO_CLI>';
        Write REBAHILI;
        Eval HLDES3='    <DOM_PISO_CLI>'+
                         %TRIM(%EDITC(WWIPIS:'Z'))+
                         '</DOM_PISO_CLI>';
        Write REBAHILI;
        Eval HLDES3='    <DOM_DPTO_CLI>'+%TRIM(WWIDPT)+'</DOM_DPTO_CLI>';
        Write REBAHILI;
        Eval HLDES3='    <DOM_LOC_CLI>'+%TRIM(WWNLOC)+'</DOM_LOC_CLI>';
        Write REBAHILI;
        Eval HLDES3='    <COD_LOC>'+%TRIM(%EDITC(WWICPO:'Z')) +'</COD_LOC>';
        Write REBAHILI;
        Eval HLDES3='    <COD_SUCURSAL>'+%TRIM(%EDITW(OIISUC:'   0 '))+
                        '</COD_SUCURSAL>';
        Write REBAHILI;
        Eval HLDES3='    <SUCURSAL>'+%TRIM(AVNSUC)+'</SUCURSAL>';
        Write REBAHILI;
        Eval HLDES3='    <TIPO_CUEN>'+%TRIM(PADESC)+'</TIPO_CUEN>';
        Write REBAHILI;
        Eval HLDES3='    <MONEDA>'+%TRIM(AMNMON)+'</MONEDA>';
        Write REBAHILI;
       //Eval HLDES3='    <TNRO_CUEN>'+%TRIM(%EDITC(OIICAH:'Z'))+'</TNRO_CUEN>';
       //Write REBAHILI;
        IF OIICAH > 99999999;
        Eval HLDES3='    <TNRO_CUEN>'+
                                    '000'+
                                    %TRIM(%EDITW(OIISUC:'   0 '))+
                                    %TRIM(FUIGRC)+
                                    '000'+
                                    %SUBST(%TRIM(%EDITC(OIICAH:'Z')):3)+
                        '</TNRO_CUEN>';
        Write REBAHILI;
        ELSE;
        Eval HLDES3='    <TNRO_CUEN>'+
                                    '000'+
                                    %TRIM(%EDITW(OIISUC:'   0 '))+
                                    %TRIM(FUIGRC)+
                                    '000'+
                                    %SUBST(%TRIM(%EDITC(OIICAH:'Z')):2)+
                        '</TNRO_CUEN>';
        Write REBAHILI;
        ENDIF;
        Eval HLDES3='    <CBU>'+%TRIM(%EDITC(WWBLQ3:'Z'))+'</CBU>';
        Write REBAHILI;
        //Eval HLDES3='    <FECHA_DESDE>'+%EDITW(WWFDES:'  /  /    ')+
        //                '</FECHA_DESDE>';
        //Write REBAHILI;
        //Eval HLDES3='    <FECHA_HASTA>'+%EDITW(WWFEHA:'  /  /    ')+
        //           '     </FECHA_HASTA>';
        //Write REBAHILI;
        Eval HLAFB1='C';
        Eval HLDES3='    <TIPO_ORDEN>'+%TRIM(BNDOFI)+'</TIPO_ORDEN>';
        Write REBAHILI;
        Eval HLDES3='    <CAT_IVA>'+%TRIM(HJDCIV)+'</CAT_IVA>';
        Write REBAHILI;
     C/END-FREE
     C                   Z-ADD     0             WWCAIN            2 0
     C     @KEY11        CHAIN     REBADCCL                           80
     C                   DOW       *IN80 = *OFF
     C                   ADD       1             WWCAIN
     C     @KEY11        READE     REBADCCL                               80
     C                   ENDDO
     C/FREE
        Eval HLDES3='  <CANT_INTEGRANTES>'+
                        %TRIM( %EDITC(WWCAIN:'Z'))+
                    '</CANT_INTEGRANTES>';
        Write REBAHILI;
        Eval HLAFB1 = 'C';
        Eval HLDES3='  </ENCABEZADO>';
        Write REBAHILI;
        Eval HLDES3='  <INTEGRANTRES>';//No está mal escrito ES ASI segun docs
        Write REBAHILI;
     C/END-FREE
     C                   Z-ADD     *ZERO         WWICNT            3 0
     C                   Z-ADD     1             WWIMAX            3 0
     C     @KEY11        CHAIN     REBADCCL                           80
     C                   DOW       *IN80 = *OFF AND WWICNT < WWIMAX
     C                   ADD       1             WWICNT
     C                   MOVE      *BLANKS       WWTIOR            7
     C                   IF        OTITTL=1
     C                   MOVE      'TIT/ORD'     WWTIOR            7
     c                   ENDIF
     C     @KEY05        CHAIN     REBAPFIS                           80
     C/FREE
        Eval HLDES3='    <INTEGRANTE>';
        Write REBAHILI;
        Eval HLDES3='      <NOMB_INTEGRANTE>'+%TRIM(AÑNYAP)+
                          '</NOMB_INTEGRANTE>';
        Write REBAHILI;
        Eval HLDES3='      <CUIL_INTEGRANTE>'+
                           %TRIM(%EDITC(AÑICUI:'Z'))+
                          '</CUIL_INTEGRANTE>';
        Write REBAHILI;
        Eval HLDES3='      <PART_INTEGRANTE>'+%TRIM(WWTIOR)+
                          '</PART_INTEGRANTE>';
        Write REBAHILI;
        Eval HLDES3='    </INTEGRANTE>';
        Write REBAHILI;
     C/END-FREE
     C     @KEY11        READE     REBADCCL                               80
     C                   ENDDO
     C*
     C/FREE
        Eval HLDES3='  </INTEGRANTRES>'; //Esto NO esta mal escrito
        Write REBAHILI;
        Eval HLDES3='  <MOVIMIENTOS>';
        Write REBAHILI;
        Eval *IN90 = *OFF;
     C/END-FREE
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WrtEndCbte: Escribe final seccion Cbte del XML
     C*----------------------------------------------------------------
     C     WrtEndCbte    BEGSR
     C/FREE
        Eval HLDES3='  </MOVIMIENTOS>';
        Write REBAHILI;

        ExSr WrtAcuMes;

        Eval HLDES3='  </RESUMEN>';
        Write REBAHILI;

        Eval HLAFB1 = 'B';

        Eval HLDES3='    <FECHA_DESDE>'+
                         %TRIM(%EDITW(WWFDES:'  /  /    ')) +
                        '</FECHA_DESDE>';
        Write REBAHILI;

        Eval HLDES3='    <FECHA_HASTA>'+
                         %TRIM(%EDITW(WWFEHA:'  /  /    ')) +
                        '</FECHA_HASTA>';
        Write REBAHILI;

     C/END-FREE
     C                   ENDSR

     C*----------------------------------------------------------------
     C* WrtGDEMov: Escribe detalle del movimiento
     C*----------------------------------------------------------------
     C     WrtDetMov     BEGSR
     C*
     C                   IF        WKFEDE > GDFALT
     C                   EVAL      WKFEDE=GDFALT
     C                   ENDIF
     C                   EVAL      WKFEHA=GDFALT
     C*
     C                   MOVEL     WWFAS1        WWFEMD            8 0
     C  N90              Z-ADD     GDFASI        WWFEDE            8 0
     C  N90              MOVE      *ON           *IN90
     C*
     C/FREE
        Eval HLDES3='    <MOVIMIENTO>';
        Write REBAHILI;
        Eval HLDES3='      <FECHA_MOV>'+
                            %TRIM(%EDITW(WWFEMD:'  /  /    ')) +
                          '</FECHA_MOV>';
        Write REBAHILI;
        Eval HLDES3='      <CONCEPTO>'+%TRIM(WWNCO1)+'</CONCEPTO>';
        Write REBAHILI;
        Eval HLDES3='      <NRO_OPER>'+
                            %TRIM(%EDITC(WWICH1:'Z'))+
                          '</NRO_OPER>';
        Write REBAHILI;
        Eval HLDES3='      <FECHA_PEND/>';
        Write REBAHILI;
        Eval HLDES3='      <DEBITO>'+
                            %TRIM(%EDITW(WW$ID1:'          . 0 ,  -')) +
                          '</DEBITO>';
        Write REBAHILI;
        Eval HLDES3='      <CREDITO>'+
                            %TRIM(%EDITW(WW$IH1:'          . 0 ,  -')) +
                          '</CREDITO>';
        Write REBAHILI;
        Eval HLDES3='      <SALDO>'+
                            %TRIM(%EDITW(WW$SA1:'          . 0 ,  -')) +
                          '</SALDO>';
        Write REBAHILI;
        Eval HLDES3='    </MOVIMIENTO>';
        Write REBAHILI;
     C/END-FREE
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WrtAcuMes: Escribe acumulado del mes
     C*----------------------------------------------------------------
     C     WrtAcuMes     BEGSR
     C*
     C                   ExSr      ClrAcuTot
     C                   EXSR      BldFechas
     C/FREE
        Eval HLDES3='    <ACUMULADOS_DEL_MES>';
        Write REBAHILI;
        DoW anodes*100+mesdes <= anohas*100 +meshas ;

            ExSr ClrAcuVar;
            ExSr GetAcuMes;
            Eval WWNMES= MonthNames(mesdes);
            ExSr AddAcuTot;
            ExSr WrtTotMes;
            mesdes +=1;
            If mesdes > 12;
                anodes +=1;
                mesdes = 1;
            EndIf;
        EndDo;
        Eval WWNMES='TOTALES';
        ExSr WrtTotMes;

        Eval HLDES3='    </ACUMULADOS_DEL_MES>';
        Write REBAHILI;
     C/END-FREE
     c*
     c                   ExSr      WrtLeyendas
     C*
     C                   ENDSR

     C*----------------------------------------------------------------
     C* GetAcuMes: Obtiene acumulados por mes para mes
     C*----------------------------------------------------------------
     C     GetAcuMes     BEGSR
     C*
     C                   Move      anodes        DSAÑOI
     C                   Move      mesdes        DSMESI
     C                   Move      01            DSMESI
     C                   Move      dsfeci        fmesde            8 0
     C                   Move      anodes        DSAÑOI
     C                   Move      mesdes        DSMESI
     C                   Move      31            DSMESI
     C                   Move      dsfeci        fmesha            8 0
     C*
     C/EXEC SQL
     C+   SELECT
     C+       IFNULL(SUM(CASE WHEN GDIMCA = 21  THEN GD$IMP ELSE 0 END), 0)
     C+       AS TOTIVA,
     C+       IFNULL(SUM(CASE WHEN GDIMCA = 550 THEN GD$IMP ELSE 0 END), 0)
     C+       AS TOTLEY,
     C+       IFNULL(SUM(CASE WHEN GDIMCA = 554 THEN GD$IMP ELSE 0 END), 0)
     C+       AS TOTIBR,
     C+       IFNULL(SUM(CASE WHEN GDIMCA = 555 THEN GD$IMP ELSE 0 END), 0)
     C+       AS TOTSIR,
     C+       IFNULL(SUM(CASE WHEN GDIMCA = 551 OR
     C+                            GDIMCA=552 THEN GD$IMP ELSE 0 END), 0)
     C+       AS TOTCRE
     C+       INTO
     C+       :TOTIVA,
     C+       :TOTLEY,
     C+       :TOTIBR,
     C+       :TOTSIR,
     C+       :TOTICR
     C+   FROM
     C+       ACMOVH
     C+   WHERE
     C+           GDISUC=:OIISUC
     C+       AND GDICAH=:OIICAH
     C+       AND GDFALT BETWEEN :FMESDE AND :FMESHA
     C/END-EXEC
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WrtTotMes: Escribe Total del mes
     C*----------------------------------------------------------------
     C     WrtTotMes     BEGSR
     C*
     C/FREE
        Eval HLDES3='      <TOTALMES>';
        Write REBAHILI;
        Eval HLDES3='        <MES>'+%TRIM(WWNMES)+'</MES>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_IVA>'+
                                %TRIM(%EDITW(ACUIVA:'          . 0 ,  -'))+
                            '</TOTAL_IVA>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_IMP_LEY>'+
                                %TRIM(%EDITW(ACULEY:'          . 0 ,  -'))+
                            '</TOTAL_IMP_LEY>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_IMP_CRED>'+
                                %TRIM(%EDITW(ACUICR:'          . 0 ,  -'))+
                            '</TOTAL_IMP_CRED>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_ING_BRUTOS>'+
                                %TRIM(%EDITW(ACUIBR:'          . 0 ,  -'))+
                            '</TOTAL_ING_BRUTOS>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_SIRCREB>'+
                                %TRIM(%EDITW(ACUSIR:'          . 0 ,  -'))+
                            '</TOTAL_SIRCREB>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_SIRCREB_CBA>'+
                                %TRIM(%EDITW(ACUSCB:'          . 0 ,  -'))+
                            '</TOTAL_SIRCREB_CBA>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_SIRCREB_CABA>'+
                                %TRIM(%EDITW(ACUSBA:'          . 0 ,  -'))+
                            '</TOTAL_SIRCREB_CABA>';
        Write REBAHILI;
        Eval HLDES3='        <TOTAL_SIRCREB_STFE>'+
                                %TRIM(%EDITW(ACUSSF:'          . 0 ,  -'))+
                            '</TOTAL_SIRCREB_STFE>';
        Write REBAHILI;
        Eval HLDES3='      </TOTALMES>';
        Write REBAHILI;
     C/END-FREE
     C                   ENDSR
     C*----------------------------------------------------------------
     C* ClrAcuTot: Borra totalizado acumuladores
     C*----------------------------------------------------------------
     C     ClrAcuTot     BEGSR
     C*
     C                   Z-ADD     *ZERO         TOTIVA           15 2
     C                   Z-ADD     *ZERO         TOTLEY           15 2
     C                   Z-ADD     *ZERO         TOTICR           15 2
     C                   Z-ADD     *ZERO         TOTIBR           15 2
     C                   Z-ADD     *ZERO         TOTSIR           15 2
     C                   Z-ADD     *ZERO         TOTSCB           15 2
     C                   Z-ADD     *ZERO         TOTSBA           15 2
     C                   Z-ADD     *ZERO         TOTSSF           15 2
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* ClrAcuVar: Borra acumuladores
     C*----------------------------------------------------------------
     C     ClrAcuVar     BEGSR
     C*
     C                   Z-ADD     *ZERO         ACUIVA           15 2
     C                   Z-ADD     *ZERO         ACULEY           15 2
     C                   Z-ADD     *ZERO         ACUICR           15 2
     C                   Z-ADD     *ZERO         ACUIBR           15 2
     C                   Z-ADD     *ZERO         ACUSIR           15 2
     C                   Z-ADD     *ZERO         ACUSCB           15 2
     C                   Z-ADD     *ZERO         ACUSBA           15 2
     C                   Z-ADD     *ZERO         ACUSSF           15 2
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* AddAcuTot: Borra acumuladores
     C*----------------------------------------------------------------
     C     AddAcuTot     BEGSR
     C*
     C                   ADD       ACUIVA        TOTIVA
     C                   ADD       ACULEY        TOTLEY
     C                   ADD       ACUICR        TOTICR
     C                   ADD       ACUIBR        TOTIBR
     C                   ADD       ACUSIR        TOTSIR
     C                   ADD       ACUSCB        TOTSCB
     C                   ADD       ACUSBA        TOTSBA
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* BldFechas: Arma las fechas en formato normal
     C*----------------------------------------------------------------
     C     BldFechas     BEGSR
     C*
     C                   Z-ADD     WkFEHA        DSFECI
     C                   Z-ADD     DSAÑOI        DSAÑON
     C                   Z-ADD     DSMESI        DSMESN
     C                   Z-ADD     DSDIAI        DSDIAN
     C                   Z-ADD     DSFECN        WWFEHA            8 0
     c*
     C                   Z-ADD     WkFEDE        DSFECI
     C                   Z-ADD     DSAÑOI        DSAÑON
     C                   Z-ADD     DSMESI        DSMESN
     C                   Z-ADD     DSDIAI        DSDIAN
     C                   Z-ADD     DSFECN        WWFDES            8 0
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WrtLeyendas: Obtiene acumulados por mes para mes
     C*----------------------------------------------------------------
     C     WrtLeyendas   BEGSR
     C*
     C/FREE
        Eval HLDES3='    <LEYENDAS>';
        Write REBAHILI;
        Eval HLDES3='      <LALEYENDA>';
        Write REBAHILI;
        Eval HLDES3='        <LEYENDA>';
        Write REBAHILI;
        Eval HLDES3='                 '+JAIRL1+
                                        JAIRL2+
                                        JAIRL3;
        Write REBAHILI;
        Eval HLDES3='                 '+JAIRL4+
                                        JAIRL5+
                                        JAIRL6;
        Write REBAHILI;
        Eval HLDES3='                 '+JAIRL7+
                                        JAIRL8+
                                        JADAM1;
        Write REBAHILI;
        Eval HLDES3='                 '+JADAM2+
                                        JADAM3+
                                        JADAM4;
        Write REBAHILI;
        Eval HLDES3='          </LEYENDA>';
        Write REBAHILI;
        Eval HLDES3='      </LALEYENDA>';
        Write REBAHILI;
        Eval HLDES3='    </LEYENDAS>';
        Write REBAHILI;
     C/END-FREE
     C*
     C                   ENDSR
