*SRCMBRTXT:Switch-Adapter      -PF-380000-Alta PF-
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: Plazos Fijos - Renovaciones Automáticas         *
     H*                                                               *
     H*  PROGRAM NAME: GENERA NUEVO/S CERTIFICADOS Y CONTABILIZA LAS  *
     H*                RENOVACIONES EN PFTMOR                         *
     H*                                                               *
     H*  PROGRAM NO: PFKZ00R1                                         *
     H*                                                               *
     H*  DATE:                                                        *
     H*                                                               *
     H*  AUTHOR:                                                      *
     H*                                                               *
     F*----------------------------------------------------------------*
     FPFCERT02  UF   E           K DISK
     F* Cartera de Plazos Fijos - Vista por Suc, Nro. Cert.
     FPFCERT    UF A E           K DISK
     F                                     RENAME(REPFCERT:RRCERT)
     F* Cartera de Plazos Fijos
     FBAMONE    IF   E           K DISK
     F* Monedas
     FPFCODI    IF   E           K DISK
     F* Códigos de Movimientos de Plazos Fijos
     FPFCODF    IF   E             DISK
     F* Códigos Fijos de Movimientos de Plazos Fijos
     FPFMOVI01  UF   E           K DISK
     F* Movimientos de Plazo Fijo
     FPFDAMP    UF   E           K DISK
     F* Datos ampliados de Plazos Fijos
     FPFPIZA    IF   E           K DISK
     F* Pizarra de plazo fijo
     FPFTRAM02  IF   E           K DISK
     F* Tramos por pizarra plazo fijo
     FPFCATR02  IF   E           K DISK
     FSGSYSV    IF   E             DISK
     F* Valores del Sistema
     FPFEXCE01  IF   E           K DISK
     F* Excepciones por inversor
     FPFCMGT01  IF   E           K DISK
     F* Temporal de movimientos PF diarios a asentar
     FBAICCL    IF   E           K DISK
     F* Archivo de Cuentas Clientes
     FBADCCL01  IF   E           K DISK
     F* Archivo de Cuentas Clientes Detalle
     FBAPFIS    IF   E           K DISK
     F* Archivo de Personas Físicas
     FBAPJUR    IF   E           K DISK
     F* Archivo de Personas Jurídicas
     FACCTAC07  IF   E           K DISK
     F* Caja de Ahorro por Cta Clte
     FCCCTCT03  IF   E           K DISK
     F* Cuenta Cte por Cta Clte
     F@CPIUSD   IF   E           K DISK
     F@CPISYS   IF   E           K DISK
     FPFDCCL    UF A E           K DISK
     F                                     RENAME(REPFDCCL:RRDCCL)
     FPFDCCL02  UF   E           K DISK
     FPFLEYE    UF A E           K DISK
     D*---------------------------------------------------------------
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D PAIRL1          C                   CONST('PF EMITIDO POR      ')
     D PAIRL2          C                   CONST('CAJERO AUTOMATICO   ')
     D ERR001          C                   CONST('301NO SE ENC. LA TNA')
     D ERR002          C                   CONST('302NO SE ENC.TRAMO  ')
     D ERR003          C                   CONST('303NO SE ENC.PIZARRA')
     D ERR005          C                   CONST('305PF IMP. INVALIDO ')
     D ERR007          C                   CONST('307PF PLAZO INVALIDO')
     D*----------------------------------------------------------------*
     D                 DS
     D  DSDACL                 1     71
     D  DSDF01                 1     20
     D  DSDF02                11     40
     D  DSDF03                41     60
     D                 DS
     D  DSININ                 1     16  0
     D  DSIPUE                 1      6  0
     D  DSIPAI                 7     10  0
     D  DSIZOR                11     13  0
     D  DSIPIS                14     16  0
     I*================================================================*
     C*                    PROCESO PRINCIPAL
     I*================================================================*
     C*
     C* ...  Limpia Variables
     C                   EXSR      SRLIMP
     C* ...  Obtiene sucursal
     C                   Z-ADD     PAISUC        WWISUC
     C* ...  Inicializa datos para Nuevo Certificado.
     C                   EXSR      SRCERT
     C* ...  Obtiene Número de Certificado.
     C                   EXSR      SRNUME
     C* ...  Genera movimientos
     C                   EXSR      SRMOVI
     C* ...  Actualiza Leyenda
     C                   EXSR      SRLEYE
     C* ...  Actualiza Ordenes de Firmantes
     C                   EXSR      SRFIRM
     C* ...  Activa Plazo Fijo
     C                   EXSR      ACTIPF
     C* ...  Devuelve Nro de Certificado y código de term. OK
     C                   MOVE      *ZEROS        PAIERR
     C                   Z-ADD     IZINCE        PAINCE
     C*
     C                   EXSR      ENDPGM
     I*================================================================*
     C* INICIO - INICIO DE PROGRAMA
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAISUB            2
     C                   PARM                    PAISUC
     C                   PARM                    PAICCL
     C                   PARM                    PAFALT
     C                   PARM                    PAHALT
     C                   PARM                    PATTNA
     C                   PARM                    PA$CAP
     C                   PARM                    PAQDPL
     C                   PARM                    PAIREA
     C                   PARM                    PAINCE
     C                   PARM                    PAPFTI            1
     C                   PARM                    PAIERR           40
     C*
     C     *LIKE         DEFINE    IZINCE        PAINCE
     C     *LIKE         DEFINE    IZISUC        PAISUC
     C     *LIKE         DEFINE    IZTTNA        PATTNA
     C     *LIKE         DEFINE    IZTTEM        WKTTEM
     C     *LIKE         DEFINE    IZICCL        PAICCL
     C     *LIKE         DEFINE    IZIREA        PAIREA
     C     *LIKE         DEFINE    IZQDPL        PAQDPL
     C     *LIKE         DEFINE    IZ$CAP        PA$CAP
     C     *LIKE         DEFINE    IZ$CAP        WW$CAP
     C     *LIKE         DEFINE    IZISUC        WWISUC
     C     *LIKE         DEFINE    IZFASI        WWFVIG
     C     *LIKE         DEFINE    IZIREA        WXIREA
     C     *LIKE         DEFINE    IZFALT        WWFALT
     C     *LIKE         DEFINE    IZFALT        PAFALT
     C     *LIKE         DEFINE    IZFALT        PAFECH
     C     *LIKE         DEFINE    IZFALT        WWFVEN
     C     *LIKE         DEFINE    IZFALT        WKFALT
     C     *LIKE         DEFINE    DCHALT        WKHALT
     C     *LIKE         DEFINE    IVTSEL        WWTSEL
     C     *LIKE         DEFINE    IV$T01        WX$T01
     C     *LIKE         DEFINE    @ZJOBN        PAIJOB
     C     *LIKE         DEFINE    IZISUC        WIISUC
     C     *LIKE         DEFINE    IZISCT        WKISCT
     C     *LIKE         DEFINE    IZITCU        WKITCU
     C     *LIKE         DEFINE    IZIOFI        WKIOFI
     C     *LIKE         DEFINE    DCHALT        PAHALT
     C*
     C                   MOVE      *BLANKS       PAIERR
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            98
     C     @PJOBN        CHAIN     @CPISYS                            97
     C     1             CHAIN     SGSYSV                             96
     C     1             CHAIN     PFCODF                             95
     C                   Z-ADD     AASFEI        WKFALT
     C                   TIME                    WKHALT
     C                   MOVEL     *BLANKS       PAIERR
     C                   Z-ADD     @ZJOBN        PAIJOB
     C*
     C* Acceso a PFCERT
     C     @KEY01        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    IZINCE
     C* Acceso a PFMOVI01
     C     @KEY02        KLIST
     C                   KFLD                    IZISUC
     C                   KFLD                    IZIGCE
     C                   KFLD                    IZITCE
     C                   KFLD                    IZINCE
     C* Acceso a PFMOVI01
     C     @KEY03        KLIST
     C                   KFLD                    IZISUC
     C                   KFLD                    IZIGCE
     C                   KFLD                    IZITCE
     C                   KFLD                    IZINCE
     C                   KFLD                    WWIPOS            3 0
     C                   KFLD                    WWIMPF            3 0
     C* Acceso a PFDAMP
     C     @KEY04        KLIST
     C                   KFLD                    IZISUC
     C                   KFLD                    IZINCE
     C* Acceso a PFPIZA
     C     @KEY05        KLIST
     C                   KFLD                    WWISUC            5 0
     C                   KFLD                    IZIGCE
     C                   KFLD                    IZITCE
     C                   KFLD                    IZIMON
     C                   KFLD                    WWFVIG
     C                   KFLD                    WWHVIG            5 0
     C* Acceso a PFTRAM
     C     @KEY06        KLIST
     C                   KFLD                    ITISUC
     C                   KFLD                    ITINPI
     C                   KFLD                    IUITTR
     C* Acceso a PFCATR
     C     @KEY07        KLIST
     C                   KFLD                    ITISUC
     C                   KFLD                    ITINPI
     C                   KFLD                    IUITTR
     C* Acceso a PFCMGT
     C     @KEY08        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    IZIGCE
     C                   KFLD                    IZITCE
     C* Acceso a PFEXCE
     C     @KEY09        KLIST
     C                   KFLD                    IZISUC
     C                   KFLD                    IZICCL
     C                   KFLD                    IRIMPF
     C* Acceso a BAICCL
     C     @VKY04        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICCL
     C* Acceso a BADCCL
     C     WKEY77        KLIST
     C                   KFLD                    OSISUC
     C                   KFLD                    OSICCL
     C* Acceso a BAPJUR
     C     WKEY78        KLIST
     C                   KFLD                    OSITIN
     C                   KFLD                    OSININ
     C* Acceso a BAPFIS
     C     WKEY79        KLIST
     C                   KFLD                    OTITDO
     C                   KFLD                    OTINDO
     C*
     C     WKEY05        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    IZIGCE
     C                   KFLD                    IZITCE
     C                   KFLD                    IZIMON
     C                   KFLD                    WWFVIG
     C                   KFLD                    WWHVIG
     C     WKEY06        KLIST
     C                   KFLD                    ITISUC
     C                   KFLD                    ITINPI
     C                   KFLD                    IUITTR
     C     WKEY07        KLIST
     C                   KFLD                    ITISUC
     C                   KFLD                    ITINPI
     C                   KFLD                    IUITTR
     C                   KFLD                    IV$T01
     C*
     c     PAISUB        IFEQ      'CA'
     C                   MOVE      'AC'          PAISUB
     C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* ENDPGM - Rutina de finalización de Proceso
     C*----------------------------------------------------------------*
     C     ENDPGM        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* BSPIZA - Busca pizarra
     C*----------------------------------------------------------------
     C     BSPIZA        BEGSR
     C*
     C                   Z-SUB     PAFALT        WWFVIG            8 0
     C                   TIME                    WKTIME            6 0
     C                   Z-SUB     WKTIME        WWHVIG
     C*
     C*
     C* Busca Pizarra con Sucursal Emisora
     C                   Z-ADD     IZISUC        WWISUC
     C     WKEY05        SETLL     PFPIZA                                 99
     C                   READ      PFPIZA                                 99
     C     *IN99         IFEQ      *ON
     C     ITISUC        ORNE      WWISUC
     C     ITIGCE        ORNE      IZIGCE
     C     ITITCE        ORNE      IZITCE
     C     ITIMON        ORNE      IZIMON
     C                   Z-ADD     *ZERO         SEVIGE            8 0
     C                   Z-ADD     *ZERO         SEHVIG            5 0
     C                   ELSE
     C                   MOVE      ITVIGE        SEVIGE
     C                   MOVE      ITHVIG        SEHVIG
     C                   ENDIF
     C*
     C                   MULT      -1            SEVIGE
     C                   MULT      -1            SEHVIG
     C* Busca Pizarra con Sucursal Generica
     C                   Z-ADD     99999         WWISUC
     C     WKEY05        SETLL     PFPIZA                                 99
     C                   READ      PFPIZA                                 99
     C     *IN99         IFEQ      *ON
     C     ITISUC        ORNE      WWISUC
     C     ITIGCE        ORNE      IZIGCE
     C     ITITCE        ORNE      IZITCE
     C     ITIMON        ORNE      IZIMON
     C                   Z-ADD     *ZEROS        SGVIGE            8 0
     C                   Z-ADD     *ZEROS        SGHVIG            5 0
     C                   ELSE
     C                   MOVE      ITVIGE        SGVIGE
     C                   MOVE      ITHVIG        SGHVIG
     C                   ENDIF
     C*
     C                   MULT      -1            SGVIGE
     C                   MULT      -1            SGHVIG
     C* Determina si existe una pizarra "buena" y cual es
     C                   SELECT
     C* Si no se encontro ninguna pizarra, error =>  cance
     C                   WHEN      SEVIGE=*ZEROS AND SGVIGE = *ZEROS
     C                   MOVEL(P)  ERR003        PAIERR
     C                   EXSR      ERRPRC
     C                   LEAVESR
     C* Hay sucursal emisora, pero no generica, usar emisor
     c                   WHEN      SEVIGE <> *ZEROS AND SGVIGE = *ZEROS
     C                   Z-ADD     IZISUC        WWISUC
     C                   Z-ADD     SEVIGE        WWFVIG
     C                   Z-ADD     SEHVIG        WWHVIG
     C* Hay sucursal generica, pero no emisora , usar generica
     C                   WHEN      SEVIGE = *ZEROS AND SGVIGE <> *ZEROS
     C                   Z-ADD     99999         WWISUC
     C                   Z-ADD     SGVIGE        WWFVIG
     C                   Z-ADD     SGHVIG        WWHVIG
     C* Hay sucursal generica y Suc. emisora, utilizar mas nueva
     C                   WHEN      SEVIGE <> *ZEROS AND SGVIGE <> *ZEROS
     C     SEVIGE        IFEQ      SGVIGE
     C* A Fechas Iguales Comparar las horas ...
     C     SGHVIG        IFGT      SEHVIG
     C                   Z-ADD     99999         WWISUC
     C                   Z-ADD     SGVIGE        WWFVIG
     C                   Z-ADD     SGHVIG        WWHVIG
     C                   ELSE
     C                   Z-ADD     IZISUC        WWISUC
     C                   Z-ADD     SEVIGE        WWFVIG
     C                   Z-ADD     SEHVIG        WWHVIG
     C                   ENDIF
     C                   ELSE
     C     SEVIGE        IFGT      SGVIGE
     C                   Z-ADD     IZISUC        WWISUC
     C                   Z-ADD     SEVIGE        WWFVIG
     C                   Z-ADD     SEHVIG        WWHVIG
     C                   ELSE
     C                   Z-ADD     99999         WWISUC
     C                   Z-ADD     SGVIGE        WWFVIG
     C                   Z-ADD     SGHVIG        WWHVIG
     C                   ENDIF
     C                   ENDIF
     C                   ENDSL
     C* Volver la Fecha Seleccionada a Negativo
     C                   MULT      -1            WWFVIG
     C                   MULT      -1            WWHVIG
     C* Acceder a Pizarra Buena
     C     WKEY05        SETLL     PFPIZA                                 99
     C                   READ      PFPIZA                                 99
     C     *IN99         IFEQ      *ON
     C     ITISUC        ORNE      WWISUC
     C     ITIGCE        ORNE      IZIGCE
     C     ITITCE        ORNE      IZITCE
     C     ITIMON        ORNE      IZIMON
     C                   MOVEL(P)  ERR003        PAIERR
     C                   EXSR      ERRPRC
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   Z-SUB     IZQDPL        IUITTR
     C     WKEY06        SETLL     PFTRAM02
     C                   READ      PFTRAM02                               99
     C     *IN99         IFEQ      *ON
     C     ITISUC        ORNE      IUISUC
     C     ITINPI        ORNE      IUINPI
     C                   MOVEL(P)  ERR003        PAIERR
     C                   EXSR      ERRPRC
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   Z-SUB     IZ$CAP        IV$T01
     C     WKEY07        SETLL     PFCATR02
     C                   READ      PFCATR02                               99
     C     *IN99         IFEQ      *ON
     C     ITISUC        ORNE      IVISUC
     C     ITINPI        ORNE      IVINPI
     C     IUITTR        ORNE      IVITTR
     C                   MOVEL(P)  ERR003        PAIERR
     C                   EXSR      ERRPRC
     C                   LEAVESR
     C                   ENDIF
     C*
     C     END001        TAG
     C*
     C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* SRLIMP - Inicializa variables de trabajo.
|    C*----------------------------------------------------------------*
|    C     SRLIMP        BEGSR
|    C*
|    C                   Z-ADD     *ZEROS        WWFALT
|    C                   Z-ADD     *ZEROS        WWFVEN
|    C                   MOVEL     *BLANKS       PXIERR            1
|    C                   MOVEL     *BLANKS       PACINV            2
|    C                   MOVEL     *BLANKS       PADSEM            2
|    C                   Z-ADD     *ZEROS        PADIAS           15 0
|    C                   Z-ADD     *ZEROS        WXISUC            5 0
|    C                   Z-ADD     *ZEROS        WPFASI           15 0
|    C                   Z-ADD     *ZEROS        WPFVEN           15 0
|    C                   Z-ADD     *ZEROS        WW$INE           15 2
|    C                   Z-ADD     *ZEROS        WW$TRE           15 2
|    C                   Z-ADD     *ZEROS        WIISUC
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* SRCERT - Obtiene Datos para Nuevo Certificado.
|    C*----------------------------------------------------------------*
|    C     SRCERT        BEGSR
|    C                   Z-ADD     PAISUC        IZISUC
|    C                   Z-ADD     PAICCL        IZICCL
|    C                   Z-ADD     1             IZIMON
     C*
|    C                   MOVE      'C'           IZIGCE
     c     PAPFTI        IFEQ      'A'
     C                   MOVE      'F'           IZIGCE
     C                   ENDIF
     C*
|    C                   Z-ADD     *ZERO         IZIBPF
|    C                   MOVE      '1'           IZITCE
|    C                   Z-ADD     999           IZILEG
|    C                   Z-ADD     *ZERO         IZPMAR
|    C                   MOVE      *BLANKS       IZIREN
|    C                   MOVE      *BLANKS       IZIPOM
|    C                   MOVE      *BLANKS       IZITAV
|    C                   MOVE      'N'           IZIRAU
|    C*
|    C* ... Ver Cuenta
|    C                   EXSR      GETCTA
|    C                   MOVEL     WKIOFI        IZIOFI
|    c* ... Ver tema de fechas y horas por proceso
|    C                   Z-ADD     PAFALT        IZFALT
|    C                   Z-ADD     PAFALT        IZFASI
|    C* ... Por LINK sin renovación automática.
|    C                   Z-ADD     *ZERO         IZIREA
|    C                   Z-ADD     PA$CAP        IZ$CAP
|    C* ... Tasas ya vienen elegidas, no hay que buscar pizarra
|    C*                  Z-ADD     PATTNA        IZTTNA
|    C*                  Z-ADD     WKTTEM        IZTTEM
|    C* ... Buscar Sector de Cuenta y Residencia del Primer Firmate de la CCL
|    C                   EXSR      GETCCL
|    C                   Z-ADD     WKISCT        IZISCT
|    C                   Z-ADD     WKITCU        IZITCU
|    C*
|    C                   Z-ADD     PAQDPL        IZITTR
|    C                   Z-ADD     PAQDPL        IZQDPL
|    C* ...  Busco pizarra
|    C                   EXSR      BSPIZA
|    C                   Z-ADD     IVTTNA        IZTTNA
|    C                   Z-ADD     IVTTEM        IZTTEM
|    c* ...
|    C                   Z-ADD     *ZEROS        IZ$INT
|    C                   Z-ADD     *ZEROS        IZ$TRE
|    C                   Z-ADD     *ZEROS        IZ$MON
|    C                   Z-ADD     *ZEROS        IZ$INE
|    C                   Z-ADD     *ZEROS        IZ$DVE
|    C                   Z-ADD     *ZEROS        IZ$DVL
|    C                   Z-ADD     *ZEROS        IZ$DAE
|    C                   Z-ADD     *ZEROS        IZ$DAL
|    C                   Z-ADD     *ZEROS        IZ$IRE
|    C                   MOVE      'E'           IZIECE
|    C                   Z-ADD     *ZEROS        IZFAPA
|    C                   Z-ADD     *ZEROS        IZFCOB
|    C                   MOVE      PAISUC        IZIRIN
|    C                   MOVE      *ZERO         IZTPLU
|    C                   MOVE      *BLANKS       IZIECE
|    C* Obtengo Fecha de Alta.
|    C                   Z-ADD     IZFALT        PAFECH
|    C                   MOVE      'IN'          PACINV
|    C                   CALL      'SBBAINFE'
|    C                   PARM                    PAFECH
|    C                   PARM                    PACINV
|    C* Obtengo días de Fecha de Alta.
|    C                   Z-ADD     PAFECH        WWFALT
|    C                   Z-ADD     WWFALT        PAFECH
|    C                   Z-ADD     *ZERO         WPFASI
|    C                   CALL      'SBBAFECH'
|    C                   PARM                    PAFECH
|    C                   PARM                    WPFASI
|    C                   PARM                    PADSEM
|    C                   PARM                    PXIERR
|    C* Obtengo días de Fecha de Vencimiento.
|    C     WPFASI        ADD       IZQDPL        WPFVEN
|    C* Obtengo de Fecha de Vencimiento.
|    C                   Z-ADD     *ZEROS        PAFECH
|    C                   CALL      'SBBAFECH'
|    C                   PARM                    PAFECH
|    C                   PARM                    WPFVEN
|    C                   PARM                    PADSEM
|    C                   PARM                    PXIERR
|    C                   Z-ADD     PAFECH        WWFVEN
|    C* Obtengo Fecha de vencimiento día Hábil.
|    C                   Z-ADD     WPFVEN        PADIAS
|    C                   Z-ADD     *ZEROS        PAFECH
|    C                   Z-ADD     IZISUC        WXISUC
|    C* El 02.06.2010 se cambia el criterio: tomar el próximo día hábil
|    C                   CALL      'SBBABFER'
|    C                   PARM                    PAFECH
|    C                   PARM                    PADIAS
|    C                   PARM                    WXISUC
|+---C     WWFVEN        IFNE      PAFECH
||   C* Obtengo Plazo Real.                                         |
||   C                   Z-ADD     *ZEROS        WPFVEN
||   C                   CALL      'SBBAFECH'
||   C                   PARM                    PAFECH
||   C                   PARM                    WPFVEN
||   C                   PARM                    PADSEM
||   C                   PARM                    PXIERR
||   C     WPFVEN        SUB       WPFASI        IZQDPL
||   C                   Z-ADD     PAFECH        WWFVEN
|+---C                   ENDIF
|    C*  Invierto fecha de Venimiento.
|    C                   Z-ADD     WWFVEN        PAFECH
|    C                   MOVE      'NI'          PACINV
|    C                   CALL      'SBBAINFE'
|    C                   PARM                    PAFECH
|    C                   PARM                    PACINV
|    C                   Z-ADD     PAFECH        IZFVEN
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* GETCCL - Obtiene ISCT e ITCU desde CCL
|    C*----------------------------------------------------------------*
|    C     GETCCL        BEGSR
|    C*
|    C     @VKY04        CHAIN     BAICCL                             89
|+---C     OSININ        IFNE      *ZEROS
||   C     WKEY78        CHAIN     BAPJUR                             99
||   C                   Z-ADD     AOISCT        WKISCT
||   C                   Z-ADD     AOITCU        WKITCU
|+---C                   ELSE
||   C     WKEY77        CHAIN     BADCCL01                           99
||   C     WKEY79        CHAIN     BAPFIS                             99
||   C                   Z-ADD     A#ISCT        WKISCT
||   C                   Z-ADD     A#ITCU        WKITCU
|+---C                   ENDIF
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* ERRPRC - Rutina de Restauración de Estado por Error
|    C*----------------------------------------------------------------*
|    C     ERRPRC        BEGSR
|    C*
|    C     @KEY01        CHAIN     PFCERT02                           80
|    C  N80              MOVEL     'C'           IZIECE
|    C  N80              Z-ADD     *ZEROS        IZFAPA
|    C  N80              UPDATE    REPFCERT
|    C*
|+---C     IZIRAU        IFEQ      'N'
||   C     @KEY01        CHAIN     PFDCCL02                           75
||+--C     *IN75         DOWEQ     *OFF
|||  C                   Z-ADD     *ZEROS        DCF002
|||  C                   Z-ADD     *ZEROS        DCHORA
|||  C                   MOVEL     *BLANKS       DCIUAR
|||  C                   Z-ADD     *ZEROS        DCICAJ
|||  C                   Z-ADD     *ZEROS        DCISAL
|||  C                   UPDATE    REPFDCCL
|||  C     @KEY01        READE     PFDCCL02                               75
||+--C                   ENDDO
|+---C                   ENDIF
|    C*
|    C     @KEY02        CHAIN     PFLEYE                             74
|+---C     *IN74         IFEQ      *OFF
||   C                   DELETE    REPFLEYE
|+---C                   ENDIF
|    C*
|    C*                  MOVEL     '1'           PAIERR
|    C*
|    C                   EXSR      ENDPGM
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* GETCTA - Obtiene Datos de Cuenta
|    C*----------------------------------------------------------------*
|    C     GETCTA        BEGSR
|    C*
|    C                   MOVEL     1             WKIOFI
|+---C     PAISUB        IFEQ      'CC'
||   C     @VKY04        CHAIN     REACCTAC                           89
||   C  N89              MOVE      FUIOFI        WKIOFI
|+---C                   ELSE
||   C     @VKY04        CHAIN     RECCCTCT                           89
||   C  N89              MOVE      BMIOFI        WKIOFI
|+---C                   ENDIF
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* SRNUME - Obtiene Nº Siguiente del Nuevo Plazo Fijo.
|    C*----------------------------------------------------------------*
|    C     SRNUME        BEGSR
|    C*
|    C                   Z-ADD     *ZEROS        IZINCE
|    C                   CALL      'PFIZ00SB'
|    C                   PARM                    IZISUC
|    C                   PARM                    IZINCE
|    C                   PARM                    PXIERR
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* SRMOVI - Rutina de Generación de Movimientos.
|    C*----------------------------------------------------------------*
|    C     SRMOVI        BEGSR
|    C*
|    C     @KEY04        CHAIN     PFDAMP                             89
|+---C     *IN89         IFEQ      *OFF
||   C                   DELETE    REPFDAMP
|+---C                   ENDIF
|    C* REPRODUCE EL PROCESO DE 'PFIZ02SB'
|    C     @KEY02        CHAIN     PFMOVI01                           80
|+---C     *IN80         DOWEQ     *OFF
||   C                   DELETE    REPFMOVI
||   C     @KEY02        READE     PFMOVI01                               80
|+---C                   ENDDO
|    C                   EXSR      PCODIF
|    C                   EXSR      PCODIV
|    C*
|    C                   Z-ADD     *ZERO         WW$INE
|    C                   Z-ADD     *ZERO         WW$TRE
|    C     @KEY02        CHAIN     PFMOVI01                           85
|+---C     *IN85         DOWEQ     *OFF
||+--C                   SELECT
|||  C     JBIASK        WHENEQ    '1'
|||  C                   ADD       JB$IMP        WW$INE
|||  C     JBIASK        WHENEQ    '2'
|||  C                   SUB       JB$IMP        WW$INE
|||  C                   ADD       JB$IMP        WW$TRE
||+--C                   ENDSL
||   C     @KEY02        READE     PFMOVI01                               85
|+---C                   ENDDO
|    C     IZ$CAP        ADD       IZ$INT        IZ$MON
|    C     IZ$MON        SUB       IZ$TRE        IZ$INE
|    C                   WRITE     RRCERT
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------
|    C* PCODIF - Genera movimientos fijos.
|    C*----------------------------------------------------------------
|    C     PCODIF        BEGSR
|    C*
|    C     1             CHAIN     PFCODF                             80
|    C* Para el código de capital
|    C     KQIMFC        CHAIN     PFCODI                             80
|    C                   Z-ADD     KQIMFC        PAIMPF
|    C                   EXSR      SRCALL
|    C* Para el código de interés
|    C     KQIMFI        CHAIN     PFCODI                             80
|    C                   Z-ADD     KQIMFI        PAIMPF
|    C                   EXSR      SRCALL
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* PCODIV - Genera movimientos variables.
|    C*----------------------------------------------------------------
|    C     PCODIV        BEGSR
|    C*
|    C* Determina los movimientos del certificado
|    C                   Z-ADD     IZISUC        WWISUC
|    C     @KEY08        CHAIN     PFCMGT01                           80
|    C   80              Z-ADD     99999         WWISUC
|    C   80@KEY08        CHAIN     PFCMGT01                           80
|+---C     *IN80         DOWEQ     *OFF
||   C     @KEY09        CHAIN     PFEXCE01                           81
||+--C     *IN81         IFEQ      *ON
|||  C* Busca pgm. asociado al código de mov. y calcula importe     ||
|||  C     IRIMPF        CHAIN     PFCODI                             82
|||  C                   Z-ADD     IRIMPF        PAIMPF            3 0
|||  C                   EXSR      SRCALL
||+--C                   ENDIF
||   C     @KEY08        READE     PFCMGT01                               80
|+---C                   ENDDO
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------
|    C* SRCALL - Llama a PGMs que calculan e impactan movimientos
|    C*----------------------------------------------------------------*
|    C     SRCALL        BEGSR
|    C*
|+---C                   SELECT
||   C     PAIMPF        WHENEQ    1
||   C                   CALL      'PFX003RG'
||   C                   PARM                    IZISUC
||   C                   PARM                    IZIGCE
||   C                   PARM                    IZITCE
||   C                   PARM                    IZINCE
||   C                   PARM                    PAIMPF
||   C                   PARM                    IZITTR
||   C                   PARM                    IZ$CAP
||   C                   PARM                    AASFEI
||   C                   PARM                    IZIMON
||   C                   PARM                    IZISCT
||   C                   PARM                    IZITCU
||   C     PAIMPF        WHENEQ    2
||   C                   CALL      'PFX001RG'
||   C                   PARM                    IZISUC
||   C                   PARM                    IZIGCE
||   C                   PARM                    IZITCE
||   C                   PARM                    IZINCE
||   C                   PARM                    PAIMPF
||   C                   PARM                    IZITTR
||   C                   PARM                    IZ$CAP
||   C                   PARM                    IZQDPL
||   C                   PARM                    AASFEI
||   C                   PARM                    IZTTNA
||   C                   PARM                    IZIMON
||   C                   PARM                    IZISCT
||   C                   PARM                    IZITCU
||   C                   PARM                    IZ$INT
||   C                   PARM                    PA$IMP           15 2
||   C     PAIMPF        WHENEQ    5
||   C                   CALL      'PFX032RG'
||   C                   PARM                    IZISUC
||   C                   PARM                    IZIGCE
||   C                   PARM                    IZITCE
||   C                   PARM                    IZINCE
||   C                   PARM                    PAIMPF
||   C                   PARM                    IZICCL
||   C                   PARM                    IZIMON
||   C                   PARM                    IZ$INT
||   C                   PARM                    IZ$TRE
||   C                   PARM                    AASFEI
||   C                   PARM                    IZISCT
||   C                   PARM                    IZITCU
||   C     PAIMPF        WHENEQ    6
||   C                   CALL      'PFX002RG'
||   C                   PARM                    IZISUC
||   C                   PARM                    IZIGCE
||   C                   PARM                    IZITCE
||   C                   PARM                    IZINCE
||   C                   PARM                    PAIMPF
||   C                   PARM                    IZITTR
||   C                   PARM                    IZ$CAP
||   C                   PARM                    AASFEI
||   C                   PARM                    IZIMON
||   C                   PARM                    IZISCT
||   C                   PARM                    IZITCU
||   C                   PARM                    IVTSEL
|+---C                   ENDSL
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* SRLEYE - Rutina de Actualiza de Leyenda
|    C*----------------------------------------------------------------*
|    C     SRLEYE        BEGSR
|    C*
|    C                   Z-ADD     IZISUC        JAISUC
|    C                   MOVEL     IZIGCE        JAIGCE
|    C                   MOVEL     IZITCE        JAITCE
|    C                   Z-ADD     IZINCE        JAINCE
|    C                   MOVEL     PAIRL1        JAIRL1
|    C                   MOVEL     PAIRL2        JAIRL2
|    C                   WRITE     REPFLEYE
|    C*
|    C                   ENDSR
|    C*----------------------------------------------------------------*
|    C* SRFIRM - Rutina de Actualiza Firmante  PFDCCL
|    C*----------------------------------------------------------------*
|    C     SRFIRM        BEGSR
|    C*
     C                   EXSR      SAVUVA
|    C*
|    C     @VKY04        CHAIN     BAICCL                             94
|    C     @VKY04        CHAIN     BADCCL01                           94
|+---C     *IN94         DOWEQ     *OFF
||   C*                                                             ||
||   C                   MOVE      IZISUC        DCISUC
||   C                   MOVE      IZINCE        DCINCE
||   C                   MOVE      PAISUB        DCISUB
||   C                   MOVE      IZIMON        DCIMON
||   C                   MOVE      IZICCL        DCICCL
||   C                   MOVE      OTITTL        DCITTL
||   C                   MOVE      OTITDO        DCITDO
||   C                   MOVE      OTINDO        DCINDO
||   C                   MOVE      OSITIN        DCITIN
||   C                   MOVE      OSININ        DCININ
||   C                   MOVE      OSNCCL        DCNCCL
||   C                   MOVEL     IZIOFI        DCIOFI
||   C                   MOVE      IZFALT        DCFALT
||   C                   MOVE      PAHALT        DCHALT
||   C*                                                             ||
||   C                   WRITE     RRDCCL
||   C     @VKY04        READE     BADCCL01                               94
|+---C                   ENDDO
|    C*
|    C                   ENDSR
     C*----------------------------------------------------------------*
     C*  SALVA COTIZACION UVA A LA IMPOSICION Y LA TASA DE PRECANCELAC.
     C*----------------------------------------------------------------*
     C     SAVUVA        BEGSR
     c*
     C     IZIGCE        IFNE      'F'
     C                   Z-ADD     *ZERO         DC$I01
     C                   Z-ADD     *ZERO         DC$I02
     C                   Z-ADD     *ZERO         DC$I03
     C                   Z-ADD     *ZERO         DC$I04
     C                   GOTO      ENSAUV
     C                   ENDIF
     C* ... Salva cotización UVA
     c*
     C                   CALL      'BAMU00RG'
     C                   PARM      99999         XXISUC            5 0
     C                   PARM      50            PAIMON            9 0
     C                   PARM      *ZEROS        XXFECH            8 0
     C                   PARM                    PACOTI           12 6
     C*
     C     PACOTI        MULT      1000000       DC$I01
     C*
     C                   Z-ADD     IZ$CAP        DC$I03
     C*
     C     IZ$CAP        DIV       PACOTI        CAPUVA           12 6
     C                   Z-ADD     CAPUVA        DC$I04
     c*
     C* ... Salva Precancelación
     c*
     C                   CALL      'BAMU00RG'
     C                   PARM      99999         XXISUC            5 0
     C                   PARM      51            PAIMON            9 0
     C                   PARM      *ZEROS        XXFECH            8 0
     C                   PARM                    PACOTI           12 6
     C*
     C     PACOTI        MULT      1000000       DC$I02
     C*
     C*
     C     ENSAUV        ENDSR
|    C*----------------------------------------------------------------*
|    C* ACTIPF Rutina de Activación de PF
|    C*----------------------------------------------------------------*
|    C     ACTIPF        BEGSR
|    C*
|    C                   CALL      'PFIZ06RG'
|    C                   PARM                    IZISUC
|    C                   PARM                    IZINCE
|    C                   PARM                    WKIERR            1 0
|    C*
|    C                   ENDSR
