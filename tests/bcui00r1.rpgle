*SRCMBRTXT:GENERA ARCHIVO XML PARA PRESENTACION EN
     H DECEDIT(',') DATEDIT(*DMY/)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: BCUI00R1                                       *
     H*                                                               *
     H*  PROGRAM NO: GENERA ARCHIVO XML PARA PRESENTACION EN UIF      *
     H*                                                               *
     H*  DATE:   19/12/2011                                           *
     H*                                                               *
     H*  AUTHOR: PR00586                                              *
     H*                                                               *
     H*****************************************************************
     FBCBUIF02  IP   E           K DISK
     FBAFCPL    UF A E           K DISK
     FSGSYSV    IF   E             DISK
     FBAPFIS    IF   E           K DISK
     F*----------------------------------------------------------------
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D*
     D PATMOD          C                   CONST('/home/metodos_formvi-
     D                                     gent/BCUIF_PF.xml')
     D PATSAL          C                   CONST('/home/UIF/')
     D*
     D*----------------------------------------------------------------*
     D                 DS
     D  WKFASI                 1      8  0
     D  WKDD                   1      2  0
     D  WKMM                   3      4  0
     D  WKAAAA                 5      8  0
     D                 DS
     D  DSNDNN                 1     55
     D  NOM                    1     55
     D                                     DIM(55)
     D                 DS
     D  DSNDN2                 1     55
     D  NM2                    1     55
     D                                     DIM(55)
     D                 DS
     D  DSICUI                 1     12  0
     D  DSIPRE                 1      3  0
     D  DSINUM                 4     11  0
     D  DSIDIG                12     12  0
     D                 DS
     D  DSXCUI                 1     15
     D  DSXPRE                 1      2  0
     D  DSXSE1                 3      4
     D  DSXNUM                 5     12  0
     D  DSXSE2                13     14
     D  DSXDIG                15     15  0
     D                 DS
     D  WWPROD                 1    820
     D  WWPRO0                 1     82
     D  WWPRO1                83    164
     D  WWPRO2               165    246
     D  WWPRO3               247    328
     D  WWPRO4               329    410
     D  WWPRO5               411    492
     D  WWPRO6               493    574
     D  WWPRO7               575    656
     D  WWPRO8               657    738
     D  WWPRO9               739    820
     D                 DS
     D  DSPATH                 1     17
     D  WWPAT1                 1     10
     D  WWPAT2                11     16
     D  WWPAT3                17     17
     D CmdStr          S           4096
     D CmdStrLen       S             15  5 INZ(4096)
     I*----------------------------------------------------------------
     C*
     C                   EXSR      SRLIMP
     C                   EXSR      SRPANV
     C     UITCA2        IFNE      *BLANKS
     c                   seton                                        90
     c                   seton                                        91
     C                   EXSR      SRPANV
     c                   endif
     C     UITCA3        IFNE      *BLANKS
     c                   seton                                        92
     c                   setoff                                       91
     C                   EXSR      SRPANV
     c                   endif
     C*
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para inicio del programa
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    WWPERI            6
     C*
     C                   MOVEL     *BLANK        WWCOMI            1
     C     1             CHAIN     SGSYSV                             83
     C                   Z-ADD     1             I                 1 0
     C*
     C* ... PARA ACCEDER A BAFCPL SOLO POR NRO DE JOB
     C     KFCPI1        KLIST
     C                   KFLD                    @PJOBN
     C* ... PARA ACCEDER A BAFCPL POR NRO DE JOB Y ETIQUETA
     C     KFCPI2        KLIST
     C                   KFLD                    @PJOBN
     C                   KFLD                    WWITAG
     C*
     C* ... PARA ACCEDER A BAPFIS / BAPJUR
     C     @KEY01        KLIST
     C                   KFLD                    UIITIN
     C                   KFLD                    UIININ
     C*
     C*LIMPIO ARCHIVO BAFCPL
     C     KFCPI1        CHAIN     REBAFCPL                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBAFCPL
     C     KFCPI1        READE     REBAFCPL                               99
     C                   ENDDO
     C*
     C                   MOVEL     '\-'          DSXSE1
     C                   MOVEL     '\-'          DSXSE2
     C*
     C                   MOVEL     PATSAL        WWPAT1
     C                   MOVEL     WWPERI        WWPAT2
     C                   MOVEL     '/'           WWPAT3
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRLIMP - Limpia e inicializa campos
     C*----------------------------------------------------------------*
     C     SRLIMP        BEGSR
     C*
     C                   MOVEL     *BLANKS       WWTDOC           50
     C                   Z-ADD     *ZEROS        WWNDOC           15 0
     C                   MOVEL     *BLANKS       WWCUIT           11 0
     C                   Z-ADD     *ZEROS        WWFALT            8 0
     C                   MOVE      *BLANKS       WWAPEL           50
     C                   MOVE      *BLANKS       WWNOMB           50
     C                   MOVE      *BLANKS       WWNOM2           50
     C                   MOVE      *BLANKS       WWITAG           10
     C                   MOVE      *BLANKS       WWCOED            1
     C                   MOVE      *BLANKS       WWVALU          400
     c                   setoff                                       90
     c                   setoff                                       91
     c                   setoff                                       92
     C                   Z-ADD     *ZEROS        W
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRPANV - Busca Datos en BCBUIF
     C*----------------------------------------------------------------*
     C     SRPANV        BEGSR
     C*
     C                   MOVEL     'Sin Producto'WWTXT            21
     C                   MOVE      's Activos'   WWTXT
     C     UITCA1        IFNE      WWTXT
     C                   MOVEL     UINDNN        DSNDNN
     C                   EXSR      SEPARA
     C                   MOVEL     UIATDO        WWTDOC
     C                   Z-ADD     UIININ        WWNDOC
     C  N90              Z-ADD     UIFALT        WWFALT
     C   91              Z-ADD     UIFECH        WWFALT
     C     UITIPP        IFEQ      'F'
     C     @KEY01        CHAIN     REBAPFIS                           99
     C  N99              Z-ADD     A#ICUI        WWCUIT
     C                   EXSR      SRIMPR
     C                   ELSE
     C                   CALL      'SBBAGCUI'
     C                   PARM                    A#INDO
     C                   PARM                    A#ISEX
     C                   PARM                    WWICUI           12 0
     C                   Z-ADD     WWICUI        WWCUIT
     C                   EXSR      SRIMPR
     C                   ENDIF
     C                   EXSR      SRIMPR
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SEPARA - RUTINA DE SEPARACION DE APELLIDO Y NOMBRES
     C*----------------------------------------------------------------*
     C     SEPARA        BEGSR
     C*
     C     1             DO        55            V                 2 0
     C                   ADD       1             W                 2 0
     C     NOM(V)        IFNE      ' '
     C                   MOVE      NOM(V)        NM2(W)
     C                   ELSE
     C                   SELECT
     C     V             WHENLE    4
     C                   MOVE      NOM(V)        NM2(W)
     C     WWAPEL        WHENEQ    *BLANKS
     C                   MOVEL     DSNDN2        WWAPEL
     C                   MOVEL(P)  *BLANKS       DSNDN2
     C                   Z-ADD     *ZEROS        W
     C     WWNOMB        WHENEQ    *BLANKS
     C                   MOVEL     DSNDN2        WWNOMB
     C                   MOVEL(P)  *BLANKS       DSNDN2
     C                   Z-ADD     *ZEROS        W
     C     WWNOM2        WHENEQ    *BLANKS
     C     V             ANDNE     55
     C                   MOVE      NOM(V)        NM2(W)
     C     WWNOM2        WHENEQ    *BLANKS
     C     V             ANDEQ     55
     C                   MOVEL     DSNDN2        WWNOM2
     C                   MOVEL(P)  *BLANKS       DSNDN2
     C                   Z-ADD     *ZEROS        W
     C                   ENDSL
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRIMPR - RUTINA DE IMPRESION
     C*----------------------------------------------------------------*
     C     SRIMPR        BEGSR
     C*
     C*.. MUEVE EL PRIMER APELLIDO
     C                   MOVEL(P)  'APE1'        WWITAG
     C                   MOVEL(P)  WWAPEL        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C*.. MUEVE EL PRIMER NOMBRE
     C                   MOVEL(P)  'NOM1'        WWITAG
     C                   MOVEL(P)  WWNOMB        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C*.. MUEVE EL SEGUNDO NOMBRE
     C     WWNOM2        IFNE      *BLANKS
     C                   MOVEL(P)  'NOM2'        WWITAG
     C                   MOVEL(P)  WWNOM2        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C                   ENDIF
     C*
     C*.. MUEVE EL TIPO DE DOCUMENTO
     C                   MOVEL(P)  'TDOC'        WWITAG
     C                   MOVEL(P)  WWTDOC        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C*.. MUEVE EL NUMERO DE DOCUMENTO
     C                   MOVEL(P)  'NDOC'        WWITAG
     C                   MOVEL(P)  WWNDOC        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C*.. MUEVE EL NUMERO DE CUIT
     C                   MOVEL(P)  'NCUI'        WWITAG
     C                   MOVEL(P)  WWCUIT        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C*.. SEPARA LA FECHA EN DIA MES A#O
     C                   MOVEL     WWFALT        WWTANI            4
     C                   MOVE      WWFALT        WWFASI            4
     C*
     C                   MOVE      WWFASI        WWTDIA            2
     C                   MOVEL     WWFASI        WWTMES            2
     C*
     C                   MOVEL(P)  'TDIA'        WWITAG
     C                   MOVEL(P)  WWTDIA        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C                   MOVEL(P)  'TMES'        WWITAG
     C                   MOVEL(P)  WWTMES        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C                   MOVEL(P)  'TANI'        WWITAG
     C                   MOVEL(P)  WWTANI        WWVALU
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C*
     C*.. MUEVE LOS PRODUCTOS
     C                   MOVEL(P)  *BLANKS       WWPROD
     c                   MOVEL(P)  '<Productos_A'initag           19
     c                   MOVE      'ctivos>'     initag
     c                   MOVEL(P)  initag        protag           29
     c                   MOVE      '<Producto>'  protag
     C  N90              MOVEL(P)  UITCA1        proval           33
     C   91              MOVEL(P)  UITCA2        proval
     C   92              MOVEL(P)  UITCA3        proval
     c                   MOVEL(P)  '</Producto>' proend           31
     c                   MOVEL(P)  '</Productos_'endtag           20
     c                   MOVE      'Activos>'    endtag
     c                   MOVE      endtag        proend
     c                   eval      protag=%trimr(protag)
     c                   eval      proend=%trimr(proend)
     c                   eval      WWPRO0=protag+%trimr(proval)+proend
     C*    UITCA2        IFNE      *BLANKS
     C*                  MOVEL(P)  UITCA2        proval
     c*                  eval      WWPRO1=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UITCA3        IFNE      *BLANKS
     C*                  MOVEL(P)  UITCA3        proval
     c*                  eval      WWPRO2=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UITXT1        IFNE      *BLANKS
     C*                  MOVEL(P)  UITXT1        proval
     c*                  eval      WWPRO3=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UITXT2        IFNE      *BLANKS
     C*                  MOVEL(P)  UITXT2        proval
     c*                  eval      WWPRO4=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UITXT3        IFNE      *BLANKS
     C*                  MOVEL(P)  UITXT3        proval
     c*                  eval      WWPRO5=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UINCU1        IFNE      *BLANKS
     C*                  MOVEL(P)  UINCU1        proval
     c*                  eval      WWPRO6=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UINCU2        IFNE      *BLANKS
     C*                  MOVEL(P)  UINCU2        proval
     c*                  eval      WWPRO7=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UIDCO1        IFNE      *BLANKS
     C*                  MOVEL(P)  UIDCO1        proval
     c*                  eval      WWPRO8=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C*    UIDCO2        IFNE      *BLANKS
     C*                  MOVEL(P)  UIDCO2        proval
     c*                  eval      WWPRO9=protag+%trimr(proval)+proend
     C*                  MOVEL(P)  *BLANKS       PROVAL
     C*                  ENDIF
     C                   MOVEL(P)  'PR01'        WWITAG
     c                   eval      WWVALU=%trim(WWPROD)
     C                   MOVEL(P)  ' '           WWCOED
     C                   EXSR      WRTTAG
     C                   MOVEL(P)  *BLANKS       WWVALU
     C*
     C* ... LLAMAMOS AL "REEMPLAZADOR" DE ETIQUETAS
     C                   MOVEL(P)  PATMOD        MODELO          255
     C                   MOVE      UIININ        WWNREG           15
     C                   MOVEL     WWNREG        WWFILE           19
     C   90              MOVEL     '1'           WWFILE           19
     C   91              MOVEL     '2'           WWFILE           19
     C                   MOVE      '.xml'        WWFILE
     C                   MOVEL     DSPATH        WWPATH           36
     C                   MOVE      WWFILE        WWPATH
     C                   MOVE      *BLANKS       SALIDA          255
     C                   MOVEL     WWPATH        SALIDA
     C                   MOVEL     X'40'         WWPARA            1
     C                   CALL      'SBFP10RG'
     C                   PARM                    MODELO
     C                   PARM                    SALIDA
     C                   PARM                    WWPARA
     C                   PARM                    WWPARA
     C                   PARM                    WWPARA
     C*
     C                   EXSR      WRTTAG
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* WRTTAG: RUTINA QUE AGREGA O ACTUALIZA UNA ETIQUETA EN BAFCPL
     C*----------------------------------------------------------------
     C     WRTTAG        BEGSR
     C*
     C     KFCPI2        CHAIN     REBAFCPL                           99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  WWCOED        FLEFOR
     C                   MOVEL(P)  WWVALU        FLDES3
     C                   UPDATE    REBAFCPL
     C                   ELSE
     C                   MOVE(P)   @PJOBN        FLIJOB
     C                   MOVEL(P)  WWITAG        FLIFLD
     C                   MOVEL(P)  WWCOED        FLEFOR
     C                   MOVEL(P)  WWVALU        FLDES3
     C                   WRITE     REBAFCPL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
