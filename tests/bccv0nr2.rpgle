*SRCMBRTXT:Convierte archivo PLANO a nuevo diseño.
     H DEBUG
     H*****************************************************************
     H*  SYSTEM NAME: SIDEBA - REGIMEN INFORMATIVO.                   *
     H*                                                               *
     H*  PROGRAM NAME: BCCV00R2                                       *
     H*                                                               *
     H*  PROGRAM NO: PROCESO AUTOMATICO P/CONVERTIR A NUEVO DISE#O.   *
     H*                                                               *
     H*  DATE:     04.04.2017                                         *
     H*                                                               *
     H*  AUTHOR:   Sergio Cortes                                      *
     H*                                                               *
     H*  REVISO:   Carolina I. Cova - PR00543                         *
     H*                                                               *
     H*  NOTA: ANTES DE COMPILAR HACER:                               *
     H*                                                               *
     H*  CRTDUPOBJ  OBJ(BATEMP) FROMLIB(*LIBL) OBJTYPE(*FILE)         *
     H*               TOLIB(QTEMP) NEWOBJ(BATEMC)                     *
     H*  OVRDBF     FILE(BATEMC) TOFILE(QTEMP/BATEMC)                 *
     H*****************************************************************
     FBATEMC    IP   E             DISK
     FBC43PL    UF   E             DISK
     D*----------------------------------------------------------------*
     D @PD             S              4  0 DIM(1000)
     D @PH             S              4  0 DIM(1000)
     D @L              S              4  0 DIM(1000)
     D @T              S              1    DIM(100)
     D @C              S              1    DIM(100)
     D @E              S              1    DIM(100)
     D*----------------------------------------------------------------*
     D                 DS
     D  TMTODO                 1     70
     D  TMHOJA                 6      6
     D  TMASTE                 7      7
     D  TM00DS                19     20
     D  TMPDES                44     47  0
     D  TMPHAS                48     51  0
     D  TMTIPO                52     52
     D  TMEDIT                53     53
     D  TMDESC                53     58
     D  TMCONV                54     54
     D                 DS
     D  OLD                    1   4096
     D                                     DIM(4096)
     D  OLD1                   1   4096
     D  OLD2                   1   4096
     D                 DS
     D  RNVO                   1   4096
     D                                     DIM(4096)
     D  RN1                    1   4096
     D  RN2                    1   4096
     D                 DS
     D  PLTODO                 1   4096
     D  PLCDIS                 1      4  0
     D*----------------------------------------------------------------
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     C*
     C  N99              EXSR      INICIO
     C                   EXSR      SRPROC
     CLR 52              EXSR      SRCONV
     C*
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para inicio del programa
     C*----------------------------------------------------------------*
     C     INICIO        BEGSR
     C*
     C                   MOVE      *ON           *IN99
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PACDIS            4 0
     C                   PARM                    PA00DS           10
     C                   PARM                    PANFOR           10
     C*
     C     *DTAARA       DEFINE    *LDA          LDA
     C                   IN        LDA
     C*
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *OFF          *IN51
     C                   MOVE      *OFF          *IN52
     C                   Z-ADD     *ZEROS        I                 3 0
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRPROC - PROCESA DATOS PRINCIPALES.
     C*----------------------------------------------------------------*
     C     SRPROC        BEGSR
     C*
     C                   MOVEL     *BLANKS       TMTODO
     C                   MOVEL     B9BLK0        TMTODO
     C     TMTODO        IFEQ      *BLANK
     C     TMHOJA        ORNE      'I'
     C     TMASTE        ORNE      ' '
     C                   GOTO      ENPROC
     C                   ENDIF
     C     TM00DS        IFEQ      'DS'
     C                   MOVE      *ON           *IN50
     C                   GOTO      ENPROC
     C                   ENDIF
     C     TMDESC        IFEQ      PA00DS
     C     *IN50         ANDEQ     *ON
     C                   MOVE      *ON           *IN51
     C                   MOVE      TMPHAS        WWLONG            4 0
     C                   GOTO      ENPROC
     C                   ENDIF
     C     *IN50         IFEQ      *ON
     C     *IN51         ANDEQ     *ON
     C                   ADD       1             I
     C                   MOVE      TMPDES        WWPDES            4 0
     C                   MOVE      WWPDES        @PD(I)
     C                   MOVE      TMPHAS        WWPHAS            4 0
     C                   MOVE      WWPHAS        @PH(I)
     C     WWPHAS        SUB       WWPDES        WCLONG            4 0
     C                   ADD       1             WCLONG
     C                   MOVE      WCLONG        @L(I)
     C     TMTIPO        IFEQ      ' '
     C                   MOVE      'A'           @T(I)
     C                   ELSE
     C                   MOVE      'N'           @T(I)
     C                   ENDIF
     C     TMCONV        IFEQ      'N'
     C                   MOVE      'N'           @C(I)
     C                   ELSE
     C     TMCONV        IFEQ      'G'
     C                   MOVE      'G'           @C(I)
     C                   ELSE
     C     TMCONV        IFEQ      'E'
     C                   MOVE      'E'           @C(I)
     C                   MOVE      TMEDIT        @E(I)
     C                   ELSE
     C                   MOVE      'S'           @C(I)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     WWPHAS        IFEQ      WWLONG
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *INLR
     C                   ENDIF
     C                   ENDIF
     C*
     C     ENPROC        ENDSR
     C*----------------------------------------------------------------*
     C* SRCONV - CONVIERTE REGISTRO.
     C*----------------------------------------------------------------*
     C     SRCONV        BEGSR
     C*
     C                   Z-ADD     I             WWCANC            3 0
     C                   READ      BC43PL                                 90
     C     *IN90         DOWEQ     *OFF
     C                   MOVEL     *BLANKS       PLTODO
     C                   MOVEL     PLTEXT        PLTODO
     C     PACDIS        IFNE      PLCDIS
     C     PACDIS        ANDGT     *ZEROS
     C     PANFOR        ANDEQ     *BLANKS
     C                   GOTO      SIGO
     C                   ENDIF
     C                   MOVE      *BLANKS       RNVO
     C                   MOVEL     *BLANKS       OLD
     C                   MOVEL     *BLANKS       OLD1
     C                   MOVEL     PLTEXT        OLD1
     C                   Z-ADD     1             I
     C                   Z-ADD     1             N                 4 0
     C     I             DOWLE     WWCANC
     C                   MOVE      @PD(I)        D                 4 0
     C                   MOVE      @PH(I)        H                 4 0
     C                   MOVE      @L(I)         L                 4 0
     C                   MOVEL     *BLANKS       CAMPO          4096
     C     L             SUBST     PLTEXT:D      CAMPO
     C                   MOVE      @T(I)         T                 1
     C                   EXSR      SRREGI
     C                   ADD       1             I
     C                   ENDDO
     C                   SUB       1             N
     C                   MOVE      ' '           RNVO(N)
     C                   MOVEL     *BLANKS       PLTEXT
     C                   MOVEL     RN1           PLTEXT
     C                   UPDATE    REBC43PL
     C     SIGO          TAG
     C                   READ      BC43PL                                 90
     C                   ENDDO
     C*
     C     ENCONV        ENDSR
     C*----------------------------------------------------------------*
     C* SRREGI - Corrije Campos.
     C*----------------------------------------------------------------*
     C     SRREGI        BEGSR
     C*
     C                   Z-ADD     *ZEROS        V                 2 0
     C                   Z-ADD     *ZEROS        W                 2 0
     C                   Z-ADD     *ZEROS        X                 2 0
     C                   Z-ADD     *ZEROS        Y                 2 0
     C                   Z-ADD     *ZEROS        Z                 2 0
     C                   MOVE      *BLANK        OLD
     C                   MOVEL     CAMPO         OLD1
     C     @C(I)         IFEQ      'E'
     C                   MOVEL     *BLANKS       DTAINP          500
     C                   MOVEL     *BLANKS       CVTTYP            1
     C                   MOVEL     *BLANKS       DTAOUT         1000
     C                   Z-ADD     *ZEROS        LENOUT           15 0
     C                   MOVEL     CAMPO         DTAINP
     C                   MOVE      @E(I)         CVTTYP
     C                   CALL      'SBBAEWRG'
     C                   PARM                    DTAINP
     C                   PARM                    CVTTYP
     C                   PARM                    DTAOUT
     C                   PARM                    LENOUT
     C*
     C                   MOVEL     DTAOUT        CAMPO
     C*
     C                   EXSR      SRMAS5
     C                   GOTO      ENREGI
     C                   ENDIF
     C     @C(I)         IFEQ      'N'
     C                   EXSR      SRMAS3
     C                   GOTO      ENREGI
     C                   ENDIF
     C     @C(I)         IFEQ      'G'
     C                   EXSR      SRMAS4
     C                   GOTO      ENREGI
     C                   ENDIF
     C     @T(I)         IFEQ      'N'
     C                   EXSR      SRMAS1
     C                   EXSR      SRMAS2
     C                   ELSE
     C                   EXSR      SRMAS3
     C                   ENDIF
     C*
     C     ENREGI        ENDSR
     C*----------------------------------------------------------------*
     C* SRMAS1 - Corrije el Campo Numérico.
     C*----------------------------------------------------------------*
     C     SRMAS1        BEGSR
     C*
     C                   MOVE      *OFF          *IN11
     C                   Z-ADD     1             Z
     C                   Z-ADD     *ZEROS        X
     C     *IN11         DOWEQ     *OFF
     C     Z             ANDLE     L
     C     OLD(Z)        IFNE      '.'
     C                   MOVE      OLD(Z)        X
     C                   ELSE
     C                   MOVE      0             OLD(Z)
     C                   ENDIF
     C     X             IFGT      0
     C                   MOVE      *ON           *IN11
     C                   ELSE
     C     OLD(Z)        IFEQ      '0'
     C     OLD(Z)        OREQ      ' '
     C                   MOVE      *BLANK        OLD(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRMAS2 - Corrije el Campo Numérico.
     C*----------------------------------------------------------------*
     C     SRMAS2        BEGSR
     C*
     C                   MOVE      *OFF          *IN12
     C                   MOVE      *OFF          *IN13
     C                   Z-ADD     1             Z
     C                   Z-ADD     *ZEROS        X
     C                   Z-ADD     *ZEROS        Y
     C     Z             DOWLE     L
     C     OLD(Z)        IFNE      ' '
     C                   ADD       1             Y
     C                   MOVE      OLD(Z)        X
     C                   MOVE      *ON           *IN12
     C                   ENDIF
     C     X             IFGT      0
     C     *IN12         OREQ      *ON
     C                   MOVE      X             OLD(Y)
     C                   MOVE      X             RNVO(N)
     C                   ADD       1             N
     C                   ENDIF
     C                   ADD       1             Z
     C                   ENDDO
     C     L             SUB       Y             W
     C     W             IFGT      0
     C     1             DO        W
     C     *IN13         IFEQ      *OFF
     C                   ADD       1             Y
     C                   MOVE      ' '           OLD(Y)
     C                   MOVE      ';'           RNVO(N)
     C                   MOVE      *ON           *IN13
     C                   ADD       1             N
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C  N13              MOVE      ';'           RNVO(N)
     C  N13              ADD       1             N
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRMAS3 - Corrije el Campo Alfabetico.
     C*----------------------------------------------------------------*
     C     SRMAS3        BEGSR
     C*
     C                   MOVE      *OFF          *IN10
     C                   MOVE      *OFF          *IN11
     C                   MOVE      *OFF          *IN15
     C                   Z-ADD     *ZEROS        R                 4 0
     C                   Z-ADD     N             P                 4 0
     C                   Z-ADD     1             Z
     C                   Z-ADD     *ZEROS        B                 2 0
     C                   MOVEL     *BLANKS       U                 1
     C     CAMPO         IFEQ      *BLANKS
     C                   MOVE      ';'           RNVO(N)
     C                   ADD       1             N
     C                   GOTO      ENMAS3
     C                   ENDIF
     C     *IN11         DOWEQ     *OFF
     C     Z             ANDLE     L
     C     OLD(Z)        IFEQ      ' '
     C                   ADD       1             B
     C                   MOVE      *ON           *IN15
     C     B             IFEQ      3
     C                   MOVE      *ON           *IN11
     C                   MOVE      *ON           *IN10
     C     I             IFLT      WWCANC
     C                   SUB       1             N
     C                   MOVE      ';'           RNVO(N)
     C                   ADD       1             N
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C     B             IFEQ      2
     C     R             ANDEQ     L
     C                   MOVE      *ON           *IN11
     C                   MOVE      *ON           *IN10
     C*                    SUB  1         N                       |||
     C                   MOVE      ';'           RNVO(N)
     C                   ADD       1             N
     C                   LEAVE
     C                   ENDIF
     C     B             IFEQ      1
     C     R             ANDEQ     L
     C                   MOVE      *ON           *IN11
     C                   MOVE      *ON           *IN10
     C                   MOVE      ';'           RNVO(N)
     C                   ADD       1             N
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     OLD(Z)        IFNE      ' '
     C   15              SUB       1             B
     C   15              MOVE      *OFF          *IN15
     C                   Z-ADD     N             P
     C                   ENDIF
     C                   MOVE      OLD(Z)        U
     C                   MOVE      U             RNVO(N)
     C                   ADD       1             N
     C                   ADD       1             Z
     C                   ADD       1             R
     C                   ENDDO
     C  N10              MOVE      ';'           RNVO(N)
     C  N10              ADD       1             N
     C     N             IFGT      P
     C     P             ADD       1             N
     C                   MOVE      ';'           RNVO(N)
     C                   ADD       1             N
     C                   ENDIF
     C*
     C     ENMAS3        ENDSR
     C*----------------------------------------------------------------*
     C* SRMAS4 - Corrije el Campo Numérico c/Signo (definida char).
     C*----------------------------------------------------------------*
     C     SRMAS4        BEGSR
     C*
     C                   MOVE      *OFF          *IN12
     C                   MOVE      *OFF          *IN13
     C                   Z-ADD     1             Z
     C                   MOVEL     *BLANKS       G                 1
     C                   Z-ADD     *ZEROS        X
     C                   Z-ADD     *ZEROS        Y
     C     Z             DOWLE     L
     C     OLD(Z)        IFNE      '-'
     C     OLD(Z)        ANDNE     '0'
     C                   MOVE      *ON           *IN12
     C                   ENDIF
     C     OLD(Z)        IFEQ      '-'
     C                   MOVE      OLD(Z)        U
     C                   MOVE      U             RNVO(N)
     C                   ADD       1             N
     C                   ADD       1             Y
     C                   GOTO      SIGM4
     C                   ENDIF
     C                   MOVE      OLD(Z)        X
     C     OLD(Z)        IFEQ      '0'
     C     *IN12         ANDEQ     *OFF
     C                   GOTO      SIGM4
     C                   ENDIF
     C     X             IFGT      0
     C     *IN12         OREQ      *ON
     C                   ADD       1             Y
     C                   MOVE      X             OLD(Y)
     C                   MOVE      X             RNVO(N)
     C                   ADD       1             N
     C                   ENDIF
     C     SIGM4         TAG
     C                   ADD       1             Z
     C                   ENDDO
     C     L             SUB       Y             W
     C     W             IFGT      0
     C     1             DO        W
     C     *IN13         IFEQ      *OFF
     C                   ADD       1             Y
     C                   MOVE      ' '           OLD(Y)
     C                   MOVE      ';'           RNVO(N)
     C                   MOVE      *ON           *IN13
     C                   ADD       1             N
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C  N13              MOVE      ';'           RNVO(N)
     C  N13              ADD       1             N
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRMAS5 - Corrije el Campo Numérico c/Signo (definida char).
     C*----------------------------------------------------------------*
     C     SRMAS5        BEGSR
     C*
     C                   MOVE      *OFF          *IN12
     C                   MOVE      *OFF          *IN13
     C                   Z-ADD     1             Z
     C                   MOVEL     *BLANKS       G                 1
     C                   Z-ADD     *ZEROS        X
     C                   Z-ADD     *ZEROS        Y
     C                   MOVEL     *BLANKS       OLD1
     C                   MOVEL     CAMPO         OLD1
     C                   Z-ADD     LENOUT        L
     C     Z             DOWLE     L
     C                   MOVE      OLD(Z)        RNVO(N)
     C                   ADD       1             N
     C                   ADD       1             Z
     C                   ENDDO
     C     *IN13         IFEQ      *OFF
     C                   MOVE      ';'           RNVO(N)
     C                   MOVE      *ON           *IN13
     C                   ADD       1             N
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
