*SRCMBRTXT:Prestamos-Solicitud de Empresa Asegurad
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA - SCORING                               *
     H*                                                               *
     H*  PROGRAM NAME: BASC09RG                                       *
     H*                                                               *
     H*  PROGRAM NO: Solicita Empresa de Seguros                      *
     H*                                                               *
     H*  DATE:   03/07/2013                                           *
     H*                                                               *
     H*  AUTHOR: Sergio Cortes.                                       *
     H*                                                               *
     F*---------------------------------------------------------------*
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     F*---------------------------------------------------------------*
     FBAENRE    IF   E           K DISK
     F*---------------------------------------------------------------*
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     FBASCTM    UF A E           K DISK
     FSGSYSV    IF   E             DISK
     F@CPISYS   UF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     D QryStr          S           2048
     D Where           S           2048
     D*----------------------------------------------------------------*
     DERRDS            DS
     D ERRTXT                  1    255
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*----------------------------------------------------------------*
     C                   EXSR      CHKCRE
     C                   EXSR      SHOWTE
     C                   DOW       @FN(03) = *Off and @FN(12)=*Off
     C     @ZRRNO        CHAIN     RTMP                               99
     C                   IF        *IN99  = *OFF
     C                   MOVE      S1ISUC        PAIMOP
     C                   MOVE      *BLANKS       PAERRO
     C                   Z-ADD     WXQCUO        PAQCUO
     C                   CALL      'BASC09R1'
     C                   PARM                    PAISUC
     C                   PARM                    PAICCL
     C                   PARM                    PAIEMP
     C                   PARM                    PAILCR
     C                   PARM                    PAQCUO
     C                   PARM                    PAIMOP
     C                   PARM                    PAERRO
     C*
     C     PAERRO        IFEQ      '1'
     C                   EVAL      WWNCU1='El cliente supera la edad'
     C                   EVAL      WWNCU2='Máxima aceptada por la   '
     C                   EVAL      WWNCU3='Entidad aseguradora.     '
     C                   EVAL      WWNCU4=''
     C                   EVAL      WWNCB1='Seleccione otra Empresa. '
     C                   EVAL      WWNCB2=''
     C                   EVAL      WWNCB3='Presione Intro p/Continuar'
     C                   EVAL      WWNCB4=''
     C                   EXSR      DSPERR
     C                   Z-ADD     *ZEROS        PAQCUO
     C                   ELSE
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   MOVE      PAIMOP        @CIOLM
     C                   UPDATE    @CPIUSRR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      SHOWTE
     C                   ENDDO
     C                   EVAL      WWNCU1='Debe Seleccionar una Empresa'
     C                   EVAL      WWNCU2='De Seguros para poder'
     C                   EVAL      WWNCU3='Continuar.'
     C                   EVAL      WWNCU4=''
     C                   EVAL      WWNCB1='Si no puede seleccionar una'
     C                   EVAL      WWNCB2='Empresa, el créd. no puede'
     C                   EVAL      WWNCB3='Otorgarse.                '
     C                   EVAL      WWNCB4='Presione Intro p/Continuar'
     C                   EXSR      DSPERR
     C                   Z-ADD     *ZEROS        PAQCUO
     C                   MOVE      'C'           PAIMOP
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* CHKCRE : VERIFICA SI EL CREDITO DEBE SOLICITAR UNA EMPRESA DE SEGURO
     C*-------------------------------------------------------------------------
     C     CHKCRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ENDPGM : FINALIZA EL PROGRAMA
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   Z-ADD     PAISUC        @CISUC
     C                   Z-ADD     PAINCR        @CINCR
     C                   Z-ADD     PAIDEG        @CIDEG
     C                   MOVEL     WXREFC        @CREFC
     C                   Z-ADD     PXIJOB        @CIJOB
     C                   Z-ADD     PXISEQ        @CISEQ
     C                   Z-ADD     PXISUC        @CISUC
     C                   Z-ADD     PXINCR        @CINCR
     C                   Z-ADD     PXIDEG        @CIDEG
     C                   Z-ADD     PXQCUO        @CQCUO
     C                   Z-ADD     PXILCR        @CILCR
     C                   Z-ADD     PXIMON        @CIMON
     C                   Z-ADD     PX$INP        @C$INP
     C                   Z-ADD     PX$IMP        @C$IMP
     C                   Z-ADD     PXTGLI        @CTGLI
     C                   MOVEL     PXREFC        @CREFC
     C                   Z-ADD     PX$SAL        @C$SAL
     C                   Z-ADD     PX$MIN        @C$MIN
     C                   Z-ADD     PXIEMP        @CIEMP
     C                   UPDATE    @CPIUSRR
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* SHOWTE : MUESTRA EL TE
     C*-------------------------------------------------------------------------
     C     SHOWTE        BEGSR
     C*
     c                   EXSR      FILLTE
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Selección de Empresa Aseguradora'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Debe Seleccionar una Empresa ' +
     c                                    'Aseguradora'
     C                   EVAL      WWTIT3='Para Continuar'
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=''
     C                   MOVEL(P)  @PJOBN        @CIJOB
     C                   UPDATE    @CPIUSRR
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FILLTE : Cargar Termporal
     C*-------------------------------------------------------------------------
     C     FILLTE        BEGSR
     C                   EXSR      DLTTMP
     C                   MOVEL     'AC'          WWITRG
     C     KEY002        CHAIN     REBAENRE                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        ER$I01<>1 AND
     C                             ( PAILCR=80  OR
     C                               PAILCR= 5  OR
     C                               PAILCR= 6  OR
     C                               PAILCR= 7    )
     C                   GOTO      SIGO
     C                   ENDIF
     C                   IF        ER$I01=2
     C                   GOTO      SIGO
     C                   ENDIF
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      @PJOBN        S1IJOB
     C                   Z-ADD     ER$I01        S1ISUC
     C                   MOVE      *BLANKS       S1REFC
     C                   EVAl      s1dacl=ERDAV1
     C                   WRITE     REBASCTM
     C     SIGO          TAG
     C     KEY002        READE     REBAENRE                               99
     C                   ENDDO
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAQCUO
     C*
     C     KEY001        KLIST
     C                   KFLD                    S1IJOB
     C*
     C     KEY002        KLIST
     C                   KFLD                    WWITRG
     C*
     C*
     C     *LIKE         DEFINE    S1QCUO        PAQCUO
     C     *LIKE         DEFINE    S1QCUO        WXQCUO
     C     *LIKE         DEFINE    S1ISUC        PAISUC
     C     *LIKE         DEFINE    S1INCR        PAINCR
     C     *LIKE         DEFINE    S1IDEG        PAIDEG
     C     *LIKE         DEFINE    @CICCL        PAICCL
     C     *LIKE         DEFINE    @CIEMP        PAIEMP
     C     *LIKE         DEFINE    @CILCR        PAILCR
     C     *LIKE         DEFINE    @PJOBN        @NJOBN
     C     *LIKE         DEFINE    @CIBIS        PAIMOP
     C     *LIKE         DEFINE    @CIBIS        PAERRO
     C     *LIKE         DEFINE    ERITRG        WWITRG
     C     *LIKE         DEFINE    S1REFC        WXREFC
     C     *LIKE         DEFINE    @CIJOB        PXIJOB
     C     *LIKE         DEFINE    @CISEQ        PXISEQ
     C     *LIKE         DEFINE    @CISUC        PXISUC
     C     *LIKE         DEFINE    @CINCR        PXINCR
     C     *LIKE         DEFINE    @CIDEG        PXIDEG
     C     *LIKE         DEFINE    @CQCUO        PXQCUO
     C     *LIKE         DEFINE    @CILCR        PXILCR
     C     *LIKE         DEFINE    @CIMON        PXIMON
     C     *LIKE         DEFINE    @C$INP        PX$INP
     C     *LIKE         DEFINE    @C$IMP        PX$IMP
     C     *LIKE         DEFINE    @CTGLI        PXTGLI
     C     *LIKE         DEFINE    @CREFC        PXREFC
     C     *LIKE         DEFINE    @C$SAL        PX$SAL
     C     *LIKE         DEFINE    @C$MIN        PX$MIN
     C     *LIKE         DEFINE    @CIEMP        PXIEMP
     C*
     C     1             CHAIN     RESGSYSV
     C                   Z-ADD     PAQCUO        WXQCUO
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   Z-ADD     @CISUC        PAISUC
     C                   Z-ADD     @CINCR        PAINCR
     C                   Z-ADD     @CIDEG        PAIDEG
     C                   MOVEL     @CREFC        WXREFC
     C*
     C                   Z-ADD     @CISUC        PAISUC
     C                   Z-ADD     @CICAH        PAICCL
     C                   Z-ADD     @CIEMP        PAIEMP
     C                   Z-ADD     @CILCR        PAILCR
     C                   MOVE      *BLANKS       PAIMOP
     C                   Z-ADD     @CIJOB        PXIJOB
     C                   Z-ADD     @CISEQ        PXISEQ
     C                   Z-ADD     @CISUC        PXISUC
     C                   Z-ADD     @CINCR        PXINCR
     C                   Z-ADD     @CIDEG        PXIDEG
     C                   Z-ADD     @CQCUO        PXQCUO
     C                   Z-ADD     @CILCR        PXILCR
     C                   Z-ADD     @CIMON        PXIMON
     C                   Z-ADD     @C$INP        PX$INP
     C                   Z-ADD     @C$IMP        PX$IMP
     C                   Z-ADD     @CTGLI        PXTGLI
     C                   MOVEL     @CREFC        PXREFC
     C                   Z-ADD     @C$SAL        PX$SAL
     C                   Z-ADD     @C$MIN        PX$MIN
     C                   Z-ADD     @CIEMP        PXIEMP
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C     @CILCR        IFEQ      80
     C     @CIEMP        ANDEQ     3
     C*                  MOVE      *BLANK        @CIOLM
     C                   MOVE      '1'           @CIOLM
     C                   UPDATE    @CPIUSRR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      *ZERO         RRN              15 0
     C*
     C*
     C                   EVAL      CMDLNE='OVRDBF FILE(BASCTMRRN)'  +
     C                             ' TOFILE(*LIBL/BASCTM)'
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDLNE         1024
     C                   PARM      1024          CMDSZE           15 5
     C                   OPEN      BASCTMRRN
     C*
     C                   EXSR      DLTTMP
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DLTTMP        BEGSR
     C*
     C     KEY001        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBASCTM
     C     KEY001        CHAIN     REBASCTM                           99
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   CALL      'BAER01RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            90
     C                   ENDSR
