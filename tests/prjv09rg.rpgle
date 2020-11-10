*SRCMBRTXT:Prestamos-Solicitud de Empresa Asegurad
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA - Préstamos                              *
     H*                                                               *
     H*  PROGRAM NAME: PRJV09RG                                       *
     H*                                                               *
     H*  PROGRAM NO: Solicita Empresa de Seguros                      *
     H*                                                               *
     H*  DATE: 30/10/2009                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*---------------------------------------------------------------*
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     F*---------------------------------------------------------------*
     FBAENRE    IF   E           K DISK
     F*---------------------------------------------------------------*
     FPRCRED    IF   E           K DISK
     FPRLICR    IF   E           K DISK
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
     C                   MOVEL     *BLANKS       PAERRO
     C                   CALL      'PRJV09R1'
     C                   PARM                    JVISUC
     C                   PARM                    JVINCR
     C                   PARM                    JVIDEG
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
     C                   ELSE
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
     C                   MOVE      'C'           PAIMOP
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* CHKCRE : VERIFICA SI EL CREDITO DEBE SOLICITAR UNA EMPRESA DE SEGURO
     C*-------------------------------------------------------------------------
     C     CHKCRE        BEGSR
     C*
     C     KEY003        CHAIN     REPRCRED                           99
     C   99              EXSR      ENDPGM
     C*
     C                   IF        JVILCR=80  OR
     C                             JVILCR= 5  OR
     C                             JVILCR= 6  OR
     C                             JVILCR= 7
     C                   MOVEL     '1'           PAIMOP
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C*    JVILCR        IFNE      5
     C*    JVILCR        ANDNE     6
     C*    JVILCR        ANDNE     7
     C     JVILCR        IFNE      9
     C     JVILCR        ANDNE     1
     C     JVILCR        ANDNE     2
     C                   EXSR      ENDPGM
     C                   ENDIF
     C     KEY004        CHAIN     REPRLICR                           99
     C     DUITRU        IFNE      10
     C     DUITRU        ANDNE     11
     C                   EXSR      ENDPGM
     C                   ENDIF
     C     DUIOPE        IFNE      80
     C                   EXSR      ENDPGM
     C                   ENDIF
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
     C                             ( JVILCR=80  OR
     C                               JVILCR= 5  OR
     C                               JVILCR= 6  OR
     C                               JVILCR= 7    )
     C                   GOTO      SIGO
     C                   ENDIF
     C                   IF        ER$I01=2
     C                   GOTO      SIGO
     C                   ENDIF
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
     C                   PARM                    PAIMOP
     C*
     C     KEY001        KLIST
     C                   KFLD                    S1IJOB
     C*
     C     KEY002        KLIST
     C                   KFLD                    WWITRG
     C*
     C     KEY003        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAINCR
     C                   KFLD                    PAIDEG
     C*
     C     KEY004        KLIST
     C                   KFLD                    JVILCR
     C*
     C     *LIKE         DEFINE    JVISUC        PAISUC
     C     *LIKE         DEFINE    JVINCR        PAINCR
     C     *LIKE         DEFINE    JVIDEG        PAIDEG
     C     *LIKE         DEFINE    @PJOBN        @NJOBN
     C     *LIKE         DEFINE    JVIMOP        PAIMOP
     C     *LIKE         DEFINE    JVIMOP        PAERRO
     C     *LIKE         DEFINE    ERITRG        WWITRG
     C     *LIKE         DEFINE    S1REFC        WXREFC
     C*
     C     1             CHAIN     RESGSYSV
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   MOVE      @CISUC        PAISUC
     C                   MOVE      @CINCR        PAINCR
     C                   MOVE      @CIDEG        PAIDEG
     C                   MOVEL     @CREFC        WXREFC
     C*
     C                   MOVE      *BLANKS       PAIMOP
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
