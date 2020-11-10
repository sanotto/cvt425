*SRCMBRTXT:Personas fisicas Supervision de terrori
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     F*----------------------------------------------------------------*
     FBASUPT01  UF   E           K DISK
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     FBASCTM    UF A E           K DISK
     FSGSYSV    IF   E             DISK
     F@CPISYS   UF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     D*----------------------------------------------------------------*
     D T               S             55    DIM(11) CTDATA PERRCD(1)
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
     C                   EXSR      SHOWTE
     C                   DOW       @FN(03) =*OFF AND
     C                             @FN(12) = *OFF
     C     @ZRRNO        CHAIN     RTMP                               99
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     C                   Z-ADD     S1ISUC        STITDO
     C                   Z-ADD     S1INCR        STINDO
     C                   EXSR      SUPERVISAR
     C                   EXSR      SHOWTE
     C                   ENDDO
     C                   SETON                                        LR
     C*-------------------------------------------------------------------------
     C* SUPERVISAR:
     C*-------------------------------------------------------------------------
     C     SUPERVISAR    BEGSR
     C     KEY001        CHAIN     REBASUPT                           99
     C                   IF        *IN99 = *OFF
     C                   MOVE      *BLANKS       ERRTXT
     C                   MOVEL(P)  T(1)          WWNCU1
     C                   MOVEL(P)  T(2)          WWNCU2
     C                   MOVEL(P)  T(3)          WWNCU3
     C                   MOVEL(P)  STNYAP        WWNCU4
     C                   EVAL      WWNCB1=%trim(T(5))+%EDITC(STITDO:'J')
     C                   EVAL      WWNCB2=%trim(T(6))+%EDITC(STINDO:'J')
     C                   EVAL      WWNCB4=T(8)
     C                   EXSR      DSPERR
     C                   IF        @FN(10)=*ON
     C                   Z-ADD     AASFEI        STFAAL
     C                   MOVE      @PUSER        STIUSA
     C                   UPDATE    REBASUPT
     C                   ENDIF
     C                   IF        @FN(22)=*ON
     C                   Z-SUB     1             STFAAL
     C                   UPDATE    REBASUPT
     C                   ENDIF
     C                   IF        @FN(19)=*ON
     C                   Z-SUB     2             STFAAL
     C                   UPDATE    REBASUPT
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* SHOWTE : MUESTRA EL TE
     C*-------------------------------------------------------------------------
     C     SHOWTE        BEGSR
     C*
     c                   EXSR      FILLTE
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Supervision Control de Terroristas'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Subsistema .........'+
     C                                    x'22'+'CL Administración de clientes'
     C                   EVAL      WWTIT3='Menú ...............'+
     C                                    x'22'+'OF Operaciones de oficina'
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+'Tipo de Doc.' +
     C                                    x'21'+ 'Nro De Doc. ' +
     C                                    x'21'+ 'Nombre'
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
     C     *LOVAL        SETLL     rebasupt
     c                   READ      REBASUPT                               99
     C                   DOW       *IN99 = *OFF
     C                   Z-ADD     STITDO        S1ISUC
     C                   Z-ADD     STINDO        S1INCR
     C                   EVAl      s1dacl=' '+%EDITC(STITDO:'J')+'  '+
     C                                    %EDITW(STINDO:'               ')
     C                                    +' '+
     C                                    STNYAP
     C                   WRITE     REBASCTM
     c                   READ      REBASUPT                               99
     C                   ENDDO
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     KEY001        KLIST
     C                   KFLD                    STITDO
     C                   KFLD                    STINDO
     C     KEY002        KLIST
     C                   KFLD                    S1IJOB
     C*
     C     1             CHAIN     RESGSYSV
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
     C     KEY002        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBASCTM
     C     KEY002        CHAIN     REBASCTM                           99
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
123456*890123456789012345678901234567890123456789012345
**
Supervisión de control de Terroristas                      1
                                                           2
Autorizar el Alta de:                                      3
                                                           4
Tipo de Documento:                                         5
Nro  de Docuemnto:                                         6
                                                           7
F3=Salir F10=Autorizar F22=Denegar  F19=Error de Carga     8
                                                           9
                                                           10
                                                           11
