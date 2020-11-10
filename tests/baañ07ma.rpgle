*SRCMBRTXT:Consulta base de terroristas           
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: BAAÑ07MA                                       *
     H*                                                               *
     H*  PROGRAM NO: Consulta base de terroristas y lavado de dinero  *
     H*                                                               *
     H*  DATE:29/06/2006                                              *
     H*                                                               *
     H*  AUTHOR: Fabian Romero                                        *
     H*                                                               *
     H*****************************************************************
     FBASUPT    O    E             DISK
     FSGSYSV    IF   E             DISK
     F@CPIUSD   IF   E           K DISK
     F*----------------------------------------------------------------*
     D*----------------------------------------------------------------
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D ERRDS           DS
     D  ERRTXT                 1    255
     D  WWNCU1                 1     55
     D  WWNCU2                56    110
     D  WWNCU3               111    165
     D  WWNCU4               166    220
     D  WWNCB1               221    275
     D  WWNCB2               276    330
     D  WWNCB3               331    385
     D  WWNCB4               386    440
     D*----------------------------------------------------------------*
     D SBMSUP          C                   CONST('SE HA SOMETIDO UNA P-
     D                                     ETICION DE SUPERVISI-
     D                                     ON PARA DAR DE ALTA -
     D                                     A ESTA PERSONA')
     C*---------------------------------------------------------------*
     C* PRINCIPAL
     C*---------------------------------------------------------------*
     C* ... Edicion del nombre
     C*                  CALL      'BAAÑ08RG'
     C*                  PARM                    WWNYAP           30
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             CONT              1 0
     C                   CALL      'BAAÑ07R1'
     C                   PARM                    I
     C                   PARM                    O
     C                   PARM                    FOUND
     C     FOUND         DOWEQ     *ON
     C*
     C     CONT          IFGT      1
     C     LINE          CAT       '%20':0       LINE
     C                   ENDIF
     C     LINE          CAT       O:0           LINE
     C*
     C                   MOVE      *BLANKS       O               255
     C                   CALL      'BAAÑ07R1'
     C                   PARM                    I
     C                   PARM                    O
     C                   PARM                    FOUND
     C                   ADD       1             CONT
     C                   ENDDO
     C                   MOVE      WWINDO        AUINDO           15
     C                   MOVE      *OFF          INNBR             1
     C                   MOVE      *BLANKS       PAINDO           15
     C                   MOVE      *BLANKS       C                 1
     C     1             DO        15            X                 2 0
     C                   EVAL      C=%SUBST(AUINDO:X:1)
     C                   IF        C <> '0'
     C                   MOVE      *ON           INNBR
     C                   ENDIF
     C                   IF        INNBR = *ON
     C                   EVAL      PAINDO=%TRIM(PAINDO)+ C
     C                   ENDIF
     C                   ENDDO
     C*
     C                   CALL      'BAAÑ07CL'
     C                   PARM                    LINE
     C                   PARM                    WWNYAP           30
     C                   PARM                    PAINDO
     C                   PARM                    WWTPER            1
     C     @PJOBN        CHAIN     @CPIUSD                            99
     C     @COPEC        IFEQ      'N'
     C                   MOVE      WWITDO        STITDO
     C                   MOVE      WWINDO        STINDO
     C                   MOVE      WWNYAP        STNYAP
     C                   TIME                    STHORA
     C                   Z-ADD     *ZERO         STHSUP
     C                   MOVE      AASFEI        STFING
     C                   Z-ADD     *ZERO         STFAAL
     C                   MOVE      @PUSER        STIUSR
     C                   WRITE     REBASUPT
     C                   MOVEL(P)  SBMSUP        ERRTXT
     C                   EXSR      DSPERR
     C                   ENDIF
     C                   SETON                                        LR
     C*----------------------------------------------------------------*
     C* INICIO
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    WWNYAP           30
     C                   PARM                    WWITDO            2 0
     C                   PARM                    WWINDO           15 0
     C                   PARM                    WWTPER            1
     C*
     C     KSUPT         KLIST
     C                   KFLD                    WWITDO
     C                   KFLD                    WWINDO
     C*
     c* Este programa deja de controlar las altas de nuevos clientes contra
     c* la base de terroristas. Desde el 05/04/2017 este control lo realiza
     c* el sistema worldsys. Esto fue solicitado por Noe Gomez.
     c* Se agregan estan lineas para evitar la modificacion del MM
     c*
     C                   SETON                                        LR
     c                   RETURN
     C     1             CHAIN     RESGSYSV                           99
     C*
     C                   MOVE      WWNYAP        I               255
     C                   MOVE      *BLANKS       O               255
     C                   MOVE      *BLANKS       LINE            255
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* DSPERR
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   CALL      'BAER00RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C                   ENDSR
