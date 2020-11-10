*SRCMBRTXT:Recupera Cotización de Una Moneda al dí
     FCOTIZA    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     C     @KEY07        CHAIN     COTIZA                             80
     C   80              Z-ADD     *ZERO         PACOTI
     C   80              EXSR      ENDPGM
     C*
     C                   Z-ADD     *ZEROS        WYFECH            8 0
     C                   Z-ADD     *ZEROS        DXFECH            8 0
     C*
     C                   Z-ADD     PAFECH        DXFECH
     C                   Z-ADD     PAFECH        PAFECH
     C                   Z-ADD     PAFECH        WYFECH
     C*
     C     INIDOL        TAG
     C                   Z-ADD     *ZEROS        PACOTI           12 6
     C*
     C     @KEY08        CHAIN     COTIZA                             80
     C  N80              Z-ADD     MU$COR        PACOTI
     C*
     C     VUELV1        TAG
     C     *IN80         IFEQ      *ON
     C     PACOTI        OREQ      1
     C     PAFECH        SUB       1             WYFECH
     C                   Z-ADD     PAFECH        PAFEC2            8 0
     C                   Z-ADD     *ZEROS        WYISUC            5 0
     C                   MOVE      *OFF          *IN43
     C     *IN43         DOWEQ     *OFF
     C                   MOVE      'IN'          PACINV            2
     C                   CALL      'SBBAINFE'
     C                   PARM                    WYFECH
     C                   PARM                    PACINV
     C                   MOVEL     *BLANK        PAIERR            1
     C                   MOVE      *OFF          *IN43
     C                   Z-ADD     WYFECH        PAFEC3            8 0
     C                   CALL      'SBBAVFEC'
     C                   PARM                    PAFEC3
     C                   PARM                    WYISUC
     C                   PARM                    PAIERR
     C     PAIERR        IFEQ      ' '
     C                   MOVE      *ON           *IN43
     C                   ELSE
     C                   SUB       1             PAFEC2
     C                   Z-ADD     PAFEC2        WYFECH
     C                   ENDIF
     C                   ENDDO
     C                   MOVE      'NI'          PACINV
     C                   CALL      'SBBAINFE'
     C                   PARM                    WYFECH
     C                   PARM                    PACINV
     C                   Z-ADD     99999         PAISUC
     C                   Z-ADD     WYFECH        PAFECH
     C     @KEY08        CHAIN     COTIZA                             80
     C  N80              Z-ADD     MU$COR        PACOTI
     C   80              GOTO      VUELV1
     C  N80MU$COR        IFEQ      1
     C                   SUB       1             WYFECH
     C                   GOTO      VUELV1
     C                   ENDIF
     C                   ENDIF
     C*
     C     ENDDOL        TAG
     C                   EXSR      ENDPGM
     C*----------------------------------------------------------------
     C* ENDPGM  : FIN DE PROGRAMA
     C*----------------------------------------------------------------*
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* *INZSR  : Inicialización
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAISUC            5 0
     C                   PARM                    PAIMON            9 0
     C                   PARM                    PAFECH            8 0
     C                   PARM                    PACOTI           12 6
     C*  Acceso a COTIZA
     C     @KEY07        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAIMON
     C     @KEY08        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAIMON
     C                   KFLD                    PAFECH
     C*
     C     1             CHAIN     SGSYSV                             80
     C*
     C     PAFECH        IFEQ      *ZERO
     C                   Z-ADD     AASFEI        PAFECH
     C                   ENDIF
     C*
     C                   ENDSR
