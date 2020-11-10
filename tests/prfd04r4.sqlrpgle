*SRCMBRTXT:Aut.Federal-Man.Alta de Op.-Gen cupones
     FPRCUOT06  IF   E           K DISK
     FPRMOVI02  UF   E           K DISK
     FSGSYSV    IF   E             DISK
     c*
     c                   ExSr      LimpiaPRMOVI
|    C     KeyCre        Chain     REPRCUOT                           99
|+---C                   DoW       *IN99=*OFF
     c                   ExSr      GeneraCupon
||   C     KeyCre        ReadE     REPRCUOT                               99
|+---C                   ENDDO
     c*
     C                   EXSR      EndPgm
     C*-------------------------------------------------------------------------
     C* GeneraCupon: Genera Cupón para la cuota
     c*-------------------------------------------------------------------------
     c     GeneraCupon   BegSr
     c*
|||  C                   MOVEL     AASFEI        PAFAAS            6 0
|||  C                   MOVEL     AASFEI        PASFEI            8 0
|||  C                   CALL      'PRHA01SC'
|||  C                   PARM                    PAFAAS
|||  C                   PARM                    WWISUC
|||  C                   PARM                    WWINCR
|||  C                   PARM                    WWIDEG
|||  C                   PARM                    KGICUO
|||  C                   PARM                    PASFEI
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* LimpiaPRMOVI: Para Generar los cupones nuevamente
     C*-------------------------------------------------------------------------
     c     LimpiaPRMOVI  BegSr
     C*
|    C     KeyCre        Chain     REPRMOVI                           99
|+---C                   DoW       *IN99 = *OFF
||   C                   Delete    REPRMOVI
||   C     KeyCre        ReadE     REPRMOVI                               99
|+---C                   EndDo
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR: Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *Entry        PList
     c                   Parm                    WWISUC
     c                   Parm                    WWINCR
     c                   Parm                    WWIDEG
     c*
     c     KEYCRE        KList
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c*
     c     *Like         Define    MIISUC        WWISUC
     c     *Like         Define    MIINCR        WWINCR
     c     *Like         Define    MIIDEG        WWIDEG
     C*
     C     1             Chain     RESGSYSV
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* EndPgm: Finalización de Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BEGSR
     C*
     C                   EVAL      *INLR=*OFF
     C                   RETURN
     C*
     C                   ENDSR
