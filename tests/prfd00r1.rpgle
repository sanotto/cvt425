*SRCMBRTXT:Aut.Federal-Recupera Autorización      
     H DEBUG
     H DECEDIT(',') DATEDIT(*DMY/)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: - Obtiene el Recibo y el Nro de Autorización   *
     H*                  de Federal para una credito pasado como      *
     H*                  parámetro                                    *
     H*                                                               *
     H*  PROGRAM NO: PRFD00R1                                         *
     H*                                                               *
     H*  DATE:    24/04/2013                                          *
     H*                                                               *
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*****************************************************************
     FPRAFED02  IF   E           K DISK
     C*
     C     KAF023        SetGt     REPRAFED
     C     KAF023        ReadPe    REPRAFED                               99
     C                   DoW       *IN99 = *Off
     c                   Move      *Off          PAIERR
     c                   If        AFIOPT='A' and AFESTA='A'
     c                   Z-Add     AFINDO        PAINDO
     c                   Move      AFIBCF        PAIBCF
     c                   Move      AFIRED        PAIRED
     c                   Move      AFISEX        PAISEX
     c                   Z-Add     AFINCE        PAINCE
     c                   Move      AFFAU1        PAFAU1
     c                   Move      AFHAU1        PAHAU1
     c                   Z-Add     AF$CUO        PA$CUO
     c                   Leave
     c                   EndIf
     C     KAF023        ReadPe    REPRAFED                               99
     c                   EndDo
     c                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C* ENDPGM: Fin del Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     c*
     C                   SetOn                                        LR
     C                   Return
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR: Rutina de Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PList
     c                   Parm                    PAISUC
     c                   Parm                    PAINCR
     c                   Parm                    PAIDEG
     c                   Parm                    PAINDO
     c                   Parm                    PAIBCF
     c                   Parm                    PAIRED
     c                   Parm                    PAISEX
     c                   Parm                    PAINCE
     c                   Parm                    PAFAU1
     c                   Parm                    PAHAU1
     c                   Parm                    PA$CUO
     c                   Parm                    PAIERR            1
     C*
     c     *Like         Define    AFISUC        PAISUC
     c     *Like         Define    AFINCR        PAINCR
     c     *Like         Define    AFIDEG        PAIDEG
     c     *Like         Define    AFINDO        PAINDO
     c     *Like         Define    AFIBCF        PAIBCF
     c     *Like         Define    AFIRED        PAIRED
     c     *Like         Define    AFISEX        PAISEX
     c     *Like         Define    AFINCE        PAINCE
     c     *Like         Define    AFFAU1        PAFAU1
     c     *Like         Define    AFHAU1        PAHAU1
     c     *Like         Define    AF$CUO        PA$CUO
     C*
     C     KAF023        KList
     c                   KFld                    PAISUC
     c                   KFld                    PAINCR
     c                   KFld                    PAIDEG
     C*
     c                   Move      *On           PAIERR
     C*
     c                   Z-Add     *ZERO         PAINDO
     c                   Move      *Blanks       PAIBCF
     c                   Move      *Blanks       PAIRED
     c                   Move      *Blanks       PAISEX
     c                   Z-Add     *ZERO         PAINCE
     c                   Z-Add     *ZERO         PA$CUO
     c                   Move      *Blanks       PAFAU1
     c                   Move      *Blanks       PAHAU1
     C*
     C                   ENDSR
