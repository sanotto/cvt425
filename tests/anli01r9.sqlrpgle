*SRCMBRTXT:LINK: Limpia Giro en Desc en/CAH LINK  
     H
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: ANLI00R9                                       *
     H*                                                               *
     H*  PROGRAM NO: Genera Giro en Desc. en C. Ah.                   *
     H*                                                               *
     H*  DATE: 09/08/2013                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*----------------------------------------------------------------
     FACCTAC11  UF   E           K DISK    RENAME(REACCTAC:R11)
     C*-------------------------------------------------------------------------
     c*
     c                   ExSr      LimpiaGDE
     c                   ExSr      Endpgm
     C*-------------------------------------------------------------------------
     c* LimpiaGDE: Limpia los adelantos que hubiere
     C*-------------------------------------------------------------------------
     c     LimpiaGDE     BegSr
     c*
     C                   If        FirstRun=*Off
     c                   LeaveSr
     c                   EndIf
     c                   Z-ADD     1             FUIMON
     C                   MOVE      '04'          FUIGRC
     C*
     c     KFU113        Chain     R11                                99
     C                   DoW       *In99 = *Off
     c                   Z-Add     *Zero         FU$GDE
     c                   Update    R11
     c     KFU113        ReadE     R11                                    99
     c                   EndDo
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* EndPgm: Finalizar el Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR : Subrutina de inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     c     KFU113        KList
     c                   KFld                    FUIMON
     c                   KFld                    FUIGRC
     c*
     c                   Z-Add     *Zero         PAFEPR            8 0
     c*
     C* ... Buscar Ultimo periodo vigente en anliha
     c/Exec Sql
     c+                  SELECT
     C+                            MAX(ANFEPR)
     C+                  INTO      :PAFEPR
     C+                  FROM      ANLIHA
     c/End-Exec
     C*
     C* ... Ver si es la primera corrida para el periodo
     c                   Move      *Blank        WWFLAG            1
     c/Exec Sql
     C+   SELECT
     C+       MAX(SUBSTR(ANRENG, 12, 1))
     C+       INTO :WWFLAG
     C+   FROM
     C+       ANLIHA
     C+   WHERE
     C+        ANFEPR = :PAFEPR
     c/End-Exec
     c*
     C                   Move      *On           FirstRun          1
     C                   If        WWFLAG = '*'
     C                   Move      *Off          FirstRun
     c                   EndIf
     C*
     C                   EndSr
