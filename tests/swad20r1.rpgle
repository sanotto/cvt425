*SRCMBRTXT:Switch-Adapter      -PF-380000-Cons PF 
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD20R1                                       *
     H*                                                               *
     H*  PROGRAM NO: Busca Plazo Fijo y devuelve tipo                 *
     H*                                                               *
     H*  DATE: 13/09/2017                                             *
     H*                                                               *
     H*  AUTHOR: Herrera, Federico                                    *
     H*                                                               *
     F*----------------------------------------------------------------
     FPFCERT06  IF   E           K DISK
     FPFCERT    IF   E           K DISK    RENAME(REPFCERT:RPF)
     F*----------------------------------------------------------------
     c*
     c                   ExSr      BuscaPF
     c                   ExSr      EndPgm
     c*--------------------------------------------------------------------
     c* BuscaPF: Busca Plazo Fijo
     c*--------------------------------------------------------------------
     c     BuscaPF       BegSr
     c*
     c*
     C                   Move      PANCER        KKINCE            7 0
     C                   Z-Add     KKINCE        WKINCE
     c     KIZ0000       Chain     RPF                                99
     c                   If        *In99 = *Off
     c                   Move      IZIGCE        PATIPF
     c                   Eval      PAIERR=*blanks
     c                   Else
     c                   Eval      PAIERR='No se encontro PF'
     C                   Endif
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* *InzSr: Inicialización del Programa
     c*--------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    PATIPF            1
     c                   Parm                    PADBSU            5 0
     c                   Parm                    PANCER           10 0
     c                   Parm                    PAIERR           40
     c*
     c     *Like         Define    IZISUC        WKISUC
     c     *Like         Define    IZINCE        WKINCE
     c     *Like         Define    IZICCL        WKICCL
     c*
     c*
     c                   Z-Add     PADBSU        WKISUC
     c*
     C     KIZ0600       KList
     c                   KFld                    WKISUC
     c                   KFld                    WKICCL
     C     KIZ0000       KList
     c                   KFld                    WKISUC
     c                   KFld                    WKINCE
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* EndPgm: Fin de Programa
     c*--------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
