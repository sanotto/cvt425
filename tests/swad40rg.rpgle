*SRCMBRTXT:Switch-Adapter      -PF-380000-Contrasi
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD40RG                                       *
     H*                                                               *
     H*  PROGRAM NO: Consulta Plazos Fijos-REVERSA                    *
     H*                                                               *
     H*  DATE: 18/09/2018                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*----------------------------------------------------------------
     FPFCERT    UF   E           K DISK
     FPFTMOD01  UF   E           K DISK
     FACMOVD03  UF   E           K DISK
     FCCMOCT06  UF   E           K DISK
     c                   ExSr      ContraPF
     c                   ExSr      ContMovEnCta
     c                   ExSr      EndPgm
     c*----------------------------------------------------------------
     c* ContMovEnCta
     c*----------------------------------------------------------------
     c     ContMovEnCta  BegSr
     c*
     c                   If        PADBTC='CC'
     c                   ExSr      ContMovCC
     c                   Else
     c                   ExSr      ContMovCA
     c                   EndIf
     c*
     c                   EndSr
     c*----------------------------------------------------------------
     c* ContMovCC
     c*----------------------------------------------------------------
     c     ContMovCC     BegSr
     c*
     C     KCF0100       Chain     RECCMOCT                           99
     c                   DoW       *IN99 = *Off
     c                   If        CFICHE= PAPFCE and CF$IMP=PA$IMP
     c                   Z-Add     1             CFIASC
     c                   Update    RECCMOCT
     c                   Leave
     c                   Endif
     C     KCF0100       ReadE     RECCMOCT                               99
     c                   EndDo
     c*
     c                   EndSr
     c*----------------------------------------------------------------
     c* ContMovCA
     c*----------------------------------------------------------------
     c     ContMovCa     BegSr
     c*
     C     KGC0100       Chain     REACMOVD                           99
     c                   DoW       *IN99 = *Off
     c                   If        GCICHE= PAPFCE and GC$IMP=PA$IMP
     c                             AND GCIMCA=778
     c                   Z-Add     1             GCIASC
     c                   Update    REACMOVD
     c                   Leave
     c                   Endif
     C     KGC0100       ReadE     REACMOVD                               99
     c                   EndDo
     c*
     c                   EndSr
     c*----------------------------------------------------------------
     c* ContraPF
     c*----------------------------------------------------------------
     c     ContraPF      BegSr
     c*
     C     KIZ0100       Chain     REPFCERT                           99
     c   99              Eval      PAIERR='213Mov. no enc.para reversa'
     c   99              ExSr      EndPgm
     c                   If        IZIECE='C'
     c                   Eval      PAIERR='213Mov. no enc.para reversa'
     c                   ExSr      EndPgm
     c                   EndIf
     c                   Move      'C'           IZIECE
     c                   Update    REPFCERT
     c*
     c                   ExSr      ContraPFTMOD
     c*
     c*
     c                   EndSr
     c*----------------------------------------------------------------
     c* ContraPFTMOD
     c*----------------------------------------------------------------
     c     ContraPFTMOD  BegSr
     c*
     C     KKZ0100       Chain     REPFTMOD                           99
     c                   DoW       *IN99 = *Off
     c                   Delete    REPFTMOD
     C     KKZ0100       ReadE     REPFTMOD                               99
     c                   EndDo
     c*
     c                   EndSr
     c*----------------------------------------------------------------
     c* *InzSr:Rutina Inicializacion
     c*----------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     C                   Parm                    PADBTC            2
     C                   Parm                    PADBSU            5 0
     C                   Parm                    PADBCT           11 0
     C                   Parm                    PAFING            8 0
     C                   Parm                    PAHING            6 0
     C                   Parm                    PAICHE            7 0
     C                   Parm                    PAPFCE           11 0
     C                   Parm                    PA$IMP           15 2
     C                   Parm                    PAIERR           40
     c*
     c     KIZ0100       KList
     c                   KFld                    PADBSU
     C                   KFld                    PAPFCE
     c     KGC0100       KList
     c                   KFld                    PADBSU
     c                   KFld                    PADBCT
     c                   KFld                    IZFALT
     c     KCF0100       KList
     c                   KFld                    PADBSU
     c                   KFld                    PADBCT
     c                   KFld                    IZFALT
     c     KKZ0100       KList
     c                   KFld                    IZISUC
     c                   KFld                    IZIGCE
     c                   KFld                    IZITCE
     c                   KFld                    IZINCE
     c*
     c                   Z-Add     778           WWIMCC            3 0
     c*
     c                   EndSr
     c*----------------------------------------------------------------
     c* EndPgm:
     c*----------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
