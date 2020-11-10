*SRCMBRTXT:Obtiene CUIT-NOMBRE p/Transferencia    
     FSPTRMH01  IF   E           k DISK
     FSPTRTR05  IF   E           k DISK
     c                   Move      *Blanks       PAOBSO
     C                   Select
     c                   When      PAIMCA = 264
     C                   ExSr      BusTrInmLink
     c                   When      PAIMCA = 480 OR
     c                             PAIMCA = 481 OR
     c                             PAIMCA = 482 OR
     c                             PAIMCA = 213 OR
     c                             PAIMCA = 303 OR
     c                             PAIMCA = 304
     C                   ExSr      BusTrCBU
     c                   EndSl
     C                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     C     BusTrInmLink  BegSr
     c*
     C     KMH0100       Chain     RESPTRMH                           99
     c                   If        *In99 = *Off
     c                   Eval      PAOBSO=%Trim(
     c                                      %EditW(mhicui:'   -        - ')
     c                                     ) +' '+
     c                                    %Trim(MHNCCL)
     C                   LeaveSr
     c                   EndIf
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C     BusTrCBU      BegSr
     c*
     C     KTR0500       Chain     RESPTRTR                           99
     c                   If        *In99 = *Off
     c                   Eval      PAOBSO=%Trim(
     c                                      %EditW(TRICUI:'      -        - ')
     C                                    ) + ' '+
     c                                    %Trim(TRNRSO)
     C                   LeaveSr
     c                   EndIf
     c*
     c                   ExSr      DiaAnterior
     c*
     C     KTR0500       Chain     RESPTRTR                           99
     c                   If        *In99 = *Off
     c                   Eval      PAOBSO=%Trim(
     c                                      %EditW(TRICUI:'      -        - ')
     C                                    ) + ' '+
     c                                    %Trim(TRNRSO)
     C                   LeaveSr
     c                   EndIf
     c*
     c                   ExSr      DiaAnterior
     c*
     C     KTR0500       Chain     RESPTRTR                           99
     c                   If        *In99 = *Off
     c                   Eval      PAOBSO=%Trim(
     c                                      %EditW(TRICUI:'      -        - ')
     C                                    ) + ' '+
     c                                    %Trim(TRNRSO)
     C                   LeaveSr
     c                   EndIf
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C     DiaAnterior   BegSr
     c*
     c                   Call      'SBBAINFE'
     c                   Parm                    PAFALT
     c                   Parm      'IN'          PACINV            2
     c*
     C                   Z-ADD     *ZERO         PADIAS           15 0
     C                   CALL      'SBBABFE5'
     C                   PARM                    PAFALT
     C                   PARM                    PADIAS
     C                   PARM                    PAISUC
     c*
     c                   Call      'SBBAINFE'
     c                   Parm                    PAFALT
     c                   Parm      'NI'          PACINV            2
     c*
     C                   Z-Add     PAFALT        TRFECO
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    PAISUB            2
     c                   Parm                    PAISUC            5 0
     c                   Parm                    PAICAH           11 0
     c                   Parm                    PAFALT            8 0
     c                   Parm                    PAHALT            6 0
     c                   Parm                    PA$IMP           15 2
     c                   Parm                    PAICHE            7 0
     c                   Parm                    PAIMCA            3 0
     c                   Parm                    PAOBSO           50
     c*
     c     KMH0100       KList
     c                   KFld                    WKICSC
     c                   KFld                    WKENDE
     c                   KFld                    WKEDSU
     c                   KFld                    WKICCC
     c                   KFld                    WKFALT
     c                   KFld                    WKHALT
     c                   KFld                    WKICHE
     c                   KFld                    WK$MT1
     c*
     c     KTR0500       KList
     c                   KFld                    TRISUB
     c                   KFld                    TRFECO
     c                   KFld                    TREDSU
     c                   KFld                    TRICCC
     c                   KFld                    TRIULN
     C                   KFld                    TR$IMP
     C*
     c*
     C                   Move      PAISUB        TRISUB
     C                   Z-Add     PAFALT        TRFECO
     C                   Z-Add     PAISUC        TREDSU
     C                   Z-Add     PAICAH        TRICCC
     C                   Z-Add     PAICHE        TRIULN
     C                   Z-Add     PA$IMP        TR$IMP
     C*
     c                   Move      'DE'          WKICSC            2
     c                   Z-Add     309           WKENDE           15 0
     c                   Z-Add     PAISUC        WKEDSU            5 0
     c                   Z-Add     PAICAH        WKICCC           11 0
     c                   Z-Add     PAFALT        WKFALT            8 0
     c                   Z-Add     PAHALT        WKHALT            6 0
     c                   Z-Add     PAICHE        WKICHE            7 0
     c                   Z-Add     PA$IMP        WK$MT1           12 2
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
