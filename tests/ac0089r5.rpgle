*SRCMBRTXT:LINK:TRANSF. ENTRE REDES-MANEJADOR     
     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
     H DATEDIT(*YMD)
     FSGSYSV    IF   E             DISK
     FLISERJ    UF A E           K DISK
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
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
     D*-------------------------------------------------------------------------
     D VerNum          S              2P 0
     D FilNam          S             30A
     D FullPath        S            255A
     D AllUsed         S               N
     C*-------------------------------------------------------------------------
     C                   ExSr      GetFilNam
     C*
     C                   Move      VerNum        VerChr            1
     C                   Z-Add     PAMARE        MaxRec           15 0
     C*
     C                   Move      VerNum        VerChr            1
     C                   Z-Add     PAMARE        MaxRec           15 0
     C*
     C                   Eval      FullPath='/home/LINKBEE/REFRESH/'+
     C                                      %trim(FilNam)
     C                   Eval      FILNME=%TRIM(FilNam)
     C*
     C                   Call      'AC0089R6'
     C                   Parm                    FullPath
     C                   Parm                    VerChr
     C                   Parm                    CanReg           15 0
     C*
     C                   ExSr      RegArchivo
     C*
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     1             CHAIN     SGSYSV                             80
     C     *ENTRY        PList
     C                   Parm                    PAMARE           15 0
     C                   Parm                    FILNME          255
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetFilNam     BegSr
     C*
     C                   Move      *ON           AllUsed
     C                   Move      AASFEI        Today             8 0
     C                   Move      Today         AAMMDD            6
     C                   For       VerNum= 5 to 9
     C                   Move      VerNum        VerNux            1 0
     C                   Eval      FilNam='CBU0309'+ %editc(VerNux:'Z')+
     C                                    AAMMDD
     C     FilNam        Chain     RELISERJ                           99
     C                   If        *IN99 = *ON
     C                   Move      *OFF          AllUsed           1
     C                   Leave
     C                   EndIf
     C                   EndFor
     C*
     c                   If        AllUsed
     C                   Eval      ErrTxt='Solo se permiten 9 envíos '+
     C                                    'por día.'
     C                   ExSr      DspErr
     C                   ExSr      EndPgm
     C                   EndIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     DspErr        BegSr
     C                   Call      'BAER00RS'
     C                   Parm                    WWNCU1
     C                   Parm                    WWNCU2
     C                   Parm                    WWNCU3
     C                   Parm                    WWNCU4
     C                   Parm                    WWNCB1
     C                   Parm                    WWNCB2
     C                   Parm                    WWNCB3
     C                   Parm                    WWNCB4
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     RegArchivo    BegSr
     C*
     C                   Move      FilNam        RJDACO
     C                   Z-Add     CanReg        RJCAN1
     C                   Z-Add     AASFEI        RJFALT
     C                   Time                    RJHORA
     C                   Move      @PUSER        RJIUSR
     C                   Write     RELISERJ
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------