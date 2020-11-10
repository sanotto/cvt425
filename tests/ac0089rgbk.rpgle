*SRCMBRTXT:LINK:TRANSF. ENTRE REDES-MANEJADOR     
     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
     H DATEDIT(*YMD)
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
     D FilNamD         S             30A
     D FilNamA         S             30A
     D FilNamM         S             30A
     D FullPath        S            255A
     D FullPathD       S            255A
     D FullPathA       S            255A
     D FullPathM       S            255A
     D AllUsed         S               N
     C*-------------------------------------------------------------------------
     C                   ExSr      GetFilNam
     C*
     C                   Move      VerNum        VerChr            1
     C                   Z-Add     PAMARE        MaxRec           15 0
     C                   Call      'AC0089R1'
     C                   Parm                    VerChr
     C                   Parm                    MaxRec
     C*
     C                   Move      VerNum        VerChr            1
     C                   Z-Add     PAMARE        MaxRec           15 0
     C                   Call      'AC0089R2'
     C                   Parm                    VerChr
     C                   Parm                    MaxRec
     C*
     C                   Eval      FullPath='/home/LINKBEE/REFRESH/'+
     C                                      %trim(FilNam)
     C                   Eval      FullPathD='/home/LINKBEE/REFRESH/'+
     C                                      %trim(FilNamD)
     C                   Eval      FullPathA='/home/LINKBEE/REFRESH/'+
     C                                      %trim(FilNamA)
     C                   Eval      FullPathM='/home/LINKBEE/REFRESH/'+
     C                                      %trim(FilNamM)
     C                   Eval      FILNME=%TRIM(FilNam)
     C                   Eval      FILNMD=%TRIM(FilNamD)
     C                   Eval      FILNMA=%TRIM(FilNamA)
     C                   Eval      FILNMM=%TRIM(FilNamM)
     C*
     C                   Call      'AC0089R3'
     C                   Parm                    FullPath
     C                   Parm                    FullPathD
     C                   Parm                    FullPathA
     C                   Parm                    FullPathM
     C                   Parm                    VerChr
     C                   Parm                    CanReg           15 0
     C                   Parm                    CanRegD          15 0
     C                   Parm                    CanRegA          15 0
     C                   Parm                    CanRegM          15 0
     C*
     C                   ExSr      RegArchivo
     C*
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *ENTRY        PList
     C                   Parm                    PAMARE           15 0
     C                   Parm                    FILNME          255
     C                   Parm                    FILNMD          255
     C                   Parm                    FILNMA          255
     C                   Parm                    FILNMM          255
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetFilNam     BegSr
     C*
     C                   Eval      FilNam='PAUC309'
     c*
     C                   Move      *ON           AllUsed
     C                   Move      *DATE         Today             8 0
     C                   Move      Today         AAMMDD            6
     C                   For       VerNum= 1 to 7 by 3
     C                   Move      VerNum        VerNux            1 0
     C     VerNux        Add       1             VerNux2           1 0
     C     VerNux        Add       2             VerNux3           1 0
     C                   Eval      FilNamD='CBU0309'+ %editc(VerNux:'Z')+
     C                                    AAMMDD
     C                   Eval      FilNamA='CBU0309'+ %editc(VerNux2 :'Z')+
     C                                    AAMMDD
     C                   Eval      FilNamM='CBU0309'+ %editc(VerNux3 :'Z')+
     C                                    AAMMDD
     C     FilNamA       Chain     RELISERJ                           99
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
     C                   Z-Add     *DATE         RJFALT
     C                   Time                    RJHORA
     C                   Move      @PUSER        RJIUSR
     C                   Write     RELISERJ
     C*
     C                   Move      FilNamD       RJDACO
     C                   Z-Add     CanReg        RJCAN1
     C                   Z-Add     *DATE         RJFALT
     C                   Time                    RJHORA
     C                   Move      @PUSER        RJIUSR
     C                   Write     RELISERJ
     C                   Move      FilNamA       RJDACO
     C                   Z-Add     CanReg        RJCAN1
     C                   Z-Add     *DATE         RJFALT
     C                   Time                    RJHORA
     C                   Move      @PUSER        RJIUSR
     C                   Write     RELISERJ
     C                   Move      FilNamM       RJDACO
     C                   Z-Add     CanReg        RJCAN1
     C                   Z-Add     *DATE         RJFALT
     C                   Time                    RJHORA
     C                   Move      @PUSER        RJIUSR
     C                   Write     RELISERJ
     C                   EndSr
     C*-------------------------------------------------------------------------
