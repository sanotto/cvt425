*SRCMBRTXT:Switch-Adapter      -PF-380000-Precance
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD50RG                                       *
     H*                                                               *
     H*  PROGRAM NO:   PRECAN. DE PLAZO FIJO UVA                      *
     H*                                                               *
     H*  DATE: 21/02/2029                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     H*---------------------------------------------------------------*
     FPFCERT    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     C*
     C*
     C                   If        PATIPF<>'F'
     C                   Eval      PAIERR='057No soportado'
     C                   ExSr      EndPgm
     C                   else
     C                   ExSr      BuscaCert
     C                   ExSr      ValidaDiaTr
     C                   ExSr      PrecaPF
     C                   EndIf
     C*
     C*--------------------------------------------------------------------
     C* BuscaCert: Busca certificado
     C*--------------------------------------------------------------------
     C     BuscaCert     BegSr
     C*
     C     KIZ0000       Chain     PFCERT                             98
     C   98              Eval      PAIERR='305Certificado No enc.   '
     C   99              ExSr      EndPgm
     C*
     C                   EndSr
     C*--------------------------------------------------------------------
     C* ValidaDiaTr: Valida dias transcurrido 30 dias
     C*--------------------------------------------------------------------
     C     ValidaDiaTr   BegSr
     C                   z-add     AASFEN        PAFALT            8 0
     C                   movel     *blanks       PADIAS           15 0
     C                   movel     *blanks       PADSEM            2
     C                   movel     *blanks       PADIRR            1
     C*
     C                   Call      'SBBAFECH'
     C                   parm                    PAFALT
     C                   parm                    PADIAS
     C                   parm                    PADSEM
     C                   parm                    PADIRR
     C*
     C                   z-add     PADIAS        AADIAS           15 0
     C                   z-add     IZFASI        PAFALT
     C                   movel     *blanks       PADIAS
     C                   movel     *blanks       PADSEM
     C                   movel     *blanks       PADIRR
     C*
     C                   move      'IN'          PACINV            2
     C                   Call      'SBBAINFE'
     C                   parm                    PAFALT
     C                   parm                    PACINV
     C*
     C                   Call      'SBBAFECH'
     C                   parm                    PAFALT
     C                   parm                    PADIAS
     C                   parm                    PADSEM
     C                   parm                    PADIRR
     C
     C     AADIAS        Sub       PADIAS        DIADIF
     C*
     C                   If        DIADIF < 30
     C                   Eval      PAIERR='308No PRECAN   '
     C                   ExSr      EndPgm
     C                   EndIf
     C                   EndSr
     C*--------------------------------------------------------------------
     C* PrecaPF   : Llama a pgm precancelador
     C*--------------------------------------------------------------------
     C     PrecaPF       BegSr
     C*
     C                   Call      'PFIZ80RG'
     C                   parm                    PADBSU
     C                   parm                    PANCER
     C                   parm      'P'           paacci            1
     C                   parm                    paerro           50
     C*
     c                   If        paerro = *blanks
     c                   Eval      paerro='000OK'
     c                   Else
     C                   Eval      PAIERR='057No soportado'
     c                   EndIf
     C*
     C                   MoveL     paerro        PAIERR
     C*
     C*
     C                   ExSr      EndPgm
     C                   EndSr
     C*--------------------------------------------------------------------
     C* *InzSr: Inicialización del Programa
     C*--------------------------------------------------------------------
     C     *InzSr        BegSr
     C*
     C     *Entry        PList
     C                   Parm                    PATIPF            1
     C                   Parm                    PADBSU            5 0
     C                   Parm                    PADBCT           11 0
     C                   Parm                    PANCER           10 0
     C                   Parm                    PAIERR           40
     c*
     C     1             CHAIN     RESGSYSV                           99
     C                   z-add     *zeros        DIADIF           15 0
     C                   z-add     PANCER        paince           11 0
     c*
     C     KIZ0000       KList
     C                   KFld                    padbsu
     C                   KFld                    paince
     C*
     C                   EndSr
     C*--------------------------------------------------------------------
     C* EndPgm: Fin de Programa
     C*--------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
