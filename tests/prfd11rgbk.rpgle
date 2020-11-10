*SRCMBRTXT:FEDERAL: Generación de Minutas p/PC p/T
     FBASCT1    UF A E           K DISK
     FBASCTM    UF A E           K DISK
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     c                   ExSr      DelBASCT1
|    C                   EVAL      S1IJOB=@PJOBN
|    C                   EVAL      S2IJOB=@PJOBN
|    C                   EVAL      S2ISEQ=1
     C                   ExSr      Ap002Trn339
|    C                   EVAL      S2ISEQ=S2ISEQ+1
     C                   ExSr      Ap001Trn439
     c                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     c* Ap002Trn339: Insertar Ap 2-339
     C*-------------------------------------------------------------------------
     C     Ap002Trn339   BegSr
     C*
|    C                   EVAL      S2DACL=' 2 339'+
|    C                             ' SUC:'+
     C                             %subst(%EDITW(PAISUC:'     0 '): 4 : 4)+
|    C                             ' CTA:'+%EDITW(PAICAH:'           0 ')
     C                   EVAL      %SUBST(S2DACL:56:16)=
|    C                             ' IMP:'+%SUBST(
|    C                                         %EDITW(PA$IMP:'            0,  ')
|    C                                   : 6 :11)
     c*
     C                   EVAL      S2ILCR=2
     C                   EVAL      S2IMON=339
     C                   EVAL      S2$A03=PAICAH
     C                   EVAL      S2$INP=PA$IMP
     C                   EVAL      S2$A04=0
     C                   EVAL      S2ISUC=PAISUC
     C                   EVAL      S2INCR=PAINCR
     C                   EVAL      S2IDEG=PAIDEG
     C                   EVAL      S2QCUO=0
     C                   EVAL      S2DF03=*Blanks
     C*
|    C                   WRITE     REBASCT1
|    C                   ExSr      WrtBASCTM
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* Ap001Trn439: Insertar Ap 1-439
     C*-------------------------------------------------------------------------
     c     Ap001Trn439   BegSr
     C*
     C                   Z-Add     *Zero         WWISUC            5 0
     C                   Z-Add     31006065      WWICCC           11 0
     C*
|    C                   EVAL      S2DACL=' 1 439'+
|    C                             ' SUC:'+
     C                             %subst(%EDITW(WWISUC:'     0 '): 4 : 4)+
|    C                             ' CTA:'+%EDITW(WWICCC:'           0 ')
     C                   EVAL      %SUBST(S2DACL:56:16)=
|    C                             ' IMP:'+%SUBST(
|    C                                         %EDITW(PA$IMP:'            0,  ')
|    C                                   : 6 :11)
     c*
     C                   EVAL      S2ILCR=1
     C                   EVAL      S2IMON=439
     C                   EVAL      S2$A03=WWICCC
     C                   EVAL      S2$INP=PA$IMP
     C                   EVAL      S2$A04=0
     C                   EVAL      S2ISUC=WWISUC
     C                   EVAL      S2INCR=PAINCR
     C                   EVAL      S2IDEG=PAIDEG
     C                   EVAL      S2QCUO=0
     C                   EVAL      S2DF03=*Blanks
     C*
|    C                   WRITE     REBASCT1
|    C                   ExSr      WrtBASCTM
     c*
     C                   EndSr
     C*
     C*-------------------------------------------------------------------------
     c* WrtBASCTM : Escribir BASCTM para mostrar el TE
     C*-------------------------------------------------------------------------
     c     WrtBASCTM     BegSr
     C*
|    C                   EVAL      S1DACL=S2DACL
     C                   EVAL      S1ILCR=S2ILCR
     C                   EVAL      S1IMON=S2IMON
     C                   EVAL      S1$A03=S2$A03
     C                   EVAL      S1$INP=S2$INP
     C                   EVAL      S1$A04=S2$A04
     C                   EVAL      S1ISUC=S2ISUC
     C                   EVAL      S1INCR=S2INCR
     C                   EVAL      S1IDEG=S2IDEG
     C                   EVAL      S1QCUO=S2QCUO
     C                   EVAL      S1DF03=S2DF03
     C*
|    C                   WRITE     REBASCTM
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* DelBASCT1: Elimina movimientos del BASCTM1
     C*-------------------------------------------------------------------------
     C     DelBASCT1     BegSr
     C*
     C     @PJOBN        Chain     REBASCT1                           99
     C                   DoW       *IN99 = *Off
     c                   Delete    REBASCT1
     C     @PJOBN        ReadE     REBASCT1                               99
     C                   EndDo
     C*
     C     @PJOBN        Chain     REBASCTM                           99
     C                   DoW       *IN99 = *Off
     c                   Delete    REBASCTM
     C     @PJOBN        ReadE     REBASCTM                               99
     C                   EndDo
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR : Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     c     *Entry        PList
     c                   Parm                    PAISUC            5 0
     c                   Parm                    PAINCR           15 0
     c                   Parm                    PAIDEG            4 0
     c                   Parm                    PAICAH           11 0
     c                   Parm                    PA$IMP           15 2
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* EndPgm : Fin del Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
