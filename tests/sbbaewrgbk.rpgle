*SRCMBRTXT:Edita un campo Númerico con Edit Word  
     c                   Select
     c                   When      CvtTyp='M'
     c                   Move      DtaInp        Aux152           15 2
     c                   Eval      DtaOut=%EditW(Aux152:'            0,  ')
     c                   When      CvtTyp='N'
     c                   Move      DtaInp        Aux150           15 0
     c                   Eval      DtaOut=%EditW(Aux150:'              0')
     c                   When      CvtTyp='O'
     c                   Move      DtaInp        Aux80             8 0
     c                   Eval      DtaOut=%EditW(Aux80 :'       0')
     c                   When      CvtTyp='P'
     c                   Move      DtaInp        Aux110           11 0
     c                   Eval      DtaOut=%EditW(Aux110:'          0')
     c                   When      CvtTyp='Q'
     c                   Move      DtaInp        Date8             8 0
     c                   Eval      DtaOut=%EditW(Date8 :'  /  /    ')
     c                   When      CvtTyp='R'
     c                   Move      DtaInp        Date8             8 0
     c                   Eval      DtaOut=%EditW(Date8 :'    /  /  ')
     c                   When      CvtTyp='S'
     c                   Move      DtaInp        Date8             8 0
     c                   Eval      DtaOut=%EditW(Date8 :'  -  -    ')
     c                   When      CvtTyp='T'
     c                   Move      DtaInp        Date8             8 0
     c                   Eval      DtaOut=%EditW(Date8 :'    -  -  ')
     c                   EndSl
     c                   Eval      DtaOut=%Trim(DtaOut)
     c                   Eval      LenOut=%Len(%Trim(DtaOut))
     c*
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* Inicializacion
     c*---------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    DtaInp          500
     c                   Parm                    CvtTyp            1
     c                   Parm                    DtaOut         1000
     c                   Parm                    LenOut           15 0
     c*
     c                   EvalR     DtaInp=%Trim(DtaInp)
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Fin del Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        Lr
     c                   Return
     c*
     c                   EndSr
