*SRCMBRTXT:Valida Sentencia QSH                   
     DMdto             S            255
     DMdtos            S            255    DIM(255)
     DMdtosCount       S             10  0
     DErrorFlag        S               N
     DI                S             10  0
     DJ                S             10  0
     C                   EXSR      BreakStm
     C                   Eval      ErrorFlag=*OFF
     C                   For       I = 1 to MdtosCount
     C                   Eval      Mdto=Mdtos(I)
     C                   Exsr      AnzMdto
     C                   If        ErrorFlag
     C                   Leave
     C                   Endif
     C                   EndFor
     C                   EXSR      ENDPGM
     C*---------------------------------------------------------------------
     C* AnzMdato: Analiza Si el Mandato es válido
     C*---------------------------------------------------------------------
     C     AnzMdto       BEGSR
     C
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* BreakStm: Parte la Linea de Comandos en Sub Comandos
     C*---------------------------------------------------------------------
     C     BreakStm      BEGSR
     C                   Move      1             STRSTR            3 0
     C                   Move      *ZERO         MdtosCount
     C                   For       j = 1 to 255
     C                   If        %Subst(CMDSTR:j:1)=';'
     C                   Exsr      AddCmd
     C                   EndIf
     C                   EndFor
     C                   Exsr      AddCmd
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* AddCmd: Agregar Un Mandato
     C*---------------------------------------------------------------------
     C     AddCmd        BEGSR
     C*
     C                   Add       1             MdtosCount
     C                   Eval      Mdtos(MdtosCount)=
     C                             %SUBST(CMDSTR:STRSTR:J-STRSTR)
     C                   Eval      STRSTR=J+1
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* *INZSR: SUBRUTINA INICIALIZACION
     C*---------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    CMDSTR          255
     C                   PARM                    ErrorFlag         1
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ENDPGM: SUBRUTINA FINALIZACION
     C*---------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
