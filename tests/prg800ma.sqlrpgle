*SRCMBRTXT:Consulta de Movs. de Prestamos         
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     F*----------------------------------------------------------------*
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     FPRTMOD50  IF   E           K DISK
     FBASCTM    UF A E           K DISK
     F@CPISYS   UF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     D*----------------------------------------------------------------*
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     D QryStr          S           2048
     D Where           S           2048
     D*----------------------------------------------------------------*
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
     D*----------------------------------------------------------------*
     C                   CALL      'PRG800RS'
     C     @PJOBN        CHAIN(N)  @CPIUSRR                           99
     C     @PJOBN        CHAIN(N)  @CPISYS                            99
     C     @FN(03)       DOWEQ     *OFF
     C     @FN(12)       ANDEQ     *OFF
     C                   EXSR      BLDQRY
     C                   EXSR      FILTMP
     C                   CALL      'PRG800RS'
     C     @PJOBN        CHAIN(N)  @CPIUSRR                           99
     C     @PJOBN        CHAIN(N)  @CPISYS                            99
     C                   ENDDO
     C                   SETON                                        LR
     C*-------------------------------------------------------------------------
     C* FILTMP : RELLENAR TEMPORAL
     C*-------------------------------------------------------------------------
     C     FILTMP        BEGSR
     C*
     C                   EXSR      DLTTMP
     C*
     C                   Z-ADD     *ZERO         TOTAL             8 2
     C                   Z-ADD     *ZERO         CUENTA            8 0
     C*
     C/EXEC SQL
     C+ PREPARE S1 FROM :QRYSTR
     C/END-EXEC
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR S1
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :G8ISUC, :G8INCR, :G8IDEG, :G8ILCR,
     C+ :G8ICUO, :G8IMPR, :G8$IMP, :RRN
     C/END-EXEC
     C                   DOW       SQLCOD=0
     C                   EXSR      ADDLINE
     C                   IF        Cuenta> 9997
     C                   LEAVE
     C                   ENDIF
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :G8ISUC, :G8INCR, :G8IDEG, :G8ILCR,
     C+ :G8ICUO, :G8IMPR, :G8$IMP, :RRN
     C/END-EXEC
     C                   ENDDO
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C*
     C                   IF        Cuenta>  9997
     C                   EVAL      ERRTXT='Hay mas de  9.998 registros en '+
     c                             'el conjunto de resultados, se muestran '+
     c                             'y se suman solo los primeros  9.998.Ing'+
     c                             'rese mas parámetros para acotar la bus'+
     c                             'queda.'
     c                   Exsr      DspErr
     C                   ENDIF
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Mov. de Prestamos-Consulta'
     C                   EVAL      WWTIT1='Filtro:'+%Subst(where:  4: 63)
     C                   EVAL      WWTIT2=%Subst(where: 67: 70)
     C                   EVAL      WWTIT3=%Subst(where:137: 70)
     C                   EVAL      WWTIT4='Cuenta:'+%EDITC(Cuenta:'Z')+
     C                             ' Total:'+%EDITW(Total:'            0,  ')
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5='Suc.     Credito Desg. Lin '+
     C                                    'Cta  Mov           Importe'
     C                   MOVEL(P)  @PJOBN        @CIJOB
     C                   UPDATE    @CPIUSRR
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     C                   DOW       @FN(03) =*OFF AND
     C                             @FN(12) = *OFF
     C     @ZRRNO        CHAIN     RTMP                               99
     C     @PJOBN        CHAIN     @CPISYS                            80
     C                   Z-ADD     S1INCR        @ZRRNO
     C                   UPDATE    @CPISYSR
     C                   CALL      'PRG800RE'
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     C                   ENDDO
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ADDLINE: AGREGAR LINEA
     C*-------------------------------------------------------------------------
     C     ADDLINE       BEGSR
     C*
     C                   Z-ADD     G8INCR        INCR              8 0
     C                   Z-ADD     G8IDEG        IDEG              2 0
     C*
     C                   EVAl      s1dacl=%EDITC(G8ISUC:'J')+' '+
     C                                    %EDITW(  INCR:'       0')+' '+
     C                                    %EDITC(  IDEG:'J')+' '+
     C                                    %EDITC(G8ILCR:'Z')+' '+
     C                                    %EDITC(G8ICUO:'J')+' '+
     C                                    %EDITC(G8IMPR:'J')+' '+
     C                                    %EDITW(G8$IMP:'            0,  ')
     C                   EVAL      S1INCR=RRN
     C                   WRITE     REBASCTM
     C                   ADD       1             CUENTA
     C                   ADD       G8$IMP        TOTAL
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* BLDQRY : BUILD QUERY
     C*-------------------------------------------------------------------------
     C     BLDQRY        BEGSR
     C*
     C                   EVAL      where=''
     C*
     C                   Eval      QryStr='SELECT G8ISUC, G8INCR, G8IDEG, '+
     C                                    'G8ILCR, G8ICUO, G8IMPR, G8$IMP '+
     C                                    ', RRN(PRTMOD50)                '+
     C                                    'FROM PRTMOD50 WHERE G8Impr<>8     '
     C* Fecha Asto Desde....
     C                   IF        @CFDES <>  0
     C                   MOVE      @CFDES        CHFDES            8
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8FSPR>='+CHFDES
     C                   ENDIF
     C* Fecha Asto HASTA....
     C                   IF        @CFHAS <>  0
     C                   MOVE      @CFHAS        CHFHAS            8
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8FSPR<='+CHFHAS
     C                   ENDIF
     C* Número de sucursal .
     C                   IF        @CISUC <> 99999
     C                   MOVE      @CISUC        CHISUC            5
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ISUC='+CHISUC
     C                   ENDIF
     C* Nro.prést.o doc.des.
     C                   IF        @CINCR <>  0
     C                   MOVE      @CINCR        CHINCR           15
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8INCR='+CHINCR
     C                   ENDIF
     C* Número de desglose .
     C                   IF        @CIDEG <>  0
     C                   MOVE      @CIDEG        CHIDEG            4
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8IDEG='+CHIDEG
     C                   ENDIF
     C* Número de cuota.....
     C                   IF        @CICUO <>  0
     C                   MOVE      @CICUO        CHICUO            3
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ICUO='+CHICUO
     C                   ENDIF
     C* Momento de Concrec..
     C                   IF        @CIMOC <>  ' '
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8IMOC ='''+@CIMOC+''''
     C                   ENDIF
     C* Cód Mov
     C                   IF        @CIMPR <>  0
     C                   MOVE      @CIMPR        CHIMPR            3
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8IMPR='+CHIMPR
     C                   ENDIF
     C* Situación ..........
     C                   IF        @CISIT <>  0
     C                   MOVE      @CISIT        CHISIT            2
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ISIT ='+CHISIT
     C                   ENDIF
     C* Asiento?
     C                   IF        @CIBIS <>  ' '
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8IGAS ='''+@CIBIS+''''
     C                   ENDIF
     C* Sucursal de Alta....
     C                   IF        @CISAL <>  99999
     C                   MOVE      @CISAL        CHISAL            5
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ISAL ='+CHISAL
     C                   ENDIF
     C* Sector de Cuenta....
     C                   IF        @CISCT <>  0
     C                   MOVE      @CISCT        CHISCT            4
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ISCT ='+CHISCT
     C                   ENDIF
     C* Linea
     C                   IF        @CILCR <>  0
     C                   MOVE      @CILCR        CHILCR            4
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ILCR ='+CHILCR
     C                   ENDIF
     C* Moneda
     C                   IF        @CIMON <>  0
     C                   MOVE      @CIMON        CHIMON            9
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8IMON ='+CHIMON
     C                   ENDIF
     C* Tipo de entrega cobro
     C                   IF        @CITEC <>  0
     C                   MOVE      @CITEC        CHITEC            2
     C                   Eval      where =%TRIM(where )+
     C                             ' AND G8ITEC ='+CHITEC
     C                   ENDIF
     c                   Eval      qrystr=%trim(qrystr)+' '+where
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DLTTMP        BEGSR
     C*
     C     KEY002        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBASCTM
     C     KEY002        CHAIN     REBASCTM                           99
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     KEY002        KLIST
     C                   KFLD                    S1IJOB
     C*
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      *ZERO         RRN              15 0
     C*
     C*
     C                   EVAL      CMDLNE='OVRDBF FILE(BASCTMRRN)'  +
     C                             ' TOFILE(*LIBL/BASCTM)'
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDLNE         1024
     C                   PARM      1024          CMDSZE           15 5
     C                   OPEN      BASCTMRRN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   CALL      'BAER00RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C                   ENDSR
