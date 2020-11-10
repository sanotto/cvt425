*SRCMBRTXT:AASC00MA: Gen. Arch. con deudas para TE
     F*
     FPRCUOT02  UF   E           K DISK
     FCOTIZA    IF   E           K DISK
     FTCRESU05  IF   E           K DISK
     FTCRESD01  IF   E           K DISK
     FTCPAGO01  IF   E           K DISK
     FTIUSUA05  IF   E           K DISK
     FPRREMT    IF   E           K DISK
     FBASCTM    UF A E           K DISK
     FBASCT1    UF A E           K DISK
     FBASCT2    UF A E           K DISK
     FBASCCL    UF A E           K DISK
     FBADCCL06  IF   E           K DISK
     FBAICCL06  IF   E           K DISK
     FACCTAC07  IF   E           K DISK
     FACCTAC    IF   E           K DISK    RENAME(REACCTAC: CAJAHO)
     FACMOVD11  IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     FSGSYSV    IF   E             DISK
     FPRMOVI01  UF   E           K DISK
     F*
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D @PJOBN                264    269S 0
     I*----------------------------------------------------------------*
     D                 DS
     D DSBLK1                  1    200
     D DSSMIN                120    130  0
     I*--------------------------------------------------------------
     D                 DS
     D DSBLK2                  1    200
     D DSSALV                179    189  0
     D*-------------------------------------------------------------------------
     D* Estructura de datos para guardar operación anterior
     D                 DS
     D  DSOPAN                 1     15  0
     D  DSSUCA                 1      3  0
     D  DSINCA                 4     15  0
     D*-------------------------------------------------------------------------
     C                   EXSR      Inicio
     C                   EXSR      DeclCursores
     C*                  EXSR      VerDeudaTime
     C*                  EXSR      VerDeudaSol
     C                   EXSR      VerDeudaTC
     C                   EXSR      VerDeudaPres
     C                   EXSR      VerDeudaMut
     C     @PJOBN        CHAIN     @CPIUSD
     C                   EVAL      @C$MON=TC$TOT+PR$TOT+TS$TOT+TM$TOT
     C                   UPDATE    @CPIUSRR
+----C                   IF        @C$MON > 0
|    C                   EVAL      S1ISEQ=-10
|    C                   EVAL      S1DACL='                            '+
|    C                                    '            Total Cobrado       '+
|    C                             %SUBST(
|    c                             %EDITW(@C$MON:'             ,  ')
|    C                             :6 :11)
|    C                   EXSR      AgrLinea
|    C                   EVAL      S1DACL=*ALL'-'
|    C                   EXSR      AgrLinea
+----C                   ENDIF
     C                   EXSR      EndPgm
     C*-------------------------------------------------------------------------
     C* EndPgm: Finalización de Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BEGSR
     C* ... Salvar Cuenta Cliente
     C     @PJOBN        CHAIN     @CPIUSRR                           99
     C                   MOVE      WKISUC        @CISUC
     C                   MOVE      WKICCL        @CICCL
     C                   UPDATE    @CPIUSRR
     C*
     C                   EVAL      *INLR=*OFF
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* Inicio: Subrutina de Inicialización
     C*-------------------------------------------------------------------------
     C     Inicio        BEGSR
     C                   SETOFF                                       909192
     C*   ... Borra registros anteriores si los hay
     C     @PJOBN        CHAIN     BASCTM                             99
+----C                   DOW       *IN99=*OFF
|    C                   DELETE    REBASCTM
|    C     @PJOBN        READE     BASCTM                                 99
+----C                   ENDDO
     C     @PJOBN        CHAIN     BASCT1                             99
+----C                   DOW       *IN99=*OFF
|    C                   DELETE    REBASCT1
|    C     @PJOBN        READE     BASCT1                                 99
+----C                   ENDDO
     C     @PJOBN        CHAIN     BASCT2                             99
+----C                   DOW       *IN99=*OFF
|    C                   DELETE    REBASCT2
|    C     @PJOBN        READE     BASCT2                                 99
+----C                   ENDDO
     C     @PJOBN        CHAIN     BASCCL                             99
+----C                   DOW       *IN99=*OFF
|    C                   DELETE    BASCCL
|    C     @PJOBN        READE     BASCCL                                 99
+----C                   ENDDO
     C* ... KLISTS
     C     @KEY03        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWICAH
     C                   KFLD                    WWIMCA
     C     KBADCCL06     KLIST
     C                   KFLD                    @CINDO
     C                   KFLD                    WWITTL
     C     KACCTAC       KLIST
     C                   KFLD                    OTISUC
     C                   KFLD                    OTICCL
     C     KCUENTA       KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWICAH
     C     KPRCUOT02     KLIST
     C                   KFLD                    C1SUCA
     C                   KFLD                    C1INCR
     C                   KFLD                    C1IDEG
     C     KPRCUOT03     KLIST
     C                   KFLD                    C1SUCA
     C                   KFLD                    C1INCR
     C                   KFLD                    C1IDEG
     C                   KFLD                    C1ICUO
     C* Acceso a TCRESU05
     C     WKEY28        KLIST
     C                   KFLD                    WXFAAM
     C                   KFLD                    @CINDO
     C* Acceso a TCPAGO01
     C     WKEY29        KLIST
     C                   KFLD                    WYFAAM
     C                   KFLD                    TMISUC
     C                   KFLD                    TMIGRC
     C                   KFLD                    TMINCT
     C*   Acceso a COTIZA
     C     WKEY30        KLIST
     C                   KFLD                    WXISUC            5 0
     C                   KFLD                    WWIMON            9 0
     C                   KFLD                    WWFECH            8 0
     C*   Acceso a PRREMT
     C     KRT000        KLIST
     C                   KFLD                    @PJOBN
     C*   Acceso a BAICCL06
     C     KOS006        KLIST
     C                   KFLD                    RTSUCA
     C                   KFLD                    RTICCL
     C* Acceso a TIUSUA05
     C     WKEY03        KLIST
     C                   KFLD                    @CINDO
     C* ... Variables de Trabajo
     C     *LIKE         DEFINE    @CIMCA        WWIMCA
     C     *LIKE         DEFINE    @CISUC        WKISUC
     C     *LIKE         DEFINE    @CICCL        WKICCL
     C     *LIKE         DEFINE    @CITTL        WWITTL
     C     *LIKE         DEFINE    @CSUCA        RFSUCA
     C     *LIKE         DEFINE    @CINCR        RFINCR
     C     *LIKE         DEFINE    @CIDEG        RFIDEG
     C     *LIKE         DEFINE    @CSUCA        C1SUCA
     C     *LIKE         DEFINE    @CINCR        C1INCR
     C     *LIKE         DEFINE    @CIDEG        C1IDEG
     C     *LIKE         DEFINE    @CICUO        C1ICUO
     C     *LIKE         DEFINE    TMFAAM        WXFAAM
     C     *LIKE         DEFINE    TMFAAM        WYFAAM
     C     *LIKE         DEFINE    FUISUC        WWISUC
     C     *LIKE         DEFINE    FUICAH        WWICAH
     C* ... Para PRMFED
     C     *LIKE         DEFINE    S2ILCR        WRIAPC
     C     *LIKE         DEFINE    S2IMON        WRITRN
     C     *LIKE         DEFINE    S2$A03        WRICCL
     C     *LIKE         DEFINE    S2$INP        WR$IMP
     C     *LIKE         DEFINE    S2$A04        WRFAAM
     C     *LIKE         DEFINE    S2ISUC        WRISUC
     C     *LIKE         DEFINE    S2INCR        WRINCR
     C     *LIKE         DEFINE    S2IDEG        WRIDEG
     C     *LIKE         DEFINE    S2QCUO        WRICUO
     C     *LIKE         DEFINE    S2DF03        WRDAV1
     C*
     C                   MOVE      *BLANKS       C1ISUB           20
     C                   MOVE      1             WWITTL
     C* ... Recuperar Parámetros
     C     1             CHAIN     SGSYSV
     C     @PJOBN        CHAIN(N)  @CPIUSRR
     C* ... Salvar Parámetros
     C                   MOVE      @CSUCA        RFSUCA
     C                   MOVE      @CINCR        RFINCR
     C                   MOVE      @CIDEG        RFIDEG
     C* ... Salvar Cuenta Cliente
     C                   MOVE      @CISUC        WKISUC
     C                   MOVE      @CICCL        WKICCL
     C*
     C                   Z-ADD     *ZERO         TC$TOT           15 2
     C                   Z-ADD     *ZERO         TS$TOT           15 2
     C     @PJOBN        CHAIN     @CPIUSRR
     C                   Z-ADD     *ZERO         @C$MON           15 2
     C                   UPDATE    @CPIUSRR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VerDeudaTime: Verificar Deuda con Time
     C*-------------------------------------------------------------------------
     C     VerDeudaTime  BEGSR
      *
     C                   Z-ADD     @CIEMP        S1IEMP
     C                   MOVEL     *BLANK        PAFLAG            1
     C                   MOVE      *OFF          *IN51
     C*                    MOVE *ON       *IN52
      *
     C     WKEY03        CHAIN     TIUSUA05                           87
+----C     *IN87         DOWEQ     *OFF
|+---C     QRFBAJ        IFEQ      *ZEROS
||   C     QR$LCR        ANDGT     *ZEROS
||   C     QRIETI        ANDEQ     'X'
||   C     QRFBAJ        OREQ      *ZEROS
||   C     QR$LCR        ANDGT     *ZEROS
||   C     QRIETI        ANDEQ     ' '
||   C     @PJOBN        CHAIN     @CPIUSD                            80
||   C                   MOVEL     'MAL'         @CIFUN
||   C                   UPDATE    @CPIUSRR
||    *                                                            ||
||   C* TITULO PARA TIME
||   C* DETALLE PARA TIME
||   C  N92              EVAL      S1DACL='-----------------------------------'+
||   C                                    'Nro.usr.tarjeta  Grupo            N'+
||   C                                    'ro tarjeta   Estado tarjeta   Lim. '+
||   C                                    'cdto. mon. dos'
||   C  N92              EXSR      AgrLinea
||   C  N92              SETON                                        92
||   C                   EVAL      S1DACL=%EDITW(QRIUTI: '      0')+' '+
||   C                                          QRIGRC +' '+
||   C                                   %EDITW(QRITAR:'                0')+' '+
||   C                                          QRIETI +' '+
||   C                                   %EDITW(QR$LCR: '            0,  ')
||   C                   EXSR      AgrLinea
||   C                   LEAVE
|+---C                   ENDIF
|    C     WKEY03        READE     TIUSUA05                               87
+----C                   ENDDO
      *
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VerDeudaMut : Verificar Deuda Mutuales
     C*-------------------------------------------------------------------------
     C     VerDeudaMut   BEGSR
     C                   Z-ADD     *ZERO         TM$TOT           15 2
     C     @PJOBN        CHAIN(N)  @CPIUSRR                           99
+----C     @CINCE        IFNE      *ZERO
|    C     KRT000        CHAIN     REPRREMT                           99
|+---C                   DOW       *IN99 = *OFF
||+--C                   IF        RT$IMP > 0
|||  C                   EVAL      TM$TOT=TM$TOT+RT$IMP
||+--C                   ENDIF
||   C     KRT000        READE     REPRREMT                               99
|+---C                   ENDDO
|    C     KACCTAC       CHAIN     REBAICCL                           99
|    C                   EVAL      S1DACL='02 339'+
|    C                             ' SUC:'+%EDITW(WKISUC:'     0 ')+
|    C                             ' CAH:'+%EDITW(WKICCL:'           ')
|    C                   EVAL      %SUBST(S1DACL:56:16)=
|    C                             ' IMP:'+
|    C                             %SUBST(
|    C                                   %EDITW(TM$TOT: '            0,  ')
|    C                             : 6: 11)
|    c*
|    c                   Eval      WRIAPC=2
|    c                   Eval      WRITRN=339
|    c                   Eval      WRISUC=WKISUC
|    c                   Eval      WRICCL=WKICCL
|    c                   Eval      WR$IMP=TM$TOT
|    c*
|    C                   EXSR      AgrLinea
|    C*
|    C                   Z-ADD     *ZERO         TM$TOT           15 2
|    C     KRT000        CHAIN     REPRREMT                           99
|+---C                   DOW       *IN99 = *OFF
||+--C                   IF        RT$IMP > 0
|||  C     KOS006        CHAIN     REBAICCL                           99
|||  C                   Z-ADD     RTSUCA        WWISUC
|||  C                   Z-ADD     RTICCL        WWICAH
|||  C     KCUENTA       CHAIN     CAJAHO                             99
|||  C  N99              MOVE      '02'          TIPCTA            2
|||  C   99              MOVE      '01'          TIPCTA
|||  C                   EVAL      S1DACL= TIPCTA  +' 439'+
|||  C                             ' SUC:'+ %EDITW(RTSUCA:'     0 ')+
|||  C                             ' CTA:'+ %EDITW(RTICCL:'           ') +
|||  C                             ' NOM:'+ %SUBST(OSNCCL : 1  : 16)
|||  C                   EVAL      %SUBST(S1DACL:56:16)=
|||  C                             ' IMP:'   +
|||  C                             %SUBST(
|||  C                                   %EDITW(RT$IMP: '            0,  ')
|||  C                             : 6: 11)
|||  C                   EVAL      TM$TOT=TM$TOT+RT$IMP
|||  c*
|||  C                   Move      TIPCTA        WRIAPC
|||  c                   Eval      WRITRN=439
|||  c                   Eval      WRISUC=RTSUCA
|||  c                   Eval      WRICCL=RTICCL
|||  c                   Eval      WRDAV1=OSNCCL
|||  c*
|||  C                   EXSR      AgrLinea
||+--C                   ENDIF
||   C     KRT000        READE     REPRREMT                               99
|+---C                   ENDDO
+----C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VerDeudaSol : Verificar Deuda Tarjeta SOL
     C*-------------------------------------------------------------------------
     C     VerDeudaSOL   BEGSR
     C*
+----C                   IF        @CINCE = 0
|    C* ... Deuda de Creditos Normales (Faltante del Minimo)
|    C     KBADCCL06     CHAIN     REBADCCL                           99
|+---C                   DOW       *IN99 = *OFF
||   C     KACCTAC       CHAIN     REACCTAC                           99
||+--C                   IF        *IN99 = *OFF AND FUFBAJ=0 AND FUIBAC=0
|||  C                   EXSR      SolDifMin
||+--C                   ENDIF
||   C     KBADCCL06     READE     REBADCCL                               9999
|+---C                   ENDDO
+----C                   ELSE
|    C* ... Deuda de Creditos Mutuales (Todo el Saldo)
|    C     KBADCCL06     CHAIN     REBADCCL                           99
|+---C                   DOW       *IN99 = *OFF
||   C     KACCTAC       CHAIN     REACCTAC                           99
||+--C                   IF        *IN99 = *OFF AND FUFBAJ=0 AND FUIBAC=0
|||  C                   EXSR      SolDeuTot
|||  C                   LEAVE
||+--C                   ENDIF
||   C     KBADCCL06     READE     REBADCCL                               9999
|+---C                   ENDDO
|    C*
+----C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* SolDifMin   : Obtiene diferencia con el pago minimo de sol
     C*-------------------------------------------------------------------------
     C     SolDifMin     BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD
     C                   Z-ADD     FUISUC        @CISUC
     C                   Z-ADD     FUICAH        @CICAH
     C                   UPDATE    @CPIUSRR
     C                   CALL      'SES780R0'
     C                   PARM      *ZERO         PA$IMP           15 2
     C                   PARM                    PA$MIN           15 2
     C                   PARM                    PA$MAX           15 2
     C                   PARM                    PA$DEB           15 2
     C                   PARM                    PA$SOP           15 2
     C                   PARM                    PAIUSU            7 0
     C                   PARM                    WWNYAP           30
     C                   PARM                    WWFINI            8 0
     C*
     C                   MOVEL     WWFINI        WWPERI            6 0
     C                   Z-ADD     *ZEROS        WW$SOI           15 2
     C                   Z-ADD     *ZEROS        WW$SOM           15 2
     C     PA$MIN        SUB       PA$DEB        XX$MIN           15 2
     C*
+----C                   IF        (PA$MIN-PA$DEB) > 0
|    C                   Z-ADD     XX$MIN        PA$IMP
|    C                   EXSR      ADD02392
|    C                   EVAL      S1DACL=' 8  66'+
|    C                             ' USU:'+ %EDITW(PAIUSU:'     0 ')+
|    C                             ' PER:'+%EDITW(WWPERI:'    /  ')
|    C                   EVAL      %SUBST(S1DACL:56:16)=
|    C                             ' IMP:'+%SUBST(
|    C                                   %EDITW(XX$MIN: '            0,  ')
|    C                             : 6: 11)
|    C                   EVAL      TS$TOT=TS$TOT+XX$MIN
|    c*
|    c                   Eval      WRIAPC=8
|    c                   Eval      WRITRN=66
|    c                   Eval      WRICCL=PAIUSU
|    c                   Eval      WRFAAM=WWPERI
|    c*                  Eval      WR$IMP=TS$TOT
|    c                   Eval      WR$IMP=XX$MIN
|    c*
|    C                   EXSR      AgrLinea
+----C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* SolDeuTot   : Obtiene deuda total de SOL
     C*-------------------------------------------------------------------------
     C     SolDeuTot     BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD
     C                   Z-ADD     FUISUC        @CISUC
     C                   Z-ADD     FUICAH        @CICAH
     C                   UPDATE    @CPIUSRR
     C                   CALL      'SES780R5'
     C                   PARM                    @CINDO
     C                   PARM      *ZERO         PA$IMP           15 2
     C                   PARM      *ZERO         PAIUSU            7 0
+----C                   IF        (PA$IMP       ) > 0
|    C                   EXSR      ADD02392
|    C                   EVAL      S1DACL=' 8  66'+
|    C                             ' USR:'+%EDITW(PAIUSU:'     0 ')
|    C                   EVAL      %SUBST(S1DACL:56:16)=
|    C                             ' IMP:'+
|    C                             %SUBST(
|    C                                   %EDITW(PA$IMP: '            0,  ')
|    C                             : 6: 11)
|    c*
|    c                   Eval      WRIAPC=8
|    c                   Eval      WRITRN=66
|    c                   Eval      WRICCL=PAIUSU
|    c                   Eval      WR$IMP=PA$IMP
|    c*
|    C                   EXSR      AgrLinea
|    C                   EVAL      TS$TOT=TS$TOT+PA$IMP
+----C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* Add02393: Agrega Linea Transaccion 02 392
     C*-------------------------------------------------------------------------
     C     Add02392      BEGSR
     C*
     C                   EVAL      S1DACL='02 392'+
     C                             ' SUC:'+%EDITW(WKISUC:'     0 ')+
     C                             ' CTA:'+%EDITW(WKICCL:'           ')
     C                   EVAL      %SUBST(S1DACL:56:16)=
     C                             ' IMP:'+
     C                             %SUBST(
     C                                   %EDITW(PA$IMP: '            0,  ')
     C                             : 6: 11)
     c*
     c                   Eval      WRIAPC=2
     c                   Eval      WRITRN=392
     c                   Eval      WRISUC=WKISUC
     c                   Eval      WRICCL=WKICCL
     c                   Eval      WR$IMP=PA$IMP
     c*
     C                   EXSR      AgrLinea
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* Add02491: Agrega Linea Transaccion 02 491
     C*-------------------------------------------------------------------------
     C     Add02491      BEGSR
     C*
     C                   EVAL      S1DACL='02 491'+
     C                             ' SUC:'+%EDITW(WKISUC:'     0 ')+
     C                             ' CTA:'+%EDITW(WKICCL:'           ')
     C                   EVAL      %SUBST(S1DACL:56:16)=
     C                             ' IMP:'+
     C                             %SUBST(
     C                                   %EDITW(PA$IMP: '            0,  ')
     C                             : 6: 11)
     c*
     c                   Eval      WRIAPC=2
     c                   Eval      WRITRN=491
     c                   Eval      WRISUC=WKISUC
     c                   Eval      WRICCL=WKICCL
     c                   Eval      WR$IMP=PA$IMP
     c*
     C                   EXSR      AgrLinea
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VerDeudaTC  : Verificar Deuda con Tarjetas de Créditos
     C*-------------------------------------------------------------------------
     C     VerDeudaTC    BEGSR
     C* ... Obtiene Periodo a Cobrar de Tarjetas
     c     *HiVal        SetGT     RETCRESU
     c                   ReadP     RETCRESU                               99
     C                   MOVEL     TMFAAM        WXFAAM
     C* ... Revisar Resúmenes
     C     WKEY28        CHAIN     TCRESU05                           87
+----C     *IN87         DOWEQ     *OFF
|    C* ... Pagos
|    C                   EXSR      CheckPagos
|    C                   MOVE      *BLANKS       CONDSC           20
|    C                   MOVE      *BLANKS       TRNCDE            3
|+---C                   IF        WK$SAL > 0
||   C                   EVAL      TC$TOT=TC$TOT+(WK$SAL)
||   C                   EXSR      AgrLinea
||   c*
||   c                   EVAL      PA$IMP= WK$SAL
     c                   IF        TMIGRC='06'
||   C                   EXSR      ADD02491
     c                   ELSE
||   C                   EXSR      ADD02392
|+---C                   ENDIF
     c*
|+---C                   ENDIF
|    C     WKEY28        READE     TCRESU05                               87
+----C                   ENDDO
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ChecPagos: Verifica Pagos Tarjeta de Créditos
     C*-------------------------------------------------------------------------
     C     CheckPagos    BEGSR
     C                   Z-ADD     TM$IMP        WW$MIN           15 2
     C                   Z-ADD     *ZEROS        WW$PAP           15 2
     C                   Z-ADD     *ZEROS        WW$PAD           15 2
     C                   Z-ADD     *ZEROS        WW$PAT           15 2
     C                   MOVEL     WXFAAM        WYFAAM
     C* ... Calculo Saldo y Pago Minimo
+----C                   If        tmigrc='06' or tmigrc = '07'
|    C     WKEY29        CHAIN     TCRESD01                           82
|+---C                   if        *in82 = *Off
||   C                   MOVEL     *BLANKS       DSBLK2
||   C                   MOVEL     TRBLK2        DSBLK2
||   C     DSSALV        DIV       100           WW$SAL           15 2
||   C                   MOVEL     *BLANKS       DSBLK1
||   C                   MOVEL     TRBLK1        DSBLK1
||   C     DSSMIN        DIV       100           PA$MIN           15 2
||   C*
||   c*  ... Veo pagos ya efectuados
||   C     WKEY29        CHAIN     TCPAGO01                           80
||+--C     *IN80         DOWEQ     *OFF
|||+-C     TGIMON        IFEQ      1
|||| C                   ADD       TG$IMP        WW$PAP
|||+-C                   ELSE
|||| C                   EXSR      SRCOTI
|||| C     TG$IMP        MULT      WW$COR        WW$PAT
|||| C                   ADD       WW$PAT        WW$PAP
|||+-C                   ENDIF
|||  C     WKEY29        READE     TCPAGO01                               80
||+--C                   ENDDO
||   C* ... Veo si hay pagos en los movs del día
|||| C                   Z-ADD     *ZEROS        WW$491           15 2
|||| C                   CALL      'TCRE10R4'
|||| C                   PARM                    TMINDO
|||| C                   PARM                    WW$491           15 2
|||| C                   ADD       WW$491        WW$PAP
||   C* ... Calculamos dif a cobrar
||+--C*    WW$PAP        IFGT      *ZERO
|||  C*    PA$MIN        SUB       WW$PAP        WW$DIF           15 2
||+--C*                  ELSE
|||  C*    WW$SAL        SUB       PA$MIN        WW$DIF
||+--C*                  ENDIF
||   C*
||+--C     PA$MIN        IFGE      WW$PAP
|||  C     PA$MIN        SUB       WW$PAP        WW$DIF           15 2
     C                   ELSE
|||  C                   Z-ADD     *ZERO         WW$DIF           15 2
     C                   ENDIF
||   C*
||   C                   Z-Add     WW$PAP        WW$PAG           15 2
||   C                   Z-Add     WW$DIF        WK$SAL            8 2
||   c*
|+---c                   EndIf
+----c                   EndIf
     c*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* SRCOTI - BUSCA COTIZACION
     C*-------------------------------------------------------------------------
     C     SRCOTI        BEGSR
     C                   Z-ADD     99999         WXISUC
     C                   Z-ADD     2             WWIMON
     C                   Z-ADD     TGFASI        WWFECH
     C     WKEY30        CHAIN     COTIZA                             80
+----C     *IN80         IFEQ      *OFF
|    C                   Z-ADD     MU$COR        WW$COR           12 6
+----C                   ELSE
|    C     WKEY30        SETLL     COTIZA
|    C                   READP     COTIZA                                 80
|    C   80WKEY30        SETLL     COTIZA
|    C   80              READ      COTIZA                                 80
|    C  N80              Z-ADD     MU$COR        WW$COR           12 6
|    C   80              Z-ADD     1             WW$COR           12 6
|    C
+----C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VerDeudaPres: Verificar Deuda en Prestamos
     C*-------------------------------------------------------------------------
     C     VerDeudaPres  BEGSR
     C                   EXSR      OpenC1
     C                   Z-ADD     *ZERO         PR$TOT           15 2
     C                   EXSR      FetchC1
+----C                   DOW       SQLCOD = *ZERO
|    C                   EXSR      VerificarPres
|    C                   EXSR      FetchC1
+----C                   ENDDO
     C                   EXSR      CloseC1
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VerificarPres: Verificar Prestamos
     C*-------------------------------------------------------------------------
     C     VerificarPres BEGSR
     C*
+----C                   IF        C1ISUB='Cobro Cta Atrasada  '
|+---C                   IF        C1ICUO <> 0
||   C                   EXSR      EmiteCupon
|+---C                   ELSE
||   C                   GOTO      NOLIN
|+---C                   ENDIF
|    C                   EVAL      TRNCDE='  2'
+----C                   ELSE
     C* ... Verifica que la op no halla sido dada de alta con un scoring tem
     c                   EXSR      VerScTemp
     c                   IF        EsScTemp = *On
     c                   LEAVESR
     c                   ENDIF
|    C                   EXSR      CancelaCred
|    C                   EVAL      TRNCDE='  4'
+----C                   ENDIF
     C*
     C                   EVAL      S1DACL=' 2  20'+
     C                             ' SUC:'+
     C                             %subst(%EDITW(WKISUC:'     0 '): 4 : 4)+
     C                             ' CTA:'+%EDITW(WKICCL:'           0 ')
     C                   EVAL      %SUBST(S1DACL:56:16)=
     C                             ' IMP:'+%SUBST(
     C                                         %EDITW(WW$IMP:'            0,  ')
     C                                   : 6 :11)
     c*
     c                   Eval      WRIAPC=2
     c                   Eval      WRITRN=20
     c                   Eval      WRISUC=WKISUC
     c                   Eval      WRICCL=WKICCL
     c                   Eval      WR$IMP=WW$IMP
     c*
     C                   EXSR      AgrLinea
     C                   EVAL      S1DACL='20 '+TRNCDE+
     C                             ' SUC:'+
     C                             %subst(%EDITW(C1SUCA:'     0 '): 4 : 4)+
     C                             ' CRE:'+%EDITW(C1INCR:'              0')+' '+
     C                             ' DES:'+
     C                                    %EDITW(C1IDEG:'   0 ')
+----C                   IF        TRNCDE='  2'
|    C                   EVAL      S1DACL=%TRIM(S1DACL)+
|    C                             ' CTA:'+%SUBST(%EDITW(C1ICUO:'  0'):1:3)
+----C                   ENDIF
     C                   EVAL      %SUBST(S1DACL:56:16)=
     C                             ' IMP:'+%SUBST(
     C                                     %EDITW(WW$IMP:'            0,  ')
     C                                     : 6 :11)
     c*
     c                   Eval      WRIAPC=20
     c                   Move      TRNCDE        WRITRN
     c                   Eval      WRISUC=C1SUCA
     c                   Eval      WRINCR=C1INCR
     c                   Eval      WRIDEG=C1IDEG
     c                   Eval      WRICUO=C1ICUO
     c                   Eval      WR$IMP=WW$IMP
     c*
     C                   EXSR      AgrLinea
     c*
     C                   EVAL      PR$TOT=PR$TOT + WW$IMP
     C*
     C     NOLIN         ENDSR
     C*-------------------------------------------------------------------------
     C* VerScTemp
     C*-------------------------------------------------------------------------
     c     VerScTemp     BEGSR
     C*
     C                   Z-ADD     *ZERO         WWILCR            4 0
     C                   Z-ADD     *ZERO         CANREC           15 0
     c/exec sql
     c+ SELECT
     c+ JVILCR ,
     c+ (SELECT COUNT(*) FROM bascor
     c+   WHERE
     c+    sciemp= b.tbiemp) INTO :WWILCR, :CANREC
     c+
     c+  FROM prcred
     c+  LEFT JOIN prhatb b on
     c+       b.tbitin=jvitin and b.tbinin=jvinin
     c+
     c+ WHERE     JVISUC=:C1SUCA
     c+       AND JVINCR=:C1INCR
     c+       AND JVIDEG=:C1IDEG
     c/END-EXEC
     C*
     C                   MOVE      *OFF          EsScTemp          1
     c                   IF        WWILCR = 7 and CANREC = 0
     C                   MOVE      *ON           EsScTemp
     c                   ENDIF
     c                   IF        WWILCR = 15 and CANREC = 0
     C                   MOVE      *ON           EsScTemp
     c                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* AgrLinea
     C*-------------------------------------------------------------------------
     c     AgrLinea      BEGSR
     C                   EVAL      S1IJOB=@PJOBN
     C                   EVAL      S1ISEQ=S1ISEQ+1
     C                   WRITE     REBASCTM
     C                   EVAL      S2IJOB=@PJOBN
     C                   EVAL      S2ISEQ=S1ISEQ+1
     C                   EVAL      S2DACL=S1DACL
     C* ... Guardamos en BASCT1 para pasar a PRMFED
     C                   EVAL      S2ILCR=WRIAPC
     C                   EVAL      S2IMON=WRITRN
     C                   EVAL      S2$A03=WRICCL
     C                   EVAL      S2$INP=WR$IMP
     C                   EVAL      S2$A04=WRFAAM
     C                   EVAL      S2ISUC=WRisuc
     C                   EVAL      S2INCR=WRINCR
     C                   EVAL      S2IDEG=WRIDEG
     C                   EVAL      S2QCUO=WRICUO
     C                   EVAL      S2DF03=WRDAV1
     C*
     C                   WRITE     REBASCT1
     C*
     C* ... Luego de grabado, blanqueamos
     C                   Clear                   WRIAPC
     C                   Clear                   WRITRN
     C                   Clear                   WRICCL
     C                   Clear                   WR$IMP
     C                   Clear                   WRFAAM
     C                   Clear                   WRisuc
     C                   Clear                   WRINCR
     C                   Clear                   WRIDEG
     C                   Clear                   WRICUO
     C                   Clear                   WRDAV1
     C*
     C                   EVAL      S3IJOB=@PJOBN
     C                   EVAL      S3ISEQ=S1ISEQ+1
     C                   EVAL      S3DACL=S1DACL
     C                   WRITE     REBASCT2
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* EmiteCupon
     C*-------------------------------------------------------------------------
     C     EmiteCupon    BEGSR
     C     @PJOBN        CHAIN     @CPIUSD
     C                   Z-ADD     C1SUCA        @CISUC
     C                   Z-ADD     C1INCR        @CINCR
     C                   Z-ADD     C1IDEG        @CIDEG
     C                   Z-ADD     C1ICUO        @CICUO
     C                   Z-ADD     C1SUCA        DSSUCA
     C                   Z-ADD     C1INCR        DSINCA
     C                   Z-ADD     DSOPAN        @CINCV
     C                   Z-ADD     C1IDEG        @CIDEV
     C                   UPDATE    @CPIUSRR
     C*                  CALL      'PRKGSCSB'
     C*                  PARM                    PAFLAG            1
     C                   MOVEL     AASFEI        PAFAAS            6 0
     C                   MOVEL     AASFEI        PASFEI            8 0
     C                   CALL      'PRHA01SC'
     C                   PARM                    PAFAAS
     C                   PARM                    C1SUCA
     C                   PARM                    C1INCR
     C                   PARM                    C1IDEG
     C                   PARM                    C1ICUO
     C                   PARM                    PASFEI
     C* ... ... Calcular total de la cuota
     C                   CALL      'BASC05RG'
     C                   PARM                    AASFEI
     C                   PARM                    C1SUCA
     C                   PARM                    C1INCR
     C                   PARM                    C1IDEG
     C                   PARM                    C1ICUO
     C                   PARM                    WW$IMP           15 2
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CancelaCred
     C*-------------------------------------------------------------------------
     C     CancelaCred   BEGSR
     C     KPRCUOT02     CHAIN     REPRMOVI                           99
+----C                   DOW       *IN99 = *OFF
|    C                   DELETE    REPRMOVI
|    C     KPRCUOT02     READE     REPRMOVI                               99
+----C                   ENDDO
     C     KPRCUOT02     CHAIN(N)  PRCUOT02                           99
+----C                   DOW       *IN99=*OFF
|+---C                   IF        KGFEPA=*ZERO
||   C*    @PJOBN        CHAIN     @CPIUSD                            99
||   C*                  Z-ADD     C1SUCA        @CISUC
||   C*                  Z-ADD     C1INCR        @CINCR
||   C*                  Z-ADD     C1IDEG        @CIDEG
||   C*                  Z-ADD     KGICUO        @CICUO
||   C*                  UPDATE    @CPIUSRR
||   C*                  CALL      'PRKGSCSB'
||   C*                  PARM                    PAFLAG
||   c*
||   C                   MOVEL     AASFEI        PAFAAS            6 0
||   C                   MOVEL     AASFEI        PASFEI            8 0
||   C                   CALL      'PRHA01SC'
||   C                   PARM                    PAFAAS
||   C                   PARM                    C1SUCA
||   C                   PARM                    C1INCR
||   C                   PARM                    C1IDEG
||   C                   PARM                    KGICUO
||   C                   PARM                    PASFEI
||   c*
|+---C                   ENDIF
|    C     KPRCUOT02     READE(N)  PRCUOT02                               99
+----C                   ENDDO
     C* ... Pone en Arch. Temporal de Creditos en Cancelación
     C                   Z-ADD     @PJOBN        CLIJOB
     C                   ADD       1             CLISEQ
     C                   Z-ADD     C1SUCA        CLISUC
     C                   Z-ADD     C1INCR        CLINCR
     C                   Z-ADD     C1IDEG        CLIDEG
     C                   WRITE     REBASCCL
     C* ... Cálc. Neto a pagar (Cód. 9) deja en @C$MON
     C                   Z-ADD     *ZERO         WWICUO            3 0
     C                   Z-ADD     *ZERO         WW$IMP           15 2
     C                   CALL      'BASC05RG'
     C                   PARM                    AASFEI
     C                   PARM                    C1SUCA
     C                   PARM                    C1INCR
     C                   PARM                    C1IDEG
     C                   PARM                    WWICUO
     C                   PARM                    WW$IMP
     C                   ENDSR
     c*
     C*-------------------------------------------------------------------------
     C* DeclCursores: Declarar los cursores que utilizaremos
     C*-------------------------------------------------------------------------
     C     DeclCursores  BEGSR
     C*
     C* ... Para Leer Créditos que tenga esta persona
     c* ... ]]]]IMPORTANTE]]]]
     c* ... No dar F4 sobre la sentencia que la desacomoda y despues es
     C*     INENTENDIBLE
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR
     C+  SELECT DISTINCT
     C+          JVISUC,
     C+          JVINCR,
     C+          JVIDEG,
     C+          CAST( CASE
+----C+                  WHEN ((
     C+                             (SCDF05='S/RECIBO')
     C+                          OR (SCDF05 IS NULL AND TB$A03 =1)
     C+                        )
     C+                        AND NOT
     C+                        (     JVISUC=:RFSUCA
     C+                          AND JVINCR=:RFINCR
     C+                          AND JVIDEG=:RFIDEG
     C+                        )
     C+                       )
     C+                           THEN 'Cred. sin Rec.Sueldo'
     C+                 WHEN     JVISUC=:RFSUCA
     C+                      AND JVINCR=:RFINCR
     C+                      AND JVIDEG=:RFIDEG
     C+                 THEN 'Cancelacion P/Refin.'
     C+                 ELSE
     C+                            'Cobro Cta Atrasada  '
     C+                 END AS CHAR(20)),
     C+                 IFNULL(KGICUO, 0)
     C+  FROM BADCCL
     C+       INNER JOIN PRCRED ON
     C+             JVISUC=OTISUC
     C+         AND JVICCL=OTICCL
     C+         AND JVFEPA=0
     C+         AND (SELECT COUNT(*)
     C+               FROM PRMOVI60
     C+              WHERE M2ISUC=JVISUC
     C+                AND M2INCR=JVINCR
     C+                AND M2IDEG=JVIDEG
     C+                AND M2INCR<> :RFINCR
     C+                AND M2FASI = :AASFEI
     C+             ) = 0
     C+       LEFT JOIN PRHATB ON
     C+                   JVITIN=TBITIN
     C+              AND  JVININ=TBININ
     C+       LEFT  JOIN BASCOR ON
     C+                 JVISUC=SCISUC
     C+             AND JVINCR=SCINCR
     C+             AND JVIDEG=SCIDEG
     C+       LEFT JOIN PRCUOT ON
     C+                 JVISUC=KGISUC
     C+             AND JVINCR=KGINCR
     C+             AND JVIDEG=KGIDEG
     C+             AND KGFEPA=0
     C+             AND KGFVCU <= :AASFEI
     C+             AND CAST(
     C+                      CASE WHEN ((
     C+                                  (SCDF05='S/RECIBO') OR
     C+                                  (SCDF05 IS NULL  AND TB$A03=1)
     C+                                 )
     C+                                 AND NOT
     C+                                 (     JVISUC= :RFSUCA
     C+                                   AND JVINCR =:RFINCR
     C+                                   AND JVIDEG= :RFIDEG
     C+                                 )
     C+                                )
     C+                                THEN 'SR'
     C+                           WHEN    JVISUC=:RFSUCA
     C+                               AND JVINCR=:RFINCR
     C+                               AND JVIDEG=:RFIDEG
     C+                                THEN 'RF'
     C+                           ELSE      'CA'
     C+                           END AS CHAR(2))='CA'
     C+  WHERE
     C+       OTINDO=:@CINDO
     C+   AND OTITTL=1
     C+   AND JVILCR NOT IN (
     C+       800, 801, 802,
     C+       803, 90, 1, 2,
     C+       887, 888, 889,
     C+       890, 891, 7997,
     C+       7998, 7999, 892,
     C+       991, 997, 998,
     C+       999, 81)
     C+   AND JVILCR < 8000
     C+   AND JVIESA='1'
     C+   AND JVVIGE>0
     C+  ORDER BY
     C+      CAST( CASE
     C+            WHEN ((
     C+                      (SCDF05='S/RECIBO')
     C+                   OR (SCDF05 IS NULL AND TB$A03 =1)
     C+                  )
     C+                  AND NOT
     C+                  (     JVISUC=:RFSUCA
     C+                    AND JVINCR=:RFINCR
     C+                    AND JVIDEG=:RFIDEG
     C+                  )
     C+                 )
     C+                 THEN 'Cred. sin Rec.Sueldo'
     C+                 WHEN     JVISUC=:RFSUCA
     C+                      AND JVINCR=:RFINCR
     C+                      AND JVIDEG=:RFIDEG
     C+                 THEN 'Cancelacion P/Refin.'
     C+                 ELSE
     C+                      'Cobro Cta Atrasada  '
     C+                 END AS CHAR(20)),
     C+      JVISUC ,
     C+      JVINCR
     C/END-EXEC
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* OpenC1: Abre Cursor 1
     C*-------------------------------------------------------------------------
     C     OpenC1        BEGSR
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FetchC1: Recupera Datos Cursor1
     C*-------------------------------------------------------------------------
     C     FetchC1       BEGSR
     C/EXEC SQL
     C+ FETCH C1 INTO :C1SUCA, :C1INCR, :C1IDEG, :C1ISUB, :C1ICUO
     C/END-EXEC
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CloseC1: Cierra Cursor1
     C*-------------------------------------------------------------------------
     C     CloseC1       BEGSR
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDSR
