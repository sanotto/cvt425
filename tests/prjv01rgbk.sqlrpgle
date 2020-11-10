*SRCMBRTXT:null                                   
      *OVRDBF FILE(BASCTMRRN) TOFILE(*LIBL/BASCTM)
     FBAICCL    IF   E           K DISK
     FPRENCO    IF   E           K DISK
     FBASCTM    UF A E           K DISK
     FBASCTMRRN IF   E             DISK    RENAME(REBASCTM:RTMP) USROPN
     F@CPIUSD   UF   E           K DISK
     F@CPISYS   IF   E           K DISK
     D*-------------------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
     D*-------------------------------------------------------------------------
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*-------------------------------------------------------------------------
     D ERRDS           DS
     D  ERRTXT                 1    255
     D  WWNCU1                 1     55
     D  WWNCU2                56    110
     D  WWNCU3               111    165
     D  WWNCU4               166    220
     D  WWNCB1               221    275
     D  WWNCB2               276    330
     D  WWNCB3               331    385
     D  WWNCB4               386    440
     D*-------------------------------------------------------------------------
     D ERR001          C                   CONST('NO EXISTE ESE TIPO D-
     D                                     E ENTREGA/COBRO')
     D ERR002          C                   CONST('NO EXISTE ESA CUENTA-
     D                                      CLIENTE       ')
     D ERR003          C                   CONST('NO HAY CUENTAS VALID-
     D                                     AS PARA LA OPERATORI-
     D                                     A')
     D*-------------------------------------------------------------------------
     D QRYSTR          S           2048
     D*-------------------------------------------------------------------------
     C                   EXSR      CLRTMP
     C                   EXSR      OPNCUR
     C                   EXSR      FILTMP
     C     WWCNT         IFLE      *ZERO
     C                   EVAL      ERRTXT='NO EXISTEN CTAS VALIDAS DE TIPO:'+
     C                             JLISUB+' PARA LA OPERATORIA:'+WWTEXT
     C                   EXSR      ABORT
     C                   ENDIF
     C                   EXSR      DSPCTA
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C     CLRTMP        BEGSR
     C*
     C     @PJOBN        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBASCTM
     C     @PJOBN        READE     REBASCTM                               99
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     ABORT         BEGSR
     C                   MOVE      *ON           WWERRO
     C                   EXSR      DSPMSG
     C                   EXSR      ENDPGM
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    WWITEC            2 0
     C                   PARM                    WWISUC            5 0
     C                   PARM                    WWICCC           15 0
     C                   PARM                    WWICCL            9 0
     C                   PARM                    WWTEXT           10
     C                   PARM                    WWERRO            1
     C*
     C                   MOVE      WWISUC        CHISUC            5
     C                   MOVE      WWICCL        CHICCL            9
     C                   MOVE      *BLANKS       WRPERS            2
     C                   MOVE      *BLANKS       WRNCCL           30
     C                   MOVE      *BLANKS       WRISUB            2
     C                   MOVE      *ZERO         WRISUC            5 0
     C                   MOVE      *ZERO         WRICCC           15 0
     C*
     C*
     C     WKEY01        KLIST
     C                   KFLD                    WWITEC
     C*
     C     WKEY02        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWICCL
     C*
     C                   EVAL      CMDLNE='OVRDBF FILE(BASCTMRRN)'  +
     C                             ' TOFILE(*LIBL/BASCTM)'
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDLNE         1024
     C                   PARM      1024          CMDSZE           15 5
     C                   OPEN      BASCTMRRN
     C*
     C     WKEY01        CHAIN     REPRENCO                           99
     C   99              MOVEL(P)  ERR001        ERRTXT
     C   99              EXSR      ABORT
     C     WKEY02        CHAIN     BAICCL                             99
     C   99              MOVEL(P)  ERR002        ERRTXT
     C   99              EXSR      ABORT
     C                   EXSR      FETCC1
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DSPMSG        BEGSR
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
     C*-------------------------------------------------------------------------
     C     OPNCUR        BEGSR
     C/FREE
       IF OSITIN <> *ZERO AND OSININ <> 0;
        QRYSTR='  SELECT CASE WHEN A.OSITIN <> 0 THEN ''PJ'' ' +
        'ELSE ''PF'' END ,B.OSNCCL, CASE WHEN FUICAH IS NOT NULL THEN ''AC'' '+
        'WHEN BMICCC IS NOT NULL THEN ''CC'' ELSE ''OT'' END , IFNULL(BMISUC, '+
        'FUISUC), IFNULL(BMICCC, FUICAH) FROM BAICCL A LEFT JOIN BAICCL B '+
        'ON A.OSITIN=B.OSITIN AND A.OSININ=B.OSININ LEFT JOIN ACCTAC ON  ' +
        'FUISUC=B.OSISUC AND FUICCL=B.OSICCL and fufbaj=0         '+
        ' and fuibac=0                       LEFT JOIN CCCTCT ON  '+
        'BMISUC=B.OSISUC AND BMICCL=B.OSICCL and bmfbaj=0                  '+
        ' and bmibcc=0                       WHERE CASE WHEN FUICAH IS NOT '+
        'NULL THEN ''AC'' WHEN BMICCC IS NOT NULL THEN ''CC'' '+
        ' ELSE ''OT'' END ='''+
         JLISUB+''' AND A.OSISUC='+CHISUC+' AND A.OSICCL= '+CHICCL;
       ELSE;
        QRYSTR='SELECT                                                   '+
        'CASE WHEN A.OSITIN <> 0 THEN ''PJ'' ELSE ''PF'' END,            '+
        'A.OSNCCL,                                                       '+
        'CASE WHEN FUICAH IS NOT NULL THEN ''AC'' WHEN  BMICCC IS        '+
        ' NOT NULL THEN ''CC'' ELSE ''OT'' END , IFNULL(BMISUC, FUISUC), '+
        'IFNULL(BMICCC, FUICAH)                                          '+
        'FROM      BAICCL A                                              '+
        'LEFT JOIN BADCCL B ON                                           '+
        '     A.OSISUC=B.OTISUC AND  A.OSICCL=B.OTICCL                   '+
        'LEFT JOIN BADCCL C ON                                           '+
        '   B.OTINDO=C.OTINDO AND B.OTITDO=C.OTITDO AND C.OTITTL=1       '+
        'LEFT JOIN ACCTAC ON                                             '+
        ' FUISUC=C.OTISUC AND FUICCL=C.OTICCL AND FUFBAJ=0 AND FUIBAC=0  '+
        'LEFT JOIN CCCTCT ON                                             '+
        ' BMISUC=C.OTISUC AND BMICCL=C.OTICCL AND BMFBAJ=0 AND BMIBCC=0  '+
        'WHERE                                                           '+
        '   A.OSISUC='+CHISUC+' AND                                      '+
        '   A.OSICCL='+CHICCL+' AND                                      '+
        '   CASE WHEN FUICAH IS NOT NULL THEN ''AC'' WHEN  BMICCC IS     '+
        '   NOT NULL THEN ''CC'' ELSE ''OT'' END ='''+JLISUB+'''         ';
       ENDIF;
      /END-FREE

     C/EXEC SQL
     C+ PREPARE S1 FROM :QRYSTR
     C/END-EXEC

     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR S1
     C/END-EXEC

     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     FETCC1        BEGSR
     C/EXEC SQL
     C+ FETCH C1 INTO :WRPERS, :WRNCCL, :WRISUB, :WRISUC, :WRICCC
     C/END-EXEC
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DSPCTA        BEGSR
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Selecc. Una Cta P/'+WWTEXT
     C                   EVAL      WWTIT1=*BLANKS
     C                   EVAL      WWTIT2='Por favor seleccione una '+
     C                             ' de las siguientes ctas'
     C                   EVAL      WWTIT3='Como cuenta para: '+WWTEXT
     C                   EVAL      WWTIT4=*BLANKS
     C                   EVAL      WWTIT5=*BLANKS
     C                   EVAL      WWATR='    H'
     C                   EVAL      WWTIT5='Suc.       Cuenta   Nombre '
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
     C     @ZRRNO        CHAIN     RTMP                               99
     C                   IF        @FN(03) =*ON OR @FN(12)=*ON
     C                   MOVE      *ON           WWERRO
     C                   EXSR      ENDPGM
     C                   ENDIF
     C                   IF        *IN99 = *OFF
     C                   MOVE      S1ISUC        WWISUC
     C                   MOVE      S1INCR        WWICCC
     C                   MOVE      *OFF          WWERRO
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     FILTMP        BEGSR
     C                   Z-ADD     *ZERO         WWCNT            15 0
     C                   EXSR      FETCC1
     C     SQLCOD        DOWEQ     *ZERO
     C                   ADD       1             WWCNT
     C     WWICCC        IFEQ      WRICCC
     C                   MOVE      *BLANKS       WWERRO
     C                   EXSR      ENDPGM
     C                   ENDIF
     C                   EVAL      S1DACL=%EDITC(WRISUC:'J')+' '+
     C                                    %EDITC(WRICCC:'Z')+' '+
     C                                    WRNCCL
     C                   MOVE      WRISUC        S1ISUC
     C                   MOVE      WRICCC        S1INCR
     C                   MOVE      @PJOBN        S1IJOB
     C                   WRITE     REBASCTM
     C                   EXSR      FETCC1
     C                   ENDDO
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     *pssr         BEGSR
     C*
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   EXSR      CLRTMP
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   EXSR      CLRTMP
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
