*SRCMBRTXT:BASC00S9: Valida que el cliente no teng
     FBASCTM    UF A E           K DISK
     FBAICCL    IF   E           K DISK
     D*----------------------------------------------------------------
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
     DMYPSDS          SDS
     D @PJOBN                264    269S 0
     D*-------------------------------------------------------------------------
     C                   MOVE      *OFF          GESMOR            1
     C                   EXSR      WRTHDR
     C                   EXSR      OPNQRY
     C                   EXSR      FETCH1
     C     SQLCOD        DOWEQ     *ZERO
     C                   ADD       1             CUENTA
     C     KEYICCL       CHAIN     REBAICCL
     C                   EVAL      S1DACL=%EDITC(S1ISUC:'3') +
     C                             ' ' +  %EDITC(S1INCR:'3') +
     C                             ' ' +  %EDITC(S1IDEG:'3') +
     C                             ' ' +  %EDITC(S1ITTL:'3') +
     C                             ' ' +  %EDITC(S1ICCL:'3') +
     C                             ' ' +  OSNCCL
     C     S1ILCR        IFGE      2000
     C                   MOVE      *ON           GESMOR            1
     C                   ENDIF
     C                   WRITE     REBASCTM
     C                   EXSR      FETCH1
     C                   ENDDO
     C     CUENTA        IFNE      *ZERO
     C                   CALL      'BASC02TE'
     C     GESMOR        IFEQ      *ON
     c                   EVAL      errtxt='El cliente tiene créditos '+
     C                                    'en Gestión Y Mora, o tiene'+
     c                                    ' un Crédito activo en Line'+
     c                                    'a 10.  Debe dirigirse al A'+
     c                                    'nexo San Martín a la ofici'+
     c                                    'na de Gestión y Mora para '+
     c                                    'regularizar su situacion.'
     c                   ELSE
     c                   EVAL      errtxt='El cliente tiene créditos '+
     C                                    'en mora'
     c                   ENDIF
     c                   EXSR      dsperr
     C                   ENDIF
     C                   EXSR      ENDPGM
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     WRTHDR        BEGSR
     C                   Z-ADD     @PJOBN        S1IJOB
     C                   Z-ADD     *ZERO         S1IMON
     C                   EVAL      S1DACL='EXISTEN CREDITOS EN MORA'
     C                   WRITE     REBASCTM
     C                   EVAL      S1DACL='------------------------'
     C                   WRITE     REBASCTM
     C                   EVAL      S1DACL='                        '
     C                   WRITE     REBASCTM
     C                   EVAL      S1DACL='SUC   CREDITO         DESG' +
     C                                    ' TI CTA.CLTE.'
     C                   WRITE     REBASCTM
     C                   EVAL      S1DACL='----- --------------- ----' +
     C                                    ' -- ----------------------------'
     C                   WRITE     REBASCTM
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     OPNQRY        BEGSR
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT JVISUC, JVINCR, JVIDEG, JVILCR,
     C+ JVICCL, OTITTL FROM BADCCL INNER JOIN PRCRED ON OTISUC=JVISUC AND
     C+ OTICCL=JVICCL AND JVFEPA=0 AND JVIESA = '1' AND
     C+ ( (JVITIN=91 AND JVININ=63 ) OR  JVIMDV <> ' '
     C+ OR JVILCR >= 2000 OR JVILCR = 10) AND JVILCR NOT IN (800, 801,
     C+ 802, 803, 90, 887, 888, 889, 890, 891, 7997, 7998, 7999, 892,
     C+ 991, 997, 998, 999, 81) WHERE OTINDO=:PAINDO AND OTITTL <> 20 AND
     C+ NOT ( OTICCL > 990000000 AND OTICCL < 999999999 AND OTITTL > 1 )
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     FETCH1        BEGSR
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :S1ISUC, :S1INCR, :S1IDEG, :S1ILCR,
     C+ :S1ICCl, :s1ITTL
     C/END-EXEC
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     ENDPGM        BEGSR
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
     C*----------------------------------------------------------------
     C*
     C*----------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAINDO           15 0
     C                   PARM                    CUENTA           15 0
     C                   Z-ADD     *ZERO         CUENTA           15 0
     C                   Z-ADD     *ZERO         S1ICCL            9 0
     C                   Z-ADD     *ZERO         S1ITTL            2 0
     C*   ... Borra registros anteriores si los hay
     C     @PJOBN        CHAIN     BASCTM                             99
+----C     *IN99         DOWEQ     *OFF
MOVE C                   DELETE    REBASCTM
MOVE C     @PJOBN        READE     BASCTM                                 99
+----C                   ENDDO
     C*
     C     KEYICCL       KLIST
     C                   KFLD                    S1ISUC
     C                   KFLD                    S1ICCL
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* DSPERR: Mostrar Mensaje de Error
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
