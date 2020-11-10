*SRCMBRTXT:BASC00S9: Valida que el cliente no teng
     FBASCTM    UF A E           K DISK
     FBAICCL    IF   E           K DISK
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D @PJOBN                264    269S 0
     D*-------------------------------------------------------------------------
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
     C                   WRITE     REBASCTM
     C                   EXSR      FETCH1
     C                   ENDDO
     C     CUENTA        IFGT      *ZERO
     C                   CALL      'BASC02TE'
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
     C+ DECLARE C1 CURSOR FOR SELECT jvisuc, jvincr, jvideg, jviLCR,
     C+ JVICCL, OTITTL FROM BADCCL INNER JOIN PRCRED ON OTISUC=JVISUC AND
     C+ OTICCL=JVICCL AND jvfepa=0 and ( (JVITIN=91 AND JVININ=63 ) or
     C+ JVIMDV <> ' ') AND JVILCR NOT IN (800, 801, 802, 803, 90, 887,
     C+ 888, 889, 890, 891, 7997, 7998, 7999, 892, 991, 997, 998, 999)
     C+ WHERE OTINDO=:PAINDO AND OTITTL <> 20 AND NOT ( OTICCL >
     C+ 990000000 AND OTICCL < 999999999 AND OTITTL > 1 )
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
