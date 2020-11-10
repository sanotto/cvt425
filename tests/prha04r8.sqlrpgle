*SRCMBRTXT:Ces.Hab.-Proceso Nocturno     -Exp. Arc
     FBASCOR04  UF   E           K DISK
     FBASCOR01  UF   E           K DISK    RENAME(REBASCOR:BASCOR1)
     FBADCCL01  IF   E           K DISK
     FBASCAR02  IF   E           K DISK
     FBASCTM    UF A E           K DISK
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D @PJOBN                264    269S 0
     DDSMUTU           DS
     D MUTLIN                  1     20
     D MUTFDE                  1      6  0
     D MUTFHA                  7     12  0
     D MUTNCE                 13     20  0
     D*-------------------------------------------------------------------------
     c*
     C                   EXSR      Inicio
     C                   EXSR      OpnCred8
     C                   EXSR      FetCred8
     C                   DOW       SQLCOD=*ZERO
     C                   EXSR      VerCredito
     C                   EXSR      FetCred8
     C                   ENDDO
     C                   EXSR      CloCred8
     C                   EXSR      EndPgm
     C*-------------------------------------------------------------------------
     C* VerCredito: Verifica que el Credito este correctamente asociado
     C*-------------------------------------------------------------------------
     C     VerCredito    BEGSR
     C*
     C     KBASCOR04     CHAIN     REBASCOR                           99
     C                   IF        *IN99 = *OFF
     C                   EXSR      ValidaFecha
     C                   ELSE
     C                   EXSR      AsociaCredito
     C                   ENDIF
     C                   EXSR      EscribeLog
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* EscribeLog: ...
     C*----------------------------------------------------------------
     C     EscribeLog    BEGSR
     C*
     C                   EVAL      S1IJOB=@PJOBN
     C                   EVAL      S1$SAL=WKFECH
     C                   EVAL      S1$CUI=SCINDO
     C                   EVAL      S1ISUC=C1ISUC
     C                   EVAL      S1INCR=C1INCR
     C                   EVAL      S1IDEG=C1IDEG
     C                   EVAL      S1DF02=SCNYAP
     C                   EVAL      S1ISEQ=SC$A06
     C                   IF        ErrTxt=*blanks
     c                   EVAL      S1DF01='CREDITO ASOCIADO A..'
     C                   EVAL      S1REFC=SCREFC
     C                   ELSE
     c                   EVAL      S1DF01=Errtxt
     C                   EVAL      S1REFC=*BLANKS
     C                   ENDIF
     C                   WRITE     REBASCTM
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ValidaFecha: Valida y Corrige Fecha de Toma
     C*-------------------------------------------------------------------------
     C     ValidaFecha   BEGSR
     C*
     C                   SELECT
     C                   WHEN      SC$CUO=*ZERO
     c                   EVAL      ErrTxt='SIN FECHA DE TOMA... ACT.'
     C                   EVAL      SC$CUO=WKFECH
     C                   UPDATE    REBASCOR
     C                   WHEN      ErrTxt<>*ZERO AND SC$CUO <> WKFECH
     c                   EVAL      ErrTxt='FECHA TOMA INCORR... ACT.'
     C                   EVAL      SC$CUO=WKFECH
     C                   WHEN      SC$CUO<>*ZERO AND SC$CUO =  WKFECH
     c                   EVAL      ErrTxt='OK'
     C                   ENDSL
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* AsociaCredito: Intenta Asociar el Credito al Recibo
     C*-------------------------------------------------------------------------
     C     AsociaCredito BEGSR
     C*
     C* ... Busca el Area del Credito
     C     KBASCAR       CHAIN     REBASCAR                           99
     C                   IF        *IN99 = *ON
     C                   EVAL      ErrTxt='Credito sin Area Asociada     '
     C                   LEAVESR
     C                   ENDIF
     C* ... Busca Primer Firmante
     C     KBADCCL       CHAIN     REBADCCL                           99
     C                   IF        *IN99 = *ON
     C                   EVAL      ErrTxt='Cta Clte Inex o S/1er Firmante'
     C                   ENDIF
     C*
     C                   EVAL      ErrTxt='No se pudo asociar un recibo  '
     C     OTINDO        CHAIN     BASCOR1                            99
     C                   DOW       *IN99 = *OFF
     C                   IF        SCDCAR=*BLANKS
     C                   MOVE      *ZEROS        SCDCAR
     C                   ENDIF
     C                   MOVE      SCDCAR        MUTLIN
     C                   Z-ADD     MUTFDE        @CFDH1            8 0
     C                   Z-ADD     MUTFHA        @CFDH2            8 0
     C                   ADD       20000000      @CFDH1
     C                   ADD       20000000      @CFDH2
     C                   IF        SCIBCF=ARIBCF AND SC$A03 > 0   AND
     C                             @CFDH1 >= WKFECH AND WKFECH <= @CFDH2
     C                   MOVE      C1ISUC        SCISUC
     C                   MOVE      C1INCR        SCINCR
     C                   MOVE      C1IDEG        SCIDEG
     C                   Z-ADD     WKFECH        SC$CUO
     C                   UPDATE    BASCOR1
     C                   EVAL      ErrTxt=*BLANKS
     C                   LEAVE
     C                   ENDIF
     C     OTINDO        READE     BASCOR1                                99
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* OpnCred8: Abrir Cursor Creditos Linea 8 del Dia
     C*-------------------------------------------------------------------------
     C     OpnCred8      BEGSR
     C*
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT
     C+  JVISUC,
     C+  JVINCR,
     C+  JVIDEG,
     C+  JVICCL,
     C+  JVITIN,
     C+  JVININ
     C+ FROM
     C+  PRCRED
     C+ WHERE JVILCR=8 AND JVIESA='1' and JVFALT= :WKFECH  AND JVFEPA=0
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN  C1
     C/END-EXEC
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FetCred8: Leer Fila de Cursor C1
     C*-------------------------------------------------------------------------
     C     FetCred8      BEGSR
     C*
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :C1ISUC, :C1INCR, :C1IDEG, :C1ICCL,
     C+ :C1ITIN, :C1ININ
     C/END-EXEC
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* OpnCred8: Cerrar
     C*-------------------------------------------------------------------------
     C     CloCred8      BEGSR
     C*
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* EndPgm: Finalización de Programa
     C*-------------------------------------------------------------------------
     C     EndPgm        BEGSR
     C                   EVAL      *INLR=*OFF
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* Inicio : Subrutina de Inicializacion
     C*-------------------------------------------------------------------------
     C     Inicio        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PAFECH            6
     C* ... Convertir Fecha a Formato ISO
     C                   MOVE      PAFECH        CHFECH            6
     C                   MOVE      *BLANKS       WWFECH            8
     C                   EVAL      WWFECH='20'+%SUBST(CHFECH:5:2)+
     C                                         %SUBST(CHFECH:3:2)+
     C                                         %SUBST(CHFECH:1:2)
     C                   MOVE      WWFECH        WKFECH            8 0
     C*
     C     *LIKE         DEFINE    SCISUC        C1ISUC
     C     *LIKE         DEFINE    SCINCR        C1INCR
     C     *LIKE         DEFINE    SCIDEG        C1IDEG
     C     *LIKE         DEFINE    OTICCL        C1ICCL
     C     *LIKE         DEFINE    ARITIN        C1ITIN
     C     *LIKE         DEFINE    ARININ        C1ININ
     C     *LIKE         DEFINE    OTITTL        WWITTL
     C*
     C     KBASCOR04     KLIST
     C                   KFLD                    C1ISUC
     C                   KFLD                    C1INCR
     C                   KFLD                    C1IDEG
     C     KBADCCL       KLIST
     C                   KFLD                    C1ISUC
     C                   KFLD                    C1ICCL
     C                   KFLD                    WWITTL
     C     KBASCAR       KLIST
     C                   KFLD                    C1ITIN
     C                   KFLD                    C1ININ
     C     KPRCRED       KLIST
     C                   KFLD                    SCISUC
     C                   KFLD                    SCINCR
     C                   KFLD                    SCIDEG
     C*
     C                   Z-ADD     1             WWITTL
     C                   MOVE      *BLANKS       ErrTxt           20
     C*   ... Borra registros anteriores si los hay
     C     @PJOBN        CHAIN     BASCTM                             99
+----C                   DOW       *IN99=*OFF
|    C                   DELETE    REBASCTM
|    C     @PJOBN        READE     BASCTM                                 99
+----C                   ENDDO
     C*
     C                   ENDSR
