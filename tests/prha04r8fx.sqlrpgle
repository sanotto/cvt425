*SRCMBRTXT:Ces.Hab.-Proceso Nocturno     -Exp. Arc
     FBASCOR04  iF   E           K DISK    RENAME(REBASCOR:BASCOR4)
     FBASCOR01  UF   E           K DISK    RENAME(REBASCOR:BASCOR1)
     FPRCRED    UF   E           K DISK
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
     C                   EXSR      Inicio
     C                   EXSR      OpnCred8
     C                   EXSR      FetCred8
     C                   DOW       SQLCOD=*ZERO
     C                   EXSR      UpdBascor
     C                   EXSR      FetCred8
     C                   ENDDO
     C                   EXSR      CloCred8
     C                   EXSR      EndPgm
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     DeclareC2     BEGSR
     C*
     C/EXEC SQL
     C+ DECLARE C2  CURSOR FOR
     C+ SELECT SH$A06, SHDCAR,SH$A03, SH$A04, SH$A05
     C+ FROM        BASCOH WHERE SH$A06 > 0
     C+ AND SHFAAM>=200806  AND SHINDO= :OTINDO
     C+ AND
     C+ CAST('20'||SUBSTR(SHDCAR, 1, 6) AS DEC(8, 0)) <= :C1FALT  AND
     C+ :C1FALT  <   CAST('20'||SUBSTR(SHDCAR, 7, 6) AS DEC(8, 0))
     C/END-EXEC
     C*
     C                   endsr
     C*-------------------------------------------------------------------------
     C* UpdBascor: Actualiza Bascor
     C*-------------------------------------------------------------------------
     C     UpdBascor     BEGSR
     C*
     C                   Z-ADD     *ZERO         NROCER
     C* ... Esta marcado en Bascor ?
     C     KBASCOR04     CHAIN     BASCOR4                            99
     C                   If        *IN99 = *ON
     C                   Exsr      BuscaCer
     C                   Else
     C                   Exsr      EsCertVal
     C                   If        CerVal = *OFF
     C                   Exsr      BuscaCer
     C                   Endif
     C                   Endif
     C*
     C     KPRCRED       CHAIN     REPRCRED
     C                   If        CerVal =*ON
     C* ... ... Si, Marcar PRCRED con Nro de Certificado
     C                   Z-ADD     NROCER        JVISOL
     C                   Else
     C* ... No Existe Certificado Válido, informar
     C                   Z-ADD     *HIVAL        JVISOL
     C                   Endif
     C                   UPDATE    REPRCRED
     C                   EXSR      EscribeLog
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* EsCertVal: Valida si el certificado estaba vigente a la fec alta
     C*----------------------------------------------------------------
     C     EsCertVal     BEGSR
     C*
     C                   MOVE      *OFF          CERVAL            1
     C                   IF        SCDCAR=*BLANKS
     C                   MOVE      *ZEROS        SCDCAR
     C                   ENDIF
     C                   MOVE      SCDCAR        MUTLIN
     C                   Z-ADD     MUTFDE        @CFDH1            8 0
     C                   Z-ADD     MUTFHA        @CFDH2            8 0
     C                   ADD       20000000      @CFDH1
     C                   ADD       20000000      @CFDH2
     C                   IF        @CFDH1 >= C1FALT AND  C1FALT <= @CFDH2
     C                             AND SC$A06 > 0
     C                   MOVE      *ON           CERVAL
     c                   MOVE      SC$A06        NROCER
     C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* EscribeLog: ...
     C*----------------------------------------------------------------
     C     EscribeLog    BEGSR
     C*
     C                   EVAL      S1IJOB=@PJOBN
     C                   EVAL      S1$SAL=C1FALT
     C                   EVAL      S1$CUI=SCINDO
     C                   EVAL      S1ISUC=C1ISUC
     C                   EVAL      S1INCR=C1INCR
     C                   EVAL      S1IDEG=C1IDEG
     C                   EVAL      S1DF02=SCNYAP
     C                   EVAL      S1ISEQ=NROCER
     c                   EVAL      S1DF01=ERRTXT
     C                   EVAL      S1REFC=SCREFC
     C                   WRITE     REBASCTM
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* BuscaCer: Buscar un Certificado Válido
     C*-------------------------------------------------------------------------
     C     BuscaCer      BEGSR
     C*
     C                   MOVE      *OFF          CERVAL            1
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
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   EVAL      ErrTxt='No se pudo asociar un recibo  '
     C     otindo        CHAIN     BASCOR1                            99
     C                   DOW       *IN99 = *OFF
     C                   IF        SCDCAR=*BLANKS
     C                   MOVE      *ZEROS        SCDCAR
     C                   ENDIF
     C                   MOVE      SCDCAR        MUTLIN
     C                   Z-ADD     MUTFDE        @CFDH1            8 0
     C                   Z-ADD     MUTFHA        @CFDH2            8 0
     C                   ADD       20000000      @CFDH1
     C                   ADD       20000000      @CFDH2
     C                   IF        SCIBCF=ARIBCF AND SC$A06 > 0   AND
     C                             @CFDH1 >= C1FALT AND C1FALT <= @CFDH2
     C                   MOVE      C1ISUC        SCISUC
     C                   MOVE      C1INCR        SCINCR
     C                   MOVE      C1IDEG        SCIDEG
     C                   Z-ADD     C1FALT        SC$CUO
     C                   EVAL      ErrTxt=*BLANKS
     C                   MOVE      SC$A06        NROCER
     C                   MOVE      *ON           CERVAL            1
     C                   UPDATE    BASCOR1
     C                   LEAVESR
     C                   ENDIF
     C     OTINDO        READE     BASCOR1                                99
     C                   ENDDO
     C* ... Buscar en BASCOH
     C/EXEC SQL
     C+ OPEN C2 USING :OTINDO, :C1FALT, :C1FALT
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH C2 INTO :C2$A06, :C2DCAR, :C2$A03, :C2$A04, :C2$A05
     C/END-EXEC
     C                   IF        SQLCOD=*ZERO
     C                   MOVE      C2$A06        NROCER
     C                   MOVE      C2DCAR        SCDCAR
     C                   MOVE      C2$A03        SC$A03
     C                   MOVE      C2$A04        SC$A04
     C                   MOVE      C2$A05        SC$A05
     C                   MOVE      C2$A06        SC$A06
     C                   EVAL      ErrTxt=*BLANKS
     C                   MOVE      *ON           CERVAL            1
     C                   ENDIF
     C/EXEC SQL
     C+ CLOSE C2
     C/END-EXEC
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
     C+  JVININ,
     C+  JVFALT,
     C+  JVILEG
     C+ FROM
     C+  PRCRED
     C+ WHERE JVILCR=8 AND JVIESA='1'  AND JVFEPA=0
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
     C+ :C1ITIN, :C1ININ, :C1FALT, :C1ILEG
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
     C*
     C     *LIKE         DEFINE    SCISUC        C1ISUC
     C     *LIKE         DEFINE    SCINCR        C1INCR
     C     *LIKE         DEFINE    SCIDEG        C1IDEG
     C     *LIKE         DEFINE    OTICCL        C1ICCL
     C     *LIKE         DEFINE    ARITIN        C1ITIN
     C     *LIKE         DEFINE    ARININ        C1ININ
     C     *LIKE         DEFINE    SCFING        C1FALT
     C     *LIKE         DEFINE    JVILEG        C1ILEG
     C     *LIKE         DEFINE    SCDCAR        C2DCAR
     C     *LIKE         DEFINE    SC$A03        C2$A03
     C     *LIKE         DEFINE    SC$A04        C2$A04
     C     *LIKE         DEFINE    SC$A05        C2$A05
     C     *LIKE         DEFINE    SC$A06        C2$A06
     C     *LIKE         DEFINE    SC$A06        NROCER
     C     *LIKE         DEFINE    OTITTL        WWITTL
     C*
     C                   MOVE      *ZERO         C1ILEG            6 0
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
     C                   KFLD                    C1ISUC
     C                   KFLD                    C1INCR
     C                   KFLD                    C1IDEG
     C*
     C                   Z-ADD     1             WWITTL
     C                   MOVE      *BLANKS       ErrTxt           20
     C*   ... Borra registros anteriores si los hay
     C     @PJOBN        CHAIN     BASCTM                             99
+----C                   DOW       *IN99=*OFF
|    C                   DELETE    REBASCTM
|    C     @PJOBN        READE     BASCTM                                 99
+----C                   ENDDO
     C*   ... Blanquea campo SC$CUO del Archivo Bascor
     C     *LOVAL        SETLL     BASCOR1
     C                   READ      BASCOR1                                99
+----C                   DOW       *IN99=*OFF
     C                   Z-ADD     *ZERO         SC$CUO
     C                   UPDATE    BASCOR1
     C                   READ      BASCOR1                                99
+----C                   ENDDO
     C*
     C                   ENDSR
