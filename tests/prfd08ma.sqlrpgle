*SRCMBRTXT:Aut.Federal-Manejador Carga PRAFED c/Op
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA - PRESTAMOS.                             *
     H*                                                               *
     H*  PROGRAM NAME: NUEVO DISEÑO DE ARCHIVO P/FEDERAL.             *
     H*                                                               *
     H*  PROGRAM NO: prfd08ma                                         *
     H*                                                               *
     H*  DATE:   12/12/2012                                           *
     H*                                                               *
     H*  AUTHOR: SANTIAGO OTTONELLO                                   *
     H*****************************************************************
     F@CPISYS   UF A E           K DISK
     F@CPIUSD   UF A E           K DISK
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     DERRDS            DS
     D ERRTXT                  1    385
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*----------------------------------------------------------------*
     c                   Call      'BAFECHSV'
     C                   ExSr      ReadParams
     c                   If        @FN(03)=*OFF AND @FN(12) = *OFF
     c                   ExSr      CheckExistence
     c*                  Call      'PRFD05MA'
     c                   ExSr      OpenC1
     c                   ExSr      FetchC1
     c                   Z-Add     *ZERO         Count            15 0
     C                   DoW       SQLCOD = *ZERO
     c                   Add       1             Count
     c                   If        WWFEPA = 0
     C                   ExSr      InsAut
     C                   EndIf
     c                   ExSr      FetchC1
     C                   EndDo
     c                   ExSr      CloseC1
     c                   Eval      ErrTxt='Se Agregaron:'+%EDITC(count:'Z')+
     C                                    ' Pedidos de Autorización'
     c                   ExSr      DspErr
     c                   Else
     c                   Eval      ErrTxt='Se Cancelo el proceso'
     c                   ExSr      DspErr
     c                   EndIf
     c                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     c* InsAut: Insertar Pedido de Autorización
     c*-------------------------------------------------------------------------
     c     InsAut        BegSr
     c*
     C     @PJOBN        Chain     @CPIUSD                            80
     c                   Z-Add     WWISUC        @CISUC
     c                   Z-Add     WWINCR        @CINCR
     c                   Z-Add     WWIDEG        @CIDEG
     c                   Z-Add     1             @CIEMP
     c                   Move      WWREFC        @CREFC
     c                   ExSr      BuildABAN
     c                   Move      WWABAN        @CABAN
     C                   Update    @CPIUSRR
     c                   Move      *Blanks       PAERRO            1
     c                   Move      *on           PABATC            1
     c                   Call      'PRFD08RG'
     C                   Parm                    PAERRO
     C                   Parm                    PABATC
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* BuildABAN: Busca Credito que esta refinanciandose
     C*-------------------------------------------------------------------------
     C     BuildABAN     BegSr
     C*
     c                   Move      *Blanks       WWABAN
     C* ... Calcular Período Anterior
     c                   MoveL     @CFING        WWPERI            6 0
     c*                  MoveL     WWPERI        WWANIO            4 0
     c*                  Move      WWPERI        WWMES             2 0
     c*                  SUB       1             WWMES
     C*                  If        WWMES <= 0
     C*                  Z-ADD     1             WWMES
     c*                  SUB       1             WWANIO
     C*                  EndIf
     C*                  MoveL     WWANIO        WWPERI
     c*                  Move      WWMES         WWPERI
     C*
     C     *LIKE         DEFINE    @CISUC        WKISUC
     C     *LIKE         DEFINE    @CINCR        WKINCR
     C     *LIKE         DEFINE    @CIDEG        WKIDEG
     C*
     C                   IF        WWIRED='0'
     C                   MOVE      '1'           WWIRED
     C                   ENDIF
     C*
     C/EXEC SQL
     C+ SELECT
     C+        MVISUC,
     C+        MVINCR,
     C+        MVIDEG
     C+ INTO
     C+        :WKISUC,
     C+        :WKINCR,
     C+        :WKIDEG
     C+ FROM SDBFIL/PRHAMV
     C+ WHERE
     C+   MVFAAM =  :WWPERI AND
     C+   MVINDO =  :WWINDO AND
     C+   MVIBCF =  :WWIBCF AND
     C+   MVIRED =  :WWIRED
     C/END-EXEC
     C*
     C                   IF        SQLCOD = 0
     C                   MOVE      WKISUC        KKISUC            2
     C                   MOVE      WKINCR        KKINCR           15
     C                   MOVE      WKIDEG        KKIDEG            1
     C     KKISUC        CAT       KKINCR:1      WWABAN
     C     WWABAN        CAT       KKIDEG:1      WWABAN
     c                   ENDIF
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* CheckExistence: Ver si ya hay autorizaciones para la fecha
     C*-------------------------------------------------------------------------
     C     CheckExistenceBegSr
     c                   Z-Add     *Zero         Count            15 0
     c/EXEC SQL
     c+ SELECT COUNT(*) INTO :Count FROM PRAFED WHERE AFFECH = :@CFING
     C/END-EXEC
     C*
     c                   If        Count > 0
     c                   Eval      ErrTxt='Ya Hay Autorizaciones para la fecha'+
     C                             ' F10=Procesar de Cualquier Manera'
     c                   ExSr      DspErr
     C                   ExSr      ReadParams
     C                   If        @FN(10) <> *ON
     c                   Eval      ErrTxt='Se Cancelo el proceso'
     c                   ExSr      DspErr
     C                   ExSr      EndPgm
     c                   EndIf
     C                   EndIf
     c/EXEC SQL
     c+ DELETE FROM PRAFED WHERE AFFECH = :@CFING
     C/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM SDBFIL/PRMFED WHERE MCFECH = :@CFING
     C/END-EXEC
     c/EXEC SQL
     C+ DELETE FROM SDBFIL/PRMOVI60 WHERE M2FASI = :@CFING
     C/END-EXEC
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *inzsr: Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     c     *LIKE         DEFINE    @CISUC        WWISUC
     c     *LIKE         DEFINE    @CINCR        WWINCR
     c     *LIKE         DEFINE    @CIDEG        WWIDEG
     c     *LIKE         DEFINE    @CREFC        WWREFC
     c     *LIKE         DEFINE    @CABAN        WWABAN
     c     *LIKE         DEFINE    @CINDO        WWINDO
     c     *LIKE         DEFINE    @CIBCF        WWIBCF
     c     *LIKE         DEFINE    @CFEPA        WWFEPA
     c                   Move      *Blanks       WWIRED            1
     c                   Move      *Blanks       WWISEX            1
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* OpenC1: Abrir Cursor
     C*-------------------------------------------------------------------------
     C     OpenC1        BEGSR
     c*
     c/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT JVISUC, JVINCR, JVIDEG, SCREFC,
     C+ SCINDO, SCIBCF, SCIRED, SCISEX, JVFEPA
     C+ FROM SDBFIL/PRCRED
     C+ LEFT JOIN
     C+   SDBFIL/BADCCL ON JVISUC=OTISUC AND JVICCL=OTICCL AND OTITTL=1
     C+ LEFT JOIN
     C+   SDBFIL/BASCOR ON  SCINDO=OTINDO
     C+ INNER JOIN PRHATB ON JVITIN=TBITIN AND JVININ=TBININ AND TBIEMP IN
     C+ (1, 18)
     C+  WHERE ( JVFASI = :@CFING)   AND
     C+       (JVISUC=0    AND JVINCR=          137161 )
     C+  OR   (JVISUC=3    AND JVINCR=           23226 )
     C+  OR   (JVISUC=3    AND JVINCR=           25131 )
     C+  OR   (JVISUC=5    AND JVINCR=            6733 )
     C+  OR   (JVISUC=0    AND JVINCR=          138383 )
     C+  OR   (JVISUC=0    AND JVINCR=          138383 )
     C+  OR   (JVISUC=0    AND JVINCR=          148923 )
     C+  OR   (JVISUC=0    AND JVINCR=          135578 )
     C+  OR   (JVISUC=0    AND JVINCR=          135578 )
     C+  OR   (JVISUC=0    AND JVINCR=          148925 )
     C+  OR   (JVISUC=3    AND JVINCR=           22585 )
     C/END-EXEC
     C*
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* CloseC1: Cerrar Cursor
     C*-------------------------------------------------------------------------
     C     CloseC1       BEGSR
     c*
     c/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* FetchC1: Leer C1
     C*-------------------------------------------------------------------------
     C     FetchC1       BEGSR
     c*
     c/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :WWISUC, :WWINCR, :WWIDEG, :WWREFC,
     C+ :WWINDO, :WWIBCF, :WWIRED, :WWISEX, :WWFEPA
     C/END-EXEC
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* DspErr: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DspErr        BEGSR
     C*
     C                   CALL      'BAER01RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ReadParms:Leer Parámetros
     C*---------------------------------------------------------------------
     C     ReadParams    BegSr
     C*
     C* Lee registro de Usuario
     C     @PJOBN        Chain(N)  @CPIUSD                            80
     C   80              Z-Add     @PJOBN        @ZJOBN
     C   80              Write     @CPIUSRR
     C* Lee registro de sistema
     C     @PJOBN        Chain(N)  @CPISYS                            90
     C   90              Z-Add     @PJOBN        @ZJOBN
     C   90              Write     @CPISYSR
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
