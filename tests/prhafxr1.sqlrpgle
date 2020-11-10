*SRCMBRTXT:Ces.Hab.-Fix -Detect Rec. SobreAfec    
     FBASCOR04  IF   E           K DISK
     FPRHAT1    UF A E           K DISK
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D*@PJOBN                264    269S 0
     D*---------------------------------------------------------------------
     D Cmd             S           4096A
     D curper          S              6P 0
     D FlgIndo         S             15P 0
     D RecCount        S             15P 0
     C*---------------------------------------------------------------------
     c*
     C                   ExSr      CrearAfectados
     C                   ExSr      OPNAFE
     C                   ExSr      FETAFE
     C                   DoW       SQLCOD = *ZERO
     C                   ExSr      PRCRecibos
     C                   ExSr      PRCCreditos
     C                   ExSr      FETAFE
     C                   EndDo
     C                   ExSr      CLOAFE
     C*
     C                   ExSr      EndPgm
     C*---------------------------------------------------------------------
     C* *INZSR : Inicialización:
     C*---------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     KTEMP         KList
     C                   KFld                    @PJOBN
     C     KTEMP2        KList
     C                   KFld                    @PJOBN
     C                   KFld                    WWINDO
     C     KTEMP3        KList
     C                   KFld                    @PJOBN
     C                   KFld                    WWINDO
     C                   KFld                    RecCount
     C     KBASCOR04     KList
     C                   KFld                    WWISUC
     C                   KFld                    WWINCR
     C                   KFld                    WWIDEG
     C*
     C     *loval        setll     REPRHAT1                           25
     C                   Read      REPRHAT1                               25
     C                   DoW       *IN25 =   *OFF
     C                   Delete    REPRHAT1
     C                   Read      REPRHAT1                               25
     C                   EndDo
     C*
     C     *LIKE         DEFINE    WKINDO        WWINDO
     C     *LIKE         DEFINE    WKNYAP        WWNYAP
     C     *LIKE         DEFINE    WKNYAP        W1NYAP
     C     *LIKE         DEFINE    WKABCF        WWIBCF
     C     *LIKE         DEFINE    WKARED        WWIRED
     C     *LIKE         DEFINE    WKCSUC        WWRSUC
     C     *LIKE         DEFINE    WKCNCR        WWRNCR
     C     *LIKE         DEFINE    WKCDEG        WWRDEG
     C     *LIKE         DEFINE    WK$IMP        WW$IMP
     C     *LIKE         DEFINE    WK$IMP        WW$CUO
     C     *LIKE         DEFINE    WKISUC        WWISUC
     C     *LIKE         DEFINE    WKINCR        WWINCR
     C     *LIKE         DEFINE    WKIDEG        WWIDEG
     C     *LIKE         DEFINE    WK$IMP        WW$INP
     C*
     C                   Z-ADD     *ZERO         @PJOBN            6 0
     C*
     C/Exec SQL
     C+ commit
     C/End-Exec
     C*
     C/Exec SQL
     C+ DECLARE CRE CURSOR FOR
     C+ select
     C+     OTINDO,
     C+     JVISUC,
     C+     JVINCR,
     C+     JVIDEG,
     C+     JV$INP,
     C+     CUO
     C+ from qtemp/cred
     C+ where
     C+ otindo=:WWINDO
     C+ order by
     C+ jv$inp desc
     C/End-Exec
     C*
     C/Exec SQL
     C+ DECLARE REC CURSOR FOR
     C+   SELECT
     C+     SCINDO,
     C+     SCNYAP,
     C+     SCIBCF,
     C+     SCIRED,
     C+     SCISUC,
     C+     SCINCR,
     C+     SCIDEG,
     C+     SC$INP
     C+  FROM
     C+       BASCOR
     C+  WHERE
     C+        SCIEMP=1
     C+    AND SCINDO = :WWINDO AND SCDF05 <> 'S/RECIBO'
     C+  ORDER BY
     C+        SC$INP DESC
     C/End-Exec
     c*
     C/Exec SQL
     C+ DECLARE C1 CURSOR FOR
     C+  SELECT
     C+   *
     C+  FROM
     C+   QTEMP/AFECTADOS
     C/End-Exec
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* OPNAFE
     C*---------------------------------------------------------------------
     C     OPNAFE        BEGSR
     C*
     C/Exec SQL
     C+ OPEN C1
     C/End-Exec
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* CLOAFE
     C*---------------------------------------------------------------------
     C     CLOAFE        BEGSR
     C*
     C/Exec SQL
     C+ CLOSE C1
     C/End-Exec
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* PRCCreditos
     C*---------------------------------------------------------------------
     C     PRCCreditos   BEGSR
     C*
     C*
     C                   MOVE      *OFF          FLAG              1
     C                   ExSr      OPNCRE
     C                   Eval      RecCount=*ZERO
     C                   ExSr      FETCRE
     C                   DoW       SQLCOD =  *ZERO
     C                   Eval      RecCount=RecCount +1
     C     KTEMP3        CHAIN     REPRHAT1                           25
     C                   IF        *IN25 = *OFF
     C                   MOVE      WWISUC        WKCSUC
     C                   MOVE      WWINCR        WKCNCR
     C                   MOVE      WWIDEG        WKCDEG
     C                   MOVE      WW$IMP        WKCIMP
     C                   UPDATE    REPRHAT1
     C                   IF        WKCNCR <> WKANCR
     C                   MOVE      *ON           FLAG              1
     C                   ENDIF
     C                   ENDIF
     C                   ExSr      FETCRE
     C                   EndDo
     C                   ExSr      CLOCRE
     C*
     C                   IF        FLAG=*OFF
     C     KTEMP2        CHAIN     REPRHAT1                           25
     C                   DOW       *IN25 = *OFF
     C                   DELETE    REPRHAT1
     C     KTEMP2        READE     REPRHAT1                               25
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* PRCRecibos
     C*---------------------------------------------------------------------
     C     PRCRecibos    BEGSR
     C*
     C                   ExSr      OPNREC
     C                   Eval      RecCount=*ZERO
     C                   ExSr      FETREC
     C                   DoW       SQLCOD = *ZERO
     C                   Eval      RecCount=RecCount +1
     C                   MOVE      @PJOBN        WKIJOB
     C                   MOVE      WWINDO        WKINDO
     C                   MOVE      W1NYAP        WKNYAP
     C                   MOVE      RecCount      WKIRRN
     C                   MOVE      *ZERO         WKISUC
     C                   MOVE      *ZERO         WKINCR
     C                   MOVE      *ZERO         WKIDEG
     C                   MOVE      WW$INP        WK$IMP
     C                   MOVE      WWIBCF        WKABCF
     C                   MOVE      WWIRED        WKARED
     C                   MOVE      WWISUC        WKASUC
     C                   MOVE      WWINCR        WKANCR
     C                   MOVE      WWIDEG        WKADEG
     C                   MOVE      *ZERO         WKAIMP
     C                   MOVE      *ZERO         WKAAFC
     C                   MOVE      *BLANKS       WKCBCF
     C                   MOVE      *BLANKS       WKCRED
     C                   MOVE      *ZERO         WKCSUC
     C                   MOVE      *ZERO         WKCNCR
     C                   MOVE      *ZERO         WKCDEG
     C                   MOVE      *ZERO         WKCIMP
     C                   MOVE      *ZERO         WKCAFC
     C                   MOVE      *ZERO         WKACTL
     C                   Write     REPRHAT1
     C                   ExSr      FETREC
     C                   EndDo
     C                   ExSr      CLOREC
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* ENDPGM
     C*---------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* FETAFE   : Lee Entrada del Archivo de Afectados
     C*---------------------------------------------------------------------
     C     FETAFE        BEGSR
     C*
     C/Exec SQL
     C+ FETCH C1 INTO
     C+    :WWINDO
     C/End-Exec
     C*
     C                   ENDSR
     C*---------------------------------------------------------------------
     C* OPNCRE: Abrir Archivos de Creditos
     C*---------------------------------------------------------------------
     C     OPNCRE        BegSr
     C*
     C/Exec SQL
     C+ OPEN CRE USING :WWINDO
     C/End-Exec
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* FETCRE: Leer Archivos de Creditos
     C*---------------------------------------------------------------------
     C     FETCRE        BegSr
     C*
     C/Exec SQL
     C+ FETCH CRE INTO
     C+               :WWINDO,
     C+               :WWISUC,
     C+               :WWINCR,
     C+               :WWIDEG,
     C+               :WW$IMP,
     C+               :WW$CUO
     C/End-Exec
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* CLOCRE: Cerrar Archivos de Creditos
     C*---------------------------------------------------------------------
     C     CLOCRE        BegSr
     C*
     C/Exec SQL
     C+ CLOSE CRE
     C/End-Exec
     C*
     C                   EndSr
     C*
     C*---------------------------------------------------------------------
     C* OPNREC: Abrir Archivos de Reciboss
     C*---------------------------------------------------------------------
     C     OPNREC        BegSr
     C*
     C/Exec SQL
     C+ OPEN REC USING :WWINDO
     C/End-Exec
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* FETREC: Leer Archivos de Recibos
     C*---------------------------------------------------------------------
     C     FETREC        BegSr
     C*
     C/Exec SQL
     C+ FETCH REC INTO
     C+               :WWINDO,
     C+               :W1NYAP,
     C+               :WWIBCF,
     C+               :WWIRED,
     C+               :WWISUC,
     C+               :WWINCR,
     C+               :WWIDEG,
     C+               :WW$INP
     C/End-Exec
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* CLOREC: Cerrar Archivos de Recibos
     C*---------------------------------------------------------------------
     C     CLOREC        BegSr
     C*
     C/Exec SQL
     C+ CLOSE REC
     C/End-Exec
     C*
     C                   EndSr
     C*
     C*---------------------------------------------------------------------
     C* CrearAfectados: Crea Archivo de Afectados en QTEMP
     C*---------------------------------------------------------------------
     C     CrearAfectadosBegSr
     C*
     C/Exec SQL
     C+ SELECT MAX(MVFAAM) INTO :CURPER FROM PRHAMV WHERE MVIEMP=1
     C/End-Exec
     C*
      /Free
         Cmd='EXCSQLSTM SQLSTM(''SELECT scindo FROM bascor WHERE sciemp=1' +
                          ' AND SCDF05 <> "S/RECIBO" '+
                          'GROUP BY scindo HAVING count(*) > 1'')'+
                    ' OUTPUT(*OUTFILE) ' +
                    ' OUTFILE(QTEMP/AFECTADOS)';
      /End-Free
     C*
     C                   Call      'QCMDEXC'
     C                   Parm                    Cmd
     C                   Parm      4096          CmdLen           15 5
     C*
      /Free
        Cmd='EXCSQLSTM SQLSTM(''select                                     '+
            '  otindo, jvisuc, jvincr, jvideg, jv$inp,                     '+
            '  CAST((jv$inp/jvqcuo) AS DEC (15, 2)) CUO                    '+
            '  FROM            badccl inner join prcred on jvisuc=otisuc   '+
            ' and jviccl=oticcl                 where  jvfepa=0 and        '+
            '          otittl= 1    and                                    '+
            '          jviesa = "1" and jvinin in (select tbinin from      '+
            ' prhatb where tbiemp=1) order by  CUO            desc''       '+
            '                 )                                            '+
            '          OUTPUT(*OUTFILE)                                    '+
            '          OUTFILE(QTEMP/CRED)';
      /End-Free
     C*
     C                   Call      'QCMDEXC'
     C                   Parm                    Cmd
     C                   Parm      4096          CmdLen           15 5
     C*
     C                   EndSr
