*SRCMBRTXT:Switch-Adapter      -Movs C.A.         
     H
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: PRHA04R3                                       *
     H*                                                               *
     H*  PROGRAM NO: Genera Mov. en C. Ah.                            *
     H*                                                               *
     H*  DATE: 23/07/1993                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*----------------------------------------------------------------
     FACCTAC    UF   E           K DISK
     FANLIHA15  IF   E           K DISK
     FACMOVB07  UF   E           K DISK
     FSGSYSV    IF   E             DISK
     FANLIAP    IF   E           K DISK
     F*----------------------------------------------------------------
     c                   Z-Add     20            ImpMinParaComi   15 2
     c*
     C     WKEY01        CHAIN(n)  ACCTAC                             80
     C*    FUIGRC        IFEQ      '04'
     C*    FUISGC        ANDEQ     'AN'
     C*    PAIMCA        ANDEQ     48
     C*    FU$GDE        ANDGT     0
     C     FU$GDE        IFGT      0
     C                   EXSR      TRNCAC
     C                   ELSE
     C                   EXSR      TRNNOR
     C                   ENDIF
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* TRNCAC: TRANSACCION CON ACUERDO
     C*-------------------------------------------------------------------------
     C     TRNCAC        BEGSR
     C*
     C     PATIPT        IFEQ      'R'
     C                   EXSR      REVCAC
     C                   ELSE
     C                   EXSR      IMPCAC
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* IMPCAC: IMPACTO CON ACUERDO
     C*-------------------------------------------------------------------------
     C     IMPCAC        BEGSR
     C*
     C* ...Si hay saldo... es una transacción normal
     C     FU$SOP        IFGE      PA$IMP
     C                   EXSR      TRNNOR
     C                   EXSR      ENDPGM
     C                   ENDIF
     c* ... Determinar cuanto del acuerdo toma
     C     FU$SOP        ADD       FU$GDE        DINDIS           15 2
     C     DINDIS        SUB       PA$IMP        SALACU           15 2
     C* ...Ver si c/acuerdo no alcanza tampoco =>normal p/q rechaze normalmente
     C     SALACU        IFLT      *ZERO
     C                   EXSR      TRNNOR
     C                   EXSR      ENDPGM
     C                   ENDIF
     c* ... Actualizar $GDE en ACCTAC
     C     FU$GDE        SUB       SALACU        IMPACU           15 2
     C     WKEY01        CHAIN     ACCTAC                             80
     C                   Z-ADD     SALACU        FU$GDE
     C                   UPDATE    REACCTAC
     c* ... Meter Credito en la Cuenta
     C                   EXSR      IMPCRE
     c* ... Meter Codigo en la Bolsa
     C                   EXSR      IMPBOL
     c* ... Ahora Ejecutar Normalmente, pues ya hay saldo
     C                   EXSR      TRNNOR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* IMPCRE: Impactar el Acuerdo utilizado como Crédito en la Cta
     C*-------------------------------------------------------------------------
     C     IMPCRE        BEGSR
     c*
     C                   MOVE      'F'           WKTIPT            1
     C                   MOVE      *BLANKS       WKIATM           16
     C                   Z-ADD     267           WKIMCA            3 0
     C                   Z-ADD     PAISUC        WKISUC            5 0
     C                   Z-ADD     PAICAH        WKICAH           11 0
     C                   Z-ADD     PAICHE        WKICHE            7 0
     C                   Z-ADD     IMPACU        WK$IMP           15 2
     C                   MOVE      *BLANKS       WKIERR           40
     C                   MOVE      PAFASI        WKFASI            8 0
     C                   MOVE      PAHALT        WKHALT            6 0
     c*
     C                   CALL      'SWAD01R2'
     C                   PARM                    WKTIPT
     C                   PARM                    WKIATM
     C                   PARM                    WKIMCA
     C                   PARM                    WKISUC
     C                   PARM                    WKICAH
     C                   PARM                    WKICHE
     C                   PARM                    WK$IMP
     C                   PARM                    WKIERR           40
     C                   PARM                    WKFASI
     C                   PARM                    WKHALT            6 0
     c*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* IMPBOL: Efectua el impacto del Mov. En BOLSA
     C*-------------------------------------------------------------------------
     C     IMPBOL        BegSr
     C*
     C                   Z-ADD     WWICHE        WWIULN           15 0
     C                   MOVE      'CA'          WWSUBS            2
     C                   Z-ADD     268           WWIMCC            3 0
     C                   Z-ADD     PAISUC        WWISUC            5 0
     C                   Z-ADD     PAICAH        WWICCC           11 0
     C                   Z-ADD     PAICHE        WWICHE            7 0
     C                   Z-ADD     IMPACU        WW$IMP           15 2
     C                   Z-ADD     *ZERO         WWFACR            8 0
     C                   EXSR      GETFACR
     C*
     C                   CALL      'SWAD00R3'
     C                   PARM                    WWIULN
     C                   PARM                    WWSUBS
     C                   PARM                    WWIMCC
     C                   PARM                    WWISUC
     C                   PARM                    WWICCC
     C                   PARM                    WWICHE
     C                   PARM                    WW$IMP
     C                   PARM                    WWFACR
     C* ... Solo cobrar comisiones si se supera el importe mínimo.
     c                   If        IMPACU >= ImpMinParaComi
     c                   ExSr      GetImpComi
     c                   If        IMPCOMI > 0
     C*
     C                   Z-ADD     WWICHE        WWIULN           15 0
     C                   MOVE      'CA'          WWSUBS            2
     C                   Z-ADD     57            WWIMCC            3 0
     C                   Z-ADD     PAISUC        WWISUC            5 0
     C                   Z-ADD     PAICAH        WWICCC           11 0
     C                   Z-ADD     PAICHE        WWICHE            7 0
     C                   Z-ADD     IMPCOMI       WW$IMP           15 2
     C                   Z-ADD     *ZERO         WWFACR            8 0
     C                   EXSR      GETFACR
     C*
     C                   CALL      'SWAD00R3'
     C                   PARM                    WWIULN
     C                   PARM                    WWSUBS
     C                   PARM                    WWIMCC
     C                   PARM                    WWISUC
     C                   PARM                    WWICCC
     C                   PARM                    WWICHE
     C                   PARM                    WW$IMP
     C                   PARM                    WWFACR
     C* ... Iva de la comisión
     C                   CALL      'AC0203R7'
     C                   PARM      *Zero         PAIDAC            3 0
     C                   PARM                    WWISUC
     C                   PARM                    WWICCC
     C                   PARM                    WWFACR
     C                   PARM                    WWIMCC
     C                   PARM                    WW$IMP
     C                   PARM                    WWICHE
     C                   PARM      *Blanks       PAIAYN           15
     C*
     C                   EndIf
     C                   EndIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* GetImpComi: Obtener el importe de la comisión
     C*-------------------------------------------------------------------------
     C     GetImpComi    BEGSR
     c*
     C                   Z-Add     *ZEROS        IMPCOMI          15 2
     C     KAP001        Chain     REANLIAP                           99
     C                   DoW       *IN99 = *Off
     C                   If            IMPACU >= AP$MIN
     C                             and IMPACU <  AP$MAX
     C                   Z-Add     AP$IMP        IMPCOMI
     C                   Leave
     C                   EndIf
     C     KAP001        ReadE     REANLIAP                               99
     C                   EndDo
     c*
     c                   ENDSR
     C*-------------------------------------------------------------------------
     C* GETFACR: OBTNER FECHA DE ACRED. PROXIMO PAGO DE ANSES
     C*-------------------------------------------------------------------------
     C     GETFACR       BEGSR
     C* ... Buscar por Cuenta y Período
     C                   MOVEL     AASFEI        PERIODO           6 0
     C                   Z-ADD     *ZERO         WWFACR
     C     KAN150        CHAIN     REANLIHA                           99
     C                   DOW       *IN99=*OFF
     C                   IF        ANFEPR=PERIODO
     C                   Z-ADD     ANIFPP        WWFACR
     c                   CALL      'SBBAINFE'
     C                   PARM                    WWFACR
     C                   PARM      'NI'          MODE              2
     C                   LEAVE
     C                   ENDIF
     C     KAN150        READE     REANLIHA                               99
     C                   ENDDO
     c* ... Si no encontré... calcular 30 día hacía adelante
     C                   IF        WWFACR = *ZERO
     C                   Z-ADD     PAFASI        PAFECH            8 0
     C                   CALL      'SBBAINFE'
     C                   PARM                    PAFECH
     C                   PARM      'IN'          PACINV            2
     C*
     C                   Z-ADD     *ZERO         PADIAS           15 0
     C                   MOVE      *BLANKS       PAIERS            1
     C                   CALL      'SBBACFEC'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PAIERR
     C*
     C                   ADD       30            PADIAS
     C*
     C                   Z-ADD     *ZERO         PAFECH
     C                   MOVE      *BLANKS       PAIERS            1
     C                   CALL      'SBBACFEC'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PAIERR
     C                   CALL      'SBBAINFE'
     C                   PARM                    PAFECH
     C                   PARM      'NI'          PACINV            2
     C*
     C                   Z-ADD     PAFECH        WWFACR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* REVCAC: REVERSA CON ACUERDO
     C*-------------------------------------------------------------------------
     C     REVCAC        BEGSR
     C*
     C* ... Contrasentar Acreditacion normalmente
     C                   EXSR      TRNNOR
     c* ... Tiene Mov en Bolsa? Eliminarlo y dejar Bandera
     c                   Move      *Off          HayBolsa          1
     c                   EXSR      BUSBOL
     C                   IF        HayBolsa=*On
     C* ... ... Contrasentar Acreditación de Acuerdo
     C                   EXSR      RevCreAcu
     C* ... ... Eliminar Comisión e IVA
     c                   ExSr      DECOIV
     c* ... ... Devolver saldo a FU$GDE
     C     WKEY01        CHAIN     ACCTAC                             80
     C                   Add       IMPACU        FU$GDE
     C                   UPDATE    REACCTAC
     C*
     c                   Endif
     C                   ENDSR
     c*-------------------------------------------------------------------------
     C*    BUSBOL: Busca 268 en Bolsa
     c*-------------------------------------------------------------------------
     C     BUSBOL        BEGSR
     C*
     C                   Z-ADD     268           WKIMCA            3 0
     C     KIN070        CHAIN     REACMOVB                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        PAICHE=INICHE
     c                   Move      *On           HayBolsa
     C                   Z-ADD     IN$IMP        IMPACU
     C                   DELETE    REACMOVB
     C                   LEAVE
     C                   ENDIF
     C     KIN070        READE     REACMOVB                               99
     C                   ENDDO
     C*
     C                   ENDSR
     c*-------------------------------------------------------------------------
     C*    DECOIV : Elimina Comisión e IVA de la bolsa
     c*-------------------------------------------------------------------------
     C     DECOIV        BEGSR
     C*
     C                   Z-ADD     57            WKIMCA            3 0
     C     KIN070        CHAIN     REACMOVB                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        PAICHE=INICHE
     C                   DELETE    REACMOVB
     C                   LEAVE
     C                   ENDIF
     C     KIN070        READE     REACMOVB                               99
     C                   ENDDO
     C*
     C                   Z-ADD     21            WKIMCA            3 0
     C     KIN070        CHAIN     REACMOVB                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        PAICHE=INICHE
     C                   DELETE    REACMOVB
     C                   LEAVE
     C                   ENDIF
     C     KIN070        READE     REACMOVB                               99
     C                   ENDDO
     c*
     C                   ENDSR
     c*-------------------------------------------------------------------------
     c* RevCreAcu=Reversa Crédito del Acuerdo
     c*-------------------------------------------------------------------------
     C     RevCreAcu     BEGSR
     c*
     C                   MOVE      'R'           WKTIPT            1
     C                   MOVE      *BLANKS       WKIATM           16
     C                   Z-ADD     267           WKIMCA            3 0
     C                   Z-ADD     PAISUC        WKISUC            5 0
     C                   Z-ADD     PAICAH        WKICAH           11 0
     C                   Z-ADD     PAICHE        WKICHE            7 0
     C                   Z-ADD     IMPACU        WK$IMP           15 2
     C                   MOVE      *BLANKS       WKIERR           40
     C                   MOVE      PAFASI        WKFASI            8 0
     C                   MOVE      PAHALT        WKHALT            6 0
     c*
     C                   CALL      'SWAD01R2'
     C                   PARM                    WKTIPT
     C                   PARM                    WKIATM
     C                   PARM                    WKIMCA
     C                   PARM                    WKISUC
     C                   PARM                    WKICAH
     C                   PARM                    WKICHE
     C                   PARM                    WK$IMP
     C                   PARM                    WKIERR           40
     C                   PARM                    WKFASI
     C                   PARM                    WKHALT            6 0
     c*
     c                   ENDSR
     C*-------------------------------------------------------------------------
     C* TRNNOR: TRANSACCION NORMAL
     C*-------------------------------------------------------------------------
     C     TRNNOR        BEGSR
     C*
     C                   CALL      'SWAD01R2'
     C                   PARM                    PATIPT            1
     C                   PARM                    PAIATM           16
     C                   PARM                    PAIMCA
     C                   PARM                    PAISUC
     C                   PARM                    PAICAH           11 0
     C                   PARM                    PAICHE
     C                   PARM                    PA$IMP
     C                   PARM                    PAIERR           40
     C                   PARM                    PAFASI
     C                   PARM                    PAHALT            6 0
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ENDPGM: Finaliza el Programa
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* *INZSR : Subrutina de inicialización
     C*----------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PATIPT            1
     C                   PARM                    PAIATM           16
     C                   PARM                    PAIMCA            3 0
     C                   PARM                    PAISUC            5 0
     C                   PARM                    PAICAH           11 0
     C                   PARM                    PAICHE            7 0
     C                   PARM                    PA$IMP           15 2
     C                   PARM                    PAIERR           40
     C                   PARM                    PAFASI            8 0
     C                   PARM                    PAHALT            6 0
     C*
     C     WKEY01        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICAH
     C     KAN150        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICAH
     C     KIN070        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICAH
     C                   KFLD                    FUIMON
     C                   KFLD                    WKIMCA
     C     KAP001        KLIST
     C                   KFLD                    WWIEMP
     C*
     C     *LIKE         DEFINE    APIEMP        WWIEMP
     C*
     C                   Z-ADD     2             WWIEMP
     C*
     C     1             CHAIN     RESGSYSV
     C*
     C                   ENDSR
