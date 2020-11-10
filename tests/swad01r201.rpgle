*SRCMBRTXT:Switch-Adapter      -Movs C.A.         
     H
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: PRHA01R2                                       *
     H*                                                               *
     H*  PROGRAM NO: Genera Mov. en C. Ah.                            *
     H*                                                               *
     H*  DATE: 23/07/1993                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     F*----------------------------------------------------------------
     FSGSYSV    IF   E             DISK
     FACCODI    IF   E           K DISK
     FACCTAC    IF   E           K DISK
     FACBLOQ    IF   E           K DISK
     FACSUCU    IF   E           K DISK
     FBASUCU    IF   E           K DISK
     FACMOVB08  UF   E           K DISK
     FACPGMS    IF   E           K DISK
     FACMOVD03  UF   E           K DISK
     FBAEXEP01  IF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     F*----------------------------------------------------------------
     D @MSG            S             40    DIM(16) CTDATA PERRCD(1)
     D*----------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
     I*  Program status data structure.
     I*----------------------------------------------------------------*
     C*
     C                   EXSR      SAVATM
     C*
+----C     PATIPT        IFEQ      'R'
|    C                   EXSR      REVMOV
+----C                   ELSE
|    C                   EXSR      INGMOV
+----C                   ENDIF
     C*
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* SAVATM: Salva ATM en CPI para uso de programa HOST
     C*-------------------------------------------------------------------------
     C     SAVATM        BEGSR
     C*
     C* ...  Inicializa registro de CPI
     C                   CALL      '@CPIPGSD'
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            99
     C  N99              MOVEL     PAIATM        @CITRL
     C  N99              UPDATE    @CPIUSRR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* REVMOV: REVERSA DE MOVIMIENTOS
     C*-------------------------------------------------------------------------
     C     REVMOV        BEGSR
     C*
     C                   EXSR      CAFASI
     C*
     C     PAIMCA        IFEQ      558
     C     PAIMCA        OREQ      8
     C                   Z-ADD     *ZERO         WWISAL
     C                   ELSE
     C                   Z-ADD     PAISUC        WWISAL
     C                   ENDIF
     C*
     C     WKEY04        CHAIN(N)  REACMOVD                           99
+----C     *IN99         IFEQ      *OFF
|    C*
|+---C     GCIASC        IFEQ      0
||   C                   EXSR      VALIDA
||   C                   EXSR      ASOREV
|+---C     WWERRO        IFNE      *BLANKS
||   C                   MOVEL(P)  WWERRO        PAIERR
||   C                   MOVEL     '201'         PAIERR
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
|+---C                   ELSE
||   C                   MOVE(P)   *ZEROS        PAIERR
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
+----C                   ELSE
     C     PAIMCA        IFEQ      48
     C                   MOVE      56            WKIMCA
     C                   ELSE
     C                   MOVE      PAIMCA        WKIMCA
     C                   ENDIF
|    C* ... Buscar si el mov. fue a Bolsa
|    C     KAN080        CHAIN     REACMOVB                           99
|+---C     *IN99         IFEQ      *OFF
||   C                   DELETE    REACMOVB
||   C                   MOVE(P)   @MSG(15)      PAIERR
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
|    C* ... Mov. No encontrado para la Reversa
|    C                   MOVE(P)   @MSG(13)      PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* INGMOV: INGRESO DE MOVIMIENTOS
     C*-------------------------------------------------------------------------
     C     INGMOV        BEGSR
     C*  .........Valida la cuenta
     C                   EXSR      VALIDA
     C*
     C                   EXSR      CAFASI
     C*
     C*  ........Impacta con Códigos Asociados
     C                   EXSR      ASOIMP
 +---C     WWERRO        IFNE      *BLANKS
 |   C                   MOVEL(P)  WWERRO        PAIERR
 |   C                   MOVEL     '201'         PAIERR
 |   C                   EXSR      ENDPGM
 +---C                   ENDIF
     C*  ........Actualiza Sucursal de Alta y Generación de Asientos
     C                   EXSR      ACTSAL
     C*
     C                   ENDSR
     C*
     C*----------------------------------------------------------------
     C* ACTSAL: Actualiza Sucursal de Alta y Generación de Asientos
     C*----------------------------------------------------------------
     C     ACTSAL        BEGSR
     C*
     C     FXIMCA        IFEQ      558
     C     FXIMCA        OREQ      8
     C                   Z-ADD     *ZERO         WWISAL
     C                   ELSE
     C                   Z-ADD     FUISUC        WWISAL
     C                   ENDIF
     C*
     C     WKEY14        CHAIN     REACMOVD                           90
     C     *IN90         DOWEQ     *OFF
     C*
     C     GCICHE        IFEQ      PAICHE
     C                   MOVE      'S'           GCIGAS
     C                   Z-ADD     WWISAL        GCISAL
     C                   UPDATE    REACMOVD
     C                   ENDIF
     C*
     C     WKEY14        READE     REACMOVD                               90
     C                   ENDDO
     C*
     C                   ENDSR
     C*--------------------------------------------------------------*
     C* VALIDA: Valida la cuenta
     C*--------------------------------------------------------------*
     C     VALIDA        BEGSR
     c*
     C*   .......Testea la existencia y vigencia de la cuenta
     C     WKEY01        CHAIN     ACCTAC                             80
+----C     *IN80         IFEQ      *ON
|    C                   MOVE      @MSG(7)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*   .......Blanquea error
     C                   MOVE      *BLANK        PAIERR
     C*   .......Verifica si el Sub-Sistema esta Cerrado
     C                   EXSR      CHKSBS
     C*   .......Características de código de movimiento
     C     PAIMCA        CHAIN     ACCODI                             80
+----C     *IN80         IFEQ      *ON
|    C                   MOVE      @MSG(5)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*   .......Válida Sucursal
     C                   EXSR      VALSUC
     C*   .......Testea la existencia y vigencia de la cuenta
+----C     FUFBAJ        IFNE      *ZERO
|    C                   MOVE      @MSG(1)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
+----C     FUISGC        IFEQ      'IN'
|    C                   MOVE      @MSG(1)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
+----C     FUIUCA        IFNE      'S'
|    C                   MOVE(P)   @MSG(7)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*   .......Compara saldo de la cuenta si tipo mov=1 (Extracciones)
+----C     FXITMO        IFEQ      1
|    C     PATIPT        ANDNE     'R'
|    C     FU$SOP        ANDLT     PA$IMP
|    C                   MOVE      @MSG(6)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*  ........Valida bloqueo operaciones de débito
     C                   EXSR      VABLOQ
+----C     PAIERR        IFNE      *BLANKS
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*  ........Valida excepciones
     C                   EXSR      VALEXE
+----C     PAIERR        IFNE      *BLANKS
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*  ........Valida depositos en cuentas grupo 5
     C     FUICAH        IFGE      52500000
     C     FUICAH        ANDLE     52599999
     C     PAIMCA        ANDEQ     208
     C                   MOVE      @MSG(4)       PAIERR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       PAERRO
     C                   CALL      'ANBA00R1'
     C                   PARM                    PAISUC
     C                   PARM                    PAICCL
     C                   PARM                    PAFASI
     C                   PARM                    PAERRO            1
     C
     C     PAERRO        IFEQ      '1'
     C                   MOVE      @MSG(6)       PAIERR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   ENDSR
     C*--------------------------------------------------------------*
     C* ASOIMP: Impacta Códigos Asociados
     C*--------------------------------------------------------------*
     C     ASOIMP        BEGSR
     C*
     C                   Z-ADD     203           PAIDAC            3 0
     C                   MOVE      *BLANKS       WWERRO           40
     C*
     C                   CALL      'AC0223R4'
     C                   PARM                    PAIDAC
     C                   PARM                    PAISUC
     C                   PARM                    FUIMON
     C                   PARM                    PAICAH
     C                   PARM                    PAIMCA
     C                   PARM                    PA$IMP
     C                   PARM                    PAICHE
     C                   PARM                    WWICAJ
     C                   PARM                    PAHALT
     C                   PARM                    PAFASI
     C                   PARM                    WWERRO
     C*
     C                   ENDSR
     C*--------------------------------------------------------------*
     C* ASOREV: Reversa Códigos Asociados                            *
     C*--------------------------------------------------------------*
     C     ASOREV        BEGSR
     C*
     C                   MOVE      *BLANKS       WWERR1           70
     C*
     C                   CALL      'AC0223R6'
     C                   PARM                    PAISUC
     C                   PARM                    FUIMON
     C                   PARM                    PAICAH
     C                   PARM                    PAIMCA
     C                   PARM                    PA$IMP
     C                   PARM                    PAICHE
     C                   PARM                    WWICAJ
     C                   PARM                    PAHALT
     C                   PARM                    WWERR1
     C*
     C                   MOVEL     WWERR1        WWERRO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CAFASI: Calcula Fecha de Asiento
     C*-------------------------------------------------------------------------
     C     CAFASI        BEGSR
     C*
     C     PAIMCA        CHAIN     ACCODI                             80
+----C                   SELECT
+----C     FXISAF        WHENEQ    0
|    C                   Z-ADD     AASFEI        GCFASI
+----C     FXISAF        WHENEQ    1
|    C                   Z-ADD     AAFS24        GCFASI
+----C     FXISAF        WHENEQ    2
|    C                   Z-ADD     AAFS48        GCFASI
+----C     FXISAF        WHENEQ    3
|    C                   Z-ADD     AAFS72        GCFASI
+----C                   ENDSL
     C*
     C                   Z-ADD     GCFASI        PAFASI
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VABLOQ: Valida Bloqueos en la cuenta.
     C*-------------------------------------------------------------------------
     C     VABLOQ        BEGSR
     C*
     C     FUIBAC        CHAIN     ACBLOQ                             99
     C*
     C*  ........Valida bloqueo operaciones de débito
+----C     FXITMO        IFEQ      1
|    C     FVIBDB        ANDNE     *ZERO
|+---C     FU$SBL        IFEQ      0
||   C                   MOVEL     @MSG(3)       PAIERR
||   C                   EXSR      ENDPGM
|+---C                   ELSE
||   C     FU$SOP        SUB       PA$IMP        WW$$SA           15 2
||+--C     WW$$SA        IFLT      FU$SBL
|||  C                   MOVEL     @MSG(3)       PAIERR
|||  C                   EXSR      ENDPGM
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDIF
     C*  ........Valida bloqueo operaciones de crédito
+----C     FXITMO        IFEQ      2
|    C     FVIBCR        ANDNE     0
|+---C     FU$SBL        IFEQ      0
||   C                   MOVEL     @MSG(4)       PAIERR
||   C                   EXSR      ENDPGM
|+---C                   ELSE
||   C     FU$SOP        ADD       PA$IMP        WW$$SA
||+--C     FU$SBL        IFLT      WW$$SA
|||  C                   MOVEL     @MSG(4)       PAIERR
|||  C                   EXSR      ENDPGM
||+--C                   ELSE
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VALEXE: Valida excepciones
     C*-------------------------------------------------------------------------
     C     VALEXE        BEGSR
     C*
     C                   MOVEL     'AC'          WWISUB
     C     @KEY07        CHAIN     BAEXEP01                           99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     @MSG(4)       PAIERR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*-------------------------------------------------------------------------
     C* VALSUC: Busca datos de sucursal
     C*-------------------------------------------------------------------------
     C     VALSUC        BEGSR
     C*
     C                   Z-ADD     PAISUC        AVISUC
     C*   .......Valida Sucursal Habilitada para operar Ctas. Ctes
     C     WKEY02        CHAIN     ACSUCU                             80
+----C     *IN80         IFEQ      *ON
|    C                   MOVEL(P)  @MSG(11)      PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C     WKEY02        CHAIN     BASUCU                             80
+----C     *IN80         IFEQ      *ON
|    C                   MOVEL(P)  @MSG(10)      PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*
     C     ENDSUC        ENDSR
     C*-------------------------------------------------------------------------
     C* CHKSBS: Verifica que el Sub-Sistema Este cerrado
     C*-------------------------------------------------------------------------
     C     CHKSBS        BEGSR
     C*
     C                   MOVEL     'ACPD01RG'    LBIPGM
     C*   .......Controla si el Subsistema de Caja no se cerro aún...
     C     LBIPGM        CHAIN     ACPGMS                             80
+----C     *IN80         IFEQ      *OFF
|    C                   MOVEL     @MSG(8)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C* *INZSR: Subrutina de inicialización
     C*----------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PATIPT            1
     C                   PARM                    PAIATM           16
     C                   PARM                    PAIMCA
     C                   PARM                    PAISUC
     C                   PARM                    WWICAH           11 0
     C                   PARM                    PAICHE
     C                   PARM                    PA$IMP
     C                   PARM                    PAIERR           40
     C                   PARM                    PAFASI
     C                   PARM                    PAHALT            6 0
     C*
     C     *LIKE         DEFINE    FXIMCA        PAIMCA
     C     *LIKE         DEFINE    FXIMCA        WKIMCA
     C     *LIKE         DEFINE    FUISUC        PAISUC
     C     *LIKE         DEFINE    FUISUC        WWISAL
     C     *LIKE         DEFINE    FUICAH        PAICAH
     C     *LIKE         DEFINE    GC$IMP        PA$IMP
     C     *LIKE         DEFINE    GCFASI        PAFASI
     C     *LIKE         DEFINE    GCICHE        PAICHE
     C     *LIKE         DEFINE    GCICAJ        WWICAJ
     C     *LIKE         DEFINE    EXISUB        WWISUB
     C     *LIKE         DEFINE    FUICCL        PAICCL
     C*
     C                   Z-ADD     WWICAH        PAICAH
     C*          *LIKE     DEFN FXISAF    WWISAF
     C*
     C                   Z-ADD     998           WWICAJ
     C*
     C     WKEY01        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICAH
     C     WKEY02        KLIST
     C                   KFLD                    PAISUC
     C     WKEY03        KLIST
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICAH
     C                   KFLD                    PAFASI
     C     WKEY04        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    WWICAH
     C                   KFLD                    AASFEI
     C                   KFLD                    PAHALT
     C                   KFLD                    PAIMCA
     C                   KFLD                    WWISAL
     C                   KFLD                    WWICAJ
     C                   KFLD                    PA$IMP
     C                   KFLD                    PAFASI
     C     WKEY14        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    WWICAH
     C                   KFLD                    AASFEI
     C                   KFLD                    PAHALT
     C     KAN080        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    WWICAH
     C                   KFLD                    WKIMCA
     C                   KFLD                    PA$IMP
     C                   KFLD                    PAICHE
     C     @KEY07        KLIST
     C                   KFLD                    WWISUB
     C                   KFLD                    PAISUC
     C                   KFLD                    WWICAH
     C                   KFLD                    PAIMCA
     C*
     C*   .......Busca datos de sistema
     C     1             CHAIN     SGSYSV                             80
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
     c*-------------------------------------------------------------------------
     c* *PSSR: Recuperación de errores
     c*-------------------------------------------------------------------------
     C     *PSSR         BEGSR
     c                   Move      *Blanks       CmdStr          255
     c                   Eval      CmdStr='SNDMSG MSG('''+
     c                             'SWAD01R2 Se Cancela pues Recibio:'+
     c                             @PEXEP +
     c                             @PEXNO +
     c                             ''') TOUSR(*SYSOPR)'
     c                   Call      'QCMDEXC'
     C                   Parm                    CmdStr          255
     c                   Parm      255           CmdLen           15 5
     c                   Eval      PAIERR='208Ya fue ejecutado el cierre de ca'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
**
201La caja de ahorro está dada de baja
202La caja de ahorro no existe
203La cuenta está bloqueada para debitos
204La cuenta está bloqueada para créditos
205El código de movimiento no esta definido
206El saldo de la cuenta es insuficiente
207La cuenta no se encuentra definida
208Ya fue ejecutado el cierre de caja ah.
209Saldos históricos insuficientes
210Suc.no definida
211Suc.no habilitada CC
212Sucursal en Feriado
213Movimiento No encontrado para reversa
214Reversa ya efectuada
000Reversa de Forzado Borrado de la Bolsa
216El certificado de supervivencia sé encuentra vencido.
