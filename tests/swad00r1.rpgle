*SRCMBRTXT:Switch-Adapter      -Movs C.C.         
     H
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD00R1                                       *
     H*                                                               *
     H*  PROGRAM NO: Genera Movimientos en Cuenta Corriente           *
     H*                                                               *
     H*  DATE: 23/02/2004                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     H*****************************************************************
     FCCCTCT    IF   E           K DISK
     FCCACUE01  IF   E           K DISK
     FCCCODI    IF   E           K DISK
     FCCPGMS    IF   E           K DISK
     FCCSUCU    IF   E           K DISK
     FBASUCU    IF   E           K DISK
     FCCBLOQ    IF   E           K DISK
     FBAEXEP02  IF   E           K DISK
     FSGSYSV    IF   E             DISK
     FCCMOCT06  UF   E           K DISK
     FCCMOVB10  UF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     F*----------------------------------------------------------------
     D @MSG            S             40    DIM(15) CTDATA PERRCD(1)
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
     C* REVMOV: REVERSA MOVIMIENTOS
     C*-------------------------------------------------------------------------
     C     REVMOV        BEGSR
     C*
     C                   EXSR      CAFASI
     C*
     C     WKEY03        CHAIN(N)  RECCMOCT                           99
+----C     *IN99         IFEQ      *OFF
|    C*
|+---C     CFIASC        IFEQ      0
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
     C     PAIMCC        IFEQ      48
     C                   MOVE      56            WKIMCC
     C                   ELSE
     C                   MOVE      PAIMCC        WKIMCC
     C                   ENDIF
|    C     KC1100        CHAIN     RECCMOVB                           99
|+---C     *IN99         IFEQ      *OFF
||   C                   DELETE    RECCMOVB
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
     C*
     C                   EXSR      VALIDA
     C*
     C*  ........Valida Acumulado Mensual de Extracciones "FP".
     C                   EXSR      VALIFP
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
     C*----------------------------------------------------------------
     C* ACTSAL: Actualiza Sucursal de Alta y Generación de Asientos
     C*----------------------------------------------------------------
     C     ACTSAL        BEGSR
     C*
     C     CFIMCC        IFEQ      558
     C     CFIMCC        OREQ      8
     C                   Z-ADD     *ZERO         WWISAL
     C                   ELSE
     C                   Z-ADD     CFISUC        WWISAL
     C                   ENDIF
     C*
     C     WKEY14        CHAIN     RECCMOCT                           90
     C     *IN90         DOWEQ     *OFF
     C*
     C     CFICHE        IFEQ      PAICHE
     C                   MOVE      'S'           CFIGAS
     C                   Z-ADD     WWISAL        CFISAL
     C                   UPDATE    RECCMOCT
     C                   ENDIF
     C*
     C     WKEY14        READE     RECCMOCT                               90
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VALIDA: Valida la cuenta
     C*-------------------------------------------------------------------------
     C     VALIDA        BEGSR
     C*
     C*   .......Testea la existencia y vigencia de la cuenta
     C     WKEY01        CHAIN     CCCTCT                             80
+----C     *IN80         IFEQ      *ON
|    C                   MOVE(P)   @MSG(7)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*   .......Blanquea error
     C                   MOVE      *BLANK        PAIERR
     C*   .......Verifica si el Sub-Sistema esta Cerrado
     C                   EXSR      CHKSBS
     C*   .......Características de código de movimiento
     C     PAIMCC        CHAIN     CCCODI                             80
+----C     *IN80         IFEQ      *ON
|    C                   MOVE(P)   @MSG(5)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*   .......Válida Sucursal
     C                   EXSR      VALSUC
     C*   .......Testea la existencia y vigencia de la cuenta
+----C     BMFBAJ        IFNE      *ZERO
|    C                   MOVE(P)   @MSG(1)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
+----C     BMISGC        IFEQ      'IN'
|    C                   MOVE(P)   @MSG(1)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*  ........Valida bloqueo de la cuenta
     C                   EXSR      VABLOQ
+----C     PAIERR        IFNE      *BLANKS
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*  ........Valida excepciones
     C                   EXSR      VALEXE
+----C     PAIERR        IFNE      *BLANKS
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*   .......Actualiza Saldos
+----C     BLITMO        IFEQ      1
|    C     PATIPT        ANDNE     'R'
|    C                   EXSR      VALSAL
|+---C     PAIERR        IFNE      *BLANKS
||   C                   EXSR      ENDPGM
|+---C                   ENDIF
+----C                   ENDIF
     C*
+----C     PAIERR        IFNE      *BLANKS
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C                   ENDSR
     C*--------------------------------------------------------------*
     C* ASOIMP: Impacta Códigos Asociados                            *
     C*--------------------------------------------------------------*
     C     ASOIMP        BEGSR
     C*
     C                   MOVE      *BLANKS       WWERRO           70
     C*
     C                   CALL      'CC0223R4'
     C                   PARM      2             CFIDBC            3 0
     C                   PARM                    PAISUC
     C                   PARM                    BMIMON
     C                   PARM                    PAICCC
     C                   PARM                    PAIMCC
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
     C                   MOVE      *BLANKS       WWERRO           70
     C                   CALL      'CC0223R6'
     C                   PARM                    PAISUC
     C                   PARM                    BMIMON
     C                   PARM                    PAICCC
     C                   PARM                    PAIMCC
     C                   PARM                    PA$IMP
     C                   PARM                    PAICHE
     C                   PARM                    WWICAJ
     C                   PARM                    PAHALT
     C                   PARM                    WWERRO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CAFASI: Calcula Fecha de Asiento
     C*-------------------------------------------------------------------------
     C     CAFASI        BEGSR
     C*
     C     PAIMCC        CHAIN     CCCODI                             80
+----C                   SELECT
+----C     BLISAF        WHENEQ    0
|    C                   Z-ADD     AASFEI        CFFASI
+----C     BLISAF        WHENEQ    1
|    C                   Z-ADD     AAFS24        CFFASI
+----C     BLISAF        WHENEQ    2
|    C                   Z-ADD     AAFS48        CFFASI
+----C     BLISAF        WHENEQ    3
|    C                   Z-ADD     AAFS72        CFFASI
+----C                   ENDSL
     C*
     C                   MOVE      CFFASI        PAFASI
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VABLOQ: Valida Bloqueos en la cuenta.
     C*-------------------------------------------------------------------------
     C     VABLOQ        BEGSR
     C*
     C     BMIBCC        CHAIN     CCBLOQ                             80
     C*
     C*  ........Valida bloqueo operaciones de débito
+----C     BLITMO        IFEQ      1
|    C     BFIBDB        ANDNE     *ZERO
|+---C     BM$SBL        IFEQ      0
||   C                   MOVEL     @MSG(3)       PAIERR
|+---C                   ELSE
||   C     BM$SOP        SUB       PA$IMP        WW$$SA           15 2
||+--C     WW$$SA        IFLT      BM$SBL
|||  C                   MOVEL     @MSG(3)       PAIERR
||+--C                   END
|+---C                   END
+----C                   END
     C*  ........Valida bloqueo operaciones de crédito
+----C     BLITMO        IFEQ      2
|    C     BFIBCR        ANDNE     0
|+---C     BM$SBL        IFEQ      0
||   C                   MOVEL     @MSG(4)       PAIERR
|+---C                   ELSE
||   C     BM$SOP        ADD       PA$IMP        WW$$SA
||+--C     BM$SBL        IFGT      WW$$SA
|||  C                   MOVEL     @MSG(4)       PAIERR
||+--C                   END
|+---C                   END
+----C                   END
     C     ENBLOQ        ENDSR
     C*-------------------------------------------------------------------------
     C* VALEXE: Valida excepciones
     C*-------------------------------------------------------------------------
     C     VALEXE        BEGSR
     C*
     C                   MOVEL     'CC'          WWISUB
     C     @KEY07        CHAIN     BAEXEP02                           99
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
     C                   Z-ADD     PAISUC        AVISUC
     C*   .......Valida Sucursal Habilitada para operar Ctas. Ctes
     C     WKEY02        CHAIN     CCSUCU                             80
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
     C* VALSAL: Para Extracciones Valida Saldos y Acuerdos
     C*-------------------------------------------------------------------------
     C     VALSAL        BEGSR
     C*
     C* ... Verifica saldo de la cuenta
     C     BM$SOP        SUB       PA$IMP        WW$SAL           15 2
+----C     WW$SAL        IFGE      *ZERO
|    C                   GOTO      ENSALD
+----C                   ENDIF
     C*
     C* ... Testea si la cuenta posee autorizados giros en descubierto
+----C     BM$GDE        IFGT      *ZEROS
|    C     BMFVGD        ANDGE     AASFEI
|    C                   ADD       BM$GDE        WW$SAL
+----C                   END
+----C     WW$SAL        IFGE      *ZERO
|    C                   GOTO      ENSALD
+----C                   END
     C*
     C* ... Busca acuerdos vigentes de la cuenta corriente
     C     WKEY01        CHAIN     CCACUE01                           80
+----C     *IN80         DOWEQ     *OFF
|+---C     BEFVEN        IFGE      AASFEI
||+--C     BEFBAJ        IFEQ      0
|||  C     BEFBAJ        ORGT      AASFEI
|||  C                   ADD       BE$IMP        WW$SAL
||+--C                   ENDIF
|+---C                   ENDIF
|    C     WKEY01        READE     CCACUE01                               80
+----C                   ENDDO
+----C     WW$SAL        IFGE      *ZERO
|    C                   GOTO      ENSALD
+----C                   ENDIF
     C*
     C*   .......Mueve error por falta de saldo
     C                   MOVEL(P)  @MSG(06)      PAIERR
     C     ENSALD        ENDSR
     C*-------------------------------------------------------------------------
     C* VALIFP: Valida Acumulado Mensual de Extracciones "FP".
     C*-------------------------------------------------------------------------
     C     VALIFP        BEGSR
     C*
     C     PAIMCC        IFEQ      48
     C*
     C                   MOVEL     *BLANKS       PAERRO           40
     C                   CALL      'CCEXFPRG'
     C                   PARM                    PAISUC
     C                   PARM                    PAICCC
     C                   PARM                    PA$IMP
     C                   PARM                    PAFASI
     C                   PARM                    PAERRO
     C*
     C     PAERRO        IFNE      *BLANKS
     C                   MOVEL(P)  @MSG(06)      PAIERR
     C                   MOVEL     '106'         PAIERR
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CHKSBS: Verifica que el Sub-Sistema Este cerrado
     C*-------------------------------------------------------------------------
     C     CHKSBS        BEGSR
     C*
     C                   MOVEL     'CCPDDIAR'    LBIPGM
     C     LBIPGM        CHAIN     CCPGMS                             80
+----C     *IN80         IFEQ      *OFF
|    C                   MOVEL(P)  @MSG(8)       PAIERR
|    C                   EXSR      ENDPGM
+----C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* *INZSR: Subrutina de inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    PATIPT            1
     C                   PARM                    PAIATM           16
     C                   PARM                    PAIMCC
     C                   PARM                    PAISUC
     C                   PARM                    PAICCC
     C                   PARM                    PAICHE
     C                   PARM                    PA$IMP
     C                   PARM                    PAIERR           40
     C                   PARM                    PAFASI
     C                   PARM                    PAHALT            6 0
     C*
     C     *LIKE         DEFINE    BLIMCC        PAIMCC
     C     *LIKE         DEFINE    BLIMCC        WKIMCC
     C     *LIKE         DEFINE    BMISUC        PAISUC
     C     *LIKE         DEFINE    BMISUC        WWISAL
     C     *LIKE         DEFINE    BMICCC        PAICCC
     C     *LIKE         DEFINE    CF$IMP        PA$IMP
     C     *LIKE         DEFINE    CFFASI        PAFASI
     C     *LIKE         DEFINE    CFICHE        PAICHE
     C*          *LIKE     DEFN BLISAF    WWISAF
     C     *LIKE         DEFINE    CFICAJ        WWICAJ
     C     *LIKE         DEFINE    EXISUB        WWISUB
     C*
     C                   Z-ADD     998           WWICAJ
     C*
     C     WKEY01        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICCC
     C     WKEY14        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICCC
     C                   KFLD                    AASFEI
     C                   KFLD                    PAHALT
     C     WKEY02        KLIST
     C                   KFLD                    PAISUC
     C     WKEY03        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICCC
     C                   KFLD                    AASFEI
     C                   KFLD                    PAHALT
     C                   KFLD                    PAIMCC
     C                   KFLD                    PAISUC
     C                   KFLD                    WWICAJ
     C                   KFLD                    PA$IMP
     C                   KFLD                    PAICHE
     C                   KFLD                    PAFASI
     C*
     C     KC1100        KLIST
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICCC
     C                   KFLD                    PAICHE
     C                   KFLD                    WKIMCC
     C                   KFLD                    PA$IMP
     C     @KEY07        KLIST
     C                   KFLD                    WWISUB
     C                   KFLD                    PAISUC
     C                   KFLD                    PAICCC
     C                   KFLD                    PAIMCC
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
     c                             'SWAD00R1 Se Cancela pues Recibio:'+
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
**                  1
101Cta. Cte. de baja
102Cta. Cte. No existe
103Cta.Bloqueada P/Deb.
104Cta.Bloqueada P/Cred.
105Cód.de Mov.no Def.
106Saldo Insuficiente
107Cta no Existe
108Subsis C.C. Cerrado
109Saldos hist insuf.
110Suc.no definida
111Suc.no habilitada CC
112Sucursal en Feriado
113Movimiento No encontrado para reversa
114Reversa ya efectuada
000Reversa de Forzado Borrado de la Bolsa
