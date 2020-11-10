*SRCMBRTXT:COELSA-Refresh de CBU                  
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     FSGSYSV    IF   E             DISK
     FACCTAC    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     FBAPJUR    IF   E           K DISK
     F*-------------------------------------------------------------------------
     D  NormalizaName  PR            30A
     D    Name                       30A
     D*---------------------------------------------------------------------
     D* Prototipos para las llamadas a la API
     D*---------------------------------------------------------------------
     D*------------------
     D opendir         PR              *   EXTPROC('opendir')
     D   dirname                       *   VALUE options(*string)
     D*------------------
     D closedir        PR            10I 0 EXTPROC('closedir')
     D   dirhandle                     *   VALUE
     D*------------------
     D readdir         PR              *   EXTPROC('readdir')
     D   dirhandle                     *   VALUE
     D*------------------
     D rename          PR            10I 0 EXTPROC('Qp0lRenameUnlink')
     D   old                           *   VALUE options(*string)
     D   new                           *   VALUE options(*string)

     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value options(*string)
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)

     D fseek           PR            10I 0 ExtProc('fseek')
     D  filehandler                  10I 0 value
     D  fileoffset                   10I 0 value
     D  start_pos                    10I 0 value

     D unlink          PR            10I 0 ExtProc('unlink')
     D   path                          *   Value options(*string)

     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D Close           PR            10I 0 ExtProc('close')
     D   Sock_Desc                   10I 0 Value
     D*---------------------------------------------------------------------
     D* Estructuras de Datos que devuelven las API
     D*---------------------------------------------------------------------
      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file]
      *********************************************************
     D O_WRONLY        C                   2
     D SEEK_SET        C                   0
     D O_RDWR          C                   4
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_TEXTDATA      C                   16777216
     D O_CODEPAGE      C                   8388608

     D*** file permissions
     D S_IRUSR         S             10I 0 INZ(256)
     D S_IWUSR         S             10I 0 INZ(128)
     D S_IXUSR         S             10I 0 INZ(64)
     D S_IRWXU         S             10I 0 INZ(448)
     D S_IRGRP         S             10I 0 INZ(32)
     D S_IWGRP         S             10I 0 INZ(16)
     D S_IXGRP         S             10I 0 INZ(8)
     D S_IRWXG         S             10I 0 INZ(56)
     D S_IROTH         S             10I 0 INZ(4)
     D S_IWOTH         S             10I 0 INZ(2)
     D S_IXOTH         S             10I 0 INZ(1)
     D S_IRWXO         S             10I 0 INZ(7)
     D AsciiCodePage   S             10U 0 INZ(850)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     I*----------------------------------------------------------------*
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     d buf             s            200
     d TIPCTA          s              2
     d Count           s              9S 0
     D*---------------------------------------------------------------------
     c                   ExSr      OpenStmF
     c                   ExSr      WrtHeader
     C                   Z-Add     1             Count
     c                   ExSr      ProcesarCA
     c                   ExSr      ProcesarCC
     c                   ExSr      WrtFooter
     c                   ExSr      CloseStmF
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C* ProcesarCA: Indica a Procesar cuentas que procese CA
     C*-------------------------------------------------------------------------
     C     ProcesarCA    BegSr
     c*
     c* ... In25=*On Estamos Procesando Caja de Ahorro *Off Cuentas Corrientes
     C                   Move      *On           *In25             1
     c                   ExSr      ProcCuentas
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* ProcesarCC: Indica a Procesar cuentas que procese CC
     C*-------------------------------------------------------------------------
     C     ProcesarCC    BegSr
     c*
     c* ... In25=*On Estamos Procesando Caja de Ahorro *Off Cuentas Corrientes
     C                   Move      *Off          *In25             1
     c                   ExSr      ProcCuentas
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* ProcCuentas: Procesar Cuentas
     c*-------------------------------------------------------------------------
     c     ProcCuentas   BegSr
     c*
     c   25*LOVAL        SetLL     REACCTAC
     c   25              Read      REACCTAC                               99
     c  N25*LOVAL        SetLL     RECCCTCT
     c  N25              Read      RECCCTCT                               99
+----C                   DoW       *In99 = *Off
|    C                   ExSr      EsCtaAProcesar
|+---c                   If        CtaAProcesar=*Off
||   C                   ExSr      GetTipoCta
||   C                   ExSr      GetCBU
||   C                   ExSr      BuscaFirmantes
||   c                   ExSr      WriteLine
||+--c                   If        CANTIT>3
|||  c                   ExSr      BuscaFirmAdici
||+--c                   EndIf
|+---c                   EndIf
|    c   25              Read      REACCTAC                               99
|    c  N25              Read      RECCCTCT                               99
+----c                   EndDo
     C*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C* WriteLine: Escribe una linea en el archivo
     c*-------------------------------------------------------------------------
     c     WriteLine     BegSr
     c*
     C                   Add       1             Count
     C                   Eval      buf=*blanks
     c                   Move      Count         WWCOUNT           9
     C                   Eval      %subst(buf:001:009)=WWCOUNT
     C                   Eval      %subst(buf:010:001)='D'
     C                   Eval      %subst(buf:011:003)='309'
     C                   Eval      %subst(buf:014:022)=WWNCBU
     C                   Eval      %subst(buf:036:003)='032'
+----c                   If        FUIMON=2
|    C                   Eval      %subst(buf:036:003)='840'
+----c                   EndIf
     C                   Eval      %subst(buf:039:002)=TIPCTA
     C                   Eval      %subst(buf:041:001)='F'
     C                   Eval      %subst(buf:042:001)=WWTIPP
     c                   Move      CANTIT        WWCATI            2
     C                   Eval      %subst(buf:043:002)=WWCATI
     c                   Move      WWCUI1        WWCUIT           11
     C                   Eval      %subst(buf:045:011)=WWCUIT
     C                   Eval      %subst(buf:056:040)=WWNYAP
     c                   Move      WWCUI2        WWCUIT           11
     C                   Eval      %subst(buf:096:011)=WWCUIT
     C                   Eval      %subst(buf:107:040)=WWNYA1
     c                   Move      WWCUI3        WWCUIT           11
     C                   Eval      %subst(buf:147:011)=WWCUIT
     C                   Eval      %subst(buf:158:040)=WWNYA2
     C                   Eval      %subst(buf:198:003)=*ZEROS
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* WriteLineAdici: Escribe linea Adicionales
     c*-------------------------------------------------------------------------
     c     WriteLineAdiciBegSr
     c*
     C                   Add       1             Count
     C                   Eval      buf=*blanks
     c                   Move      Count         WWCOUNT           9
     C                   Eval      %subst(buf:001:009)=WWCOUNT
     C                   Eval      %subst(buf:010:001)='A'
     c                   Move      WWCUI4        WWCUIT           11
     C                   Eval      %subst(buf:011:011)=WWCUIT
     C                   Eval      %subst(buf:022:040)=WWNYA3
     C                   Eval      %subst(buf:062:139)=*ZEROS
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* EsCtaAProcesar: Determinar si esta es una cuenta a procesar
     C*-------------------------------------------------------------------------
     C     EsCtaAProcesarBegSr
     C*
     c                   Move      *Off          CtaAProcesar      1
+----c                   If        *In25=*On
|+---C     FUIGRC        IFEQ      '98'
||   C     FUIGRC        OREQ      '99'
||   C     FUIGRC        OREQ      '70'
||   C     FUIMON        ORNE      1
||   C     FUFBAJ        ORGT      0
||   C     FUIBAC        ORGT      0
||   C     FUISGC        OREQ      'IN'
||   C     FUIINM        OREQ      'I'
||   C     FUIUCA        OREQ      'N'
||   c                   Move      *On           CtaAProcesar      1
|+---c                   EndIf
>    c                   Else
|+---C     BMIGRC        IFEQ      '44'
||   C     BMIGRC        OREQ      '20'
||   C     BMISGC        OREQ      'PE'
||   C     BMIMON        ORNE      1
||   C     BMFBAJ        ORGT      0
||   C     BMIBCC        ORGT      0
||   C     BMISGC        OREQ      'IN'
||   C     BMIUCA        OREQ      'N'
||   c                   Move      *On           CtaAProcesar      1
|+---C                   EndIf
+----C                   EndIf
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* GetTipoCta: Obtiene el Tipo de Cuenta
     C*-------------------------------------------------------------------------
     C     GetTipoCta    BegSr
     C*
+----c                   If        *In25=*On
|    C                   Eval      TIPCTA = '10'
|+---c                   If        FUIMON=2
||   C                   Eval      TIPCTA = '11'
|+---C                   Endif
>    c                   Else
|    C                   Eval      TIPCTA = '20'
|+---c                   If        BMIMON=2
||   C                   Eval      TIPCTA = '21'
|+---C                   Endif
+----c                   EndIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* GetCBU: Busca el CBU de la Cuenta
     C*-------------------------------------------------------------------------
     C     GetCBU        BegSr
     c*
+----c                   If        *In25=*On
|    c*
|+---C     FUISGC        IFEQ      'CE'
||   C                   Z-ADD     3             WWTIPO            2 0
|>   C                   ELSE
||   C                   Z-ADD     2             WWTIPO
|+---C                   ENDIF
|    C                   Z-ADD     *ZEROS        WWBLQ1            8 0
|    C                   Z-ADD     *ZEROS        WWBLQ2           14 0
|    C                   Z-ADD     *ZEROS        WWBLQ3           22 0
|    C                   CALL      'CBU000RG'
|    C                   PARM                    WWTIPO
|    C                   PARM                    FUISUC
|    C                   PARM                    FUICAH
|    C                   PARM                    WWBLQ1
|    C                   PARM                    WWBLQ2
|    C                   PARM                    WWBLQ3
|    C*
|    C                   MOVE      WWBLQ3        WWNCBU           22
|    C*
>    c                   Else
|+---C     BMISGC        IFEQ      'CE'
||   C                   Z-ADD     3             WWTIPO            2 0
|>   C                   ELSE
||   C                   Z-ADD     1             WWTIPO
|+---C                   ENDIF
|    C                   Z-ADD     *ZEROS        WWBLQ1            8 0
|    C                   Z-ADD     *ZEROS        WWBLQ2           14 0
|    C                   Z-ADD     *ZEROS        WWBLQ3           22 0
|    C                   CALL      'CBU000RG'
|    C                   PARM                    WWTIPO
|    C                   PARM                    BMISUC
|    C                   PARM                    BMICCC
|    C                   PARM                    WWBLQ1
|    C                   PARM                    WWBLQ2
|    C                   PARM                    WWBLQ3
|    C*
|    C                   MOVE      WWBLQ3        WWNCBU
|    C*
+----c                   EndIf
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtHeader     BegSr
     C*
     C                   Eval      %subst(buf:001:003)='002'
     C                   Eval      %subst(buf:004:009)='000000001'
     C                   Eval      %subst(buf:013:001)='H'
     C                   Eval      %subst(buf:014:003)='309'
     C                   Eval      %subst(buf:017:008)=%EditW(AASFEI:'        ')
     c* ... 0 Refresco Completo 1 Refresco Parcial
     C                   Eval      %subst(buf:025:001)='0'
     C                   Eval      %subst(buf:026:175)=*ZEROS
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtFooter     BegSr
     C*
     C                   Eval      %subst(buf:001:009)=%EditW(Count:'         ')
     C                   Eval      %subst(buf:010:001)='T'
     c                   Eval      %subst(buf:011:003)='309'
     C                   Eval      %subst(buf:014:008)=%EditW(AASFEI:'        ')
     C                   Eval      %subst(buf:022:008)=%EditW(Count:'         ')
     C                   Eval      %subst(buf:031:160)=*Blanks
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     C*
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHnd)
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                   O_WRONLY+O_TEXTDATA)
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     BuscaFirmantesBegSr
     C*
     C                   Z-Add     *ZERO         WWCUI2
     C                   Move      *BLANKS       WWNYA1
     C                   Z-Add     *ZERO         WWCUI3
     C                   Move      *BLANKS       WWNYA2
     C                   Z-Add     *ZERO         CANTIT            2 0
     C*
     C   25KOSCA         Chain     REBAICCL                           99
     C  N25KOSCC         Chain     REBAICCL                           99
+----C                   If        OSININ <> 0
|    c                   ExSr      ProcPJ
+----C                   Else
|    c                   ExSr      ProcPF
+----C                   Endif
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c*BuscaFirmAdici
     C*-------------------------------------------------------------------------
     C     BuscaFirmAdiciBegSr
     C*
     C                   Z-Add     *ZERO         WWCUI4
     C                   Move      *BLANKS       WWNYA3
     C*
     C                   Z-Add     4             WWITTL
     C*
     c     sigAdici      Tag
     C   25KOTCA         Chain     REBADCCL                           99
     C  N25KOTCC         Chain     REBADCCL                           99
+----c                   If        *IN99 = *Off
|+---C                   If        WWITTL <= 12
||   c                   ExSr      ProcPFAdici
||   C                   Z-Add     *ZERO         WWCUI4
||   C                   Move      *BLANKS       WWNYA3
||   c                   goto      sigAdici
|+---C                   Endif
+----C                   Endif
     C*
     C     finadici      EndSr
     C*-------------------------------------------------------------------------
     C     ProcPJ        BegSr
     C*
     C                   Move      'J'           WWTIPP            1
     C                   Z-Add     1             CANTIT
     C*
     C     KAO000        Chain     REBAPJUR                           99
     C*
     C*                  Eval      WWCUI1=AOICUI
     C                   MOVE      AOICUI        WWCUI1
     C                   Eval      WWNYAP=NormalizaName(AONRSO)
     C*
     c                   Move      *Zeros        WWCUI2
     c                   Move      *Blanks       WWNYA1
     c                   Move      *Zeros        WWCUI3
     c                   Move      *Blanks       WWNYA2
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     ProcPF        BegSr
     C*
     C                   Move      'F'           WWTIPP
     C*
     c                   Move      *Zeros        WWCUI2
     c                   Move      *Blanks       WWNYA1
     c                   Move      *Zeros        WWCUI3
     c                   Move      *Blanks       WWNYA2
     C                   Move      *Off          *In98             1
     C*
     C                   Z-Add     1             CANTIT
     C                   Z-Add     1             WWITTL
     C   25KOTCA         Chain     REBADCCL                           99
     C  N25KOTCC         Chain     REBADCCL                           99
     C     KPF000        Chain     REBAPFIS                           99
     C                   Eval      WWNYAP=NormalizaName(AÑNYAP)
     C                   MOVE      AÑICUI        WWCUI1
     C*                  Eval      WWCUI1=AÑICUI
     C*
     C                   Z-Add     2             WWITTL
     C   25KOTCA         Chain     REBADCCL                           99
     C  N25KOTCC         Chain     REBADCCL                           99
+----C                   IF        *IN99 = *OFF
|    C     KPF000        Chain     REBAPFIS                           99
|+---C                   If        *IN99 = *OFF
||   C                   Z-Add     2             CANTIT
||   C                   Eval      WWNYA1=NormalizaName(AÑNYAP)
||   C                   Eval      WWCUI2=AÑICUI
|+---C                   Endif
+----C                   EndIf
     C*
     C                   Z-Add     3             WWITTL
     C   25KOTCA         Chain     REBADCCL                           99
     C  N25KOTCC         Chain     REBADCCL                           99
+----C                   IF        *IN99 = *OFF
|    C     KPF000        Chain     REBAPFIS                           99
|+---C                   If        *IN99 = *OFF
||   C                   Z-Add     3             CANTIT
||   C                   Eval      WWNYA2=NormalizaName(AÑNYAP)
||   C                   Eval      WWCUI3=AÑICUI
|+---C                   EndIf
+----C                   EnDIf
     C*
+----C                   IF        WWCUI3> 0
|    c     sig           Tag
|    C                   Add       1             WWITTL
|+---C                   If        WWITTL <= 12
||   C   25KOTCA         Chain     REBADCCL                           99
||   C  N25KOTCC         Chain     REBADCCL                           99
||+--C                   IF        *IN99 = *OFF
|||  C     KPF000        Chain     REBAPFIS                           99
|||+-C                   If        *IN99 = *OFF
|||| C                   Add       1             CANTIT
|||| C                   Move      *On           *In98             1
|||+-C                   EndIf
||+--C                   EnDIf
||   c*
||+--c                   IF        *In98 = *On
|||  c                   goto      sig
||+--c                   EnDIf
|+---c                   EnDIf
+----C                   EnDIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c*ProcPFAdici:
     C*-------------------------------------------------------------------------
     C     ProcPFAdici   BegSr
     C*
     C                   Z-Add     *ZERO         WWCUI4
     C                   Move      *BLANKS       WWNYA3
     C*
     C     KPF000        Chain     REBAPFIS                           99
     C                   Eval      WWNYA3=NormalizaName(AÑNYAP)
     C                   MOVE      AÑICUI        WWCUI4
     C                   Eval      WWCUI4=AÑICUI
     c                   ExSr      WriteLineAdici
     C*
     C                   Add       1             WWITTL
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     CloseStmF     BegSr
     C*
     c                   callp     close(FilHnd)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *ENTRY        PLIST
     C                   PARM                    txtfile         255
     C*
     C     *LIKE         Define    OTITTL        WWITTL
     C     *LIKE         Define    AÑICUI        WWCUI1
     C     *LIKE         Define    AÑNYAP        WWNYAP
     C     *LIKE         Define    AÑICUI        WWCUI2
     C     *LIKE         Define    AÑNYAP        WWNYA1
     C     *LIKE         Define    AÑICUI        WWCUI3
     C     *LIKE         Define    AÑNYAP        WWNYA2
     C     *LIKE         Define    AÑICUI        WWCUI4
     C     *LIKE         Define    AÑNYAP        WWNYA3
     C*
     C     KOSCA         KLIST
     C                   KFld                    FUISUC
     C                   KFld                    FUICCL
     C     KOTCA         KLIST
     C                   KFld                    FUISUC
     C                   KFld                    FUICCL
     C                   KFld                    WWITTL
     C     KOSCC         KLIST
     C                   KFld                    BMISUC
     C                   KFld                    BMICCL
     C     KOTCC         KLIST
     C                   KFld                    BMISUC
     C                   KFld                    BMICCL
     C                   KFld                    WWITTL
     C     KAO000        KLIST
     C                   KFld                    OSITIN
     C                   KFld                    OSININ
     C     KPF000        KLIST
     C                   KFld                    OTITDO
     C                   KFld                    OTINDO
     C*
     C*
     C     1             Chain     RESGSYSV
     C*
     C                   EndSr
     C*=========================================================================
     P NormalizaName   B
     D  NormalizaName  PI            30A
     D    Name                       30A
     D*
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D Symbols         C                   '|°¬!"#$%&/()=?\¡¿*+~[]{}_-;,:.<>'
     D SymBlanks       C                   '                                '
     D Acentos         C                   'ñÑáéíóúäëïöüãõàèìòùâêîôû@'
     D AceBlanks       C                   'nNAEIOUAEIOUAOAEIOUAEIOU '
     D Apos            C                   ''''
     D APosBlank       C                   ' '
     D*
     C                   Eval      NAME = %XLATE(Symbols:SymBlanks:NAME)
     C                   Eval      NAME = %XLATE(Acentos:AceBlanks:NAME)
     C                   Eval      NAME = %XLATE(Apos:AposBlank:NAME)
     C                   Eval      NAME = %XLATE(lo:up:NAME)
     C                   Return    NAME
     P NormalizaName   E
