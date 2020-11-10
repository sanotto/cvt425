*SRCMBRTXT:COELSA-Refresh de CBU                  
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     F*-------------------------------------------------------------------------
     FSGSYSV    IF   E             DISK
     FACCTAC    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     FBADCCL01  IF   E           K DISK
     FBAPFIS    IF   E           K DISK
     FBAPJUR    IF   E           K DISK
     F@CPIUSD   IF   E           K DISK
     F@CPISYS   IF   E           K DISK
     FSPPCBU    UF A E           K DISK
     FBANUME    UF A E           K DISK
     FBC4300    O    E             DISK
     FLISERJ    O    E             DISK
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
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     I*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     D DSCTRL          DS
     D  DSNCBU                 1     22  0
     D  DSMONE                23     25
     D  DSTCTA                26     27
     D  DSTIPP                28     28
     D  DSCATI                29     30
     D  DSCUI1                31     41  0
     D  DSNYA1                42     81
     D  DSCUI2                82     92  0
     D  DSNYA2                93    132
     D  DSCUI3               133    143  0
     D  DSNYA3               144    183
     D  DSCUI4               184    194  0
     D  DSNYA4               195    234
     D  DSCUI5               235    245  0
     D  DSNYA5               246    285
     D  DSCUI6               286    296  0
     D  DSNYA6               297    336
     D  DSCUI7               337    347  0
     D  DSNYA7               348    387
     D  DSCUI8               388    398  0
     D  DSNYA8               399    438
     D  DSCUI9               439    449  0
     D  DSNYA9               450    489
     D  DSCU10               490    500  0
     D  DSPAR1                 1    500
     D  DSNY10               501    540
     D  DSCU11               541    551  0
     D  DSNY11               552    591
     D  DSCU12               592    602  0
     D  DSNY12               603    642
     D  DSIGRC               643    644
     D  DSISGC               645    646
     D  DSPAR2               501    646
     D*----------------------------------------------------------------*
     D DSARCO          DS
     D  DSPREO                 1      9
     D  DSEXTO                10     13
     D DSARCH          DS
     D  DSPREH                 1      9
     D  DSGUI1                10     10
     D  DSNUMH                11     12
     D  DSGUI2                13     13
     D  DSENVH                14     16
     D  DSEXTH                17     20
     D DSNUME          DS
     D  DSIULM                14     15
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     d buf             s            202
     d TIPCTA          s              2
     d Count           s              9S 0
     d CountFinal      s              9S 0
     D*=====================================================================
     c                   ExSr      OpenStmF
     c                   ExSr      WrtHeader
     C                   Z-Add     1             Count
     c                   ExSr      ProcesarCA
     c                   ExSr      ProcesarCC
     c                   ExSr      WrtFooter
     c                   ExSr      CloseStmF
     c                   ExSr      EndPgm
     c*=====================================================================----
     c* OpenStmF:
     c*-------------------------------------------------------------------------
     c     OpenStmF      BegSr
     c*
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHnd)
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                   O_WRONLY+O_TEXTDATA)
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* WrtHeader: Escribe Cabecera
     c*-------------------------------------------------------------------------
     c     WrtHeader     BegSr
     c*
     c                   Eval      %subst(buf:001:003)='002'
     c                   Eval      %subst(buf:004:009)='000000001'
     c                   Eval      %subst(buf:013:001)='H'
     c                   Eval      %subst(buf:014:003)='309'
     c                   Eval      %subst(buf:017:008)=%EditW(AASFEI:'        ')
     c* ... 0 Refresco Completo 1 Refresco Parcial
     c                   Eval      %subst(buf:025:001)='0'
     c                   Eval      %subst(buf:026:175)=*ZEROS
     C                   Eval      %subst(buf:201:002)=X'0D'+X'25'
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* ProcesarCA: Indica a Procesar cuentas que procese CA
     c*-------------------------------------------------------------------------
     c     ProcesarCA    BegSr
     c*
     c* ... In25=*On Estamos Procesando Caja de Ahorro *Off Cuentas Corrientes
     c                   Move      *On           *In25             1
     c                   ExSr      ProcCuentas
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* ProcesarCC: Indica a Procesar cuentas que procese CC
     c*-------------------------------------------------------------------------
     c     ProcesarCC    BegSr
     c*
     c* ... In25=*On Estamos Procesando Caja de Ahorro *Off Cuentas Corrientes
     c                   Move      *Off          *In25             1
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
+----c                   DoW       *In99 = *Off
|    c                   ExSr      EsCtaAProcesar
|+---c                   If        CtaAProcesar=*Off
||   c                   ExSr      GetTipoCta
||   c                   ExSr      GetCBU
||   c                   ExSr      BuscaFirmantes
||   c                   ExSr      WriteLine
||   c                   ExSr      WritePadron
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
     c* EsCtaAProcesar: Determinar si esta es una cuenta a procesar
     c*-------------------------------------------------------------------------
     c     EsCtaAProcesarBegSr
     c*
     c                   Move      *Off          CtaAProcesar      1
+----c                   If        *In25=*On
|+---c     FUIGRC        IFEQ      '98'
||   c     FUIGRC        OREQ      '99'
||   c     FUIGRC        OREQ      '70'
||   c     FUIMON        ORNE      1
||   c     FUFBAJ        ORGT      0
||   c     FUIBAC        ORGT      0
||   c     FUISGC        OREQ      'IN'
||   c     FUIINM        OREQ      'I'
||   c     FUIUCA        OREQ      'N'
||   c                   Move      *On           CtaAProcesar      1
|+---c                   EndIf
>    c                   Else
|+---c     BMIGRC        IFEQ      '44'
||   c     BMIGRC        OREQ      '20'
||   c     BMISGC        OREQ      'PE'
||   c     BMIMON        ORNE      1
||   c     BMFBAJ        ORGT      0
||   c     BMIBCC        ORGT      0
||   c     BMISGC        OREQ      'IN'
||   c     BMIUCA        OREQ      'N'
||   c                   Move      *On           CtaAProcesar      1
|+---c                   EndIf
+----c                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* GetTipoCta: Obtiene el Tipo de Cuenta
     C*-------------------------------------------------------------------------
     C     GetTipoCta    BegSr
     C*
     c                   Move      *BLANKS       MONEDA            3
+----c                   If        *In25=*On
|    C                   Eval      TIPCTA = '10'
|    C                   Eval      MONEDA = '032'
|+---c                   If        FUIMON=2
||   C                   Eval      TIPCTA = '11'
||   C                   Eval      MONEDA = '840'
|+---C                   Endif
>    c                   Else
|    C                   Eval      MONEDA = '032'
|    C                   Eval      TIPCTA = '20'
|+---c                   If        BMIMON=2
||   C                   Eval      TIPCTA = '21'
||   C                   Eval      MONEDA = '840'
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
     C* BuscaFirmantes: Busca Firmantes
     C*-------------------------------------------------------------------------
     C     BuscaFirmantesBegSr
     C*
     C                   Z-Add     *ZERO         WWCUI1
     C                   Move      *BLANKS       WWNYA1
     C                   Z-Add     *ZERO         WWCUI2
     C                   Move      *BLANKS       WWNYA2
     C                   Z-Add     *ZERO         WWCUI3
     C                   Move      *BLANKS       WWNYA3
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
     C*ProcPJ:
     C*-------------------------------------------------------------------------
     C     ProcPJ        BegSr
     C*
     C                   Move      'J'           WWTIPP            1
     C                   Z-Add     1             CANTIT
     C                   Z-Add     *Zeros        CountFinal
     C*
     C     KAO000        Chain     REBAPJUR                           99
     C*
     C*                  Eval      WWCUI1=AOICUI
     C                   MOVE      AOICUI        WWCUI1
     C                   Eval      WWNYA1=NormalizaName(AONRSO)
     C*
     c                   Move      *Zeros        WWCUI2
     c                   Move      *Blanks       WWNYA2
     c                   Move      *Zeros        WWCUI3
     c                   Move      *Blanks       WWNYA3
     c                   Move      *Zeros        WWCUI4
     c                   Move      *Blanks       WWNYA4
     c                   Move      *Zeros        WWCUI5
     c                   Move      *Blanks       WWNYA5
     c                   Move      *Zeros        WWCUI6
     c                   Move      *Blanks       WWNYA6
     c                   Move      *Zeros        WWCUI7
     c                   Move      *Blanks       WWNYA7
     c                   Move      *Zeros        WWCUI8
     c                   Move      *Blanks       WWNYA8
     c                   Move      *Zeros        WWCUI9
     c                   Move      *Blanks       WWNYA9
     c                   Move      *Zeros        WWCU10
     c                   Move      *Blanks       WWNY10
     c                   Move      *Zeros        WWCU11
     c                   Move      *Blanks       WWNY11
     c                   Move      *Zeros        WWCU12
     c                   Move      *Blanks       WWNY12
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*ProcPF
     C*-------------------------------------------------------------------------
     C     ProcPF        BegSr
     C*
     C                   Move      'F'           WWTIPP
     C*
     c                   Move      *Zeros        WWITTL
     c                   Move      *Zeros        WWCUI1
     c                   Move      *Blanks       WWNYA1
     c                   Move      *Zeros        WWCUI2
     c                   Move      *Blanks       WWNYA2
     c                   Move      *Zeros        WWCUI3
     c                   Move      *Blanks       WWNYA3
     c                   Move      *Zeros        WWCUI4
     c                   Move      *Blanks       WWNYA4
     c                   Move      *Zeros        WWCUI5
     c                   Move      *Blanks       WWNYA5
     c                   Move      *Zeros        WWCUI6
     c                   Move      *Blanks       WWNYA6
     c                   Move      *Zeros        WWCUI7
     c                   Move      *Blanks       WWNYA7
     c                   Move      *Zeros        WWCUI8
     c                   Move      *Blanks       WWNYA8
     c                   Move      *Zeros        WWCUI9
     c                   Move      *Blanks       WWNYA9
     c                   Move      *Zeros        WWCU10
     c                   Move      *Blanks       WWNY10
     c                   Move      *Zeros        WWCU11
     c                   Move      *Blanks       WWNY11
     c                   Move      *Zeros        WWCU12
     c                   Move      *Blanks       WWNY12
     C                   Move      *Off          *In98             1
     C                   Z-Add     *Zeros        CountFinal
     C*
     c     sig           Tag
     C                   Move      *Off          *In98             1
     C                   Add       1             WWITTL
+----C                   If        WWITTL <= 12
|    C   25KOTCA         Chain     REBADCCL                           99
|    C  N25KOTCC         Chain     REBADCCL                           99
|+---C                   IF        *IN99 = *OFF
||   C     KPF000        Chain     REBAPFIS                           99
||+--C                   If        *IN99 = *OFF
|||  C                   Add       1             CANTIT
|||  C                   Move      *On           *In98             1
|||+-c                   Select
|||| c                   When      WWITTL=1
|||| C                   Eval      WWNYA1=NormalizaName(AÑNYAP)
|||| C                   MOVE      AÑICUI        WWCUI1
|||| c                   When      WWITTL=2
|||| C                   Eval      WWNYA2=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI2=AÑICUI
|||| c                   When      WWITTL=3
|||| C                   Eval      WWNYA3=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI3=AÑICUI
|||| c                   When      WWITTL=4
|||| C                   Eval      WWNYA4=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI4=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=5
|||| C                   Eval      WWNYA5=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI5=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=6
|||| C                   Eval      WWNYA6=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI6=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=7
|||| C                   Eval      WWNYA7=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI7=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=8
|||| C                   Eval      WWNYA8=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI8=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=9
|||| C                   Eval      WWNYA9=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCUI9=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=10
|||| C                   Eval      WWNY10=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCU10=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=11
|||| C                   Eval      WWNY11=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCU11=AÑICUI
     C                   Add       1             CountFinal
|||| c                   When      WWITTL=12
|||| C                   Eval      WWNY12=NormalizaName(AÑNYAP)
|||| C                   Eval      WWCU12=AÑICUI
     C                   Add       1             CountFinal
|||+-c                   EndSl
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
     C* WriteLine: Escribe una linea en el archivo
     c*-------------------------------------------------------------------------
     c     WriteLine     BegSr
     c*
     C                   Add       1             Count
     C                   Add       Count         CountFinal
     C                   Eval      buf=*blanks
     c                   Move      Count         WWCOUNT           9
     C                   Eval      %subst(buf:001:009)=WWCOUNT
     C                   Eval      %subst(buf:010:001)='D'
     C                   Eval      %subst(buf:011:003)='309'
     C                   Eval      %subst(buf:014:022)=WWNCBU
     C                   Eval      %subst(buf:036:003)=MONEDA
     C                   Eval      %subst(buf:039:002)=TIPCTA
     C                   Eval      %subst(buf:041:001)='F'
     C                   Eval      %subst(buf:042:001)=WWTIPP
     c                   Move      CANTIT        WWCATI            2
     C                   Eval      %subst(buf:043:002)=WWCATI
     c                   Move      WWCUI1        WWCUIT           11
     C                   Eval      %subst(buf:045:011)=WWCUIT
     C                   Eval      %subst(buf:056:040)=WWNYA1
     c                   Move      WWCUI2        WWCUIT           11
     C                   Eval      %subst(buf:096:011)=WWCUIT
     C                   Eval      %subst(buf:107:040)=WWNYA2
     c                   Move      WWCUI3        WWCUIT           11
     C                   Eval      %subst(buf:147:011)=WWCUIT
     C                   Eval      %subst(buf:158:040)=WWNYA3
     C                   Eval      %subst(buf:198:003)=*ZEROS
     C                   Eval      %subst(buf:201:002)=X'0D'+X'25'
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* WritePadron: Escribe Registro en SPPCBU
     c*-------------------------------------------------------------------------
     c     WritePadron   BegSr
     c*
     c                   Move      'V'           SBICAP
     c                   Move      'B'           SBIOLM
     c                   Move      'D'           SBACTL
     c                   Move      'SA'          SBIPRE
     C                   Z-Add     *ZERO         SBITAC
     C                   Z-Add     AASFEI        SBFING
     C                   Z-Add     WWHORA        SBHALT
     c                   Move      @ZUSER        SBIUSA
     c                   Move      @ZJBNM        SBITTE
     c                   Move      'AC0091RG'    SBIPGP
     C                   Z-add     Count         SBISEQ
     C                   Z-add     CountFinal    SBISCD
     c   25              Move      'AC'          SBISUB
     c  N25              Move      'CC'          SBISUB
     c   25              Z-Add     FUISUC        SBISUC
     c   25              Z-Add     FUICAH        SBICCC
     c  N25              Z-Add     BMISUC        SBISUC
     c  N25              Z-Add     BMICCC        SBICCC
     c   25              Z-Add     FUIMON        SBIMON
     c  N25              Z-Add     BMIMON        SBIMON
     c   25              Move      FUIGRC        SBIGRC
     c  N25              Move      BMIGRC        SBIGRC
     C   25              Move      FUISGC        SBISGC
     C  N25              Move      BMISGC        SBISGC
     c                   Move      WWNCBU        SBNCBU
     c                   Move      'F'           SBIASK
     c                   Move      WWTIPP        SBTIPP
     c                   Move      WWCATI        SBQDCD
     c                   Z-Add     WWCUI1        SBCUIA
     c                   Move      WWNYA1        SBNYAP
     c                   Z-Add     WWCUI2        SBCUI2
     c                   Move      WWNYA2        SBNYA2
     c                   Z-Add     WWCUI3        SBCUI3
     c                   Move      WWNYA3        SBDNI3
     c                   Z-Add     WWCUI4        SBCUI4
     c                   Move      WWNYA4        SBDNI4
     c                   Z-Add     WWCUI5        SBCUI5
     C                   Move      WWNYA5        SBDNI5
     c                   Z-Add     WWCUI6        SBCUI6
     C                   Move      WWNYA6        SBDNI6
     c                   Z-Add     WWCUI7        SBCUI7
     c                   Move      WWNYA7        SBDNI7
     c                   Z-Add     WWCUI8        SBCUI8
     C                   Move      WWNYA8        SBDNI1
     c                   Z-Add     WWCUI9        SBCUI9
     C                   Move      WWNYA9        SBDNI2
     c                   Z-Add     WWCU10        SBCUI0
     c                   Move      WWNY10        SBNYA1
     c                   Z-Add     WWCU11        SBCUI1
     C                   Move      WWNY11        SBNYAA
     c                   Z-Add     WWCU12        SBCUIB
     C                   Move      WWNY12        SBDNRC
     C                   Z-Add     AASFEI        SBFALT
     C                   Z-Add     WWHORA        SBHORA
     c                   Move      @ZUSER        SBIUSR
     c                   Move      @ZJBNM        SBITER
     c                   Move      'AC0091RG'    SBIPGM
     c                   Movel     DSARCH        SBDACO
     c                   Move      *BLANKS       SBITRL
     c                   Move      *BLANKS       SBITRB
     c                   Z-Add     *ZERO         SBFECH
     c                   Z-Add     *ZERO         SBHEMI
     c                   Move      *BLANKS       SBIUAR
     c                   Move      *BLANKS       SB$STA
     c                   Move      *BLANKS       SBIPGC
     c                   Move      *BLANKS       SBDACT
     c                   Z-Add     *ZERO         SB$PAT
     c                   Z-Add     *ZERO         SBICDD
     c                   Move      *BLANKS       SBDAEC
     c                   Move      *BLANKS       SBDAIB
     c                   Move      *BLANKS       SBESTR
     c                   Move      *BLANKS       SBIMO1
     c                   Move      *BLANKS       SBINAC
     c                   Move      *BLANKS       SBNEMP
     c                   Move      *BLANKS       SBPRBM
     c*
     c                   ExSr      ArmaNroCtrol
     c                   Move      DSPAR1        SBISTR
     c                   Move      DSPAR2        SBT400
     c*
     c                   Move      *BLANKS       SBINI1
     c                   Z-Add     *ZERO         SBFASI
     c                   Z-Add     *ZERO         SBFECO
     c                   Z-Add     *ZERO         SBFATR
     c                   Z-Add     *ZERO         SBFBAJ
     c                   Z-Add     *ZERO         SB$IMP
     c                   Z-Add     *ZERO         SB$IMR
     c                   Z-Add     *ZERO         SB$IMS
     c                   Z-Add     *ZERO         SB$IMN
     c                   Move      *BLANKS       SBDES3
     c                   Move      *BLANKS       SBDF01
     c                   Move      *BLANKS       SBDF02
     c                   Move      *BLANKS       SBDF03
     c                   Move      *BLANKS       SBDF04
     c                   Z-Add     *ZERO         SBFAAM
     c                   Z-Add     *ZERO         SBFDMA
     c                   Z-Add     *ZERO         SBFEPR
     c                   Move      *BLANKS       SBINI2
     c                   Move      *BLANKS       SBINI3
     c                   Move      *BLANKS       SBINI4
     c                   Z-Add     *ZERO         SBITI1
     c                   Z-Add     *ZERO         SBITI2
     c                   Z-Add     *ZERO         SBITI3
     c                   Z-Add     *ZERO         SBITJU
     c                   Write     RESPPCBU
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* ArmaNroCtrol: Arma Numero de Control
     C*-------------------------------------------------------------------------
     C     ArmaNroCtrol  BegSr
     C*
     c                   Move      *BLANKS       DSCTRL
     c                   Move      WWNCBU        DSNCBU
     c                   Move      MONEDA        DSMONE
     c                   Move      TIPCTA        DSTCTA
     c                   Move      WWTIPP        DSTIPP
     c                   Move      WWCATI        DSCATI
     c                   Z-Add     WWCUI1        DSCUI1
     c                   Move      WWNYA1        DSNYA1
     c                   Z-Add     WWCUI2        DSCUI2
     C                   Move      WWNYA2        DSNYA2
     c                   Z-Add     WWCUI3        DSCUI3
     C                   Move      WWNYA3        DSNYA3
     c                   Z-Add     WWCUI4        DSCUI4
     c                   Move      WWNYA4        DSNYA4
     c                   Z-Add     WWCUI5        DSCUI5
     C                   Move      WWNYA5        DSNYA5
     c                   Z-Add     WWCUI6        DSCUI6
     C                   Move      WWNYA6        DSNYA6
     c                   Z-Add     WWCUI7        DSCUI7
     c                   Move      WWNYA7        DSNYA7
     c                   Z-Add     WWCUI8        DSCUI8
     C                   Move      WWNYA8        DSNYA8
     c                   Z-Add     WWCUI9        DSCUI9
     C                   Move      WWNYA9        DSNYA9
     c                   Z-Add     WWCU10        DSCU10
     c                   Move      WWNY10        DSNY10
     c                   Z-Add     WWCU11        DSCU11
     C                   Move      WWNY11        DSNY11
     c                   Z-Add     WWCU12        DSCU12
     C                   Move      WWNY12        DSNY12
     c   25              Move      FUIGRC        DSIGRC
     c  N25              Move      BMIGRC        DSIGRC
     C   25              Move      FUISGC        DSISGC
     C  N25              Move      BMISGC        DSISGC
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c*BuscaFirmAdici
     C*-------------------------------------------------------------------------
     C     BuscaFirmAdiciBegSr
     C*
||+--C                   If        WWCUI4> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCUI4        WWCUIT           11
     c                   Move      WWNYA4        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCUI5> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCUI5        WWCUIT           11
     c                   Move      WWNYA5        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCUI6> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCUI6        WWCUIT           11
     c                   Move      WWNYA6        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCUI7> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCUI7        WWCUIT           11
     c                   Move      WWNYA7        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCUI8> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCUI8        WWCUIT           11
     c                   Move      WWNYA8        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCUI9> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCUI9        WWCUIT           11
     c                   Move      WWNYA9        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCU10> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCU10        WWCUIT           11
     c                   Move      WWNY10        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCU11> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCU11        WWCUIT           11
     c                   Move      WWNY11        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
||+--C                   If        WWCU12> *ZERO
     c                   Move      *BLANKS       WWCUIT           11
     c                   Move      *BLANKS       WWNYAP
     c                   Move      WWCU12        WWCUIT           11
     c                   Move      WWNY12        WWNYAP
     c                   ExSr      WriteLineAdici
||+--C                   EnDIf
     C*
     C                   EndSr
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
     C                   Eval      %subst(buf:011:011)=WWCUIT
     C                   Eval      %subst(buf:022:040)=WWNYAP
     C                   Eval      %subst(buf:062:139)=*ZEROS
     C                   Eval      %subst(buf:201:002)=X'0D'+X'25'
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C*WrtFooter
     C*-------------------------------------------------------------------------
     C     WrtFooter     BegSr
     C*
     C                   Z-Add     *Zeros        CountFinal
     C                   Add       Count         CountFinal
     C                   Add       1             CountFinal
     C                   Add       1             Count
     C                   Eval      buf=*blanks
     c                   Move      Count         WWCOUNT           9
     c                   Eval      %subst(buf:001:009)=WWCOUNT
     C                   Eval      %subst(buf:010:001)='T'
     c                   Eval      %subst(buf:011:003)='309'
     C                   Eval      %subst(buf:014:008)=%EditW(AASFEI:'        ')
     C                   Eval      %subst(buf:022:009)=WWCOUNT
     C                   Eval      %subst(buf:031:170)=*Zeros
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*CloseStmF
     C*-------------------------------------------------------------------------
     C     CloseStmF     BegSr
     C*
     c                   callp     close(FilHnd)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*EndPgm
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     c                   Movel     Archive       B0ISTR
     C                   Write     REBC4300
     C*
     C                   Clear                   RELISERJ
     C                   Movel     DSARCH        RJDACO
     C                   Z-add     CountFinal    RJCAN1
     C                   Z-add     AASFEI        RJFALT
     C                   Movel     @PUSER        RJIUSR
     C                   Z-add     WWHORA        RJHORA
     C                   Write     RELISERJ
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C**INZSR
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *ENTRY        PLIST
     C                   PARM                    txtfile         255
     C                   PARM                    Archive          13
     C                   PARM                    Archivh          30
     C*
     C     *LIKE         Define    OTITTL        WWITTL
     C     *LIKE         Define    AÑNYAP        WWNYAP
     C     *LIKE         Define    AÑICUI        WWCUI1
     C     *LIKE         Define    AÑNYAP        WWNYA1
     C     *LIKE         Define    AÑICUI        WWCUI2
     C     *LIKE         Define    AÑNYAP        WWNYA2
     C     *LIKE         Define    AÑICUI        WWCUI3
     C     *LIKE         Define    AÑNYAP        WWNYA3
     C     *LIKE         Define    AÑICUI        WWCUI4
     C     *LIKE         Define    AÑNYAP        WWNYA4
     C     *LIKE         Define    AÑICUI        WWCUI5
     C     *LIKE         Define    AÑNYAP        WWNYA5
     C     *LIKE         Define    AÑICUI        WWCUI6
     C     *LIKE         Define    AÑNYAP        WWNYA6
     C     *LIKE         Define    AÑICUI        WWCUI7
     C     *LIKE         Define    AÑNYAP        WWNYA7
     C     *LIKE         Define    AÑICUI        WWCUI8
     C     *LIKE         Define    AÑNYAP        WWNYA8
     C     *LIKE         Define    AÑICUI        WWCUI9
     C     *LIKE         Define    AÑNYAP        WWNYA9
     C     *LIKE         Define    AÑICUI        WWCU10
     C     *LIKE         Define    AÑNYAP        WWNY10
     C     *LIKE         Define    AÑICUI        WWCU11
     C     *LIKE         Define    AÑNYAP        WWNY11
     C     *LIKE         Define    AÑICUI        WWCU12
     C     *LIKE         Define    AÑNYAP        WWNY12
     C     *LIKE         Define    FUIGRC        WWIGRC
     C     *LIKE         Define    FUISGC        WWISGC
     C*
     C     KOSCA         KLIST
     C                   KFld                    FUISUC
     C                   KFld                    FUICCL
     C*
     C     KOTCA         KLIST
     C                   KFld                    FUISUC
     C                   KFld                    FUICCL
     C                   KFld                    WWITTL
     C*
     C     KOSCC         KLIST
     C                   KFld                    BMISUC
     C                   KFld                    BMICCL
     C*
     C     KOTCC         KLIST
     C                   KFld                    BMISUC
     C                   KFld                    BMICCL
     C                   KFld                    WWITTL
     C*
     C     KAO000        KLIST
     C                   KFld                    OSITIN
     C                   KFld                    OSININ
     C*
     C     KPF000        KLIST
     C                   KFld                    OTITDO
     C                   KFld                    OTINDO
     C*
     C     KBANU0        KLIST
     C                   KFld                    WNIPF1
     C                   KFld                    WNIPF2
     C                   KFld                    WNIPF3
     C*
     C     1             Chain     RESGSYSV
     C     @PJOBN        CHAIN(N)  @CPIUSRR                           79
     C     @PJOBN        CHAIN(N)  @CPISYS                            79
     c                   time                    WWHORA            6 0
     C                   Movel     'CAMARA'      WNIPF1
     C                   Movel     'COELSA'      WNIPF2
     C                   Movel     AASFEI        WNIPF3
     C                   Movel     *Blanks       Archivh
     C*
     C     KBANU0        Chain     REBANUME                           20
     C   20              Z-ADD     *Zeros        WNIULN
     C   20              Movel     '0'           WNIPF4
     C   20              Write     REBANUME
     C  N20              ADD       1             WNIULN
     C  N20              Movel     '0'           WNIPF4
     C  N20              Update    REBANUME
     C                   Movel     *Blanks       DSNUME
     C                   Movel     WNIULN        DSNUME
     c*
     c                   Movel     *Blanks       DSARCO
     c                   Movel     Archive       DSARCO
     c*
     c                   Movel     *Blanks       DSARCH
     c                   Movel     DSPREO        DSPREH
     c                   Movel     '-'           DSGUI1
     c                   Movel     DSIULM        DSNUMH
     c                   Movel     '-'           DSGUI2
     c                   Movel     'ENV'         DSENVH
     c                   Movel     DSEXTO        DSEXTH
     C                   Movel     DSARCH        Archivh
     c
     c*
     c                   EndSr
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
