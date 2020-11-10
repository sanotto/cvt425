*SRCMBRTXT:COELSA-Refresh de CBU - Reprocesar     
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     F*-------------------------------------------------------------------------
     FSPPCBU01  UP A E           K DISK
     FSGSYSV    IF   E             DISK
     F@CPIUSD   IF   E           K DISK
     F@CPISYS   IF   E           K DISK
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
     d MONEDA          s              3
     d OPERAC          s              1
     d TPOOPE          s              1
     d WWTIPP          s              1
     d WWNCBU          s             22
     d Count           s              9S 0
     d CountFinal      s              9S 0
     D*=====================================================================
     IRESPPCBU      01
     c   99              ExSr      OpenStmF
     c   99              ExSr      WrtHeader
     C   99              Z-Add     1             Count
     C   99              Move      *Off          *In99
     c   01              ExSr      ReProcesar
     clr                 ExSr      WrtFooter
     clr                 ExSr      CloseStmF
     clr                 ExSr      EndPgm
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
     c                   Eval      %subst(buf:025:001)=OPERAC
     c                   Eval      %subst(buf:026:175)=*ZEROS
     C                   Eval      %subst(buf:201:002)=X'0D'+X'25'
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* ReProcesar:  Reprocesar
     c*-------------------------------------------------------------------------
     c     ReProcesar    BegSr
     c*
+----c                   If        *In99=*Off
|+---C                   If        AASFEI=SBFING
||+--C                   If        SBICAP='V'
|||  c                   ExSr      GetTipoCta
|||  c                   ExSr      GetCBU
|||  c                   ExSr      BuscaFirmantes
|||  c                   ExSr      WriteLine
|||  c                   ExSr      WritePadron
|||  c                   ExSr      BuscaFirmAdici
||+--c                   EndIf
|+---c                   EndIf
+----c                   EndIf
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* GetTipoCta: Obtiene el Tipo de Cuenta
     C*-------------------------------------------------------------------------
     C     GetTipoCta    BegSr
     C*
     c                   Move      *BLANKS       MONEDA
     c                   Move      *BLANKS       TIPCTA
     c                   Move      *Blanks       TPOOPE
     C                   Move      *Blanks       WWTIPP
     C                   Z-Add     *ZERO         CANTIT            2 0
     c*
++---c                   If        SBIMON=1
|    C                   Eval      MONEDA = '032'
|+---c                   If        SBISUB='AC'
||   C                   Eval      TIPCTA = '10'
|>   c                   Else
||   C                   Eval      TIPCTA = '20'
|+---C                   Endif
>    c                   Else
|    C                   Eval      MONEDA = '840'
|+---c                   If        SBISUB='AC'
||   C                   Eval      TIPCTA = '11'
|>   c                   Else
||   C                   Eval      TIPCTA = '21'
|+---C                   Endif
+----C                   Endif
     C*
     c                   Move      SBIASK        TPOOPE
     C                   Move      SBTIPP        WWTIPP
     C                   Z-Add     SBQDCD        CANTIT
     C                   Z-Add     SBISEQ        CountFinal
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* GetCBU: Busca el CBU de la Cuenta
     C*-------------------------------------------------------------------------
     C     GetCBU        BegSr
     c*
     C                   MOVE      *Blanks       WWNCBU
     C                   MOVE      SBNCBU        WWNCBU
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* BuscaFirmantes: Busca Firmantes
     C*-------------------------------------------------------------------------
     C     BuscaFirmantesBegSr
     C*
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
     C*
     C                   Eval      WWNYA1=NormalizaName(SBNYAP)
     C                   MOVE      SBCUIA        WWCUI1
     C                   Eval      WWNYA2=NormalizaName(SBNYA2)
     C                   Eval      WWCUI2=SBCUI2
     C                   Eval      WWNYA3=NormalizaName(SBDNI3)
     C                   Eval      WWCUI3=SBCUI3
     C                   Eval      WWNYA4=NormalizaName(SBDNI4)
     C                   Eval      WWCUI4=SBCUI4
     C                   Eval      WWNYA5=NormalizaName(SBDNI5)
     C                   Eval      WWCUI5=SBCUI5
     C                   Eval      WWNYA6=NormalizaName(SBDNI6)
     C                   Eval      WWCUI6=SBCUI6
     C                   Eval      WWNYA7=NormalizaName(SBDNI7)
     C                   Eval      WWCUI7=SBCUI7
     C                   Eval      WWNYA8=NormalizaName(SBDNI1)
     C                   Eval      WWCUI8=SBCUI8
     C                   Eval      WWNYA9=NormalizaName(SBDNI2)
     C                   Eval      WWCUI9=SBCUI9
     C                   Eval      WWNY10=NormalizaName(SBNYA1)
     C                   Eval      WWCU10=SBCUI0
     C                   MOVE      SBNYAA        WXNY11
     C                   Eval      WWNY11=NormalizaName(WXNY11)
     C                   Eval      WWCU11=SBCUI1
     C                   MOVE      SBDNRC        WXNY12
     C                   Eval      WWNY12=NormalizaName(WXNY12)
     C                   MOVE      SBCUIB        WXCU12
     C                   Eval      WWCU12=WXCU12
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* WriteLine: Escribe una linea en el archivo
     c*-------------------------------------------------------------------------
     c     WriteLine     BegSr
     c*
     C                   Eval      buf=*blanks
     c                   Move      *Blanks       WWCOUNT           9
     c                   Move      *Blanks       WWNCBU           22
     c*
     c                   Move      SBISEQ        WWCOUNT
     c                   Move      SBNCBU        WWNCBU
     c*
     C                   Eval      %subst(buf:001:009)=WWCOUNT
     C                   Eval      %subst(buf:010:001)='D'
     C                   Eval      %subst(buf:011:003)='309'
     C                   Eval      %subst(buf:014:022)=WWNCBU
     C                   Eval      %subst(buf:036:003)=MONEDA
     C                   Eval      %subst(buf:039:002)=TIPCTA
     C                   Eval      %subst(buf:041:001)=TPOOPE
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
     c                   Movel     DSARCH        SBDACO
     c                   Z-Add     *ZERO         SBFECH
     c                   Z-Add     *ZERO         SBHEMI
     c                   Move      *BLANKS       SBIUAR
     c                   Move      *BLANKS       SB$STA
     c                   Move      *BLANKS       SBIPGC
     c                   Move      *BLANKS       SBINI2
     c                   Move      *BLANKS       SBDACT
     c                   Z-Add     *ZERO         SB$PAT
     c                   Z-Add     *ZERO         SBICDD
     c                   Move      *BLANKS       SBDAEC
     c                   Move      *BLANKS       SBDES3
     c                   Update    RESPPCBU
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c*BuscaFirmAdici
     C*-------------------------------------------------------------------------
     C     BuscaFirmAdiciBegSr
     C*
     C                   Z-ADD     *Zeros        Count
     c                   Move      *Blanks       WWCOUNT
     C*
+----C                   If        WWCUI4> *ZERO
|    C                   Add       3             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCUI4        WWCUIT           11
|    c                   Move      WWNYA4        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCUI5> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCUI5        WWCUIT           11
|    c                   Move      WWNYA5        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCUI6> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCUI6        WWCUIT           11
|    c                   Move      WWNYA6        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCUI7> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCUI7        WWCUIT           11
|    c                   Move      WWNYA7        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCUI8> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCUI8        WWCUIT           11
|    c                   Move      WWNYA8        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCUI9> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCUI9        WWCUIT           11
|    c                   Move      WWNYA9        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCU10> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCU10        WWCUIT           11
|    c                   Move      WWNY10        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCU11> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCU11        WWCUIT           11
|    c                   Move      WWNY11        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
+----C                   If        WWCU12> *ZERO
|    C                   Add       1             Count
|    c                   Move      *BLANKS       WWCUIT           11
|    c                   Move      *BLANKS       WWNYAP
|    c                   Move      WWCU12        WWCUIT           11
|    c                   Move      WWNY12        WWNYAP
|    c                   ExSr      WriteLineAdici
+----C                   EnDIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* WriteLineAdici: Escribe linea Adicionales
     c*-------------------------------------------------------------------------
     c     WriteLineAdiciBegSr
     c*
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
     C                   Add       1             CountFinal
     C                   Eval      buf=*blanks
     c                   Move      CountFinal    WWCOUNT           9
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
     C     *LIKE         Define    SBNYAP        WWNYAP
     C     *LIKE         Define    SBCUIA        WWCUI1
     C     *LIKE         Define    SBNYAP        WWNYA1
     C     *LIKE         Define    SBCUIA        WWCUI2
     C     *LIKE         Define    SBNYAP        WWNYA2
     C     *LIKE         Define    SBCUIA        WWCUI3
     C     *LIKE         Define    SBNYAP        WWNYA3
     C     *LIKE         Define    SBCUIA        WWCUI4
     C     *LIKE         Define    SBNYAP        WWNYA4
     C     *LIKE         Define    SBCUIA        WWCUI5
     C     *LIKE         Define    SBNYAP        WWNYA5
     C     *LIKE         Define    SBCUIA        WWCUI6
     C     *LIKE         Define    SBNYAP        WWNYA6
     C     *LIKE         Define    SBCUIA        WWCUI7
     C     *LIKE         Define    SBNYAP        WWNYA7
     C     *LIKE         Define    SBCUIA        WWCUI8
     C     *LIKE         Define    SBNYAP        WWNYA8
     C     *LIKE         Define    SBCUIA        WWCUI9
     C     *LIKE         Define    SBNYAP        WWNYA9
     C     *LIKE         Define    SBCUIA        WWCU10
     C     *LIKE         Define    SBNYAP        WWNY10
     C     *LIKE         Define    SBCUIA        WWCU11
     C     *LIKE         Define    SBNYAP        WWNY11
     C     *LIKE         Define    SBNYAP        WXNY11
     C     *LIKE         Define    SBCUIA        WWCU12
     C     *LIKE         Define    SBCUIA        WXCU12
     C     *LIKE         Define    SBNYAP        WWNY12
     C     *LIKE         Define    SBNYAP        WXNY12
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
     C                   ADD       1             WNIULN
     C                   Movel     WNIPF4        OPERAC
     C                   Update    REBANUME
     C*
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
     C                   Z-Add     *Zeros        CountFinal
     c
     c                   Move      *On           *In99             1
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
