*SRCMBRTXT:Gen. de Archivo Refresh de Recibos de A
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DEBUG DATEDIT(*YMD)
     F*----------------------------------------------------------------*
     F*  ANLI08R1                                                      *
     F*  REFRESH DE RECIBOS DE ANSES PARA LINK                         *
     F*
     F*  CREADO     POR: S. OTTONELLO     - PR00525 - 27.04.2011
     F*----------------------------------------------------------------*
     FSGSYSV    IF   E           K DISK
     F*  The system values file
     F*----------------------------------------------------------------*
     F@CPISYS   IF   E           K DISK
     F*  Program interface containing system values.
     F*----------------------------------------------------------------*
     F@CPIUSD   IF   E           K DISK
     F*  Program interface containing user values.
     F*----------------------------------------------------------------*
     FANLIHA15  IF   E           K DISK
     F*  Registro unico de liquidacion de haberes
     F*----------------------------------------------------------------*
     FACCTAC    IF   E           k DISK
     F*  Registro unico de liquidacion de haberes
     F*----------------------------------------------------------------*
     FBAENTI    IF   E           K DISK
     F*  Datos generales de la entidad
     F*----------------------------------------------------------------*
     FANCODI    IF   E           K DISK
     F*  Códigos de pago / descuento ANSES
     F*----------------------------------------------------------------*
     D  NormalizaStr   PR           360A
     D    Name                      360A
     I*  Program status data structure.
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
     F*----------------------------------------------------------------*
     D*  Function keys from common program interface                   *
     D*----------------------------------------------------------------*
     D* Series utilizadas para datos de campos
     D @DES            S             16    DIM(25)
     D TABCOD          S              3  0 DIM(80) CTDATA PERRCD(1)
     D TABEMP          S              3  0 DIM(80) ALT(TABCOD)
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D                 DS
     D  WRKARA                 1   3600
     D  LINREC                 1   3600
     D                                     DIM(100)
     D  PAGREC                 1   3600
     D                                     DIM(10)
     D
     D*----------------------------------------------------------------*
     D* Estructura de datos para @DES
     D                 DS
     D  DESCUE                 1     16
     D  WWCONC                 1      3  0
     D  WWEMPR                 4      6  0
     D  WWIMPO                 7     16  2
     D* Estructura de datos para armar la hora HHMM
     D                 DS
     D  HORA                   1      6  0
     D  WWHORA                 1      4  0
     D* Estructura de datos para armar la fecha DDMMAA
     D                 DS
     D  FECHA                  1      8  0
     D  DIA                    1      2  0
     D  MES                    3      4  0
     D  SIGLO                  5      6  0
     D  AÑO                    7      8  0
     D  WWFECH                 9     14  0
     D  WWDIA                  9     10  0
     D  WWMES                 11     12  0
     D  WWAÑO                 13     14  0
     D* Estructura de datos para descuentos del ANSES
     D                 DS
     D  WWKEYA                 1     27
     D  WWCUIA                 1     11  0
     D  WWIECJ                12     13  0
     D  WWITBE                14     14  0
     D  WWINBE                15     21  0
     D  WWIICP                22     22  0
     D  WWIDVA                23     23  0
     D  WWIPEA                24     27  0
     D* Estructura de datos para Concepto Pago / Desc.
     D                 DS
     D  CODCON                 1      6  0
     D  WWCON1                 1      3  0
     D  WWEMP1                 4      6  0
     D                 DS
     D  WXCODI                 1      3  0
     D  WXCONC                 1      3  0
     D  WXEMPR                 4      6  0
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*  Fetch external description of Local Data Area
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     d buf             s            460
     d TIPCTA          s              2
     d Count           s              8S 0
     d txtfile         s            255
     d WWNCTA          s             19
     D*---------------------------------------------------------------------
     d*-------------------------------------------------------------------------
     D CTADS           DS
     D  CTALNK                 1     19
     D  LINSUC                 1      3  0
     D  LINFI2                 4      4  0
     D  LINGRP                 5      6
     D  LINFIL                 7      9  0
     D  LINCAH                10     16  0
     I*----------------------------------------------------------------*
     IANLIHA15  AA
     I                                  1    2 0ANIGRP
     I                                  3   10 0ANFDPA
     I                                 11   18 0ANFHPA
     I                                 19   29 0ANCUIA
     I                                 30   31 0ANIECJ
     I                                 32   32 0ANITBE
     I                                 33   39 0ANINBE
     I                                 40   40 0ANIICP
     I                                 41   41 0ANIDVA
     I                                 42   44 0ANICBP
     I                                 45   47 0ANIABP
     I                                 48   74  ANNYAA
     I                                 75   75 0ANITDA
     I                                 76   83 0ANINDA
     I                                 84   85 0ANIPRA
     I                                 86   96 0ANICUA
     I                                 97  123  ANNAAP
     I                                124  124 0ANITDP
     I                                125  132 0ANINDP
     I                                133  134 0ANIPRB
     I                                135  534  @DES
     I                                135  137 0ANCC01
     I                                138  140 0ANCE01
     I                                141  150 2ANIM01
     I                                151  153 0ANCC02
     I                                154  156 0ANCE02
     I                                157  166 2ANIM02
     I                                167  169 0ANCC03
     I                                170  172 0ANCE03
     I                                173  182 2ANIM03
     I                                183  185 0ANCC04
     I                                186  188 0ANCE04
     I                                189  198 2ANIM04
     I                                199  201 0ANCC05
     I                                202  204 0ANCE05
     I                                205  214 2ANIM05
     I                                215  217 0ANCC06
     I                                218  220 0ANCE06
     I                                221  230 2ANIM06
     I                                231  233 0ANCC07
     I                                234  236 0ANCE07
     I                                237  246 2ANIM07
     I                                247  249 0ANCC08
     I                                250  252 0ANCE08
     I                                253  262 2ANIM08
     I                                263  265 0ANCC09
     I                                266  268 0ANCE09
     I                                269  278 2ANIM09
     I                                279  281 0ANCC10
     I                                282  284 0ANCE10
     I                                285  294 2ANIM10
     I                                295  297 0ANCC11
     I                                298  300 0ANCE11
     I                                301  310 2ANIM11
     I                                311  313 0ANCC12
     I                                314  316 0ANCE12
     I                                317  326 2ANIM12
     I                                327  329 0ANCC13
     I                                330  332 0ANCE13
     I                                333  342 2ANIM13
     I                                343  345 0ANCC14
     I                                346  348 0ANCE14
     I                                349  358 2ANIM14
     I                                359  361 0ANCC15
     I                                362  364 0ANCE15
     I                                365  374 2ANIM15
     I                                375  377 0ANCC16
     I                                378  380 0ANCE16
     I                                381  390 2ANIM16
     I                                391  393 0ANCC17
     I                                394  396 0ANCE17
     I                                397  406 2ANIM17
     I                                407  409 0ANCC18
     I                                410  412 0ANCE18
     I                                413  422 2ANIM18
     I                                423  425 0ANCC19
     I                                426  428 0ANCE19
     I                                429  438 2ANIM19
     I                                439  441 0ANCC20
     I                                442  444 0ANCE20
     I                                445  454 2ANIM20
     I                                455  457 0ANCC21
     I                                458  460 0ANCE21
     I                                461  470 2ANIM21
     I                                471  473 0ANCC22
     I                                474  476 0ANCE22
     I                                477  486 2ANIM22
     I                                487  489 0ANCC23
     I                                490  492 0ANCE23
     I                                493  502 2ANIM23
     I                                503  505 0ANCC24
     I                                506  508 0ANCE24
     I                                509  518 2ANIM24
     I                                519  521 0ANCC25
     I                                522  524 0ANCE25
     I                                525  534 2ANIM25
     I                                535  546 2AN$THA
     I                                547  558 2AN$DEA
     I                                559  570 2AN$LQA
     I                                571  571 0ANIMA1
     I                                572  575 0ANIPEA
     I                                576  577 0ANITPA
     I                                578  578 0ANIFPA
     I                                579  579 0ANITCB
     I                                580  599  ANNCBA
     I                                600  607 0ANIFPP
     I                                608  615 0ANIFPH
     I                                616  653  ANLR01
     I                                654  691  ANLR02
     I                                692  729  ANLR03
     I                                730  767  ANLR04
     I                                768  805  ANLR05
     I                                806  843  ANLR06
     I                                844  881  ANLR07
     I                                882  919  ANLR08
     I                                920  920 0ANCPIA
     I                                921  926 0ANIFEA
     I                                927  927 0ANIPTA
     I                                928  928 0ANIMIM
     I                                929  935 0ANINCP
     I                                936  943 0ANIFEU
     I                                944  944 0ANIRET
     I                                947  947  ANCOMA
     I                                948  952 1ANCAUR
     I                                953  962 2AN$AF1
     I                                963  972 2AN$AF2
     I                                973  984 2AN$LQ2
     I                                985 1022  ANRENG
     I                             P 1023 1026 0ANFEPR
     I                             P 1027 1030 0ANHORA
     I                             P 1031 1033 0ANICAJ
     I                             P 1034 1037 0ANIUTI
     I                             P 1038 1043 0ANICAH
     I                               1044 1053  ANIUSR
     I                             P 1054 1056 0ANISUC
     C*----------------------------------------------------------------*
     c
     c                   Z-Add     *ZERO         WKISUC
     c                   Z-Add     *ZERO         WKICAH
     c                   Z-Add     *ZERO         Count
     C                   Read      ANLIHA15                               99
     C                   DoW       *IN99 = *OFF
     c                   If        ANICAH <> 0
     c     KFU000        Chain     REACCTAC                           99
     c                   If        *IN99 = *OFF and FUIUCA='S'
     c                   If        ANISUC <> WKISUC or ANICAH <> WKICAH
     C                   If        WKICAH <> *ZERO
     c                   ExSr      WriteVector
     C                   EndIf
     c                   Clear                   LINREC
     C                   Z-Add     *ZERO         I                 2 0
     c                   Z-Add     ANISUC        WKISUC
     c                   Z-Add     ANICAH        WKICAH
     C                   ExSr      GetCtaLink
     c                   EndIf
     c                   ExSr      BuildVector
     c                   EndIf
     c                   EndIf
     C                   Read      ANLIHA15                               99
     C                   EndDo
     c                   ExSr      WrtFilFtr
     C                   ExSr      EndPgm
     C*
     C*-------------------------------------------------------------------------
     c     WrtFilHdr     BegSr
     C*-------------------------------------------------------------------------
     C*
     C                   Eval      buf=*blanks
     C                   Eval      %subst(buf:001:006)='HR_ICP'
     C                   Eval      %subst(buf:007:004)='0309'
     C                   Eval      %subst(buf:011:006)='999999'
     C                   Eval      %subst(buf:017:008)=%EDITW(AASFEI:'        ')
     C                   Eval      %subst(buf:025:436)=*BLANKS
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     WrtFilFtr     BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Incluir Registros de Pie y Cabecera en la Cantidad
     c                   Add       2             Count
     C                   Eval      buf=*blanks
     C                   Eval      %subst(buf:001:006)='TR_ICP'
     C                   Eval      %subst(buf:007:008)=%TRIM(
     C                                                %EDITW(Count:'0        ')
     C                                                 )
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     WriteVector   BegSr
     C*-------------------------------------------------------------------------
     C     I             DIV       10            CANPAG            2 0
     C                   IF        %REM(I:10) <> 0
     C                   ADD       1             CANPAG
     C                   ENDIF
     C                   Z-ADD     *ZERO         J                 2 0
     C                   FOR       J= 1 TO CANPAG
     C                   Eval      buf=*blanks
     C                   Eval      %subst(buf:001:002)='11'
     C                   Eval      %subst(buf:003:019)=WWNCTA
     C                   Eval      %subst(buf:022:008)='00000000'
     C                   Eval      %subst(buf:030:008)='00000000'
     c                   Z-Add     ANINBE        AXINBE           12 0
     C                   MOVE      AXINBE        CODBEN           12
     C                   Eval      %subst(buf:038:012)=CODBEN
     C                   Z-ADD     ANINDA        AXICUI           11 0
     C                   MOVE      AXICUI        ANICUI           11
     C                   Eval      %subst(buf:050:011)=ANICUI
     C                   Eval      %subst(buf:061:002)=%TRIM(%EDITW(J:'0  '))
     C                   Eval      %subst(buf:063:002)=%TRIM(%EDITW(CANPAG:
     C                                                  '0  '))
     C                   Eval      %subst(buf:065:036)='CBTE. PREVISIONAL PAG.'+
     C                             ':'+%TRIM(%EDITW(J     :
     c                              '0  '))+ ' DE ' +
     c                             %TRIM(%EDITW(CANPAG:
     c                              '0  '))
     C                   Eval      %subst(buf:101:360)=NormalizaStr(PAGREC(J))
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c                   Add       1             Count
     C*
     C                   ENDFOR
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     BldRecHdr     BegSr
     C*-------------------------------------------------------------------------
     C*
      /FREE
       //         0        1         2         3
       //         123456789012345678901234567890123456
       I=I+1;
       LINREC(I)='MTEySS,SSS/ANSES CUIT: 33-63761744-9';
       I=I+2;
       LINREC(I)=MLNBAN;
       I=I+1;
       LINREC(I)='SUC.:'+WWISUC+' AGENCIA ANSES:'+%EDITW(ANICBP:' 0 ')+'-'+
                                                  %EDITW(ANIABP:' 0 ');
       I=I+2;
       LINREC(I)='TITULAR:'+ANNYAA;
       I=I+1;
       LINREC(7)='DOC:'+%EDITW(ANITDA:'  ')+'-'+%EDITW(ANINDA:'  .   .   ')+
                 ' CUIL:'+%EDITW(ANCUIA:'  -        - ');
       I=I+1;
       LINREC(8)='APODERADO:'+ANNAAP;
       I=I+1;
       LINREC(9)='DOC.:'+%EDITW(ANITDP:'  ')+'-'+%EDITW(ANINDP:'  .   .   ');
       I=I+2;
       LINREC(11)='BENEFICIO:'+ %TRIM(%EDITW(ANIECJ:'0 ')      ) +'-'+
                                %TRIM(%EDITW(ANITBE:'0 ')      ) +'-'+
                                %TRIM(%EDITW(ANINBE:'0       ')) +'-'+
                                %TRIM(%EDITW(ANIICP:'0 ')      ) +'-'+
                                %TRIM(%EDITW(ANIDVA:'0 ')      )  ;
       I=I+1;
       LINREC(12)='PERIODO LIQUIDADO:'+%EDITW(ANIPEA:'  /  ');
       I=I+1;

      /END-FREE
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*-------------------------------------------------------------------------
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetCtaLink    BegSr
     C*-------------------------------------------------------------------------
     C*
     C                   Z-ADD     FUISUC        LINSUC
     C                   MOVE      FUIGRC        LINGRP
     C                   Z-ADD     FUICAH        LINCAH
     C                   Z-ADD     *ZEROS        LINFIL
     C                   Z-ADD     *ZEROS        LINFI2
     C
     C                   MOVE      CTALNK        WWNCTA
     C*
     C                   EndSr
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C*----------------------------------------------------------------*
     C*  ........get the LDA
     C                   IN        LDA
     C*  ........LDA data structure as *LDA
     C     *DTAARA       DEFINE    *LDA          LDA
     C*  ........Get the program starting date and time
     C     *LOVAL        SETLL     SGSYSV
     C                   READ      SGSYSV                                 99
     C                   MOVE      '00000'       WWISUC            5
     C                   MOVE      ANISUC        WWISUC
     C* .........Arma la fecha del sistema con formato DDMMAA
     C                   Z-ADD     AASFEN        FECHA
     C                   Z-ADD     DIA           WWDIA
     C                   Z-ADD     MES           WWMES
     C                   Z-ADD     AÑO           WWAÑO
     C*  ........Accede a información sobre la entidad
     C     *LOVAL        SETLL     BAENTI
     C                   READ      BAENTI                                 99
     C*
     c     KFU000        KLIST
     C                   KFLD                    ANISUC
     C                   KFLD                    ANICAH
     C*
     C                   Z-ADD     *ZEROS        WWRRNO           11 0
     c*
     c     *LIKE         DEFINE    ANISUC        WKISUC
     c     *LIKE         DEFINE    ANICAH        WKICAH
     c*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Formato Nombre ibbbeeeeevaammdd
     C*     -I         (Mayúscula) Valor Fijo
     C*     -bbb       Código Banco BCRA
     C*     -eeeee     Entidad             (99999 Para ANSES)
     C*     -V         Nro de Versión en el día (Por si se manda mas de una vez)
     C*     -aammdd    Fecha de Envío
     C*
     c                   MOVE      AASFEI        FECHA6            6
     c                   EVAL      txtfile='/home/LINKBEE/REFRESH/'+
     c                                     'I30999991'+FECHA6
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
     C     CloseStmF     BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   callp     close(FilHnd)
     C*
     C                   EndSr
     C*----------------------------------------------------------------
     C* FOR003   Formulario FORM003
     C*----------------------------------------------------------------
     C     BuildVector   BEGSR
     C*----------------------------------------------------------------
     C* Impresión de cabecera de la pisada                           %TRIM(
     C                   ExSr      BldRecHdr
     C* Impresión de detalle de la pisada                            %TRIM(
     C                   SETOFF                                       969798
     C                   SETOFF                                       616263
     C                   Z-ADD     *ZEROS        WXIMPO            7 2
     C                   Z-ADD     *ZEROS        WX$LQ2            7 2
     C     1             DO        25            N                 2 0
     C                   MOVEL     @DES(N)       DESCUE
     C                   Z-ADD     WWCONC        CONC01            3 0
     C                   Z-ADD     WWEMPR        EMPR01            3 0
     C                   Z-ADD     WWCONC        WWCON1
     C                   Z-ADD     WWEMPR        WWEMP1
     C                   MOVE      *OFF          *IN61
     C                   Z-ADD     *ZEROS        WXCODI
     C                   Z-ADD     WWCONC        WXCONC
     C     WXCONC        LOOKUP    TABCOD        TABEMP                   66
     C     *IN66         IFEQ      *ON
     C                   MOVE      TABEMP        WXEMPR
     C     WXEMPR        IFEQ      999
     C                   MOVE      *ON           *IN61
     C                   MOVE      *ON           *IN62
     C                   ADD       WWIMPO        WXIMPO            7 2
     C                   GOTO      SIGO
     C                   ENDIF
     C     WXEMPR        IFEQ      WWEMPR
     C                   MOVE      *ON           *IN61
     C                   MOVE      *ON           *IN62
     C                   ADD       WWIMPO        WXIMPO            7 2
     C                   ENDIF
     C                   ENDIF
     C     SIGO          TAG
     C                   Z-ADD     CODCON        WWICCU            6 0
     C     WWICCU        CHAIN     REANCODI                           95
     C     *IN95         IFEQ      *ON
     C                   MOVEL     *BLANKS       CDDTXT
     C                   ENDIF
     C     WWCONC        IFGE      200
     C     WWCONC        ANDLE     699
     C                   SETON                                        96
     C                   ELSE
     C                   SETOFF                                       96
     C                   END
     C                   Z-ADD     WWIMPO        IMPO01            7 2
     C     CONC01        IFEQ      *ZEROS
     C                   GOTO      PIE
     C                   ELSE
     c                   ADD       1             I
     C                   MOVE      ' $ '         SIMBOLO           3
     C                   IF        *IN61=*ON
     C                   MOVE      '(*)'         SIMBOLO           3
     C                   ENDIF
     C                   MOVE      *BLANKS       IMPCHAR          12
     C                   EVAL      IMPCHAR  =' '+
     C                             %EDITW(IMPO01: '     0 ,  ') +' '
     C                   IF        *IN96 = *ON
     C                   EVAL      IMPCHAR  ='('+
     C                             %EDITW(IMPO01: '     0 ,  ') +')'
     C                   ENDIF
     C                   EVAL      LINREC(I)= %TRIM(%EDITW(CONC01:'0   '))+'-'+
     c                                        %TRIM(%EDITW(EMPR01:'0   '))+' '+
     C                                         %SUBST(CDDTXT:1:13)+
     C                                         SIMBOLO+IMPCHAR
     C                   Z-ADD     *ZEROS        CONC01
     C                   Z-ADD     *ZEROS        EMPR01
     C                   Z-ADD     *ZEROS        IMPO01
     C                   ENDIF
     C* Impresión de pie de la pisada                                %TRIM(
     C     PIE           TAG
     C     AN$AF1        IFNE      *ZEROS
     C     AN$AF2        ORNE      *ZEROS
     C                   SETON                                        97
     C                   END
     C   62AN$LQ2        SUB       WXIMPO        WX$LQ2
     C   62WX$LQ2        IFLT      *ZEROS
     C                   SETON                                        63
     C                   ENDIF
     C                   ENDDO
     C                   ExSr      BldRecFtr
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     BldRecFtr     BegSr
     C*
      /FREE
       I=I+1;
       I=I+1;
       LINREC(I)= 'TOTAL HABERES   :$ '+%EDITW(AN$THA:' .   .   .   ,  ');
       I=I+1;
       LINREC(I)= 'TOTAL DESCUENTOS:$('+%EDITW(AN$DEA:' .   .   .   ,  ')+')';
       I=I+1;
       LINREC(I)= 'NETO ANSES      :$('+%EDITW(AN$LQA:' .   .   .   ,  ')+')';
       IF *IN97 = *ON;
         I=I+1;
         LINREC(I)= 'CTA MORAT.AFIP:$('+%EDITW(AN$AF1:' .   .   .   ,  ')+')';
         I=I+1;
         LINREC(I)= 'RETROAC.MOR.AFIP:$('+%EDITW(AN$AF2:' .   .   .   ,  ')+')';
       ENDIF;
       I=I+1;
       LINREC(I)=   'NETO A COBRAR   :$ '+%EDITW(AN$LQ2:' .   .   .   ,  ')+' ';
       IF *IN62 = *ON;
         I=I+1;
         LINREC(I)= 'DESC P/SCOR(*)  :$'+%EDITW(WXIMPO:' .   .   .   ,  ')+' ';
       IF *IN63 = *ON;
       I=I+1;
       LINREC(I)=   'NETO P/SCOR(*)  :$ '+%EDITW(WX$LQ2:' .   .   .   ,  ')+' ';
       ELSE;
       I=I+1;
       LINREC(I)=   'NETO P/SCOR(*)  :$('+%EDITW(WX$LQ2:' .   .   .   ,  ')+')';
       ENDIF;
       ENDIF;
       I=I+1;
       I=I+1;
       LINREC(I)= 'FECHA PROX. COBRO DESDE:'+ %EDITW(ANIFPP:'  /  /    ');
      /END-FREE
     C*
     C                   EndSr
     C*=========================================================================
     P NormalizaStr    B
     D  NormalizaStr   PI           360A
     D    Name                      360A
     D*
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D Symbols         C                   '|°¬/¡¿~[]{}'
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
     P NormalizaStr    E
     C*----------------------------------------------------------------
** Codigos ANSES
002000
022022
135000
061000
076000
010000
102000
060000
170000
195999
060999
022999
108999
116999
123999
124999
125999
126999
700999
106025
133033
058000
133035
