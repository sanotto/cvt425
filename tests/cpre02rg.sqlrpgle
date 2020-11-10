*SRCMBRTXT:Archivo de compras                     
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     FMOD221LA  IF   E           K DISK
     FMOD408    IF   E           K DISK
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
     D NormalizarCUIT  PR            20A
     D  inCUIT                       15A
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D FILHND          S             10I 0
     D FILHNDD         S             10I 0
     d buf             s            327
     d bufD            s             86
     d TIPCTA          s              2
     d Count           s              8S 0
     d txtfile         s            255
     d txtfiled        s            255
     d WWNCTA          s             19
     d*
     d    C1ID220      s             15  0
     d    C1IFECH      s              8  0
     d    C1ICUIT      s             15
     d    C1INIBR      s             15
     d    C1NRSRO      s             50
     d    C1TICBT      s              6
     d    C1PTVTA      s              4  0
     d    C1INCBT      s             15  0
     d    C1CONCE      s             50
     d    C1$BRUT      s             15  2
     d    C1$IIVA      s             15  2
     d    C1OTIMP      s             15  2
     d    C1SUBTO      s             15  2
     d    C1NETDE      s             15  2
     d    C1IVADE      s             15  2
     d    C1OTIDE      s             15  2
     d    C1IVAD1      s              6
     d    C1IVAD2      s              6
     d    C1CANDE      s             15  0
     c*-------------------------------------------------------------------------
     c                   ExSr      OpenStmF
     C                   ExSr      OpenC1
     C                   ExSr      FetchC1
     C                   DoW       SQLCOD = *ZERO
     c                   ExSr      WriteRecord
     C                   ExSr      FetchC1
     C                   EndDo
     c                   ExSr      CloseStmF
     C                   ExSr      CloseC1
     c                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    FecDes            8 0
     c                   Parm                    FecHas            8 0
     c                   Parm                    txtFile         255
     c                   Parm                    txtFileD        255
     c*
     C     K2211         KList
     c                   KFld                    C1ID220
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     OpenC1        BegSr
     c*-------------------------------------------------------------------------
     c/exec sql
     c+ DECLARE C1 CURSOR FOR
     c+ SELECT
     c+   ID220,
     c+   IFNULL(A.FE220, 0)  FECHA,
     c+   IFNULL(C.NR204, '')  CUIT,
     c+   IFNULL(I.NR204, '')  IBR,
     c+   NM200 ORIGEN,
     c+   a.tv220 tipcbt,
     c+   A.PV220 PTOVTA,
     c+   A.NR220 NROCBTE,
     c+   A.DS220 CONCEPTO,
     c+   A.IM220 BRUTO,
     c+   A.IM2201 IVA,
     c+   A.IM2202 OTRIMP,
     c+   SUM(PR2211) AS SUBTOT,
     c+   SUM(PR2212) AS NETDET,
     c+   SUM(PR2213) AS IVADET,
     c+   SUM(PR2214) AS OTIDET,
     c+   MIN(IV221)  AS IVADE1 ,
     c+   MAX(IV221)  AS IVADE2,
     c+   COUNT(*)    AS CANDET
     c+ FROM MOD220 A
     c+   INNER JOIN
     c+    MOD200 ON  A.PE220= MOD200.PE200
     c+   LEFT JOIN
     c+    MOD250 ON A.CC220=CC250
     c+   LEFT JOIN
     c+     MOD221 D ON A.ID220 = D.CB221
     c+   LEFT JOIN
     c+    MOD260 ON A.TC220=TC260
     c+  LEFT JOIN
     c+    MOD204 C ON C.PE204 = A.PE220 AND C.CD204='CUIT  '
     c+  LEFT JOIN
     c+    MOD204 I ON I.PE204 = A.PE220 AND I.CD204='IBR   '
     C+    AND I.NR204 =  (
     C+    SELECT MAX(X.NR204) FROM MOD204 X WHERE X.PE204 = A.PE220
     C+    )
     c+  WHERE  a.tc220 = 'PRVCBV' and  a.sr220='PAGADA' and
     c+        A.FE220 >=  :FecDes      AND A.FE220 <= :FecHas
     c+ GROUP BY
     c+   A.ID220,
     c+   A.FE220,
     c+   DS260 ,
     c+   A.PE220,
     c+   C.NR204,
     c+   I.NR204,
     c+   NM200 ,
     c+   A.TC220 ,
     c+   a.tv220 ,
     c+   A.PV220 ,
     c+   A.NR220 ,
     c+   A.DS220 ,
     c+   A.IM220 ,
     c+   A.IM2201 ,
     c+   A.IM2202
     c+ ORDER BY
     C+     NM200
     c/end-exec
     c/exec sql
     C+ OPEN C1
     c/end-exec
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     CloseC1       BegSr
     c*-------------------------------------------------------------------------
     c/exec sql
     C+ CLOSE C1
     c/end-exec
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     FetchC1       BegSr
     c*-------------------------------------------------------------------------
     c/exec sql
     C+ FETCH C1 INTO
     c+   :C1ID220,
     c+   :C1IFECH,
     c+   :C1ICUIT,
     c+   :C1INIBR,
     c+   :C1NRSRO,
     c+   :C1TICBT,
     c+   :C1PTVTA,
     c+   :C1INCBT,
     c+   :C1CONCE,
     c+   :C1$BRUT,
     c+   :C1$IIVA,
     c+   :C1OTIMP,
     c+   :C1SUBTO,
     c+   :C1NETDE,
     c+   :C1IVADE,
     c+   :C1OTIDE,
     c+   :C1IVAD1,
     c+   :C1IVAD2,
     c+   :C1CANDE
     c/end-exec
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHnd)
     c                   EVAL      FilHnd=open(%trim(txtfile):
     c                                   O_WRONLY+O_TEXTDATA)
     C*
     c                   EVAL      FilHndD=open(%trim(txtfileD):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHndD)
     c                   EVAL      FilHndD=open(%trim(txtfileD):
     c                                   O_WRONLY+O_TEXTDATA)
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     CloseStmF     BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   callp     close(FilHnd)
     c                   callp     close(FilHndD)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     WriteRecord   BegSr
     C*-------------------------------------------------------------------------
     c                   Move      *Blanks       Buf
     C                   Eval      %subst(buf:001:008)=
     c                             %TRIM(%EDITW(C1IFECH:'0        '))
     C*
     C                   ExSr      FndCbtType
     C                   Eval      %subst(buf:009:003)=TipCbt
     c*
     C                   Move      *Zeros        PtoVta            5
     c                   Move      C1PTVTA       PtoVta
     c                   If        PtoVta = *Zeros
     C                   Move      '00001'       PtoVta            5
     c                   EndIf
     c                   Eval      %subst(buf:012:005)= PtoVta
     c*
     C                   Move      *Zeros        NroCbt           20
     c                   Move      C1INCBT       NroCbt
     c                   Eval      %subst(buf:017:020)= NroCbt
     c*
     c                   Eval      %subst(buf:053:002)='80'
     c*
     c                   Move      *Zeros        NroCuit          20
     c                   Eval      NroCuit=NormalizarCUIT(C1ICUIT)
     c                   Eval      %subst(buf:055:020)=NroCuit
     c*
     c                   Eval      %subst(buf:075:030)=%SubSt(C1NRSRO:1:30)
     c*
     c*
     c                   Eval      %subst(buf:120:015)=*Zeros
     c                   Eval      %subst(buf:135:015)=*Zeros
     c                   Eval      %subst(buf:150:015)=*Zeros
     c                   Eval      %subst(buf:165:015)=*Zeros
     c                   Eval      %subst(buf:180:015)=*Zeros
     c                   Eval      %subst(buf:195:015)=*Zeros
     c                   Eval      %subst(buf:210:015)=*Zeros
     c*
     c                   Eval      %subst(buf:225:003)='PES'
     c*
     c                   Eval      %subst(buf:228:010)='0001000000'
     c                   Eval      %subst(buf:239:001)='0'
     c                   Eval      %subst(buf:255:015)=*Zeros
     c                   Eval      %subst(buf:270:011)=*Zeros
     c                   Eval      %subst(buf:281:030)=*Blanks
     c                   Eval      %subst(buf:311:015)=*Zeros
     c*
     c                   Eval      %subst(buf:326:002)=X'0D'+X'25'
     c*
     c                   ExSr      WriteDetalle
     c                   If        C1$IIVA=0
     c                   Eval      %subst(buf:239:001)='A'
     c                   EndIf
     c                   Eval      %subst(buf:238:001)=CanAli
     C                   If        TipCbt='011'
     C                             or TipCbt='006'
     c                   Eval      %subst(buf:238:001)='0'
     c                   EndIf
     c*                  If        C1$IIVA=0 or NroCuit = *Zeros
     c*                  Eval      %subst(buf:238:001)='0'
     c*                  EndIf
     c*                  If        NroCuit > '00000000030000000000'
     c*                  Eval      %subst(buf:238:001)='1'
     c*                  EndIf
     c*
     c     neto          add       iva           TOTALN           15 2
     c                   add       perciva       TOTALN           15 2
     c                   add       percibb       TOTALN           15 2
     c                   Move      TOTALN        TOTALC           15
     c*                  Move      C1$BRUT       TOTALC           15
     c                   Eval      %subst(buf:105:015)=TOTALC
     c*
     c*                  Move      C1$IIVA       IvaC             15
     c                   Move      IVA           IvaC             15
     c*                  Eval      %subst(buf:240:015)=IvaC
     c                   Eval      %subst(buf:240:015)=*ZEROS
     C                   Move      PERCIVA       PERCIVAC         15
     C                   If        TipCbt='011'
     C                             or TipCbt='006'
     c                   Move      *Zeros        PERCIVAC
     c                   EndIf
     c                   Eval      %subst(buf:150:015)=percivac
     C                   Move      PERCIBB       PERCIBBC         15
     c                   Eval      %subst(buf:180:015)=percibbc
     c*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     WriteDetalle  BegSr
     C*-------------------------------------------------------------------------
     C                   If        TipCbt='011'
     C                             or TipCbt='006'
     c                   LeaveSr
     c                   EndIf
     c                   Z-Add     *Zero         CanAliN           1 0
     c                   Z-Add     *Zero         NET10            15 2
     c                   Z-Add     *Zero         NET21            15 2
     c                   Z-Add     *Zero         NET27            15 2
     c                   Z-Add     *Zero         NET25            15 2
     c                   Z-Add     *Zero         NETO             15 2
     c                   Z-Add     *Zero         IVA10            15 2
     c                   Z-Add     *Zero         IVA21            15 2
     c                   Z-Add     *Zero         IVA27            15 2
     c                   Z-Add     *Zero         IVA25            15 2
     c                   Z-Add     *Zero         IVA              15 2
     c                   Z-Add     *Zero         PERCIVA          15 2
     c                   Z-Add     *Zero         PERCIBB          15 2
     C     K2211         Chain     RMOD221                            25
     C                   DoW       *IN25=*Off
     C
     C                   If        TD221='RENART' and IT221= 286
     C                   ADD       PR2212        PERCIBB
     c                   GoTo      Next
     c                   EndIf
     C                   If        TD221='RENART' and IT221= 259
     C                   ADD       PR2212        PERCIVA
     c                   GoTo      Next
     c                   EndIf
     C                   If        (TD221='RENART' and IT221=  22 ) or
     C                             (TD221='RENART' and IT221=  65 ) or
     C                             (TD221='RENART' and IT221= 308 ) or
     C                             (TD221='RENART' and IT221= 309 )
     c                   Z-Add     27            TL408
     c                   If        TipCbt <> '011'
     c                   Eval      PR2212 = pr2213 / (TL408/100)
     c                   EndIf
     c                   Add       PR2212        NETO
     c                   Add       PR2212        NET27
     c                   Add       PR2213        IVA27
     c                   Add       PR2213        IVA
     c                   GoTo      Next
     c                   EndIf
     C                   If        (TD221='RENART' and IT221= 340 )
     c                   Z-Add     2.5           TL408
     c                   If        TipCbt <> '011'
     c                   Eval      PR2212 = pr2213 / (TL408/100)
     c                   EndIf
     c                   Add       PR2212        NETO
     c                   Add       PR2212        NET25
     c                   Add       PR2213        IVA25
     c                   Add       PR2213        IVA
     c                   GoTo      Next
     c                   EndIf
     C     IV221         Chain     RMOD408                            25
     c                   If        *IN25 = *Off
     c                   if        TL408=21
     c                   If        TipCbt <> '011'
     c                   Eval      PR2212 = pr2213 / (TL408/100)
     c                   EndIf
     c                   Add       PR2212        NETO
     c                   Add       PR2212        NET21
     c                   Add       PR2213        IVA21
     c                   Add       PR2213        IVA
     c                   Else
     c                   If        TipCbt <> '011'
     c                   Eval      PR2212 = pr2213 / (TL408/100)
     c                   EndIf
     c                   Add       PR2212        NETO
     c                   Add       PR2212        NET10
     c                   Add       PR2213        IVA10
     c                   Add       PR2213        IVA
     c                   EndIf
     c                   EndIf
     c     Next          Tag
     C     K2211         ReadE     RMOD221                                25
     c                   EndDo
     c*
     c                   If        IVA21 <> *ZERO
     c                   Add       1             CanAliN
     c                   Move      *Blanks       BufD
     C                   Eval      %subst(bufD:001:003)=TipCbt
     c                   Eval      %subst(bufD:004:005)=PtoVta
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:029:002)='80'
     c                   Eval      %subst(bufD:031:020)=NroCuit
     c                   Move      NET21         NETODET          15
     c                   If        TipCbt = '011'
     c                   Move      *Zeros        NETODET
     c                   EndIf
     c                   Eval      %subst(bufD:051:015)=NETODET
     c                   Eval      %subst(bufD:066:004)='0005'
     c                   Move      IVA21         IVADET           15
     c                   Eval      %subst(bufD:070:015)=IVADET
     c                   Eval      %subst(bufD:085:002)=X'0D'+X'25'
     c                   CALLP     write(FilHndD: %addr(bufD  ): %size(bufD  ))
     C                   EndIf
     c*
     c                   If        IVA10 <> *ZERO
     c                   Add       1             CanAliN
     c                   Move      *Blanks       BufD
     C                   Eval      %subst(bufD:001:003)=TipCbt
     c                   Eval      %subst(bufD:004:005)=PtoVta
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:029:002)='80'
     c                   Eval      %subst(bufD:031:020)=NroCuit
     c                   Move      NET10         NETODET          15
     c                   If        TipCbt = '011'
     c                   Move      *Zeros        NETODET
     c                   EndIf
     c                   Eval      %subst(bufD:051:015)=NETODET
     c                   Eval      %subst(bufD:066:004)='0004'
     c                   Move      IVA10         IVADET           15
     c                   Eval      %subst(bufD:070:015)=IVADET
     c                   Eval      %subst(bufD:085:002)=X'0D'+X'25'
     c                   CALLP     write(FilHndD: %addr(bufD  ): %size(bufD  ))
     C                   EndIf
     c*
     c                   If        IVA27 <> *ZERO
     c                   Add       1             CanAliN
     c                   Move      *Blanks       BufD
     C                   Eval      %subst(bufD:001:003)=TipCbt
     c                   Eval      %subst(bufD:004:005)=PtoVta
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:029:002)='80'
     c                   Eval      %subst(bufD:031:020)=NroCuit
     c                   Move      NET27         NETODET          15
     c                   If        TipCbt = '011'
     c                   Move      *Zeros        NETODET
     c                   EndIf
     c                   Eval      %subst(bufD:051:015)=NETODET
     c                   Eval      %subst(bufD:066:004)='0006'
     c                   Move      IVA27         IVADET           15
     c                   Eval      %subst(bufD:070:015)=IVADET
     c                   Eval      %subst(bufD:085:002)=X'0D'+X'25'
     c                   CALLP     write(FilHndD: %addr(bufD  ): %size(bufD  ))
     C                   EndIf
     c                   If        IVA25 <> *ZERO
     c                   Add       1             CanAliN
     c                   Move      *Blanks       BufD
     C                   Eval      %subst(bufD:001:003)=TipCbt
     c                   Eval      %subst(bufD:004:005)=PtoVta
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:029:002)='80'
     c                   Eval      %subst(bufD:031:020)=NroCuit
     c                   Move      NET25         NETODET          15
     c                   If        TipCbt = '011'
     c                   Move      *Zeros        NETODET
     c                   EndIf
     c                   Eval      %subst(bufD:051:015)=NETODET
     c                   Eval      %subst(bufD:066:004)='0009'
     c                   Move      IVA25         IVADET           15
     c                   Eval      %subst(bufD:070:015)=IVADET
     c                   Eval      %subst(bufD:085:002)=X'0D'+X'25'
     c                   CALLP     write(FilHndD: %addr(bufD  ): %size(bufD  ))
     C                   EndIf
      *
     c                   Move      CanAliN       CanAli            1
     c*
     c                   If        CanAli = '0'
     c                   Move      '1'           CanAli
     c                   Move      *Blanks       BufD
     C                   Eval      %subst(bufD:001:003)=TipCbt
     c                   Eval      %subst(bufD:004:005)=PtoVta
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:009:020)= NroCbt
     c                   Eval      %subst(bufD:029:002)='80'
     c                   Eval      %subst(bufD:031:020)=NroCuit
     C                   MOVE      *ZERO         BRUTO            15 2
     c                   EVAL      BRUTO  =NETO+IVA
     C                   MOVE      BRUTO         NETODET
     c                   If        TipCbt = '011'
     c                   Move      *Zeros        NETODET
     c                   EndIf
     c                   Eval      %subst(bufD:051:015)=NETODET
     c                   Eval      %subst(bufD:066:004)='0003'
     c                   Move      *ZERO         IVADET           15
     c                   Eval      %subst(bufD:070:015)=IVADET
     c                   Eval      %subst(bufD:085:002)=X'0D'+X'25'
     c                   CALLP     write(FilHndD: %addr(bufD  ): %size(bufD  ))
     c                   EndIf
     c*
     C                   Z-ADD     NETO          SINDEC           15 0
     C                   IF        NETO <> SINDEC
     C                   MULT      1             SINDEC
     C                   ENDIF
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     FndCbtType    BegSr
     C*-------------------------------------------------------------------------
     C                   Move      '011'         TipCbt            3
     c                   Select
     c                   When      C1TICBT='FACTUA'
     C                   Move      '001'         TipCbt            3
     c                   When      C1TICBT='FACTUB'
     C                   Move      '006'         TipCbt            3
     c                   When      C1TICBT='FACTUC'
     C                   Move      '011'         TipCbt            3
     c                   EndSl
     c*
     c                   EndSr
     P NormalizarCUIT  B
     D NormalizarCUIT  PI            20A
     D   inCUIT                      15A
     D i               S              2S 0
     D j               S              2S 0
     D outCUIT         S             20A
     D char            S              1A
     c                   Move      *Zeros        outCUIT
     c                   Z-Add     20            j
     c                   For       i = 15 DownTo 1
     c                   Eval      char = %SubSt(inCuit:i:1)
     c                   If        %scan(char:'0123456789')<>0
     c                   Eval      %SubSt(outCuit:j:1)=char
     c                   Eval      j=j-1
     c                   EndIf
     c                   EndFor
     c                   Return    outCUIT
     P NormalizarCUIT  E
