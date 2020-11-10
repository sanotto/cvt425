*SRCMBRTXT:LINK:TRANSF. ENTRE REDES-GENERA PLANO  
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     FLITERH01  UF   E           K DISK
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
     d buf             s            218
     d TIPCTA          s              2
     d Count           s              9S 0
     D*---------------------------------------------------------------------
     c                   ExSr      OpenStmF
     c                   ExSr      WrtHeader
     C                   Z-Add     *ZERO         Count
     C     KTM000        Chain     RELITERH                           99
     C                   DoW       *IN99 = *OFF
     C*
     C                   Eval      buf=*blanks
     C                   Eval      %subst(buf:001:001)=TMIASK
     C                   Eval      %subst(buf:002:022)=%editc(TMNCBU:'Z')
     C                   ExSr      GetTipoCtaLnk
     C                   Eval      %subst(buf:024:002)=TIPCTA
     C                   MoveL     TMNCTA        CHRCTL           19
     C                   Eval      %subst(buf:026:019)=CHRCTL
     c                   Move      TMISUC        CHRSUC            5
     c                   Move      *ZEROS        CHRCT1           12
     c                   Move      TMICCC        CHRCT1
     c                   MoveL     CHRCT1        CHRCT2           11
     c                   Move      CHRCT1        CHRCT3            1
     C                   Eval      %subst(buf:045:019)= CHRSUC+'-'+CHRCT2+'/'+
     c                                                  CHRCT3
     C                   Eval      %subst(buf:064:001)=TMCSTS
     C                   Eval      %subst(buf:065:001)=TMTIPP
     C                   Eval      %subst(buf:066:011)=%editc(TMCUI1:'Z')
     C                   Eval      %subst(buf:077:040)=TMNYAP
     C                   Eval      %subst(buf:117:011)=%editc(TMCUI2:'Z')
     C                   Eval      %subst(buf:128:040)=TMNYA1
     C                   Eval      %subst(buf:168:011)=%editc(TMCUI3:'Z')
     C                   Eval      %subst(buf:179:040)=TMNYA2
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C                   Add       1             Count
     C*
     C                   Move      'H'           TMACTL
     C                   Update    RELITERH
     C*
     C     KTM000        ReadE     RELITERH                               99
     C                   EndDo
     c                   ExSr      WrtFooter
     c                   ExSr      CloseStmF
     C                   Z-Add     Count         CanReg
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C     GetTipoCtaLnk BegSr
     C*
     C                   Select
     c                   When      TMIMON=1 and TMISUB ='AC'
     C                   Eval      TIPCTA = '11'
     c                   When      TMIMON=2 and TMISUB ='AC'
     C                   Eval      TIPCTA = '15'
     c                   When      TMIMON=1 and TMISUB ='CC'
     C                   Eval      TIPCTA = '01'
     c                   When      TMIMON=2 and TMISUB ='CC'
     C                   Eval      TIPCTA = '07'
     C                   EndSl
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtHeader     BegSr
     C*
     C                   Eval      %subst(buf:001:005)='HRCBU'
     C                   Eval      %subst(buf:006:009)='0001.0000'
     C                   Eval      %subst(buf:015:004)='0309'
     C                   Eval      %subst(buf:019:200)=*BLANKS
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtFooter     BegSr
     C*
     C                   Eval      %subst(buf:001:005)='TRCBU'
     C                   Add       2             Count
     C                   Move      Count         CNTCHR            9
     C                   Eval      %subst(buf:006:009)=CNTCHR
     C                   Eval      %subst(buf:015:204)=*BLANKS
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
     C                   PARM                    WWIDAR
     C                   PARM                    CanReg           15 0
     C*
     C     *LIKE         Define    TMACTL        WWACTL
     C     *LIKE         Define    TMFASI        WWFASI
     C     *LIKE         Define    tmidar        WWidar
     C*
     C     KTM000        KLIST
     C                   KFld                    WWACTL
     C                   KFld                    WWFASI
     C                   KFld                    WWIDAR
     C*
     C                   MOVE      'N'           WWACTL
     C                   Z-ADD     *DATE         WWFASI
     C*
     C                   EndSr
