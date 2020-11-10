*SRCMBRTXT:BAHILI: ENVIO DE PLANO A LINK          
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     FBAHILI    UF   E           K DISK
     FSGSYSV    IF   E             DISK
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
     D buf             S            173
     D TIPCTA          S              2
     D WKSFEI          S              8A
     D*---------------------------------------------------------------------
     c                   ExSr      OpenStmF
     c                   Eval      WWIPGM='AC3021CL'
     c                   Eval      WWIFIL='CORREO'
     c                   ExSr      ProcSubsis
     c                   Eval      WWIPGM='CC5011CL'
     c                   Eval      WWIFIL='CC3021P1'
     c                   ExSr      ProcSubsis
     c                   ExSr      CloseStmF
     C                   ExSr      EndPgm
     D*---------------------------------------------------------------------
     c     ProcSubsis    BegSr
     c*
     c     KHL003        Chain     REBAHILI                           25
     c                   DoW       *In25=*Off
     c                   If        HLFTRA = *Zero
     c                   ExSr      EscribirLinea
     c                   Z-Add     *Date         HLFTRA
     c                   Update    REBAHILI
     c                   EndIf
     c     KHL003        ReadE     REBAHILI                               25
     c                   EndDo
     c*
     c                   EndSr
     D*---------------------------------------------------------------------
     c     EscribirLinea BegSr
     C*
     C                   Eval      buf=*blanks
     C*
     C                   ExSr      GetTipoCtaLnk
     C                   ExSr      GetCtaLnk
     C*
     C                   Eval      %subst(buf:001:008)=WKSFEI
     C                   Eval      %subst(buf:009:002)=TIPCTA
     C                   Eval      %subst(buf:011:019)=WKCTAL
     C                   Move      HLIHUR        WKIHUR            5
     C                   Eval      %subst(buf:030:005)=WKIHUR
     C                   Eval      %subst(buf:035:001)=HLAFB1
     C                   Eval      %subst(buf:036:003)=HLCTRE
     C                   Eval      %subst(buf:039:001)=HLESAN
     C                   Eval      %subst(buf:040:132)=%SubSt(HLDES3:1:132)
     C                   Eval      %subst(buf:172:002)=X'0D'+X'25'
     C*
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     GetTipoCtaLnk BegSr
     C*
     c                   Eval      TIPCTA='CC'
     C                   If        HLIPGM='AC3021CL'
     c                   Eval      TIPCTA='CA'
     C                   EndIf
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     GetCtaLnk     BegSr
     c*
     c                   Move      *Blanks       WKCTAL           19
     c                   Move      HLISUC        WKISUC            3
     c                   Move      HLICCC        WKICCC           11
     C                   Eval      WKCTAL=WKISUC+%subst(wkiccc:1:3)+
     c                                    '000' +
     c                                    %subst(wkiccc:5:7)
     c*
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
     C*
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
     C*
     C     KHL003        KList
     c                   KFld                    WWIPGM
     c                   KFld                    WWIFIL
     c                   KFld                    WWFALT
     C*
     C     *LIKE         DEFINE    HLIPGM        WWIPGM
     C     *LIKE         DEFINE    HLIFIL        WWIFIL
     C     *LIKE         DEFINE    HLFALT        WWFALT
     C*
     C     1             Chain     RESGSYSV                           25
     C*
     c                   Z-Add     AASFEI        WWFALT
     c                   Move      AASFEI        WKSFEI
     C*
     C                   EndSr
