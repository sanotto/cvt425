*SRCMBRTXT:BAHILI: Imprimir Hoja De Resumen       
     FBAHILI    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER USROPN
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*---------------------------------------------------------------------
     D file_name       S            255A
     D cmdstr          S           4096A
     D cmdlen          S             15P 5 INZ(4096)
     D*---------------------------------------------------------------------
     c                   ExSr      BuildFileName
     c                   ExSr      OpenPrinter
     c     KHL000        Chain     REBAHILI                           99
     c                   DoW       *In99 = *Off
     c                   ExSr      PrintLinea
     c     KHL000        ReadE     REBAHILI                               99
     c                   EndDo
     c                   ExSr      ClosePrinter
     C                   ExSr      EnviarCorreo
     C                   ExSr      RemoveFile
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     C     EnviarCorreo  BegSr
     c*---------------------------------------------------------------------
     C                   Move      *Blanks       Destino          10
     c*
     c/exec sql
     C+ SELECT CVIQOU INTO :Destino FROM SGUSUA WHERE CVIUSR = :@PUSER
     c/end-exec
     c*
      /free
         cmdstr= 'SNDMAIL RECP('+%TRIM(DESTINO)+')                   '+
                 '      SUBJECT(''Hoja Nro:'+CHIHUR                     +
                 ' Cuenta  :'+CHISUC+'-'+CHICCC  +''') '+
                 '         MESG(''Hoja Nro:'+CHIHUR                     +
                 '                 Cuenta  :'+CHISUC+'-'+CHICCC  +''') '+
                 '         FILE('''+%trim(file_name) +''')            ';
      /end-free
     c                   Call      'QCMDEXC'
     C                   Parm                    CmdStr
     C                   Parm                    CmdLen
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     C     BuildFileName BegSr
     c*---------------------------------------------------------------------
     c*
     c                   Move      @PJOBN        CHJOBN            6
     c                   Move      PAISUC        CHISUC            5
     c                   Move      PAICCC        CHICCC           11
     c                   Move      PAIHUR        CHIHUR            5
     c                   Eval      file_name='/home/tmp/ccresu_'+CHJOBN+
     c                                       '.pdf'
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     C     RemoveFile    BegSr
     c*---------------------------------------------------------------------
     c*
      /free
         cmdstr='RMVLNK OBJLNK('''+%TRIM(FILE_NAME)+''')';
      /end-free
     c                   Call      'QCMDEXC'
     C                   Parm                    CmdStr
     C                   Parm                    CmdLen
     c*
     c                   Open      QSYSPRT
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     C     OpenPrinter   BegSr
     c*---------------------------------------------------------------------
     c*
      /free
         cmdstr='OVRPRTF    FILE(QSYSPRT ) DEVTYPE(*AFPDS) PAGESIZE(66 '+
                '             132) LPI(6) CPI(15) OVRFLW(66) PAGRTT(0) '+
                '             DUPLEX(*YES) UOM(*CM) FRONTOVL(REACFT 0,50) '+
                '             BACKOVL(REACRV) FORMTYPE(ACRESUMEN)      '+
                '             HOLD(*YES)                               '+
                '             SAVE(*YES) USRDTA(ACRESUMEN)             '+
                '             SPLFNAME(MOSTRADOR) SHARE(*YES)          '+
                '             TOSTMF('''+%Trim(file_name)+''')         '+
                '             WSCST(*PDF)                              ';
      /end-free
     c                   Call      'QCMDEXC'
     C                   Parm                    CmdStr
     C                   Parm                    CmdLen
     c*
     c                   Open      QSYSPRT
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     C     ClosePrinter  BegSr
     c*---------------------------------------------------------------------
     c*
     c                   Close     QSYSPRT
      /free
         cmdstr='DLTOVR *ALL';
      /end-free
     c                   Call      'QCMDEXC'
     C                   Parm                    CmdStr
     C                   Parm                    CmdLen
     c*
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     C     EndPgm        BegSr
     c*---------------------------------------------------------------------
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     C     PrintLinea    BegSr
     c*---------------------------------------------------------------------
     c                   Move      *Blanks       Linea           132
     c                   Eval      Linea=%SubSt(HLDES3:5:132)
     C                   If        HLCTRE <> *BLANKS
     c                   If        HLCTRE='001'
     c                   Except    S1
     c                   Z-Add     1             CurLine           2 0
     C                   Else
     c                   Move      HLCTRE        I                 3 0
     c                   DoW       CurLine < I
     c                   Except    Blank
     c                   Add       1             CurLine
     c                   EndDo
     c                   Except    E
     c                   Move      *Blanks       Linea
     c                   EndIf
     C                   Else
     c                   Move      HLESAN        J                 1 0
     c                   DoW       J>0
     c                   Except    Blank
     c                   Sub       1             J
     c                   Add       1             CurLine
     c                   EndDo
     c                   Except    E
     c                   Move      *Blanks       Linea
     C                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR : Subrutina de inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     c*
     C     *Entry        PList
     c                   Parm                    PAIPGM
     c                   Parm                    PAIFIL
     c                   Parm                    PAFALT
     c                   Parm                    PAISUC
     c                   Parm                    PAICCC
     c                   Parm                    PAIHUR
     c*
     c     *Like         Define    HLIPGM        PAIPGM
     c     *Like         Define    HLIFIL        PAIFIL
     c     *Like         Define    HLFALT        PAFALT
     c     *Like         Define    HLISUC        PAISUC
     c     *Like         Define    HLICCC        PAICCC
     c     *Like         Define    HLIHUR        PAIHUR
     c*
     c     KHL000        KList
     c                   KFld                    PAIPGM
     c                   KFld                    PAIFIL
     c                   KFld                    PAFALT
     c                   KFld                    PAISUC
     c                   KFld                    PAICCC
     c                   KFld                    PAIHUR
     c*
     C                   EndSr
     OQSYSPRT   E            S1                1
     O                       Linea
     O          E            E           0  0
     O                       Linea
     O          E            Blank       1
