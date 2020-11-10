*SRCMBRTXT:Enviar reportes por MAIL               
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD   ')
     HDATEDIT(*YMD) DATFMT(*YMD)

     FBADREP    IF   E           K DISK
     FSGUSUA    IF   E           K DISK
     FREPERS    IF   E           K DISK
     FBAPFDA    IF   E           K DISK

     D/copy LE00525/sockets,socket_h
     D/copy LE00525/sockets,ERRNO_H
     D/copy LE00525/sockets,sockutil_h
     D/copy LE00525/to10US,TOS1PS0RI

     D die             PR
     D   peMsg                      256A   const
     D sndmsg          PR
     D   peMsg                      256A   const
     D  Shell          PR             7A
     D   Command                   1024A   VALUE


     C*-------------------------------------------------------------------------
     C* Variables Locales
     C*-------------------------------------------------------------------------
     D rc              S              7A
     D flatt           S            255A
     D frmmbr          S            255A
     D cmd             S           1024A

     C*-------------------------------------------------------------------------
     C* Main Program
     C*-------------------------------------------------------------------------
     c                   If        USR <> '*ALL'
     c                   Eval      TOUSER=USR
     c                   EndIf
     c*
     c                   ExSr      BuildReport
     c*
     C     TOUSER        Chain     REBADREP                           99
     C                   If        *In99 = *ON and USR <> '*ALL'
     c                   ExSr      SendToAddress
     c                   Else
     c                   ExSr      SendToGroup
     c                   EndIf
     C*
     c                   If        flatt <> ''
     c                   Eval      rc=Shell('RMVLNK OBJLNK('''+%Trim(flatt)+
     c                             ''')' )
     c                   EndIf
     C*
     C                   ExSr      ENDPGM

     c*------------------------------------------------------------
     c* BuildReport : Si existe el arch.qtemp/report gen una stmf
     c*------------------------------------------------------------
     c     BuildReport   BegSr
     c*
     c                   Eval      flatt=''
     c*
     c                   Eval      rc=Shell('CHKOBJ OBJ(QTEMP/REPORTE) '+
     c                                      '       OBJTYPE(*FILE)     ')
     c                   If        rc = 'CPF9801'
     c                   LeaveSr
     c                   EndIf
     C*
     c                   ExSr      BuildFileName
     c                   ExSr      ExpPcDbf
     c*
     c                   EndSr
     c*------------------------------------------------------------
     c* ExpPcDbf: Exporta el reporte como un archivo de Excel
     c*------------------------------------------------------------
     c     ExpPcDbf      BegSr
     c*
     c*                  Eval      cmd='EXPPCDBF SQLSTM('''+
     c*                            'select * from qtemp/reporte'  +
     c*                                ''') OUTPAT('''+%Trim(Flatt)+''')'
     c*
     c                   Eval      frmmbr='/QSYS.LIB/QTEMP.LIB/'+
     c                                    'REPORTE.FILE/REPORTE.MBR'
     c*
     c                   Eval      cmd='CPYTOSTMF                          '+
     c                                 'FROMMBR('''+%Trim(FRMMBR)+''')     '+
     c                                 'TOSTMF('''+%Trim(Flatt)+''')       '+
     c                                 'STMFOPT(*REPLACE)                  '+
     c                                 'STMFCCSID(*PCASCII)                '
     c
     c*
     c                   Eval      rc=Shell(cmd)
     c*
     c                   EndSr
     c*------------------------------------------------------------
     c* BuildFileName: Crea un nombre para el archivo de Attach
     c*------------------------------------------------------------
     c     BuildFileName BegSr
     c*
     c                   Move      @PJOBN        CHJOBN            6
     c                   Time                    AUHORA            6 0
     c                   Move      AUHORA        CHHORA            6
     c                   Move      *Date         CHFECH            8
     c*
     c                   Eval      flatt='/tmp/attach'+
     c                                   '_' + %Trim(CHFECH) +
     c                                   '_' + %Trim(CHHORA) +
     c                                   '_' + %Trim(CHJOBN) +
     c                                   '.txt'
     c*
     c                   EndSr
     c*------------------------------------------------------------
     c* SendToAddres: Envía un mail a una dirección
     c*------------------------------------------------------------
     c     SendToAddress BegSr
     c*
     C     TOUSER        Chain     SGUSUA                             99
     C  N99CVILEG        Chain     REREPERS                           99
     C  N99KPFDA         Chain     REBAPFDA                           99
     c                   If        *in99 = *off and qqcoel <> *blanks
     C                   Eval      TOMAIL=QQCOEL
     c                   EndIf
     c*
     C                   ExSr      SndEml
     c*
     C                   EndSr

     c*------------------------------------------------------------
     c* SendToGroup : Envía un mail a un grupo
     c*------------------------------------------------------------
     c     SendToGroup   BegSr
     c*
     C                   DoW       *In99 = *OFF
     C     M4IUSR        Chain     SGUSUA                             99
     C  N99CVILEG        Chain     REREPERS                           99
     C  N99KPFDA         Chain     REBAPFDA                           99
     C  N99              EVAL      TOMAIL=QQCOEL
     C  N99              EXSR      SNDEML
     C     TOUSER        ReadE     REBADREP                               99
     C                   EndDo
     c*
     c                   EndSr

     c*------------------------------------------------------------
     c* *INZSR : Inicialización
     c*------------------------------------------------------------
     c     *INZSR        BegSr
     c*
     C     *ENTRY        PLIST
     C                   PARM                    USR              10
     C                   PARM                    TOUSER           10
     C                   PARM                    subject          50
     C                   PARM                    msg            1024
     c*
     C                   Move      *Blanks       TOMAIL           50
     c*
     C     KPFDA         KLIST
     C                   KFLD                    CQITDO
     C                   KFLD                    CQINDO
     c*
     c                   EndSr


     C*--------------> Finalizar programa y cerrar todo lo que hubiera abierto
     C     ENDPGM        BEGSR
     C                   EVAL      *INLR=*ON
     C                   RETURN
     C                   ENDSR
     c*
     C*-------------------------------------------------------------------------
     C     SNDEML        BEGSR
     c*
     c                   MoveL(P)  TOMAIL        To
     c                   MoveL(P)  subject       Subject
     c                   MoveL(P)  msg           Body
     c                   MoveL(P)  flatt         Attach
     c
     c*
     c                   Call      'SBGMSNRG'
     c                   Parm                    To               50
     c                   Parm                    Subject          50
     c                   Parm                    Body            255
     c                   Parm                    Attach          255
     c*
     C                   ENDSR

      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *  This ends this program abnormally, and sends back an escape.
      *   message explaining the failure.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P die             B
     D die             PI
     D   peMsg                      256A   const

     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)

     D dsEC            DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256

     D wwMsgLen        S             10I 0
     D wwTheKey        S              4A

     c                   eval      wwMsgLen = %len(%trimr(peMsg))
     c                   if        wwMsgLen<1
     c                   return
     c                   endif

     c                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
     c                               peMsg: wwMsgLen: '*INFO  ':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *  This show program info
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     P sndmsg          B
     D sndmsg          PI
     D   peMsg                      256A   const

     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)

     D dsEC            DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256

     D wwMsgLen        S             10I 0
     D wwTheKey        S              4A

     c                   eval      wwMsgLen = %len(%trimr(peMsg))
     c                   if        wwMsgLen<1
     c                   return
     c                   endif

     c                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
     c                               peMsg: wwMsgLen: '*INFO ' :
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E

