*SRCMBRTXT:Recuperar Tama¦o en Bytes de un archivo
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     D/COPY SDB01.SRC/QRPGSRC,IFSIO_H
     D/copy LE00525/socketsrpg,errno_h
     D die             PR
     D   peMsg                      256A   const
     C*=========================================================================
     D rc              S             10  0
     D file_handler    S             10  0
     C*=========================================================================
     D MyStat          S                   like(statds)
     C*=========================================================================
     c     *Entry        Plist
     c                   Parm                    Path            255
     c                   Parm                    Size             15 0
     C*
     c                   Eval      Path=%trim(Path)+x'00'
     c                   Eval      Size=-1
     c                   Eval      rc=Stat(%addr(Path):%addr(MyStat))
     c                   If        rc < 0
     c                   callp     die('stat(): ' + %str(strerror(errno)))
     c                   EndIf
     c                   Eval      p_statds=%addr(mystat)
     c                   Eval      Size=st_size
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   CallP     closef(file_handler)
     C*
     C                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
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
     c                               peMsg: wwMsgLen: '*ESCAPE':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E
     C*=========================================================================
      /define ERRNO_LOAD_PROCEDURE
      /copy LE00525/socketsrpg,errno_h
