*SRCMBRTXT:Borrar Archivo IFS                     
     H DFTACTGRP(*no)
     H BNDDIR('QC2LE')

     D/COPY sdb02.src/qrpgsrc,ERRNO_H
     D UnLink          PR            10I 0 EXTPROC('unlink')
     D   OutPat                    1024    CONST
     D op              s            251

     C     *ENTRY        PLIST
     C                   PARM                    OUTPAT          250
     C                   PARM                    WWERRO           50
     C                   Z-ADD     *ZERO         FLAG              1 0
     C                   EVAL      op=%trim(OUTPAT)+x'00'
     C                   EVAL      FLAG=UnLink(op)
     C                   IF        FLAG <> 0
     C                   EVAL      WWERRO=%STR(STRERROR(ERRNO()):50)
     C                   ELSE
     C                   EVAL      WWERRO=*BLANKS
     C                   ENDIF
     C                   SETON                                        LR
     C                   RETURN
     c/define ERRNO_LOAD_PROCEDURE
     c/COPY sdb02.src/qrpgsrc,ERRNO_H
