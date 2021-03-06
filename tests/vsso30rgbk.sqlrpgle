*SRCMBRTXT:Code Test  -SQL Din sal hacia PC .DBF-V
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE')

     FVSSORG30  CF   E             WORKSTN SFILE(D01:NRR)
     FSGSYSV    IF   E             DISK
     ***************************************************************************
     ** PROTOTYPES
     ***************************************************************************
     D*-------------------------------------------------------------------------
     D*>DATASTRUCTURE : SYSERR
     D*>DESCRIPTION   : Data structure to receive error generated by OPM APIS
     D*>USE           : Retrieve OPM API Error codes
     D*>RELATEDFUNCT  : sysSndPgmMsg
     D*-------------------------------------------------------------------------
      /IF NOT DEFINED(SYSERR_DEFINED)
      /DEFINE SYSERR_DEFINED
     D SYSERR          DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256
      /ENDIF

     D Cvt2Asc         PR
     D  StrPtr                         *   value
     D  StringLen                     4S 0 value
     D*------------------------------------------------------------------------
     D*>DESCRIPTION   : Sends a program message
     D*>RETURNS       : A string containing the message key generated by the Sy
     D*>PARAMETER     : A string containig MSGI
     D*>PARAMETER     : A string containig MSGF LIB (OPT)
     D*>PARAMETER     : A string containig User data (OPT)
     D*>USAGE         : Eval      rc=sysSndPgmMsg('CPF9801':'QCPFMSG   *LIBL
     D*------------------------------------------------------------------------
     D  sysSndPgmMsg   PR             4A
     D   msgID                        7A   CONST
     D   msgF                        20A   CONST OPTIONS(*NOPASS)
     D   msgDta                     256A   CONST OPTIONS(*NOPASS)
     D   msgType                     10A   CONST OPTIONS(*NOPASS)
     D*------------------------------------------------------------------------

      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file!
      *********************************************************
     D O_WRONLY        C                   2
     D SEEK_SET        C                   0
     D O_RDWR          C                   4
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_CODEPAGE      C                   8388608

     D Sndpm           PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)
     ***************************************************************************
     ** PROGRAM START
     ***************************************************************************
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     I*----------------------------------------------------------------*
     I*----------------------------------------------------------------*
     D*------------------
     D*Field Directory Table
     D*------------------

     D*------------------
     D*Number Convertion Data Structure
     D*------------------
     D CvtDs           DS
     D CvtVal                  1      4B 0
     D  CvtBy1                 1      1A
     D  CvtBy2                 2      2A
     D  CvtBy3                 3      3A
     D  CvtBy4                 4      4A

     D SQL_NUM         C                   CONST(300)

     D QryStr          S           1024A
     D i               S             10I 0
     D j               S             10I 0
     D k               S             10I 0
     D RowSze          S             10I 0
     D FldByt          S             10I 0
     D FldLen          S             10I 0
     D FldSca          S             10I 0
     D OffSet          S             10I 0
     D RcdLen          S             10I 0
     D FilHnd          S             10I 0
     D HdrBuf          S           8192A   INZ(*ALLX'00')
     D HdrLon          S             10I 0
     D CurDig          S              2S 0
     d RecPtr          S               *
     D RowPtr          S               *
     D IndPtr          S               *
     D RunPtr          S               *
     D StrAdr          S               *
     D RunPtrDS        DS                  BASED(RunPtr)
     D  RunPtrChr              1      1A
     D  RunPtrByt              1      1S 0
     D RunPt2          S               *
     D RunPt2DS        DS                  BASED(RunPt2)
     D  RunPt2Chr              1      1A
     D  RunPt2Byt              1      1S 0
     D RunInd          S               *
     D RowCtr          S             10I 0
     D FldVec          S             32A   DIM(300)
     D FldBeg          S             10I 0 DIM(300)
     D FldTpe          S             10I 0 DIM(300)
     D FldCOH          S            255A   DIM(300)
     D FldEnd          S             10I 0 DIM(300)
     D FldLar          S             10I 0 DIM(300)
     D NegFlg          S              1A
     D CV2DIN          S              1A
     D COHELI          S           4096A
     D Linea           S           4096A   DIM(30)

     D*------------------
     D*Cabecera de Archivo Dbase III
     D*------------------
     D DBFHdr          DS
     D  HdrStr                 1     32
     D  VerNum                 1      1A   INZ(X'03')
     D  LstUpd                 2      4A   INZ(X'010101')
     D  RecNum                 5      8A   INZ(X'00000000')
     D  HdrLen                 9     10A
     D  RecLen                11     12A
     D  ResSp1                13     14A   INZ(X'0000')
     D  IncTrn                15     15A   INZ(X'00')
     D  EncFlg                16     16A   INZ(X'00')
     D  FreThd                17     20A   INZ(X'00000000')
     D  ResSp2                21     28A   INZ(X'0000000000000000')
     D  MDXFlg                29     29A   INZ(X'00')
     D  LngDrv                30     30A   INZ(X'01')
     D  ResSp3                31     32A   INZ(X'0000')

     D*------------------
     D*Field Directory Table
     D*------------------
     D DBFFld          DS
     D  FldDsc                 1     32A
     D  FldNam                 1     11A
     D  FldTyp                12     12A
     D  FldOff                13     16A
     D  FldLon                17     17A
     D  FldDec                18     18A
     D  ResSc1                19     20A   INZ(x'0000')
     D  WarIde                21     21A   INZ(x'00')
     D  ResSc2                22     23A   INZ(x'0000')
     D  FldFlg                24     24A   INZ(x'00')
     D  ResSc3                25     31A   INZ(x'00000000000000')
     D  FldIdx                32     32A   INZ(X'00')


     D CHRVAL          S              1    DIM(257) CTDATA PERRCD(1)
     D ASCVAL          S              3  0 DIM(257) ALT(CHRVAL)


     c     *entry        plist
     c                   parm                    PGMNME           10
     c                   parm                    PGMTIT           30
     c                   parm                    HEDLI1           78
     c                   parm                    HEDLI2           78
     c                   parm                    HEDLI3           78
     c                   parm                    FUNKY1           64
     c                   parm                    FUNKY2           64
     c                   parm                    parstr         1024
     C* setup number of sqlda entries and length of the sqlda
     C                   EVAL       SQLD = 300
     C                   EVAL       SQLN = 300
     C                   EVAL       SQLDABC =SQLN*%LEN(SQLVAR)+16
     C*
     c     1             Chain     RESGSYSV                           25
     C                   EVAL      QryStr=parstr
     C/Exec Sql
     C+ INCLUDE SQLDA
     C/End-Exec
     C/Exec Sql
     C+ PREPARE S1 FROM :QryStr
     C/End-Exec
     C/Exec Sql
     C+ DECLARE C1 SCROLL CURSOR FOR S1
     C/End-Exec
     C                   EXSR      CHKCDE
     C/Exec Sql
     C+ OPEN C1
     C/End-Exec
     C                   EXSR      CHKCDE
     C/Exec Sql
     C+ ALLOCATE DESCRIPTOR 'SQLDA' WITH MAX 300
     C/End-Exec

     C/Exec Sql
     C+ DESCRIBE S1 INTO :SQLDA
     C/End-Exec
     C                   IF        SQLN < SQLD
     C                   Z-ADD     SQLD          MAXROWS          15 0
     C/Exec Sql
     C+ ALLOCATE DESCRIPTOR 'SQLDA' WITH MAX :MAXROWS
     C/End-Exec
     C/Exec Sql
     C+ DESCRIBE S1 INTO :SQLDA
     C/End-Exec
     C                   ENDIF
     C                   EXSR      CHKCDE
     C*
     C                   EXSR      BLDBUF
     C                   EXSR      BLDVEC
     C                   EXSR      FILHDR
     C                   EXSR      BLDREC
     C*
     c                   ExSr      BuildColHead
     C*
     c                   Z-ADD     *ZERO         Paginas          15 0
     c                   Z-ADD     10            MaxReng           5 0
     c                   ExSr      NextPage
     c                   ExSr      ShowSubfile
     C                   DoW       *IN03 = *OFF And *IN12 = *Off
     c                   If        *In46=*On
     c                   ExSr      NextPage
     c                   ExSr      ShowSubfile
     C                   EndIf
     c                   If        *In45=*On
     c                   ExSr      PrevPage
     c                   ExSr      ShowSubfile
     C                   EndIf
     C                   EndDo
     C/Exec Sql
     C+ CLOSE C1
     C/End-Exec
     C                   EVAL      CvtVal=RowCtr
     C                   DEALLOC                 RowPtr
     C                   DEALLOC                 RecPtr

     C                   Seton                                        LR

     c*-------------------------------------------------------------------------
     c     ShowSubfile   BegSr
     C*
     c                   Movel     COHELI        COLHDG
     C                   SETOFF                                       303126
     C                   Z-ADD     0             NRR               2 0
     C                   WRITE     C01
     C                   For       NRR = 1 To MaxReng
     C                   Eval      SFLLIN=Linea(NRR)
     C                   WRITE     D01                                    26
     C                   EndFor
     C     NRR           COMP      *ZERO                              30
     C                   SETON                                        31
     C                   WRITE     P01
     C                   EXFMT     C01
     c
     C*
     C                   EndSr
     c*-------------------------------------------------------------------------
     C*
     c*-------------------------------------------------------------------------
     c     NextPage      BegSr
     c*-------------------------------------------------------------------------
     C*
     c                   Z-ADD     1             Renglones         5 0
     c                   Z-ADD     *ZERO         RowCtr
     c                   Clear                   Linea
     C/Exec Sql
     C+ FETCH NEXT FROM C1 USING DESCRIPTOR :SQLDA
     C/End-Exec
     C                   DOW       SQLCOD = *ZERO and Renglones <= MaxReng
     c                   Add       1             Renglones         5 0
     C                   EXSR      PRCROW
     C/Exec Sql
     C+ FETCH NEXT FROM C1 USING DESCRIPTOR :SQLDA
     C/End-Exec
     C                   ENDDO
     c*
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     PrevPage      BegSr
     c*-------------------------------------------------------------------------
     C*
     c                   Clear                   Linea
     c                   For       I = 1 To ((MaxReng * 2) - 1)
     C/Exec Sql
     C+ FETCH PRIOR FROM C1 USING DESCRIPTOR :SQLDA
     C/End-Exec
     C                   EndFor
     c*
     c                   ExSr      NextPage
     c*
     c                   EndSr
     C*------------------
     C* Rellenar Cabecera
     C*------------------
     C     FILHDR        BEGSR
     C                   EVAL      CvtVal=OffSet
     C                   EVAL      RecLen=CvtBy4+CvtBy3
     C                   EVAL      CvtVal=32+(sqld*32)+1
     C                   EVAL      HdrLon=CvtVal
     C                   EVAL      HdrLen=CvtBy4+CvtBy3
     C                   ENDSR

     C*------------------
     C* CREAR ARRAY DE CAMPOS
     C*------------------
     C     BLDVEC        BEGSR
     C                   EVAL      RcdLen=1
     C                   EVAL      OffSet=1
     C                   FOR       i=1 TO SQLD
     C                   MOVE      SQL_VAR(i)    SQLVAR
     C                   EXSR      DETLEN
     C                   EVAL      RcdLen=RcdLen+FldLen
     C                   EVAL      CvtVal=FldLen
     C                   EVAL      FldLon=CvtBy4+CvtBy3
     C                   EVAL      CvtVal=OffSet
     C                   EVAL      FldOff=CvtBy4+CvtBy3+CvtBy2+CvtBy1
     C                   EVAL      FldBeg(i)=OffSet
     C                   EVAL      FldCOH(i)=SQLNAME
     C                   EVAL      OffSet=OffSet+FldLen+2
     C                   EVAL      FldLar(i)=FldLen
     C                   EVAL      FldEnd(i)=OffSet
     C                   EVAL      FldTpe(i)=SQLTYPE
     C                   EVAL      CvtVal=FldSca
     C                   EVAL      FldDec=CvtBy4
     C                   EVAL      FldVec(i)=FldDsc
     C                   ENDFOR
     C
     C                   ENDSR

     C*------------------
     C* CHECK SQL CODES
     C*------------------
     C     CHKCDE        BEGSR
     C                   IF        SQLCOD <> *ZERO
     C                   DEALLOC                 RowPtr
     C                   DEALLOC                 RecPtr
     C                   MOVE      SQLCOD        CDEAU1            4 0
     C                   MULT      -1            CDEAU1
     C                   MOVE      CDEAU1        CDEAU2
     C                   MOVEL(P)  'SQL'         CDEAU2            7
     C*                  callp     sysSndPgmMsg('CPF9897':'QCPFMSG   *LIBL':
     C*                                         'Error SQL:'+%CHAR(SQLCOD):
     C*                                         '*ESCAPE'   )
     C*                  callp     sysSndPgmMsg(CDEAU2   :'QSQLMSG   *LIBL':
     C*                                         *OMIT                     :
     C*                                         '*ESCAPE'   )
     C                   Seton                                        LR
     C                   Return
     C                   ENDIF
     C                   ENDSR

     C*------------------
     C* PROCES ROW
     C*------------------
     C     PRCROW        BEGSR
     C                   FOR       i=1 TO SQLD
     C                   MOVE      SQL_VAR(i)    SQLVAR
     C                   EXSR      DETLEN
     C                   MOVE      FldVec(i)     FldDsc
     C                   SELECT
     C                   WHEN      FldTyp=X'44'
     C                   EXSR      DTEFLD
     C                   WHEN      FldTyp=X'43'
     C                   EXSR      TXTFLD
     C                   WHEN      FldTyp=X'4E' AND (SQLTYPE=484 OR SQLTYPE=485)
     C                   EXSR      PAKFLD
     C                   WHEN      FldTyp=X'4E' AND (SQLTYPE=488 OR SQLTYPE=489)
     C                   EXSR      ZONFLD
     C                   ENDSL
     C                   ENDFOR
     C                   CALLP     Cvt2Asc(RecPtr:RcdLen)
     c*                  CALLP     write(FilHnd: RecPtr: RcdLen)
     C                   EVAL      RowCtr=RowCtr+1
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)+3
     c                   For       I=1 TO RcdLen
     C                   EVAL      %SUBST(Linea(RowCtr):i:1)=RunPt2Chr
     c                   Eval      RunPt2=RunPt2+1
     C                   EndFor
     C                   ENDSR
     C*------------------
     C* Build Column Headers
     C*------------------
     C     BuildColHead  BEGSR
     C                   FOR       i=1 TO SQLD
     c                   Move      FldBeg(i)     inicio            8 0
     c                   Z-Add     *zero         Largo             5 0
     c                   Eval      Largo = FldEnd(i)-FldBeg(i)
     c                   If        FldTpe(i) = 452 or FldTpe(i) = 453
     c                   if        Inicio > 2
     c                   Sub       2             Inicio
     c                   EndIf
     c                   EndIf
     c                   Eval      %Subst(COHELI:Inicio:Largo )=FldCOH(i)
     C                   ENDFOR
     C                   ENDSR

     C*------------------
     C* DATE FIELD DATA COPY
     C*------------------
     C     DTEFLD        BEGSR
     C                   EVAL      RunPtr=SQLDATA
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)-1
     C                   MOVE      *BLANKS       CHAR              1
     C*                  FOR       J=1 TO fldlen
     c* (A)AAAMMDD
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR=%CHAR(%DIV(CV2DOU:16))
     C                   EVAL      RunPt2Chr=CHAR
     C                   EVAL      RunPt2=RunPt2+1
     c* A(A)AAMMDD
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR=%CHAR(%REM(CV2DOU:16))
     C                   EVAL      RunPt2Chr=CHAR
     C                   EVAL      RunPt2=RunPt2+1
     C* ... Avanzo en SQLDATA
     C                   EVAL      RunPtr=RunPtr+1
     C*
     c* AA(A)AMMDD
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%DIV(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     c* AAA(A)MMDD
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%REM(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     C* ... Avanzo en SQLDATA
     C                   EVAL      RunPtr=RunPtr+1
     c* AAAA(M)MDD
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%DIV(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     c* AAAAM(M)DD
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%REM(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     C* ... Avanzo en SQLDATA
     C                   EVAL      RunPtr=RunPtr+1
     c* AAAAMM(D)D
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%DIV(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     c* AAAAMMD(D)
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%REM(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     C* ... Avanzo en SQLDATA
     C                   EVAL      RunPtr=RunPtr+1
     c* AAAAMMD(D)
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CHAR     =%CHAR(%DIV(CV2DOU:16))
     C                   EVAL      RunPt2Chr= CHAR
     C                   EVAL      RunPt2=RunPt2+1
     C* ... Avanzo en SQLDATA
     C                   EVAL      RunPtr=RunPtr+1
     c*
     C                   ENDSR

     C*------------------
     C* PACKED FIELD DATA COPY
     C*------------------
     C     PAKFLD        BEGSR
     C                   EXSR      CLRFLD
     C                   EVAL      RunPtr=SQLDATA+FldByt
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)+FldLen
     C                   EVAL      NegFlg=*OFF
     C                   EVAL      k=0
     C                   FOR       j=1 TO FldByt
     C                   EVAL      RunPtr=RunPtr-1
     C                   EVAL      CV2DIN=RunPtrChr
     C                   EXSR      CV2DEC
     C                   EVAL      CurDig= %REM(CV2DOU:16)
     C                   EXSR      PRCDIG
     C                   EVAL      CurDig= %DIV(CV2DOU:16)
     C                   EXSR      PRCDIG
     C                   ENDFOR
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)
     C                   FOR       j=1 TO FldLen
     C                   IF        RunPt2Chr ='0' OR RunPt2Chr =*BLANK
     C                   EVAL      RunPt2Chr=*BLANK
     C                   EVAL      RunPt2=RunPt2+1
     C                   ELSE
     C                   IF        NegFlg = *ON
     C                   EVAL      RunPt2=RunPt2-1
     C                   EVAL      RunPt2Chr='-'
     C                   EVAL      NegFlg = *OFF
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDFOR
     C                   ENDSR

     C     PRCDIG        BEGSR
     C                   IF        CurDig = 13  OR CurDig=15
     C                   IF        CurDig = 13
     C                   EVAL      NegFlg=*ON
     C                   ENDIF
     C                   ELSE
     C                   EVAL      RunPt2=RunPt2-1
     C                   EVAL      k=k+1
     C                   EVAL      RunPt2Chr=%CHAR(CurDig)
     C                   IF        K=FldSca
     C                   EVAL      RunPt2=RunPt2-1
     C                   EVAL      RunPt2Chr='.'
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*---------------------
     C*
     C*---------------------
     C     CV2DEC        BEGSR
     C                   Z-ADD     *ZERO         CV2DOU            3 0
     C                   TESTB     '0'           CV2DIN                   30
     C                   TESTB     '1'           CV2DIN                   31
     C                   TESTB     '2'           CV2DIN                   32
     C                   TESTB     '3'           CV2DIN                   33
     C                   TESTB     '4'           CV2DIN                   34
     C                   TESTB     '5'           CV2DIN                   35
     C                   TESTB     '6'           CV2DIN                   36
     C                   TESTB     '7'           CV2DIN                   37
     C   30              ADD       128           CV2DOU
     C   31              ADD       64            CV2DOU
     C   32              ADD       32            CV2DOU
     C   33              ADD       16            CV2DOU
     C   34              ADD       8             CV2DOU
     C   35              ADD       4             CV2DOU
     C   36              ADD       2             CV2DOU
     C   37              ADD       1             CV2DOU
     C                   ENDSR

     C*------------------
     C* Clear Field
     C*------------------
     C     CLRFLD        BEGSR
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)
     C                   FOR       j=1 TO FldLen
     C                   EVAL      RunPt2Chr=' '
     C                   EVAL      RunPt2=RunPt2+1
     C                   ENDFOR
     C                   ENDSR

     C*------------------
     C* ZONED FIELD DATA COPY
     C*------------------
     C     ZONFLD        BEGSR
     C                   EXSR      CLRFLD
     C                   EVAL      RunPtr=SQLDATA+FldByt
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)+FldLen
     C                   EVAL      NegFlg=*OFF
     C                   FOR       j=1 TO FldByt
     C                   EVAL      RunPtr=RunPtr-1
     C                   EVAL      RunPt2=RunPt2-1
     C                   IF        RunPtrChr >='J' and RunPtrChr <='R'
     C                   EVAL      NegFlg=*ON
     C                   EVAL      RunPtrChr=%CHAR(%ABS(RunPtrByt))
     C                   ENDIF
     C                   EVAL      RunPt2Chr=RunPtrChr
     C                   IF        j=FldSca
     C                   EVAL      RunPt2=RunPt2-1
     C                   EVAL      RunPt2Chr='.'
     C                   ENDIF
     C                   ENDFOR
     C                   EVAL      RunPt2=RecPtr+1+FldBeg(i)
     C                   FOR       j=1 TO FldLen
     C                   IF        RunPt2Chr ='0' OR RunPt2Chr =*BLANK
     C                   EVAL      RunPt2Chr=*BLANK
     C                   EVAL      RunPt2=RunPt2+1
     C                   ELSE
     C                   IF        NegFlg = *ON
     C                   EVAL      RunPt2=RunPt2-1
     C                   EVAL      RunPt2Chr='-'
     C                   EVAL      NegFlg = *OFF
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDFOR
     C                   ENDSR

     C*------------------
     C* TEXT FIELD DATA COPY
     C*------------------
     C     TXTFLD        BEGSR
     C                   EVAL      RunPtr=SQLDATA
     C                   EVAL      RunPt2=RecPtr+FldBeg(i)
     C                   FOR       J=1 TO SQLLEN
     C                   EVAL      RunPt2Chr=RunPtrChr
     C                   EVAL      RunPtr=RunPtr+1
     C                   EVAL      RunPt2=RunPt2+1
     C                   ENDFOR
     C                   ENDSR

     C*------------------
     C* BUILD BUFFER
     C*------------------
     C     BLDBUF        BEGSR
     C                   EXSR      CALRBS
     C                   MONITOR
     C                   EVAL      RowPtr=%ALLOC(RowSze)
     C                   ON-ERROR  425:426
     C                   ENDMON
     C                   MONITOR
     C                   EVAL      IndPtr=%ALLOC(SQLD * 2)
     C                   ON-ERROR  425:426
     C                   ENDMON
     C                   EXSR      ASGPTR
     C                   ENDSR

     C*------------------
     C* BUILD .DBF RECORD
     C*------------------
     C     BLDREC        BEGSR
     C                   MONITOR
     C                   EVAL      RecPtr=%ALLOC(RcdLen)
     C                   ON-ERROR  425:426
     C                   ENDMON
     C                   ENDSR

     C*------------------
     C* CALC ROW BUFFER SIZE
     C*------------------
     C     CALRBS        BEGSR
     C                   EVAL      RowSze=0
     C                   FOR       i=1 TO SQLD
     C                   MOVE      SQL_VAR(I)    SQLVAR
     C                   EXSR      DETLEN
     C                   EVAL      RowSze=RowSze+FldByt
     C                   ENDFOR
     C                   ENDSR

     C*------------------
     C* ASIGN POINTERS
     C*------------------
     C     ASGPTR        BEGSR
     C                   EVAL      RunPtr=RowPtr
     C                   EVAL      RunInd=IndPtr
     C                   FOR       i=1 TO SQLD
     C                   MOVE      SQL_VAR(i)    SQLVAR
     C                   EVAL      SQLDATA=RunPtr
     C                   EVAL      SQLIND=RunInd
     C                   MOVE      SQLVAR        SQL_VAR(i)
     C                   EXSR      DETLEN
     C                   EVAL      RunPtr=RunPtr+FldByt
     C                   EVAL      RunInd=RunInd+2
     C                   ENDFOR
     C                   ENDSR

     C*------------------
     C* DETERMINE FIELD LENGHT
     C*------------------
     C     DETLEN        BEGSR
     C                   EVAL      FldNam=%SUBST(SQLNAME:1:10)
     C                   IF        %SUBST(FldNam:1:1) >= '0' and
     C                             %SUBST(FldNam:1:1) <='9'
     C                   EVAL      %SUBST(FldNam:1:1) ='E'
     C                   ENDIF
     C                   CallP     Cvt2Asc(%ADDR(FldNam):%LEN(FldNam))
     C                   EVAL      FldNam=%XLATE(X'20':x'00':FldNam)
     C                   EVAL      FldSca=0
     C                   SELECT
     C                   WHEN      SQLTYPE=384 OR SQLTYPE=385
     C                   EVAL      FldByt=      10
     C                   EVAL      FldLen=       8
     C                   EVAL      FldLen=      10
     C                   EVAL      FldTyp=x'44'
     C                   WHEN      SQLTYPE=388 OR SQLTYPE=389
     C                   EVAL      FldByt=       8
     C                   EVAL      FldLen=       8
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=392 OR SQLTYPE=393
     C                   EVAL      FldByt=      26
     C                   EVAL      FldLen=      26
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=396 OR SQLTYPE=397
     C                   EVAL      FldByt=       SQLLEN
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'43'
     C*                  //CHAR
     C                   WHEN      SQLTYPE=452 OR SQLTYPE=453
     C                   EVAL      FldByt=       SQLLEN
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=455 OR SQLTYPE=456
     C                   EVAL      FldByt=       SQLLEN
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=464 OR SQLTYPE=465
     C                   EVAL      FldByt=       SQLLEN
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=468 OR SQLTYPE=469
     C                   EVAL      FldByt=       SQLLEN
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=472 OR SQLTYPE=473
     C                   EVAL      FldByt=       SQLLEN
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'43'
     C                   WHEN      SQLTYPE=480
     C                   EVAL      FldByt=       4
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'4E'
     C                   WHEN      SQLTYPE=481
     C                   EVAL      FldByt=       8
     C                   EVAL      FldLen=       SQLLEN
     C                   EVAL      FldTyp=x'4e'
     C*                  //PACKED
     C                   WHEN      SQLTYPE=484 OR SQLTYPE=485
     C                   EVAL      FldByt= %INTH(((SQLLEN/256)+ 1)/2)
     C                   EVAL      FldLen= %INT(SQLLEN/256) + 2
     C                   EVAL      FldSca= %REM(SQLLEN:256)
     C                   EVAL      FldTyp=x'4e'
     C                   IF        %SUBST(SQLNAME:1:5)='@INT@'
     C                   EVAL      FldNam=%SUBST(SQLNAME:6:10)
     C                   CallP     Cvt2Asc(%ADDR(FldNam):%LEN(FldNam))
     C                   EVAL      FldNam=%XLATE(X'20':x'00':FldNam)
     C                   EVAL      FldLen= %INT(SQLLEN/256)
     C                   EVAL      FldSca= 0
     C                   ENDIF
     C*                  //ZONED
     C                   WHEN      SQLTYPE=488 OR SQLTYPE=489
     C                   EVAL      FldByt= %INTH(((SQLLEN/256)+ 1)  )-1
     C                   EVAL      FldLen= %INT(SQLLEN/256) + 2
     C                   EVAL      FldSca= %REM(SQLLEN:256)
     C                   EVAL      FldTyp=x'4e'
     C                   IF        %SUBST(SQLNAME:1:5)='@INT@'
     C                   EVAL      FldNam=%SUBST(SQLNAME:6:10)
     C                   CallP     Cvt2Asc(%ADDR(FldNam):%LEN(FldNam))
     C                   EVAL      FldNam=%XLATE(X'20':x'00':FldNam)
     C                   EVAL      FldLen= %INT(SQLLEN/256)
     C                   EVAL      FldSca= 0
     C                   ENDIF
     C                   WHEN      SQLTYPE=496 OR SQLTYPE=497
     C                   EVAL      FldByt=       4
     C                   EVAL      FldTyp=x'4e'
     C                   WHEN      SQLTYPE=500 OR SQLTYPE=501
     C                   EVAL      FldByt=       2
     C                   EVAL      FldTyp=x'4e'
     C                   ENDSL
     C                   IF        %SUBST(SQLNAME:1:4)='@F8@'
     C                             AND FLDSCA=0 AND FLDLEN=10
     C                   EVAL      FldNam=%SUBST(SQLNAME:5:10)
     C                   CallP     Cvt2Asc(%ADDR(FldNam):%LEN(FldNam))
     C                   EVAL      FldNam=%XLATE(X'20':x'00':FldNam)
     C                   EVAL      FldByt=       8
     C                   EVAL      FldLen=       8
     C                   EVAL      FldTyp=x'44'
     C                   ENDIF
     C                   IF        %SUBST(SQLNAME:1:4)='@F6@'
     C                             AND FLDSCA=0 AND FLDLEN=8
     C                   EVAL      FldNam=%SUBST(SQLNAME:5:10)
     C                   CallP     Cvt2Asc(%ADDR(FldNam):%LEN(FldNam))
     C                   EVAL      FldNam=%XLATE(X'20':x'00':FldNam)
     C                   EVAL      FldByt=       6
     C                   EVAL      FldLen=       8
     C                   EVAL      FldTyp=x'44'
     C                   ENDIF
     C                   ENDSR
     **-------------------------------------------------------------------------
     **
     **-------------------------------------------------------------------------
     P Cvt2Asc         B                   EXPORT
     D Cvt2Asc         PI
     D  StrPtr                         *   value
     D  StringLen                     4S 0 value

     D  I              S             10I 0
     D  WrkPtr         S               *
     D  Char           S              1A   BASED(Wrkptr)
     D  RplChr         S              1A
     D  Asc            S              3S 0
     D

     C                   EVAL      WrkPtr=StrPtr
     C                   FOR       I=1 TO StringLen
     C                   EVAL      Asc=%LOOKUP(Char:CHRVAL)
     C                   IF        Asc <> *ZERO
     C                   EVAL      Asc=ASCVAL(Asc)
     C                   EVAL      CvtVal=Asc
     C                   EVAL      RplChr=CvtBy4
     C                   ELSE
     C                   EVAL      RplChr=x'20'
     C                   ENDIF
     C*                  EVAL      Char=RplChr
     C                   Eval      WrkPtr=WrkPtr+1
     C                   ENDFOR

     P Cvt2Asc         E

     P sysSndPgmMsg    B                   EXPORT
     D  sysSndPgmMsg   PI             4A
     D   msgID                        7A   CONST
     D   msgF                        20A   CONST OPTIONS(*NOPASS)
     D   msgDta                     256A   CONST OPTIONS(*NOPASS)
     D   msgTyp                      10A   CONST OPTIONS(*NOPASS)

     D MSGKEY          S              4A

     D Sndpm           PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)


     D wwMsgLen        S             10I 0
     D wwMsgF          S             20A
     D wwMsgD          S            256A
     D wwTheKey        S              4A
     D wwMsgTyp        S             10A   INZ('*ESCAPE')

     c                   eval      wwMsgF='QCPFMSG   *LIBL     '
     c                   eval      wwMsgD=*BLANKS
     C                   if        %parms  >= 2
     c                   eval      wwMsgF=msgF
     c                   endif
     C                   if        %parms  >= 3
     c                   eval      wwMsgD=msgDta
     c                   endif
     C                   if        %parms  >= 4
     c                   eval      wwMsgTyp=msgTyp
     c                   endif
     c                   eval      wwMsgLen = %len(%trimr(wwMsgD))
     c                   callp     SndPm(msgID :wwMsgF     :
     c                                   wwMsgD: wwMsgLen: wwMsgTyp   :
     c                                   '*CTLBDY': 1: wwTheKey: SYSERR)

     C                   RETURN    wwTheKey

     P sysSndPgmMsg    E
     **-------------------------------------------------------------------------
**
 032
]033
"034
Ñ035
$036
%037
&038
'039
(040
)041
*042
+043
,044
-045
.046
/047
0048
1049
2050
3051
4052
5053
6054
7055
8056
9057
:058
;059
<060
=061
>062
?063
@064
A065
B066
C067
D068
E069
F070
G071
H072
I073
J074
K075
L076
M077
N078
O079
P080
Q081
R082
S083
T084
U085
V086
W087
X088
Y089
Z090
^091
\092
!093
¢094
_095
`096
a097
b098
c099
d100
e101
f102
g103
h104
i105
j106
k107
l108
m109
n110
o111
p112
q113
r114
s115
t116
u117
v118
w119
x120
y121
z122
{123
|124
}125
¨126
¡161
[162
£163
¤164
¥165
ñ166
§167
~168
©169
ª170
«171
¬172
­173
®174
¯175
°176
±177
²178
³179
´180
µ181
¶182
·183
¸184
¹185
º186
»187
¼188
½189
¾190
¿191
À192
Á193
Â194
Ã195
Ä196
Å197
Æ198
Ç199
È200
É201
Ê202
Ë203
Ì204
Í205
Î206
Ï207
Ð208
#209
Ò210
Ó211
Ô212
Õ213
Ö214
×215
Ø216
Ù217
Ú218
Û219
Ü220
Ý221
Þ222
ß223
à224
á225
â226
ã227
ä228
å229
æ230
ç231
è232
é233
ê234
ë235
ì236
í237
î238
ï239
ð240
¦241
ò242
ó243
ô244
õ245
ö246
÷247
ø248
ù249
ú250
û251
ü252
ý253
þ254
ÿ255
