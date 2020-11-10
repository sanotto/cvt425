*SRCMBRTXT:Actualiza BASCSJ desde Miembro de Traba
     H DFTACTGRP(*NO)
     H*
     FBASCTM    UF A E           K DISK
     F@CPIUSD   UF   E           K DISK
     F@CPISYS   IF   E           K DISK
     D*----------------------------------------------------------------*
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D*----------------------------------------------------------------*
     D                 DS
     D  @ZFNKY                 1     24
     D  @FN                    1     24
     D                                     DIM(24)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     DERRDS            DS
     D ERRTXT                  1    255
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*----------------------------------------------------------------*
     D rc              S              7A
     D Count           S             15S 0
     D*----------------------------------------------------------------*
     D/COPY LE00525/TO10US,TOS1PS0RI
     D*----------------------------------------------------------------*
     c                   ExSr      FndLstPerEmp3
     c                   If        CURPER <> *ZERO
     c                   ExSr      CalcNextPer
     c                   ExSr      CheckBASCSJ
     c                   ExSr      OpenMbrList
     c                   ExSr      ReadMbrList
     c                   Eval      Count=*ZERO
     c                   DoW       SQLCOD=*ZERO
     c                   ExSr      AnzMiembro
     c                   ExSr      ReadMbrList
     C                   EndDo
     c                   ExSr      CloseMbrList
     c                   Select
     C                   When      Count = *ZERO
     c                   Eval      ErrTxt='No se pudo encontrar un '+
     c                                    'Miembro que contenga el Sco'+
     c                                    'ring de este mes consulte con'+
     c                                    ' personal a ver si ya liquido '+
     c                                    'sueldos'
     c                   ExSr      DspErr
     c                   When      Count >= 1
     c                   ExSr      SelectMember
     c                   Endsl
     c                   Else
     c                   Eval      ErrTxt='No se pudo encontrar el '+
     c                                    'Periodo anterior en BASCOR.'+
     c                                    'Se cancela el proceso...'
     c                   ExSr      DspErr
     c                   EndIf
     c                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* CheckBASCSJ: Verifica si no se cargo ya el periodo
     c*---------------------------------------------------------------------
     c     CheckBASCSJ   BegSr
     c*
     C                   Z-ADD     *ZERO         AUXCONT          15 0
     c*
     C/EXEC SQL
     C+ SELECT COUNT(*) INTO :AUXCONT  FROM BASCSJ WHERE SCFAAM = :NEXPER
     C/END-EXEC
     c*
     C                   If        AUXCONT > 0
     c                   Eval      ErrTxt='Ya se proceso el SCORING'+
     c                                    ' para el Periodo:'+
     c                                    %EDITW(NEXPER:'    /  ')
     c*                  ExSr      DspErr
     c                   ExSr      EndPgm
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* SelectMember: Seleccionar Miembro
     c*---------------------------------------------------------------------
     c     SelectMember  BegSr
     c*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Carga de SCORING BANCO            '
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Subsistema .........'+
     C                                    x'22'+'PR Prestamos                 '
     C                   EVAL      WWTIT3='Menú ...............'+
     C                                    x'22'+'PE Procesos eventuales '
     C                   EVAL      WWTIT4=''
     C                   EVAL      WWTIT4='Por favor sel la liq. corresp.'
     C                   EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+'Liq.Nro     '
     C                   MOVEL(P)  @PJOBN        @CIJOB
     C                   UPDATE    @CPIUSRR
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     c*
     c                   If        @FN(03) = *OFF AND @FN(12) =*OFF
     c                   Move      *BLANKS       CPYAUX           18
     c                   Move      *BLANKS       CPYNME           10
     c/exec sql
     c+ select S1REFC into  :CPYAUX from basctm where rrn(basctm)= :@ZRRNO
     c/end-exec
     c                   EndIf
     c                   Eval      CPYNME=%TRIM(CPYAUX)
     c                   If        CPYNME <> *BLANKS
     c                   Eval      rc=Shell('CPYF FROMFILE(BASCSJ)   '  +
     c                                      '     TOFILE(BASCSJ)     '  +
     c                                      '     FROMMBR('+CPYNME+')'  +
     c                                      '     MBROPT(*ADD)       '  )
     c                   EndIf
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* AnzMiembro: Analizar el Miembro
     c*---------------------------------------------------------------------
     c     AnzMiembro    BegSr
     c*
     c                   Z-Add     *ZERO         MBRPER            6 0
     c                   ExSr      GetMbrPer
     c                   if        MBRPER = NEXPER  AND MBRNME <> 'BASCSJ'
     c                   Eval      Count=Count+1
     c                   Eval      S1IJOB=@PJOBN
     c                   Eval      S1ISEQ=Count
     c                   Eval      S1DACL='Liq.Nro:'+MBRNME
     c                   Eval      S1REFC=MBRNME
     c                   Write     REBASCTM
     c                   else
     C                   if        MbrPer < CURPER
     c                   ExSr      RmvMbr
     c                   EndIf
     c                   EndIf
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* RmvMbr: Eliminar Miembros
     c*---------------------------------------------------------------------
     c     RmvMbr        BegSr
     c*
     c                   Eval      rc=Shell('RMVM FILE(BASCSJ) ' +
     c                                      '     MBR('+MBRNME+')' )
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* GetMbrPer: Obtener el Periodo Contenido en el Miembro
     c*---------------------------------------------------------------------
     c     GetMbrPer     BegSr
     c*
     c                   Eval      rc=Shell('OVRDBF FILE(BASCSJKK) ' +
     c                                      ' TOFILE(*LIBL/BASCSJ) ' +
     c                                      ' MBR('+MBRNME+')'      )
     c/exec sql
     c+ select max(scfaam) into :MBRPER from BASCSJKK
     c/end-exec
     c                   Eval      rc=Shell('DLTOVR *ALL         '  )
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* OpenMbrList: Obtener Listado de Miembros
     c*---------------------------------------------------------------------
     c     OpenMbrList   BegSr
     c*
     c                   Eval      rc=Shell('DLTF QTEMP/SCSJMBRS     ' )
     c                   Eval      rc=Shell('DSPFD FILE(BASCSJ)      ' +
     c                                      ' TYPE(*MBRLIST)         ' +
     c                                      ' OUTPUT(*OUTFILE)       ' +
     c                                      ' OUTFILE(QTEMP/SCSJMBRS)' )
     c*
     C/Exec SQL
     C+  Declare C1 cursor for
     C+    Select MLNAME from qtemp/scsjmbrs
     C/End-Exec
     C/Exec SQL
     C+  Open C1
     C/End-Exec
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* CloseMbrList: Cerrar Listado de Miembros
     c*---------------------------------------------------------------------
     c     CloseMbrList  BegSr
     C/Exec SQL
     C+  Close  C1
     C/End-Exec
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* ReadMbrList: Leer   Listado de Miembros
     c*---------------------------------------------------------------------
     c     ReadMbrList   BegSr
     C*
     c                   Move      *BLANKS       MBRNME           10
     C*
     C/Exec SQL
     C+  Fetch  C1 into :MBRNME
     C/End-Exec
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* CalcNextPer: Calcula el próximo periodo a Procesar
     c*---------------------------------------------------------------------
     c     CalcNextPer   BegSr
     c*
     c                   Movel     CURPER        Ano               4 0
     c                   Move      CURPER        Mes               2 0
     c                   If        Mes = 12
     c                   Z-Add     1             Mes
     c                   Add       1             Ano
     c                   Else
     c                   Add       1             Mes
     c                   EndIf
     c*
     c                   Movel     Ano           NEXPER            6 0
     c                   Move      Mes           NEXPER
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Que mas se puede decir???
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* *InzSr: Que mas se puede decir???
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     c*
     C     kbasctm       KList
     c                   KFld                    @PJOBN
     c*
     c     kbasctm       chain     REBASCTM                           99
     c                   DoW       *IN99 = *OFF
     c                   Delete    REBASCTM
     c     kbasctm       ReadE     REBASCTM                               99
     c                   EndDo
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* FndLstPerEmp3: Ubicar ultimo Periodo de Scoring para empresa 3
     c*---------------------------------------------------------------------
     c     FndLstPerEmp3 BegSr
     c*
     c                   z-Add     *zero         CURPER            6 0
     c*
     c/exec sql
     c+ select max(scfaam) into :CURPER from bascor where sciemp=3
     c/end-exec
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* DspErr: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DspErr        BegSr
     C                   Call      'BAER00RS'
     C                   Parm                    WWNCU1
     C                   Parm                    WWNCU2
     C                   Parm                    WWNCU3
     C                   Parm                    WWNCU4
     C                   Parm                    WWNCB1
     C                   Parm                    WWNCB2
     C                   Parm                    WWNCB3
     C                   Parm                    WWNCB4
     C                   EndSr
     C*=====================================================================----
     P Shell           B                   EXPORT
     C*=====================================================================----
     D  Shell          PI             7A
     D   Command                   1024A   VALUE
     D CommLen         S             15  5

     C                   Call      'QCMDEXC'
     c                   PARM                    Command
     C                   PARM      1024          CommLen
     C                   Return    'CPF0000'
     c     *PSSR         BEGSR
     C                   Return    @EXC_TYP+@EXC_NUM
     c                   ENDSR
     c
     P Shell           E
