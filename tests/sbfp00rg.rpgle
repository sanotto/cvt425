*SRCMBRTXT:Formularios: Parser de Etiquetas       
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('QC2LE')

     FBAFCPI    IF   E           K DISK

     D*---------------------------
     D* Conversion Mayusculas Minusculas
     D*---------------------------
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D*---------------------------
     D* Buffers Para Archivos
     D*---------------------------
     D bufInp          S          32575A
     D bufOut          S          32575A
     D*---------------------------
     D* Handlers Para Archivos
     D*---------------------------
     D hndInp          S             10I 0
     D hndOut          S             10I 0
     D*---------------------------
     D* Punteros Para Buffers
     D*---------------------------
     D ptrInp          S               *   INZ(%ADDR(bufInp))
     D ptrOut          S               *   INZ(%ADDR(bufOut))
     D*---------------------------
     D* Punteros de Trabajo
     D*---------------------------
     D wrkPtr          S               *
     D*---------------------------
     D* Variables para datos apuntados por los punteros
     D*---------------------------
     D chrInp          S              1A   BASED(ptrInp)
     D chrOut          S              1A   BASED(ptrOut)
     D*---------------------------
     D* Estructura para recuperar valores binarios
     D*---------------------------
     D CVTDS           DS
     D  BINVAL                 1      4B 0 INZ(*ZERO)
     D  BYTE01                 1      1
     D  BYTE02                 2      2
     D  BYTE03                 3      3
     D  BYTE04                 4      4
     D*---------------------------
     D* Variables
     D*---------------------------
     D*Para guardar returns codes
     D rc              S             10I 0
     D*Para guardar returns codes
     D mk              S              4A
     D*Para guardar Bytes Leidos en el Buffer
     D byteCnt         S             10I 0
     D*Contadores
     D I               S             10I 0
     D J               S             10I 0
     D*Para guardar Estado maquina Finita
     D CurSts          S             10I 0
     D*Para llevar cuenta del Buffer lleno
     D outCnt          S             10I 0
     D*Token String
     D tknStr          S             10A   INZ(*BLANKS)
     D*Largo de Token
     D tknLen          S              5  0

     D  A2E            PR             1A
     D   chrASCII                     1A   CONST
     D  E2A            PR             1A
     D   chrEBCDIC                    1A   CONST

      /COPY SDB01.SRC/QRPGSRC,SBFP00HH

     C*-------------------------------------------------------------------------
     C                   EXSR      OPNFIL
     C                   EXSR      FILINB
     C                   DOW       byteCnt > *ZERO
     C                   EXSR      PRCBUF
     C                   EXSR      FILINB
     C                   ENDDO
     C                   EXSR      FLUSHO
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* OPNFIL: Abrir Archivos
     C*-------------------------------------------------------------------------
     C     OPNFIL        BEGSR
     C*
     C* ... Intentar abrir archivo de entrada
     C                   EVAL      QPNINP=%trim(QPNINP)+X'00'
     C                   EVAL      hndInp = open(%ADDR(QPNINP) :
     C                                           O_RDONLY)
     C                   IF        hndInp < *ZERO
     C                   EVAL      mk=sysSndPgmMsg('CPF9897'
     C                             :'QCPFMSG   *LIBL      '
     C                             :'No se pudo abrir archivo de entrada'
     C                             :'*NOTIFY'                            )
     C                   EXSR      ENDPGM
     C                   ENDIF
     C* ... Intentar abrir archivo de Salida
     C                   EVAL      QPNOUT=%trim(QPNOUT)+X'00'
     C                   EVAL      hndOut = open(%ADDR(QPNOUT) :
     C                              O_WRONLY + O_TRUNC + O_CREAT :
     C                              S_IRUSR + S_IWUSR + S_IRGRP + S_IROTH )
     C                   IF        hndOut < *ZERO
     C                   EVAL      mk=sysSndPgmMsg('CPF9897'
     C                             : 'QCPFMSG   *LIBL      '
     C                             : 'No se pudo abrir archivo de Salida'
     C                             : '*NOTIFY'                           )
     C
     C                   EXSR      ENDPGM
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FILINB: Rellenar Buffer de Entrada
     C*-------------------------------------------------------------------------
     C     FILINB        BEGSR
     C*
     C* ... Rellenar Buffer de Entrada
     C                   EVAL      byteCnt=read(hndInp:%ADDR(bufInp)
     C                                                : %len(bufInp) )
     C* ... Resetear Punteros
     C                   EVAL      ptrInp=%ADDR(bufInp)
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* PRCBUF: Rellenar Archivo de Salida
     C*-------------------------------------------------------------------------
     C     PRCBUF        BEGSR
     C                   FOR       I= 1 TO byteCnt
     C                   SELECT
     C                   WHEN      CurSts=0 AND chrInp   = X'3C'
     C                   EVAL      CurSts=1
     C                   WHEN      CurSts=0 AND chrInp  <> X'3C'
     C                   EXSR      WRTOUT
     C                   WHEN      CurSts=1 AND chrInp   = X'3F'
     C                   EVAL      CurSts=2
     C                   EVAL      tknStr=*BLANKS
     C                   WHEN      CurSts=1 AND chrInp  <> X'3F'
     C                   EXSR      WRTOUT
     C                   EVAL      CurSts=0
     C                   WHEN      CurSts=2 AND chrInp   = X'3E'
     C                   EXSR      RPLDTA
     C                   EVAL      CurSts=0
     C                   WHEN      CurSts=2 AND chrInp  <> X'3E'
     C                   EVAL      tknStr=%trim(tknStr)+chrInp
     C                   ENDSL
     C*
     C                   EVAL      PtrInp=PtrInp + 1
     C                   ENDFOR
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTOUT: Escribe archivo de salida
     C*-------------------------------------------------------------------------
     C     WRTOUT        BEGSR
     C                   MOVE      chrInp        ChrOut
     C                   EVAL      PtrOut=PtrOut + 1
     C                   EVAL      outCnt=outCnt+1
     C                   IF        outCnt=%len(bufOut)
     C                   EVAL      rc=write(hndOut: %addr(bufOut) :
     C                               %size(bufOut))
     C                   EVAL      outCnt=0
     C                   EVAL      PtrOut=%addr(bufOut)
     C                   CLEAR                   bufOut
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FLUSHO: Flush del Buffer de Salida si quedo algo
     C*-------------------------------------------------------------------------
     C     FLUSHO        BEGSR
     C                   IF        outCnt>0
     C                   EVAL      rc=write(hndOut: %addr(bufOut) :
     C                               outCnt)
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ENDPGM: Finalizar Programa, liberar Recursos
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C* ... Cerrar Archivos
     C                   EVAL      rc=close(hndInp)
     C                   EVAL      rc=close(hndOut)
     C* ... Salir del programa
     C                   RETURN
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* RPLDTA: Reemplazar Datos
     C*-------------------------------------------------------------------------
     C     RPLDTA        BEGSR
     C*
     C* ... Convertir token a EBCDIC
     C                   EVAL      tknLen=%len(%trim(tknStr))
     C                   MOVEL     'Q850337284'  XLTTAB           10
     C                   CALL      'QDCXLATE'
     C                   PARM                    TKNLEN            5 0
     C                   PARM                    TKNSTR
     C                   PARM                    XLTTAB           10
     C* ... Convertir token a MAYUSCULAS, la API devuelve minusculas...
     C                   EVAL      tknStr=%xlate(lo:up:tknStr)
     C* ... Buscar Token en CPI
     C     WKEY01        CHAIN     REBAFCPI                           25
     C                   IF        *IN25 = *OFF
     C* ... Validar Códigos de Edición
     C                   SELECT
     C                   WHEN      FMEFOR='Y'
     C                   EVAL      FMDAV1= %SUBST(FMDAV1:7:2)+'/'+
     C                                     %SUBST(FMDAV1:5:2)+'/'+
     C                                     %SUBST(FMDAV1:1:4)
     C                   WHEN      FMEFOR='J'
     C                   MOVEL     FMDAV1        AUXIMP           15 2
     C                   EVAL      FMDAV1=%trim(%EDITC(AUXIMP:'J'))
     C                   WHEN      FMEFOR='*'
     C                   MOVEL     FMDAV1        AUXIMP           15 2
     C                   EVAL      FMDAV1=%trim(%EDITC(AUXIMP:'J':*ASTFILL))
     C                   WHEN      FMEFOR='1'
     C                   MOVEL     FMDAV1        AUXIMP           15 2
     C                   EVAL      FMDAV1=%trim(%EDITC(AUXIMP:'1'))
     C                   WHEN      FMEFOR='0'
     C                   MOVEL     FMDAV1        AUXIM2           15 0
     C                   EVAL      FMDAV1=%trim(%EDITC(AUXIM2:'Z'))
     C                   ENDSL
     C* ... Convertir reemplazo a ASCII
     C                   EVAL      tknLen=%len(%trim(FMDAV1))
     C     tknLen        IFGT      *ZERO
     C                   MOVEL     'Q284BF850 '  XLTTAB           10
     C                   CALL      'QDCXLATE'
     C                   PARM                    TKNLEN            5 0
     C                   PARM                    FMDAV1
     C                   PARM                    XLTTAB           10
     C* ... Copiar reemplazo a Buffer
     C                   EVAL      wrkPtr=%ADDR(FMDAV1)
     C                   FOR       J= 1 TO tknLen
     C                   EVAL      chrInp=%subst(FMDAV1:j:1)
     C                   EXSR      WRTOUT
     C                   ENDFOR
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* *INZSR: Subrutina de Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QPNINP          255
     C                   PARM                    QPNOUT          255
     C*
     C     WKEY01        KLIST
     C                   KFLD                    @JOB_NUM
     C                   KFLD                    TKNSTR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* Enviar Mensajes de Programa (API)
     C*-------------------------------------------------------------------------
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
     C*-------------------------------------------------------------------------
     C* Convertir de ASCII a EBCDIC
     C*-------------------------------------------------------------------------
     P A2E             B
     D  A2E            PI             1A
     D   chrASCII                     1A   CONST

     D chrEBCDIC       S              1

     C                   EVAL      chrEBCDIC=chrASCII
     C                   RETURN    chrEBCDIC
     P A2E             E
     C*-------------------------------------------------------------------------
     C* Convertir de EBCDIC a ASCII
     C*-------------------------------------------------------------------------
     P E2A             B
     D  E2A            PI             1A
     D   chrEBCDIC                    1A   CONST

     D chrASCII        S              1

     C                   EVAL      chrASCII=chrEBCDIC
     C                   RETURN    chrASCII
     P E2A             E
