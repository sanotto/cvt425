*SRCMBRTXT:Link-Host to Host-Ejecutar Msg Transacc
     H*-------------------------------------------------------------------------
     H* Sistema     : Conexión Host To Host
     H* Módulo      : Ejecutador de Transacciones
     H* Autor       : Santiago Ottonello
     H* Fec. Inicial: 2006/11/30
     H*
     H* Recibe      :
     H* Devuelve    :
     H*
     H**************************************************************************
     H* Responsabilidades:
     H**************************************************************************
     P ExecTrn         B
     D ExecTrn         PI             2A

     D rc              S              2A
     D RuleNotFound    S               N   INZ(*ON)
     D TarjetaDS       DS
     D  Track2                 1     37A
     D  Tarjet                 1     16S 0

     c                   MoveL(P)  iMsgType      TRNMSG
     c                   MoveL(P)  wMsgFldVal(3) TRCTRN
     C     KeyLIMTRN     Chain     RELIMTRN
     C                   If        not %Found()
     C* ... ... ... ...  Transacción inválida
     C                   Eval      rc='12'
     C                   Else
     C                   ExSr      DoTransac
     C                   Endif
     C                   Return    rc
     C*------------------
     C*   DoTransac: Efectúa las acciones que tenga definida la transaccion
     C*------------------
     C     DoTransac     BegSr
     C*
     C     TRVCAS        CasEq     'S'           DoVCAS
     C     TRVTAR        CasEq     'S'           DoVTAR
     C     TRVCEF        CasEq     'S'           DoVCEF
     C     TRVCEC        CasEq     'S'           DoVCEC
     C     TRCODD        CasNe     *ZERO         DoCODD
     C     TRCODC        CasNe     *ZERO         DoCODC
     C     TRIPGM        CasNe     *Blanks       DoIPGM
     C                   EndCs
     C*
     C                   EndSr
     C*------------------
     C*   DoVCAS: Es un Contrasiento
     C*------------------
     C     DoVTAR        BegSr
     C* ... Verificar si la transaccion ya se recibio
     C* ... ... Se Recibio y no esta contrasentada contrasentarla
     C* ... ... ... Si es una transf interna  hay que contra el deb y el cre
     C* ... ... No se recibio todavia contestar con 68 para forzar reenvio
     C                   EndSr
     C*------------------
     C*   DoVTAR: Valida la tarjeta de la transacción
     C*------------------
     C     DoVTAR        BegSr
|    C* ... Validar tarjeta contra maestro de tarjetas
     C                   Eval      Track2=wMsgFldVal(35)
|    C     Tarjet        CHAIN     LIKMTR03                           51
|+---C                   IF        Not %Found()
||   C* ... Si no existe en Validar contra Tarjeta miembro
||   C           TARJET    CHAINLIKMBR               51
||+--C           *IN51     IFEQ *ON
|||  C                     MOVEL@ERR,9    WWDERR
|||  C                     MOVEL'14'      CORES1
|||  C                     GOTO FINSAD
||+--C                     ENDIF
||   C                     Z-ADD7         TAILDB
|+---C                     ENDIF
     C                   EndSr
     C*------------------
     C*   DoVCEF: Valida el contador de Efectivo
     C*------------------
     C     DoVCEF        BegSr
     C                   EndSr
     C*------------------
     C*   DoVCEC: Valida el contador de Extracciones
     C*------------------
     C     DoVCEC        BegSr
     C                   EndSr
     >*------------------
     C*   DoCODD: Valida y efectúa debito si corresponde
     C*------------------
     C     DoCODD        BegSr
     C* ... Si hay def un pgm Host dejar el impacto al mismo
     C                   If        TRIPGM <> *BLANKS
     C                   LeaveSr
     C                   Endif
     C                   EndSr
     C*------------------
     C*   DoCODC: Valida y efectúa Crédito si corresponde
     C*------------------
     C     DoCODC        BegSr
     C* ... Si hay def un pgm Host dejar el impacto al mismo
     C                   If        TRIPGM <> *BLANKS
     C                   LeaveSr
     C                   Endif
     C* ... Si ademas de este credito la trn tiene un debito tomar datos del
     C*     credito de la segunda cuenta
     C                   EndSr
     C*------------------
     C*   DoIPGM: Llam a Programa Host Si Corresponde
     C*------------------
     C     DoIPGM        BegSr
     C                   EndSr
     P ExecTrn         E
