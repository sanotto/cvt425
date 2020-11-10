*SRCMBRTXT:Switch-Adapter      -Manejador Principa
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD00MA                                       *
     H*                                                               *
     H*  PROGRAM NO:   ADAPTADOR DE SWITCH                            *
     H*                                                               *
     H*  DATE: 15/02/2007                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     H*---------------------------------------------------------------*
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DatFMT(*ISO)   DATEDIT(*YMD)
     H BNDDIR('QC2LE') BNDDIR('LE00525/TO10BD   ')

     FLICONN    UF   E           K DISK
     FLILOGF    O    E           K DISK
     FBANUME    UF A E           K DISK
     F*
     FACCTAC    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     F*
     FSGSYSV    IF   E             DISK
     D*-------------------------------------------------------------------------
     D* Definiciones de Prototipos
     D*-------------------------------------------------------------------------
     D SysError        PR
     D   peMsg                      256A   const
     D GetUsrCmd       PR             1A
     D  ConnNme                       6A
     D Translate       PR                  ExtPgm('QDCXLATE')
     D    peLength                    5P 0 const
     D    peBuffer                32766A   options(*varsize)
     D    peTable                    10A   const
     D*--------------------------------------------------------------------
     D/copy LE00525/sockets,socket_h
     D/copy LE00525/sockets,ERRNO_H
     D/copy LE00525/sockets,sockutil_h
     D/copy LE00525/to10US,ioprots
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*-------------------------------------------------------------------------
     D* Estructura del Mensaje del Switch
     D*-------------------------------------------------------------------------
     D REGDS           DS
     D REGDSS                  1    622

     D WKDIRE                  1      3

     D WCFING                  4     11
     D WKFING                  4     11  0

     D WCHING                 12     17  0
     D WKHING                 12     17  0

     D  WKTIPT                18     18

     D WCFASI                 19     26
     D WKFASI                 19     26  0

     D WCNSEQ                 27     32
     D WKNSEQ                 27     32  0

     D  WKNREF                33     44
     D  WKIATM                45     59
     D  WKTLNK                60     65
     D  WKMREV                66     67

     D WCDBCO                 68     70
     D WKDBCO                 68     70  0

     D WCDBSU                 71     75
     D WKDBSU                 71     75  0

     D WCDBMO                 76     80
     D WKDBMO                 76     80  0

     D  WKDBTC                81     82

     D WCDBCT                 83     92
     D WKDBCT                 83     92  0

     D WCDBIM                 93    104
     D WKDBIM                 93    104  2

     D WCCRCO                105    107
     D WKCRCO                105    107  0

     D WCCRSU                108    112
     D WKCRSU                108    112  0

     D WCCRMO                113    117
     D WKCRMO                113    117  0

     D  WKCRTC               118    119

     D WCCRCT                120    129
     D WKCRCT                120    129  0

     D WCCRIM                130    141
     D WKCRIM                130    141  2

     D  WKCORE               142    144

     D WC$SAL                145    156
     D WK$SAL                145    156  2

     D  WKUMOV               157    468
     D  WKSEIN               469    472
     D  WKSETC               473    474
     D  WKSECT               475    493
     D  WKENTE               494    496
     D  WKCLTE               497    515
     D  WKEMPR               513    518
     D  WKPROD               519    521
     D  WKARTI               522    553
     D  WKCBUD               554    575

     D WCCOVE                576    583
     D WKCOVE                576    583  3

     D WCCOCO                584    591
     D WKCOCO                584    591  3

     D WCmdis                592    603
     D WKmdis                592    603  2

     D WKNTAR                604    622
     D*-------------------------------------------------------------------------
     D* DS Para convertir Bytes a Leer+
     D*-------------------------------------------------------------------------
     D CNBDS           DS
     D  CNBCHR                 1      4
     D  CNBNUM                 1      4S 0


     D ExitPgm         S               N   INZ(*OFF)

     D MIBUF           DS
     D  MIBUFLIN               1    626
     D  MIBUFSZE               1      4
     D  MIBUFDAT               5    626

     D sock            S             10I 0
     D port            S              5U 0
     D addrlen         S             10I 0
     D ch              S              1A
     D host            s             32A
     D file            s             32A
     D addr            s             10U 0
     D p_Connto        S               *
     D RC              S             10I 0
     D RecBuf          S            132A
     D CountBytes      S             10I 0
     D err             S             10I 0
     D x               S              3  0
     D fname           S            256A
     D ErrorText       S             80A

     C                   Exsr      OpenConn
     C                   Exsr      GetMsg
     C                   DoW       ExitPgm  = *OFF
     C                   ExSr      PrcMsg
     C                   Exsr      GetMsg
     C                   EndDo
     C                   ExSr      GenPBF
     C                   ExSr      SendLogOff
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C*------------------
     C* PrcMsg: Procesa Mensaje
     C*------------------
     C     PrcMsg        BegSr
     C*
     C                   Move      WKTIPT        LRTIPT
     C                   Select
     C                   When      WKTIPT = 'L'
     C                   ExSr      SendLogon
     C                   When      WKTIPT = 'E'
     C                   ExSr      SendEcho
     C                   When      WKTLNK = '941000' OR WKTLNK='942000'
     C                   ExSr      GetUMOV
     C                   When      WKTLNK = 'D21000' OR WKTLNK='D22000'
     C                   ExSr      GetCBU
     C                   When      WKTLNK = '1B1000' OR WKTLNK='1B2000'
     C                   ExSr      TrnCBU
     C                   Other
     C                   ExSr      ExecTrn
     C                   EndSl
     C*
     C                   ExSr      SendMsg
     C*
     C                   EndSr
     C*------------------
     C* TrnCBU : Transferencias Via CBU
     C*------------------
     C     TrnCBU        BegSr
     C*
     C                   Z-ADD     309           TRINBA            4 0
     C                   Z-ADD     0             TRCUI0           11 0
     C                   MOVE      WKDBTC        TRDBTC            2
     C                   Z-ADD     WKDBSU        TRDBSU            5 0
     C                   Z-ADD     WKDBCT        TRDBCT           11 0
     C                   MOVE      WKCBUD        TRCBUC           23
     C                   MOVEL     WKCBUD        WWIBAN            3
     C                   MOVE      WWIBAN        TRIBAN            4 0
     C                   Z-ADD     *ZERO         TRCUI1           11 0
     C                   MOVE      *BLANKS       TRCRTC            2
     C                   Z-ADD     *ZERO         TRCRSU            5 0
     C                   Z-ADD     *ZERO         TRCRCT           11 0
     C                   MOVE      *BLANKS       TRDOMI           40
     C                   MOVE      'ALQ'         TRMOTR            3
     C                   Z-ADD     WKDBIM        TR$IMP           15 2
     C                   MOVE      *BLANKS       TRMORE            3
     C                   MOVE      *BLANKS       TRDAV1           40
     C                   Call      'SWAD00R6'
     C                   PARM                    WKTIPT
     C                   PARM                    TRINBA            4 0
     C                   PARM                    TRCUI0           11 0
     C                   PARM                    TRDBTC            2
     C                   PARM                    TRDBSU            5 0
     C                   PARM                    TRDBCT           11 0
     C                   PARM                    TRCBUC           23
     C                   PARM                    TRIBAN            4 0
     C                   PARM                    TRCUI1           11 0
     C                   PARM                    TRCRTC            2
     C                   PARM                    TRCRSU            5 0
     C                   PARM                    TRCRCT           11 0
     C                   PARM                    TRDOMI           40
     C                   PARM                    TRMOTR            3
     C                   PARM                    TR$IMP           15 2
     C                   PARM                    TRMORE            3
     C                   PARM                    TRDAV1           40
     C                   ExSr      GetSal
     C*
     C                   EndSr
     C*------------------
     C* GetUMOV: Obtener Ultimos Movimientos
     C*------------------
     C     GetUMOV       BegSr
     C*
     C                   ExSr      CheckDig
     C                   Z-Add     WKDBSU        WWDBSU            5 0
     C                   Z-Add     WKDBCT        WWDBCT           10 0
     C                   Call      'SWAD00R5'
     C                   Parm                    WKDBTC
     C                   Parm                    WWDBSU
     C                   Parm                    WWDBCT
     C                   Parm                    WKUMOV
     C*
     C                   Eval      WKCORE='000'
     C*
     C                   EndSr
     C*------------------
     C* GetCBU: Obtener CBU
     C*------------------
     C     GetCBU        BegSr
     C*
     C                   If        WKDBTC='CC'
     C                   Z-Add     1             WWTIPO            2 0
     C                   Else
     C                   Z-Add     3             WWTIPO            2 0
     C                   EndIf
     C*
     C                   Z-ADD     *ZEROS        WWBLQ1            8 0
     C                   Z-ADD     *ZEROS        WWBLQ2           14 0
     C                   Z-ADD     *ZEROS        WWBLQ3           22 0
     C
     C                   CALL      'CBU000RG'
     C                   PARM                    WWTIPO
     C                   PARM                    WKDBSU
     C                   PARM                    WKDBCT
     C                   PARM                    WWBLQ1
     C                   PARM                    WWBLQ2
     C                   PARM                    WWBLQ3
     C*
     C                   Movel(P)  WWBLQ3        WKCBUD
     C*
     C                   EndSr
     C*------------------
     C* ExecTrn: Ejecutar Transaccion
     C*------------------
     C     ExecTrn       BegSr
     C*
     C                   Move      WKTIPT        WWTIPT            1
     C                   Move      *ZEROS        WWERDB            3
     C                   Move      *ZEROS        WWERCR            3
     C* ... Vino Débito  ?
     C                   If        WKDBCO<>0
     C                   ExSr      DoDebit
     C                   EndIf
     C* ... Vino Crédito ?
     C                   If        WKCRCO<>0  and WWERDB = '000'
     C                   ExSr      DoCredit
     C                   EndIf
     C* ... Verificar errores
     C                   If        WKDBCO <> 0 AND WKCRCO <> 0 AND
     C                             WWERCR <> '000'
     C                   ExSr      RevDebito
     C                   EndIf
     C* ... Recuperar el Saldo de la Transacción
     C                   ExSr      GetSal
     C*
     C                   EndSr
     C*------------------
     C* RevDebito: Reversa Debito x error en credito
     C*------------------
     C     RevDebito     BegSr
     C* ... Mover campos de debito a parms impactador
     C                   Move      WKDBTC        WWSUBS            2
     C                   Move      WKDBCO        WWIMCC            3 0
     C                   Move      WKDBSU        WWISUC            5 0
     C                   Move      WKDBCT        WWICCC           11 0
     C                   MoveL     WKNREF        WWICHE            7 0
     C                   Z-Add     WKFASI        PAFASI            8 0
     C                   Z-Add     WKHING        PAHALT            6 0
     C* ... Marcar tipo de transaccion como reversa
     C                   Move      'R'           WWTIPT
     C                   ExSr      DoImpact
     C                   EndSr
     C*------------------
     C* DoDebit :  Impactar Debito
     C*------------------
     C     DoDebit       BegSr
     C* ...              Mover campos de debito a parametros impactador
     C                   Move      WKDBTC        WWSUBS            2
     C                   Move      WKDBCO        WWIMCC            3 0
     C                   Move      WKDBSU        WWISUC            5 0
     C                   Move      WKDBCT        WWICCC           11 0
     C                   MoveL     WKNREF        WWICHE            7 0
     C                   Z-Add     WKFASI        PAFASI            8 0
     C                   Z-Add     WKHING        PAHALT            6 0
     C                   Z-Add     WKDBIM        WWmdis           15 2
     C                   ExSr      DoImpact
     C                   MoveL(P)  WKCORE        WWERDB
     C*
     C* ... Si el Impacto fue una reversa parcial impactar el imp entregado como
     C*     Forzado si ya se efectuo no impactar parcial
     C                   If        WWTIPT='R' and WKmdis <> 0 and WWERDB='000'
     C                   Move      'F'           WWTIPT
     C                   Z-Add     WKmdis        WWmdis           15 2
     C                   ExSr      DoImpact
     C                   EndIf
     C* ... Si la reversa ya se efectuo igual hay que contestar OK
     C                   If         WWERDB='114' or WWERDB='214'
     C                   Move      *ZEROS        WWERDb
     C                   Endif
     C*
     C                   EndSr
     C*------------------
     C* DoCredit:  Impactar Crédito
     C*------------------
     C     DoCredit      BegSr
     C*
     C* ...              Mover campos de credito  parametros impactador
     C                   Move      WKCRTC        WWSUBS            2
     C                   Move      WKCRCO        WWIMCC            3 0
     C                   Move      WKCRSU        WWISUC            5 0
     C                   Move      WKCRCT        WWICCC           11 0
     C                   MoveL     WKNREF        WWICHE            7 0
     C                   Z-Add     WKFASI        PAFASI            8 0
     C                   Z-Add     WKHING        PAHALT            6 0
     C                   Z-Add     WKCRIM        WWmdis           15 2
     C                   ExSr      DoImpact
     C                   MoveL(P)  WKCORE        WWERCR
     C*
     C                   EndSr
     C*------------------
     C* GetSal  :  Recupera el Saldo de la Cuenta Operada
     C*------------------
     C     GetSal        BegSr
     C*
     C                   Z-Add     *ZERO         WWNCTA
     C                   Move      *BLANKS       WWTIPC            2
     C* ... Si Debito=>Cta Debito, si Credito=>Cta Credito Ambos=>Cta Debito
     C                   Select
     C                   When      WKDBCO <> 0  and WKCRCO =  0
     C                   Eval      WWNSUC=WKDBSU
     C                   Eval      WWNCTA=WKDBCT
     C                   Eval      WWTIPC=WKDBTC
     C                   When      WKDBCO =  0  and WKCRCO <> 0
     C                   Eval      WWNSUC=WKCRSU
     C                   Eval      WWNCTA=WKCRCT
     C                   Eval      WWTIPC=WKCRTC
     C                   When      WKDBCO <> 0  and WKCRCO <> 0
     C                   Eval      WWNSUC=WKDBSU
     C                   Eval      WWNCTA=WKDBCT
     C                   Eval      WWTIPC=WKDBTC
     C                   EndSl
     C*
     C  N99              Z-ADD     *ZERO         WK$SAL
     C                   If        WWTIPC='CC'
     C     KCuenta       Chain     RECCCTCT                           99
     C  N99              Z-ADD     BM$SOP        WK$SAL
     C                   Else
     C     KCuenta       Chain     REACCTAC                           99
     C  N99              Z-ADD     FU$SOP        WK$SAL
     C                   EndIf
     C*
     C                   EndSr
     C*------------------
     C* doImpact:  Efectua el impacto del Mov. Original
     C*------------------
     C     doImpact      BegSr
     C*
     C                   If        WWSUBS='CC'
     C                   Eval      WWIHOL='SWAD00R1'
     C                   Else
     C                   Eval      WWIHOL='SWAD00R2'
     C                   EndIf
     C*
     C                   MoveL     *BLANK        WWDERR           70
     C                   Z-Add     *ZERO         WWIPOS            3 0
     C                   MoveL(P)  WKIATM        PAIATM           16
     C                   MoveL(P)  WWTIPT        PATIPT            1
     C*
     C                   CALL      WWIHOL
     C                   PARM                    PATIPT
     C                   PARM                    PAIATM
     C                   PARM                    WWIMCC
     C                   PARM                    WWISUC
     C                   PARM                    WWICCC
     C                   PARM                    WWICHE
     C                   PARM                    WWmdis
     C                   PARM                    PAIERR           40
     C                   PARM                    PAFASI
     C                   PARM                    PAHALT
     C*
     C                   Move(P)   PAIERR        LRROBS
     C                   MoveL(P)  PAIERR        WKCORE
     C                   IF        WKCORE = '   '
     C                   Move      *ZEROS        WKCORE
     C                   EndIf
     C*
     C                   If        WWTIPT='F' AND WKCORE <> '000'
     C                   ExSr      ImpactBol
     C                   EndIf
     C*
     C                   EndSr
     C*------------------
     C* ImpactBol: Efectua el impacto del Mov. En BOLSA
     C*------------------
     C     ImpactBol     BegSr
     C*
     C                   CALL      'SWAD00R3'
     C                   PARM                    WNIULN
     C                   PARM                    WWSUBS
     C                   PARM                    WWIMCC
     C                   PARM                    WWISUC
     C                   PARM                    WWICCC
     C                   PARM                    WWICHE
     C                   PARM                    WWmdis
     C                   PARM                    PAFASI
     C*
     C                   EndSr
     C*------------------
     C* SendEcho: Escribe campos para Echo Test
     C*------------------
     C     SendEcho      BegSr
     C*
     C                   Clear                   REGDSS
     C                   Eval      WKTIPT = 'E'
     C                   Eval      WKCORE ='000'
     C                   Time                    WKHING
     C                   MOVE      AASFEI        WKFING
     C*
     C                   EndSr
     C*------------------
     C* SendLogon Escribe campos para Logon
     C*------------------
     C     SendLogon     BegSr
     C*
     C                   Clear                   REGDSS
     C                   Eval      WKTIPT = 'L'
     C                   Eval      WKCORE ='000'
     C                   Time                    WKHING
     C                   MOVE      AASFEI        WKFING
     C*
     C                   EndSr

     C*------------------
     C* CheckDig: Valida Para que no haya errores de digito/signo
     C*------------------
     C     CheckDig      BegSr
     C*
     C     WCFING        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCFING
     C     WCFASI        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCFASI
     C     WCNSEQ        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCNSEQ
     C     WCDBCO        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCDBCO
     C     WCDBSU        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCDBSU
     C     WCDBMO        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCDBMO
     C     WCDBCT        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCDBCT
     C     WCDBIM        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCDBIM
     C     WCCRCO        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCRCO
     C     WCCRSU        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCRSU
     C     WCCRMO        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCRMO
     C     WCCRCT        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCRCT
     C     WCCRIM        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCRIM
     C     WC$SAL        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WC$SAL
     C     WCCOVE        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCOVE
     C     WCCOCO        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCCOCO
     C     WCmdis        COMP      *BLANKS                                99
     C   99              MOVE      *ZEROS        WCmdis
     C*
     C                   ENDSR
     C*------------------
     C* WrtLog: Escribir LOG del Mensaje
     C*------------------
     C     WrtLog        BegSr
     C*
     C                   ExSr      CheckDig
     C*
     C                   Move      WKTIPT        LRTIPT
     C                   Move      WWICON        LRICON
     C                   Move      WKDIRE        LRDIRE
     C                   Move      WKFING        LRFING
     C                   Move      WKHING        LRHING
     C                   Move      WKFASI        LRFASI
     C                   Move      WKNSEQ        LRNSEQ
     C                   Move      WKNREF        LRNREF
     C                   Move      WKIATM        LRIATM
     C                   Move      WKTLNK        LRTLNK
     C                   Move      WKMREV        LRMREV
     C                   Move      WKDBCO        LRDBCO
     C                   Move      WKDBSU        LRDBSU
     C                   Move      WKDBMO        LRDBMO
     C                   Move      WKDBTC        LRDBTC
     C                   Move      WKDBCT        LRDBCT
     C                   Move      WKDBIM        LRDBIM
     C                   Move      WKCRCO        LRCRCO
     C                   Move      WKCRSU        LRCRSU
     C                   Move      WKCRMO        LRCRMO
     C                   Move      WKCRTC        LRCRTC
     C                   Move      WKCRCT        LRCRCT
     C                   Move      WKCRIM        LRCRIM
     C                   Move      WKCORE        LRCORE
     C                   Move      WK$SAL        LR$SAL
     C                   Move      WKUMOV        LRUMOV
     C                   Move      WKSEIN        LRSEIN
     C                   Move      WKSETC        LRSETC
     C                   Move      WKSECT        LRSECT
     C                   Move      WKENTE        LRENTE
     C                   Move      WKCLTE        LRCLTE
     C                   Move      WKEMPR        LREMPR
     C                   Move      WKPROD        LRPROD
     C                   Move      WKARTI        LRARTI
     C                   Move      WKCBUD        LRCBUD
     C                   Move      WKCOCO        LRCOCO
     C                   Move      WKCOVE        LRCOVE
     C                   Z-Add     WNIULN        LRIRRN
     C                   Move      WKNTAR        LRNTAR
     C                   Z-Add     WKMDIS        LRMDIS
     C*
     C                   Write     RELILOGF
     C*
     C                   EndSr
     C*------------------
     C* GetNbr - Obtiene Nro Correlativo de Registro
     C*------------------
     C     GetNbr        BegSr
     C     KBANUME       Chain     REBANUME                           99
+----C                   If        *IN99 = *ON
Move C                   Z-Add     *ZERO         WNIULN
Move C                   Write     REBANUME
     C                   Else
     C                   Add       1             WNIULN
     C                   Update    REBANUME
     C                   EndIf
     C                   EndSr
     C*------------------
     C* EndPgm: Finalizar el Programa
     C*------------------
     C     EndPgm        BegSr
     C*
     C                   Exsr      CloseConn
     C*
     c                   CallP     SysError(ErrorText)
     C*
     C     WWICON        Chain     RELICONN                           99
     C                   If        *IN99 = *OFF
     C                   MOVEL(P)  @PUSER        COIUSR
     C                   Z-ADD     AASFEI        COFLST
     C                   TIME                    COHLST
     C                   Eval      CODMSG=ErrorText
     C                   Update    RELICONN
     C                   EndIf
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*------------------
>    C* GenPBF: Genera archivo PBF y envia a Servidor Switch
     C*------------------
     C     GenPBF        BegSr
     C*
     C                   Call      'AC0084CL'
     C*
     C                   Z-Add     *DATE         WWFENU            8 0
     C                   Time                    WWHONU            6 0
     C                   Move      WWFENU        WWFECH            8
     C                   Move      WWFECH        WWMEDI            4
     C                   Move      WWHONU        WWHOCH            8
     C                   Eval      LRROBS='PBF0309'+WWFECH+WWHOCH+' '+
     C                                    COIPAD +' '+WWMEDI
     C* ... Enviarlo por FTP al servidor en cuestion
     C                   Call      'SWAD00R4'
     C                   Parm                    LRROBS
     C*
     C     *DTAARA       Define    *LDA          ok                1
     C                   If        ok='0'
     C                   Eval      LRROBS='ER'
     C                   Else
     C                   Eval      LRROBS='OK'+'PBF0309'+WWFECH+WWHOCH
     C                   EndIf
     C*
     C                   EndSr
     C*------------------
     C* GetMsg: Lee estructura desde el puerto y ordenes desde cola de datos
     C*------------------
     C     GetMsg        BegSr
     C*
     C* ... Lee el puerto
     C                   EVAL      rc = rdline(sock:%ADDR(CNBCHR):
     C                             %SIZE(CNBCHR) : *On               )
     C                   If        rc=0  or CNBCHR=*blanks
     c                   Eval      ErrorText='Error en lectura             '
     c                   ExSr      EndPgm
     c                   Endif
     c                   Z-Add     *Zero         offset           10 0
     C                   DoW       CNBNUM > 0
     C                   Clear                   regdss
     C                   EVAL      rc = rdline(sock:%ADDR(REGDSS)+offset:
     C                             CNBNUM: *On: X'0A' : X'0D')
     C                   If        rc=0
     c                   Eval      ErrorText='Error en lectura             '
     c                   ExSr      EndPgm
     C                   EndIf
     C                   ExSr      CheckDig
     C                   ExSr      GetNbr
     C                   Eval      WKDIRE='REQ'
     C                   ExSr      WrtLog
     c                   add       rc            offset
     c                   Sub       rc            CNBNUM
     C                   EndDo
     C* ... Lee cola de datos
     C                   Eval      ExitPgm      =GetUsrCmd(WWICON)
     C*
     C                   EndSr
     C*------------------
     C* SendLogOff: Envia un LogOff
     C*------------------
     C     SendLogOff    BegSr
     C*
     C* ... Escribe datos en campo de respuesta
     C                   Time                    WKHING
     C                   Z-Add     *DATE         WKFING
     C* ... Envía mensaje
     C                   ExSr      SendMsg
     C*
     C     WWICON        Chain     RELICONN
     C                   If        %Found()
     C                   MOVEL(P)  @PUSER        COIUSR
     C                   Z-ADD     AASFEI        COFLST
     C                   TIME                    COHLST
     C                   Eval      CODMSG='Logoff Enviado'
     C                   Update    RELICONN
     C                   Endif
     C*
     C                   EndSr
     C*------------------
     C* SendMsg : Envia la estructura por el puerto y graba el LOG
     C*------------------
     C     SendMsg       BegSr
     C*
     C                   Eval      MIBUFSZE='0622'
     C                   Eval      WKDIRE='RSP'
     C                   Eval      MIBUFDAT=REGDSS
     C*                  callp     WrLine(sock: MIBUFLIN )
     C                   callp     Translate(%size(MIBUFLIN):
     C                                              MIBUFLIN: 'QTCPASC')
     C                   eval      rc = send(sock: %addr(MIBUFLIN):
     C                                             %size(MIBUFLIN):0)
     C                   ExSr      WrtLog
     C*
     C                   EndSr
     C*------------------
     C* *INZSR: Inicialización
     C*------------------
     C     *INZSR        BegSr
     C*
     C     *ENTRY        PLIST
     C                   PARM                    WWICON
     C*
     C     *LIKE         Define    COICON        WWICON
     C     *LIKE         Define    FUISUC        WWNSUC
     C     *LIKE         Define    FUICAH        WWNCTA
     C*
     C                   Move      *BLANKS       WWIHOL           10
     C*
     C     KBANUME       KLIST
     C                   KFLD                    WNIPF1
     C                   KFLD                    WNIPF2
     C                   KFLD                    WNIPF3
     C                   KFLD                    WNIPF4
     C                   KFLD                    WNIPF5
     C     KCUENTA       KLIST
     C                   KFLD                    WWNSUC
     C                   KFLD                    WWNCTA
     C*
     C                   MOVEL(P)  'SWITCH'      WNIPF1
     C                   MOVEL(P)  'LILOGF'      WNIPF2
     C                   MOVE      *BLANKS       WNIPF3
     C                   MOVE      *BLANKS       WNIPF4
     C                   MOVE      *BLANKS       WNIPF5
     C*
     C     1             CHAIN     SGSYSV                             99
     C*
     C                   Clear                   REGDSS
     C                   Z-Add     *Zero         WKCRCO
     C                   Z-Add     *Zero         WKDBCO
     C*
     C     WWICON        Chain     RELICONN
     C                   If        Not %Found()
     C                   Eval      ErrorText='Definicion de conexión:'+
     C                             WWICON+ ' NO Encontrada'
     C                   ExSr      EndPgm
     C                   Else
     C                   MOVEL(P)  @PUSER        COIUSR
     C                   Z-ADD     AASFEI        COFLST
     C                   TIME                    COHLST
     C                   Eval      CODMSG='Conexión levantada'
     C                   Update    RELICONN
     C                   Endif
     C*
     C                   EndSr
     C*------------------
     C* OpenConn: Abre puerto TCP/IP en la IP y Puerto Especificados
     C*------------------
     C     OpenConn      BegSr
     C*
     c                   eval      port = COPORT

     C*************************************************
     C* Get the 32-bit network IP address for the host
     C*  that was supplied by the user:
     C*************************************************
     c                   eval      addr = inet_addr(%trim(COIPAD))
     c                   if        addr = INADDR_NONE
     c                   eval      p_hostent = gethostbyname(%trim(host))
     c                   if        p_hostent = *NULL
     c                   Eval      ErrorText='No pude encontrar el host    '
     c                   ExSr      EndPgm
     c                   endif
     c                   eval      addr = h_addr
     c                   endif

     C*************************************************
     C* Create a socket
     C*************************************************
     c                   eval      sock = socket(AF_INET: SOCK_STREAM:
     c                                           IPPROTO_IP)
     c                   if        sock < 0
     c                   Eval      ErrorText='No pude abrir el socket   '
     c                   ExSr      EndPgm
     c                   endif

     C*************************************************
     C* Create a socket address structure that
     C*   describes the host & port we wanted to
     C*   connect to
     C*************************************************
     c                   eval      addrlen = %size(sockaddr)
     c                   alloc     addrlen       p_connto

     c                   eval      p_sockaddr = p_connto
     c                   eval      sin_family = AF_INET
     c                   eval      sin_addr = addr
     c                   eval      sin_port = port
     c                   eval      sin_zero = *ALLx'00'

     C*************************************************
     C* Connect to the requested host
     C*************************************************
     C                   if        connect(sock: p_connto: addrlen) < 0
     c                   Eval      ErrorText='La conexión:'+COICON+' No cont'+
     C                             'esta'
     c                   ExSr      EndPgm
     c                   return
     c                   endif
     C                   EndSr
     C*------------------
     C* CerrarrConexión: Cierra el Puerto
     C*------------------
     C     CloseConn     BegSr
     C*
     c                   callp     close(sock)
     C*
     C                   EndSr
     C**************************************************************************
     C* ZONA DE PROCEDIMIENTOS
     C**************************************************************************
     P SysError        B
     D SysError        PI
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
     c                               peMsg: wwMsgLen: '*INFO':
     c                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E

     P GetUsrCmd       B
     D GetUsrCmd       PI             1A
     D  ConnNme                       6A

     D QConCmd         S             10A
     D Command         S              1A

     C                   Eval      Command=*OFF
     C                   Eval      QConCmd='CTLQ'+ConnNme
     C                   Call      'QRCVDTAQ'
     C                   Parm                    QConCmd
     C                   Parm      'QGPL'        QLib             10
     C                   Parm      1             QDtaLen           5 0
     C                   Parm                    Command
     C                   Parm      0             QWaitTime         5 0
     C*
     C                   Return    Command
     C*

     P GetUsrCmd       E

