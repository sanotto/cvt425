*SRCMBRTXT:VARIOS: Carga LILOGD desKK PF (arc c kk
     FKK        IP   E             DISK
     FLILOGD    O    E             DISK
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
     D  WKOPCO                60     61
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

     D  WKUMOV               157    468
     D* --- DATOS ORIGINALES DEL MOV REVERSADO, VIENE DENTRO DE UMOV ----
     D   WRFASI              157    164
     D   WRHING              165    170
     D   WRNREF              171    182
     D* --- TIPO DE DEPOSITO                                         ----
     D   WRTIDE              195    195
     D* --- TIPO DE TERMINAL 54-HOME BAMKIN 56-LINK CELULAR 75-LINK CEL--
     D*     TIPO DE TERMINAL 63-POS C/INGRESO MANUAL 64-POS CON LECT BANDA MAGNE
     D   WRTITE              196    197
     D* --- PARA AHORRAR ESPACIO Y NO MOD LA ESTR PREACORDADA        ----
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
     c                   MOVEL     KKFLD         REGDSS
     C                   ExSr      CheckDig
     C*
     C                   Move      WKTIPT        LRTIPT
     C                   Move      *blanks       LRICON
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
     C                   Move      *ZERO         LR$SAL
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
     C                   Z-Add     *ZERO         LRIRRN
     C                   Move      WKNTAR        LRNTAR
     C                   Z-Add     WKMDIS        LRMDIS
     C                   Move      WKUMOV        LRUMOV
     C*
     C                   Write     RELILOGD
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
     C     ' ':'0'       XLATE     WCDBCT        WCDBCT
     C     ' ':'0'       XLATE     WCCRCT        WCCRCT
     C                   ENDSR
