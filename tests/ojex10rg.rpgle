*SRCMBRTXT:OJ-Export Arch. Internet-Valida Respues
     FOJMRES02  IF   E           K DISK
     FOJMOFI01  IF   E           K DISK
     FOJNTDO01  IF   E           K DISK
     FOJCTPR    IF   E           K DISK
     FOJEX10PR  O    E             PRINTER OFLIND(*IN99)
     FOJTRES    O    F  378        DISK
     D C               S              2    DIM(12) CTDATA PERRCD(1)
     D V               S             14    DIM(12) ALT(C)
     D*-------------------------------------------------------------------------
     D FECIDS          DS
     D  FECINV                 1      8
     D  AÑO                    1      4
     D  MES                    5      6
     D  DIA                    7      8
     D OK              C                   CONST('RESPUESTA CORRECTA  ')
     D E1              C                   CONST('OF. NO ENCONTRADO   ')
     D E2              C                   CONST('OF. CONTEST. TOTALM.')
     D E3              C                   CONST('TIPO DE OF. INEXIST.')
     D E4              C                   CONST('TIPO DE CTA INEXIST.')
     D E5              C                   CONST('CTA NO PUEDE SER 0  ')
     D E6              C                   CONST('IMPORTE DEBE SER > 0')
     D E7              C                   CONST('FEC EMBARGO INVALIDA')
     D E8              C                   CONST('ES CLTE <> DE S/N   ')
     D E9              C                   CONST('TIPO DOC INVALIDO   ')
     D E10             C                   CONST('NRO DOC DEBE SER <>0')
     D E11             C                   CONST('TIPO ACT INVALIDA   ')
     D E12             C                   CONST('CANT DEBE SER >= 0  ')
     D E13             C                   CONST('COTIZ DEBE SER >= 0 ')
     D E14             C                   CONST('FECHA COTIZ INVALIDA')
     D*
     D LINEA           S            378
     D KEYCHAR         S              1
     I*-------------------------------------------------------------------------
     C                   WRITE     ERRTIT
     C     KMRES         SETLL     REOJMRES
     C     KMRES         READE     REOJMRES                               25
     C     *IN25         DOWEQ     *OFF
     C  N70              MOVE      *ON           ISTS
     C  N70              MOVE      *ON           *IN70
     C                   MOVE      *ON           CHKSTS            1
     C                   EXSR      CHKRES
     C     CHKSTS        IFEQ      *ON
     C                   EXCEPT    DETAL
     C                   MOVEL(P)  OK            ERRTXT
     C   99              WRITE     ERRTIT
     C                   WRITE     ERRDET
     C                   ENDIF
     C     KMRES         READE     REOJMRES                               25
     C                   ENDDO
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C*--------------->CHEQUEAR SI FORMATO DE LA RESPUESTA ES CORRECTO
     C     CHKRES        BEGSR
     C                   SETOFF                                       98
     C                   MOVE      *BLANKS       LINEA
     C                   EXSR      GETOFI
     C                   EVAL      KEYCHAR=%SUBST(%TRIM(OJITRG):1:1)
     C                   MOVEL     KEYCHAR       KEY               2
     C                   IF        MRIOPT <> 'S'
     C                   MOVE      'N'           MRIOPT
     C                   ENDIF
     C                   MOVE      MRIOPT        KEY
     C                   Z-ADD     1             X                 2 0
     C     KEY           LOOKUP    C(X)                                   25
     C     *IN25         IFEQ      *ON
     C                   MOVE      V(X)          VEC              14
     C                   MOVEA     VEC           *IN(80)
     C                   EVAL      LINEA=FECRES+'@'+CODBAN+'@'+ %CHAR(OJININ)
     C                                   +'@'+ %CHAR(MRINUI) +'@'
     C
     C   83              EXSR      COL04
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   84              EXSR      COL05
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   85              EXSR      COL06
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   86              EXSR      COL07
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   87              EXSR      COL08
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   88              EXSR      COL09
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   89              EXSR      COL10
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   90              EXSR      COL11
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   91              EXSR      COL12
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   92              EXSR      COL13
     C                   EVAL      LINEA=%TRIM(LINEA)+'@'
     C   93              EXSR      COL14
     C                   ENDIF
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 4  NO NULA
     C     COL04         BEGSR
     C     MRIBCF        CHAIN     REOJCTPR                           25
     C   25              MOVEL(P)  E4            ERRTXT
     C   25              EXSR      PUTERR
     C  N25              EVAL      LINEA=%TRIM(LINEA) + %SUBST(MRIBCF:1:2)
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 5
     C     COL05         BEGSR
     C     MRICCC        COMP      *ZERO                                  25
     C   25              MOVEL(P)  E5            ERRTXT
     C   25              EXSR      PUTERR
     C  N25              EVAL      LINEA=%TRIM(LINEA) + %CHAR(MRICCC)
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 6  NO NULA
     C     COL06         BEGSR
     C     MR$ICU        COMP      *ZERO                                25
     C   25              MOVEL(P)  E6            ERRTXT
     C   25              EXSR      PUTERR
     C  N25              EVAL      LINEA=%TRIM(LINEA) + %CHAR(MR$ICU)
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 7  NO NULA
     C     COL07         BEGSR
     C                   Z-ADD     MRFACR        PAFECH            8 0
     C                   MOVE      '0'           PAIERR            1
     C                   Z-ADD     *ZERO         PADIAS           15 0
     C                   CALL      'SBBACFEC'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PAIERR
     C     PAIERR        COMP      *ZERO                              2525
     C   25              MOVEL(P)  E7            ERRTXT
     C   25              EXSR      PUTERR
     C  N25              MOVE      IFEC          FECINV
     C  N25              MOVE      *BLANKS       FECRET           10
     C  N25AÑO           CAT       FECRET:0      FECRET
     C  N25'/'           CAT       FECRET:0      FECRET
     C  N25MES           CAT       FECRET:0      FECRET
     C  N25'/'           CAT       FECRET:0      FECRET
     C  N25DIA           CAT       FECRET:0      FECRET
     C  N25              EVAL      LINEA=%TRIM(LINEA) + FECRET
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 8  NO NULA
     C     COL08         BEGSR
     C     MRIOPT        IFNE      'S'
     C     MRIOPT        ANDNE     'N'
     C                   MOVEL(P)  E8            ERRTXT
     C                   EXSR      PUTERR
     C                   ELSE
     C                   EVAL      LINEA=%TRIM(LINEA) + MRIOPT
     C                   ENDIF
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 9  NO NULA
     C     COL09         BEGSR
     C                   IF        MRNTDO <> *ZERO
     C     KNTDO         KLIST
     C                   KFLD                    OJIINC
     C                   KFLD                    OJNTDO
     C     KNTDO         CHAIN     REOJNTDO                           25
     C   25              MOVEL(P)  E9            ERRTXT
     C   25              EXSR      PUTERR
     C  N25              MOVE      TDIITI        TIPDOC            2
     C  N25              EVAL      LINEA=%TRIM(LINEA) + TIPDOC
     C                   ENDIF
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 10 NO NULA
     C     COL10         BEGSR
     C     MRIINI        COMP      *ZERO                                  25
     C   25              MOVEL(P)  E10           ERRTXT
     C   25              EXSR      PUTERR
     C  N25              EVAL      LINEA=%TRIM(LINEA) + %CHAR(MRIINI)
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 11 NO NULA
     C     COL11         BEGSR
     C     CPIDOD        IFEQ      'S'
     C                   IF        MRIBCF <> *BLANKS  AND
     C                             %SUBST(MRIBCF:3:4) <>  *BLANKS   AND
     C                             %SUBST(MRIBCF:3:4) <>  *ZEROS
     C     MRIBCF        CHAIN     REOJCTPR                           25
     C   25              MOVEL(P)  E11           ERRTXT
     C   25              EXSR      PUTERR
     C  N25              MOVEL     MRIBCF        AUX001            3
     C  N25              MOVE      '-'           AUX001
     C  N25              MOVE      MRIBCF        AUX002            7
     C  N25              MOVEL     AUX001        AUX002
     C  N25              MOVE      AUX002        CODACT            7
     C  N25              EVAL      LINEA=%TRIM(LINEA) + CODACT
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 12 NO NULA
     C     COL12         BEGSR
     C     CPIDOD        IFEQ      'S'
     C     MR$INP        COMP      *ZERO                                25
     C   25              MOVEL(P)  E12           ERRTXT
     C   25              EXSR      PUTERR
     C  N25              EVAL      LINEA=%TRIM(LINEA) + %CHAR(MR$INP)
     C                   ENDIF
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 13 NO NULA
     C     COL13         BEGSR
     C     CPIDOD        IFEQ      'S'
     C     MRTSEL        COMP      *ZERO                                2525
     C   25              MOVEL(P)  E13           ERRTXT
     C   25              EXSR      PUTERR
     C  N25              EVAL      LINEA=%TRIM(LINEA) + %CHAR(MRTSEL)
     C                   ENDIF
     C                   ENDSR
     C*--------------->CHEQUEAR COLUMNA 14 NO NULA
     C     COL14         BEGSR
     C     CPIDOD        IFEQ      'S'
     C                   Z-ADD     MRFING        PAFECH            8 0
     C                   MOVE      '0'           PAIERR            1
     C                   Z-ADD     *ZERO         PADIAS           15 0
     C                   CALL      'SBBACFEC'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PAIERR
     C     PAIERR        COMP      *ZERO                              2525
     C   25              MOVEL(P)  E14           ERRTXT
     C   25              EXSR      PUTERR
     C  N25              MOVE      IFEC          FECINV
     C  N25              MOVE      *BLANKS       FECCOT           10
     C  N25AÑO           CAT       FECCOT:0      FECCOT
     C  N25'/'           CAT       FECCOT:0      FECCOT
     C  N25MES           CAT       FECCOT:0      FECCOT
     C  N25'/'           CAT       FECCOT:0      FECCOT
     C  N25DIA           CAT       FECCOT:0      FECCOT
     C  N25              EVAL      LINEA=%TRIM(LINEA) + FECCOT
     C                   ENDIF
     C                   ENDSR
     C*--------------->OBTNER OFICIO DEL MAESTRO DE OFICIOS
     C     GETOFI        BEGSR
     C     KMOFI         CHAIN     REOJMOFI                           25
     C   25              MOVEL(P)  E1            ERRTXT
     C   25              EXSR      PUTERR
     C*    OJIETI        COMP      'T'                                    25
     C*  25              MOVEL     E2            ERRTXT
     C*  25              EXSR      PUTERR
     C*                  MOVE      MRINUI        OFINRO           11
     C                   ENDSR
     C*--------------->ESCRIBE UN ERROR EN EL LISTADO DE ERRORES
     C     PUTERR        BEGSR
     C                   MOVE      *OFF          ISTS
     C                   MOVE      *OFF          CHKSTS
     C   99              WRITE     ERRTIT
     C                   WRITE     ERRDET
     C                   SETON                                        98
     C                   ENDSR
     C*--------------->FIN DEL PROGRAMA
     C     ENDPGM        BEGSR
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
     C*--------------->SUBRUTINA INICIAL
     C     *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    IFEC              8 0
     C                   PARM                    CODBAN            5
     C                   PARM                    ISTS              1
     C*
     C     KMRES         KLIST
     C                   KFLD                    IFEC
     C     KMOFI         KLIST
     C                   KFLD                    MRININ
     C                   KFLD                    MRINUI
     C                   KFLD                    MRNTDO
     C                   KFLD                    MRIINI
     C     KTACT         KLIST
     C                   KFLD                    MRIBCF
     C*
     C                   MOVE      *OFF          ISTS
     C                   MOVE      IFEC          FECINV
     C                   MOVE      *BLANKS       FECRES           10
     C     AÑO           CAT       FECRES:0      FECRES
     C     '/'           CAT       FECRES:0      FECRES
     C     MES           CAT       FECRES:0      FECRES
     C     '/'           CAT       FECRES:0      FECRES
     C     DIA           CAT       FECRES:0      FECRES
     C                   ENDSR
     OOJTRES    E            DETAL
     O                       LINEA
  1234*678901234
**
1S11111111111111
1N11100001000000
2S11111111111111
2N11100001000000
3S11100011110000
3N00000000000000
4S11111111110000
4N00000000000000
5S11111111111111
5N11100001000000
6S11100011000000
6N00000000000000
