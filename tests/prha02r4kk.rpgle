*SRCMBRTXT:Ces.Hab.-Gen Plano            -Fto Fede
     H DEBUG DATEDIT(*YMD)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME  : SIDEBA - PRESTAMOS                            *
     H*                                                               *
     H*  PROGRAM NAME : PRHA00MA                                      *
     H*                                                               *
     H*  PROGRAM TITLE: Ces.Hab.-Gen PC .DBF                          *
     H*                                                               *
     H*  DATE         : 21/10/2003                                    *
     H*                                                               *
     H*  AUTHOR       : Ottonello, Santiago                           *
     H*                                                               *
     H*  DESCRIPTION  :                                               *
     H*****************************************************************
     FPRHAMV01  UF   E           K DISK
     FBASCOR04  IF   E           K DISK
     FBAPFIS05  IF   E           K DISK
     FPRHADO    UF A E           K DISK
     D*-------------------------------------------------------------------------
     D FECHAD          DS
     D  WKDESD                 1     10
     D  DIAD                   1      2  0
     D  FIL1D                  3      3
     D  MESD                   4      5  0
     D  FIL2D                  6      6
     D  AÑOD                   7     10  0
     D FECHAH          DS
     D  WKHAST                 1     10
     D  DIAH                   1      2  0
     D  FIL1H                  3      3
     D  MESH                   4      5  0
     D  FIL2H                  6      6
     D  AÑOH                   7     10  0
     D*-------------------------------------------------------------------------
     D ERR001          C                   CONST('MAINPG: Sin Area   ')
     D ERR002          C                   CONST('FNDSEX:Sexo Inv.en S-
     D                                     COR y en BAPFIS')
     D ERR003          C                   CONST('FNDSEX:Sexo Inv.en S-
     D                                     COR e inex en BAPFIS')
     D ERR004          C                   CONST('DATPF :DNI Inex en B-
     D                                     APFIS')
     C*-------------------------------------------------------------------------
     C                   Z-ADD     *ZERO         COUNT            15 0
     C                   Z-ADD     *ZERO         DUPLI            15 0
     C     KEY001        CHAIN     REPRHAMV                           99
+----C     *IN99         DOWEQ     *OFF
*LIKEC                   MOVE      *ON           DTAOK             1
*LIKEC                   MOVE(P)   *BLANKS       MVDF05
     C                   Z-ADD     MV$IMP        PA$IMP
     C     MV$INF        IFNE      *ZERO
     C     MV$INF        DIV       100           PA$IMP
     C                   ENDIF
     C                   EXSR      UPPRHA
*LIKEC                   EXSR      FILREC
     C*          PA$IMP    MULT 100       MV$INF
|+---C     DTAOK         IFEQ      *ON
||   C                   EXSR      WRTUPD
|+---C                   ENDIF
*LIKEC                   UPDATE    REPRHAMV
     C                   ADD       1             COUNT
*LIKEC     KEY001        READE     REPRHAMV                               99
+----C                   ENDDO
     C                   SETON                                        LR
     C*----------------------------------------------------------------
     C     UPPRHA        BEGSR
     C*
     C     MVIMDS        IFEQ      *ZEROS
     C                   MOVE      570           MVIMDS
     C     MVILCR        IFEQ      8
     C     MVILCR        OREQ      2008
     C     MVILCR        OREQ      2738
     C                   MOVE      572           MVIMDS
     C                   ENDIF
     C                   ENDIF
     C     MVIRED        IFEQ      ' '
     C                   MOVE      '1'           MVIRED
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     WRTUPD        BEGSR
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     FILREC        BEGSR
     C*
     C     MVIEMP        IFEQ      1
     C     MVILCR        IFEQ      8
     C     MVILCR        OREQ      2008
     C     MVILCR        OREQ      2738
     C                   CALL      'PRHA02RR'
     C                   PARM                    MVIEMP
     C                   PARM                    MVINDO
     C                   PARM                    MVISUC
     C                   PARM                    MVINCR
     C                   PARM                    MVIDEG
     C                   PARM                    PA$IMP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      PA$IMP        WWAUX1           15
     C                   MOVEL     WWAUX1        WWENTE           13
     C                   MOVE      WWAUX1        WWDECI            2
     C*
     C                   MOVE      WWENTE        WKENTE
     C                   MOVE      '.'           WKDOT
     C                   MOVE      WWDECI        WKDEC
     C                   MOVE      MVIMDS        WKCODI
     C                   MOVEL     MVIBCF        WKIBCF
+----C     MVIBCF        IFEQ      *BLANKS
*LIKEC                   MOVEL     ERR001        MVDF05
*LIKEC                   MOVE      *OFF          DTAOK
*LIKEC                   GOTO      ENDFIL
+----C                   ENDIF
     C*
     C     KEYSCO        CHAIN     REBASCOR                           99
+----C     *IN99         IFEQ      *OFF
*LIKEC                   EXSR      DATSCO
+----C                   ELSE
*LIKEC                   EXSR      DATPF
+----C                   ENDIF
     C*
     C                   MOVE      WWISEX        WKISEX
     C                   MOVE      WWIJOB        WKIJOB
     C                   MOVE      WWESCU        WKESCU
     C*
     C                   EXSR      CHKNDO
     C                   MOVE      MVINDO        WKINDO
     C                   MOVE      MVNYAP        WKNYAP
     C*
     C                   EXSR      FIXJOB
     C*
     C     ENDFIL        ENDSR
     C*----------------------------------------------------------------
     C     FIXJOB        BEGSR
     C                   SELECT
     C     WKIJOB        WHENEQ    '0A'
     C                   MOVE      '10'          WKIJOB
     C     WKIJOB        WHENEQ    '0B'
     C                   MOVE      '11'          WKIJOB
     C     WKIJOB        WHENEQ    '0C'
     C                   MOVE      '12'          WKIJOB
     C     WKIJOB        WHENEQ    '0D'
     C                   MOVE      '13'          WKIJOB
     C     WKIJOB        WHENEQ    '0E'
     C                   MOVE      '14'          WKIJOB
     C     WKIJOB        WHENEQ    '0F'
     C                   MOVE      '15'          WKIJOB
     C     WKIJOB        WHENEQ    '0G'
     C                   MOVE      '16'          WKIJOB
     C     WKIJOB        WHENEQ    '0H'
     C                   MOVE      '17'          WKIJOB
     C     WKIJOB        WHENEQ    '0I'
     C                   MOVE      '18'          WKIJOB
     C     WKIJOB        WHENEQ    '0J'
     C                   MOVE      '19'          WKIJOB
     C     WKIJOB        WHENEQ    '0K'
     C                   MOVE      '20'          WKIJOB
     C     WKIJOB        WHENEQ    '0L'
     C                   MOVE      '21'          WKIJOB
     C     WKIJOB        WHENEQ    '0M'
     C                   MOVE      '22'          WKIJOB
     C     WKIJOB        WHENEQ    '0N'
     C                   MOVE      '23'          WKIJOB
     C     WKIJOB        WHENEQ    '0O'
     C                   MOVE      '24'          WKIJOB
     C     WKIJOB        WHENEQ    '0P'
     C                   MOVE      '25'          WKIJOB
     C     WKIJOB        WHENEQ    '0Q'
     C                   MOVE      '26'          WKIJOB
     C     WKIJOB        WHENEQ    '0R'
     C                   MOVE      '27'          WKIJOB
     C     WKIJOB        WHENEQ    '0S'
     C                   MOVE      '28'          WKIJOB
     C     WKIJOB        WHENEQ    '0T'
     C                   MOVE      '29'          WKIJOB
     C     WKIJOB        WHENEQ    '0U'
     C                   MOVE      '30'          WKIJOB
     C     WKIJOB        WHENEQ    '0V'
     C                   MOVE      '31'          WKIJOB
     C     WKIJOB        WHENEQ    '0W'
     C                   MOVE      '32'          WKIJOB
     C     WKIJOB        WHENEQ    '0X'
     C                   MOVE      '33'          WKIJOB
     C     WKIJOB        WHENEQ    '0Y'
     C                   MOVE      '34'          WKIJOB
     C     WKIJOB        WHENEQ    '0Z'
     C                   MOVE      '35'          WKIJOB
     C                   ENDSL
     C     WKIJOB        IFEQ      '00'
     C                   MOVE      '01'          WKIJOB
     C                   ENDIF
     C                   ENDSR
     C*----------------------------------------------------------------
     C     DATSCO        BEGSR
     C                   EXSR      FNDSEX
     C                   MOVE      WWISEX        ISEX
     C                   MOVE      *ZEROS        WWIJOB
+----C     SCIRED        IFEQ      *BLANK
*LIKEC                   MOVE      *ZERO         SCIRED
+----C                   ENDIF
     C                   MOVE      SCIRED        WWIJOB            2
     C                   MOVEL     SCACCL        WWESCU            3
     C                   ENDSR
     C*----------------------------------------------------------------
     C     DATPF         BEGSR
     C     MVINDO        CHAIN     REBAPFIS                           99
+----C     *IN99         IFEQ      *OFF
*LIKEC                   MOVE      AÑISEX        WWISEX            1
*LIKEC                   MOVE      MVIRED        WWIJOB
*LIKEC                   MOVE      *BLANKS       WWESCU            3
+----C                   ELSE
*LIKEC                   MOVEL(P)  ERR004        MVDF05
*LIKEC                   MOVE      *OFF          DTAOK
+----C                   ENDIF
     C                   ENDSR
     C*----------------------------------------------------------------
     C     FNDSEX        BEGSR
     C*
     C                   MOVE      *BLANKS       WWISEX            1
     C* ... Busco sexo en Scoring
     C                   MOVE      SCISEX        WWISEX
     C* ... El sexo en scoring es valido ?
+----C     WWISEX        IFNE      'M'
*LIKEC     WWISEX        ANDNE     'F'
*LIKEC* ... ... No, Buscar en Bapfis
*LIKEC     MVINDO        CHAIN     REBAPFIS                           99
|+---C     *IN99         IFEQ      *OFF
||+--C     AÑISEX        IFEQ      'M'
|||  C     AÑISEX        OREQ      'F'
|||  C                   MOVE      AÑISEX        WWISEX
||+--C                   ELSE
|||  C                   MOVEL(P)  ERR002        MVDF05
|||  C                   MOVE      *OFF          DTAOK
||+--C                   ENDIF
||   C                   MOVEL(P)  ERR003        MVDF05
||   C                   MOVE      *OFF          DTAOK
|+---C                   ENDIF
+----C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    WWFAAS            6
     C                   PARM                    WWIEMP            5
     C     KEY001        KLIST
     C                   KFLD                    MVFAAM
     C                   KFLD                    MVIEMP
     C     KEYSCO        KLIST
     C                   KFLD                    MVISUC
     C                   KFLD                    MVINCR
     C                   KFLD                    MVIDEG
     C     KEYDO         KLIST
     C                   KFLD                    DDINDP
     C                   KFLD                    DDIBCF
     C                   KFLD                    DDISEX
     C                   KFLD                    DDIRED
     C                   KFLD                    DDINDO
     C     KEYPLA        KLIST
     C                   KFLD                    WKINDO
     C                   KFLD                    WKISEX
     C                   KFLD                    WKIJOB
     C                   KFLD                    WKIBCF
     C*
     C*
     C     *LIKE         DEFINE    INDO          WKINDO
     C     *LIKE         DEFINE    ISEX          WKISEX
     C     *LIKE         DEFINE    IJOB          WKIJOB
     C     *LIKE         DEFINE    NYAP          WKNYAP
     C     *LIKE         DEFINE    ENTE          WKENTE
     C     *LIKE         DEFINE    DOT           WKDOT
     C     *LIKE         DEFINE    DEC           WKDEC
     C     *LIKE         DEFINE    CODI          WKCODI
     C     *LIKE         DEFINE    DESDE         WKDESD
     C     *LIKE         DEFINE    HASTA         WKHAST
     C     *LIKE         DEFINE    IBCF          WKIBCF
     C     *LIKE         DEFINE    ESCU          WKESCU
     C     *LIKE         DEFINE    MV$IMP        PA$IMP
     C*
     C                   MOVE      WWFAAS        MVFAAM
     C                   MOVE      WWIEMP        MVIEMP
     C* ... Fecha Desde
     C                   MOVEL     WWFAAS        AÑOD
     C                   MOVE      WWFAAS        MESD
     C                   SUB       1             MESD
+----C     MESD          IFLT      1
*LIKEC                   SUB       1             AÑOD
*LIKEC                   Z-ADD     12            MESD
+----C                   ENDIF
     C                   MOVE      01            DIAD
     C                   MOVE      '/'           FIL1D
     C                   MOVE      '/'           FIL2D
     C                   MOVE      '/'           FIL1H
     C                   MOVE      '/'           FIL2H
     C* ... BORRA PRHADO
     C     *LOVAL        SETLL     REPRHADO
     C                   READ      REPRHADO                               99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REPRHADO
     C                   READ      REPRHADO                               99
     C                   ENDDO
     C* ... Fecha Hasta
     C                   MOVEL     AÑOD          AÑOH
     C                   MOVE      MESD          MESH
+----C                   SELECT
+----C     MESH          WHENEQ    1
*LIKEC     MESH          OREQ      3
*LIKEC     MESH          OREQ      5
*LIKEC     MESH          OREQ      7
*LIKEC     MESH          OREQ      8
*LIKEC     MESH          OREQ      10
*LIKEC     MESH          OREQ      12
*LIKEC                   MOVE      31            DIAH
+----C     MESH          WHENEQ    2
*LIKEC     AÑOH          DIV       4             AUXDIV           15 0
*LIKEC                   MVR                     REMAIN           15 0
|+---C     REMAIN        IFEQ      *ZERO
||   C                   MOVE      29            DIAH
|+---C                   ELSE
||   C                   MOVE      28            DIAH
|+---C                   ENDIF
+----C                   OTHER
*LIKEC                   MOVE      30            DIAH
+----C                   ENDSL
     C                   ENDSR
     C*----------------------------------------------------------------
     C     CHKNDO        BEGSR
     C     MVINDO        IFGT      900000000
     C                   MOVE      MVINDO        DDINDP
     C                   MOVE      MVIBCF        DDIBCF
     C                   MOVE      WWISEX        DDISEX
     C                   MOVE      WWIJOB        DDIRED
     C                   MOVE      MVINDO        DDINDO
     C     KEYDO         CHAIN     REPRHADO                           99
     C   99              WRITE     REPRHADO
     C                   ENDIF
     C                   ENDSR
