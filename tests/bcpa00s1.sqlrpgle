*SRCMBRTXT:Operaciones Pasivas CEDROS             
     H DECEDIT(',') DATEDIT(*DMY/)
      *----------------------------------------------------------------
     FCCCTCT    IF   E           K DISK
     FBACRCE    IF   E           K DISK
     FBAICCL    IF   E           K DISK
     FCCDBSE    IF   E           K DISK
     FCOTIZA    IF   E           K DISK
     FBCES02    IF   E           K DISK
     FBADCCL03  IF   E           K DISK
     FBAPFIS05  IF   E           K DISK
     FCCCOAS01  IF   E           K DISK
     FBCOPPA01  IF A E           K DISK
      *----------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
      *----------------------------------------------------------------
     D LDA           E DS                  EXTNAME(LDA)
      *----------------------------------------------------------------
     C/EXEC SQL
     C+ DELETE FROM BCOPPA WHERE P1IFEC = :PAIFEC AND P1IMON
     C+ = 6100
     C/END-EXEC
     C                   EXSR      CEDRO
     C                   EXSR      AMPARO
     C                   EVAL      *INLR=*ON
     C                   RETURN
     C*--------------------
     C     CEDRO         BEGSR
     C/EXEC SQL
     C+ DECLARE C1 CURSOR FOR SELECT E2ISUC, E2ICCC, E2IMON ,
     C+ CAST((SUM(E2$INE) + SUM(E2$ANU) - SUM(E2$$CP) + SUM(E2$$SC)
     C+             - SUM(E2$ACU))                             AS DEC(15,
     C+ 2)) WW$CAP , CAST( SUM(E3$INT) AS DEC(15, 2)) WW$IND, CAST(
     C+ ((SUM(E2$INE) + SUM(E2$ANU) - SUM(E2$$CP) + SUM(E2$$SC) -
     C+ SUM(E2$INT) - SUM(E2$ACU) + sum(e3$int)) * min(b7pret) ) -
     C+ ((SUM(E2$INE) + SUM(E2$ANU) - SUM(E2$$CP) + SUM(E2$$SC) -
     C+               SUM(E2$ACU) + sum(e3$int)) ) AS DEC(15, 2)) WW$MAJ
     C+ , MIN(E2FECH), MAX(E2FECH) FROM CCEDT2 left join ccedt3 on E2ISUC
     C+ = E3ISUC and E2ICCC = E3ICCC and E2FECH = E3FVEN left join bacrce
     C+ on b7falt=:WWFEHA WHERE E2ICCC NOT IN (201000788, 201000761,
     C+ 201000850, 201000699, 201000060, 201000156, 201000164, 201000982,
     C+ 201002071, 201000508, 201000001, 201000257, 201000753, 201000885,
     C+ 201000486, 201001873, 201002772, 201003345, 201003299, 201000176,
     C+ 201000156 ) AND E2FECH > :WWFEHA AND E2FMDV= 0 AND E2IMON= 2
     C+ GROUP BY E2ISUC, E2ICCC, E2IMON
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C1
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :WWISUC, :WWINCT, :WWIMON, :WW$CAP,
     C+                         :WW$IND, :WW$MAJ, :WWFALT, :WWFVEN
     C/END-EXEC
     C                   DOW       SQLCOD = *ZERO
     C     @KEY02        CHAIN     CCCTCT                             75
     C                   EXSR      SRLIMP
     C                   EXSR      SRTITU
     C                   EXSR      SRTASA
     C                   Z-ADD     3117520910    WWICCE
     C                   EXSR      SRGRAB
     C/EXEC SQL
     C+ FETCH NEXT FROM C1 INTO :WWISUC, :WWINCT, :WWIMON, :WW$CAP,
     C+                         :WW$IND, :WW$MAJ, :WWFALT, :WWFVEN
     C/END-EXEC
     C                   ENDDO
     C*
     C/EXEC SQL
     C+ CLOSE C1
     C/END-EXEC
     C                   ENDSR
     C*--------------------
     C     AMPARO        BEGSR
     C/EXEC SQL
     C+ DECLARE C2 CURSOR FOR SELECT E2ISUC, E2ICCC, E2IMON ,
     C+ CAST((SUM(E2$INE) + SUM(E2$ANU) - SUM(E2$$CP) + SUM(E2$$SC) -
     C+ SUM(E2$ACU)) AS DEC(15, 2)) WW$CAP , CAST( 0 AS DEC(15, 2) )
     C+ WW$IND, CAST( 0 AS DEC(15, 2)) WW$MAJ , MIN(E2FECH), MAX(E2FECH)
     C+ FROM CCEDT2 left join ccedt3 on E2ISUC = E3ISUC and E2ICCC =
     C+ E3ICCC and E2FECH = E3FVEN left join bacrce on b7falt=:WWFEHA
     C+ WHERE E2ICCC IN (201000788, 201000761, 201000850, 201000699,
     C+ 201000060, 201000156, 201000164, 201000982, 201002071, 201000508,
     C+ 201000001, 201000257, 201000753, 201000885, 201000486, 201001873,
     C+ 201002772, 201003345, 201003299, 201000176, 201000156 ) AND
     C+ E2FECH > :WWFEHA AND E2FMDV= 0 AND E2IMON= 2 GROUP BY E2ISUC,
     C+ E2ICCC, E2IMON
     C/END-EXEC
     C/EXEC SQL
     C+ OPEN C2
     C/END-EXEC
     C/EXEC SQL
     C+ FETCH NEXT FROM C2 INTO :WWISUC, :WWINCT, :WWIMON, :WW$CAP,
     C+                         :WW$IND, :WW$MAJ, :WWFALT, :WWFVEN
     C/END-EXEC
     C                   DOW       SQLCOD = *ZERO
     C     @KEY02        CHAIN     CCCTCT                             75
     C                   EXSR      SRLIMP
     C                   EXSR      SRTITU
     C                   EXSR      SRTASA
     C                   Z-ADD     3117540810    WWICCE
     C                   EXSR      COTUSD
     C                   EVAL      WW$CAP=(WW$CAP/1.40) * MU$COR
     C                   EXSR      SRGRAB
     C/EXEC SQL
     C+ FETCH NEXT FROM C2 INTO :WWISUC, :WWINCT, :WWIMON, :WW$CAP,
     C+                         :WW$IND, :WW$MAJ, :WWFALT, :WWFVEN
     C/END-EXEC
     C                   ENDDO
     C*
     C/EXEC SQL
     C+ CLOSE C2
     C/END-EXEC
     C                   ENDSR
     C     COTUSD        BEGSR
     C     KCOTI         KLIST
     C                   KFLD                    MUISUC
     C                   KFLD                    MUIMON
     C                   KFLD                    MUVIGE
     C                   Z-ADD     99999         MUISUC
     C                   Z-ADD     2             MUIMON
     C                   Z-ADD     WWFEHA        MUVIGE
     C     KCOTI         SETGT     RECOTIZA
     C                   READP     RECOTIZA                               25
     C   25              EVAL      MU$COR=1
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para inicio del programa
     C*----------------------------------------------------------------*
     C     *INZSR        BEGSR
     C     *LIKE         DEFINE    P1IFEC        PAIFEC
     C     *LIKE         DEFINE    P1ISUC        WWISUC
     C     *LIKE         DEFINE    P1ISUC        WWISU2
     C     *LIKE         DEFINE    P1IPA2        WWIPA2
     C     *LIKE         DEFINE    P1IMON        WWIMON
     C     *LIKE         DEFINE    P1INCT        WWINCT
     C     *LIKE         DEFINE    P1INCE        WWINCE
     C     *LIKE         DEFINE    P1QDPL        WWQDPL
     C     *LIKE         DEFINE    P1FALT        WWFALT
     C     *LIKE         DEFINE    P1FVEN        WWFVEN
     C     *LIKE         DEFINE    P1TTNA        WWTTNA
     C     *LIKE         DEFINE    P1PCOM        WWPCOM
     C     *LIKE         DEFINE    P1IT01        WWIT01
     C     *LIKE         DEFINE    P1CUI1        WWCUI1
     C     *LIKE         DEFINE    P1DNI1        WWDNI1
     C     *LIKE         DEFINE    P1IT02        WWIT02
     C     *LIKE         DEFINE    P1CUI2        WWCUI2
     C     *LIKE         DEFINE    P1DNI2        WWDNI2
     C     *LIKE         DEFINE    P1IT03        WWIT03
     C     *LIKE         DEFINE    P1CUI3        WWCUI3
     C     *LIKE         DEFINE    P1DNI3        WWDNI3
     C     *LIKE         DEFINE    P1IT04        WWIT04
     C     *LIKE         DEFINE    P1CUI4        WWCUI4
     C     *LIKE         DEFINE    P1DNI4        WWDNI4
     C     *LIKE         DEFINE    P1IT05        WWIT05
     C     *LIKE         DEFINE    P1CUI5        WWCUI5
     C     *LIKE         DEFINE    P1DNI5        WWDNI5
     C     *LIKE         DEFINE    P1$CAP        WW$CAP
     C     *LIKE         DEFINE    P1$IND        WW$IND
     C     *LIKE         DEFINE    P1$PRM        WW$PRM
     C     *LIKE         DEFINE    P1IECP        WWIECP
     C     *LIKE         DEFINE    P1ICCE        WWICCE
     C     *LIKE         DEFINE    P1DTXT        WWDTXT
     C     *LIKE         DEFINE    AÑINDO        WWINDO
     C     *LIKE         DEFINE    P1IT01        WWIT00
     C     *LIKE         DEFINE    HBIMCC        WWIMCC
     C     *LIKE         DEFINE    HBICMO        WWICMO
     C     *LIKE         DEFINE    HBINI1        WWINI1
     C     *LIKE         DEFINE    HBINI2        WWINI2
     C     *LIKE         DEFINE    HBINI3        WWINI3
     C     *LIKE         DEFINE    HBINI4        WWINI4
     C     *LIKE         DEFINE    HBINI5        WWINI5
     C     *LIKE         DEFINE    HBINI6        WWINI6
     C     *LIKE         DEFINE    HBINI7        WWINI7
     C     *LIKE         DEFINE    P1TIPP        WWTIPP
     C     *LIKE         DEFINE    P1$MAJ        WW$MAJ
      *
     C     *ENTRY        PLIST
     C                   PARM                    PAIFEC
     C                   PARM                    PAFEDE            8
     C                   PARM                    PAFEHA            8
     C                   MOVE      PAFEDE        WWFEDE            8 0
     C                   MOVE      PAFEHA        WWFEHA            8 0
      *
     C     *DTAARA       DEFINE    *LDA          LDA
     C                   IN        LDA
      * Construye KLIST para acceso a BCOPPA01
     C     WKEY01        KLIST
     C                   KFLD                    PAIFEC
      * Construye KLIST para acceso a BCOPPA01
     C     @KEY01        KLIST
     C                   KFLD                    PAIFEC
     C                   KFLD                    WWISUC
     C                   KFLD                    WWIPA2
     C                   KFLD                    WWIMON
     C                   KFLD                    WWINCT
     C                   KFLD                    WWINCE
      *
      * Construye KLIST para acceso a CCCTCT
     C     @KEY02        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWINCT
      *
      * Construye KLIST para acceso a BADCCL03
     C     @KEY03        KLIST
     C                   KFLD                    BMISUC
     C                   KFLD                    BMICCL
      * Construye KLIST para acceso a CCCOAS01
     C     @KEY04        KLIST
     C                   KFLD                    WWIMCC
      * Construye KLIST para acceso a CCDBSE
     C     @KEY05        KLIST
     C                   KFLD                    WWISU2
     C                   KFLD                    WWIMON
     C                   KFLD                    BMITIT
      * Construye KLIST para acceso a BCES02
     C     @KEY06        KLIST
     C                   KFLD                    PAIFEC
     C                   KFLD                    WWISUB
     C                   MOVE      'CC'          WWISUB            2
      * Construye KLIST para acceso a CCEDT3
     C     @KEY07        KLIST
     C                   KFLD                    WWISUC
     C                   KFLD                    WWINCT
      *
     C                   Z-ADD     99999         WWISU2
     C                   Z-ADD     *ZEROS        I                 2 0
      *
     C                   Z-ADD     *ZERO         COUNT             5 0
     C                   Z-ADD     5             WWIPA2
      *
     C                   CLEAR                   P1IMCA
     C                   CLEAR                   P1ICMO
     C                   CLEAR                   P1INI1
     C                   CLEAR                   P1INI2
     C                   CLEAR                   P1INI3
     C                   CLEAR                   P1INI4
     C                   CLEAR                   P1INI5
     C                   CLEAR                   P1INI6
     C                   CLEAR                   P1INI7
     C                   CLEAR                   P1ISCT
     C                   CLEAR                   P1ITCU
     C                   CLEAR                   P1IGRC
     C                   CLEAR                   P1ISGC
     C                   ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRLIMP - Limpia e inicializa campos
     C*----------------------------------------------------------------*
     C     SRLIMP        BEGSR
      *
     C*                  Z-ADD     *ZEROS        WWISUC
     C                   Z-ADD     *ZEROS        WWIPA2
     C*                  Z-ADD     *ZEROS        WWINCT
     C                   Z-ADD     *ZEROS        WWINCE
     C                   Z-ADD     *ZEROS        WWQDPL
     C*                  Z-ADD     *ZEROS        WWFALT
     C*                  Z-ADD     *ZEROS        WWFVEN
     C                   Z-ADD     *ZEROS        WWTTNA
     C                   Z-ADD     *ZEROS        WWPCOM
     C                   Z-ADD     *ZEROS        WWIT01
     C                   Z-ADD     *ZEROS        WWCUI1
     C                   MOVEL     *BLANK        WWDNI1
     C                   Z-ADD     *ZEROS        WWIT02
     C                   Z-ADD     *ZEROS        WWCUI2
     C                   MOVEL     *BLANK        WWDNI2
     C                   Z-ADD     *ZEROS        WWIT03
     C                   Z-ADD     *ZEROS        WWCUI3
     C                   MOVEL     *BLANK        WWDNI3
     C                   Z-ADD     *ZEROS        WWIT04
     C                   Z-ADD     *ZEROS        WWCUI4
     C                   MOVEL     *BLANK        WWDNI4
     C                   Z-ADD     *ZEROS        WWIT05
     C                   Z-ADD     *ZEROS        WWCUI5
     C                   MOVEL     *BLANK        WWDNI5
     C*                  Z-ADD     *ZEROS        WW$IND
     C                   Z-ADD     *ZEROS        WW$PRM
     C                   Z-ADD     *ZEROS        WWIECP
     C                   Z-ADD     *ZEROS        WWICCE
     C                   MOVEL     *BLANK        WWDTXT
     C                   Z-ADD     *ZEROS        I
     C                   Z-ADD     *ZEROS        WWIT00
     C                   Z-ADD     *ZEROS        WWICMO
     C                   MOVEL     *BLANK        WWINI1
     C                   MOVEL     *BLANK        WWINI2
     C                   MOVEL     *BLANK        WWINI3
     C                   MOVEL     *BLANK        WWINI4
     C                   MOVEL     *BLANK        WWINI5
     C                   MOVEL     *BLANK        WWINI6
     C                   MOVEL     *BLANK        WWINI7
     C                   Z-ADD     201           WWIMCC
      *
     C                   ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRTITU - Busca Titulares
     C*----------------------------------------------------------------*
     C     SRTITU        BEGSR
      *
     C                   MOVE      'F'           WWTIPP
     C     @KEY03        CHAIN     REBAICCL                           74
     C     *IN74         IFEQ      *OFF
     C     OSITIN        ANDNE     *ZERO
     C     OSININ        ANDNE     *ZERO
     C                   MOVE      'J'           WWTIPP
     C                   ENDIF
     C     @KEY03        CHAIN     BADCCL03                           74
     C     *IN74         DOWEQ     *OFF
     C                   ADD       1             I
     C                   SELECT
     C     OTITDO        WHENEQ    96
     C                   Z-ADD     1             WWIT00
     C     OTITDO        WHENEQ    89
     C                   Z-ADD     2             WWIT00
     C     OTITDO        WHENEQ    90
     C                   Z-ADD     3             WWIT00
     C     OTITDO        WHENEQ    80
     C                   Z-ADD     4             WWIT00
     C                   OTHER
     C                   Z-ADD     7             WWIT00
     C                   ENDSL
     C     OTINDO        CHAIN     BAPFIS05                           75
     C                   SELECT
     C     I             WHENEQ    1
     C                   Z-ADD     WWIT00        WWIT01
     C                   Z-ADD     OTINDO        WWCUI1
     C                   MOVEL     AÑNYAP        WWDNI1
     C     I             WHENEQ    2
     C                   Z-ADD     WWIT00        WWIT02
     C                   Z-ADD     OTINDO        WWCUI2
     C                   MOVEL     AÑNYAP        WWDNI2
     C     I             WHENEQ    3
     C                   Z-ADD     WWIT00        WWIT03
     C                   Z-ADD     OTINDO        WWCUI3
     C                   MOVEL     AÑNYAP        WWDNI3
     C     I             WHENEQ    4
     C                   Z-ADD     WWIT00        WWIT04
     C                   Z-ADD     OTINDO        WWCUI4
     C                   MOVEL     AÑNYAP        WWDNI4
     C     I             WHENEQ    5
     C                   Z-ADD     WWIT00        WWIT05
     C                   Z-ADD     OTINDO        WWCUI5
     C                   MOVEL     AÑNYAP        WWDNI5
     C                   ENDSL
     C     @KEY03        READE     BADCCL03                               74
     C                   ENDDO
      *
     C                   ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRTASA - Busca Tasa
     C*----------------------------------------------------------------*
      *
     C     SRTASA        BEGSR
      *
     C     @KEY05        CHAIN     CCDBSE                             76
     C                   Z-ADD     BRPACU        WWTTNA
      *
     C     @KEY06        CHAIN     BCES02                             57
     C     *IN57         IFEQ      *OFF
     C     BMIMON        IFEQ      1
     C                   Z-ADD     ESPT01        WWPCOM
     C                   ELSE
     C                   Z-ADD     ESPT02        WWPCOM
     C                   ENDIF
     C                   ENDIF
     C     BMIMON        IFEQ      1
     C                   Z-ADD     7             WWTTNA
     C                   Z-ADD     7             WWPCOM
     C                   ELSE
     C                   Z-ADD     2             WWTTNA
     C                   Z-ADD     2             WWPCOM
     C                   ENDIF
     C                   ENDSR
     C*------------------->
     C     FETCOA        BEGSR
     C     @KEY04        CHAIN     CCCOAS01                           78
     C     *IN78         DOWEQ     *OFF
     C     HBICCE        IFGT      *ZEROS
     C     HBIGRC        ANDEQ     '20'
     C                   Z-ADD     HBICCE        WWICCE
     C                   LEAVE
     C                   ENDIF
     C     @KEY04        READE     CCCOAS01                               78
     C                   ENDDO
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRGRAB - Graba datos en BCOPPA01
     C*----------------------------------------------------------------*
      *
     C     SRGRAB        BEGSR
      *
     C     @KEY01        CHAIN     BCOPPA01                           77
     C     *IN77         IFEQ      *ON
     C                   Z-ADD     PAIFEC        P1IFEC
     C                   Z-ADD     WWISUC        P1ISUC
     C                   Z-ADD     6100          P1IPA2
     C                   Z-ADD     WWIMON        P1IMON
     C*                  Z-ADD     6100          P1IMON
     C                   Z-ADD     WWINCT        P1INCT
     C                   Z-ADD     WWINCE        P1INCE
     C                   Z-ADD     WWQDPL        P1QDPL
     C                   Z-ADD     WWFALT        P1FALT
     C                   Z-ADD     WWFVEN        P1FVEN
     C                   Z-ADD     WWTTNA        P1TTNA
     C                   Z-ADD     WWPCOM        P1PCOM
     C                   Z-ADD     WWIT01        P1IT01
     C                   Z-ADD     WWCUI1        P1CUI1
     C                   MOVEL     WWDNI1        P1DNI1
     C                   Z-ADD     WWIT02        P1IT02
     C                   Z-ADD     WWCUI2        P1CUI2
     C                   MOVEL     WWDNI2        P1DNI2
     C                   Z-ADD     WWIT03        P1IT03
     C                   Z-ADD     WWCUI3        P1CUI3
     C                   MOVEL     WWDNI3        P1DNI3
     C                   Z-ADD     WWIT04        P1IT04
     C                   Z-ADD     WWCUI4        P1CUI4
     C                   MOVEL     WWDNI4        P1DNI4
     C                   Z-ADD     WWIT05        P1IT05
     C                   Z-ADD     WWCUI5        P1CUI5
     C                   MOVEL     WWDNI5        P1DNI5
     C                   EVAL      P1$CAP=WW$CAP
     C                   Z-ADD     WW$IND        P1$IND
     C                   Z-ADD     WW$PRM        P1$PRM
     C                   Z-ADD     5             P1IECP
     C                   Z-ADD     WWICCE        P1ICCE
     C                   Z-ADD     WW$MAJ        P1$MAJ
     C                   MOVEL     WWDTXT        P1DTXT
     C                   MOVE      WWTIPP        P1TIPP
     C                   Z-ADD     WW$CAP        P1$IMP
     C                   ADD       WW$IND        P1$IMP
     C                   ADD       WW$MAJ        P1$IMP
     C                   WRITE     REBCOPPA
     C                   ENDIF
      *
     C                   ENDSR
      *
     C*----------------------------------------------------------------
