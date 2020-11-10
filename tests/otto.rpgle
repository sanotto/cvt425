*SRCMBRTXT:Banca Electrónica-Man. P/Proc. Extracts
     H        1
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SISTEMA SCORING                                 *
     H*                                                               *
     H*  PROGRAM NAME: Graba nuevo periodo en SCORING                 *
     H*                                                               *
     H*  PROGRAM NO: BASC04R2                                         *
     H*                                                               *
     H*  DATE:    17/01/2001                                          *
     H*                                                               *
     H*  AUTHOR:  SERGIO CORTES                                       *
     H*                                                               *
     F*----------------------------------------------------------------*
     FBASCPL  IP  F     500            DISK
     FBASCAR01IF  E           K        DISK
     FBAIEMP  IF  E           K        DISK
     FBASCOR05UF  E           K        DISK                      A
     F*----------------------------------------------------------------*
     IBASCPL  AA  01   1NC
     I                                        1 200 DSTOD1
     I                                      201 400 DSTOD2
     I                                      401 500 DSTOD3
     I                                        1   60DSNEMP
     I                                        7  11 DSAREA
     I                                       12  19 DSDOCU
     I                                       20  20 DSTRAB
     I                                       21  45 DSNOMB
     I                                       46  55 DSFING
     I                                       56  57 DSANTI
     I                                       58  59 DSCUI1
     I                                       60  60 DSCUI2
     I                                       61  61 DSSEXO
     I                                       62  64 DSTDOC
     I                                       65  84 DSLPAG
     I                                       85  87 DSESCU
     I                                       88  88 DSZONA
     I                                       89  90 DSCARG
     I                                       91  93 DSCATE
     I                                       94  98 DSNREC
     I                                       99 104 DSFLIQ
     I                                      105 1132DS$THA
     I                                      114 1222DS$HST
     I                                      123 1292DS$TIK
     I                                      130 1382DS$FAM
     I                                      139 1472DS$NDL
     I                                      148 1562DS$SLG
     I                                      157 174 DSRECI
     I                                      175 200 DSFILL
     I        NZ  98   1 C                                                AUTC06
     I*-----------CUIT-------------------------------------------------
     I            DS
     I                                        1  11 CSCUIT
     I                                        1   2 CSICU1
     I                                        3  10 CSICU2
     I                                       11  11 CSICU3
     I*-----------RECIBO-----------------------------------------------
     I            DS
     I                                        1  18 RSRECI
     I                                        1   8 RSINDO
     I                                        9   9 RSTRAB
     I                                       10  14 RSAREA
     I                                       15  15 RSSEXO
     I                                       16  18 RSFILL
      *---------------------------------------------------------------
      *
     C   98                GOTO END
      *
     C           *IN99     IFEQ *OFF                       ---------.
     C                     EXSR INICIO                              |
     C                     ENDIF                           ---------'
      *
     C                     EXSR LIMPIA
     C   01                EXSR SRPROC
      *
     C           END       TAG
      *
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para inicio del programa
     C*----------------------------------------------------------------*
     C*
     C           INICIO    BEGSR
     C                     MOVE *ON       *IN99
     C*
     C           *ENTRY    PLIST
     C                     PARM           PAFAAM                       o
     C*
     C           *LIKE     DEFN SCFAAM    PAFAAM
     C           *LIKE     DEFN SCINDO    PAINDO
     C           *LIKE     DEFN SCIEMP    PAIEMP
     C           *LIKE     DEFN SCREFC    PAREFC
     C           *LIKE     DEFN SCITIN    DSITIN
     C           *LIKE     DEFN SCININ    DSININ
     C           *LIKE     DEFN SCISCO    DSISCO
     C           *LIKE     DEFN SCINCO    DSINCO
     C           *LIKE     DEFN ARIBCF    PAAREA
     C*
     C* Acceso a BASCOR03
     C           WKEY01    KLIST
     C                     KFLD           PAFAAM
     C                     KFLD           PAINDO
     C                     KFLD           PAIEMP
     C                     KFLD           PAREFC
      *
     C                     ENDSR
      *
     C*----------------------------------------------------------------*
     C* LIMPIA - Limpia e inicializa campos
     C*----------------------------------------------------------------*
      *
     C           LIMPIA    BEGSR
      *
     C                     Z-ADD*ZEROS    DSITIN
     C                     Z-ADD*ZEROS    DSININ
     C                     Z-ADD*ZEROS    DSISCO
     C                     Z-ADD*ZEROS    DSINCO
      *
     C                     CLEARREBASCOR
      *
     C                     ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRPROC - Llama a rutina de proceso
     C*----------------------------------------------------------------*
      *
     C           SRPROC    BEGSR
      *
      *  Arma Recibo
     C                     MOVE DSDOCU    RSINDO
     C                     MOVE DSTRAB    RSTRAB
     C                     MOVE DSAREA    RSAREA
     C                     MOVE DSSEXO    RSSEXO
     C                     MOVE *BLANK    RSFILL
      *  Arma CUIT/CUIL
     C                     MOVE DSCUI1    CSICU1
     C                     MOVE DSDOCU    CSICU2
     C                     MOVE DSCUI2    CSICU3
      *
      *  Arma Clave
     C                     EXSR SRWKEY
      *
     C                     MOVELDSAREA    PAAREA
     C                     MOVEL*BLANKS   DSDF04  1
     C           PAAREA    CHAINBASCAR01             84
     C           *IN84     IFEQ *OFF                       ---------.
     C                     Z-ADDARITIN    DSITIN                    |
     C                     Z-ADDARININ    DSININ                    |
     C                     Z-ADDARISCO    DSISCO                    |
     C                     Z-ADDARINCO    DSINCO                    |
     C                     ELSE                                     <
     C                     MOVEL'F'       DSDF04                    |
     C                     ENDIF                           ---------'
      *
      *  Actualizo archivo
     C           WKEY01    CHAINBASCOR05             85
     C           *IN85     IFEQ *ON                        ---------.
     C           DSDF04    IFEQ *BLANKS                    --------.|
     C           ARIEPL    IFEQ 'A'                        -------.||
     C                     MOVEL'A'       DSDF04                  |||
     C                     ELSE                                   <||
     C                     MOVEL'B'       DSDF04                  |||
     C                     ENDIF                           -------'||
     C                     ENDIF                           --------'|
     C                     EXSR SRGRAB                              |
     C                     ELSE                                     <
     C           DSDF04    IFEQ *BLANKS                    --------.|
     C           ARIEPL    ANDEQ'A'                        --------||
     C                     MOVEL'C'       DSDF04                   ||
     C                     ENDIF                           --------'|
     C                     EXSR SRACTU                              |
     C                     ENDIF                           ---------'
      *
     C                     ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRWKEY - Arma clave para actualizar BASCOR05
     C*----------------------------------------------------------------*
     C           SRWKEY    BEGSR
      *
     C                     MOVE DSDOCU    PAINDO
     C                     MOVE DSNEMP    PAIEMP
      *
     C           DSNEMP    IFEQ 1                          ---------.
     C                     MOVE RSRECI    PAREFC                    |
     C                     ELSE                                     <
     C                     MOVE DSRECI    PAREFC                    |
     C                     ENDIF                           ---------'
      *
     C           WKEY02    KLIST
     C                     KFLD           PAIEMP
     C           WKEY02    CHAINBAIEMP               80
      *
     C                     ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRGRAB - Graba en BASCOR05
     C*----------------------------------------------------------------*
      *
     C           SRGRAB    BEGSR
      *
     C                     MOVE DSNEMP    SCIEMP           EMPRESA
     C                     MOVELDSAREA    SCIBCF           AREA
     C                     MOVE DSITIN    SCITIN           TIP.REP.
     C                     MOVE DSININ    SCININ           N° REP.
     C                     MOVE DSTRAB    SCIRED           TRABAJO
     C                     MOVELDSNOMB    SCNYAP           NOMBRE
     C                     MOVE DSFING    SCFING           F.INGRESO
     C                     MOVE DSANTI    SCFAFO           ANTIGUEDAD
     C                     MOVE CSCUIT    SCICUI           CUIT
     C                     MOVE DSTDOC    SCITDO           TIPO DOC.
     C                     MOVE DSDOCU    SCINDO           N°DOC.
     C                     MOVELDSSEXO    SCISEX           SEXO
     C                     MOVELDSLPAG    SCNDEP           LUGAR PAGO
     C                     MOVELDSESCU    SCACCL           ESCUELA
     C                     MOVE DSZONA    SCNZON           ZONA
     C                     MOVE DSCARG    SCIPLA           CARGO
     C                     MOVE *BLANK    SCDCAR           DESC.CARGO
     C                     MOVE DSCATE    SCICAT           CATEGORIA
     C                     MOVELPAREFC    SCREFC           RECIBO BCO
     C                     MOVE DSNREC    SCNREC           RECIBO EMP.
     C                     MOVE PAFAAM    SCFAAM           PERIODO
     C                     MOVE *ZEROS    SC$INP
     C                     MOVE *ZEROS    SCISAL
     C                     MOVE *ZEROS    SCICCL
     C                     MOVE *ZEROS    SCISUC
     C                     MOVE *ZEROS    SCINCR
     C                     MOVE *ZEROS    SCIDEG
     C                     MOVE *ZEROS    SCICUO
     C                     MOVE DSISCO    SCISCO
     C                     MOVE DSINCO    SCINCO
     C                     MOVE *BLANK    SCIESA
     C                     MOVE *ZEROS    SCICOB
     C                     MOVE *ZEROS    SC$CUO
     C                     MOVE *ZEROS    SC$A03
     C                     MOVE *BLANK    SCDF01
     C                     MOVE *ZEROS    SC$A04
     C                     MOVE *BLANK    SCDF02
     C                     MOVE *ZEROS    SC$A05
     C                     MOVE *BLANK    SCDF03
     C*                    MOVEL'ALTA'    SCDF04
     C                     MOVELDSDF04    SCDF04
     C                     MOVE *ZEROS    SC$A06
     C                     MOVE *BLANK    SCDF05
     C*
     C                     MOVE DS$HST    SC$BRU           BRUTO
     C*
     C                     MOVE DS$THA    SC$CRE           C/APORTES
     C                     MOVE DS$HST    SC$SUA           S/APORTES
     C                     MOVE DS$TIK    SC$DEA           TIKET
     C                     MOVE DS$NDL    SC$SUE           NO DE LEY
     C                     MOVE DS$FAM    SC$ANT           SL.FAMIL.
     C                     MOVE DS$SLG    SC$DAA           LIQUIDO GOB
     C                     MOVE DSANTI    SCFAFO           ANTIGUEDAD
      *
     C           SC$BRU    MULT 0.825     SC$INP           LIQUIDO BCO
     C                     ADD  SC$DEA    SC$INP
     C           IEICON    IFEQ 'S'                        ---------.
     C                     SUB  SC$SUE    SC$INP           NO DE LEY|
     C                     ENDIF                           ---------'
      *
     C                     MOVELDSTOD1    SCBLK0           TODO
      *
     C                     WRITEREBASCOR
      *
     C                     ENDSR
      *
     C*----------------------------------------------------------------*
     C* SRACTU - Actualiza en BASCOR
     C*----------------------------------------------------------------*
      *
     C           SRACTU    BEGSR
      *
     C*
     C                     MOVE DS$HST    SC$BRU           BRUTO
     C*
     C                     MOVE DS$THA    SC$CRE           C/APORTES
     C                     MOVE DS$HST    SC$SUA           S/APORTES
     C                     MOVE DS$TIK    SC$DEA           TIKET
     C                     MOVE DS$NDL    SC$SUE           NO DE LEY
     C                     MOVE DS$FAM    SC$ANT           SL.FAMIL.
     C                     MOVE DS$SLG    SC$DAA           LIQUIDO GOB
     C                     MOVE DSANTI    SCFAFO           ANTIGUEDAD
     C                     MOVE *ZEROS    SC$INP           LIQUIDO BCO
      *
     C           SC$BRU    MULT 0.825     SC$INP           LIQUIDO BCO
     C                     ADD  SC$DEA    SC$INP
     C           IEICON    IFEQ 'S'                        ---------.
     C                     SUB  SC$SUE    SC$INP           NO DE LEY|
     C                     ENDIF                           ---------'
      *
     C                     MOVELDSESCU    SCACCL           ESCUELA
     C                     Z-ADD*ZEROS    SC$A03
     C                     MOVEL*BLANK    SCDF01
     C                     Z-ADD*ZEROS    SC$A04
     C                     MOVEL*BLANK    SCDF02
     C                     Z-ADD*ZEROS    SC$A05
     C                     MOVEL*BLANK    SCDF03
     C                     MOVEL*BLANK    SCDF04
     C                     MOVELDSDF04    SCDF04
     C                     Z-ADD*ZEROS    SC$A06
     C                     MOVEL*BLANK    SCDF05
     C                     MOVELDSTOD1    SCBLK0           TODO
     C                     UPDATREBASCOR
      *
     C                     ENDSR
      *
     C*=====================================================================
