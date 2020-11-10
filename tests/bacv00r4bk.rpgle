*SRCMBRTXT:Validador de Ctas. Debitos Automáticos 
     H DEBUG
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME:                                                 *
     H*                                                               *
     H*  PROGRAM NAME:                                                *
     H*                                                               *
     H*  PROGRAM NO: BACV00R4                                         *
     H*                                                               *
     H*  DATE:    11/08/2011                                          *
     H*                                                               *
     H*  AUTHOR:  PR00586                                             *
     H*                                                               *
     F*----------------------------------------------------------------*
     FTCPAD5    IP   F  900        DISK
     F* ...Archivo Plano
     FBACVIN01  IF A E           K DISK
     FSGSYSV    IF   E           K DISK
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     ITCPAD5    AA
     I                                  1  900  DSTODO
     I                                  9   11  DSTCIR
     I                                 12   17  DSNCIR
     I                                 18   20 0DSTDOC
     I                                 21   34 0DSNDOC
     I                                 35  184  DSAPEL
     I                                185  234  DSNOMB
     I                                236  315  DSOBSE
     I*----------------------------------------------------------------*
     I          AB
     I                                  1   11 0DSCUIL
     I                                  1    2 0DSDIG1
     I                                  3   10 0DSNDO1
     I                                 11   11 0DSDIG2
     C*----------------------------------------------------------------
     C                   EXSR      INICIO
     C                   EXSR      VALTDO
     C                   EXSR      VALCIR
     C                   EXSR      VALNOM
     C                   EXSR      SRPROC
     CLR                 EXSR      SRFINA
     C*----------------------------------------------------------------*
     C* INICIO - Busca datos para Inicio del programa
     C*----------------------------------------------------------------*
     C     INICIO        BEGSR
     C*
     C                   Z-ADD     *ZEROS        WWTDOC            2 0
     C                   MOVE      *BLANKS       WWTCIR            3
     C                   MOVE      *BLANKS       WWNDNN           55
     C*
     C                   READ      RESGSYSV                               90
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRPROC - Llama a rutina de proceso
     C*----------------------------------------------------------------*
     C     SRPROC        BEGSR
     C*
     C* ...Klist para acceder a BACVIN
+----C     DSTCIR        IFNE      *ZEROS
     C     DSNDOC        ANDNE     *ZEROS
|    C                   Z-ADD     DSNDOC        WWNDOC           15 0
|    C     @KEY01        KLIST
|    C                   KFLD                    WWTDOC
|    C                   KFLD                    WWNDOC
|    C*
|    C     @KEY01        CHAIN     BACVIN01                           99
|+---C     *IN99         IFEQ      *ON
||   C                   Z-ADD     WWTDOC        K3ITIN
||   C                   Z-ADD     DSNDOC        K3ININ
||   C                   MOVE      WWNDNN        K3NDNN
||   C                   Z-ADD     AASFEI        K3FING
||   C                   Z-ADD     AASFEI        K3FALT
||   C                   MOVEL     DSOBSE        K3OBSO
||   C                   MOVEL     'SUB.AUT.'    K3IUSR
||   C                   MOVEL     DSTDOC        K3IF02
||   C                   MOVEL     WWTCIR        K3IF01
||   C                   MOVE      DSNCIR        K3IF01
||   C                   WRITE     REBACVIN
|+---C                   ENDIF
+----C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* VALTDO - VALIDA TIPO DE DOCUMENTO
     C*----------------------------------------------------------------*
     C     VALTDO        BEGSR
     C*
+----C     DSTDOC        IFEQ      001
|    C                   Z-ADD     96            WWTDOC
+----C                   ENDIF
+----C     DSTDOC        IFEQ      003
|    C                   Z-ADD     89            WWTDOC
+----C                   ENDIF
+----C     DSTDOC        IFEQ      004
|    C                   Z-ADD     90            WWTDOC
+----C                   ENDIF
+----C     DSTDOC        IFEQ      007
|    C                   Z-ADD     94            WWTDOC
+----C                   ENDIF
     C*
     C* Para determinar si se trata de una persona juridica
     C*
+----C     DSNDOC        IFGT      30000000000
|    C     DSNDOC        ANDLT     40000000000
|    C                   Z-ADD     80            WWTDOC
+----C                   ENDIF
     C*
     C*Obtiene número de documento desde CUIL
     C*
+----C     DSNDOC        IFLT      30000000000
|    C     DSNDOC        ANDGT     20000000000
|    C                   Z-ADD     96            WWTDOC
     C                   Z-ADD     DSNDOC        DSCUIL
     C                   Z-ADD     DSNDO1        DSNDOC
+----C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* VALCIR - VALIDA TIPO DE CIRCULAR
     C*----------------------------------------------------------------*
     C     VALCIR        BEGSR
     C*
+----C     DSTCIR        IFEQ      '001'
|    C                   MOVE      'A'           WWTCIR
+----C                   ENDIF
+----C     DSTCIR        IFEQ      '002'
|    C                   MOVE      'B'           WWTCIR
+----C                   ENDIF
+----C     DSTCIR        IFEQ      '003'
|    C                   MOVE      'C'           WWTCIR
+----C                   ENDIF
+----C     DSTCIR        IFEQ      '004'
|    C                   MOVE      'COM.PREN'    WWTCIR
+----C                   ENDIF
+----C     DSTCIR        IFEQ      '005'
|    C                   MOVE      'STAF'        WWTCIR
+----C                   ENDIF
+----C     DSTCIR        IFEQ      '006'
|    C                   MOVE      'D'           WWTCIR
+----C                   ENDIF
+----C     DSTCIR        IFEQ      '007'
|    C                   MOVE      'V'           WWTCIR
+----C                   ENDIF
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* VALNOM - QUITA ESPACIOS AL FINAL DEL NOMBE Y APELLIDO
     C*----------------------------------------------------------------*
     C     VALNOM        BEGSR
     C*
      /FREE
        WWNDNN= %TRIM(DSAPEL)+' '+%TRIM(DSNOMB) ;
      /END-FREE
     C*
     C                   ENDSR
     C*----------------------------------------------------------------*
     C* SRFINA - Final
     C*----------------------------------------------------------------*
     C     SRFINA        BEGSR
     C*
     C*
     C                   ENDSR
     C*=====================================================================
