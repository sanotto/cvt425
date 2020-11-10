*SRCMBRTXT:RUTINA: NORMALIZA TEXTO ELIM ACENTOS, S
     C*
     C     *ENTRY        PLIST
     C                   PARM                    STRTXT          255
     C
     C                   MOVE      *BLANKS       CV2DIN            1
     C     1             DO        255           X                 3 0
     C                   EVAL      CV2DIN=%SUBST(STRTXT:X:1)
     C                   EXSR      Chr2Decimal
     C                   IF        (CV2DOU >=  0 AND CV2DOU <= 64) OR
     C                             (CV2DOU >= 65 AND CV2DOU <= 73) OR
     C                             (CV2DOU >= 81 AND CV2DOU <= 89) OR
     C                             (CV2DOU >= 98 AND CV2DOU <=105) OR
     C                             (CV2DOU >=112 AND CV2DOU <=120) OR
     C                             (CV2DOU >=128 AND CV2DOU <=128) OR
     C                             (CV2DOU >=138 AND CV2DOU <=144) OR
     C                             (CV2DOU >=154 AND CV2DOU <=160) OR
     C                             (CV2DOU >=170 AND CV2DOU <=191) OR
     C                             (CV2DOU >=202 AND CV2DOU <=207) OR
     C                             (CV2DOU >=218 AND CV2DOU <=223) OR
     C                             (CV2DOU >=225 AND CV2DOU <=225) OR
     C                             (CV2DOU >=234 AND CV2DOU <=239) OR
     C                             (CV2DOU >=250 AND CV2DOU <=255)
     C*
     C                   EVAL      CV2DIN = ' '
     C*
     C                   ENDIF
     C                   EVAL      %SUBST(STRTXT:X:1)=CV2DIN
     C                   ENDDO
     C*
     C                   SetOn                                        LR
     c                   Return
     c*----------------------------------------------------------------
     C     Chr2Decimal   BegSr
     c*
     C                   Z-ADD     *ZERO         CV2DOU            3 0
     C                   TESTB     '0'           CV2DIN                   30
     C                   TESTB     '1'           CV2DIN                   31
     C                   TESTB     '2'           CV2DIN                   32
     C                   TESTB     '3'           CV2DIN                   33
     C                   TESTB     '4'           CV2DIN                   34
     C                   TESTB     '5'           CV2DIN                   35
     C                   TESTB     '6'           CV2DIN                   36
     C                   TESTB     '7'           CV2DIN                   37
     C   30              ADD       128           CV2DOU
     C   31              ADD       64            CV2DOU
     C   32              ADD       32            CV2DOU
     C   33              ADD       16            CV2DOU
     C   34              ADD       8             CV2DOU
     C   35              ADD       4             CV2DOU
     C   36              ADD       2             CV2DOU
     C   37              ADD       1             CV2DOU
     C                   ENDSR
