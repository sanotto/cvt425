*SRCMBRTXT:BAHILI: Selecciona Hoja Resumen CA     
     F@CPIUSD   UF   E           K DISK
     D               ESDS                  EXTNAME(@PSDS)
     C     @PJOBN        CHAIN     @CPIUSD                            80
     c  n80              Z-ADD     @CICAH        @CICCC
     c  n80              Update    @CPIUSRR
     C                   CALL      'BAHI00R2'
     C                   PARM      'CA'          SUBSYS            2
     C                   SETON                                        LR
     C                   RETURN
