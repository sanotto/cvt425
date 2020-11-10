*SRCMBRTXT:Aut.Federal-Elimina pedido de Aut. en P
     FPRAFED02  UF   E           K DISK
     FPRAFED05  UF   E           K DISK    RENAME(REPRAFED:REBAJA)
     FPRMFED    UF   E           K DISK
     FPRMOVI60  UF   E           K DISK
     FSGSYSV    IF   E             DISK
     C*=========================================================================
     c                   ExSr      DelPedAut
     c                   ExSr      DelMinuta
     c                   ExSr      DelMovs
     c                   ExSr      EndPgm
     c*-------------------------------------------------------------------------
     C* DelPedAut: Eliminar Pedido de Autorización
     c*-------------------------------------------------------------------------
     c     DelPedAut     BegSr
     c*
     c     KAF023        Chain     REPRAFED                           99
     c                   If        AFESTA = *BLANK
     c                   If        *In99 = *Off
     c                   Z-Add     AFFECH        WWFECH
     c                   Delete    REPRAFED
     c* ... Pedido de Baja
     c     KAF050        Chain     REBAJA                             99
     c                   If        *In99 = *Off
     c                   Delete    REBAJA
     C                   EndIf
     C*
     C                   EndIf
     C                   Else
     C* ... El pedido ya fue procesado por federal, no borrar
     c                   ExSr      EndPgm
     C                   EndIf
     c*
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C* DelMinuta: Eliminar Minuta
     c*-------------------------------------------------------------------------
     c     DelMinuta     BegSr
     c*
     c     KMC000        Chain     REPRMFED                           99
     c                   DoW       *In99 = *Off
     c                   Delete    REPRMFED
     c     KMC000        ReadE     REPRMFED                               99
     C                   EndDo
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C* DelMovs  : Eliminar PRMOVI60
     c*-------------------------------------------------------------------------
     c     DelMovs       BegSr
     c*
     c     KM2000        Chain     REPRMO60                           99
     c                   DoW       *In99 = *Off
     c                   Delete    REPRMO60
     c     KM2000        ReadE     REPRMO60                               99
     C                   EndDo
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     C* EndPgm: Fin
     c*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     c*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C* *INZSR: Inicialización
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     c     *ENTRY        Plist
     c                   Parm                    PAISUC
     c                   Parm                    PAINCR
     c                   Parm                    PAIDEG
     C*
     c     *Like         Define    AFISUC        PAISUC
     c     *Like         Define    AFINCR        PAINCR
     c     *Like         Define    AFIDEG        PAIDEG
     c     *Like         Define    AFFECH        WWFECH
     C*
     c     KAF023        KList
     c                   KFld                    PAISUC
     c                   KFld                    PAINCR
     c                   KFld                    PAIDEG
     c     KAF050        KList
     c                   KFld                    PAISUC
     c                   KFld                    PAINCR
     c                   KFld                    PAIDEG
     c     KMC000        KList
     c                   KFld                    WWFECH
     c                   KFld                    PAISUC
     c                   KFld                    PAINCR
     c                   KFld                    PAIDEG
     c     KM2000        KList
     c                   KFld                    WWFECH
     c                   KFld                    PAISUC
     c                   KFld                    PAINCR
     c                   KFld                    PAIDEG
     C*
     C     1             Chain     RESGSYSV
     C*
     C                   EndSr
