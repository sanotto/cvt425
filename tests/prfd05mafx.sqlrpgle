*SRCMBRTXT:Aut.Federal-Inserta pedidos de Baja    
     H DEBUG
     H DECEDIT(',') DATEDIT(*DMY/)
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H BNDDIR('LE00525/TO10BD   ')
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: - Inserta Pedido de Baja en PRAFED             *
     H*  PROGRAM NO: PRFD05MA                                         *
     H*                                                               *
     H*  DATE:    02/08/2012                                          *
     H*                                                               *
     H*  AUTHOR:                                                      *
     H*                                                               *
     H*****************************************************************
     FPRAFED01  IF   E           K DISK    RENAME(REPRAFED:R1)
     FPRAFED02  IF A E           K DISK
     FSGSYSV    IF   E             DISK
     FBANUME    UF A E           K DISK
     FBASCOR03  IF   E           K DISK
     FBASCAR01  IF   E           K DISK
     D NO              C                   CONST('0')
     D SI              C                   CONST('1')
     C*
     c                   ExSr      GetLastNo
     C                   ExSr      AbrirCursor
     C                   ExSr      LeerCursor
     c                   DoW       SQLCOD = *ZERO
     c                   ExSr      YaExistePedido
     c                   If        ExistePedido = NO
     c                   ExSr      InsertarBaja
     c                   EndIf
     C                   ExSr      LeerCursor
     C                   EndDo
     C                   ExSr      CerrarCursor
     C                   ExSr      EndPgm
     c*---------------------------------------------------------------------
     c* YaExistePedido: Verifica si Ya existe un pedido de baja p/la oper.
     c*---------------------------------------------------------------------
     c     YaExistePedidoBegSr
     C*
     c                   Move      NO            ExistePedido      1
     C*
     C     KAF020        Chain     REPRAFED                           99
     c                   DoW       *IN99 = *Off
     C                   If        AFIOPT='B' AND AFFECH=AASFEI
     c                   Move      SI            ExistePedido
     c                   Leave
     C                   EndIf
     C     KAF020        ReadE     REPRAFED                               99
     c                   EndDo
     C*
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* AbrirCursor
     c*---------------------------------------------------------------------
     c     AbrirCursor   BegSr
     c* ... Creditos que teniendo un alta en Federal, esten de baja en Prestamos
     c*     Y no tengan una baja informada en Federal
     c/exec sql
     C+ DECLARE C1 CURSOR FOR
     C+ SELECT
     C+       JVISUC,
     C+       JVINCR,
     C+       JVIDEG
     c+ -- Desde PRCRED Filtrado ver WHERE (Solo pagados y d/Lin 1,5,8,10)
     C+ FROM PRCRED
     c+ -- INNER Join para obtener solo las op. de Emp 1 y 18
     C+ INNER JOIN PRHATB ON
     C+       JVITIN=TBITIN
     C+   AND JVININ=TBININ
     C+   AND TBDF03='F'
     c+ -- INNER Join para obtener solo las op. que tengan Alta en Federal
     C+ INNER JOIN PRAFED A ON
     C+       JVISUC=A.AFISUC
     C+   AND JVINCR=A.AFINCR
     C+   AND JVIDEG=A.AFIDEG
     C+   AND A.AFIOPT='A'
     c+ -- LEFT Join p/obt. tanto filas que cruzan como las que no (Ver WHERE)
     C+ LEFT  JOIN PRAFED B ON
     C+       JVISUC=B.AFISUC
     C+   AND JVINCR=B.AFINCR
     C+   AND JVIDEG=B.AFIDEG
     C+   AND B.AFFECH>0
     C+   AND B.AFIOPT='B'
     C+   AND B.AFESTA='A'
     C+ WHERE
     C+ -- Solo Usar creditos que esten pagados
     C+       JVILCR = 9 AND A.AFINDO IN (
     C+       10393905, 18529458,
     C+       13694482, 16567481,
     C+       24820812, 13446245,
     C+       14828822, 16615903,
     C+       13699335, 13938674,
     C+       11922622, 12569477,
     C+       16433572, 16429567,
     C+       26686448, 14567461,
     C+       16433428, 10640661 )
     C+ -- Solo Usar creditos que no tengan baja en PRAFED
     C+   AND B.AFIRRN IS NULL
     c/end-exec
     c*
     C/exec sql
     C+ OPEN C1
     C/END-EXEC
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* Leer Cursor
     c*---------------------------------------------------------------------
     c     LeerCursor    BegSr
     C*
     c/exec sql
     C+ FETCH NEXT FROM C1 INTO :WWISUC, :WWINCR, :WWIDEG
     c/end-exec
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* Cerrar Cursor
     c*---------------------------------------------------------------------
     c     CerrarCursor  BegSr
     C*
     c/exec sql
     c+ CLOSE C1
     c/end-exec
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     InsertarBaja  BegSr
     c*
     C     KAF020        Chain     REPRAFED                           99
     c                   DoW       *IN99 = *Off
     C                   If        AFIOPT='A'
     c                   Add       1             WWIRRN
     c                   Add       1             WWISEQ
     c                   Z-Add     WWIRRN        AFIRRN
     c                   Z-Add     WWISEQ        AFISEQ
     c                   Move      'B'           AFIOPT
     C                   Z-Add     AASFEI        AFFECH
     C                   Move      *BLANKS       AFESTA
     c                   Move      *BLANKS       AFAMRC
     c*
     c                   Move      *Off          Flag              1
     c* ... Buscar si el recibo asociado esta todavía como activo
     C     KASCOR        Chain     BASCOR03                           89
     c                   DoW       *IN89 = *Off
     C                   If        SCDF05='            ' And
     c                             SCIRED =AFIRED And
     c                             SCIBCF =AFIBCF
     C                   Move      SCIRED        AFIRED
     C                   Move      SCIBCF        AFIBCF
     C*                  Z-ADD     570           AFIMDS
     C     SCIBCF        Chain     BASCAR01                           88
     C                   If        *IN88 = *OFF
     C                   If        ARIOPT='M'
     C*                  Z-ADD     571           AFIMDS
     C                   EndIf
     C                   EndIf
     c                   Move      *On           Flag              1
     c                   Leave
     C                   EndIf
     C     KASCOR        READE     BASCOR03                               89
     c                   EndDo
     c*
     c* ... No se encontró rec asoc. activo, buscar el primer recibo activo
     c                   If        Flag = *Off
     C     KASCOR        Chain     BASCOR03                           89
     c                   DoW       *IN89 = *Off
     C                   If        SCDF05='            '
     C                   Move      SCIRED        AFIRED
     C                   Move      SCIBCF        AFIBCF
     C*                  Z-ADD     570           AFIMDS
     C     SCIBCF        Chain     BASCAR01                           88
     C                   If        *IN88 = *OFF
     C                   If        ARIOPT='M'
     C*                  Z-ADD     571           AFIMDS
     C                   EndIf
     C                   EndIf
     c                   Leave
     C                   EndIf
     C     KASCOR        READE     BASCOR03                               89
     c                   EndDo
     c                   EndIf
     c*
     c* ... Escribir registro en prafed
     C                   Write     REPRAFED
     c                   Leave
     C                   EndIf
     C     KAF020        ReadE     REPRAFED                               99
     c                   EndDo
     C*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* EndPgm: Finalizar Programa
     c*---------------------------------------------------------------------
     c     EndPgm        BegSr
     C*
     c                   SetOn                                        LR
     c                   Return
     C*
     C                   EndSr
     C*---------------------------------------------------------------------
     C* GetLastNo: Obtener Ultimo Número
     C*---------------------------------------------------------------------
     C     GetLastNo     BegSr
     C*
     c* ... Obtiene Ultimo IRRN
     C                   MoveL(P)  'PRFD00RG'    WNIPF1
     C                   MoveL(P)  'AFIRRN  '    WNIPF2
     C                   Move      *BLANKS       WNIPF3
     C                   Move      *BLANKS       WNIPF4
     C                   Move      *BLANKS       WNIPF5
     C     KWN010        Chain     REBANUME                           99
+----C                   If        *In99 = *On
     c/EXEC SQL
     C+ SELECT MAX(AFIRRN) +1 INTO :WNIULN FROM PRAFED
     C/END-EXEC
|    C                   Write     REBANUME
+----C                   Else
|    C                   Add       1             WNIULN
|    C                   Update    REBANUME
+----C                   EndIf
     C                   Z-Add     WNIULN        WWIRRN
     C*
     c* ... Obtiene Ultimo ISEQ para el día
     C                   MoveL(P)  'PRFD00RG'    WNIPF1
     C                   MoveL(P)  'AFISEQ  '    WNIPF2
     C                   Move(P)   AASFEI        WNIPF3
     C                   Move      *BLANKS       WNIPF4
     C                   Move      *BLANKS       WNIPF5
     C     KWN010        Chain     REBANUME                           99
+----C                   If        *In99 = *On
     c                   Z-Add     1             WNIULN
|    C                   Write     REBANUME
+----C                   Else
|    C                   Add       1             WNIULN
|    C                   Update    REBANUME
+----C                   EndIf
     C                   Z-Add     WNIULN        WWISEQ
     C*
     C                   EndSr
     c*---------------------------------------------------------------------
     c* *INZSR: Rutina de Inicialización
     c*---------------------------------------------------------------------
     c     *INZSR        BegSr
     C*
     c*
     c     *Like         Define    AFISUC        WWISUC
     c     *Like         Define    AFINCR        WWINCR
     c     *Like         Define    AFIDEG        WWIDEG
     C     *Like         Define    AFIRRN        WWIRRN
     C     *Like         Define    AFISEQ        WWISEQ
     C     *Like         Define    SCIEMP        WWIEMP
     c*
     c     KAF020        KList
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     C*
     C     KWN010        KList
     C                   KFld                    WNIPF1
     C                   KFld                    WNIPF2
     C                   KFld                    WNIPF3
     C                   KFld                    WNIPF4
     C                   KFld                    WNIPF5
     C*
     c     KASCOR        KList
     c                   KFld                    AFINDO
     c                   KFld                    WWIEMP
     c*
     c                   Z-ADD     1             WWIEMP
     c*
     C     1             Chain     RESGSYSV
     c*
     C                   Z-Add     AASFEI        FECCOR            8 0
     C                   Move      '01'          FECCOR
     c*
     C                   EndSr
