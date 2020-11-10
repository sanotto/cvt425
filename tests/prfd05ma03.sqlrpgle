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
     H*  AUTHOR: SO                                                   *
     H*                                                               *
     H*****************************************************************
     FPRAFED01  IF   E           K DISK    RENAME(REPRAFED:R1)
     FPRAFED02  IF A E           K DISK
     FSGSYSV    IF   E             DISK
     FBANUME    UF A E           K DISK
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
     C+ WHERE
     C+ -- Solo Usar creditos que esten pagados
     C+       JVFEPA <> 0
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
     c*
     C     1             Chain     RESGSYSV
     c*
     C                   EndSr
