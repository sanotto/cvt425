*SRCMBRTXT:Switch-Adapter      -PF-370000-Cons Tas
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA                                          *
     H*                                                               *
     H*  PROGRAM NAME: SWAD10RG                                       *
     H*                                                               *
     H*  PROGRAM NO: Consulta Publicitaria de Plazos Fijos            *
     H*                                                               *
     H*  DATE: 13/09/2017                                             *
     H*                                                               *
     H*  AUTHOR: Ottonello, Santiago                                  *
     H*                                                               *
     H*  PAPFTI   1    Tipo de Cert                                   *
     H*                 T  Tradicional  P  Precancelable              *
     H*                "V tasa variable  B  tasa baibor               *
     H*                 C  Ajus p/CER.                                *
     H*  PAPFIM  15 2   Importe    7                                  *
     H*  PAPFPL   3 0   Plazo                                         *
     H*  PAIERR  40     De Salida, código de error, ver NPC12008      *
     H*  PARESP 360     10 lineas de 36 para el ticket                *
     H*  PANPAG   2     De Entrada Salida                             *
     H*                 Si viene un nro NN devuelve página NN+1 y NN+1*
     H*                 en PANPAG.                                    *
     H*                 Si hay una sola página se devuelve 1P         *
     F*----------------------------------------------------------------
     D BUFFDS          DS                  OCCURS(999)
     D  buflin                       36
     D*
     D PAGEDS          DS                  OCCURS(10)
     D  txtlin                       36

     D PAGSTR          S            360    BASED(PAGLIN)
     D PAGLIN          S               *

     D I               S              3  0
     D J               S              3  0
     D Desde           S              3  0
     D Hasta           S              3  0
     D BUFLEN          S              3  0
     D CANPAG          S              3  0
     D WKTRAM          S              5  0
     D WKTASA          S             13  7
     D ERR003          C                   CONST('303NO SE ENC.PIZARRA')
     D*----------------------------------------------------------------
     c                   MoveL(P)  '000'         PAIERR
     c                   ExSr      CheckTipoPF
     c                   Select
     c                   When      PAPFIM <> 0 and PAPFPL =  0
     c                   ExSr      LstPlazosTasas
     c                   When      PAPFIM =  0 and PAPFPL <> 0
     c                   ExSr      LstTasa
     c                   When      PAPFIM <> 0 and PAPFPL <> 0
     c                   ExSr      LstTasa2
     c                   EndSl
     c* ... Mover lineas a salida
     C     1             Occur     PAGEDS
     C                   Eval      PAGLIN=%ADDR(PAGEDS)
     C                   MoveL(P)  PAGSTR        PARESP
     c*
     c                   ExSr      EndPgm
     c*--------------------------------------------------------------------
     c* CheckTipoPF: Chequea si el tipo de PF solic. es soportado p/SIDEBA
     c*--------------------------------------------------------------------
     c     CheckTipoPF   BegSr
     c*
     c* ... Solo aceptamos PF tradicionales (PAPFTI='T') O UVA (PAPFTI='A')
     c                   If        PAPFTI='P' or
     c                             PAPFTI='B' or
     c                             PAPFTI='V' or
     c                             PAPFTI='C'
     c*
     c                   Eval      PAIERR='047Tipo d/PF:'+PAPFTI+'.No soportado'
     c*
     c                   EndIf
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* LstTasa: Se lista la tasa para el importe y el plazo indicado
     c*--------------------------------------------------------------------
     c     LstTasa       BegSr
     c*
     c* ... Esta respuesta siempre será de una única página, así que
     c                   Move      '1P'          PANPAG
     c*
     c                   ExSr      OpenC1
     c                   ExSr      FetchC1
     c*
     c                   If        SQLCOD <> *zero
     c                   MoveL(P)  ERR003        PAIERR
     c                   ExSr      CloseC1
     c                   LeaveSr
     c                   EndIf
     c*
     c                   DoW       SQLCOD = *Zero
     c                   If        WKTRAM=PAPFPL
     c     1             Occur     BUFFDS
     c                   Eval      buflin= %EditW(WKTRAM:'     ')+' '+
     c                                     %EditW(WKTASA:'          ,       ')
     c                   Leave
     c                   EndIf
     c                   ExSr      FetchC1
     c                   EndDo
     c                   ExSr      CloseC1
     c*
     c                   Z-Add     1             BUFLEN
     c                   ExSr      FillPage
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* LstTasa2: Se lista la tasa para el importe y el plazo indicado
     c*--------------------------------------------------------------------
     c     LstTasa2      BegSr
     c*
     c* ... Esta respuesta siempre será de una única página, así que
     c                   Move      '1P'          PANPAG
     c*
     c                   ExSr      OpenC2
     c                   ExSr      FetchC2
     c*
     c                   If        SQLCOD <> *zero
     c                   MoveL(P)  ERR003        PAIERR
     c                   ExSr      CloseC2
     c                   LeaveSr
     c                   EndIf
     c*
     c                   DoW       SQLCOD = *Zero
     c                   If        WKTRAM=PAPFPL
     c     1             Occur     BUFFDS
     c                   Eval      buflin= ' '+%Trim(
     c                                     %EditW(WKTASA:'          ,       '))
     c                   Leave
     c                   EndIf
     c                   ExSr      FetchC2
     c                   EndDo
     c                   ExSr      CloseC2
     c*
     c                   Z-Add     1             BUFLEN
     c                   ExSr      FillPage
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* LstPlazosTasas: Lista las tasas según plazos para un importe dado
     c*--------------------------------------------------------------------
     c     LstPlazosTasasBegSr
     c*
     c                   Z-Add     *Zero         BUFLEN
     c*
     c                   Z-Add     *Zero         J
     c                   ExSr      OpenC1
     c                   ExSr      FetchC1
     c*
     c                   If        SQLCOD <> *zero
     c                   MoveL(P)  ERR003        PAIERR
     c                   ExSr      CloseC1
     c                   LeaveSr
     c                   EndIf
     c*
     c                   ExSr      PutColHead
     c                   DoW       SQLCOD = *Zero
     c                   Add       1             J
     c     j             Div       10            x                 3 0
     c                   MVR                     r                 1 0
     c                   If        r=0
     c                   ExSr      PutColHead
     c                   EndIf
     c     j             Occur     BUFFDS
     c                   Eval      buflin= %EditW(WKTRAM:'     ')+' '+
     c                                     %EditW(WKTASA:'          ,       ')
     c                   ExSr      FetchC1
     c                   EndDo
     c                   ExSr      CloseC1
     c*
     c                   ExSr      FillPage
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* PutColHead: Poner Cabeceras de Columnas
     c*--------------------------------------------------------------------
     c     PutColHead    BegSr
     c*
     c                   Add       1             J
     c     j             Occur     BUFFDS
     c                   Eval      buflin='Plazo TNA'
     c                   Add       1             J
     c     j             Occur     BUFFDS
     c                   Eval      buflin='----- -----------'
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* LstImportTasas: Lista las tasas según importes para un plazo dado
     c*--------------------------------------------------------------------
     c     LstImportTasasBegSr
     c*
     c                   Z-Add     *Zero         BUFLEN
     c                   ExSr      FillPage
     c*
     c                   EndSr
     c*---------------------------------------------------------------------
     c* FillPage: Rellena la DS Multiple PAGEDS desde la BUFFDS p/pág PANPAG
     c*---------------------------------------------------------------------
     c     FillPage      BegSr
     c*
     c                   If        PANPAG='1P'
     c                   Z-Add     *Zero         Page              2 0
     c                   Else
     c                   TestN                   PANPAG               99
     c  N99              Z-Add     *Zero         Page              2 0
     c   99              Move      PANPAG        Page              2 0
     c                   Endif
     c*
     c                   Add       1             Page
     c*
     c     BUFLEN        Div       10            CANPAG
     c                   MVR                     Remainder         1 0
     c     Remainder     Comp      *Zero                              9999
     c   99              Add       1             CANPAG
     c*
     c                   Select
     c                   When      CanPag = 1
     c                   Move      '1P'          PANPAG
     c                   When      Page=CanPag
     c                   Move      'LP'          PANPAG
     c                   When      Page<CanPag and CanPag > 1
     c                   Move      Page          PANPAG
     c                   EndSl
     c*
     c                   ExSr      ClearPage
     c*
     c                   Eval      Desde=((Page - 1)*10 + 1)
     c                   Eval      Hasta=Desde +  9
     c                   Z-Add     1             J
     c                   For       I= Desde to Hasta
     C     I             Occur     BUFFDS
     C     J             Occur     PAGEDS
     c                   Move      buflin        txtlin
     c                   Add       1             J
     c                   EndFor
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* ClearPage: Limpia la página de Respuesta
     c*--------------------------------------------------------------------
     c     ClearPage     BegSr
     C*
     c                   For       I= 1 to 10
     C     i             Occur     PAGEDS
     c                   Move      *Blanks       txtlin
     c                   EndFor
     C*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* *InzSr: Inicialización del Programa
     c*--------------------------------------------------------------------
     c     *InzSr        BegSr
     c*
     c     *Entry        PList
     c                   Parm                    PAPFTI            1
     c                   Parm                    PAPFIM           15 2
     c                   Parm                    PAPFPL            3 0
     c                   Parm                    PARESP          360
     c                   Parm                    PANPAG            2
     c                   Parm                    PAIERR           40
     c*
     C                   MOVE      'C'           SDBTPF            1
     C                   IF        PAPFTI='A'
     C                   MOVE      'F'           SDBTPF            1
     C                   ENDIF
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* OpenC1: Declarar Cursor C1
     c*--------------------------------------------------------------------
     c     OpenC1        BegSr
     c*
     c/exec sql
     c+ DECLARE C1 CURSOR FOR
     c+ SELECT
     c+       ABS(PFCATR.IVITTR),
     C+       PFCATR.IVTTNA
     c+ FROM PFPIZA , PFCATR
     c+ WHERE
     c+       ITISUC = 99999
     c+   AND ITIGCE = :SDBTPF
     c+   AND ITIMON = 1
     c+   AND ITISUC = IVISUC
     c+   AND ITINPI = IVINPI
     c+   AND ITINPI IN ( SELECT
     c+                      ITINPI
     c+                   FROM PFPIZA
     c+                   WHERE
     c+                          ITISUC = 99999
     c+                      AND ITIGCE = :SDBTPF
     c+                      AND ITIMON = 1
     c+                   LIMIT 1
     C+                 )
     c+ ORDER BY
     c+         PFCATR.IVITTR DESC
     c/end-exec
     c*
     c/exec sql
     c+ OPEN C1
     c/end-exec
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* OpenC2: Declarar Cursor C2
     c*--------------------------------------------------------------------
     c     OpenC2        BegSr
     c*
     c/exec sql
     C+  DECLARE C2 CURSOR FOR
     C+  SELECT
     C+       ABS(PFCATR.IVITTR),
     C+       PFCATR.IVTTNA
     C+  FROM PFPIZA , PFCATR
     C+  WHERE
     C+          ITISUC = 99999
     C+      AND ITIGCE = :SDBTPF
     C+      AND ITIMON = 1
     C+      AND ITISUC= IVISUC
     C+      AND ITINPI = IVINPI
     C+      AND  ITINPI IN (
     C+                      SELECT
     C+                            MAX(ITINPI)
     C+                      FROM PFPIZA
     C+                      WHERE
     C+                                ITISUC = 99999
     C+                           AND  ITIGCE = :SDBTPF
     C+                           AND ITIMON = 1
     C+                     )
     C+      AND  ABS(PFCATR.IV$T01 ) <= :PAPFIM
     C+      ORDER BY
     C+           PFCATR.IVITTR DESC
     c/end-exec
     c*
     c/exec sql
     c+ OPEN C2
     c/end-exec
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* FetchC2: Leer   Cursor C2
     c*--------------------------------------------------------------------
     c     FetchC2       BegSr
     c*
     c/exec sql
     c+ FETCH C2 INTO  :WKTRAM, :WKTASA
     c/end-exec
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* CloseC2: Cerrar Cursor C2
     c*--------------------------------------------------------------------
     c     CloseC2       BegSr
     c*
     c/exec sql
     c+ CLOSE C2
     c/end-exec
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* FetchC1: Leer C1
     c*--------------------------------------------------------------------
     c     FetchC1       BegSr
     c*
     c/exec sql
     c+ FETCH C1 INTO  :WKTRAM, :WKTASA
     c/end-exec
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* CloseC1: Cerrar Cursor C1
     c*--------------------------------------------------------------------
     c     CloseC1       BegSr
     c*
     c/exec sql
     c+ CLOSE C1
     c/end-exec
     c*
     c                   EndSr
     c*--------------------------------------------------------------------
     c* EndPgm: Fin de Programa
     c*--------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
