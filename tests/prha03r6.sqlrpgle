*SRCMBRTXT:Ces.Hab.-Validación    -Valida Nuevo Ft
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME  : SIDEBA - PRESTAMOS                            *
     H*                                                               *
     H*  PROGRAM NAME : PRHA03R6                                      *
     H*                                                               *
     H*  PROGRAM TITLE: Ces.Hab.-Valida                               *
     H*                                                               *
     H*  DATE         : 31/10/2013                                    *
     H*                                                               *
     H*  AUTHOR       : Ottonello, Santiago                           *
     H*                                                               *
     H*  DESCRIPTION  :                                               *
     H*---------------------------------------------------------------*
     FPRHAMV04  UF   E           K DISK
     FBASCAR01  IF   E           K DISK
     FSGSYSV    IF   E             DISK
     D*----------------------------------------------------------------
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------
     DMYPSDS          SDS
     D @PRC_NAM          *PROC
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D @USR_NAM              254    263
     D*-------------------------------------------------------------------------
     D*Aca estan las definiciones de Prototipos para llamar a las API del IFS
     D*-------------------------------------------------------------------------
     D/COPY SDB01.SRC/QRPGSRC,IFSIO_H
     D/copy LE00525/socketsrpg,errno_h
     D*-------------------------------------------------------------------------
     D readline        PR            10I 0
     D  fd                           10I 0 value
     D  text                           *   value
     D  maxlen                       10I 0 value
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   4096A   VALUE
     D*-------------------------------------------------------------------------
     D buf             S            158
     D rc              S              7A
     D path_from       S            255A
     D err             S             10I 0
     D errmsg          S            250A
     D fdi             S             10I 0
     D line            S           1024A
     D CMDSTR          S           4096A   INZ(*BLANKS)
     D ec              S             10I 0
     D*-------------------------------------------------------------------------
     D RecDs           DS
     D  FilLin                 1    158
     D   WKFAAM                1      6  0
     D   WKINDO                7     14  0
     D   WKIBCF               15     19
     D   WKIRED               20     21
     D   WKISEX               22     22
     D   WKNYAP               23     47
     D   WKIMDS               48     50
     D   WKISUC               51     52  0
     D   WKINCR               53     62  0
     D   WKIDEG               63     64  0
     D   WKICUO               65     67  0
     D   WKICUI               68     78
     D   WK$IMP               79     93  2
     D   WK$COB               94    108  2
     D   WKOBSE              109    158
     D*-------------------------------------------------------------------------
     C* ... Abrir Archivo de Entrada
     c                   ExSr      OpenInputF
     C                   Z-Add     *Zero         WWIMPO
     C                   Z-Add     *Zero         WWCANR
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
+----c                   Eval      ec=readline(fdi: %addr(line): %size(line))
+----c                   DOW       ec>=0 or  line <> *blanks
     c                   MoveL     Line          FilLin
     c                   Add       1             WWCANR
     c                   Add       WK$COB        WWIMPO
     C                   ExSr      UpdMaster
+----c                   Eval      ec=readline(fdi: %addr(line): %size(line))
+----C                   ENDDo
     c* .... Cerrar los archivos
     c                   CallP     closef(fdi)
     C                   EXSR      ENDPGM
|    C*-------------------------------------------------------------------------
|    C* UpdMaster: Actualizar el Maestro de Cesión de Haberes(PRHAMV)
|    C*-------------------------------------------------------------------------
|    C     UpdMaster     BEGSR
     c*
     c                   Z-Add     WKISUC        WWISUC
     c                   Z-Add     WKINCR        WWINCR
     c                   Z-Add     WKIDEG        WWIDEG
     c                   Z-Add     WKICUO        WWICUO
     c* ... El registro debe encontrarse en PRHAMV, caso contrario, descartarlo
     c     KMV045        Chain     REPRHAMV                           99
     c                   If        *In99 = *On
     c                   LeaveSr
     c                   EndIf
     c* ... Actualiza Recibo con la información recibida
     c* ... ... Si cambio el area, buscar el tipo y nro de inscripción
     c                   MoveL     WKIBCF        WWIBCF
     c                   If        WWIBCF <>     MVIBCF
     C     WWIBCF        Chain     REBASCAR                           99
     c                   If        *In99 = *Off
     c                   Move      ARITIN        MVITIN
     c                   Move      ARININ        MVININ
     c                   EndIf
     c                   EndIf
     c*
     c                   ExSr      FixIRED
     c                   Move      WWIRED        MVIRED
     c*
     c                   If        WK$COB > 0
     c* ... Marca como cobrado
     c                   Move      '1'           MVCREC
     c* ... ... Pone Importes
     c                   Z-Add     WK$COB        MV$IMO
     c                   Z-Add     MV$IMP        MV$ICU
     c* ... ... Si le descontaron demas, poner la diferencia en DEPOSITAR
     C                   Eval      MV$IDP = 0
     c                   If        MV$IMO > MV$IMP
     C                   Eval      MV$IDP = MV$IMO - MV$IMP
     c                   EndIf
     c                   Else
     c* ... Si NO le descontaron, cerear campos, marcar como no cobrado
     c                   Z-Add     *Zero         MV$IMO
     c                   Z-Add     *Zero         MV$ICU
     c                   Move      '2'           MVCREC
     c                   EndIf
     c* ... Guardar las observaciones que hubiera
     C                   Eval      MVDF05=*Blanks
     C                   Eval      MVDF06=*Blanks
     C                   Eval      MVDF07=*Blanks
     C                   Eval      MVDF05=%subst(WKOBSE: 1:20)
     C                   Eval      MVDF06=%subst(WKOBSE:21:20)
     C                   Eval      MVDF07=%subst(WKOBSE:41:10)
     c* ... Marca PRHAMV como validado
|    C                   Z-ADD     AASFEI        MVFALT
|    C                   TIME                    MVHALT
|    C                   MOVE      @USR_NAM      MVIUSR
|    C                   MOVE      @PRC_NAM      MVIPGM
|    C                   Z-ADD     AASFEI        MVFPRC
     c*
|    C                   Update    REPRHAMV
     c*
     c                   EndSr
|    C*-------------------------------------------------------------------------
|    C* FixIred: Arregla Nro de Trabajo
|    C*-------------------------------------------------------------------------
|    C     FixIRED       BegSr
     c*
     C                   Move      WKIRED        wwired
     C                   SELECT
     C     WWIRED        WHENEQ    '10'
     C                   MOVE      'A'           WWIRED
     C     WWIRED        WHENEQ    '11'
     C                   MOVE      'B'           WWIRED
     C     WWIRED        WHENEQ    '12'
     C                   MOVE      'C'           WWIRED
     C     WWIRED        WHENEQ    '13'
     C                   MOVE      'D'           WWIRED
     C     WWIRED        WHENEQ    '14'
     C                   MOVE      'E'           WWIRED
     C     WWIRED        WHENEQ    '15'
     C                   MOVE      'F'           WWIRED
     C     WWIRED        WHENEQ    '16'
     C                   MOVE      'G'           WWIRED
     C     WWIRED        WHENEQ    '17'
     C                   MOVE      'H'           WWIRED
     C     WWIRED        WHENEQ    '18'
     C                   MOVE      'I'           WWIRED
     C     WWIRED        WHENEQ    '19'
     C                   MOVE      'J'           WWIRED
     C     WWIRED        WHENEQ    '20'
     C                   MOVE      'K'           WWIRED
     C     WWIRED        WHENEQ    '21'
     C                   MOVE      'L'           WWIRED
     C     WWIRED        WHENEQ    '22'
     C                   MOVE      'M'           WWIRED
     C     WWIRED        WHENEQ    '23'
     C                   MOVE      'N'           WWIRED
     C     WWIRED        WHENEQ    '24'
     C                   MOVE      'O'           WWIRED
     C     WWIRED        WHENEQ    '25'
     C                   MOVE      'P'           WWIRED
     C     WWIRED        WHENEQ    '26'
     C                   MOVE      'Q'           WWIRED
     C     WWIRED        WHENEQ    '27'
     C                   MOVE      'R'           WWIRED
     C     WWIRED        WHENEQ    '28'
     C                   MOVE      'S'           WWIRED
     C     WWIRED        WHENEQ    '29'
     C                   MOVE      'T'           WWIRED
     C     WWIRED        WHENEQ    '30'
     C                   MOVE      'U'           WWIRED
     C     WWIRED        WHENEQ    '31'
     C                   MOVE      'V'           WWIRED
     C     WWIRED        WHENEQ    '32'
     C                   MOVE      'W'           WWIRED
     C     WWIRED        WHENEQ    '33'
     C                   MOVE      'X'           WWIRED
     C     WWIRED        WHENEQ    '34'
     C                   MOVE      'Y'           WWIRED
     C     WWIRED        WHENEQ    '25'
     C                   MOVE      'Z'           WWIRED
     C                   ENDSL
     C     WWIRED        IFEQ      '00'
     C                   MOVE      '1'           WWIRED
     C                   ENDIF
     c*
     c                   EndSr
|    C*-------------------------------------------------------------------------
|    C* *INZSR : Inicialización
|    C*-------------------------------------------------------------------------
|    C     *INZSR        BEGSR
|    C     *ENTRY        PLIST
|    C                   PARM                    PAFAAM            6
|    C                   PARM                    PAIEMP            5
|    C                   PARM                    PADESC           30
     C                   PARM                    WWIMPO           15 2
     C                   PARM                    WWCANR           10 0
     C                   PARM                    PAFILE          250
|    C* ... Recupera Fecha de Sistema
|    C     1             CHAIN     SGSYSV                             80
|    C*
     c     *Like         Define    MVFAAM        WWFAAM
     c     *Like         Define    MVIEMP        WWIEMP
     c     *Like         Define    MVISUC        WWISUC
     c     *Like         Define    MVINCR        WWINCR
     c     *Like         Define    MVIDEG        WWIDEG
     c     *Like         Define    MVICUO        WWICUO
     c     *Like         Define    MVIRED        WWIRED
     c     *Like         Define    MVITIN        WWITIN
     c     *Like         Define    MVININ        WWININ
     c     *Like         Define    MVIBCF        WWIBCF
|    C*
     c     KMV045        KList
     c                   KFld                    WWFAAM
     c                   KFld                    WWIEMP
     c                   KFld                    WWISUC
     c                   KFld                    WWINCR
     c                   KFld                    WWIDEG
     c                   KFld                    WWICUO
|    C*
|    C                   Move      PAFAAM        WWFAAM
|    C                   Move      PAIEMP        WWIEMP
|    C*
|    C                   ENDSR
     C*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   SetOn                                        LR
     C                   Return
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     OpenInputF    BegSr
     C*-------------------------------------------------------------------------
     c*
     c                   Eval      path_from=%trim(pafile)+x'00'
     c                   Eval      fdi = open(%addr(path_from): O_RDONLY +
     c                                        O_TEXTDATA     )
+----c                   If        fdi  < 0
|    c                   Eval      PADESC='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----C                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     IFSError      BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   eval      err= errno
     c                   eval      errmsg= %str(strerror(errno))
     c                   eval      err= errno
     c                   eval      PADESC=%trim(PADESC)+' Causa:'+errmsg
     c                   ExSr      Abort
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     Abort         BegSr
     C*-------------------------------------------------------------------------
     C*
     C                   Z-Add     *Zero         WWIMPO
     C                   Z-Add     *Zero         WWCANR
     c*
     c                   Eval      PADESC='FATAL:'+%trim(PADESC)
     c                   ExSr      EndPgm
     c*
     c                   EndSr
     C*=========================================================================
     P readline        B                   export
     D  readline       PI            10I 0
     D   fd                          10I 0 value
     D   text                          *   value
     D   maxlen                      10I 0 value
     D
     D rdbuf           S           1024A   static
     D rdpos           S             10I 0 static
     D rdlen           S             10I 0 static
     D
     D p_retstr        S               *
     D RetStr          S          32766A   based(p_retstr)
     D len             S             10I 0
     c
     c                   eval      len = 0
     c                   eval      p_retstr = text
     c                   eval      %subst(RetStr:1:MaxLen) = *blanks
     c
+----c                   dow       1 = 1
|    c
|    c* Load the buffer
|+---c                   if        rdpos>=rdlen
||   c                   eval      rdpos = 0
||   c                   eval      rdlen=read(fd:%addr(rdbuf):%size(rdbuf))
||   c
||+--c                   if        rdlen < 1
|||  c                   return    -1
||+--c                   endif
|+---c                   endif
|    c
|    c* Is this the end of the line?
|    c                   eval      rdpos = rdpos + 1
|+---c                   if        %subst(rdbuf:rdpos:1) = x'25'
||   c                   return    len
|+---c                   endif
|    c
|    C* Otherwise, add it to the text string.
|+---c                   if        %subst(rdbuf:rdpos:1) <> x'0d'
||   c                               and len<>maxlen
||   c                   eval      len = len + 1
||   c                   eval      %subst(retstr:len:1) =
||   c                               %subst(rdbuf:rdpos:1)
|+---c                   endif
|    c
+----c                   enddo
     c
     c                   return    len
     P                 E
     P*---------------------------------------------------------------------
     P* PROCEDIMIENTOS DEBEN DECLARARSE DESPUES DE LA HOJA O, SI ESTA EXISTE
     P*---------------------------------------------------------------------
     P Shell           B                   EXPORT
     D  Shell          PI             7A
     D   Command                   4096A   VALUE
     D CommLen         S             15  5

     C                   Call      'QCMDEXC'
     c                   PARM                    Command
     C                   PARM      1024          CommLen
     C                   Return    'CPF0000'
     c     *PSSR         BEGSR
     C                   Return    @EXC_TYP+@EXC_NUM
     c                   ENDSR
     c
     P Shell           E
     C*=========================================================================

      /define ERRNO_LOAD_PROCEDURE
      /copy LE00525/socketsrpg,errno_h
