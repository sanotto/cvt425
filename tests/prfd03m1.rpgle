*SRCMBRTXT:Aut.Federal-Manejador de Recepción de A
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     FPRAFED01  UF   E           K DISK
     F*----------------------------------------------------------------*
     FSGSYSV    IF   E             DISK
     F*  The system values file
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     D*----------------------------------------------------------------*
     D*-------------------------------------------------------------------------
     D*Aca estan las definiciones de Prototipos para llamar a las API del IFS
     D*-------------------------------------------------------------------------
     D/COPY SDB01.SRC/QRPGSRC,IFSIO_H
     D/copy LE00525/socketsrpg,errno_h
     D readline        PR            10I 0
     D  fd                           10I 0 value
     D  text                           *   value
     D  maxlen                       10I 0 value
     D buf             S             45
     D bufer           S             29
     D buff            S             31
     D buf1            S            194
     D
     D*-------------------------------------------------------------------------
     DERRDS            DS
     D ERRTXT                  1    255
     D WWNCU1                  1     55
     D WWNCU2                 56    110
     D WWNCU3                111    165
     D WWNCU4                166    220
     D WWNCB1                221    275
     D WWNCB2                276    330
     D WWNCB3                331    385
     D WWNCB4                386    440
     D*
     DCODIG            DS
     DLINCOD                   1      8
     DWWNAME                   1      2
     DWWCODI                   3      8  0
     D*-------------------------------------------------------------------------
     D RC              S              7A
     D indo            S              8A
     D doc             S              8S 0
     D ndoc            S             15S 0
     D count           S              8S 0
     D path            S            255A
     D path_from       S            255A
     D path_f          S            255A
     D path_to         S            255A
     D path_out        S            255A
     D deno            S             50A
     D mesg            s            115A
     D err             S             10I 0
     D fdi             S             10I 0
     D fdo             S             10I 0
     D fdf             S             10I 0
     D fdt             S             10I 0
     D fdm             S             10I 0
     D errmsg          S            250A
     D p_dir           S               *
     D file_name       S            255A
     D newname         S            255A
     D line            S           1024A
     D msg             S            200A
     D file            S             20A
     D prefix          S              6A
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D CMDSTR          S           4096A   INZ(*BLANKS)
     D ec              S             10I 0
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   4096A   VALUE
     D**********************************************************************
     c*                  ExSr      CheckStruct
     c                   ExSr      SndLotus
     c                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     c     *INZSR        BegSr
     C*-------------------------------------------------------------------------
     C*
     C     KFD0100       KList
     c                   KFld                    AFFECH
     c                   KFld                    AFISEQ
     C*
     c     1             Chain     SGSYSV
     c                   Move      AASFEI        CHSFEI            8
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     c     SndLotus      BegSr
     C*-------------------------------------------------------------------------
     c                   Eval      path_out='/home/tmp/fed_acep_'+chsfei+'.xls'
     C                   Eval      CmdStr='EXPPCDBF SQLSTM('' '+
     C                                    'SELECT             '+
     C                                    ' AFFECH AS FECHA,  '+
     C                                    ' AFINDO AS DOC,    '+
     C                                    ' AFNYAP AS NOMBRE, '+
     C                                    ' AFISUC AS SUC,    '+
     C                                    ' AFINCR AS CRED,   '+
     C                                    ' AFIDEG AS DESG,   '+
     C                                    ' AF$IMP AS IMPORTE,'+
     C                                    ' AFQCUO AS CUOTAS, '+
     C                                    ' AFEDSU AS SUC,    '+
     C                                    ' AFINCV AS CRED,   '+
     C                                    ' AFIDEV AS DESG,   '+
     C                                    ' AFESTA AS ESTADO, '+
     C                                    ' AFAMRC AS MOTIVO, '+
     C                                    ' AFICHE AS COMPR   '+
     C                                    'FROM PRAFED        '+
     C                                    'WHERE              '+
     C                                    ' AFFECH=' + chsfei +
     C                                    ' AND AFESTA=''''A'''' '') '+
     C                                    'OUTPAT('''+%TRIM(path_out)+''')'
     c                   Eval      rc=Shell(CmdStr)
     c                   Eval      CmdStr='SNDMAIL RECP(PRHA04CL) '+
     c                             'SUBJECT(''Op. del dia'')'+
     c                             ' FILE('''+%trim(path_out)+''')'
     c                   Eval      rc=Shell(CmdStr)
     c                   Eval      path_out='/home/tmp/fed_rech_'+chsfei+'.xls'
     C                   Eval      CmdStr='EXPPCDBF SQLSTM('' '+
     C                                    'SELECT             '+
     C                                    ' AFFECH AS FECHA,  '+
     C                                    ' AFINDO AS DOC,    '+
     C                                    ' AFNYAP AS NOMBRE, '+
     C                                    ' AFISUC AS SUC,    '+
     C                                    ' AFINCR AS CRED,   '+
     C                                    ' AFIDEG AS DESG,   '+
     C                                    ' AF$IMP AS IMPORTE,'+
     C                                    ' AFQCUO AS CUOTAS, '+
     C                                    ' AFEDSU AS SUC,    '+
     C                                    ' AFINCV AS CRED,   '+
     C                                    ' AFIDEV AS DESG,   '+
     C                                    ' AFESTA AS ESTADO, '+
     C                                    ' AFAMRC AS MOTIVO, '+
     C                                    ' AFICHE AS COMPR   '+
     C                                    'FROM PRAFED        '+
     C                                    'WHERE              '+
     C                                    ' AFFECH=' + chsfei +
     C                                    ' AND AFESTA<>''''A'''' '') '+
     C                                    'OUTPAT('''+%TRIM(path_out)+''')'
     c                   Eval      rc=Shell(CmdStr)
     c                   Eval      CmdStr='SNDMAIL RECP(PRHA04CL) '+
     c                             'SUBJECT(''Op. Rechazadas del dia'')'+
     c                             ' FILE('''+%trim(path_out)+''')'
     c                   Eval      rc=Shell(CmdStr)
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   SetOn                                        LR
     C                   Return
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
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
      *-------------------------------------------------------------------------

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
