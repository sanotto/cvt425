*SRCMBRTXT:Aut.Federal-Manejador de Recepción de A
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     FPRAFED01  UF a E           K DISK
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
     DCREDS            DS
     D NROOPE                  1     15
     D chisuc                  2      3
     D chincr                  4     13
     D chideg                 14     15
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
     c                   ExSr      CheckStruct
     c                   ExSr      ProcDir
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
     c     ProcDir       BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   Eval      path='/home/Cesion_de_haberes/Federal/'+
     c                                  'Recibidos'+x'00'
     c                   Eval      p_dir=opendir(%addr(path))
+----c                   If        p_dir = *NULL
|    c                   eval      errtxt='No se pudo leer:'+path+' Causa:'+
|    c                             errmsg
|    c                   ExSr      Abort
+----c                   EndIf
     C*
     c                   Move      *On           NoFileFound       1
     c                   Eval      p_dirent = readdir(p_dir)
+----c                   DoW       p_dirent <> *NULL
|    c                   Eval      file_name= %subst(d_name:1:d_namelen)
|    c                   Eval      file_name=%xlate(lo:up:file_name)
|+---c                   Eval      prefix = %subst(file_name:1:3)
|+---c                   if        prefix = 'NBR'
     c                   Move      *Off          NoFileFound
||   c                   ExSr      ProcessFile
|+---c                   EndIf
|    c                   Eval      p_dirent = readdir(p_dir)
+----c                   EndDo
     C*
     c                   CallP     closedir(p_dir)
     C*
     c                   If        NoFileFound = *On
     c                   Eval      WWNCU1='No se encontraron archivos p/Proc.'
     c                   Eval      WWNCU2='Copie los archivos a procesar en: '
     c                   Eval      WWNCU3=path
     c                   Eval      WWNCB4='Presione F3 Para Salir            '
     c                   ExSr      DspErr
     C                   EndIf
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     ProcessFile   BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   Z-Add     *ZERO         AFISEQ
     c                   Z-Add     *ZERO         AFIRRN
     C* ... Abrir Archivo de Entrada
     c                   ExSr      OpenInputF
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
+----c                   Eval      ec=readline(fdi: %addr(line): %size(line))
+----c                   DOW       ec>=0 or  line <> *blanks
  134c                   Add       1             AFIRRN
  134c                   Z-Add     *ZERO         AFFECH
  134c                   Z-Add     *ZERO         AFICAR
  134c                   Add       1             AFISEQ
  134c                   Move      'A'           AFIOPT
     c                   Move      *Blanks       CHIMDS            3
     c                   Eval      CHIMDS=%subst(line:011:003)
     c                   Move      CHIMDS        AFIMDS
     c*
     c                   Eval      AFISEX=%subst(line:014:001)
     c*
     c                   Move      *Blanks       CHINDO            8
     c                   Eval      CHINDO=%subst(line:015:008)
     c                   Move      CHINDO        AFINDO
     c*
     c                   Eval      AFNYAP=%subst(line:023:030)
     c*
     c                   Move      *Blanks       CHIRED            2
     c                   Eval      CHIRED=%subst(line:053:002)
     c                   Move      CHIRED        X                 2 0
     c                   add       1             x
     c                   Eval      AFIRED=%subst('01234567890ABC'+
     C                                            'DEFGHIJKLMN#OP'+
     C                                            'QRSTUVWXYZ':X)
     c*
     c                   Eval      AFIBCF=%subst(line:055:5)
     c*
     c                   Move      *BLANKS       CHIND1           14
     c                   Eval      CHIND1=%subst(line:060:014)
     c                   Move      CHIND1        AFIND1
     c*
     c                   Move      *BLANKS       CHIND2           14
     c                   Eval      CHIND1=%subst(line:074:014)
     c                   Move      CHIND2        AFIND2
     c*
     c*
     c                   Move      *BLANKS       CH$IMP            8
     c                   Eval      CH$IMP=%subst(line:098:006)+
     c                                    %subst(line:105:002)
     c                   Move      CH$IMP        AF$IMP
     c*
     c                   Move      *BLANKS       CHQCUO            3
     c                   Eval      CHQCUO=%subst(line:107:003)
     c                   Move      CHQCUO        AFQCUO
     c*
     c                   Move      *BLANKS       CH$CUO            8
     c                   Eval      CH$CUO=%subst(line:110:006)+
     c                                    %subst(line:117:002)
     c                   Move      CH$CUO        AF$CUO
     c*
     c                   Move      *BLANKS       CHIPRC            6
     c                   Eval      CHIPRC=%subst(line:119:006)
     c                   Move      CHIPRC        AFIPRC
     c*
     c                   Move      *BLANKS       CHINCE            9
     c                   Eval      CHINCE=%subst(line:125:009)
     c                   Move      CHINCE        AFINCE
     c*
     c                   Z-ADD     AFINCE        AFICHE
     c*
     c                   Move      *BLANKS       CHFECH           16
     c                   Move      *BLANKS       CHFAU1            8
     c                   Move      *BLANKS       CHHAU1            6
     c                   Eval      CHFECH=%subst(line:134:016)
     C                   Eval      CHFAU1=%subst(CHFECH:7:4)+
     c                                    %subst(CHFECH:4:2)+
     c                                    %subst(CHFECH:1:2)
     C                   Eval      CHHAU1=%subst(CHFECH:12:2)+
     c                                    %subst(CHFECH:15:2)+ '00'
     c                   Move      CHFAU1        AFFAU1
     c                   Move      CHHAU1        AFHAU1
     c*
     c                   Eval      AFESTA=%subst(line:150:001)
     c*
     c                   Eval      CHFECH=%subst(line:134:010)
     c                   Move      *BLANKS       CHFE01            8
     C                   Eval      CHFE01=%subst(CHFECH:7:4)+
     c                                    %subst(CHFECH:4:2)+
     c                                    %subst(CHFECH:1:2)
     c                   Move      CHFE01        AFFE01
     c*
     c                   Move      *BLANKS       CH$SRD            8
     c                   Eval      CH$SRD=%subst(line:163:006)+
     c                                    %subst(line:170:002)
     c                   Move      CH$SRD        AF$SRD
     c*
     c                   Eval      AFAMRC=%subst(line:172:050)
     c*
     c                   Movel     AFIND1        NROOPE
     c*
     c                   Move      CHISUC        AFISUC
     c                   Move      CHINCR        AFINCR
     c                   Move      CHIDEG        AFIDEG
     c*
     c                   Write     REPRAFED
     c*
+----c                   Eval      ec=readline(fdi: %addr(line): %size(line))
+----C                   ENDDo
     c* .... Cerrar los archivos
     c                   CallP     closef(fdi)
     c*
     c                   Move      AASFEI        chsfei            8
     c                   Time                    wwhora            6 0
     c                   move      wwhora        chhora            6
     c                   Eval      newname=%trim(file_name)+'_'+%trim(@puser)+
     c                             '_'+chsfei+'_'+chhora
     c                   Eval      CmdStr='MOVE OBJ('''+%TRIM(path_f)+
     c                             ''') TOOBJ(''/home/Cesion_de_Haberes/'+
     c                             'Federal/Procesados/'+%trim(newname)+''')'
     C                   Eval      rc=Shell(CmdStr)
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     CheckStruct   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Check si existe el dir ANSES
     c                   Eval      path='/home/Cesion_de_haberes/Federal/'+
     c                                  'Recibidos'+x'00'
+----c                   If        Access(%addr(path): F_OK) < 0
|+---c                   if        errno = ENOENT
||   c                   eval      errtxt='No Existe el Dir.:'+path
||   c                   ExSr      IFSError
|+---C                   EndIf
|+---c                   if        errno = EACCES
||   c                   eval      errtxt='Usuario sin permiso al Dir.:'+path
||   c                   ExSr      IFSError
|+---C                   EndIf
+----c                   EndIf
     C*
     C* ... Check si existe el dir, lo creamos
     C*
     c                   Eval      path='/home/Cesion_de_haberes'+x'00'
     c                   ExSr      EnsureDir
     c                   Eval      path='/home/Cesion_de_haberes/Federal'+x'00'
     c                   ExSr      EnsureDir
     c                   Eval      path='/home/Cesion_de_haberes/Federal/'+
     c                                  '/Recibidos'+x'00'
     c                   ExSr      EnsureDir
     C*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     EnsureDir     BegSr
     c*-------------------------------------------------------------------------
     c*
+----c                   If        Access(%ADDR(path): F_OK) < 0
|    c                   eval      err= errno
|    c                   eval      errmsg= %str(strerror(errno))
|+---c                   if        err = ENOENT
||+--c                   if        mkdir(%addr(path): 0) < 0
|||  c                   eval      errtxt='Dir.:'+%trim(path)+' inexistente'+
|||  c                                    ' Y no pudo ser creado.'
|||  c                   ExSr      IFSError
||+--C                   EndIf
|+---C                   EndIf
+----C                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     Abort         BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   Eval      errtxt='FATAL:'+%trim(errtxt)
     c                   ExSr      DspErr
     c                   ExSr      EndPgm
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     IFSError      BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   eval      err= errno
     c                   eval      errmsg= %str(strerror(errno))
     c                   eval      err= errno
     c                   eval      errtxt=%trim(errtxt)+' Causa:'+errmsg
     c                   ExSr      Abort
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     c     OpenInputF    BegSr
     C*-------------------------------------------------------------------------
     c*
     c                   Eval      path_from='/home/Cesion_de_haberes/Federal/'+
     c                                  '/Recibidos/'+
     c                                  %trim(file_name)+x'00'
     c                   Eval      path_f='/home/Cesion_de_haberes/Federal/'+
     c                                  '/Recibidos/'+
     c                                  %trim(file_name)
     c                   Eval      fdi = open(%addr(path_from): O_RDONLY +
     c                                        O_TEXTDATA     )
+----c                   If        fdi  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----C                   EndIf
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
     C* DSPERR: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C                   CALL      'BAER00RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     C                   ENDSR
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
