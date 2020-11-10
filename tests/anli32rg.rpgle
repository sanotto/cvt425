*SRCMBRTXT:UVHI - Procesa mensajes de ANSES al for
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     F*----------------------------------------------------------------*
     FSGSYSV    IF   E           K DISK
     F*  The system values file
     FANLIAF02  IF   E           K DISK
     FANLIHA08  IF   E           K DISK
     FANATMS02  UF A E           K DISK
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
     D line            S           1024A
     D msg             S            200A
     D file            S             20A
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D CMDSTR          S            255A   INZ(*BLANKS)
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D**********************************************************************
     c                   ExSr      CheckStruct
     c                   ExSr      ProcDir
     C                   ExSr      SndLotus
     c                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     c     ProcDir       BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   Eval      path='/home/ANSES/UVHI/Entrada'+x'00'
     c                   Eval      p_dir=opendir(%addr(path))
+----c                   If        p_dir = *NULL
|    c                   eval      errtxt='No se pudo leer:'+path+' Causa:'+
|    c                             errmsg
|    c                   ExSr      Abort
+----c                   EndIf
     C*
     c                   Eval      p_dirent = readdir(p_dir)
+----c                   DoW       p_dirent <> *NULL
|    c                   Eval      file_name= %subst(d_name:1:d_namelen)
|    c                   Eval      file_name=%xlate(lo:up:file_name)
|+---c                   if        %subst(file_name:1:4) = 'UVHI'
||   c                   ExSr      GetCodAccion
||   c                   ExSr      ProcessFile
|+---c                   EndIf
|    c                   Eval      p_dirent = readdir(p_dir)
+----c                   EndDo
     C*
     c                   CallP     closedir(p_dir)
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     ProcessFile   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Abrir Archivo de Entrada
     c*PRIMER PASO
     c                   ExSr      OpenInputF
     C* ... Abrir Archivo de Salida
     c                   ExSr      OpenOutputF
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
     c                   z-add     *zeros        count
     c                   exsr      PrtHead
     c                   z-add     *zeros        countt           15 0
+----c                   DOW       readline(fdi: %addr(line): %size(line))>=0
|    C                   Eval      indo=%subst(line:017:025)
|    c                   MOVE      INDO          NDOC
|    c                   MOVE      INDO          DOC
|    c     NDOC          Chain     REANLIAF                           99
|+---c                   If        *IN99 = *OFF and AFFBAJ=0
||   c     DOC           Chain     REANLIHA                           99
||+--c                   If        *IN99 = *OFF
|||  c                   add       1             count
|||  c                   exsr      PrtBody
||+--c                   endif
|+---c                   endif
+----C                   ENDDo
     c                   exsr      PrtFoot
+----c                   If        count = 0
|    c                   Eval      errtxt='No se encontraron registros '       +
|    c                                    'coincidentes en la '                +
|    c                                    'base de clientes. El archivo'       +
|    c                                    ' generado es nulo.'
|    c                   ExSr      Mov2Proc
|    c                   ExSr      IFSError
+----c                   else
|    c                   ExSr      ProcTACA
|    c                   exsr      PrtFilCnt
+----C                   EndIf
     c* aqui proceso del archivo para cargar los mensajes
     c                   exsr      FileMsg
     c* .... Cerrar los archivos
     c                   CallP     closef(fdi)
     c                   CallP     closef(fdo)
     c                   CallP     closef(fdf)
     C* ... Mover Archivo a Procesados, renombrandolo
     c                   ExSr      Mov2Proc
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtFilCnt: Genera refresh de archivos enviados por la entidad
     C*-------------------------------------------------------------------------
     c     PrtFilCnt     BegSr
     c*
     c                   exsr      OpenRefresh
     c* CABECERA
     C                   Eval      %subst(bufer:001:015)='HRCONTROLAVISOS'
     C                   Eval      %subst(bufer:016:004)='0309'
     C                   Eval      %subst(bufer:020:008)='        '
     C                   Eval      %subst(bufer:028:002)=X'0D'+X'25'
     c                   CALLP     write(fdf: %addr(bufer): %size(bufer))
     C*DETALLE
     C                   Eval      %subst(bufer:001:004)='TADI'
     C                   Eval      %subst(bufer:005:015)='TADI03091'+FECHA
     c                   z-add     count         bytes             8 0
     c                   add       2             bytes
     c                   mult      45            bytes
     C                   Eval      %subst(bufer:020:008)=%TRIM(
     C                                                %EDITW(bytes:'0        '))
     C                   Eval      %subst(bufer:028:002)=X'0D'+X'25'
     c                   CALLP     write(fdf: %addr(bufer): %size(bufer))
+----c                   if        countt > 0
|    c                   z-add     *zeros        bytes
|    C                   Eval      %subst(bufer:001:004)='TACA'
|    C                   Eval      %subst(bufer:005:015)='TACA03091'+FECHA
|    c                   z-add     countt        bytes             8 0
|    c                   add       2             bytes
|    c                   mult      31            bytes
|    C                   Eval      %subst(bufer:020:008)=%TRIM(
|    C                                                %EDITW(bytes:'0        '))
|    C                   Eval      %subst(bufer:028:002)=X'0D'+X'25'
|    c                   CALLP     write(fdf: %addr(bufer): %size(bufer))
+----c                   endif
     C*PIE
     C                   Eval      %subst(bufer:001:015)='TRCONTROLAVISOS'
     c                   z-add     count         total             8 0
     c                   add       2             total
+----c                   if        countt > 0
|    c                   add       countt        total
|    c                   add       2             total
+----c                   end
     C                   Eval      %subst(bufer:016:008)=%TRIM(
     C                                                %EDITW(total:'0        '))
     C                   Eval      %subst(bufer:024:004)='    '
     C                   Eval      %subst(bufer:028:002)=X'0D'+X'25'
     c                   CALLP     write(fdf: %addr(bufer): %size(bufer))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtHead: Imprime registros de Cabecera
     C*-------------------------------------------------------------------------
     c     PrtHead       BegSr
     c*
     C                   Eval      %subst(buf:001:017)='HRTARJETASDIRECTA'
     C                   Eval      %subst(buf:018:004)='0309'
     C                   Eval      %subst(buf:022:022)='        '
     C                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody: Imprime registros de detalle
     C*-------------------------------------------------------------------------
     c     PrtBody       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:019)=%TRIM(
     C                                                %EDITW(
     c                                             AFNTAR:'                    '
     C                                                 ))
     c     @key01        chain     reanatms                           99
     c                   Eval      %subst(buf:022:006)=%TRIM(MSIBCF)
     c                   Eval      %subst(buf:028:002)='00'
     c                   Eval      %subst(buf:030:002)='03'
     c                   MOVE      'NI'          PACINV            2
     c                   CALL      'SBBAINFE'
     c                   PARM                    ANFDPA
     c                   PARM                    PACINV
     c                   move      ANFDPA        WWFDPA            6 0
     c                   Eval      %subst(buf:032:006)=%TRIM(%EDITW(
     c                                                   WWFDPA:'      '))
     c                   MOVE      'NI'          PACINV            2
     c                   CALL      'SBBAINFE'
     c                   PARM                    ANFHPA
     c                   PARM                    PACINV
     c                   move      ANFHPA        WWFHPA            6 0
     c                   Eval      %subst(buf:038:006)=%TRIM(%EDITW(
     c                                                   WWFHPA:'      '))
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtFoot: Imprime registros del Pie
     C*-------------------------------------------------------------------------
     c     PrtFoot       BegSr
     c*
     C                   Eval      %subst(buf:001:017)='TRTARJETASDIRECTA'
     C                   Eval      %subst(buf:018:008)=%TRIM(
     C                                                %EDITW(
     c                                                     count:'0        '))
     C                   Eval      %subst(buf:026:018)='                '
     C                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     OpenInputF    BegSr
     C*-------------------------------------------------------------------------
     c*
     c                   Eval      path_from='/home/ANSES/UVHI/Entrada/'+
     c                                     %trim(file_name)+x'00'
     c                   Eval      fdi = open(%addr(path_from): O_RDONLY +
     c                                        O_TEXTDATA     )
+----c                   If        fdi  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----C                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     GetCodAccion  BegSr
     C*-------------------------------------------------------------------------
     c*
     c     @key01        KLIST
     c                   KFLD                    WWFECH
     c                   KFLD                    msg
     c*
     C     *LOVAL        SETLL     SGSYSV
     C                   READ      SGSYSV                                 99
     c                   MOVE      AASFEI        FECHA             6
     c                   Z-ADD     AASFEI        WWFECH            8 0
     C* ... Abrir Archivo de Entrada
     c                   ExSr      OpenInputF
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
     c                   z-add     *zeros        count
+----c                   DoW       readline(fdi: %addr(line): %size(line))>=0
|    C                   Eval      msg=%subst(line:078:200)
|    c     @key01        Chain     REANATMS                           99
|+---c                   If        *IN99 = *On
     C                   move      'AN'          WWCODS            2
     c                   call      'ANLI33RG'
     C                   parm                    WWCODS
     C                   parm                    WWCODG            6
||   c                   z-add     wwfech        MSSFEI
||   c                   move      msg           MSBLK0
     C                   move      wwcodg        MSIBCF
     C                   move      wwcods        MSISUB
||   c                   write     reanatms
||   c     DOC           Chain     REANLIHA                           99
||+--c                   If        *IN99 = *OFF
|||  c                   add       1             count
|||  c                   exsr      PrtBody
||+--c                   endif
|+---c                   endif
+----C                   EndDo
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     OpenRefresh   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Formato Nombre CAVeeeeAAMMDD
     c*     -C         Indica que se trata de un archivo de control
     c*     -AV        indentificador del sistema de avisos
     C*     -eeeee     Entidad 0309
     C*     -V         Nro de Versión en el día (Por si se manda mas de una vez)
     C*     -aammdd    Fecha de Envío
     C*
     c                   EVAL      path_from='/home/ANSES/UVHI/Salida/'+
     c                                     'CAV0309'+FECHA+x'00'
     c                   EVAL      fdf=open(%addr(path_from):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  01252   )
     c                   callp     closef(fdf)
     c                   EVAL      fdf=open(%addr(path_from):
     c                                   O_WRONLY+O_TEXTDATA)
+----c                   If        fdf  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----c                   end
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     OpenOutputF   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Formato Nombre TADIeeeevaammdd
     C*     -eeeee     Entidad 0309
     C*     -V         Nro de Versión en el día (Por si se manda mas de una vez)
     C*     -aammdd    Fecha de Envío
     C*
     C     *LOVAL        SETLL     SGSYSV
     C                   READ      SGSYSV                                 99
     c                   MOVE      AASFEI        FECHA             6
     c                   EVAL      path_from='/home/ANSES/UVHI/Salida/'+
     c                                     'TADI03091'+FECHA+x'00'
     c                   EVAL      fdo=open(%addr(path_from):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  01252   )
     c                   callp     closef(fdo)
     c                   EVAL      fdo=open(%addr(path_from):
     c                                   O_WRONLY+O_TEXTDATA)
+----c                   If        fdo  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----c                   end
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     IFSError      BegSr
     C*
     c                   eval      err= errno
     c                   eval      errmsg= %str(strerror(errno))
     c                   eval      err= errno
     c                   eval      errtxt=%trim(errtxt)+' Causa:'+errmsg
     c                   ExSr      Abort
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     Mov2Proc      BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   move      *date         wndate            8 0
     c                   time                    wntime            6 0
     c                   move      wndate        wcdate            8
     c                   move      wntime        wctime            6
     c                   Eval      path_from='/home/ANSES/UVHI/Entrada/'+
     c                                     %trim(file_name)+x'00'
     c                   Eval      path_to='/home/ANSES/UVHI/Procesados/'+
     c                                     %trim(file_name)+'_'+wcdate+
     c                                     wctime+x'00'
+----c                   if        rename(%addr(path_from): %addr(path_to))< 0
|    c                   eval      errtxt='No se pudo mover:'+path_from
|    c                   ExSr      IFSError
+----c                   EndIf
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* ProcTACA: Genera archivo de tarjetas en campa¦a
     C*-------------------------------------------------------------------------
     c     ProcTACA      BegSr
     c*
     c                   CallP     closef(fdi)
     C* ... Abrir Archivo de Entrada
     c                   ExSr      OpenInputF
     C* ... Formato Nombre TACAeeeeVaammdd
     C*     -eeeee     Entidad 0309
     C*     -V         Nro de Versión en el día (Por si se manda mas de una vez)
     C*     -aammdd    Fecha de Envío
     C*
     C     *LOVAL        SETLL     SGSYSV
     C                   READ      SGSYSV                                 99
     c                   MOVE      AASFEI        FECHA             6
     c                   EVAL      path_from='/home/ANSES/UVHI/Salida/'+
     c                                     'TACA03091'+FECHA+x'00'
     c                   EVAL      fdt=open(%addr(path_from):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  01252   )
     c                   callp     closef(fdt)
     c                   EVAL      fdt=open(%addr(path_from):
     c                                   O_WRONLY+O_TEXTDATA)
+----c                   If        fdt  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----c                   end
     c*
     c* Imprime cabecera del archivo TACA
     c*
     C                   Eval      %subst(buff:001:020)='HRTARJETASENCAMPANIA'
     C                   Eval      %subst(buff:021:004)='0309'
     C                   Eval      %subst(buff:025:005)='NBLR1'
     C                   Eval      %subst(buff:030:002)=X'0D'+X'25'
     c                   CALLP     write(fdt: %addr(buff): %size(buff))
     c*
     c                   z-add     *zeros        countt           15 0
+----c                   DoW       readline(fdi: %addr(line): %size(line))>=0
|    C                   Eval      indo=%subst(line:017:025)
|    c                   MOVE      INDO          NDOC
|    c                   MOVE      INDO          DOC
|    c     NDOC          Chain     REANLIAF                           99
|+---c                   If        *IN99 = *OFF and count < 80
||   c                   add       1             countt
||   c*
||   c* Imprime detalle del archivo TACA
||   c*
||   C                   Eval      %subst(buff:001:002)='TC'
||   C                   Eval      %subst(buff:003:019)=%TRIM(
||   C                                                %EDITW(
||   c                                             AFNTAR:'                  '))
||   C                   Eval      %subst(buff:022:008)='        '
||   C                   Eval      %subst(buff:030:002)=X'0D'+X'25'
||   c                   CALLP     write(fdt: %addr(buff): %size(buff))
||   c*
|+---c                   endif
+----C                   EndDo
     c*
     c* Imprime pie del archivo TACA
     c*
     C                   Eval      %subst(buff:001:020)='TRTARJETASENCAMPANIA'
     C                   Eval      %subst(buff:021:008)=%TRIM(
     C                                              %EDITW(countt:'0        '))
     C                   Eval      %subst(buff:029:001)=' '
     C                   Eval      %subst(buff:030:002)=X'0D'+X'25'
     c                   CALLP     write(fdt: %addr(buff): %size(buff))
     c*
     c                   callp     closef(fdt)
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* FileMsg: Genera archivo con mensajes a transmitir
     C*-------------------------------------------------------------------------
     c     FileMsg       BegSr
     c*
     c                   CallP     closef(fdi)
     C* ... Abrir Archivo de Entrada
     c                   ExSr      OpenInputF
     C* ... Formato Nombre MENSAJESVaammdd
     C*     -V         Nro de Versión en el día (Por si se manda mas de una vez)
     C*     -aammdd    Fecha de Envío
     C*
     C     *LOVAL        SETLL     SGSYSV
     C                   READ      SGSYSV                                 99
     c                   MOVE      AASFEI        FECHA             6
     c                   EVAL      path_from='/home/ANSES/UVHI/Salida/'+
     c                                     'MENSAJES1'+FECHA+x'00'
     c                   EVAL      fdm=open(%addr(path_from):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  01252   )
     c                   callp     closef(fdm)
     c                   EVAL      fdt=open(%addr(path_from):
     c                                   O_WRONLY+O_TEXTDATA)
+----c                   If        fdm  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----c                   end
     c*
     c* Imprime numero de documento, nombre de la persona, numero de tarjeta
     c*  y  mensaje
     c*
     c                   z-add     *zeros        countt           15 0
+----c                   DoW       readline(fdi: %addr(line): %size(line))>=0
|    C                   Eval      indo=%subst(line:017:008)
|    C                   Eval      deno=%subst(line:025:050)
|    C                   Eval      mesg=%subst(line:078:115)
|    c                   MOVE      INDO          NDOC
|    c                   MOVE      INDO          DOC
|    c     NDOC          Chain     REANLIAF                           99
|+---c                   If        *IN99 = *OFF and count < 80
||   c                   add       1             countt
||   c*
||   c* Imprime detalle del archivo TACA
||   c*
||   C                   Eval      %subst(buf1:001:008)=indo
||   C                   Eval      %subst(buf1:009:050)=deno
||   C                   Eval      %subst(buf1:059:018)=%TRIM(
||   C                                                %EDITW(
||   c                                             AFNTAR:'                  '))
||   C                   Eval      %subst(buf1:077:114)=mesg
||   C                   Eval      %subst(buf1:191:002)=X'0D'+X'25'
||   c                   CALLP     write(fdm: %addr(buf1): %size(buf1))
||   c*
|+---c                   endif
+----C                   EndDo
     c*
     c                   callp     closef(fdm)
     c*
     c                   EndSr
     c*
     c     CheckStruct   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Check si existe el dir ANSES
     c                   Eval      path='/home/ANSES'+x'00'
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
     C* ... Check si existe el dir ANSES/UVHI, si no lo creamos
     C*
     c                   Eval      path='/home/ANSES/UVHI'+x'00'
     c                   ExSr      EnsureDir
     c                   Eval      path='/home/ANSES/UVHI/Entrada'+x'00'
     c                   ExSr      EnsureDir
     c                   Eval      path='/home/ANSES/UVHI/Salida'+x'00'
     c                   ExSr      EnsureDir
     c                   Eval      path='/home/ANSES/UVHI/Procesados'+x'00'
     c                   ExSr      EnsureDir
     C*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c     EnsureDir     BegSr
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
     c     SndLotus      BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   call      'ANLI32CL'
     C*
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
     D   Command                   1024A   VALUE
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
