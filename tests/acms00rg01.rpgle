*SRCMBRTXT:'Procesa mensajes para codigos de bolsa
     H*MENSAJES LINK POR ATM, SE INFORMA A CLIENTES SOBRE MOV. EN BOLSA
     H*GENERA ARCHIVOS CAV Y TADI PARA ENVIAR A LINK
     H*PR00525 | PR00586
     H*
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     F*----------------------------------------------------------------*
     FSGSYSV    IF   E           K DISK
     F*  The system values file
     FACMOVB    IP   E             DISK
     FLIKMTR01  IF   E           K DISK
     FESTAH6    UF A E           K DISK
     FACCODI    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FACCTAC    IF   E           K DISK
     F@CPIUSD   IF   E           K DISK
     FBADCCL06  IF   E           K DISK
     FQSYSPRT   O    F  255        PRINTER USROPN
     D*-------------------------------------------------------------------------
     D*Aca estan las definiciones de Prototipos para llamar a las API del IFS
     D*-------------------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
     D*-------------------------------------------------------------------------
     D/COPY SDB02.SRC/QRPGSRC,IFSIO_H
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
     D mess            S              6A
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
     c     @pjobn        chain     @cpiusd                            90
     c                   seton                                        96
     C                   z-add     INICAH        WWICAH            9 0
     c     INIMCA        Chain     REACCODI                           99
+----C                   If        FXITMO = 1
|+---c                   if        INIMCA = 56 or INIMCA = 44 or INIMCA = 63
||   c                             or INIMCA = 8 or INIMCA = 58 or
||   C                             INIMCA = 128
||   c                   ExSr      ChkCta
||   c   97              ExSr      WrtLine
|+---c                   EndIf
+----c                   EndIf
     c                   move      '0'           PAERRO            1
     clr                 Exsr      CalACCT
     clr                 Exsr      msganses
     clr                 ExSr      PrtFoot
     clr                 exsr      PrtFilCnt
     clr                 exsr      DelReg
     C*-------------------------------------------------------------------------
     c     *INZSR        BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Abrir Archivo de Entrada
     c*PRIMER PASO
     C*
     C* KLIST PARA ACCESO A LIKMTR
     C*
     c     @key01        KLIST
     c                   KFLD                    WWITTL
     c                   KFLD                    msg
     C*
     C     @KEY02        KLIST
     C                   KFLD                    INISUC
     C                   KFLD                    INICAH
     C*
     C     @KEY03        KLIST
     C                   KFLD                    WWISUB
     C                   KFLD                    INISUC
     C                   KFLD                    WWICAH
     C*
     c     @key04        KLIST
     c                   KFLD                    NDOC
     c                   KFLD                    WWITT2
     C*
     c                   Z-ADD     *zero         WWITTL            8 0
     c                   Z-ADD     20            WWITT2            2 0
     C* ... Abrir Archivo de Salida
     c                   ExSr      OpenOutputF
     c*
     c* Contadores para cantidad de registros
     c* Se inicializan con 2 para considerar cabecera y pie del archivo
     c*
     c                   z-add     2             count
     c                   z-add     *zeros        countt           15 0
     c                   move      'AC'          WWISUB            2
     c*
     c*
     C*//Escribo cabecera del archivo
     c*
     c                   exsr      PrtHead
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     DelReg        BegSr
     C*-------------------------------------------------------------------------
     c* Elimino registros en ESTAH6
     c*
     c     @key03        chain     ESTAH6                             90
+----c     *in90         doweq     '0'
|+---c     @pjobn        ifeq      E6IJOB
||   c                   delete    REESTAH6
|+---c                   endif
|    c     @key03        reade     ESTAH6                                 90
+----c                   enddo
     c
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     ChkCta        BegSr
     C*-------------------------------------------------------------------------
     C* ... Leer datos y escribir datos
     C     @KEY03        CHAIN     ESTAH6                             97
+----c                   if        *in97 = '1'
|    c                   move      'AC'          E6ISUB
|    c                   z-add     INISUC        E6ISUC
|    c                   z-add     INICAH        E6ICCL
|    c                   z-add     @pjobn        E6IJOB
|    c                   write     REESTAH6
+----C                   EndIf
     c
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtLine       BegSr
     C*-------------------------------------------------------------------------
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
     C     @KEY02        CHAIN     LIKMTR01                           98
+----C                   if        *in98 = '0'
|+---c                   if        TAFBAJ = 0 or TAFEBL = 0
||   c                   exsr      PrtBody
||   c                   add       1             count
|+---C                   EndIf
+----C                   EndIf
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtLine2      BegSr
     C*-------------------------------------------------------------------------
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
     C     @KEY02        CHAIN     LIKMTR01                           98
+----C                   if        *in98 = '0'
|+---c                   if        TAFBAJ = 0 or TAFEBL = 0
||   c                   Z-ADD     *zero         WWFVEN            8 0
||   c                   eval      WWFVEN = AASFEI - 5
||+--c                   if        TAFAAL <= wwfven or TAFAAL = 0
|||  c                   exsr      PrtBody2
|||  c                   add       1             count
||+--C                   EndIf
|+---C                   EndIf
+----C                   EndIf
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
     c                   mult      45            bytes
     C                   Eval      %subst(bufer:020:008)=%TRIM(
     C                                                %EDITW(bytes:'0        '))
     C                   Eval      %subst(bufer:028:002)=X'0D'+X'25'
     c                   CALLP     write(fdf: %addr(bufer): %size(bufer))
     C*PIE
     C                   Eval      %subst(bufer:001:015)='TRCONTROLAVISOS'
     c                   z-add     3             total             8 0
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
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     C                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c*CODIGO DE MENSAJE MSG004 certificado de supervivencia
     c                   Eval      %subst(buf:022:006)='MSG004'
     C*
     C*DIAS DE ESPERA PARA VOLVER  MOSTRAR EL MENSAJE 01
     C*CANTIDAD DE VECES QUE SE REPITE PARA EL MISMO CLIENTE 03
     c                   Eval      %subst(buf:028:004)='0103'
     C*
     C*FECHA DE VIGENCIA DE LA ACCION YY/MM/DD 13/06/04
     c                   Eval      %subst(buf:032:006)='130604'
     C*
     C*FECHA DE FINALIZACION DE LA ACCION YY/MM/DD 50/12/31
     C*LOS PARAMETROS DE FECHA ESTAN CARGADOS EN EL PANEL WEB
     C*HABLAR CON NELSON PINEDA
     c                   Eval      %subst(buf:038:006)='501231'
     C*
     C*FIN DE LINEA Y SALTO
     C                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody: Imprime registros de detalle
     C*-------------------------------------------------------------------------
     c     PrtBody2      BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     C                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c*CODIGO DE MENSAJE MSG006 deuda en bolsa
     c                   Eval      %subst(buf:022:006)='MSG006'
     C*
     C*DIAS DE ESPERA PARA VOLVER  MOSTRAR EL MENSAJE 01
     C*CANTIDAD DE VECES QUE SE REPITE PARA EL MISMO CLIENTE 03
     c                   Eval      %subst(buf:028:004)='0103'
     C*
     C*FECHA DE VIGENCIA DE LA ACCION YY/MM/DD 25/11/16
     c                   Eval      %subst(buf:032:006)='161125'
     C*
     C*FECHA DE FINALIZACION DE LA ACCION YY/MM/DD 50/12/31
     C*LOS PARAMETROS DE FECHA ESTAN CARGADOS EN EL PANEL WEB
     C*HABLAR CON NELSON PINEDA
     c                   Eval      %subst(buf:038:006)='501231'
     C*
     C*FIN DE LINEA Y SALTO
     C                   Eval      %subst(buf:044:002)=X'0D'+X'25'
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
     c                   EVAL      path_from='/home/LINKBEE/REFRESH/'+
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
     c                   MOVE      AAFS24        FECHA             6
     c                   EVAL      path_from='/home/LINKBEE/REFRESH/'+
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
     c*-------------------------------------------------------------------------
     C*
     c                   eval      err= errno
     c                   eval      errmsg= %str(strerror(errno))
     c                   eval      err= errno
     c                   eval      errtxt=%trim(errtxt)+' Causa:'+errmsg
     c                   ExSr      Abort
     C*
     C                   EndSr
     c*-------------------------------------------------------------------------
     c     Abort         BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   Eval      errtxt='FATAL:'+%trim(errtxt)
     c                   ExSr      DspErr
     c                   ExSr      EndPgm
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
     c* CalACCT: Llama a programa para buscar en ACCTAC
     C*-------------------------------------------------------------------------
     c     CalACCT       BegSr
     c*
     c*Mensaje de deuda en bolsa, caduca el 31/12/2050
+----c     20501231      ifgt      AASFEI
|    c     *LOVAL        SETLL     REACCTAC
|+---C                   DOW       *IN25 = *OFF
||+--c                   if        FUFBAJ = 0
|||  c                             and FUISGC <> 'IN'
|||  c                             and FUIUCA = 'S'
|||  c                             and FUIGRC = '04'
|||  c                   z-add     FUISUC        INISUC
|||  c                   z-add     FUICAH        INICAH
|||  c                   z-add     FUICAH        WWICAH
|||  c                   call      'ANBA00R1'
|||  c                   parm                    INISUC
|||  c                   parm                    WWICAH
|||  c                   parm                    AASFEI
|||  c                   parm                    PAERRO
|||+-c                   if        PAERRO = '1'
|||| c                   exsr      ChkCta
|||| c                   exsr      WrtLine2
|||+-c                   end
||+--c                   end
||   C                   READ      REACCTAC                               25
|+---C                   ENDDO
+----c                   endif
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* msganses Imprime mensajes anses
     C*-------------------------------------------------------------------------
     c     msganses      BegSr
     c*
     c*                  ExSr      CheckStruct
     c                   ExSr      ProcDir
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C* DSPERR: Mostrar Mensaje de Error
     C*-------------------------------------------------------------------------
     C     DSPERR        BEGSR
     C*
     C                   OPEN      QSYSPRT
     C                   EXCEPT    E01
     C                   CLOSE     QSYSPRT
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     CheckStruct   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Check si existe el dir ANSES
     c                   Eval      path='/home/LINKBEE/msg'+x'00'
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
     C* ... Check si existe el dir /home/LINKBEE/msg si no lo creamos
     C*
     c                   Eval      path='/home/LINKBEE/msg'+x'00'
     c                   ExSr      EnsureDir
     c                   Eval      path='/home/LINKBEE/Procesados'+x'00'
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
     c     ProcDir       BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   Eval      path='/home/LINKBEE/msg/'+x'00'
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
|+---c                   if        %subst(file_name:1:7) = 'MSG.TXT'
||   c                   ExSr      ProcessFile
|+---c                   EndIf
|    c                   Eval      p_dirent = readdir(p_dir)
+----c                   EndDo
     C*
     c                   CallP     closedir(p_dir)
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c     OpenInputF    BegSr
     C*-------------------------------------------------------------------------
     c*
     c                   Eval      path_from='/home/LINKBEE/msg/'+
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
     c     ProcessFile   BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Abrir Archivo de Entrada
     c*PRIMER PASO
     c                   ExSr      OpenInputF
     C* ... Leer datos y escribir datos
     C                   Eval      buf=*blanks
     c                   z-add     *zeros        countt           15 0
     c                   Z-ADD     1             WWITT2
+----c                   DOW       readline(fdi: %addr(line): %size(line))>=0
|    C                   Eval      mess=%subst(line:001:006)
|    c* Imprime codigo de mensaje MSG011 - Ben. Rep. Historica
|+---c     20190103      ifgt      AASFEI
||+--c                   if        mess = 'MSG011'
|||  C                   Eval      indo=%subst(line:037:008)
|||  c                   exsr      srextarj
|||+-c     wwntar        ifne      *zero
|||| c                   exsr      PrtBod5
|||| c                   z-add     *zero         wwntar
|||+-c                   endif
||+--c                   endif
|+---c                   endif
|    c* Imprime codigo de mensaje MSG012 - Hom. Rep. Historica
|+---c     20190112      ifgt      AASFEI
||+--c                   if        mess = 'MSG012'
|||  C                   Eval      indo=%subst(line:037:008)
|||  c                   exsr      srextarj
|||+-c     wwntar        ifne      *zero
|||| c                   exsr      PrtBod6
|||| c                   z-add     *zero         wwntar
|||+-c                   endif
||+--c                   endif
|+---c                   endif
|    c* Imprime codigo de mensaje MSG013 - UVHI pres. doc. 2014
|+---c     20170713      ifgt      AASFEI
||+--c                   if        mess = 'MSG013'
|||  C                   Eval      indo=%subst(line:037:008)
|||  c                   exsr      srextarj
|||+-c     wwntar        ifne      *zero
|||| c                   exsr      PrtBod7
|||| c                   z-add     *zero         wwntar
|||+-c                   endif
||+--c                   endif
|+---c                   endif
|    c* Imprime codigo de mensaje MSG014 - UVHI pres. doc. 2015
|+---c     20170713      ifgt      AASFEI
||+--c                   if        mess = 'MSG014'
|||  C                   Eval      indo=%subst(line:037:008)
|||  c                   exsr      srextarj
|||+-c     wwntar        ifne      *zero
|||| c                   exsr      PrtBod8
|||| c                   z-add     *zero         wwntar
|||+-c                   endif
||+--c                   endif
|+---c                   endif
|    c* Imprime codigo de mensaje MSG019 - ANSES REP. HISTORICA PREVI
|+---c     20170510      ifgt      AASFEI
||+--c                   if        mess = 'MSG019'
|||  C                   Eval      indo=%subst(line:037:008)
|||  c                   exsr      srextarj
|||+-c     wwntar        ifne      *zero
|||| c                   exsr      PrtBod13
|||| c                   z-add     *zero         wwntar
|||+-c                   endif
||+--c                   endif
|+---c                   endif
|    c* Imprime codigo de mensaje MSG020 - ANSES REP. HISTORICA PREVI
|+---c     20170612      ifgt      AASFEI
||+--c                   if        mess = 'MSG020'
|||  C                   Eval      indo=%subst(line:037:008)
|||  c                   exsr      srextarj
|||+-c     wwntar        ifne      *zero
|||| c                   exsr      PrtBod14
|||| c                   z-add     *zero         wwntar
|||+-c                   endif
||+--c                   endif
|+---c                   endif
+----C                   ENDDo
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     Mov2Proc      BegSr
     C*-------------------------------------------------------------------------
     C*
     c                   move      *date         wndate            8 0
     c                   time                    wntime            6 0
     c                   move      wndate        wcdate            8
     c                   move      wntime        wctime            6
     c                   Eval      path_from='/home/LINKBEE/msg/'+
     c                                     %trim(file_name)+x'00'
     c                   Eval      path_to='/home/LINKBEE/msg/Procesados/'+
     c                                     %trim(file_name)+'_'+wcdate+
     c                                     wctime+x'00'
+----c                   if        rename(%addr(path_from): %addr(path_to))< 0
|    c                   eval      errtxt='No se pudo mover:'+path_from
|    c                   ExSr      IFSError
+----c                   EndIf
     C*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody3: Imprime registros de detalle para mensaje 009
     C*-------------------------------------------------------------------------
     c     PrtBod3       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG009'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='150917'
     c                   Eval      %subst(buf:038:006)='160917'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody4: Imprime registros de detalle para mensaje 010
     C*-------------------------------------------------------------------------
     c     PrtBod4       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG010'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='160119'
     c                   Eval      %subst(buf:038:006)='170119'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody5: Imprime registros de detalle para mensaje 011
     C*-------------------------------------------------------------------------
     c     PrtBod5       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG011'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='030117'
     c                   Eval      %subst(buf:038:006)='030119'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody6: Imprime registros de detalle para mensaje 012
     C*-------------------------------------------------------------------------
     c     PrtBod6       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG012'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170112'
     c                   Eval      %subst(buf:038:006)='190112'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody7: Imprime registros de detalle para mensaje 013
     C*-------------------------------------------------------------------------
     c     PrtBod7       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG013'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170113'
     c                   Eval      %subst(buf:038:006)='170713'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody8: Imprime registros de detalle para mensaje 014
     C*-------------------------------------------------------------------------
     c     PrtBod8       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG014'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170113'
     c                   Eval      %subst(buf:038:006)='170713'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody9: Imprime registros de detalle para mensaje 015
     C*-------------------------------------------------------------------------
     c     PrtBod9       BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG015'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170301'
     c                   Eval      %subst(buf:038:006)='170331'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody10:Imprime registros de detalle para mensaje 016
     C*-------------------------------------------------------------------------
     c     PrtBod10      BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG016'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170301'
     c                   Eval      %subst(buf:038:006)='170331'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody11:Imprime registros de detalle para mensaje 017
     C*-------------------------------------------------------------------------
     c     PrtBod11      BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG017'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170301'
     c                   Eval      %subst(buf:038:006)='170331'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody12:Imprime registros de detalle para mensaje 017
     C*-------------------------------------------------------------------------
     c     PrtBod12      BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG018'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170301'
     c                   Eval      %subst(buf:038:006)='170331'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody13:Imprime registros de detalle para mensaje 019
     C*-------------------------------------------------------------------------
     c     PrtBod13      BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG019'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170401'
     c                   Eval      %subst(buf:038:006)='170510'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* PrtBody14:Imprime registros de detalle para mensaje 019
     C*-------------------------------------------------------------------------
     c     PrtBod14      BegSr
     c*
     c                   Eval      %subst(buf:001:002)='TD'
     C                   Eval      %subst(buf:003:016)=%TRIM(
     C                                                %EDITW(
     c                                           TANTAR:'                '))
     C                   Eval      %subst(buf:019:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIVIN:'0 '))
     C                   Eval      %subst(buf:020:001)=%TRIM(
     C                                                %EDITW(
     C                                           TAIDIG:'0 '))
     c                   Eval      %subst(buf:022:006)='MSG020'
     c                   Eval      %subst(buf:028:002)='01'
     c                   Eval      %subst(buf:030:002)='10'
     c                   MOVE      'NI'          PACINV            2
     c                   Eval      %subst(buf:032:006)='170501'
     c                   Eval      %subst(buf:038:006)='170612'
     c                   Eval      %subst(buf:044:002)=X'0D'+X'25'
     c                   CALLP     write(fdo: %addr(buf): %size(buf))
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c* srextarj: Obtiene numero de tarjeta dedebito
     C*-------------------------------------------------------------------------
     c     srextarj      BegSr
     c*
     c                   MOVE      INDO          NDOC
     c                   MOVE      INDO          DOC
     c     @KEY04        Chain     REBADCCL                           99
+----c                   If        *IN99 = *OFF
|    c                   Z-ADD     OTISUC        INISUC
|    c                   Z-ADD     OTICCL        INICAH
|    C     @KEY02        CHAIN     LIKMTR01                           99
|+---c                   If        *IN99 = *OFF and TANTAR <> WWNTAR
||   c                   add       1             count
||   C                   move      TANTAR        WWNTAR           16 0
|+---c                   endif
+----c                   endif
     c*
     c                   EndSr
     c*
     OQSYSPRT   E            E01               1  1
     O                       ERRTXT
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
     P ReadLine        E
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
