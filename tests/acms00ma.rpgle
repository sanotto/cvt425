*SRCMBRTXT:Manejador proceso de mensajes para ATM 
     H*MENSAJES LINK POR ATM, SE INFORMA A CLIENTES SOBRE MOV. EN BOLSA
     H*GENERA ARCHIVOS CAV Y TADI PARA ENVIAR A LINK
     H*PR00586
     H*
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     F*----------------------------------------------------------------*
     F*ESTAH6    IF   E           K DISK
     FESTAH606  UF   E           K DISK
     FSGSYSV    IF   E           K DISK
     FLIKMTR01  UF   E           K DISK
     F@CPIUSD   IF   E           K DISK
     D*-------------------------------------------------------------------------
     D*Aca estan las definiciones de Prototipos para llamar a las API del IFS
     D*-------------------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
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
     C*    *LOVAL        SETLL     REESTAH6
     C     @KEY03        CHAIN     ESTAH606                           25
     C                   DOW       *IN25 = *OFF
     c                   z-add     e6iccl        wwiccc           11 0
     C* PROC4ESO
     c                   ExSr      ChkCta
     c                   exsr      WrtLine
     C                   Z-ADD     *zero         e6faam
     C                   Z-ADD     *zero         e6inin
     C                   UPDATE    REESTAH6
     C                   READ      REESTAH6                               25
     C                   ENDDO
     c   25              ExSr      PrtFoot
     c   25              exsr      PrtFilCnt
     c   25              exsr      DelReg
     C   25              SETON                                        LR
     C*-------------------------------------------------------------------------
     c     *INZSR        BegSr
     C*-------------------------------------------------------------------------
     C*
     C* ... Abrir Archivo de Entrada
     c*PRIMER PASO
     C*
     C* KLIST PARA ACCESO A LIKMTR
     C*
     C     @KEY02        KLIST
     C                   KFLD                    E6ISUC
     C                   KFLD                    WWICCC
     C*
     C     @KEY03        KLIST
     C                   KFLD                    WWFAAM
     C                   KFLD                    WWININ
     C*
     C* ... Abrir Archivo de Salida
     c                   ExSr      OpenOutputF
     c*
     c* Contadores para cantidad de registros
     c* Se inicializan con 2 para considerar cabecera y pie del archivo
     c*
     c                   z-add     2             count
     c                   z-add     *zeros        countt           15 0
     c                   move      'AC'          WWISUB            2
     c                   z-add     999999        WWFAAM            6 0
     c                   z-add     999999        WWININ           15 0
     c*
     c*
     C*//Escribo cabecera del archivo
     c*
     C                   CALL      'ACMS00RG'
     C                   CALL      'ACMS00R1'
     C                   CALL      'ACMS00R2'
     C                   CALL      'ACMS00R3'
     C                   CALL      'ACMS00R4'
     c*                  exsr      PrtHead
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     DelReg        BegSr
     C*-------------------------------------------------------------------------
     c* Elimino registros en ESTAH6
     c*
     c*    @key03        chain     ESTAH6                               90
     c*    *in90         doweq     '0'
     c*    @pjobn        ifeq      E6IJOB
     c*                  delete    REESTAH6
     c*                  endif
     c*    @key03        chain     ESTAH6                             90
     c*                  enddo
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     ChkCta        BegSr
     C*-------------------------------------------------------------------------
     C* ... Leer datos y escribir datos
     C*    @KEY03        CHAIN     ESTAH6                             97
     c*                  if        *in97 = '1'
     c*                  move      'AC'          E6ISUB
     c*                  z-add     INISUC        E6ISUC
     c*                  z-add     INICAH        E6ICCL
     c*                  z-add     @pjobn        E6IJOB
     c*                  write     REESTAH6
     C*                  EndIf
     c
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtLine       BegSr
     C*-------------------------------------------------------------------------
     C                   Eval      buf=*blanks
     C     @KEY02        CHAIN     LIKMTR01                           98
+----C                   if        *in98 = '0'
|+---c                   if        TAFBAJ = 0 or TAFEBL = 0
||   c                   add       1             count
||   c                   exsr      PrtBody
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
     c* writead: Imprime registros de Cabecera
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
     c*CODIGO DE MENSAJES
     C                   Eval      %subst(buf:022:006)=%TRIM(
     C                                                %EDITW(
     C                                           E6DTEM:'      '))
     C*
     C*DIAS DE ESPERA PARA VOLVER  MOSTRAR EL MENSAJE 01
     C*CANTIDAD DE VECES QUE SE REPITE PARA EL MISMO CLIENTE 03
     c                   Eval      %subst(buf:028:004)='0103'
     C*
     C*FECHA DE VIGENCIA DE LA ACCION YY/MM/DD
     C*PARA DEUDA EN BOLSA -> 13/06/04
     c                   Eval      %subst(buf:032:006)='130604'
     C*
     C*FECHA DE FINALIZACION DE LA ACCION YY/MM/DD
     C*LOS PARAMETROS DE FECHA ESTAN CARGADOS EN EL PANEL WEB
     C*HABLAR CON NELSON PINEDA
     C*PARA DEUDA EN BOLSA -> 50/12/31
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
     c                                  O_CREAT + O_WRONLY + O_APPEND +
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
