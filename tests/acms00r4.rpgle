*SRCMBRTXT:Mensajes por ATM - Certificado de Super
     H*MENSAJES LINK POR ATM, SE INFORMA A CLIENTES SOBRE MOV. EN BOLSA
     H*GENERA ARCHIVOS CAV Y TADI PARA ENVIAR A LINK
     H*PR00586
     H*
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     F*----------------------------------------------------------------*
     F*  The system values file
     FACCTAC    IP   E             DISK
     FESTAH6    UF A E           K DISK
     F@CPIUSD   IF   E           K DISK
     FSGSYSV    IF   E           K DISK
     FANATMS03  IF   E           K DISK
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
     c     @pjobn        chain     @cpiusd                            90
     C                   z-add     FUICAH        WWICAH            9 0
     c                   ExSr      ChkCod
     c                   ExSr      ChkCta
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
     C                   KFLD                    FUISUC
     C                   KFLD                    FUICAH
     C*
     C     @KEY03        KLIST
     C                   KFLD                    WWISUB
     C                   KFLD                    FUISUC
     C                   KFLD                    WWICAH
     C*
     c                   move      'AC'          WWISUB            2
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     DelReg        BegSr
     C*-------------------------------------------------------------------------
     c* Elimino registros en ESTAH6
     c*
     c     @key03        chain     ESTAH6                               90
+----c     *in90         doweq     '0'
|+---c     @pjobn        ifeq      E6IJOB
||   c                   delete    REESTAH6
|+---c                   endif
|    c     @key03        chain     ESTAH6                             90
+----c                   enddo
     c
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     ChkCod        BegSr
     C*-------------------------------------------------------------------------
     C* ... Busca código mensaje ANATMS
     C     *LOVAL        SETLL     SGSYSV
     C                   READ      SGSYSV                                 99
     C                   Z-ADD     3             WWDIGI            1 0
     C     WWDIGI        CHAIN     ANATMS03                           99
     C     *IN99         DOWEQ     *OFF
     C     MSFPRE        IFLE      AASFEI
     C     MSFTOP        ANDGT     AASFEI
     C                   MOVEL     MSIBCF        WWCODI            6
     C                   ENDIF
     C     WWDIGI        READE     ANATMS03                               99
     C                   ENDDO
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     C     ChkCta        BegSr
     C*-------------------------------------------------------------------------
     c* ... Verificar si debe o no registrar la cuenta
     C                   CALL      'ANBA00R1'
     C                   PARM                    FUISUC
     C                   PARM                    WWICAH
     C                   PARM                    AASFEI
     C                   PARM                    PAERRO            1
     c*
+----c                   if        PAERRO= ' '
     C* ... Leer datos y escribir datos
     C     @KEY03        CHAIN     ESTAH6                             97
+----c                   if        *in97 = '1'
|    c                   move      'AC'          E6ISUB
|    c                   z-add     FUISUC        E6ISUC
|    c                   z-add     FUICAH        E6ICCL
|    c                   z-add     @pjobn        E6IJOB
     C                   z-add     999999        E6FAAM
     C                   z-add     999999        E6ININ
|    c                   write     REESTAH6
+----C                   EndIf
+----C                   EndIf
     c
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
