*SRCMBRTXT:Rechazar Registros Seleccionados-A Supe
     H*  SYSTEM NAME: SIDEBA - CAJA DE AHORRO
     H*
     H*  PROGRAM NAME: QBAC01S4
     H*
     H*  PROGRAM NO: Rechazar Registros Seleccionados - A Supervisar
     H*
     H*  DATE:  03/12/2018
     H*
     H*  AUTHOR: Jorge N. Quintero
     H*
     H*----------------------------------------------------------------*
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     F*----------------------------------------------------------------*
     FQBSUMA01  UF   E             DISK
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   IF   E           K DISK
     FSGSYSV    IF   E           K DISK
     FQBSUEN01  UF   E           K DISK
     FQBSEMP01  IF   E           K DISK
     FQBSPRO01  IF   E           K DISK
     FACEMCE01  IF   E           K DISK
     FBASUPE01  UF   E           K DISK
     D*-------------------------------------------------------------------------
     D*Aca estan las definiciones de Prototipos para llamar a las API del IFS
     D*-------------------------------------------------------------------------
     D               ESDS                  EXTNAME(@PSDS)
     D*-------------------------------------------------------------------------
     D/COPY SDB01.SRC/QRPGSRC,IFSIO_H
     D readline        PR            10I 0
     D  fd                           10I 0 value
     D  text                           *   value
     D  maxlen                       10I 0 value
     D bufer           S            503
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
     DDSICCL           DS
     DDSCDIS                   1      4  0
     DDSIBAN                   5      8  0
     DDSZERO                   9      9  0
     D*-------------------------------------------------------------------------
     D RC              S              7A
     D mess            S              6A
     D indo            S              8A
     D doc             S              8S 0
     D ndoc            S             15S 0
     D count           S              8S 0
     D path            S            255A
     D path_from       S             50A
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
     D file            S             20A
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D CMDSTR          S            255A   INZ(*BLANKS)
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     c**********************************************************************
     c*                    PROGRAMA PRINCIPAL                              *
     c**********************************************************************
     c     WWRRNO        Chain     QBSUMA01                           89
+----c                   If        *In89 = *Off
|    c                   Movel     QZRENG        WWRENG           38
|    c                   ExSr      RechOK
+----c                   EndIf
     c                   ExSr      EndPgm
     c**********************************************************************
     c*OpenOutputF: Abrir Archivo de Salida
     C*-------------------------------------------------------------------------
     c     OpenOutputF   BegSr
     c*
     c* ... Formato Archivo RESUEeeeessssppppppSUElll.TXT
     c*     -RESUE     Prefijo Archivo de Respuesta
     c*     -eeee      Empresa
     c*     -ssss      Sub-empresa                                          vez)
     c*     -pppppp    Periodo
     c*     -SUE       Tipo de Acreditacion
     c*     -lll       N° Lote
     c*     -.TXT      Tipo de Archivo
     c*
     c                   Movel     *Blanks       WRFILE           29
     c                   Movel     QZRENG        WRFILE
     C                   Eval      %Subst(WRFILE:001:002)='RE'
     c*
     c                   Eval      path_from='/home/acreditaciones/'+
     c                             %TRIM(WRFILE)
     c                   Eval      fdo=open(%addr(path_from):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  01252   )
     c                   callp     closef(fdo)
     c                   Eval      fdo=open(%addr(path_from):
     c                                   O_WRONLY+O_TEXTDATA)
+----c                   If        fdo  < 0
|    c                   Eval      errtxt='No se pudo abrir:'+path_from
|    c                   ExSr      IFSError
+----c                   End
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*RechOK: Rechazo OK
     c*-------------------------------------------------------------------------
     c     RechOK        BegSr
     c*
     c                   Movel     *Blanks       WWLIN1           55
     c                   Movel     *Blanks       WWLIN2           55
     c                   Movel     *Blanks       WWLIN3           55
     c                   Movel     *Blanks       WWLIN4           55
     c                   Movel     *Blanks       WWLIN5           55
     c                   Movel     *Blanks       WWLIN6           55
     c                   Movel     'SUELDO'      WWESTA           10
     c*
     c                   Eval      WWLIN1='ESTA ACCIÓN RECHAZARÁ EL LOTE'
     c                   Movel     *Blanks       WWLIN2           55
     c     @Key01        Chain     QBSEMP01                           79
+----c                   If        *In79 = *Off
|    c                   Eval      WWLIN3='Empresa: '+
|    C                             %TRIM(QCNRSO)
     c                   Movel     QCNRSO        WWNRSO           30
+----c                   EndIf
     c
     c     @Key02        Chain     QBSPRO01                           78
+----c                   If        *In78 = *Off
|    c                   Eval      WWLIN4='Tpo Acr: '+
|    c                             %TRIM(QSNRSO)
|    c                   Movel     QSNRSO        WSNRSO           30
+----c                   EndIf
     c
     c                   Movel     QZFAAM        WWFAAM            6
     c                   Eval      WWLIN5='Periodo: '+WWFAAM
     c
     c                   Movel     QZIPOS        WWIPOS            3
     c                   Eval      WWLIN6='Nr.Lote: '+WWIPOS
     c
     c                   Call      'ACS901RS'
     c                   Parm                    WWLIN1
     c                   Parm                    WWLIN2
     c                   Parm                    WWLIN3
     c                   Parm                    WWLIN4
     c                   Parm                    WWLIN5
     c                   Parm                    WWLIN6
     c
     c     @Key04        Chain     ACEMCE01                           77
+----c                   If        *In77 = *Off
|    c                   Movel     *Blanks       WWCOEL           50
|    c                   Movel     AACOEL        WWCOEL
+----c                   EndIf
     c
     c     @PJOBN        Chain     @CPIUSD                            76
+----c                   If        @COPEC= 'S'
|+---c                   If        *In76 = *Off
||   c*....Analiza el Rechazo
||+--c                   If        *In88 = *Off
|||  c                   SetOn                                            88
|||  C*....Abrir Archivo de Salida
|||  c                   ExSr      OpenOutputF
||+--c                   EndIf
||   c*....Llama Ventana de Detalle
||   c                   Movel     *Blanks       WWTHEL           60
||   c                   Call      'BABL02SV'
||   c                   Parm                    WWTHEL
||   c     @Key03        Chain     QBSUEN01                           75
||+--c     *In75         Doweq     *Off
|||+-c                   If        ZXIMOV= 1
|||| c                   Z-add     AASFEI        ZXFBAJ
|||| c                   Z-add     WWHORA        ZXHBAJ
|||| c                   Movel     @ZUSER        ZXIUSB
|||| c                   Z-add     5             ZXIMOV
|||| c                   Eval      ZXDF01='LOTE RECHAZADO......'
|||| c                   Movel     *Blanks       ZXDF02
|||| c                   Movel     WWTHEL        ZXDF02
|||| c                   Movel     WWTHEL        WHTHEL           30
|||| c*....Arma Nacha Respuesta de Rechazo
|||| C                   Eval      %Subst(ZXISTR:449:001)='R'
|||| C                   Eval      %Subst(ZXISTR:450:030)=WHTHEL
|||| c                   ExSr      GenRes
|||| c                   Update    REQBSUEN
|||| C*....Genera Archivo Respuesta
|||+-c                   EndIf
|||  c     @Key03        Reade     QBSUEN01                               75
||+--c                   Enddo
     c                   callp     closef(fdo)
||   c*....Envia Correo Externo
||   c                   ExSr      CorExt
||   c*....Envia Correo Interno
||   c                   ExSr      CorInt
||   c*....Cambia Permiso Archivo
||   c                   ExSr      CamPer
||   c
||   c                   Z-add     AASFEI        QZFBAJ
||   c                   Z-add     WWHORA        QZHBAJ
||   c                   Movel     @ZUSER        QZIUSB
||   c                   Z-add     5             QZIMOV
||   c                   Eval      QZDF01='LOTE RECHAZADO......'
||   c                   Movel     *Blanks       QZDF02
||   c                   Movel     WWTHEL        QZDF02
||   c                   Movel     *Blanks       QZINI1
||   c                   Movel     *Blanks       QZINI2
||   c                   Update    REQBSUMA
|+---c                   EndIf
+----c                   EndIf
     c                   Call      'CBX130RG'
     c                   Parm                    WWESTA
     c
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*GenRes: Genera Archivo de Respuesta
     C*-------------------------------------------------------------------------
     c     GenRes        BegSr
     c
     c                   Eval      %subst(bufer:001:500)=ZXISTR
     c                   Eval      %subst(bufer:501:001)='0'
     c                   Eval      %subst(bufer:502:002)=X'0D'+X'25'
     c                   CALLP     write(fdf: %addr(bufer): %size(bufer))
     c
     c                   EndSr
     C*-------------------------------------------------------------------------
     c*CorExt: Envia Correo Externo
     c*-------------------------------------------------------------------------
     c     CorExt        BegSr
     c
     c*....Enviar Usuario Correo
     c                   Movel     *Blanks       USUARI           50
     c                   Movel     WWCOEL        USUARI
     c*....Asunto del Correo
     c                   Movel     *Blanks       ASUNTO           50
     c                   Movel     *Blanks       LOTERE           20
     c                   Eval      ASUNTO='Acr.Sueldo: +
     c                                     LOTE RECHAZADO.................'
     c*....Mensaje del Correo
     c                   Movel     QZCDIS        WWCDIS            4
     c                   Movel     QZIBAN        WWIBAN            4
     c                   Movel     QZIPIS        WWIPIS            3
     c                   Movel     QZIACT        WWIACT            3
     c                   Movel     QZQCA1        WWQCA1           11
     c                   Movel     *Blanks       WW$ENT           13
     c                   Movel     *Blanks       WW$DEC            2
     c                   Movel     QZ$IMP        WW$ENT
     c                   Move      QZ$IMP        WW$DEC
     c                   Movel     *Blanks       MENSAJ          255
     C                   Eval      %Subst(MENSAJ:001:009)='Empresa: '
     C                   Eval      %Subst(MENSAJ:010:004)= WWCDIS
     C                   Eval      %Subst(MENSAJ:014:001)= ' '
     C                   Eval      %Subst(MENSAJ:015:004)= WWIBAN
     C                   Eval      %Subst(MENSAJ:019:001)= ' '
     C                   Eval      %Subst(MENSAJ:020:030)= WWNRSO
     C                   Eval      %Subst(MENSAJ:050:003)= ' - '
     C                   Eval      %Subst(MENSAJ:053:010)='Tipo Acr: '
     C                   Eval      %Subst(MENSAJ:063:003)= WWIPIS
     C                   Eval      %Subst(MENSAJ:066:001)= ' '
     C                   Eval      %Subst(MENSAJ:067:003)= WWIACT
     C                   Eval      %Subst(MENSAJ:070:001)= ' '
     C                   Eval      %Subst(MENSAJ:071:030)= WSNRSO
     C                   Eval      %Subst(MENSAJ:101:003)= ' - '
     C                   Eval      %Subst(MENSAJ:104:009)='Periodo: '
     C                   Eval      %Subst(MENSAJ:113:006)= WWFAAM
     C                   Eval      %Subst(MENSAJ:119:003)= ' - '
     C                   Eval      %Subst(MENSAJ:122:006)= 'Lote: '
     C                   Eval      %Subst(MENSAJ:128:003)= WWIPOS
     C                   Eval      %Subst(MENSAJ:131:003)= ' - '
     C                   Eval      %Subst(MENSAJ:134:006)= 'Cant: '
     C                   Eval      %Subst(MENSAJ:140:011)= WWQCA1
     C                   Eval      %Subst(MENSAJ:151:003)= ' - '
     C                   Eval      %Subst(MENSAJ:155:005)= 'Imp: '
     C                   Eval      %Subst(MENSAJ:160:013)= WW$ENT
     C                   Eval      %Subst(MENSAJ:173:001)= ','
     C                   Eval      %Subst(MENSAJ:174:002)= WW$DEC
     C                   Eval      %Subst(MENSAJ:176:003)= ' - '
     C                   Eval      %Subst(MENSAJ:179:006)= 'Arch: '
     C                   Eval      %Subst(MENSAJ:185:030)= WWRENG
     C                   Eval      %Subst(MENSAJ:215:003)= ' - '
     C                   Eval      %Subst(MENSAJ:218:005)= 'Obs: '
     C                   Eval      %Subst(MENSAJ:223:032)= WWTHEL
     c*....Archivo del Correo
     c                   Movel     *Blanks       ARCHIV          250
     c*....Enviar attach como enlace?
     c                   Movel     '*NO'         ENLACE            4
     c*....Tipo de Correo
     c                   Movel     *Blanks       TIPO              5
     c                   Movel     'GMAIL'       TIPO
     c*....Envia Correo
     c                   Call      'QBAC00G8'
     c                   Parm                    USUARI
     c                   Parm                    ASUNTO
     c                   Parm                    MENSAJ
     c                   Parm                    ARCHIV
     c                   Parm                    ENLACE
     c                   Parm                    TIPO
     c
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*CorInt: Envia Correo Interno
     c*-------------------------------------------------------------------------
     c     CorInt        BegSr
     c
     c*....Enviar Usuario Correo
     c                   Movel     *Blanks       USUARI           50
     c                   Movel     'QBQZ00MB'    USUARI
     c*....Asunto del Correo
     c                   Movel     *Blanks       ASUNTO           50
     c                   Movel     *Blanks       LOTERE           20
     c                   Eval      ASUNTO='Acr.Sueldo: +
     c                                     LOTE RECHAZADO DE SUPERVISION..'
     c*....Mensaje del Correo
     c                   Movel     QZCDIS        WWCDIS            4
     c                   Movel     QZIBAN        WWIBAN            4
     c                   Movel     QZIPIS        WWIPIS            3
     c                   Movel     QZIACT        WWIACT            3
     c                   Movel     QZQCA1        WWQCA1           11
     c                   Movel     *Blanks       WW$ENT           13
     c                   Movel     *Blanks       WW$DEC            2
     c                   Movel     QZ$IMP        WW$ENT
     c                   Move      QZ$IMP        WW$DEC
     c                   Movel     *Blanks       MENSAJ          255
     C                   Eval      %Subst(MENSAJ:001:009)='Empresa: '
     C                   Eval      %Subst(MENSAJ:010:004)= WWCDIS
     C                   Eval      %Subst(MENSAJ:014:001)= ' '
     C                   Eval      %Subst(MENSAJ:015:004)= WWIBAN
     C                   Eval      %Subst(MENSAJ:019:001)= ' '
     C                   Eval      %Subst(MENSAJ:020:030)= WWNRSO
     C                   Eval      %Subst(MENSAJ:050:003)= ' - '
     C                   Eval      %Subst(MENSAJ:053:010)='Tipo Acr: '
     C                   Eval      %Subst(MENSAJ:063:003)= WWIPIS
     C                   Eval      %Subst(MENSAJ:066:001)= ' '
     C                   Eval      %Subst(MENSAJ:067:003)= WWIACT
     C                   Eval      %Subst(MENSAJ:070:001)= ' '
     C                   Eval      %Subst(MENSAJ:071:030)= WSNRSO
     C                   Eval      %Subst(MENSAJ:101:003)= ' - '
     C                   Eval      %Subst(MENSAJ:104:009)='Periodo: '
     C                   Eval      %Subst(MENSAJ:113:006)= WWFAAM
     C                   Eval      %Subst(MENSAJ:119:003)= ' - '
     C                   Eval      %Subst(MENSAJ:122:006)= 'Lote: '
     C                   Eval      %Subst(MENSAJ:128:003)= WWIPOS
     C                   Eval      %Subst(MENSAJ:131:003)= ' - '
     C                   Eval      %Subst(MENSAJ:134:010)= 'Cantidad: '
     C                   Eval      %Subst(MENSAJ:144:011)= WWQCA1
     C                   Eval      %Subst(MENSAJ:155:003)= ' - '
     C                   Eval      %Subst(MENSAJ:158:009)= 'Importe: '
     C                   Eval      %Subst(MENSAJ:167:013)= WW$ENT
     C                   Eval      %Subst(MENSAJ:180:001)= ','
     C                   Eval      %Subst(MENSAJ:181:002)= WW$DEC
     C                   Eval      %Subst(MENSAJ:183:003)= ' - '
     C                   Eval      %Subst(MENSAJ:186:009)= 'Archivo: '
     C                   Eval      %Subst(MENSAJ:195:030)= WWRENG
     C                   Eval      %Subst(MENSAJ:225:003)= ' - '
     C                   Eval      %Subst(MENSAJ:228:013)= 'Observación: '
     C                   Eval      %Subst(MENSAJ:241:014)= WWTHEL
     c*....Archivo del Correo
     c                   Movel     *Blanks       ARCHIV          250
     c*....Enviar attach como enlace?
     c                   Movel     '*NO'         ENLACE            4
     c*....Tipo de Correo
     c                   Movel     *Blanks       TIPO              5
     c                   Movel     'MAIL '       TIPO              5
     c*....Envia Correo Interno
     c                   Call      'QBAC00G8'
     c                   Parm                    USUARI
     c                   Parm                    ASUNTO
     c                   Parm                    MENSAJ
     c                   Parm                    ARCHIV
     c                   Parm                    ENLACE
     c                   Parm                    TIPO
     c
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*CamPer: Cambia Permiso Archivo
     c*-------------------------------------------------------------------------
     c     CamPer        BegSr
     c
     c                   Movel     *Blanks       ARCHIV          250
     c                   Movel     path_from     ARCHIV          250
     c
     c                   Call      'SGAUTACL'
     c                   Parm                    ARCHIV
     c
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*LimSup: Limpia BASUPE
     c*-------------------------------------------------------------------------
     c     LimSup        BegSr
     c
     c                   Z-add     QZFALT        PASFEI            8 0
     c                   Z-add     *Zeros        PAISUC            5 0
     c
     c                   Movel     *Blanks       DSICCL
     c                   Z-add     QZCDIS        DSCDIS
     c                   Z-add     QZIBAN        DSIBAN
     c                   Movel     DSICCL        PAICCL            9 0
     c
     c                   Z-add     QZFAAM        PAINCR           15 0
     c                   Z-add     QZIPOS        PAIDEG            4 0
     c                   Z-add     *Zeros        PAIAPC            2 0
     c                   Z-add     QZIPIS        PAITRN            3 0
     c                   Z-add     QZIACT        PAICAJ            5 0
     c
||   c     @Key05        Chain     BASUPE01                           65
||+--c                   If        *In65 = *Off
     c                   Z-add     *Zeros        SAHSUP
     c                   Movel     *Blanks       SAIUSA
|||| c                   Update    REBASUPE
|||+-c                   EndIf
     c
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*IFSError: Error de IFS
     c*-------------------------------------------------------------------------
     c     IFSError      BegSr
     C*
     c*                   eval      err= errno
     c*                   eval      errmsg= %str(strerror(errno))
     c*                  eval      err= errno
     c                   eval      errtxt=%trim(errtxt)
     c                   ExSr      Abort
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*Abort: Abortar
     c*-------------------------------------------------------------------------
     c     Abort         BegSr
     C*
     c                   Eval      errtxt='FATAL:'+%trim(errtxt)
     c                   ExSr      DspErr
     c                   ExSr      EndPgm
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c* DspErr: Mostrar Mensaje de Error
     c*-------------------------------------------------------------------------
     c     DspErr        BEGSR
     c*
     C                   CALL      'BAER00RS'
     C                   PARM                    WWNCU1
     C                   PARM                    WWNCU2
     C                   PARM                    WWNCU3
     C                   PARM                    WWNCU4
     C                   PARM                    WWNCB1
     C                   PARM                    WWNCB2
     C                   PARM                    WWNCB3
     C                   PARM                    WWNCB4
     c*
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*EndPgm: Fin de Programa
     c*-------------------------------------------------------------------------
     c     EndPgm        BegSr
     c*
     c                   SetOn                                        LR
     c                   Return
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
     c*INZSR: Inicio
     c*-------------------------------------------------------------------------
     c     *INZSR        BegSr
     c*
     c     *LOVAL        Setll     SGSYSV
     c                   Read      SGSYSV                                 99
     c     @PJOBN        Chain     @CPIUSD                            98
     c     @PJOBN        Chain     @CPISYS                            97
     c                   Z-add     @ZRRNO        WWRRNO           11 0
     c                   TIME                    WWHORA            6 0
     c                   SetOff                                           88
     c*....Acceder a QBSEMP01
     c     @key01        KLIST
     c                   KFLD                    QZCDIS
     c                   KFLD                    QZIBAN
     c*....Acceder a QBSPRO01
     c     @key02        KLIST
     c                   KFLD                    QZIPIS
     c                   KFLD                    QZIACT
     c*....Acceder a QBSUEN01
     c     @key03        KLIST
     c                   KFLD                    QZCDIS
     c                   KFLD                    QZIBAN
     c                   KFLD                    QZIPIS
     c                   KFLD                    QZIACT
     c                   KFLD                    QZFAAM
     c                   KFLD                    QZIPOS
     c*....Acceder a ACEMCE01
     c     @key04        KLIST
     c                   KFLD                    QZCDIS
     c                   KFLD                    QZIBAN
     c                   KFLD                    QZIPIS
     c                   KFLD                    QZIACT
     c*....Acceder a BASUPE01
     c     @key05        KLIST
     c                   KFLD                    PASFEI
     c                   KFLD                    PAISUC
     c                   KFLD                    PAICCL
     c                   KFLD                    PAINCR
     c                   KFLD                    PAIDEG
     c                   KFLD                    PAIAPC
     c                   KFLD                    PAITRN
     c                   KFLD                    PAICAJ
     c*
     c                   EndSr
     C*-------------------------------------------------------------------------
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
