*SRCMBRTXT:Rechazar Registros Seleccionados-Error 
     H*  SYSTEM NAME: SIDEBA - CAJA DE AHORRO
     H*
     H*  PROGRAM NAME: QBAC03S4
     H*
     H*  PROGRAM NO: Rechazar Registros Seleccionados - Error
     H*
     H*  DATE:  13/03/2019
     H*
     H*  AUTHOR: Jorge N. Quintero
     H*
     H*----------------------------------------------------------------*
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     F*----------------------------------------------------------------*
     FQBSUMA03  UF   E             DISK
     F@CPISYS   IF   E           K DISK
     F@CPIUSD   IF   E           K DISK
     FSGSYSV    IF   E           K DISK
     FQBSUEN01  UF   E           K DISK
     FQBSEMP01  IF   E           K DISK
     FQBSPRO01  IF   E           K DISK
     FACEMCE01  IF   E           K DISK
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
     c     WWRRNO        Chain     QBSUMA03                           89
+----c                   If        *In89 = *Off
|+---c                   If        QZFING = *ZEROS
||   c                   ExSr      RechOK
     c                   Else
||   c                   ExSr      RechSu
|+---c                   EndIf
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
|||+-c                   If        ZXIMOV= 3
|||| c                   Z-add     AASFEI        ZXFBAJ
|||| c                   Z-add     WWHORA        ZXHBAJ
|||| c                   Movel     @ZUSER        ZXIUSB
|||| c                   Z-add     5             ZXIMOV
|||| c                   Eval      ZXDF01='LOTE RECHAZADO......'
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
||   c*....Envia Correo Exyerno
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
     c
|||+-c                   If        QZDF01=  *Blanks
||   c                   Eval      QZDF01='LOTE RECHAZADO......'
||   c                   Movel     WWTHEL        QZDF02
     c                   Else
|||+-c                   If        QZDF02<> *Blanks
||   c                   Movel     WWTHEL        QZDF02
     c                   Endif
|+---c                   EndIf
||   c                   Update    REQBSUMA
|+---c                   EndIf
+----c                   EndIf
     c                   Call      'CBX130RG'
     c                   Parm                    WWESTA
     c
     c                   EndSr
     c*-------------------------------------------------------------------------
     c*RechSu: Rechazo Supervision
     c*-------------------------------------------------------------------------
     c     RechSu        BegSr
     c*
     c                   Movel     *Blanks       WWLIN1           55
     c                   Movel     *Blanks       WWLIN2           55
     c                   Movel     *Blanks       WWLIN3           55
     c                   Movel     *Blanks       WWLIN4           55
     c                   Movel     *Blanks       WWLIN5           55
     c                   Movel     *Blanks       WWLIN6           55
     c*
     c                   Eval      WWLIN1='ESTA ACCIÓN RECHAZARÁ EL LOTE'+
     c                                    '            '
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
||+--c                   EndIf
||   c*....Llama Ventana de Detalle
||   c                   Movel     *Blanks       WWTHEL           60
||   c                   Call      'BABL02SV'
||   c                   Parm                    WWTHEL
||   c     @Key03        Chain     QBSUEN01                           75
||+--c     *In75         Doweq     *Off
|||+-c                   If        ZXIMOV= *Zeros
|||| c                   Z-add     *Zeros        ZXFING
|||| c                   Z-add     *Zeros        ZXHFIN
|||| c                   Movel     *Blanks       ZXIUSA
|||| c                   Z-add     1             ZXIMOV
|||| c                   Eval      ZXDF01='LOTE RECHAZADO......'
|||| c                   Movel     WWTHEL        ZXDF02
|||| c                   Movel     WWTHEL        WHTHEL           30
|||| c                   Update    REQBSUEN
|||+-c                   EndIf
|||  c     @Key03        Reade     QBSUEN01                               75
||+--c                   Enddo
||   c*....Envia Correo Interno
||   c                   ExSr      CorInt
||   c
||   c                   Z-add     *Zeros        QZFING
||   c                   Z-add     *Zeros        QZHFIN
||   c                   Movel     *Blanks       QZIUSA
||   c                   Z-add     1             QZIMOV
||   c                   Eval      QZDF01='LOTE RECHAZADO......'
||   c                   Movel     WWTHEL        QZDF02
||   c                   Update    REQBSUMA
|+---c                   EndIf
+----c                   EndIf
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
     c                   Movel     QZQCA1        WWQCA1           11
     c                   Movel     *Blanks       WW$ENT           13
     c                   Movel     *Blanks       WW$DEC            2
     c                   Movel     QZ$IMP        WW$ENT
     c                   Move      QZ$IMP        WW$DEC
     c                   Movel     *Blanks       MENSAJ          250
     c                   Eval      MENSAJ='El Lote: '+
     c                                   WWIPOS +'-Emp: '+
     c                                   WWCDIS +' ' +
     c                                   WWIBAN +' ' +
     c                                   WWNRSO +'-Periodo: '+
     c                                   WWFAAM +'-Cant: '+
     C                                   WWQCA1 +'-Imp: '+
     C                                   WW$ENT +','+
     C                                   WW$DEC +'- '+
     c                                   WWTHEL +' '
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
     c                                     LOTE RECHAZADO.................'
     c*....Mensaje del Correo
     c                   Movel     QZCDIS        WWCDIS            4
     c                   Movel     QZIBAN        WWIBAN            4
     c                   Movel     QZQCA1        WWQCA1           11
     c                   Movel     *Blanks       WW$ENT           13
     c                   Movel     *Blanks       WW$DEC            2
     c                   Movel     QZ$IMP        WW$ENT
     c                   Move      QZ$IMP        WW$DEC
     c                   Movel     *Blanks       MENSAJ          250
     c                   Eval      MENSAJ='El Lote: '+
     c                                   WWIPOS +'-Emp: '+
     c                                   WWCDIS +' ' +
     c                                   WWIBAN +' ' +
     c                                   WWNRSO +'-Periodo: '+
     c                                   WWFAAM +'-Cant: '+
     C                                   WWQCA1 +'-Imp: '+
     C                                   WW$ENT +','+
     C                                   WW$DEC +'- '+
     c                                   WWTHEL +' '
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
     c*IFSError: Error de IFS
     c*-------------------------------------------------------------------------
     c     IFSError      BegSr
     C*
     c*                   eval      err= errno
     c*                   eval      errmsg= %str(strerror(errno))
     c*                   eval      err= errno
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
