*SRCMBRTXT:Genera XML para LINK GDE               
     H DFTACTGRP(*NO)  ACTGRP('QILE')
     H DATEDIT(*YMD)
     F*---------------------------------------------------------------------
     FSGSYSV    IF   E             DISK
     D*---------------------------------------------------------------------
     D* Prototipos para las llamadas a la API
     D*---------------------------------------------------------------------
     D*------------------
     D opendir         PR              *   EXTPROC('opendir')
     D   dirname                       *   VALUE options(*string)
     D*------------------
     D closedir        PR            10I 0 EXTPROC('closedir')
     D   dirhandle                     *   VALUE
     D*------------------
     D readdir         PR              *   EXTPROC('readdir')
     D   dirhandle                     *   VALUE
     D*------------------
     D rename          PR            10I 0 EXTPROC('Qp0lRenameUnlink')
     D   old                           *   VALUE options(*string)
     D   new                           *   VALUE options(*string)

     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value options(*string)
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)

     D fseek           PR            10I 0 ExtProc('fseek')
     D  filehandler                  10I 0 value
     D  fileoffset                   10I 0 value
     D  start_pos                    10I 0 value

     D unlink          PR            10I 0 ExtProc('unlink')
     D   path                          *   Value options(*string)

     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D Close           PR            10I 0 ExtProc('close')
     D   Sock_Desc                   10I 0 Value

     D die             PR
     D   peMsg                      256A   const

     D*---------------------------------------------------------------------
     D* Estructuras de Datos que devuelven las API
     D*---------------------------------------------------------------------
      *********************************************************
      * Definitions needed to make IFS API calls.  Note that
      * these should really be in a separate /copy file]
      *********************************************************
     D O_WRONLY        C                   2
     D SEEK_SET        C                   0
     D O_RDWR          C                   4
     D O_CREAT         C                   8
     D O_TRUNC         C                   64
     D O_TEXTDATA      C                   16777216
     D O_CODEPAGE      C                   8388608

     D*** file permissions
     D S_IRUSR         S             10I 0 INZ(256)
     D S_IWUSR         S             10I 0 INZ(128)
     D S_IXUSR         S             10I 0 INZ(64)
     D S_IRWXU         S             10I 0 INZ(448)
     D S_IRGRP         S             10I 0 INZ(32)
     D S_IWGRP         S             10I 0 INZ(16)
     D S_IXGRP         S             10I 0 INZ(8)
     D S_IRWXG         S             10I 0 INZ(56)
     D S_IROTH         S             10I 0 INZ(4)
     D S_IWOTH         S             10I 0 INZ(2)
     D S_IXOTH         S             10I 0 INZ(1)
     D S_IRWXO         S             10I 0 INZ(7)
     D AsciiCodePage   S             10U 0 INZ(850)
     D*----------------------------------------------------------------*
     D               ESDS                  EXTNAME(@PSDS)
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D Lines           S             80    DIM(9999) CTDATA PERRCD(1)
     D*
     D FilHnd          S             10I 0
     D TxtFile         S            255
     d buf             S             90
     D fn              S            255
     D fqn             S            255
     D msj             S              1
     D ts              S              2
     D ParStrg         S           2048
     D FechaPr         S              8
     D*
     D CmdStr          S           4096
     D CmdStrLen       S             15  5 INZ(4096)
     D*
     D i               S              5  0
     D*---------------------------------------------------------------------
     C                   ExSr      BldFilName
     C                   ExSr      AddParam
     C                   ExSr      WrtSrcFile
     C                   ExSr      ExeSrcFile
     C                   ExSr      DltSrcFile
     C                   ExSr      EndPgm
     C*-------------------------------------------------------------------------
     C     BldFilName    BegSr
     C                   Eval      ts = ParStr
     C                   Call      'LIGDRE05'
     C                   Parm                    fn
     C                   Parm                    ts
     C                   Parm                    msj
     C                   If        msj = '1'
     C                   ExSr      DltSrcFile
     C                   Callp     die('El GDE: '+%Trim(fn)+' ya fue procesado')
     C                   Else
     C                   Eval      fqn =  fn
     C                   EndIf
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     AddParam      BegSr
     C                   Eval      FechaPr = %char(FECCOR)
     C                   Eval      ParStrg = ParStr
     C                   Eval      ParStrg = %trim(ParStrg)+' '+FechaPr+' '+
     C                                       %trim(fqn)
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     ExeSrcFile    BegSr
     C*
     C                   Eval      CmdStr=%trim('STRQSH CMD('''+
     C                                    'python '+%Trim(TxtFile) + ' '+
     C                                     %trim(ParStrg) +
     C                                    ''')')
     C*
     C                   Monitor
     C                   Call      'QCMDEXC'
     C                   PARM                    CmdStr
     C                   PARM      4096          CmdStrLen
     C                   On-Error
     C                   EndMon
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     WrtSrcFile    BegSr
     C*
     C                   ExSr      OpenStmF
     C                   For       i = 1 to 9999
     C                   Eval      buf=Lines(i)
     C                   If        %Trim(buf)='EndSource' or
     C                             %Trim(buf)='EndSrc'
     C                   Leave
     C                   EndIf
     C                   Eval      %subst(buf:90:1)=  X'25'
     C                   CallP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C                   EndFor
     C                   ExSr      CloseStmF
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     DltSrcFile    BegSr
     C*
     C                   CallP     Unlink(%Trim(TxtFile))
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   SetOn                                        LR
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     OpenStmF      BegSr
     C*
     C                   Move      @PJOBN        CHJOBN            6
     C                   Time                    AUHORA            6 0
     C                   Move      AUHORA        CHHORA            6
     C                   Move      *Date         CHFECH            8
     C*
     C                   Eval      TxtFile='/home/tmp/execpy'+
     C                                    '_' + %Trim(CHFECH) +
     C                                    '_' + %Trim(CHHORA) +
     C                                    '_' + %Trim(CHJOBN) +
     C                                    '.py'
     C*
     C                   EVAL      FilHnd=open(%Trim(TxtFile):
     C                                  O_CREAT + O_WRONLY + O_TRUNC +
     C                                  O_CODEPAGE :
     C                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     C                                  1252     )
     C                   callp     close(FilHnd)
     C                   EVAL      FilHnd=open(%trim(TxtFile):
     C                                   O_WRONLY+O_TEXTDATA)
     C*
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     CloseStmF     BegSr
     C*
     C                   callp     close(FilHnd)
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C     *INZSR        BegSr
     C*
     C     *Entry        PList
     C                   Parm                    ParStr            2
     C*
     C     1             CHAIN     SGSYSV                             25
     C                   Z-ADD     AASFEI        FECCOR            8 0
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*  This ends this program abnormally, and sends back an escape.
     C*  message explaining the failure.
     C*-------------------------------------------------------------------------
     P die             B
     D die             PI
     D   peMsg                      256A   const

     D SndPgmMsg       PR                  ExtPgm('QMHSNDPM')
     D   MessageID                    7A   Const
     D   QualMsgF                    20A   Const
     D   MsgData                    256A   Const
     D   MsgDtaLen                   10I 0 Const
     D   MsgType                     10A   Const
     D   CallStkEnt                  10A   Const
     D   CallStkCnt                  10I 0 Const
     D   MessageKey                   4A
     D   ErrorCode                32766A   options(*varsize)

     D dsEC            DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256

     D wwMsgLen        S             10I 0
     D wwTheKey        S              4A

     C                   eval      wwMsgLen = %len(%trimr(peMsg))
     C                   if        wwMsgLen<1
     C                   return
     C                   endif
     C                   callp     SndPgmMsg('CPF9897': 'QCPFMSG   *LIBL':
     C                               peMsg: wwMsgLen: '*ESCAPE':
     C                               '*PGMBDY': 1: wwTheKey: dsEC)

     c                   return
     P                 E

**
import ibm_db_dbi as dbi
import sys
import codecs
from datetime import datetime

tipo_de_servicio=sys.argv^1!
fecha_a_generar=sys.argv^2!
nombre_de_archivo=sys.argv^3!


def crear_archivo_resumen():
    subsis="'LNKAC3021'"
    if tipo_de_servicio == 'CC':
        subsis="'LNKCC3011'"

    sql = "SELECT HLDES3 FROM SDBFIL.BAHILI "
    sql = sql + "INNER JOIN SDBFIL.LIGDEA ON LGICCL = HLICCC "
    sql = sql + "WHERE HLCTRE = 'LNK' and HLIPGM="+subsis
    sql = sql + " AND HLFALT="+fecha_a_generar
    sql = sql + " AND LGFBAJ = 0 AND LGHBAJ = 0 AND LGIUSB = ''"
    sql = sql + " Order By HLIPGM,HLIFIL,HLFALT,HLISUC,HLICCC,HLIHUR,HLAFB1"
    con = dbi.connect('DATABASE=*LOCAL')
    cur = con.cursor()
    cur.execute(sql)
    archivo_datos='/home/LINKBEE/REFRESH/'+nombre_de_archivo
    lineas=^!
    for r in cur.fetchall():
        lineas.append(r^0!)
    with codecs.open(archivo_datos, "w","utf-8-sig") as file:
     for item in lineas:
        file.write(item)
        file.write('\n')


def crear_archivo_control():
    sql_cant_res ="SELECT COUNT(*) FROM ( SELECT DISTINCT "
    sql_cant_res= sql_cant_res+ "HLISUC, HLICCC FROM SDBFIL.BAHILI "
    sql_cant_res= sql_cant_res+ "INNER JOIN SDBFIL.LIGDEA ON LGICCL = HLICCC "
    sql_cant_res=sql_cant_res + "WHERE HLIPGM IN ( 'LNKAC3021' ,"
    sql_cant_res=sql_cant_res +" 'LNKCC3011' ) AND "
    sql_cant_res=sql_cant_res + "HLDES3 LIKE '%CLIENTE%' AND "
    sql_cant_res=sql_cant_res + "HLFALT= "+ fecha_a_generar
    sql_cant_res=sql_cant_res + " AND LGFBAJ = 0 AND LGHBAJ = 0 AND LGIUSB = ''"
    sql_cant_res=sql_cant_res+" ) AS TABLE"

    sql_cant = "SELECT COUNT(*) FROM SDBFIL.BAHILI "
    sql_cant = sql_cant+"INNER JOIN SDBFIL.LIGDEA ON LGICCL = HLICCC "
    sql_cant = sql_cant+"WHERE HLIPGM IN ("
    sql_cant = sql_cant+" 'LNKAC3021' , 'LNKCC3011' )"
    sql_cant = sql_cant+" AND HLFALT= " + fecha_a_generar
    sql_cant = sql_cant+" AND LGFBAJ = 0 AND LGHBAJ = 0 AND LGIUSB = ''"

    con = dbi.connect('DATABASE=*LOCAL')
    cur = con.cursor()
    cur.execute(sql_cant_res)
    cantidad_resumen=cur.fetchone()

    cur = con.cursor()
    cur.execute(sql_cant)
    cantidad_paginas=cur.fetchone()

    con.close()

    fecha_proceso=datetime.today().strftime('%Y%m%d')
    cabecera="RGDECTRL0309"+fecha_proceso+"0000000001"

    cant_res=str(cantidad_resumen^0!)
    cant_res=cant_res.zfill(15)
    cant_reg=str(cantidad_paginas^0!)
    cant_reg=cant_reg.zfill(15)
    cant_pag='0'.zfill(15)
    cant_byte='0'.zfill(15)
    str_rep="                  "

    detalle="RGDECTRL"+nombre_de_archivo+str_rep
    detalle=detalle+fecha_proceso+cant_res+cant_pag+cant_reg+cant_byte

    sub_nombre=nombre_de_archivo^0:26!
    archivo_ctl='/home/LINKBEE/REFRESH/'+sub_nombre+'.CTRL'

    with codecs.open(archivo_ctl, "w","utf-8-sig") as file:
        file.write(cabecera)
        file.write('\n')
        file.write(detalle)


crear_archivo_resumen()
crear_archivo_control()
EndSrc
