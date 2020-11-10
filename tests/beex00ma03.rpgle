*SRCMBRTXT:Banca Electrónica-Man. P/Proc. Extracts
     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
     FLISERJ    UF A E           K DISK
     FBETRAN    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     F@CPISYS   UF   E           K DISK
     F@CPIUSD   UF   E           K DISK
     FBASCTM    UF A E           K DISK
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
     D*--------------------------------------------------------------------
     D* Read From a File
     D*
     D* ssize_t read(int handle, void *buffer, size_t bytes);
     D*--------------------------------------------------------------------
     D read            PR            10I 0 ExtProc('read')
     D  handle                       10i 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value
     D*---------------------------------------------------------------------
     D* Estructuras de Datos que devuelven las API
     D*---------------------------------------------------------------------
     D/copy LE00525/SOCKETSRPG,errno_h
     D*---------------------------------------------------------------------
     D p_dirent        s               *
     D dirent          DS                  BASED(p_dirent)
     D  d_reserved1                  16A
     D  d_fileno_gid                 10U 0
     D  d_fileno                     10U 0
     D  d_reclen                     10U 0
     D  d_reserved3                  10I 0
     D  d_reserved4                   8A
     D  d_nlsinfo                    12A
     D    nls_ccsid                  10I 0 OVERLAY(d_nlsinfo:1)
     D    nls_entry                   2A   OVERLAY(d_nlsinfo:5)
     D    nls_lang                    3A   OVERLAY(d_nlsinfo:7)
     D    nls_reserv                  3A   OVERLAY(d_nlsinfo:10)
     D  d_namelen                    10U 0
     D  d_name                      640A
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
     D*----------------------------------------------------------------*
     D LDA           E DS                  EXTNAME(LDA)
     I*----------------------------------------------------------------*
     D*---------------------------------------------------------------------
     D* Variables y Estructuras de Datos del Programa
     D*---------------------------------------------------------------------
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz'
     D dir             S               *
     D filename        S             30A
     D filename2       S             30A
     D fname           S            255A
     D motivo_rec      S             50A
     D prefijo         S              5A
     D prefijo4        S              4A
     D chkpref         S              5A
     D chfech          S              8A
     D baspat          S            255A
     D from            S            255A
     D lasterror       S            255A
     D to              S            255A
     D rc              S             10I 0
     D cantproc        S             15S 0
     D mes             S              2A
     D dia             S              2A
     D daco            S             30A
     D FILHND          S             10I 0
     d buf             s             72
     D logfile         S            255A
     C*---------------------------------------------------------------------
     c                   eval      baspat='/home/LINKBEE'
     c                   eval      dir = opendir(%trim(baspat)+'/Extracts/')
     c                   if        dir = *NULL
     c                   exsr      EndPgm
     c                   endif
     c                   eval      p_dirent = readdir(dir)
     c                   eval      cantproc=0
     c                   dow       p_dirent <> *NULL
     c                   eval      filename = %subst(d_name:1:d_namelen)
     c                   if        filename <> '.' and filename <> '..'
     c                   eval      cantproc=cantproc+1
     c                   eval      prefijo= %subst(d_name:1:5)
     c                   eval      prefijo= %xlate(lo:up:prefijo)
     c                   eval      prefijo4=%subst(prefijo:1:4)
     c                   select
     c                   when      prefijo='EBETR'
     c                   exsr      proc_transfe
     c                   when      prefijo4='CBUG'
     c                   exsr      proc_trancbu
     c                   when      prefijo='EBECU'
     c                   exsr      proc_cuentas
     c                   other
     C                   eval      S1DACL=' Arch. no Reconocido: '+filename
     c                   write     REBASCTM
     c                   eval      motivo_rec='Formato Desconocido'
     c                   exsr      rechazar_arch
     c                   endsl
     c*
     C                   eval      S1DACL=' '
     c                   write     REBASCTM
     c*
     c                   endif
     c                   eval      p_dirent = readdir(dir)
     c                   enddo
     c                   if        cantproc = 0
     C                   eval      S1DACL=' No hay archivos a procesar en '+
     c                                    '/home/LINKBEE/Extracts'
     c                   write     REBASCTM
     c                   endif
     c                   exsr      showte
     c                   exsr      wrtlog
     c                   exsr      endpgm
     C*---------------------------------------------------------------------
     c     proc_transfe  begsr
     c*
     C                   eval      S1DACL=' Proc. Ext. Trans: '+filename
     c                   write     REBASCTM
     c*
     c                   exsr      chk_ya_proc
     c  N99              leavesr
     c                   exsr      build_wwfech
     c  N99              eval      motivo_rec='Nombre de Archivo incorrecto'
     c  N99              exsr      rechazar_arch
     c  N99              leavesr

     c                   eval      chkpref='EBETR'
     c                   exsr      val_proc_ayer


     c                   z-add     *zero         cantot           15 0
     c                   z-add     *zero         sumtot           15 2
     c*
     C                   call      'BEEXTRC1'
     C                   parm                    wwfech
     C                   parm                    filnam           15
     C                   parm                    cantot
     C                   parm                    sumtot
     C                   parm                    error             1
     c*
     c                   if        error = *ZERO
     C                   call      'SPMD03RG'
     C                   parm                    wwfech
     c*
     C                   z-add     cantot        RJCAN1
     C                   z-add     sumtot        RJ$TOT
     c                   exsr      Proceso_OK
     c*
     C                   eval      S1DACL='   Proceso OK - Totales'
     c                   write     REBASCTM
     C                   eval      S1DACL='   ============================='
     c                   write     REBASCTM
     C                   eval      S1DACL='   Cantidad: '+
|||| C                                   %EDITW(cantot: '              0')
     c                   write     REBASCTM
     C                   eval      S1DACL='   Importe :'+
|||| C                                   %EDITW(sumtot: '             ,  ')
     c                   write     REBASCTM
     C                   eval      S1DACL=' '
     c                   write     REBASCTM
     C                   eval      S1DACL='   Detalle de Transferencias '
     c                   write     REBASCTM
     C                   eval      S1DACL='   ============================='
     c                   write     REBASCTM
     C     KBETRAN       CHAIN     REBETRAN                           99
     c                   dow       *IN99 =*OFF
     C                   if        TRFPRE=WWFECH
     C                   eval      S1DACL= '   '+ trncdb+' '+
     c                                   %editw(tr$imp: '             ,  ')
     c                                   + ' ' + TRAINF
     c                   write     REBASCTM
     C                   endif
     C     KBETRAN       READE     REBETRAN                               99
     c                   enddo
     c                   else
     c                   eval      motivo_rec='Error en Importacion'
     c                   exsr      rechazar_arch
     c                   endif
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     proc_trancbu  begsr
     c*
     C                   eval      S1DACL=' Proc. Ext. Trans CBU: '+filename
     c                   write     REBASCTM
     c                   eval      filename2=filename
     c                   eval      filename=%subst(filename:1:4)+'-'+
     c                                      %subst(filename:5)
     c*
     c                   exsr      chk_ya_proc
     c  N99              leavesr
     c                   exsr      build_wwfech
     c                   eval      filename=filename2
     c                   eval      fname='/home/LINKBEE/Extracts/'+filename2
     c  N99              eval      motivo_rec='Nombre de Archivo incorrecto'
     c  N99              exsr      rechazar_arch
     c  N99              leavesr
     c*
     c                   call      'SWAD01R6'
     C                   parm                    fname
     c                   parm                    errstr          255
     C*
     c                   if        errstr <> *BLANKS
     c* call a programa que haga el impacto
     c                   exsr      Proceso_OK
     c                   else
     c                   eval      motivo_rec='Error en Import:'+errstr
     c                   exsr      rechazar_arch
     c                   endif
     c*
     c                   endsr
     c*---------------------------------------------------------------------
     c     proc_cuentas  begsr
     c*
     C                   eval      S1DACL=' Proc. Ext. Ctas : '+filename
     c                   write     REBASCTM
     c*
     c                   exsr      chk_ya_proc
     c  N99              leavesr
     c                   exsr      build_wwfech
     c  N99              eval      motivo_rec='Nombre de Archivo incorrecto'
     c  N99              exsr      rechazar_arch
     c  N99              leavesr
     c                   eval      chkpref='EBECU'
     c                   exsr      val_proc_ayer
     c*
     C                   call      'BEEXCUC1'
     C                   parm                    wwfech
     C                   parm                    error             1
     C                   parm                    filnam           15
     c*
     c                   if        error = *ZERO
     c                   exsr      Proceso_OK
     c                   else
     c                   eval      motivo_rec='No se pudo procesar'
     c                   exsr      rechazar_arch
     c                   endif
     c                   endsr
     c*---------------------------------------------------------------------
     c     Proceso_OK    begsr
     c*
     c                   movel(p)  filnam        RJDACO
     C                   z-add     AASFEI        RJFALT
     C                   time                    RJhora
     C                   move      @PUSER        RJIUSR
     C                   write     RELISERJ
     c*
     C                   eval      S1DACL=' Archivo: '+filename +' [OK]'
     c                   write     REBASCTM
     c*
     c                   time                    wwhora            6 0
     c                   move      wwhora        wkhora            6
     c*
     c                   eval      from=%trim(baspat)+'/Extracts/'    +
     c                                  %trim(filename )
     c                   eval      to  =%trim(baspat)+'/Procesados/'  +
     c                                  %trim(filename )+
     c                                  '_'+chfech+'_'+wkhora+'_'+@puser
     c                   eval      rc=rename(%trimr(from):%trimr(to))
     c                   eval      lasterror=%str(strerror(errno))
     c*
     c                   endsr
     c*---------------------------------------------------------------------
     c     build_wwfech  begsr
     c*
     c                   seton                                        90
     c
     c                   eval      mes= %subst(filename:8:2)
     c                   eval      dia= %subst(filename:6:2)
     c                   testn                   mes                  99
     c  n99              leavesr
     c                   testn                   dia                  99
     c  n99              leavesr
     c                   movel(p)  filename      filnam           15
     c                   MOVE      AASFEI        CHFECH
     c                   eval      chfech=%subst(chfech:1:4)+
     c                                    mes               +
     c                                    dia
     c                   move      chfech        wwfech            8 0
     c                   endsr
     c*---------------------------------------------------------------------
     c     val_proc_ayer begsr
     c*
     C                   movel     *ZEROS        dias             13
     C                   move      WWFECH        dias
     C                   call      'SBBADIAS'
     C                   parm                    DIAS
     C                   parm                    DEVO              2
     c                   z-add     1             menosdias         2 0
     C                   if        devo = 'Lu'
     c                   z-add     2             menosdias         2 0
     C                   endif
     c*
     c                   z-add     wwfech        xxfech            8 0
     c                   move      'IN'          mode              2
     c                   call      'SBBAINFE'
     c                   parm                    xxfech
     c                   parm                    mode              2
     c                   call      'SBBACFEC'
     c                   parm                    xxfech
     c                   parm                    padias           15 0
     c                   parm                    paier             1 0
     c                   sub       menosdias     padias
     c                   z-add     *zero         xxfech
     c                   call      'SBBACFEC'
     c                   parm                    xxfech
     c                   parm                    padias           15 0
     c                   parm                    paier             1 0
     c*
     c                   movel     xxfech        diames            4
     c                   movel     diames        chdia             2
     c                   move      diames        chmes             2
     c                   eval      daco=chkpref+CHDIA+CHMES+'.0309'
     C     DACO          chain     RELISERJ                           99
     c                   if        *in99=*on
     C                   eval      S1DACL=X'22'+' NO SE PROCESO: '+
     C                                    DACO
     c                   write     REBASCTM
     c                   endif
     C*
     c                   endsr
     c*---------------------------------------------------------------------
     c     chk_ya_proc   begsr
     c*
     c                   movel     aasfei        curyear           4 0
     C                   MOVE      *OFF          YAPROC            1
     c     filename      chain     RELISERJ                           99
     c                   dow       *in99 = *off
     c                   movel     rjfalt        recyear           4 0
     c                   if        recyear = curyear
     C                   MOVE      *ON           YAPROC            1
     c                   else
     c                   delete    RELISERJ
     c                   endif
     c     filename      reade     RELISERJ                               99
     c                   enddo
     c                   if        YAPROC = *ON
     c                   move      RJFALT        CHFALT            8
     c                   move      RJHORA        CHHORA            6
     c                   eval      motivo_rec='Ya Proc.Por '+%TRIM(RJIUSR) +
     c                                        ' el '+
     c                             chfalt+' a las:'+chhora
     c                   exsr      rechazar_arch
     c                   endif
     c*
     c                   endsr
     C*---------------------------------------------------------------------
     c     rechazar_arch begsr
     c*
     C                   eval      S1DACL=x'22' +
     C                                    '     Rechazado por: '+motivo_rec
     C                   WRITE     REBASCTM
     c*
     c                   move      aasfei        chfech
     c                   time                    wwhora            6 0
     c                   move      wwhora        chhora            6
     c                   eval      motivo_rec=%xlate(' ':'_':
     c                                               %trim(motivo_rec))
     c                   eval      from=%trim(baspat)+'/Extracts/'    +
     c                                  %trim(filename )
     c                   eval      to  =%trim(baspat)+'/Rechazados/'  +
     c                              %trim(filename )+'_'+%trim(motivo_rec)+
     c                                  '_'+chfech+'_'+chhora+'_'+@puser
     c
     c                   eval      rc=rename(%trimr(from):%trimr(to))
     c                   eval      lasterror=%str(strerror(errno))
     c*
     c                   endsr
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     1             CHAIN     RESGSYSV
     C*
     C     KBETRAN       KLIST
     C                   KFLD                    TRIBCF
     C                   KFLD                    AASFEI
     C*
     C                   MOVE      'LNKBEE'      TRIBCF
     C                   MOVE      @PJOBN        S1IJOB
     C                   MOVE      *ZERO         RRN              15 0
     C*
     C                   EXSR      DLTTMP
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C     DLTTMP        BEGSR
     C*
     C     S1IJOB        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    REBASCTM
     C     S1IJOB        READE     REBASCTM                               99
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* SHOWTE : MUESTRA EL TE
     C*-------------------------------------------------------------------------
     C     SHOWTE        BEGSR
     C*
     C     @PJOBN        CHAIN     @CPIUSD                            80
     C                   EVAL      @CCBAR='Proceso de Extracts de Banca Empresa'
     C                   EVAL      WWTIT1=''
     C                   EVAL      WWTIT2='Los archivos proc. OK,  se moverán '+
     C                                    'a LINKBEE/Procesados'
     C                   EVAL      WWTIT3='Los archivos rechazados se moverán '+
     C                                    'a LINKBEE/Rechazados'
     C                   EVAL      WWTIT4=''
     C*                  EVAL      WWATR='     '
     C                   EVAL      WWTIT5=x'21'+'Log de Proceso'
     C                   MOVEL(P)  @PJOBN        @CIJOB
     C                   UPDATE    @CPIUSRR
     C                   CALL      'BATM00TE'
     C                   PARM                    WWTIT1           70
     C                   PARM                    WWTIT2           70
     C                   PARM                    WWTIT3           70
     C                   PARM                    WWTIT4           70
     C                   PARM                    WWTIT5           70
     C                   PARM                    WWATR            16
     C     @PJOBN        CHAIN(N)  @CPIUSD                            80
     C     @PJOBN        CHAIN(N)  @CPISYS                            80
     C                   ENDSR
     C*-------------------------------------------------------------------------
     c     wrtlog        begsr
     c*
     c                   time                    wwhora            6 0
     c                   move      wwhora        chhora            6
     c                   MOVE      AASFEI        CHFECH
     c                   eval      logfile=
     c                             %trim(baspat)+'/Procesados/Log_'+
     c                             chfech+'_'+chhora+'_'+%trim(@puser) +
     c                             '.TXT'
     c                   EVAL      FilHnd=open(%trim(logfile):
     c                                  O_CREAT + O_WRONLY + O_TRUNC +
     c                                  O_CODEPAGE :
     c                                  S_IWUSR+S_IRUSR+S_IRGRP+S_IROTH:
     c                                  819     )
     c                   callp     close(FilHnd)
     c                   EVAL      FilHnd=open(%trim(logfile):
     c                                   O_WRONLY+O_TEXTDATA)
     c*
     C     S1IJOB        CHAIN     REBASCTM                           99
     C     *IN99         DOWEQ     *OFF
     c                   eval      buf   =%triml(%xlate(x'22':' ':
     c                                    %triml(s1dacl))+x'0d'+ x'25')
     c                   CALLP     write(FilHnd: %addr(buf   ): %size(buf   ))
     C     S1IJOB        READE     REBASCTM                               99
     C                   ENDDO
     c*
     c                   callp     close(FilHnd)
     c                   endsr
     C*---------------------------------------------------------------------
     c     EndPgm        begsr
     c                   seton                                        lr
     c                   return
     c                   endsr
      /define ERRNO_LOAD_PROCEDURE
      /copy SOCKETSRPG,errno_h
