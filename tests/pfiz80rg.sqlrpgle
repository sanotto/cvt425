*SRCMBRTXT:Canc. y PreCan. de PF UVA              
     H*
     H DFTACTGRP(*NO)  ACTGRP('QILE') BNDDIR('QC2LE')
     H DEBUG DATEDIT(*YMD) GENLVL(20)
     h*
     FPFCERT    UF   E           K DISK
     FPFTARJ    IF   E           K DISK
     FSGSYSV    IF   E             DISK
     FPFCODI    iF   E           K DISK
     F* Códigos de Movimientos de Plazos Fijos
     FPFCODF    IF   E             DISK
     F* Códigos Fijos de Movimientos de Plazos Fijos
     FPFMOVI03  UF   E           K DISK    USROPN
     FPFEXCE01  IF   E           K DISK
     F* Excepciones por inversor
     FPFCMGT01  IF   E           K DISK
     F* Tramos por pizarra plazo fijo
     FPFCATR02  IF   E           K DISK
     F* Titulares Plazo fijo
     FPFDCCL02  IF   E           K DISK
     FPFTMOd    O    E             DISK
     FBAMONE    IF   E           K DISK
     FACCTAC    IF   E           K DISK
     FCCCTCT    IF   E           K DISK
     FPFTMOD01  IF   E           K DISK    RENAME(REPFTMOD: RPFTMO)
     D*-------------------------------------------------------------------------
     D*>DATASTRUCTURE : MYPSDS
     D*>DESCRIPTION   : Data structure containing program status information
     D*>USE           : Retrieve program status information
     D*>RELATEDFUNCT  : sys_cmd
     D*-------------------------------------------------------------------------
     DMYPSDS          SDS
     D @PRC_NAM          *PROC
     D @PGM_STS          *STATUS
     D @PRV_STS               16     20S 0
     D @LIN_NUM               21     28
     D @ROU_NAM          *ROUTINE
     D @PAR_CNT          *PARMS
     D @EXC_TYP               40     42
     D @EXC_NUM               43     46
     D @PGM_LIB               81     90
     D @EXC_DTA               91    170
     D @EXC_IDE              171    174
     D @DAT_RUN              191    198
     D @DAT_YEA              199    200S 0
     D @LAS_FIL              201    208
     D @FIL_INF              209    243
     D @JOB_NAM              244    253
     D @USR_NAM              254    263
     D @JOB_NUM              264    269S 0
     D @JOB_NUM_C            264    269
     D @PJOBN                264    269  0
     D @JOB_DTE              270    275S 0
     D @RUN_DTE              276    281S 0
     D @RUN_TIM              282    287S 0
     D @CRT_DTE              288    293
     D @CRT_TIM              294    299
     D @CPL_LVL              300    303
     D @SRC_FIL              304    313
     D @SRC_MBR              324    333
     D @PRC_PGM              334    343
     D @PRC_MOD              344    353
     D*-------------------------------------------------------------------------
     D/COPY SDB01.SRC/QRPGSRC,IFSIO_H
     D/copy SDB01.SRC/QRPGSRC,errno_h
     D*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
     D*----------------------------------------------------------------
     D LDA           E DS                  EXTNAME(LDA)
     D*-------------------------------------------------------------------------
     D rc              S              7A
     D*-------------------------------------------------------------------------
     c                   exsr      BuscarPF
     c                   exsr      CrtQtempF
     c                   exsr      BuscaCotDia
     c                   exsr      CreaCupon
     c                   exsr      Totalizar
     c                   exsr      Contabilizar
     c                   exsr      ActCertific
     c                   exsr      Depositar
     c                   exsr      FixDevenga
     c                   exsr      endpgm
     c*-------------------------------------------------------------------------
     c* BuscaPF: Recupera Certificado de Plazo Fijo
     c*-------------------------------------------------------------------------
     c     BuscarPF      begsr
     c*
     c     kiz000        chain     repfcert                           99
     c                   if        *in99 = *On
     c                   eval      paerro='PF No encontrado'
     c                   exsr      endpgm
     c                   endif
     c*
     c                   z-add     iz$dal        yadeve
     c                   move      izirin        wiisuc
     c*
     c                   endsr
     c*-------------------------------------------------------------------------
     c* CrtQtempF: Crea archivos en la QTEMP
     c*-------------------------------------------------------------------------
     c     CrtQtempF     begsr
     c*
     c/free
        rc = Shell('CRTDUPOBJ OBJ(PFMOVI*) ' +
                   '    FROMLIB(DSDBFIL)   ' +
                   '    OBJTYPE(*FILE)     ' +
                   '    TOLIB(QTEMP)       ' +
                   '    DATA(*NO)          ' );

        rc = Shell('CLRPFM QTEMP/PFMOVI    ' );

        rc = Shell('RMVLIBLE QTEMP');
        rc = Shell('ADDLIBLE QTEMP');
     c/end-free
     c*
     c                   open      PFMOVI03
     c*
     c                   endsr
     c*-------------------------------------------------------------------------
     c* BuscaCotDia: Busca cotización del día
     c*-------------------------------------------------------------------------
     c     BuscaCotDia   begsr
     c*
     C                   call      'BAMU00RG'
     c                   parm      99999         PAISUC            5 0
     c                   parm      50            PAIMON            9 0
     c                   parm      *zero         PAFECH            8 0
     c                   parm                    PACOTI           12 6
     c*
     c                   endsr
     c*-------------------------------------------------------------------------
     c* CreaCupon: Crea el Cupon
     c*-------------------------------------------------------------------------
     c     CreaCupon     begsr
     c*
     c                   z-add     IZQDPL        wwqdpl
     c* ...(C)ancelar o (P)recancelar
     c                   if        paacci = 'C'
     c                   exsr      GetCapEnUva
     c                   z-add     capuva        ww$cap
     c                   z-add     izttna        wwttna
     c                   else
     c* ... Precancelacion
     c                   exsr      GetDiasAPag
     c                   exsr      GetTasaPrec
     c*
     c                   z-add     candia        wwqdpl
     c                   z-add     izttna        wwttna
     c                   z-add     capinc        ww$cap
     c                   z-add     taspre        wwttna
     c                   endif
     c*
     c                   exsr      GenCodF
     c                   exsr      GenCodV
     c*
     c* ... Si es cancelación, los calc. están en UVA, volver a $ c/cot. dia
     c                   if        paacci = 'C'
     c                   exsr      DeUvaAPesos
     c                   endif
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* GetDiasAPag: Calcula cuantos días hay que pagar de int en preca
     c*----------------------------------------------------------------
     c     GetDiasAPag   begsr
     c*
     c                   z-add     *zero         padias           15 0
     c                   z-add     *zero         pafech            8 0
     c                   move      *blanks       pacinv            2
     c                   z-add     *zero         wdfasi            8 0
     c                   z-add     *zero         wdtope            8 0
     C* Invierte fechas de asiento
     C                   Z-ADD     IZFASI        PAFECH
     C                   MOVE      'IN'          PACINV
     C                   CALL      'SBBAINFE'
     C                   PARM                    PAFECH
     C                   PARM                    PACINV
     C* Obtiene días de fecha de asiento
     C                   Z-ADD     *ZERO         PADIAS
     C                   CALL      'SBBAFECH'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PADSEM            2
     C                   PARM                    PAIERR            1
     c* ... wdfasi fecha de asiento en Juliano
     C                   Z-ADD     PADIAS        WDFASI
     C* Invierte fechas Tope.
     C                   Z-ADD     AASFEI        PAFECH
     C                   MOVE      'IN'          PACINV
     C                   CALL      'SBBAINFE'
     C                   PARM                    PAFECH
     C                   PARM                    PACINV
     C* Obtiene días de fecha TOPE.
     C                   Z-ADD     *ZERO         PADIAS
     C                   CALL      'SBBAFECH'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PADSEM
     C                   PARM                    PAIERR
     c* ... wdtope hoy en Juliano
     C*                  ADD       1             PADIAS
     C                   Z-ADD     PADIAS        WDTOPE
     c*
     c     wdtope        sub       wdfasi        candia           15 0
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* DeUvaAPesos: Convierte el Cupon en UVA a Pesos a la cot del día
     c*----------------------------------------------------------------
     c     DeUvaAPesos   begsr
     c*
     c     kjb030        chain     REPFMOVI                           99
     c                   dow       *in99 = *off
     c* ... ir al pfcodi para ver si el codigo suma o resta
     c                   mult      pacoti        jb$imp
     c                   update    REPFMOVI
     c     kjb030        reade     REPFMOVI                               99
     c                   enddo
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* Totalizar: Totalizar Cupon
     c*----------------------------------------------------------------
     c     Totalizar     begsr
     c*
     c     kjb030        chain     REPFMOVI                           99
     c                   dow       *in99 = *off
     c* ... ir al pfcodi para ver si el codigo suma o resta
     c*
     c     JBIMPF        chain     PFCODI                             80
     c                   if        JBIASK = '2'
     c                   sub       JB$IMP        ww$ine
     c                   else
     c                   add       JB$IMP        ww$ine
     c                   endif
     c                   if        JBIMPF=1
     c                   z-add     jb$IMP        fi$cap
     c                   endif
     c                   if        JBIMPF=2
     c                   z-add     jb$IMP        fi$int
     c                   endif
     c*
     c     kjb030        reade     REPFMOVI                               99
     c                   enddo
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* FixDevenga: Arregla devengamiento
     c*----------------------------------------------------------------
     c     FixDevenga    begsr
     c                   exsr      TotCapInt
     c* Escribo capital
     c                   z-add     captmod       im$mod           15 2
     c                   z-add     1             wwimpf            3 0
     c                   exsr      EscCapInt
     c* Escribo interes
     c                   z-add     inttmod       im$mod
     c                   z-add     2             wwimpf
     c                   exsr      EscCapInt
     c                   endsr
     c*----------------------------------------------------------------
     c* TotCapInt : Totaliza capital e interese devengados
     c*----------------------------------------------------------------
     c     TotCapInt     begsr
     c                   z-add     *zeros        captmod          15 2
     c                   z-add     *zeros        inttmod          15 2
     c     ksi010        chain     RPFTMO                             90
     c                   dow       *in90 = *off
     c*
     c                   if        ksimpf = 1  and ksimoc = '0'
     c                   add       ks$imp        captmod
     c                   endif
     c*
     c                   if        ksimpf = 4  and ksimoc = '0'
     c                   add       ks$imp        inttmod
     c                   endif
     c     ksi010        reade     RPFTMO                                 90
     c                   enddo
     c*
     c*Reverso montos de cap e int
     c     -1            mult      captmod       captmod
     c     -1            mult      inttmod       inttmod
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c*----------------------------------------------------------------
     c* EscCapInt : Escribea capital e interes  devengados PFTMOD
     c*----------------------------------------------------------------
     c     EscCapInt     begsr
     c
     c*
     c     IZIMON        chain     BAMONE                             80
     c*
     c                   z-add     AMICMO        KSICMO
     c                   move      AMINI1        KSINI1
     c                   move      AMINI2        KSINI2
     c                   move      AMINI3        KSINI3
     c                   move      AMINI4        KSINI4
     c                   move      AMINI5        KSINI5
     c                   move      AMINI6        KSINI6
     c                   move      AMINI7        KSINI7
     c                   movel     IZIGCE        KSIGCE
     c                   movel     IZITCE        KSITCE
     c                   z-add     IZISCT        KSISCT
     c                   z-add     IZITCU        KSITCU
     c                   z-add     wwimpf        KSIMPF
     c                   z-add     IZITTR        KSITTR
     c                   z-add     IZINCE        KSINCE
     c                   z-add     IM$MOD        KS$IMP
     c                   movel     'S'           KSIGAS
     c*
     c                   z-add     WWFASI        KSFASI
     c*
     c                   move      *BLANKS       KSIGRC
     c                   move      *BLANKS       KSISGC
     c                   z-add     wiisuc        KSISAL
     c                   z-add     *ZEROS        KSICAJ
     c*
     c                   write     REPFTMOD
     c                   endsr
     c*----------------------------------------------------------------
     c* Contabilizar: Es copiar al PRTMOD
     c*----------------------------------------------------------------
     c     Contabilizar  begsr
     c*
     c                   move      '2'           KSIMOC
     c                   z-add     *zeros        WW$IMP
     c     kjb030        chain     REPFMOVI                           99
     c                   dow       *in99 = *off
     c     JBIMPF        chain     PFCODI                             80
     C                   z-add     *zeros        KSIRES
     C                   z-add     JBISUC        KSISUC
     c                   z-add     JBIMON        KSIMON
     c     JBIMON        chain     BAMONE                             80
     c*
     c*
     c                   z-add     AMICMO        KSICMO
     c                   move      AMINI1        KSINI1
     c                   move      AMINI2        KSINI2
     c                   move      AMINI3        KSINI3
     c                   move      AMINI4        KSINI4
     c                   move      AMINI5        KSINI5
     c                   move      AMINI6        KSINI6
     c                   move      AMINI7        KSINI7
     c                   movel     JBIGCE        KSIGCE
     c                   movel     JBITCE        KSITCE
     c                   z-add     JBISCT        KSISCT
     c                   z-add     JBITCU        KSITCU
     c                   z-add     JBIMPF        KSIMPF
     c                   z-add     JBITTR        KSITTR
     c                   z-add     JBINCE        KSINCE
     c                   z-add     JB$IMP        KS$IMP
     c                   movel     'S'           KSIGAS
     c*
     c                   z-add     WWFASI        KSFASI
     c*
     c                   move      *BLANKS       KSIGRC
     c                   move      *BLANKS       KSISGC
     c                   z-add     wiisuc        KSISAL
     c                   z-add     *ZEROS        KSICAJ
     c                   if        JBIASK = '2'
     c                   sub       JB$IMP        WW$IMP
     c                   else
     c                   add       JB$IMP        WW$IMP
     c                   endif
     c                   write     REPFTMOD
     c     kjb030        reade     REPFMOVI                               99
     c                   enddo
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* ActCertific : Cod de Est, Int pag/dev y fec pag actualizados
     c*----------------------------------------------------------------
     c     ActCertific   begsr
     c*
     c                   movel     'P'           IZIECE
     c                   z-add     aasfei        IZFAPA
     c                   z-add     fi$int        IZ$DAL
     c                   z-add     fi$cap        IZ$CAP
     c                   z-add     fi$int        IZ$INT
     c                   z-add     ww$ine        IZ$INE
     c*
     c                   update    REPFCERT
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* Depositar: Según los nuevos códigos
     c*----------------------------------------------------------------
     c     Depositar     begsr
     c*
     c* ... determinar subsistema
     c                   z-add     iziccl        wwicta
     c*
     c     kbm000        chain     RECCCTCT                           99
     c                   if        *in99 = *off
     c                   move      'CC'          wwisub
     c                   z-add     BMICCC        wwicta           11 0
     c                   else
     c     kfu000        chain     REACCTAC                           99
     c                   if        *in99 = *off
     c                   move      'AC'          wwisub
     c                   z-add     FUICAH        wwicta
     c                   endif
     c*
     c                   endif
     c*
     C                   time                    WWHORA            6 0
     C                   z-add     879           WWIMCA            3 0
     C                   z-add     wiisuc        WWISAL            5 0
     C                   z-add     *ZEROS        WWICAJ            5 0
     C                   z-add     *ZEROS        WWIASC            1 0
     C                   z-add     *ZEROS        WWFASC            8 0
     C                   movel     *BLANKS       WWIUSA           10
     C                   Z-ADD     IZINCE        WWICHE            7 0
     C                   if        WWISUB='AC'
     C                   call      'SBACMOVB'
     C                   parm                    IZISUC
     C                   parm                    WWICTA
     C                   parm                    AASFEI
     C                   parm                    WWHORA
     C                   parm                    IZIMON
     C                   parm                    WWIMCA
     C                   parm                    WWISAL
     C                   parm                    WWICAJ
     C                   parm                    WW$INE
     C                   parm                    AASFEI
     C                   parm                    WWIASC
     C                   parm                    WWFASC
     C                   parm                    WWIUSA
     C                   parm                    WWIUSA
     C                   parm                    WWICHE
     C                   parm                    WWIMCA
     C                   else
     C                   call      'SBCCMOVB'
     C                   parm                    IZISUC
     C                   parm                    WWICTA
     C                   parm                    AASFEI
     C                   parm                    WWHORA
     C                   parm                    IZIMON
     C                   parm                    WWIMCA
     C                   parm                    WWISAL
     C                   parm                    WWICAJ
     C                   parm                    WW$INE
     C                   parm                    AASFEI
     C                   parm                    WWIASC
     C                   parm                    WWFASC
     C                   parm                    WWIUSA
     C                   parm                    WWIUSA
     C                   parm                    WWICHE
     C                   parm                    WWIMCA
     C                   endif
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c*----------------------------------------------------------------
     c* GetTasaPrec: Obtener tasa de pre cancelacion desde PFDCCL
     c*----------------------------------------------------------------
     c     GetTasaPrec   begsr
     c*
     c     kdc020        chain     PFDCCL02                           83
     c                   if        *in83 = *On
     c                   Eval      PAERRO='No se encontró cot. original'
     c                   exsr      endpgm
     c                   endif
     c     DC$I02        div       1000000       taspre           12 6
     c                   z-add     DC$I03        capinc           15 2
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* GetCapEnUVA: Obtener capital el UVA A LA IMPOSICION (PFDCCL)
     c*----------------------------------------------------------------
     c     GetCapEnUVA   BEGSR
     c*
     c     kdc020        chain     PFDCCL02                           83
     c                   if        *in83 = *on
     c                   Eval      PAERRO='No se encontró tasa precanc.'
     c                   exsr      endpgm
     c                   endif
     c*
     c     DC$I01        div(h)    1000000       cotdcl           12 6
     c     iz$cap        div(h)    cotdcl        capuva           15 2
     c*
     c                   endsr
     c*----------------------------------------------------------------
     c* GenCodF: Genera codigos fijos
     c*----------------------------------------------------------------
     c     GenCodF       BEGSR
     c*
     c     1             chain     PFCODF
     c
     c* Para el código de capital
     c     KQIMFC        CHAIN     PFCODI                             80
     c                   Z-ADD     KQIMFC        PAIMPF
     c                   EXSR      IngMov
     c* Para el código de interés                                     80
     c     KQIMFI        CHAIN     PFCODI
     c                   Z-ADD     KQIMFI        PAIMPF
     c                   EXSR      IngMov
     c*
     c                   ENDSR
     c*----------------------------------------------------------------
     c* GenCodV: Genera Codigos Variables
     c*----------------------------------------------------------------
     c     GenCodV       BEGSR
     c*
     c* Determina los movimientos del certificado
     c                   Z-ADD     IZISUC        WWISUC
     c     kir001        CHAIN     PFCMGT01                           80
     c   80              Z-ADD     99999         WWISUC
     c   80kir001        CHAIN     PFCMGT01                           80
     c     *IN80         DOWEQ     *OFF
     c     kiy001        CHAIN     PFEXCE01                           81
     c     *IN81         IFEQ      *ON
     c* Busca pgm. asociado al código de mov. y calcula importe     ||
     c     IRIMPF        CHAIN     PFCODI                             82
     c                   Z-ADD     IRIMPF        PAIMPF
     c                   EXSR      IngMov
     c                   ENDIF
     c     kir001        READE     PFCMGT01                               80
     c                   ENDDO
     c*
     c                   ENDSR
     c*----------------------------------------------------------------
     c* IngMov : Ingresa Movimientos
     c*----------------------------------------------------------------
     c     IngMov        BEGSR
     c*
     c                   SELECT
     c     PAIMPF        WHENEQ    1
     c                   CALL      'PFX003RG'
     c                   PARM                    IZISUC
     c                   PARM                    IZIGCE
     c                   PARM                    IZITCE
     c                   PARM                    IZINCE
     c                   PARM                    PAIMPF
     c                   PARM                    IZITTR
     c                   PARM                    WW$CAP
     c                   PARM                    AASFEI
     c                   PARM                    IZIMON
     c                   PARM                    IZISCT
     c                   PARM                    IZITCU
     c*
     c     PAIMPF        WHENEQ    2
     c                   CALL      'PFX001RG'
     c                   PARM                    IZISUC
     c                   PARM                    IZIGCE
     c                   PARM                    IZITCE
     c                   PARM                    IZINCE
     c                   PARM                    PAIMPF
     c                   PARM                    IZITTR
     c                   PARM                    WW$CAP
     c                   PARM                    WWQDPL
     c                   PARM                    AASFEI
     c                   PARM                    WWTTNA
     c                   PARM                    IZIMON
     c                   PARM                    IZISCT
     c                   PARM                    IZITCU
     c                   PARM                    IZ$INT
     c                   PARM                    PA$IMP           15 2
     c*
     c     PAIMPF        WHENEQ    5
     c                   CALL      'PFX032RG'
     c                   PARM                    IZISUC
     c                   PARM                    IZIGCE
     c                   PARM                    IZITCE
     c                   PARM                    IZINCE
     c                   PARM                    PAIMPF
     c                   PARM                    IZICCL
     c                   PARM                    IZIMON
     c                   PARM                    IZ$INT
     c                   PARM                    IZ$TRE
     c                   PARM                    AASFEI
     c                   PARM                    IZISCT
     c                   PARM                    IZITCU
     c*
     c     PAIMPF        WHENEQ    6
     c                   CALL      'PFX002RG'
     c                   PARM                    IZISUC
     c                   PARM                    IZIGCE
     c                   PARM                    IZITCE
     c                   PARM                    IZINCE
     c                   PARM                    PAIMPF
     c                   PARM                    IZITTR
     c                   PARM                    WW$CAP
     c                   PARM                    AASFEI
     c                   PARM                    IZIMON
     c                   PARM                    IZISCT
     c                   PARM                    IZITCU
     c                   PARM                    IVTSEL
     c                   ENDSL
     c*
     c                   endsr
     c*-------------------------------------------------------------------------
     c* *inzsr: Inicialización
     c*-------------------------------------------------------------------------
     c     *inzsr        begsr
     c*
     c     *entry        plist
     c                   parm                    paisuc
     c                   parm                    paince
     c                   parm                    paacci            1
     c                   parm                    paerro           50
     c*
     c                   move      *blanks       paerro           50
     c*
     c     *like         define    izisuc        paisuc
     c     *like         define    izince        paince
     c     *like         define    iz$cap        ww$cap
     c     *like         define    iz$cap        ww$imp
     c     *like         define    izttna        wwttna
     c     *like         define    iz$ine        ww$ine
     c     *like         define    iz$int        fi$int
     c     *like         define    iz$cap        fi$cap
     c     *like         define    iz$dal        yadeve
     c     *like         define    izfalt        wwfasi
     c     *like         define    izisuc        wiisuc
     c     *like         define    izqdpl        wwqdpl
     c                   z-add     *zeros        wwicta           11 0
     c*
     c                   move      *blanks       wwisub            2
     c*
     C     *DTAARA       DEFINE    *LDA          LDA
     C                   IN        LDA
     c*
     c                   z-add     *zeros        paimpf            3 0
     c                   z-add     *zeros        wwisuc            5 0
     c     kiz000        klist
     c                   kfld                    paisuc
     c                   kfld                    paince
     c
     c* Acceso a PFCMGT
     c     kir001        klist
     c                   kfld                    paisuc
     c                   kfld                    izigce
     c                   kfld                    izitce
     c* Acceso a PFDCCL02
     c     kdc020        klist
     c                   kfld                    izisuc
     c                   kfld                    paince
     c* Acceso a PFEXCE
     c     kiy001        klist
     c                   kfld                    paisuc
     c                   kfld                    iziccl
     c                   kfld                    irimpf
     c* Acceso a PFMOVI
     c     kjb030        klist
     c                   kfld                    izisuc
     c                   kfld                    paince
     c*
     c* Acceso a PFTMOD
     c     ksi010        klist
     c                   kfld                    izisuc
     c                   kfld                    izigce
     c                   kfld                    izitce
     c                   kfld                    paince
     c* Acceso a ACCTAC
     c     kfu000        klist
     c                   kfld                    izisuc
     c                   kfld                    wwicta
     c*
     c* Acceso a CCCTCT
     c     kbm000        klist
     c                   kfld                    izisuc
     c                   kfld                    wwicta
     c*
     C     1             chain     RESGSYSV                           99
     c*
     c                   exsr      DetFecAsi
     c*
     c                   endsr
     c*-------------------------------------------------------------------------
     c* DetFecAsi: Determinar fecha de asiento si preca o en proc diario
     c*-------------------------------------------------------------------------
     c     DetFecAsi     begsr
     c* ...
     C     EQIPRC        chain     PFTARJ                             80
     c                   z-add     aasfei        wwfasi
     c                   if        EQIPRC<>*blanks
     C                   z-add     KHFPPF        wwfasi
     c                   endif
     c*
     c                   endsr
     c*-------------------------------------------------------------------------
     c* endpgm: Fin del Programa
     c*-------------------------------------------------------------------------
     c     endpgm        begsr
     c*
     c                   seton                                        lr
     c                   return
     c*
     c                   endsr
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
      /copy sdb01.src/qrpgsrc,errno_h
