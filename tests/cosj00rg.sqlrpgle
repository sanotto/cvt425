*SRCMBRTXT:Contabilidad Automática                
     H DECEDIT('0,') DATEDIT(*DMY.)
     H DFTACTGRP(*no)
     FS@JF05    IF   E           K DISK    USROPN
     FCOSJHI    UF A E           K DISK
     FCOPASJ    IF   E           K DISK
     FCOTARJ    IF   E           K DISK
     FCOMESA06  IF   E           K DISK
     FSGUSUA    IF   E           K DISK
     FSGCDFV01  IF   E           K DISK
     FSGSYSV    IF   E             DISK
     F0OSJRG00  CF   E             WORKSTN sfile(d01:s1irrn)
     FCOFINE    O    E             DISK    USROPN
     C*-------------------------------------------------------------------------
     D  Shell          PR             7A
     D   Command                   1024A   VALUE
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
     D @puser                254    263
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

     D DSCTACON        DS
     D  CTACON                 1     10
     D   WWICTL                1      1  0
     D   WWIRUR                2      2  0
     D   WWIMCO                3      3  0
     D   WWINCC                4      6  0
     D   WWISCC                7     10  0
     DCTCODS           DS
     D  CTACOS                 1     10  0
     D   HQICTL                1      1  0
     D   HQIRUR                2      2  0
     D   HQIMCO                3      3  0
     D   HQINCC                4      6  0
     D   HQISCC                7     10  0
     D ERRDS           DS
     D   ERRTXT                1    255
     D   WWNCU1                1     55
     D   WWNCU2               56    110
     D   WWNCU3              111    165
     D   WWNCU4              166    220
     D   WWNCB1              221    275
     D   WWNCB2              276    330
     D   WWNCB3              331    385
     D   WWNCB4              386    440

     D CMDSTR          S           1024
     D RC              S              7
     D LIBNME          S             10
     C*-------------------------------------------------------------------------
     C                   WRITE     G01
     C                   EXFMT     W01
     C     *IN12         DOWEQ     *OFF
     C                   EXSR      CHKLIQ
     C                   IF        LIQOK = *ON
     C                   EXSR      PRCLIQ
     C                   ENDIF
     C                   EXFMT     W01
     C                   ENDDO
     C                   EXSR      ENDPGM
     C*-------------------------------------------------------------------------
     C* CHKLIQ : CHEQUEAR SI EXISTE LIQUIDACION
     C*-------------------------------------------------------------------------
     C     CHKLIQ        BEGSR
     C*
     C                   MOVE      *ON           LIQOK             1
     C                   MOVE      NRFWW         AUXNLI            4
     C                   MOVE      AMFWW         AUXPER            6
     C                   EVAL      LIBNME='@'+GRFWW+'_'+AUXNLI
     C                   EVAL      RC=SHELL('CHKOBJ OBJ('+LIBNME+
     C                             ') OBJTYPE(*LIB)')
     C                   IF        RC <> 'CPF0000'
     C                   MOVE      *OFF          LIQOK
     C                   EXFMT     E01
     C                   ENDIF
     c                   MOVEL     AMFWW         WWANO             4 0
     c                   MOVE      AMFWW         WWMES             2 0
     C                   IF        WWANO < 2000  OR  WWMES < 1 OR WWMES > 12
     C                   MOVE      *OFF          LIQOK
     C                   EXFMT     E02
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* PRCLIQ : PROCESAR LIQUIDACION
     C*-------------------------------------------------------------------------
     C     PRCLIQ        BEGSR
     C*
     C     K533          CHAIN     RECOSJHI                           99
     C   99              EXSR      WRTASI
     C*
     C                   EXSR      LODSFL
     C                   DOW       *IN12 = *OFF
     C                   IF        *IN10 = *ON AND *IN81 = *ON
     C                   EXSR      IMPASI
     C                   ENDIF
     C   11              EXSR      DELASI
     C*  22              EXSR      REVASI
     C   09              EXSR      VERASI
     C   21              EXSR      PRTASI
     C                   EXSR      LODSFL
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* DELASI: ELIMINA ASIENTO PARA SER REGENERADO
     C*-------------------------------------------------------------------------
     C     DELASI        BEGSR
     C*
     C                   IF        *IN80 = *ON
     C                   EXFMT     W04
     C                   IF        *IN12 = *OFF
     c     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   DELETE    RECOSJHI
     c     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C                   EXSR      WRTASI
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* VERASI: VER ASIENTO EN FORMATO STANDARD
     C*-------------------------------------------------------------------------
     C     VERASI        BEGSR
     C*
     C                   MOVE      AMFWW         FAAM
     C                   MOVE      GRFWW         FLIA
     C                   MOVE      NRFWW         LIQ
     C                   MOVEL(p)  '*'           OUT
     C                   CALL      'COSJ00CL'
     C                   PARM                    FAAM              6
     C                   PARM                    FLIA              1
     C                   PARM                    LIQ               4
     C                   PARM                    OUT              10
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* PRTASI: IMP ASIENTO EN FORMATO STANDARD
     C*-------------------------------------------------------------------------
     C     PRTASI        BEGSR
     C*
     C                   MOVE      AMFWW         FAAM
     C                   MOVE      GRFWW         FLIA
     C                   MOVE      NRFWW         LIQ
     C                   MOVEL(p)  '*PRINT'      OUT
     C                   CALL      'COSJ00CL'
     C                   PARM                    FAAM              6
     C                   PARM                    FLIA              1
     C                   PARM                    LIQ               4
     C                   PARM                    OUT              10
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* IMPASI: IMPACTA ASIENTO EN CONTABILIDAD
     C*-------------------------------------------------------------------------
     C     IMPASI        BEGSR
     C*
     C                   Z-ADD     AASFEI        WWFASI
     C                   EVAL      WWDESC='ASTO. SUELDOS LIQ.:'+GRFWW+' '
     C                                    +AUXNLI+' PER.:'+AUXPER
     C                   EXSR      FMTW02
     C                   IF        *IN12 = *OFF
     C                   EVAL      CMDSTR='DLTF QTEMP/COASAU'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='CRTDUPOBJ OBJ(COASAU)' +
     C                                    ' FROMLIB(SDBFIL)     ' +
     C                                    ' OBJTYPE(*FILE)      ' +
     C                                    ' TOLIB(QTEMP)        '
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='DLTF QTEMP/COFINE'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='CRTDUPOBJ OBJ(COFINE)' +
     C                                    ' FROMLIB(SDBFIL)     ' +
     C                                    ' OBJTYPE(*FILE)      ' +
     C                                    ' TOLIB(QTEMP)        '
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='OVRDBF FILE(COFINE)  ' +
     C                                    ' TOFILE(QTEMP/COFINE)'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='OVRDBF FILE(COASAU)  ' +
     C                                    ' TOFILE(QTEMP/COASAU)'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   OPEN      COFINE
     c     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        HSFASI = *ZERO
     C*
     C                   Z-ADD     *ZERO         QÑISUC
     C                   Z-ADD     WWFASI        QÑFASI
     C                   Z-ADD     1             QÑIMON
     C                   MOVE(P)   HSICCD        CTACON
     C                   MOVE      WWICTL        QÑICTL
     C                   MOVE      WWIRUR        QÑIRUR
     C                   MOVE      WWIMCO        QÑIMCO
     C                   MOVE      WWINCC        QÑINCC
     C                   MOVE      WWISCC        QÑISCC
     C                   Z-ADD     HS$IMP        QÑ$IDE
     C                   Z-ADD     *ZERO         QÑ$IHA
     C                   MOVEL(P)  WWDESC        QÑCMOV
     C                   Z-ADD     *ZERO         QÑIDCB
     C                   WRITE     RECOFINE
     C                   Z-ADD     *ZERO         QÑISUC
     C                   Z-ADD     WWFASI        QÑFASI
     C                   Z-ADD     1             QÑIMON
     C                   MOVE(P)   HSICCR        CTACON
     C                   MOVE      WWICTL        QÑICTL
     C                   MOVE      WWIRUR        QÑIRUR
     C                   MOVE      WWIMCO        QÑIMCO
     C                   MOVE      WWINCC        QÑINCC
     C                   MOVE      WWISCC        QÑISCC
     C                   Z-ADD     *ZERO         QÑ$IDE
     C                   Z-ADD     HS$IMP        QÑ$IHA
     C                   MOVEL(P)  WWDESC        QÑCMOV
     C                   Z-ADD     *ZERO         QÑIDCB
     C                   WRITE     RECOFINE
     C*
     C                   Z-ADD     AASFEI        HSFASI
     C                   UPDATE    RECOSJHI
     C                   ENDIF
     c     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C                   CLOSE     COFINE
     C                   EVAL      CMDSTR='DLTOVR *ALL '
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='OVRDBF FILE(COASAU)    '+
     C                                    ' TOFILE(QTEMP/COASAU)  '+
     C                                    ' SHARE(*YES)           '
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='OPNQRYF                  '+
     C                                    ' FILE((QTEMP/COASAU))    '+
     C                                    ' TOFILE(QTEMP/COASAU)    '+
     C                                    '        KEYFLD((HCISUC)  '+
     C                                    '                (HCFASI) '+
     C                                    '                (HCIMON) '+
     C                                    '                (HCICTL) '+
     C                                    '                (HCIRUR) '+
     C                                    '                (HCIMCO) '+
     C                                    '                (HCINCC) '+
     C                                    '                (HCISCC))'
     C
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   MOVE      'CO'          WWISUB            2
     C                   CALL      'CO00FINE'
     C                   CALL      'CO0042RG'
     C                   PARM                    WWISUB
     C                   EVAL      CMDSTR='DLTOVR COASAU'
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='CLOF COASAU '
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EXFMT     W03
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* FMTW02: PROCESA PANTALLA W02
     C*-------------------------------------------------------------------------
     C     FMTW02        BEGSR
     C*
     C     FW02AG        TAG
     C                   EXFMT     W02
     C   12              LEAVESR
     C*
     C                   IF        WWFASI > AASFEI
     C                   EVAL      ERRTXT='LA FECHA NO PUEDE SER A'+
     C                                    ' FUTURO'
     C                   EXSR      DSPERR
     C                   GOTO      FW02AG
     C                   ENDIF
     C                   IF        WWFASI < FÑFPC1
     C                   EVAL      ERRTXT='MES CERRADO'
     C                   EXSR      DSPERR
     C                   GOTO      FW02AG
     C                   ENDIF
     C*
     C     WKEY50        KLIST
     C                   KFLD                    CVIOPT
     C                   KFLD                    WWISUB            2
     C                   MOVEL     'CO'          WWISUB
     c*
     C*
     C                   Z-ADD     *ZEROS        DIAS1            15 0
     C                   Z-ADD     *ZEROS        DIAS2            15 0
     C*
     C                   Z-ADD     WWFASI        PAFECH            8 0
     C                   Z-ADD     *ZEROS        PADIAS           15 0
     C                   MOVEL     *BLANK        PADSEM            2
     C                   MOVEL     *BLANK        PADIRR            1
     C*
     C                   CALL      'SBBAINFE'
     C                   PARM                    PAFECH
     C                   PARM      'IN'          EDTMDE            2
     C                   CALL      'SBBAFECH'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PADSEM
     C                   PARM                    PADIRR
     C                   Z-ADD     PADIAS        DIAS1
     C*
     C                   Z-ADD     AASFEN        PAFECH
     C                   Z-ADD     *ZEROS        PADIAS           15 0
     C                   MOVEL     *BLANK        PADSEM            2
     C                   MOVEL     *BLANK        PADIRR            1
     C*
     C                   CALL      'SBBAFECH'
     C                   PARM                    PAFECH
     C                   PARM                    PADIAS
     C                   PARM                    PADSEM
     C                   PARM                    PADIRR
     C                   Z-ADD     PADIAS        DIAS2
     C     DIAS2         SUB       DIAS1         XXDIFE           15 0
     C     @puser        CHAIN     SGUSUA                             96
     C     WKEY50        CHAIN     SGCDFV01                           97
     C                   IF        XXDIFE > CDQCHA
     C                   EVAL      ERRTXT='EXCEDE CANTIDAD DE DIAS'+
     C                                    ' FECHA VALOR AUTORIZADA'
     C                   EXSR      DSPERR
     C                   GOTO      FW02AG
     C                   ENDIF
     C                   ENDSR
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
     C*-------------------------------------------------------------------------
     C* LODSFL: CARGA Y MUESTRA SUBFILE
     C*-------------------------------------------------------------------------
     C     LODSFL        BEGSR
     C*
     C     K533          CHAIN     RECOSJHI                           99
     C* ... Asiento ya impactado
     C     HSFASI        COMP      *ZERO                                  80
     C                   MOVE      IMPACT        *IN81
     C*
     C                   SETOFF                                       3031
     C                   Z-ADD     *ZERO         S1IRRN           15 0
     C                   WRITE     C01
     C     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   ADD       1             S1IRRN
     C                   WRITE     D01
     C     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C     S1IRRN        COMP      *ZERO                              30
     C                   SETON                                        31
     C                   WRITE     P01
     C                   EXFMT     C01
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTASI : ESCRIBE ASIENTO
     C*-------------------------------------------------------------------------
     C     WRTASI        BEGSR
     C* ... En Primer Lugar reversamos cualquier asto que haya p/el periodo
     C*    K531          CHAIN     RECOSJHI                           99
     C*                  DOW       *IN99 = *OFF
     C*                  IF        HSFASI <> 0
     C*                  MULT      -1            HS$IMP
     C*                  MOVE      HSIDEG        HSIDEV
     C*                  MOVE      NRFWW         HSIDEG
     C*                  WRITE     RECOSJHI
     C*                  ENDIF
     C*    K531          READE     RECOSJHI                               99
     C*                  ENDDO
     c/exec sql
     C+ SET OPTION COMMIT=*NONE
     c/end-exec
     c/exec sql
     c+ insert into sdbfil/cosjhi
     c+ SELECT HSFAAM, HSIGCE, :nrfww, HSIDEg, HSICCD, HSICCR, HS$IMP * -1
     c+ , 0     , HS$A03, HS$A04,HS$A05, HSDF03, HSDF04, HSDF05 FROM
     c+ sdbfil/COSJHI WHERE HSFAAM = :AMFWW and hsfasi > 0
     c+
     c/end-exec
     C* ... Generamos asiento para esta liquidación
     C                   EXSR      OPNFIL
     C                   EXSR      LIQASI
     C                   EXSR      WRTPSA
     C                   EXSR      WRTPAP
     C                   EXSR      WRTART
     C                   EXSR      WRTIAC
     C                   EXSR      WRT814
     C                   EXSR      WRTSCV
     C                   EXSR      CLOFIL
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRT814 : ESCRIBE impuesto 814
     C*-------------------------------------------------------------------------
     C     WRT814        BEGSR
     C*
     C                   Z-ADD     10005         WWCENT
     C     KPARM         CHAIN     RECOPASJ                           99
     C   99              LEAVESR
     C*
     C     SJ$01A        DIV       10000         WWFAC1            9 5
     C     SJ$02A        DIV       10000         WWFAC2            9 5
     C     SJ$03A        DIV       10000         WWFAC3            9 5
     C*
     C/EXEC SQL
     C+ SELECT
     C+ cast (
     C+ sum(case when (ccf01 <= 11) or (ccf01 is null) then imf05 * :WWFAC1
     C+          when ccf01 > 11 and ccf01 <= 21 then imf05 * :WWFAC2
     C+          when ccf01 > 21 then imf05 * :WWFAC3 end)
     C+     as dec(15, 2)) d814 INTO :HS$IMP
     C+ FROM s@jf05
     C+ left join S@J/s@jf01 on lgf011=LGF05
     C+ left join COPASJ on CDF051 = SJCENT
     C+ WHERE
     C+   SJICCD = 5600030400  OR
     C+   SJICCD = 5600030430  OR
     C+   SJICCD = 5600090400
     C/END-EXEC
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   EVAL      HSICCD = 1711170500
     C                   EVAL      HSICCR = 5600060500
     C                   WRITE     RECOSJHI
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTIAC : ESCRIBE IMPUESTO AL CHEQUE
     C*-------------------------------------------------------------------------
     C     WRTIAC        BEGSR
     C*
     C                   Z-ADD     10004         WWCENT
     C     KPARM         CHAIN     RECOPASJ                           99
     C   99              LEAVESR
     C*
     C                   MOVE      AMFWW         MES               2 0
     C                   Z-ADD     *ZERO         BASCAL           15 2
     C     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        HSICCD = 5600030400 OR
     C                             HSICCD = 5600030430 OR
     C                             HSICCD = 5600030450 OR
     C                             HSICCD = 5600060400 OR
     C                             HSICCD = 5600090400 OR
     C                             HSICCD = 5600090410 OR
     C                             HSICCD = 3311120460 OR
     C                             HSICCD = 3311120470 OR
     C                             HSICCD = 3400030420 OR
     C                             HSICCD = 1711090540 OR
     C                             HSICCD = 3211540240
     C                   ADD       HS$IMP        BASCAL
     C                   ENDIF
     C                   IF        (MES=06 OR MES=12) AND (
     C                             HSICCD = 3311120460 OR
     C                             HSICCD = 3311120470    )
     C                   SUB       HS$IMP        BASCAL
     C                   ENDIF
     C     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C     SJ$01A        DIV       1000          WWFACT
     C     WWFACT        MULT      BASCAL        HS$IMP
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   EVAL      HSICCD = SJICCD
     C                   EVAL      HSICCR = SJICCR
     C                   WRITE     RECOSJHI
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTART : ESCRIBE PAGO A ART
     C*-------------------------------------------------------------------------
     C     WRTART        BEGSR
     C*
     C*
     C                   Z-ADD     10003         WWCENT
     C     KPARM         CHAIN     RECOPASJ                           99
     C   99              LEAVESR
     C                   Z-ADD     *ZERO         BASCAL           15 2
     C     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        HSICCD = 5600030400 OR
     C                             HSICCD = 5600030430 OR
     C                             HSICCD = 5600090400
     C                   ADD       HS$IMP        BASCAL
     C                   ENDIF
     C     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C                   Z-ADD     *ZERO         CANEMP           15 2
     C/EXEC SQL
     C+ SELECT COUNT(*) INTO :CANEMP FROM S@J/S@JF01 WHERE YEF013 = 0
     C/END-EXEC
     C     SJ$01A        DIV       100000        WWFACT            9 5
     C     BASCAL        MULT      WWFACT        HS$IMP
     C     SJ$02A        DIV       100           WWFACT
     C     WWFACT        MULT      CANEMP        IMPFIJ           15 2
     C                   ADD       IMPFIJ        HS$IMP
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   EVAL      HSICCD = SJICCD
     C                   EVAL      HSICCR = SJICCR
     C                   WRITE     RECOSJHI
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTSCV : ESCRIBE PAGO SEGURO COLECTIVO DE VIDA
     C*-------------------------------------------------------------------------
     C     WRTSCV        BEGSR
     C*
     C*
     C                   Z-ADD     10006         WWCENT
     C     KPARM         CHAIN     RECOPASJ                           99
     C   99              LEAVESR
     C                   Z-ADD     *ZERO         BASCAL           15 2
     C     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        HSICCD = 5600030400 OR
     C                             HSICCD = 5600030430 OR
     C                             HSICCD = 5600090400
     C                   ADD       HS$IMP        BASCAL
     C                   ENDIF
     C     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C                   Z-ADD     *ZERO         CANEMP           15 2
     C/EXEC SQL
     C+ SELECT COUNT(*) INTO :CANEMP FROM S@J/S@JF01 WHERE YEF013 = 0
     C/END-EXEC
     C     SJ$01A        DIV       100           WWFACT            9 5
     C     CANEMP        MULT      WWFACT        HS$IMP
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   EVAL      HSICCD = SJICCD
     C                   EVAL      HSICCR = SJICCR
     C                   WRITE     RECOSJHI
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTPSA : ESCRIBE PREVISIONES PARA SAC
     C*-------------------------------------------------------------------------
     C     WRTPSA        BEGSR
     C*
     C                   Z-ADD     *ZERO         HQISUC
     C                   Z-ADD     *ZERO         HQISUC
     C                   MOVEL     AMFWW         PAFECH
     C                   MOVE      '01'          PAFECH
     C                   CALL      'SBBAINFE'
     C                   PARM                    PAFECH
     C                   PARM      'IN'          EDTMDE            2
     C                   CALL      'SBBACFFM'
     C                   PARM                    PAFECH            8 0
     C                   PARM                    HQFSAL
     C                   CALL      'SBBAINFE'
     C                   PARM                    HQFSAL
     C                   PARM      'NI'          EDTMDE            2
     C*
     C                   Z-ADD     10001         WWCENT
     C     KPARM         CHAIN     RECOPASJ                           99
     C   99              LEAVESR
     C                   MOVE      AMFWW         MES               2 0
     C                   IF        MES=06 OR MES=12
     C                   EVAL      CTACOS=SJICCR
     C                   EVAL      HQIMON=1
     C     KCOME         SETGT     RECOMESA
     C     KCOM1         READPE    RECOMESA                               99
     C                   IF        *IN99 = *OFF
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   Z-ADD     HQ$SDI        HS$IMP
     c                   IF        HS$IMP < 0
     C                   MULT      -1            HS$IMP
     C                   ENDIF
     C                   EVAL      HSICCR = SJICCD
     C                   EVAL      HSICCD = SJICCR
     C                   WRITE     RECOSJHI
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     *ZERO         BASCAL           15 2
     C     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        HSICCD = 5600030400  OR
     C                             HSICCD = 5600030430  OR
     C                             HSICCD = 5600090400
     C                   ADD       HS$IMP        BASCAL
     C                   ENDIF
     C     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C     BASCAL        DIV       SJ$01A        HS$IMP
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   EVAL      HSICCD = SJICCD
     C                   EVAL      HSICCR = SJICCR
     C                   WRITE     RECOSJHI
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* WRTPAP : ESCRIBE PREVISIONES PARA APORTES PATRONALES
     C*-------------------------------------------------------------------------
     C     WRTPAP        BEGSR
     C*
     C                   Z-ADD     10002         WWCENT
     C     KPARM         CHAIN     RECOPASJ                           99
     C   99              LEAVESR
     C                   MOVE      AMFWW         MES               2 0
     C                   IF        MES=06 OR MES=12
     C                   EVAL      CTACOS=SJICCR
     C     KCOME         SETGT     RECOMESA
     C     KCOM1         READPE    RECOMESA                               99
     C                   IF        *IN99 = *OFF
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   Z-ADD     HQ$SDI        HS$IMP
     c                   IF        HS$IMP < 0
     C                   MULT      -1            HS$IMP
     C                   ENDIF
     C                   EVAL      HSICCR  = SJICCD
     C                   EVAL      HSICCD  = SJICCR
     C                   WRITE     RECOSJHI
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     *ZERO         BASCAL           15 2
     C     K533          CHAIN     RECOSJHI                           99
     C                   DOW       *IN99 = *OFF
     C                   IF        HSICCR  = 3311120400  OR
     C                             HSICCR  = 3311120410  OR
     C                             HSICCR  = 3311120510
     C                   ADD       HS$IMP        BASCAL
     C                   ENDIF
     C     K533          READE     RECOSJHI                               99
     C                   ENDDO
     C     BASCAL        DIV       SJ$01A        HS$IMP
     C                   MOVE      *ZERO         HSIDEV
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   EVAL      HSICCD = SJICCD
     C                   EVAL      HSICCR = SJICCR
     C                   WRITE     RECOSJHI
     C                   ENDIF
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* OPNFIL : ABRE ARCHIVOS DE LIQUIDACION
     C*-------------------------------------------------------------------------
     C     OPNFIL        BEGSR
     C*
     C                   EVAL      CMDSTR='DLTOVR *ALL'
     C                   CLOSE     S@JF05
     C                   EVAL      RC=SHELL(CMDSTR)
     C                   EVAL      CMDSTR='OVRDBF  S@JF05 '+%TRIM(LIBNME)+
     C                                                      '/S@JF05 MBR(' +
     C                                     GRFWW +'_'+AUXNLI +')'
     C                   EVAL      RC=SHELL(CMDSTR)
     C*
     C                   OPEN      S@JF05
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* CLOFIL : CIERRA ARCHIVOS
     C*-------------------------------------------------------------------------
     C     CLOFIL        BEGSR
     C*
     C                   CLOSE     S@JF05
     C*
     C                   EVAL      RC=SHELL('DLTOVR *ALL')
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* LIQASI : ASIENTO PARA LA LIQUIDACION ACTUAL
     C*-------------------------------------------------------------------------
     C     LIQASI        BEGSR
     C*
     C                   MOVE      AMFWW         HSFAAM
     C                   MOVE      GRFWW         HSIGCE
     C                   MOVE      NRFWW         HSIDEG
     C                   MOVE      *ZERO         HSIDEV
     C     *LOVAL        SETLL     RS@JF05
     C                   READ      RS@JF05                                99
     C     *IN99         DOWEQ     *OFF
     C                   MOVE(P)   CDF051        SJCENT
     C     KSJ01         CHAIN     RECOPASJ                           99
     C     *IN99         IFEQ      *OFF
     C     SJICCD        ANDNE     *ZERO
     C     SJICCR        ANDNE     *ZERO
     C                   MOVE      SJICCD        HSICCD
     C                   MOVE      SJICCR        HSICCR
     C     K536          CHAIN     RECOSJHI                           99
     C   99              MOVE      AMFWW         HSFAAM
     C   99              MOVE      GRFWW         HSIGCE
     C   99              MOVE      NRFWW         HSIDEG
     C   99              Z-ADD     IMF05         HS$IMP
     C   99              WRITE     RECOSJHI
     C  N99              ADD       IMF05         HS$IMP
     C  N99              UPDATE    RECOSJHI
     C                   ENDIF
     C                   READ      RS@JF05                                99
     C                   ENDDO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* ENDPGM : FIN DE PROGRAMA
     C*-------------------------------------------------------------------------
     C     ENDPGM        BEGSR
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C* *INZSR : INICIALIZACION
     C*-------------------------------------------------------------------------
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    IMPACT            1
     C*
     C     1             CHAIN     SGSYSV
     C                   MOVE      'A'           WWIGCE
     C*
     C     KPARM         KLIST
     C                   KFLD                    WWIGCE            1
     C                   KFLD                    WWCENT            5 0
     C     K531          KLIST
     C                   KFLD                    AMFWW
     C     K533          KLIST
     C                   KFLD                    AMFWW
     C                   KFLD                    GRFWW
     C                   KFLD                    NRFWW
     C     K536          KLIST
     C                   KFLD                    HSFAAM
     C                   KFLD                    HSIGCE
     C                   KFLD                    HSIDEG
     C                   KFLD                    HSIDEV
     C                   KFLD                    HSICCD
     C                   KFLD                    HSICCR
     C     K05090        KLIST
     C                   KFLD                    CDF051
     C                   KFLD                    CDF052
     C     KSJ01         KLIST
     C                   KFLD                    GRFWW
     C                   KFLD                    SJCENT
     C     KCOME         KLIST
     C                   KFLD                    HQISUC
     C                   KFLD                    HQICTL
     C                   KFLD                    HQIRUR
     C                   KFLD                    HQIMCO
     C                   KFLD                    HQINCC
     C                   KFLD                    HQISCC
     C                   KFLD                    HQIMON
     C                   KFLD                    HQFSAL
     C     KCOM1         KLIST
     C                   KFLD                    HQISUC
     C                   KFLD                    HQICTL
     C                   KFLD                    HQIRUR
     C                   KFLD                    HQIMCO
     C                   KFLD                    HQINCC
     C                   KFLD                    HQISCC
     C                   KFLD                    HQIMON
     C*
     C                   READ      COTARJ
     C*
     C                   ENDSR

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
