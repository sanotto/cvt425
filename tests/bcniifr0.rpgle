*SRCMBRTXT:NIIF proceso de calculo op con comision
     H DECEDIT('0,') DATEDIT(*YMD) DEBUG
     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE') EXPROPTS(*RESDECPOS)
     H OPTION(*NODEBUGIO)
     H GENLVL(5)
      *---------------------------------------------------------------------
      *  SYSTEM DESC: NIIF - Proceso de calculo 01
      *                Recorre registros de las instefaces de PR y procesa
      *  NAME : BCNIFFR0
      *  DATE : 06/06/2019
      *  AUTOR: desa02067
      *
      *---------------------------------------------------------------------
     FBCNII301  if   E           k DISK
     FPRCUOT01  if   E           k DISK
     FPRCRED    if   E           k DISK
     FSGSYSV    if   E             DISK
     f@CPIUSD   if   E           k DISK
     f@CPISYS   if   E           k DISK
     FBCTIR2    UF A E           k DISK
      *---------------------------------------------------------------------
     dPRSubsidio       pr                  extpgm('BCNII1R1')
     d                                8  0 const
      *---------------------------------------------------------------------
      * Declaracion de Matrices
     D cuotas          DS                  OCCURS(9999)
     D DifDias                 1     15  0
     D importeTIR             16     31  2
     D DifFechaD       S             15  0
     d @msg            S            240
     D WA$IMP          S             15  2 DIM(120)
     D WAFVTO          S             15  2 DIM(120)
     D WAMPAG          S             15  2 DIM(120)
     d @isub           S              2
     d @imcc           S              3  0
     d @immr           S              3  0
     d @ncod           S             30
     d @1cod           S             30
     d cont            S              3  0
     D wwqcuo          S              3P 0
     D wwTIR           S              8F
     D wwtea           S              8F
     D ValActNIIF      S              8F
     D ValActSCOM      S              8F
     D patna           S              8  4
     D wwpatea         S              8  4
     D wwtpac          S              8F
     D wwtna           S              8F
     D semilla         S              8F
     D NroCuota        S              3P 0
     D wwficc          S              8P 0
     D FechaVtoCuota   S              8P 0
     D ImpDesembolso   S             15P 2
     D FechaCierreAnt  S             15P 0
     D wn              S              8F
     D Error           S              8F
     D Divisor         S              8F
     D CambioSigno     S               N
     D PrimerItera     S               N
     D FechaDesde      S               D
     D FechaHasta      S               D
     D FechaChar       S             10
     D Caracter        S              8
     D Diferencia      S             15P 0
     D Iteraciones     S             15P 0
     D IteraParcial    S              7P 0
     d TopeItera       s              7p 0 inz(100000)
     d P1Limite        s              7p 0
     d P1Pos           s              7p 0
     d P1Ideg          s              4p 0
     d P1CantDesg      s              4p 0
     D LimiteInferior  S              8F
     D LimiteSuperior  S              8F
     D*paTIR           S             15P 4
     D paTIR           S              8F
     D TIRConComi      S              8F
     D TIRSinComi      S              8F
     D WWTOLE          S              8F   INZ(0,00000001)
     D importeTIRF     S              8F
      *
      *---------------------------------------------------------------------
      *... SDS Interna del equipo
      *... Valores del sistema
     D @@DS           SDS
     D  @@PGM                  1     10
     D  @@LIB                 81     90
     D  @@JOB                244    253
     D  @@USER               254    263
     D  @@JOBN               264    269
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     D FIDS            DS
     D  @IDCPO               370    371B 0
     D  @IDPOS               370    371B 0
     D  @IDRRN               397    400B 0
      *---------------------------------------------------------------------
      *FREE
      *
      ***********************************************************
      * cargo la fecha de corte anterior duro, pero luego deberia
      * venir por parametro.
      *
      **   FechaCierreAnt = 20190228;
           FechaCierreAnt = wwfulc;
      ***********************************************************
      *
           setll (wwfulc) BCNII301;
           reade (wwfulc) BCNII301;
           dow not %eof(BCNII301);


               // proceso solo los que tienen gastos de otorgamiento

               if PL$OTO <> 0 ;
               // Valores Iniciales para los limites del intervalo de tasa TIR
               // para calculo TIR No Per con comision
                   Error = 1;
                   PrimerItera = *on;
                   LimiteInferior = 0;
                   LimiteSuperior = 9999,999999;

                   exsr BlanqueoA;
                   exsr CargaA;
                   exsr procConComi;
                   exsr CalcVANCCom;

                   exsr CalcTEA;

               // Valores Iniciales para los limites del intervalo de tasa TIR
               // para calculo TIR No Per sin comision
                   Error = 1;
                   PrimerItera = *on;
                   LimiteInferior = 0;
                   LimiteSuperior = 9999,999999;

                   exsr procSinComi;
                   exsr CalcVANSCom;

                   exsr GrabaBCTIR;
               endif;

               reade (wwfulc) BCNII301;
           enddo;

           callp PRSubsidio(wwfulc);

           *inlr = *on;
      *-------------------------------------------------------------------
      * selecciona las operaciones sobre las que se va a trabajar
      * Calcula TIR TIE NIIF Con efecto comisiones.
      *-------------------------------------------------------------------
         begsr procConComi;
      * Calculo de T.I.R. en base a T.N.A.
      * chequeo que la tasa sea distinta de 0
      *
     C                   IF        PLTPAC <> 0
      * Calculos Previos para TIR
     C                   EVAL      wwTPAC= PLTPAC
     C                   EVAL      wwtna = wwTPAC/ 100
     C                   EVAL      semilla = wwtna * (3/2)
     C                   EVAL      wwTIR = 0
     C                   EVAL      NroCuota = 0
      *
      * Calculo el capital NIIF, que es capital-comision
      *
     C***                IF        PLINCR <> 271106
     C                   EVAL      ImpDesembolso = PL$INP - PL$OTO
     C***                ELSE
      * caso especial con monto muy grande, solución temporal hasta
      * encontrar la solucion definitiva.
     C***                EVAL      ImpDesembolso = PL$INP - PL$OTO  + 7
     C****               EVAL      ImpDesembolso = PL$INP - PL$OTO
     C***                ENDIF
      *
      *...Se define la tolerancia segun el monto de la operacion
     C                   IF        ImpDesembolso > 100000000
     C                   EVAL      wwtole = 0,00000002
     C                   ELSE
     C                   EVAL      wwtole = 0,00000001
     C                   ENDIF
      *
     C                   EVAL      TI$CAD = ImpDesembolso
      * Luego lo multiplico por (-1) para poder hacer el cáculo
      * de la TIR.
     C                   EVAL      ImpDesembolso = ImpDesembolso * (-1)
      *
     C                   IF        ImpDesembolso <> 0
      *
     C                   EXSR      CargoCuotas
      *
     C                   EVAL      Iteraciones = 0
      * Itero hasta la cota de error establecida
     C                   DOW       %ABS( Error ) > wwtole
     C                             AND Iteraciones <= TopeItera
     c**
     C                   IF        NOT PrimerItera
      *
     C                   IF        CambioSigno
     C                   IF        Error > 0
     C                   EVAL      LimiteInferior = semilla
     C                   EVAL      semilla=(LimiteInferior+LimiteSuperior)/2
     C                   ELSE
     C                   EVAL      LimiteSuperior = semilla
     C                   EVAL      semilla=(LimiteInferior+LimiteSuperior)/2
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   IF        Error > 0
     C                   EVAL      LimiteInferior = semilla
     C                   MONITOR
     C                   EVAL      semilla=semilla + semilla/2
     C                   ON-ERROR  *all
     C                   EVAL      semilla=semilla + 0
     C                   ENDMON
     C                   ELSE
     C                   EVAL      LimiteSuperior = semilla
     C                   MONITOR
     C                   EVAL      semilla=semilla - semilla/4
     C                   ON-ERROR  *all
     C                   EVAL      semilla=semilla - 0
     C                   ENDMON
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      * Calculo de TIR
     C                   EVAL      wn = 0
     C                   EVAL      wwTIR = wwTIR + ( ImpDesembolso /
     C                             ( 1 + semilla ) ** wn )
      *
     C     1             DO        wwqcuo        NroCuota
      *
     C     kcuot         CHAIN     prcuot01
      *
     C     NroCuota      OCCUR     cuotas
      *
      * dividir por 360 o 365 segun corresponda
     C                   EVAL      wn = DifDias/365
     C                   EVAL      Divisor = ( (1 + semilla ) ** wn )
     C                   EVAL      ImporteTIRF = ImporteTIR
     C                   EVAL      wwTIR = wwTIR + ( importeTIRF/ Divisor)
      *
     C                   ENDDO
      *
      * No cambio de signo el valor actual con la Iteracion anterior
     C                   IF        PrimerItera
     C                   EVAL      PrimerItera = *off
     C                   EVAL      Error = wwTIR
     C                   ENDIF
     C                   IF        wwTIR * Error < 0 OR CambioSigno
     C                   IF        NOT CambioSigno
     C                   EVAL      CambioSigno = *on
     C                   ENDIF
     C                   ENDIF
      *
     C                   EVAL      Error = wwTIR
     C                   EVAL      wwTIR = 0
     C                   EVAL      Iteraciones = Iteraciones + 1
      *
     C                   ENDDO
     C                   ENDIF
      *
      * Devuelve valor de TIR
     C**
      *  Si llego al maximo de iteraciones y la cota de error no supera un limit
      *  Tomo el valor de TIR como valido, por calculo con valores extremos
     c                   IF        Iteraciones >= TopeItera AND
     c                             %ABS( Error ) <= wwtole
     c                   EVAL      Error = 0
     c                   ENDIF
     C                   IF        %ABS( Error ) > wwtole
      * Significa que salio por nro maximo de iteraciones ......
     C                   EVAL      paTIR = 0
      *
     C                   ELSE
     C                   MONITOR
      *  Informo TIR Efectivo
     C                   EVAL      paTIR  = semilla * 100
     C                   ON-ERROR  *all
     C                   EVAL      paTIR = 0
     C                   ENDMON
     C                   ENDIF
      *
     C                   ELSE
      * Si la tasa es cero .....
     C                   EVAL      paTIR = 0
     C                   ENDIF
     C                   EVAL      TIRConComi = paTIR

         endsr;

      *-------------------------------------------------------------------
      * selecciona las operaciones sobre las que se va a trabajar
      * Calcula TIR TIE NIIF Sin efecto comisiones.
      *-------------------------------------------------------------------
         begsr procSinComi;
      * Calculo de T.I.R. en base a T.N.A.
      * chequeo que la tasa sea distinta de 0
      *
     C                   IF        PLTPAC <> 0
      * Calculos Previos para TIR
     C                   EVAL      wwTPAC= PLTPAC
     C                   EVAL      wwtna = wwTPAC/ 100
     C                   EVAL      semilla = wwtna * (3/2)
     C                   EVAL      wwTIR = 0
     C                   EVAL      NroCuota = 0
      *
      * Calculo el capital NIIF, que es capital-comision
      *
     C                   EVAL      ImpDesembolso = PL$INP
      * Luego lo multiplico por (-1) para poder hacer el cáculo
      * de la TIR.
      *
      *...Se define la tolerancia segun el monto de la operacion
     C                   IF        ImpDesembolso > 100000000
     C                   EVAL      wwtole = 0,00000002
     C                   ELSE
     C                   EVAL      wwtole = 0,00000001
     C                   ENDIF
     C                   EVAL      ImpDesembolso = ImpDesembolso * (-1)
      *
     C                   IF        ImpDesembolso <> 0
      *
     C                   EXSR      CargoCuotas
      *
     C                   EVAL      Iteraciones = 0
      * Itero hasta la cota de error establecida
     C                   DOW       %ABS( Error ) > wwtole
     C                             AND Iteraciones <= TopeItera
     c**
     C                   IF        NOT PrimerItera
      *
     C                   IF        CambioSigno
     C                   IF        Error > 0
     C                   EVAL      LimiteInferior = semilla
     C                   EVAL      semilla=(LimiteInferior+LimiteSuperior)/2
     C                   ELSE
     C                   EVAL      LimiteSuperior = semilla
     C                   EVAL      semilla=(LimiteInferior+LimiteSuperior)/2
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   IF        Error > 0
     C                   EVAL      LimiteInferior = semilla
     C                   MONITOR
     C                   EVAL      semilla=semilla + semilla/2
     C                   ON-ERROR  *all
     C                   EVAL      semilla=semilla + 0
     C                   ENDMON
     C                   ELSE
     C                   EVAL      LimiteSuperior = semilla
     C                   MONITOR
     C                   EVAL      semilla=semilla - semilla/4
     C                   ON-ERROR  *all
     C                   EVAL      semilla=semilla - 0
     C                   ENDMON
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      * Calculo de TIR
     C                   EVAL      wn = 0
     C                   EVAL      wwTIR = wwTIR + ( ImpDesembolso /
     C                             ( 1 + semilla ) ** wn )
      *
     C     1             DO        wwqcuo        NroCuota
      *
     C     kcuot         CHAIN     prcuot01
      *
     C     NroCuota      OCCUR     cuotas
      *
      * dividir por 360 o 365 segun corresponda
     C                   EVAL      wn = DifDias/365
     C                   EVAL      Divisor = ( (1 + semilla ) ** wn )
     C                   EVAL      ImporteTIRF = ImporteTIR
     C                   EVAL      wwTIR = wwTIR + ( importeTIRF/ Divisor)
      *
     C                   ENDDO
      *
      * No cambio de signo el valor actual con la Iteracion anterior
     C                   IF        PrimerItera
     C                   EVAL      PrimerItera = *off
     C                   EVAL      Error = wwTIR
     C                   ENDIF
     C                   IF        wwTIR * Error < 0 OR CambioSigno
     C                   IF        NOT CambioSigno
     C                   EVAL      CambioSigno = *on
     C                   ENDIF
     C                   ENDIF
      *
     C                   EVAL      Error = wwTIR
     C                   EVAL      wwTIR = 0
     C                   EVAL      Iteraciones = Iteraciones + 1
      *
     C                   ENDDO
     C                   ENDIF
      *
      * Devuelve valor de TIR
     C**
      *  Si llego al maximo de iteraciones y la cota de error no supera un limit
      *  Tomo el valor de TIR como valido, por calculo con valores extremos
     c                   IF        Iteraciones >= TopeItera AND
     c                             %ABS( Error ) <= wwtole
     c                   EVAL      Error = 0
     c                   ENDIF
     C                   IF        %ABS( Error ) > wwtole
      * Significa que salio por nro maximo de iteraciones ......
     C                   EVAL      paTIR = 0
      *
     C                   ELSE
     C                   MONITOR
      *  Informo TIR Efectivo
     C                   EVAL      paTIR  = semilla * 100
     C                   ON-ERROR  *all
     C                   EVAL      paTIR = 0
     C                   ENDMON
     C                   ENDIF
      *
     C                   ELSE
      * Si la tasa es cero .....
     C                   EVAL      paTIR = 0
     C                   ENDIF
     C                   EVAL      TIRSinComi = paTIR

         endsr;
      *-------------------------------------------------------------------
      * Calculo la Tasa Efectiva Anual                                       com
      * TEA = =(1+TNA/(365/30))¢(365/30)-1
      *-------------------------------------------------------------------
         begsr CalcTEA;

      * accedo a PRCRED para obtener los dias de servicio del PR
     C     @key02        CHAIN     prcred
     C                   EVAL      wwTPAC= PLTPAC
     C                   EVAL      wwtna = wwTPAC/ 100
     c                   EVAL      wn = 365/jvqdsv
     C                   EVAL      wwtea = ( ( 1 + wwtna / wn ) ** wn ) - 1
     C                   EVAL      wwpatea = wwtea * 100
     C                   EVAL      TITEAA = wwpatea

         endsr;
      *
      *-------------------------------------------------------------------
      * Calcula el Valor Actual NIIF con la TIR obtenida  (TIE NIFF C/Efecto com
      * SALDO NIIF CON EFECTO COMISIONES
      *-------------------------------------------------------------------
         begsr CalcVANCCom;
      *
     C                   EVAL      TI$VAN = 0
     C                   IF        TIRConComi <> 0
      *
      * Tengo que calcular el Valor Actual NIIF por cada cuota
      * y lo voy acumulando
      *
     C     @key02        SETLL     prcuot01
     C     @key02        READE     prcuot01
     C                   IF        %equal
     C                   DOW       not %eof
      *
      * Calculo si la cuota no está vencida
     C                   if        WAFVTO(kgicuo) > FechaCierreAnt
     C                   EVAL      wwficc = FechaCierreAnt
     C                   EXSR      DiferenciaFech
      *
     C                   EVAL      DifFechaD = Diferencia
     C                   EVAL      wn = DifFechaD / 365
     C                   EVAL      Divisor = ( (1 + (TIRConComi/100)) ** wn )
      *
     C
     C                   EVAL      ValActNIIF = PL$CAV + PL$IIV +
     C                             (WA$IMP(kgicuo)/Divisor)
      *
     C                   EVAL      TI$VAN = TI$VAN + ValActNIIF
      *
     C                   ENDIF
      *
     C     @key02        READE     prcuot01
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR

      *-------------------------------------------------------------------
      * Calcula el Valor Actual NIIF con la TIR obtenida  (TIE NIFF C/Efecto com
      * SALDO NIIF SIN EFECTO COMISIONES
      *-------------------------------------------------------------------
         begsr CalcVANSCom;
      *
     C                   EVAL      TI$VAS = 0
     C                   IF        TIRSinComi <> 0
      *
      * Tengo que calcular el Valor Actual NIIF por cada cuota
      * y lo voy acumulando
      *
     C     @key02        SETLL     prcuot01
     C     @key02        READE     prcuot01
     C                   IF        %equal
     C                   DOW       not %eof
      *
      * Calculo si la cuota no está vencida
     C                   if        WAFVTO(kgicuo) > FechaCierreAnt
     C                   EVAL      wwficc = FechaCierreAnt
     C                   EXSR      DiferenciaFech
      *
     C                   EVAL      DifFechaD = Diferencia
     C                   EVAL      wn = DifFechaD / 365
     C                   EVAL      Divisor = ( (1 + (TIRSinComi/100)) ** wn )
      *
     C
     C                   EVAL      ValActSCOM = PL$CAV + PL$IIV +
     C                             (WA$IMP(kgicuo)/Divisor)
      *
     C                   EVAL      TI$VAS = TI$VAS + ValActSCOM
      *
     C                   ENDIF
      *
     C     @key02        READE     prcuot01
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR

      *-------------------------------------------------------------------------
      * Calcula la Diferencia entre dos fechas - DiferenciaFech
      *-------------------------------------------------------------------------
     C     DiferenciaFechBEGSR
      *
     C                   EVAL      caracter  = %CHAR( wwficc )
     C                   EVAL      FechaChar = %SUBST(caracter :1 :4) +'-'  +
     C                                          %SUBST(caracter :5 :2) +'-' +
     C                                          %SUBST(caracter :7 :2)
     C     *ISO          TEST(DE)                FechaChar
     C                   IF        NOT %ERROR
     C                   MOVE      FechaChar     FechaDesde
     C                   EVAL      caracter  = %CHAR( WAFVTO(KGICUO))
     C                   EVAL      FechaChar = %SUBST(caracter :1 :4) +'-'  +
     C                                          %SUBST(caracter :5 :2) +'-' +
     C                                          %SUBST(caracter :7 :2)
     C     *ISO          TEST(DE)                FechaChar
     C                   IF        NOT %ERROR
     C                   MOVE      FechaChar     FechaHasta
     C     FechaHasta    SUBDUR    FechaDesde    Diferencia:*d
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *-------------------------------------------------------------------------
      * Cargo Cuotas - Subrutina de Carga de datos de cuotas p/Cal. TIR
      *-------------------------------------------------------------------------
     C     CargoCuotas   BEGSR
      *
     C                   EVAL      TI$TOC = 0
      *
      * Parametrizacion de Tipo de IVA que se incluye en TIR    - hjicon='S'
     C     @key02        SETLL     prcuot01
     C     @key02        READE     prcuot01
     C                   EVAL      wwficc = plfasi
     C                   IF        %equal
     C                   DOW       not %eof
      * Guardo los importes por cuotas para el calculo de TIR
     C     kgicuo        OCCUR     cuotas
      *
     C                   EXSR      DiferenciaFech
     C                   EVAL      DifDias = Diferencia
     C                   EVAL      importeTIR = WA$IMP(kgicuo)
     C                   EVAL      TI$TOC = TI$TOC + WA$IMP(kgicuo)
      *
     C     @key02        READE     prcuot01
     C                   ENDDO
     C                   EVAL      wwqcuo = kgicuo
     C                   ENDIF
      *
     C                   ENDSR
      *
      *-------------------------------------------------------------------
      * CFT/TIR
      *-------------------------------------------------------------------
         begsr TIR;

         endsr;
      *-------------------------------------------------------------------
      * Blanqueo ARRAYS cuotas
      *-------------------------------------------------------------------
         begsr BlanqueoA;
           wwqcuo = 0 ;
           Cont = 1;
           dow Cont <= 120;
               WA$IMP(Cont) = 0;
               WAFVTO(Cont) = 0;
               WAMPAG(Cont) = 0;
               Cont = Cont + 1;
           enddo;
         endsr;

      *-------------------------------------------------------------------
      * Carga ARRAYS cuotas
      *-------------------------------------------------------------------
         begsr CargaA;
           // Guarda cantidad de cuotas de la operacion
           wwqcuo = PLQCUO;

           WA$IMP(001) = PLC001;
           WA$IMP(002) = PLC002;
           WA$IMP(003) = PLC003;
           WA$IMP(004) = PLC004;
           WA$IMP(005) = PLC005;
           WA$IMP(006) = PLC006;
           WA$IMP(007) = PLC007;
           WA$IMP(008) = PLC008;
           WA$IMP(009) = PLC009;
           WA$IMP(010) = PLC010;
           WA$IMP(011) = PLC011;
           WA$IMP(012) = PLC012;
           WA$IMP(013) = PLC013;
           WA$IMP(014) = PLC014;
           WA$IMP(015) = PLC015;
           WA$IMP(016) = PLC016;
           WA$IMP(017) = PLC017;
           WA$IMP(018) = PLC018;
           WA$IMP(019) = PLC019;
           WA$IMP(020) = PLC020;
           WA$IMP(021) = PLC021;
           WA$IMP(022) = PLC022;
           WA$IMP(023) = PLC023;
           WA$IMP(024) = PLC024;
           WA$IMP(025) = PLC025;
           WA$IMP(026) = PLC026;
           WA$IMP(027) = PLC027;
           WA$IMP(028) = PLC028;
           WA$IMP(029) = PLC029;
           WA$IMP(030) = PLC030;
           WA$IMP(031) = PLC031;
           WA$IMP(032) = PLC032;
           WA$IMP(033) = PLC033;
           WA$IMP(034) = PLC034;
           WA$IMP(035) = PLC035;
           WA$IMP(036) = PLC036;
           WA$IMP(037) = PLC037;
           WA$IMP(038) = PLC038;
           WA$IMP(039) = PLC039;
           WA$IMP(040) = PLC040;
           WA$IMP(041) = PLC041;
           WA$IMP(042) = PLC042;
           WA$IMP(043) = PLC043;
           WA$IMP(044) = PLC044;
           WA$IMP(045) = PLC045;
           WA$IMP(046) = PLC046;
           WA$IMP(047) = PLC047;
           WA$IMP(048) = PLC048;
           WA$IMP(049) = PLC049;
           WA$IMP(050) = PLC050;
           WA$IMP(051) = PLC051;
           WA$IMP(052) = PLC052;
           WA$IMP(053) = PLC053;
           WA$IMP(054) = PLC054;
           WA$IMP(055) = PLC055;
           WA$IMP(056) = PLC056;
           WA$IMP(057) = PLC057;
           WA$IMP(058) = PLC058;
           WA$IMP(059) = PLC059;
           WA$IMP(060) = PLC060;
           WA$IMP(061) = PLC061;
           WA$IMP(062) = PLC062;
           WA$IMP(063) = PLC063;
           WA$IMP(064) = PLC064;
           WA$IMP(065) = PLC065;
           WA$IMP(066) = PLC066;
           WA$IMP(067) = PLC067;
           WA$IMP(068) = PLC068;
           WA$IMP(069) = PLC069;
           WA$IMP(070) = PLC070;
           WA$IMP(071) = PLC071;
           WA$IMP(072) = PLC072;
           WA$IMP(073) = PLC073;
           WA$IMP(074) = PLC074;
           WA$IMP(075) = PLC075;
           WA$IMP(076) = PLC076;
           WA$IMP(077) = PLC077;
           WA$IMP(078) = PLC078;
           WA$IMP(079) = PLC079;
           WA$IMP(080) = PLC080;
           WA$IMP(081) = PLC081;
           WA$IMP(082) = PLC082;
           WA$IMP(083) = PLC083;
           WA$IMP(084) = PLC084;
           WA$IMP(085) = PLC085;
           WA$IMP(086) = PLC086;
           WA$IMP(087) = PLC087;
           WA$IMP(088) = PLC088;
           WA$IMP(089) = PLC089;
           WA$IMP(090) = PLC090;
           WA$IMP(091) = PLC091;
           WA$IMP(092) = PLC092;
           WA$IMP(093) = PLC093;
           WA$IMP(094) = PLC094;
           WA$IMP(095) = PLC095;
           WA$IMP(096) = PLC096;
           WA$IMP(097) = PLC097;
           WA$IMP(098) = PLC098;
           WA$IMP(099) = PLC099;
           WA$IMP(100) = PLC100;
           WA$IMP(101) = PLC101;
           WA$IMP(102) = PLC102;
           WA$IMP(103) = PLC103;
           WA$IMP(104) = PLC104;
           WA$IMP(105) = PLC105;
           WA$IMP(106) = PLC106;
           WA$IMP(107) = PLC107;
           WA$IMP(108) = PLC108;
           WA$IMP(109) = PLC109;
           WA$IMP(110) = PLC110;
           WA$IMP(111) = PLC111;
           WA$IMP(112) = PLC112;
           WA$IMP(113) = PLC113;
           WA$IMP(114) = PLC114;
           WA$IMP(115) = PLC115;
           WA$IMP(116) = PLC116;
           WA$IMP(117) = PLC117;
           WA$IMP(118) = PLC118;
           WA$IMP(119) = PLC119;
           WA$IMP(120) = PLC120;

           WAFVTO(001) = PLF001;
           WAFVTO(002) = PLF002;
           WAFVTO(003) = PLF003;
           WAFVTO(004) = PLF004;
           WAFVTO(005) = PLF005;
           WAFVTO(006) = PLF006;
           WAFVTO(007) = PLF007;
           WAFVTO(008) = PLF008;
           WAFVTO(009) = PLF009;
           WAFVTO(010) = PLF010;
           WAFVTO(011) = PLF011;
           WAFVTO(012) = PLF012;
           WAFVTO(013) = PLF013;
           WAFVTO(014) = PLF014;
           WAFVTO(015) = PLF015;
           WAFVTO(016) = PLF016;
           WAFVTO(017) = PLF017;
           WAFVTO(018) = PLF018;
           WAFVTO(019) = PLF019;
           WAFVTO(020) = PLF020;
           WAFVTO(021) = PLF021;
           WAFVTO(022) = PLF022;
           WAFVTO(023) = PLF023;
           WAFVTO(024) = PLF024;
           WAFVTO(025) = PLF025;
           WAFVTO(026) = PLF026;
           WAFVTO(027) = PLF027;
           WAFVTO(028) = PLF028;
           WAFVTO(029) = PLF029;
           WAFVTO(030) = PLF030;
           WAFVTO(031) = PLF031;
           WAFVTO(032) = PLF032;
           WAFVTO(033) = PLF033;
           WAFVTO(034) = PLF034;
           WAFVTO(035) = PLF035;
           WAFVTO(036) = PLF036;
           WAFVTO(037) = PLF037;
           WAFVTO(038) = PLF038;
           WAFVTO(039) = PLF039;
           WAFVTO(040) = PLF040;
           WAFVTO(041) = PLF041;
           WAFVTO(042) = PLF042;
           WAFVTO(043) = PLF043;
           WAFVTO(044) = PLF044;
           WAFVTO(045) = PLF045;
           WAFVTO(046) = PLF046;
           WAFVTO(047) = PLF047;
           WAFVTO(048) = PLF048;
           WAFVTO(049) = PLF049;
           WAFVTO(050) = PLF050;
           WAFVTO(051) = PLF051;
           WAFVTO(052) = PLF052;
           WAFVTO(053) = PLF053;
           WAFVTO(054) = PLF054;
           WAFVTO(055) = PLF055;
           WAFVTO(056) = PLF056;
           WAFVTO(057) = PLF057;
           WAFVTO(058) = PLF058;
           WAFVTO(059) = PLF059;
           WAFVTO(060) = PLF060;
           WAFVTO(061) = PLF061;
           WAFVTO(062) = PLF062;
           WAFVTO(063) = PLF063;
           WAFVTO(064) = PLF064;
           WAFVTO(065) = PLF065;
           WAFVTO(066) = PLF066;
           WAFVTO(067) = PLF067;
           WAFVTO(068) = PLF068;
           WAFVTO(069) = PLF069;
           WAFVTO(070) = PLF070;
           WAFVTO(071) = PLF071;
           WAFVTO(072) = PLF072;
           WAFVTO(073) = PLF073;
           WAFVTO(074) = PLF074;
           WAFVTO(075) = PLF075;
           WAFVTO(076) = PLF076;
           WAFVTO(077) = PLF077;
           WAFVTO(078) = PLF078;
           WAFVTO(079) = PLF079;
           WAFVTO(080) = PLF080;
           WAFVTO(081) = PLF081;
           WAFVTO(082) = PLF082;
           WAFVTO(083) = PLF083;
           WAFVTO(084) = PLF084;
           WAFVTO(085) = PLF085;
           WAFVTO(086) = PLF086;
           WAFVTO(087) = PLF087;
           WAFVTO(088) = PLF088;
           WAFVTO(089) = PLF089;
           WAFVTO(090) = PLF090;
           WAFVTO(091) = PLF091;
           WAFVTO(092) = PLF092;
           WAFVTO(093) = PLF093;
           WAFVTO(094) = PLF094;
           WAFVTO(095) = PLF095;
           WAFVTO(096) = PLF096;
           WAFVTO(097) = PLF097;
           WAFVTO(098) = PLF098;
           WAFVTO(099) = PLF099;
           WAFVTO(100) = PLF100;
           WAFVTO(101) = PLF101;
           WAFVTO(102) = PLF102;
           WAFVTO(103) = PLF103;
           WAFVTO(104) = PLF104;
           WAFVTO(105) = PLF105;
           WAFVTO(106) = PLF106;
           WAFVTO(107) = PLF107;
           WAFVTO(108) = PLF108;
           WAFVTO(109) = PLF109;
           WAFVTO(110) = PLF110;
           WAFVTO(111) = PLF111;
           WAFVTO(112) = PLF112;
           WAFVTO(113) = PLF113;
           WAFVTO(114) = PLF114;
           WAFVTO(115) = PLF115;
           WAFVTO(116) = PLF116;
           WAFVTO(117) = PLF117;
           WAFVTO(118) = PLF118;
           WAFVTO(119) = PLF119;
           WAFVTO(120) = PLF120;

           WAMPAG(001) = PLM001;
           WAMPAG(002) = PLM002;
           WAMPAG(003) = PLM003;
           WAMPAG(004) = PLM004;
           WAMPAG(005) = PLM005;
           WAMPAG(006) = PLM006;
           WAMPAG(007) = PLM007;
           WAMPAG(008) = PLM008;
           WAMPAG(009) = PLM009;
           WAMPAG(010) = PLM010;
           WAMPAG(011) = PLM011;
           WAMPAG(012) = PLM012;
           WAMPAG(013) = PLM013;
           WAMPAG(014) = PLM014;
           WAMPAG(015) = PLM015;
           WAMPAG(016) = PLM016;
           WAMPAG(017) = PLM017;
           WAMPAG(018) = PLM018;
           WAMPAG(019) = PLM019;
           WAMPAG(020) = PLM020;
           WAMPAG(021) = PLM021;
           WAMPAG(022) = PLM022;
           WAMPAG(023) = PLM023;
           WAMPAG(024) = PLM024;
           WAMPAG(025) = PLM025;
           WAMPAG(026) = PLM026;
           WAMPAG(027) = PLM027;
           WAMPAG(028) = PLM028;
           WAMPAG(029) = PLM029;
           WAMPAG(030) = PLM030;
           WAMPAG(031) = PLM031;
           WAMPAG(032) = PLM032;
           WAMPAG(033) = PLM033;
           WAMPAG(034) = PLM034;
           WAMPAG(035) = PLM035;
           WAMPAG(036) = PLM036;
           WAMPAG(037) = PLM037;
           WAMPAG(038) = PLM038;
           WAMPAG(039) = PLM039;
           WAMPAG(040) = PLM040;
           WAMPAG(041) = PLM041;
           WAMPAG(042) = PLM042;
           WAMPAG(043) = PLM043;
           WAMPAG(044) = PLM044;
           WAMPAG(045) = PLM045;
           WAMPAG(046) = PLM046;
           WAMPAG(047) = PLM047;
           WAMPAG(048) = PLM048;
           WAMPAG(049) = PLM049;
           WAMPAG(050) = PLM050;
           WAMPAG(051) = PLM051;
           WAMPAG(052) = PLM052;
           WAMPAG(053) = PLM053;
           WAMPAG(054) = PLM054;
           WAMPAG(055) = PLM055;
           WAMPAG(056) = PLM056;
           WAMPAG(057) = PLM057;
           WAMPAG(058) = PLM058;
           WAMPAG(059) = PLM059;
           WAMPAG(060) = PLM060;
           WAMPAG(061) = PLM061;
           WAMPAG(062) = PLM062;
           WAMPAG(063) = PLM063;
           WAMPAG(064) = PLM064;
           WAMPAG(065) = PLM065;
           WAMPAG(066) = PLM066;
           WAMPAG(067) = PLM067;
           WAMPAG(068) = PLM068;
           WAMPAG(069) = PLM069;
           WAMPAG(070) = PLM070;
           WAMPAG(071) = PLM071;
           WAMPAG(072) = PLM072;
           WAMPAG(073) = PLM073;
           WAMPAG(074) = PLM074;
           WAMPAG(075) = PLM075;
           WAMPAG(076) = PLM076;
           WAMPAG(077) = PLM077;
           WAMPAG(078) = PLM078;
           WAMPAG(079) = PLM079;
           WAMPAG(080) = PLM080;
           WAMPAG(081) = PLM081;
           WAMPAG(082) = PLM082;
           WAMPAG(083) = PLM083;
           WAMPAG(084) = PLM084;
           WAMPAG(085) = PLM085;
           WAMPAG(086) = PLM086;
           WAMPAG(087) = PLM087;
           WAMPAG(088) = PLM088;
           WAMPAG(089) = PLM089;
           WAMPAG(090) = PLM090;
           WAMPAG(091) = PLM091;
           WAMPAG(092) = PLM092;
           WAMPAG(093) = PLM093;
           WAMPAG(094) = PLM094;
           WAMPAG(095) = PLM095;
           WAMPAG(096) = PLM096;
           WAMPAG(097) = PLM097;
           WAMPAG(098) = PLM098;
           WAMPAG(099) = PLM099;
           WAMPAG(100) = PLM100;
           WAMPAG(101) = PLM101;
           WAMPAG(102) = PLM102;
           WAMPAG(103) = PLM103;
           WAMPAG(104) = PLM104;
           WAMPAG(105) = PLM105;
           WAMPAG(106) = PLM106;
           WAMPAG(107) = PLM107;
           WAMPAG(108) = PLM108;
           WAMPAG(109) = PLM109;
           WAMPAG(110) = PLM110;
           WAMPAG(111) = PLM111;
           WAMPAG(112) = PLM112;
           WAMPAG(113) = PLM113;
           WAMPAG(114) = PLM114;
           WAMPAG(115) = PLM115;
           WAMPAG(116) = PLM116;
           WAMPAG(117) = PLM117;
           WAMPAG(118) = PLM118;
           WAMPAG(119) = PLM119;
           WAMPAG(120) = PLM120;
         endsr;

      *---------------------------------------------------------------------
      * Rutina de inicio
      *---------------------------------------------------------------------

         begsr GrabaBCTIR;

           TIFCIE = wwfulc;
           TI$ANI = TI$VAN - TI$VAS;
           TI$ANS = 0;
           TI$SUP = 0;
           TIISUB = PLISUB;
           TIISUC = PLISUC;
           TIINCR = PLINCR;
           TIIDEG = PLIDEG;
           TIILCR = PLILCR;
           TITIRC = TIRSinComi;
           TITIRN = TIRConComi;
           TIICUI = PLICUI;
           TINDNN = PLNDNN;
           TIFASI = PLFASI;
           TIDPLN = PLDPLN;
           TINMON = PLNMON;
           TITPAC = PLTPAC;
           TI$INP = PL$INP;
           TIQCUO = WWQCUO;
           TIQCAV = PLQCAV;
           TI$OTO = PL$OTO;

         // guarda valor de comision para contabilizar si
         // la fecha de alta es posterior a la fecha del cierre
         // anterior.

           if PLFASI > wwfant;
              if PLFASI <= 20190930;
                   TI$COP = PL$OTO;
                 else;
                  if PLFASI > 20190930 and (PLILCR < 2700 or PLILCR > 2799);
                   TI$COP = PL$OTO;
                  endif;
              endif;
            else;
              TI$COP = 0;
           endif;

           TIC001      = PLC001;
           TIC002      = PLC002;
           TIC003      = PLC003;
           TIC004      = PLC004;
           TIC005      = PLC005;
           TIC006      = PLC006;
           TIC007      = PLC007;
           TIC008      = PLC008;
           TIC009      = PLC009;
           TIC010      = PLC010;
           TIC011      = PLC011;
           TIC012      = PLC012;
           TIC013      = PLC013;
           TIC014      = PLC014;
           TIC015      = PLC015;
           TIC016      = PLC016;
           TIC017      = PLC017;
           TIC018      = PLC018;
           TIC019      = PLC019;
           TIC020      = PLC020;
           TIC021      = PLC021;
           TIC022      = PLC022;
           TIC023      = PLC023;
           TIC024      = PLC024;
           TIC025      = PLC025;
           TIC026      = PLC026;
           TIC027      = PLC027;
           TIC028      = PLC028;
           TIC029      = PLC029;
           TIC030      = PLC030;
           TIC031      = PLC031;
           TIC032      = PLC032;
           TIC033      = PLC033;
           TIC034      = PLC034;
           TIC035      = PLC035;
           TIC036      = PLC036;
           TIC037      = PLC037;
           TIC038      = PLC038;
           TIC039      = PLC039;
           TIC040      = PLC040;
           TIC041      = PLC041;
           TIC042      = PLC042;
           TIC043      = PLC043;
           TIC044      = PLC044;
           TIC045      = PLC045;
           TIC046      = PLC046;
           TIC047      = PLC047;
           TIC048      = PLC048;
           TIC049      = PLC049;
           TIC050      = PLC050;
           TIC051      = PLC051;
           TIC052      = PLC052;
           TIC053      = PLC053;
           TIC054      = PLC054;
           TIC055      = PLC055;
           TIC056      = PLC056;
           TIC057      = PLC057;
           TIC058      = PLC058;
           TIC059      = PLC059;
           TIC060      = PLC060;
           TIC061      = PLC061;
           TIC062      = PLC062;
           TIC063      = PLC063;
           TIC064      = PLC064;
           TIC065      = PLC065;
           TIC066      = PLC066;
           TIC067      = PLC067;
           TIC068      = PLC068;
           TIC069      = PLC069;
           TIC070      = PLC070;
           TIC071      = PLC071;
           TIC072      = PLC072;
           TIC073      = PLC073;
           TIC074      = PLC074;
           TIC075      = PLC075;
           TIC076      = PLC076;
           TIC077      = PLC077;
           TIC078      = PLC078;
           TIC079      = PLC079;
           TIC080      = PLC080;
           TIC081      = PLC081;
           TIC082      = PLC082;
           TIC083      = PLC083;
           TIC084      = PLC084;
           TIC085      = PLC085;
           TIC086      = PLC086;
           TIC087      = PLC087;
           TIC088      = PLC088;
           TIC089      = PLC089;
           TIC090      = PLC090;
           TIC091      = PLC091;
           TIC092      = PLC092;
           TIC093      = PLC093;
           TIC094      = PLC094;
           TIC095      = PLC095;
           TIC096      = PLC096;
           TIC097      = PLC097;
           TIC098      = PLC098;
           TIC099      = PLC099;
           TIC100      = PLC100;
           TIC101      = PLC101;
           TIC102      = PLC102;
           TIC103      = PLC103;
           TIC104      = PLC104;
           TIC105      = PLC105;
           TIC106      = PLC106;
           TIC107      = PLC107;
           TIC108      = PLC108;
           TIC109      = PLC109;
           TIC110      = PLC110;
           TIC111      = PLC111;
           TIC112      = PLC112;
           TIC113      = PLC113;
           TIC114      = PLC114;
           TIC115      = PLC115;
           TIC116      = PLC116;
           TIC117      = PLC117;
           TIC118      = PLC118;
           TIC119      = PLC119;
           TIC120      = PLC120;

           select;
           when  PLFASI > 20190930 and (PLILCR < 2700 or PLILCR > 2799);
               WRITE REBCTIR2;
           when  PLFASI <= 20190930 ;
               WRITE REBCTIR2;
           endsl;

        endsr;
      *---------------------------------------------------------------------
      * Rutina de inicio
      *---------------------------------------------------------------------
     C     *INZSR        BEGSR
      *... Claves
      *
     C                   movel     @@jobn        @jobn             6 0
     c                   READ      sgsysv
      *
     C     *ENTRY        PLIST
     C                   PARM                    wwfulc            8 0
      *
      * obtiene fecha de fin de proceso anterior
     C                   CALL      'SBBACFFA'
     C                   PARM                    wwfulc
     C                   PARM                    wwfant            8 0
      *
     c     kcuot         klist
     c                   kfld                    PLISUC
     c                   kfld                    PLINCR
     c                   kfld                    PLIDEG
     c                   kfld                    NroCuota
      *
     C     @key02        KLIST
     C                   KFLD                    PLisuc
     C                   KFLD                    PLincr
     C                   KFLD                    PLideg
      *
      * Blanqueo la tabla antes de empezar a procesar.
      *
     C     wwfulc        setll     BCTIR2
     C     wwfulc        reade     BCTIR2
     C                   dow       not %eof(BCTIR2)
     C                   delete    rebctir2
     C     wwfulc        reade     bctir2
     C                   enddo
     c                   clear                   rebctir2
     C
     C                   ENDSR

      *---------------------------------------------------------------------

