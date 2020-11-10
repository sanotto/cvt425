*SRCMBRTXT:NIIF movimientos contables. Genera COAS
     H DECEDIT('0,') DATEDIT(*YMD) DEBUG
     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE') EXPROPTS(*RESDECPOS)
     H OPTION(*NODEBUGIO)
     H GENLVL(5)
      *---------------------------------------------------------------------
      *  SYSTEM DESC: NIIF - Movimientos contables. Genera COASAU
      *               En base a lo calculado en los procesos R0 y R1
      *               recorre la tabla par generar asientos.
      *  NAME : BCASAUR2
      *  DATE : 22/07/2019
      *  AUTOR: desa02067
      *
      *---------------------------------------------------------------------
     FBCTIR201  if   E           k DISK
     FPRCUOT01  if   E           k DISK
     FPRCRED    if   E           k DISK
     Fnicoas03  if   E           k DISK
     Fnicoas04  if   E           k DISK       prefix( 'TM':2 )
     F                                        rename( renicoas : tmnicoas)
     FSGSYSV    if   E             DISK
     f@CPIUSD   if   E           k DISK
     f@CPISYS   if   E           k DISK
     FBCTIR3    UF A E           k DISK
     FCOASAU    O    E             DISK       usropn
     FNIFPAR    UF A E           k DISK
      *---------------------------------------------------------------------
     dPasaCODASI       pr                  extpgm('CO0042RG')
     d                                2    const
      *---------------------------------------------------------------------
     dRepTIE           pr                  extpgm('BCRT50CL')
     d                                8p 0 const
      *---------------------------------------------------------------------
     dRepFParm         pr                  extpgm('BCFP51CL')
     d                                8p 0 const
      *---------------------------------------------------------------------
     dRepASAU          pr                  extpgm('BCAS52CL')
     d                                8p 0 const
      *---------------------------------------------------------------------
     Dsystem           PR            10I 0 extproc('system')
     D                                 *   value options(*string)
      *---------------------------------------------------------------------
      * Declaracion de Matrices
     D cuotas          DS                  OCCURS(9999)
     D DifDias                 1     15  0
     D importeTIR             16     31  2
     D DifFechaD       S             15  0
     d @msg            S            240
     D WA$IMP          S             15  2 DIM(9999)
     D WAFVTO          S             15  2 DIM(120)
     D WAMPAG          S             15  2 DIM(120)
     d wM$sup          S             15  2 DIM(9999)
     d wM$ans          S             15  2 DIM(9999)
     d wM$aja1         S             15  2 DIM(9999)
     d wM$asa1         S             15  2 DIM(9999)
     d @isub           S              2
     d @imcc           S              3  0
     d @immr           S              3  0
     d @ncod           S             30
     d @1cod           S             30
     d cont            S              3  0
     D paisub          S              2
     D wwisub          S              2
     D wwisuc          S              5P 0
     D wwilcr          S              4P 0
     D ww$ani          S             15P 2
     D ww$cop          S             15P 2
     D ww$sup          S             15P 2
     D ww$ans          S             15P 2
     d TotAjusteA2     S             15  2
     d TotAjusteA3     S             15  2
     d w1$cop          S             15  2
     d w1$sup          S             15  2
     d w1$ans          S             15  2
     d w1$aca1         S             15  2
     d w1$aca2         S             15  2
     d w1$aja1         S             15  2
     d w1$aja2         S             15  2
     d w1$ans1         S             15  2
     d w1$ans2         S             15  2
     d w1$asa1         S             15  2
     d TotMov1         S             15  2
     d TotMov2         S             15  2
     D wwficc          S              8P 0
     D x               S              4  0
     D FechaDesde      S               D
     D FechaHasta      S               D
     D FechaChar       S             10
     D Caracter        S              8
     D ImporteAsiento  S             15  2
     D LineaCOAS       S              4  0
     D Tipo            S              1A
     d CmdStr          S            300A
     D Resultado       S              5  0
      *
      *---------------------------------------------------------------------
      *... SDS Interna del equipo
      *... Valores del sistema
     D @@DS           SDS
     D  @@PGM                  1     10
     D  @@LIB                 81     90
     D  @@JOB                244    253
     D  @@USER               254    263
     D  @@jobn               264    269
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     D FIDS            DS
     D  @IDCPO               370    371B 0
     D  @IDPOS               370    371B 0
     D  @IDRRN               397    400B 0
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     D CUENTA          DS
     D  @WICTL                 1      1A
     D  @WIRUR                 2      2A
     D  @WIMCO                 3      3A
     D  @WINCC                 4      6A
     D  @WISCC                 7     10A
      *---------------------------------------------------------------------
      *FREE
      *
      *

           open COASAU;

           // Valores Iniciales

           wwisub = 'PR';

           // Llamo a proceso que calcula valores a la fecha de cierre.
           exsr ProcAct;

           // llamo a proceso que calcula valores a fecha de cierre anterior
           exsr ProcAnt;

           // Llamo a proceso que calcula totales y genera registros
           // para asientos contables.
           exsr Asientos;

           // pasa a CODASI o genera reporte si es provisorio
           exsr VuelcaCODASI;


           *inlr = *on;

      *---------------------------------------------------------------------
      * Proceso que calcula totales actuales
      *---------------------------------------------------------------------
           begsr ProcAct;

           setll (wwfulc) BCTIR201;
           reade (wwfulc) BCTIR201;
           wwisuc = tiisuc;
           wwilcr = tiilcr;
           ww$ani = 0;
           ww$cop = 0;
           ww$sup = 0;
           ww$ans = 0;
           dow not %eof(BCTIR201);

               // Primero proceso todos los PR con comision,
               // con ajuste NIIF.

               if (TI$ANI <> 0 or TI$COP <> 0
                  OR TI$ANS <>0 or TI$SUP <>0 ) and TIISUB = 'PR'
                  and tiilcr <> 80 and tiilcr <> 0;
                   ww$ani = ww$ani + ti$ani;
                   ww$cop = ww$cop + ti$cop;
                   ww$sup = ww$sup + ti$sup;
                   ww$ans = ww$ans + ti$ans;
                   t3fcie = wwfulc;
                   t3isuc = tiisuc;
                   t3ilcr = tiilcr;
               endif;

               reade (wwfulc) BCTIR201;

               if ((ww$ani <> 0 or ww$ans <> 0)  and tiisub = 'PR')
                    or wwilcr = 80;
                 if (tiisuc <> wwisuc or tiilcr <> wwilcr);
                     t3$inp = ww$ani;
                     t3$cop = ww$cop;
                     t3$sup = ww$sup;
                     t3$ans = ww$ans;
                     t3isub = wwisub;
                     t3isuc = wwisuc;
                     t3ilcr = wwilcr;
                     if wwilcr <> 80;
                       write rebctir3;
                     endif;

                     ww$ani = 0;
                     ww$cop = 0;
                     ww$sup = 0;
                     ww$ans = 0;
                     wwisuc = tiisuc;
                     wwilcr = tiilcr;
                 endif;
              endif;
           enddo;


           // calculo importes consolidados de prestamos al personal.
           // o sea con comision 0

           setll (wwfulc) BCTIR201;
           reade (wwfulc) BCTIR201;
           wwisuc = tiisuc;
           ww$ani = 0;
           ww$cop = 0;
           ww$sup = 0;
           dow not %eof(BCTIR201);

               // Ahora   proceso todos los PR al personal sin comision,
               // con ajuste NIIF.

               if TI$OTO = 0 and TI$ANI <> 0 and TIISUB = 'PR';

                   ww$ani = ww$ani + ti$ani;
                   ww$sup = ww$sup + ti$sup;
               endif;

               reade (wwfulc) BCTIR201;
           enddo;

           t3fcie = wwfulc;
           t3ilcr = 9999;
           t3isub = wwisub;
           t3isuc = 0;
           t3$inp = ww$ani;
           t3$cop = 0;
           t3$sup = ww$sup  ;
           write rebctir3;

           endsr;

      *---------------------------------------------------------------------
      * Proceso que calcula totales del proceso anterior
      *---------------------------------------------------------------------
           begsr ProcAnt;

           setll (wwfant) BCTIR201;
           reade (wwfant) BCTIR201;
           wwisuc = tiisuc;
           wwilcr = tiilcr;
           ww$ani = 0;
           ww$cop = 0;
           ww$sup = 0;
           ww$ans = 0;

           dow not %eof(BCTIR201);

               // Primero proceso todos los PR con comision,
               // con ajuste NIIF.

               if (TI$ANI <> 0 or TI$COP <> 0
                  or TI$ANS<>0) and TIISUB = 'PR'
                  and tiilcr <> 80 and tiilcr <>0;
                   ww$ani = ww$ani + ti$ani;
                   ww$cop = ww$cop + ti$cop;
                   ww$sup = ww$sup + ti$sup;
                   ww$ans = ww$ans + ti$ans;
                   t3fcie = wwfant;
                   t3isuc = tiisuc;
                   t3ilcr = tiilcr;
               endif;

               reade (wwfant) BCTIR201;

               if (ww$ani <> 0 or ww$ans <> 0) and tiisub = 'PR';
                 if (tiisuc <> wwisuc or tiilcr <> wwilcr);
                   chain (wwfulc: wwisub: wwisuc: wwilcr) bctir3;
                   if %found(bctir3);
                      t3$ana = ww$ani;
                      t3$asa = ww$ans;
                      update rebctir3;
                   endif;

                   ww$ani = 0;
                   ww$cop = 0;
                   ww$sup = 0;
                   ww$ans = 0;

                   wwisuc = tiisuc;
                   wwilcr = tiilcr;
                 endif;
               endif;
           enddo;


           // calculo importes consolidados de prestamos al personal.
           // o sea con comision 0 para el mes anterior

           setll (wwfant) BCTIR201;
           reade (wwfant) BCTIR201;
           wwisuc = tiisuc;
           ww$ani = 0;
           ww$cop = 0;
           ww$sup = 0;
           dow not %eof(BCTIR201);

               // Ahora   proceso todos los PR al personal sin comision,
               // con ajuste NIIF.

               if TI$OTO = 0 and TI$ANI <> 0 and TIISUB = 'PR'
                  and (TIILCR=80 or TIILCR = 0);

                   ww$ani = ww$ani + ti$ani;
                   ww$sup = ww$sup + ti$sup;
               endif;

               reade (wwfant) BCTIR201;
           enddo;

           wwilcr = 9999;
           chain (wwfulc: wwisub: wwisuc: wwilcr) bctir3;
           if %found(bctir3);
              t3$ana = ww$ani;
              update rebctir3;
           endif;

           // Recorro toda la tabla BCTIR2 para obtener el ajuste
           // NIIF total del mes anterior.
           TotAjusteA2 = 0;
           setll (wwfant) bctir201;
           reade (wwfant) bctir201;
           dow not %eof(bctir201);
              if ti$oto <> 0;
                 TotAjusteA2 = TotAjusteA2 + TI$ANI;
              endif;
              reade (wwfant) bctir201;
           enddo;

           // Recorro toda la tabla BCTIR3 para obtener el ajuste
           // NIIF total del mes anterior para sacar dif. por operaciones
           // ya dadas de baja.
           TotAjusteA3 = 0;
           setll (wwfulc) bctir3;
           reade (wwfulc) bctir3;
           dow not %eof(bctir3);
               // proceso solo los que tiene gastos de otorgamiento
               // que en esete archivo son los que tienen linea <> 9999
               if t3ilcr <> 9999;
                 TotAjusteA3 = TotAjusteA3 + T3$ANA;
               endif;
               reade (wwfulc) bctir3;
           enddo;

           // Inserto un registro con op. ficticia para poder
           // cargar el ajuste NIIF de operaciones dadas de baja
           // del mes anterior.
           T3FCIE = wwfulc;
           T3ISUB = wwisub;
           T3ISUC = 99999;
           T3ILCR = 0;
           T3$INP = 0;
           T3$ANA = TotAjusteA2 - TotAjusteA3;
           T3$COP = 0;
           T3$SUP = 0;
           T3$ANS = 0;
           T3$ASA = 0;


           write rebctir3;

           endsr;

      *---------------------------------------------------------------------
      * Proceso que genera los asientos
      *---------------------------------------------------------------------
           begsr Asientos;


        //  541006 Comisiones vinculadas con créditos        D 9999
        //  138301 Ajustes por medición al costo amortizado  H 9998

        // SELECT   sum(t3$cop)
        // FROM bctir3 WHERE T3FCIE =20190531 and t3ilcr <> 9999

         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$cop = 0;
         dow not %eof(bctir3);
             if t3ilcr <> 9999;
                w1$cop = w1$cop + t3$cop;
             endif;
             reade (wwfulc) bctir3;
         enddo;

         ImporteAsiento = w1$cop;
         LineaCOAS = 9999;
         Tipo = 'D';
         exsr GrabaASAU;

         ImporteAsiento = w1$cop;
         LineaCOAS = 9998;
         Tipo = 'H';
         exsr GrabaASAU;

        // ------------------------------------------------

        // 138301 Ajustes por medición al costo amortizado     D 9998
        // 511XXX Intereses por XXX                            H lineas

        // SELECT sum(t3$inp)- sum(t3$ana) + sum(t3$cop)
        // FROM bctir3 WHERE T3FCIE =20190531 and t3ilcr <  9900

         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$cop  = 0;
         w1$aja1 = 0;
         w1$aca1 = 0;
         dow not %eof(bctir3);
             if t3ilcr <> 9999;
                w1$cop  = w1$cop + t3$cop;
                w1$aca1 = w1$aca1 + t3$inp;
                w1$aja1 = w1$aja1 + t3$ana;
             endif;
             reade (wwfulc) bctir3;
         enddo;

         //138301 aca totalizaba para todas las lineas
         //       pero se pidio que se haga por pares contables
         //       por si falta parametrizaion de cuentas contables
         //TotMov1 = w1$aca1 - w1$aja1 + w1$cop;
         //ImporteAsiento = TotMov1;
         //LineaCOAS = 9998;
         //Tipo = 'D';
         //exsr GrabaASAU;


         //511XXX
         setll *loval NICOAS03;
         read NICOAS03;
         dow not %eof(NICOAS03) and NCILCR <= 9990;
            setll (wwfulc) bctir3;
            reade (wwfulc) bctir3;
            w1$cop  = 0;
            w1$aja1 = 0;
            w1$aca1 = 0;
            dow not %eof(bctir3);
                if t3ilcr = ncilcr and t3ilcr <= 9990;
                   w1$cop  = w1$cop + t3$cop;
                   w1$aca1 = w1$aca1 + t3$inp;
                   w1$aja1 = w1$aja1 + t3$ana;
                endif;
                reade (wwfulc) bctir3;
            enddo;

            // voy guardando el importe por linea
            // así despues puedo generar el asiento.
            if NCILCR <> 0;
               wa$imp(NCILCR) = w1$aca1 - w1$aja1 + w1$cop;
            endif;

            ImporteAsiento = (w1$aca1 - w1$aja1 + w1$cop) ;
            LineaCOAS = NCILCR;
            Tipo = 'H';
            if ImporteAsiento <> 0;
               exsr GrabaASAU;
               //138301 lo pongo aca para que quede en pares contables
               //TotMov1 = w1$aca1 - w1$aja1 + w1$cop;
               LineaCOAS = 9998;
               Tipo = 'D';
               exsr GrabaASAU;
            endif;

            read NICOAS03;
         enddo;

        // ------------------------------------------------

        // 560060 Gastos de Administración                 D 9995
        // 178001 Ajustes por medición al costo amortizado H 9997

        // sumo todos los subsidios posteriores en los prestamos
        // al personal del banco (t3ilcr = 9999)

        // SELECT sum(t3$sup)
        // FROM bctir3 WHERE T3FCIE =20190531 and t3ilcr = 9999

         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$sup = 0;
         dow not %eof(bctir3);
             if t3ilcr = 9999;
                w1$sup = w1$sup + t3$sup;
             endif;
             reade (wwfulc) bctir3;
         enddo;

         ImporteAsiento = w1$sup * (-1);
         LineaCOAS = 9995;
         Tipo = 'D';
         exsr GrabaASAU;

         ImporteAsiento = w1$sup * (-1);
         LineaCOAS = 9997;
         Tipo = 'H';
         exsr GrabaASAU;


        // ------------------------------------------------

        // 178001 Ajustes por medición al costo amortizado        D 9997
        // 570015 Otros ajustes e intereses por créditos diversos H 9996

        // SELECT sum(t3$ana) - sum(t3$sup) - sum(t3$inp) FROM bctir3 WHERE
        // T3FCIE =20190531 and t3ilcr =  9999

         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$sup  = 0;
         w1$aja2 = 0;
         w1$aca2 = 0;
         dow not %eof(bctir3);
             if t3ilcr =  9999;
                w1$sup  = w1$sup + t3$sup;
                w1$aca2 = w1$aca2 + t3$inp;
                w1$aja2 = w1$aja2 + t3$ana;
             endif;
             reade (wwfulc) bctir3;
         enddo;
         TotMov2 = w1$aja2 - w1$sup -w1$aca2;

         ImporteAsiento = TotMov2 * ( 1);
         LineaCOAS = 9997;
         Tipo = 'D';
         exsr GrabaASAU;

         ImporteAsiento = TotMov2 * ( 1);
         LineaCOAS = 9996;
         Tipo = 'H';
         exsr GrabaASAU;

        // ------------------------------------------------
        // Subsidios posteriores -7% lineas 32/65/102
        // Asiento 1
        // 521098 Resultado por reconocimiento inicial de préstamos D
        // 138301 Ajustes por medición al costo amortizado          H

        // SELECT sum(t3$sup) FROM bctir3 W                            HERE
        // T3FCIE =20190531 and t3ilcr =  9999

         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$sup  = 0;
         w1$aja2 = 0;
         w1$aca2 = 0;
         dow not %eof(bctir3);
             if t3$sup <> 0;  //****** poner linas
                if t3ilcr = 32 or t3ilcr = 65 or t3ilcr = 102;
                   w1$sup  = w1$sup + t3$sup;
                endif;
             endif;
             reade (wwfulc) bctir3;
         enddo;
         TotMov2 =  w1$sup;

         ImporteAsiento = TotMov2 ;
         LineaCOAS = 9994;    //***********  521098-0400  ***************
         Tipo = 'D';
         exsr GrabaASAU;

         ImporteAsiento = TotMov2 ;
         LineaCOAS = 9998;    //***********  138301-0400  *************
         Tipo = 'H';
         exsr GrabaASAU;

        // ------------------------------------------------

        // Subsidios posteriores -7% lineas 32/65/102
        // Asiento 2

        // 138301 Ajustes por medición al costo amortizado     D 9998
        // 511XXX Intereses por XXX                            H lineas

        // SELECT sum(t3$asa) - sum(t3$ans) FROM
        // bctir3 WHERE T3FCIE =20190531 and t3ilcr in (32, 65 , 102)
        // hago para linea 32
         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$sup  = 0;
         w1$ans  = 0;
         w1$aja1 = 0;
         w1$aca1 = 0;
         w1$asa1 = 0;
         dow not %eof(bctir3);
             if t3ilcr = 32;
                w1$ans  = w1$ans + t3$ans  * (-1);
                w1$asa1 = w1$asa1 + t3$asa * (-1);
             endif;
             reade (wwfulc) bctir3;
         enddo;


         //511XXX
         // hago para linea 32
               setll (wwfulc) bctir3;
               reade (wwfulc) bctir3;
               w1$sup  = 0;
               w1$aja1 = 0;
               w1$aca1 = 0;
               x = 1;
               // blanqueo array;
               dow x < 9999;
                   wM$ans(x)  = 0;
                   wM$asa1(x) = 0;
                   x = x + 1;
               enddo;

               wM$ans(32)  = wM$ans(32)   + w1$ans ;
               wM$asa1(32) = wM$asa1(32)  + w1$asa1 ;
               dow not %eof(bctir3);
                  if   T3ILCR = 32;
                     wM$ans(32)  =  wM$ans(32)  - t3$sup;
                  // wM$asa1(32) =  wM$asa1(32) + t3$asa;
                  endif;
                  reade (wwfulc) bctir3;
               enddo;


              ImporteAsiento =  wM$asa1(32)       -  wM$ans(32)      ;
              LineaCOAS = 32;
              Tipo = 'H';
              if ImporteAsiento <> 0;
                 exsr GrabaASAU;
                 //138301 lo pongo aca para que quede en pares contables
                 LineaCOAS = 9998;
                 Tipo = 'D';
                 exsr GrabaASAU;
              endif;

        // hago para linea 65
         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$sup  = 0;
         w1$ans  = 0;
         w1$aja1 = 0;
         w1$aca1 = 0;
         w1$asa1 = 0;
         dow not %eof(bctir3);
             if t3ilcr = 65;
                w1$ans  = w1$ans + t3$ans  * (-1);
                w1$asa1 = w1$asa1 + t3$asa * (-1);
             endif;
             reade (wwfulc) bctir3;
         enddo;


         //511XXX
         // hago para linea 65
               setll (wwfulc) bctir3;
               reade (wwfulc) bctir3;
               w1$sup  = 0;
               w1$aja1 = 0;
               w1$aca1 = 0;
               x = 1;
               // blanqueo array;
               dow x < 9999;
                   wM$ans(x)  = 0;
                   wM$asa1(x) = 0;
                   x = x + 1;
               enddo;

               wM$ans(65)  = wM$ans(65)   + w1$ans ;
               wM$asa1(65) = wM$asa1(65)  + w1$asa1 ;
               dow not %eof(bctir3);
                  if   T3ILCR = 65;
                     wM$ans(65)  =  wM$ans(65)  - t3$sup;
                  // wM$asa1(65) =  wM$asa1(65) + t3$asa;
                  endif;
                  reade (wwfulc) bctir3;
               enddo;


              ImporteAsiento =  wM$asa1(65)       -  wM$ans(65)      ;
              LineaCOAS = 65;
              Tipo = 'H';
              if ImporteAsiento <> 0;
                 exsr GrabaASAU;
                 //138301 lo pongo aca para que quede en pares contables
                 LineaCOAS = 9998;
                 Tipo = 'D';
                 exsr GrabaASAU;
              endif;


        // hago para linea 102
         setll (wwfulc) bctir3;
         reade (wwfulc) bctir3;
         w1$sup  = 0;
         w1$ans  = 0;
         w1$aja1 = 0;
         w1$aca1 = 0;
         w1$asa1 = 0;
         dow not %eof(bctir3);
             if t3ilcr = 102;
                w1$ans  = w1$ans + t3$ans * (-1);
                w1$asa1 = w1$asa1 + t3$asa * (-1);
             endif;
             reade (wwfulc) bctir3;
         enddo;


         //511XXX
         // hago para linea 102;

               setll (wwfulc) bctir3;
               reade (wwfulc) bctir3;
               w1$sup  = 0;
               w1$aja1 = 0;
               w1$aca1 = 0;
               x = 1;
               // blanqueo array;
               dow x < 9999;
                   wM$ans(x)  = 0;
                   wM$asa1(x) = 0;
                   x = x + 1;
               enddo;

               wM$ans(102)  = wM$ans(102)   + w1$ans ;
               wM$asa1(102) = wM$asa1(102)  + w1$asa1 ;
               dow not %eof(bctir3);
                  if   T3ILCR = 102;
                     wM$ans(102)  =  wM$ans(102)  - t3$sup;
                 //  wM$asa1(102) =  wM$asa1(102) + t3$asa;
                  endif;
                  reade (wwfulc) bctir3;
               enddo;


              ImporteAsiento = wM$asa1(102)       -  wM$ans(102)      ;
              LineaCOAS = 102;
              Tipo = 'H';
              if ImporteAsiento <> 0;
                 exsr GrabaASAU;
                 //138301 lo pongo aca para que quede en pares contables
                 LineaCOAS = 9998;
                 Tipo = 'D';
                 exsr GrabaASAU;
              endif;


         endsr;
      *---------------------------------------------------------------------
      * Proceso que Graba COASAU
      *---------------------------------------------------------------------
           begsr GrabaAsau;

           chain (LineaCOAS) NICOAS04;
           if %found(NICOAS04);
             HCISUC = 0;
             HCFASI = wwfulc;
             HCIMON = %int(tmicmo);

             if tipo = 'D';
                HC$IDE = ImporteAsiento;
                Cuenta = %CHAR(tmICCD);
             else;
                HC$IDE = 0;
             endif;

             if tipo = 'H';
                HC$IHA = ImporteAsiento;
                Cuenta = %CHAR(tmICCE);
               else;
                HC$IHA = 0;
             endif;

             HCICTL = %INT(@WICTL);
             HCIRUR = %INT(@WIRUR);
             HCIMCO = %INT(@WIMCO);
             HCINCC = %INT(@WINCC);
             HCISCC = %INT(@WISCC);

             HCCMOV = 'Asientos automaticos NIIF.';
             HCICOT = 0;
             HCIDCB = 0;
             HCEMPR = 0;

             write RECOASAU;
            else;
             HCISUC = LineaCOAS;
             HC$IDE = ImporteAsiento;
             write RECOASAU;
           endif;

           endsr;

      *---------------------------------------------------------------------
      * VuelcaCOSASI - si es definitivo llama a proceso que genera CODASI
      *              sino genera reporte.
      *---------------------------------------------------------------------

           begsr VuelcaCODASI;

           // accede a la CPI para ver si el proceso es PROVISORIO o
           // DEFINITIVO.
           CHAIN (@pjobn)  @cpiusd;
           if %found(@cpiusd);

              close COASAU;

              // en caso de ser DEFINITIVO llama a proceso que graba CODASI
              if @CICRM  = 'D';
                 paisub = 'PR';
                 exsr ReporteASAU;
              // el paso a CODASI se puso en BCNIIFMA
              // callp PasaCODASI(paisub);

              // si es PROVISORIO envia reporte de COASAU y nada mas.
              elseif @CICRM = 'P';
                 exsr ReporteASAU;
              endif;
           endif;

           // ejecuta reporte TIE para Claudia
           exsr ReporteTie;
           // reporte de parametrización faltante
           exsr FaltaParm;

           endsr;

      *---------------------------------------------------------------------
      * Reporte TIE, con todo el stock de operaciones y el ajuste por C/U
      *---------------------------------------------------------------------
           begsr ReporteTie;
           // Reporte para Claudia
           // SELECT  TIFCIE, TIILCR, TIDPLN, tiicui, tindnn,
           // TIISUB, TIISUC, TIINCR,
           // TIIDEG, TI$OTO, TITIRN, TI$VAS, TI$VAN, TI$ANI, TI$COP, TI$SUP ,
           // TIISUC, TIINCR, TIIDEG, TIILCR, TIDPLN FROM bctir2

              callp RepTIE(wwfulc);

           endsr;

      *---------------------------------------------------------------------
      * Reporte de parametrizacion faltante
      *---------------------------------------------------------------------
           begsr FaltaParm;
           setll (wwfulc) bctir3;
           reade (wwfulc) bctir3;
           dow not %eof(bctir3);
              if t3ilcr < 9900;
                chain (t3ilcr) nicoas04;
                if not %found(nicoas04);
                   // falta parametricion de cuenta contable
                   chain (t3ilcr) nifpar;
                   if not %found(nifpar);
                       g3ilcr = t3ilcr;
                       g3erro = 'Linea no parametrizada en NICOAS.';
                       write renifpar;
                   endif;
                endif;
              endif;

              reade (wwfulc) bctir3;
           enddo;

           callp RepFParm(wwfulc);

           endsr;

      *---------------------------------------------------------------------
      * Genera reporte con COASAU en corrida provisoria
      *---------------------------------------------------------------------
           begsr ReporteASAU;

            callp RepASAU(wwfulc);

             //ej manda mail bc4305cl

            //CmdStr = 'DLTOVR FILE(Prueba)';

            //Resultado = system(%trim(CmdStr));


           endsr;

      *---------------------------------------------------------------------
      * Rutina de inicio
      *---------------------------------------------------------------------
     C     *INZSR        BEGSR
      *... Claves
      *
     c                   movel     @@jobn        @pjobn            6 0
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
      * Blanque para la fecha que se esta corriendo para
      * no tener problemas en caso de reproceso
     C     WWFULC        SETLL     BCTIR3
     C     WWFULC        READE     BCTIR3
     C                   DOW       NOT %EOF(BCTIR3)
     C                   DELETE    REBCTIR3
     C     WWFULC        READE     BCTIR3
     C                   ENDDO
      *
     C                   CLEAR                   REBCTIR3
      *
      *
      * Blanquea tabla donde se graban las lineas que no estan parametrizadas
     C     *LOVAL        SETLL     NIFPAR
     C                   READ      NIFPAR
     C                   DOW       NOT %EOF(NIFPAR)
     C                   DELETE    RENIFPAR
     C                   READ      NIFPAR
     C                   ENDDO
      *
     C                   ENDSR

      *---------------------------------------------------------------------

