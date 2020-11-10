*SRCMBRTXT:C?lculo de VA (Valor actual) PF        
**free
CTL-OPT main(mainLine)
        option(*SRCSTMT)
        dftactgrp(*NO)
        actgrp(*NEW)
        bnddir('QC2LE') ;

dcl-pr Calcular_VF  Packed(15:2);
        pf_capital          Packed(15:2)    const;
        pf_plazo_cert       Packed( 5:0)    const;
        pf_tna_cert         Packed(13:7)    const;
end-pr;

dcl-pr Calcular_VA  Packed(15:2);
        pf_capital          Packed(15:2)    const;
        pf_plazo_cert       Packed( 5:0)    const;
        pf_tna_cert         Packed(13:7)    const;
end-pr;


dcl-proc mainLine;

    dcl-pi *n extpgm;

        pf_capital          Packed(15:2)    const;
        pf_plazo_cert       Packed( 5:0)    const;
        pf_tna_cert         Packed(13:7)    const;
        pf_tna_nueva        Packed(13:7)    const;

        pf_valor_final      Packed(15:2);
        pf_valor_actual     Packed(15:2);

    end-pi;


    pf_valor_final = Calcular_VF(   pf_capital:
                                    pf_plazo_cert:
                                    pf_tna_cert);

    pf_valor_actual = Calcular_VA(  pf_valor_final:
                                    pf_plazo_cert:
                                    pf_tna_nueva);

end-proc;

//==============================================================================
//Calcular_VF: Calc. el valor FINAL del PF al fin de la imposición.
//==============================================================================
dcl-proc Calcular_VF;
    dcl-pi *n   Packed(15:2);
        pf_capital          Packed(15:2)    const;
        pf_plazo_cert       Packed( 5:0)    const;
        pf_tna_cert         Packed(13:7)    const;
    end-pi;

    dcl-s i                 Packed(13:7);
    dcl-s d                 Packed(13:7);
    dcl-s interes           Packed(15:2);
    dcl-s vf                Packed(15:2);

    i =  pf_tna_cert/100;
    d =  (i / 365) * pf_plazo_cert;
    interes = pf_capital * d;
    vf = pf_capital + interes;

    return vf;

end-proc;

//==============================================================================
// Calcular_VF: Calc. el valor FINAL del PF al fin de la imposición.
//==============================================================================
dcl-proc Calcular_VA;
    dcl-pi *n   Packed(15:2);
        pf_valor_final      Packed(15:2)    const;
        pf_plazo_cert       Packed( 5:0)    const;
        pf_tna_nueva        Packed(13:7)    const;
    end-pi;

    dcl-s i                 Packed(13:7);
    dcl-s d                Packed(13:7);
    dcl-s interes           Packed(15:2);
    dcl-s va                Packed(15:2);

    i =  pf_tna_nueva/100;
    d =  (i / 365) * pf_plazo_cert;
    va = pf_valor_final / (1 + d );

    return va;

end-proc;
