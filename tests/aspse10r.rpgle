*SRCMBRTXT:ADMINISTRACION DE SEGURIDAD            
      *----------------------------------------------------------------
      *>SYSNME:XX-X...
      *>OBJNME:XX-X...
      *>OBJMET:XX-X...
      *>METPIN:0001-FLDNME-X...
      *>METPOU:0001-FLDNME-X...
      *>OBJVER:XX-X...
      *>PVRLOC:          /          .
      *>AUTNME:XXXXXXXXXX-X...
      *>CRTDTE:AAAAMMDD
      *>OBSTXT:X...
      *----------------------------------------------------------------
     FTM696     IF   E           K DISK
     FASPSE10W  CF   E             WORKSTN
     F                                     SFILE(D01:NRR)
     D*----------------------------------------------------------------
     D*-------------CLAVES DEL ARCHIVO MAESTRO
     D KR              DS
     D  PG696                001    010
     D  US696                011    020
     D  ID696                021    022
     D KC              DS
     D  FLD001               001    010
     D  FLD002               011    020
     D  FLD003               021    022
     D KN              DS
     D  FLD004               001    010
     D  FLD005               011    020
     D  FLD006               021    022
     D KP              DS
     D  FLD007               001    010
     D  FLD008               011    020
     D  FLD009               021    022
     D*-------------PROGRAM STATUS DATA STRUCTURE
     D MYPSDS         SDS
     D  PGMNME               324    333
     D*-------------PARTIDOR DE MENSAJE
     D  MSGTXT                 1    225
     D  MSG001                 1     75
     D  MSG002                76    150
     D  MSG003               151    225
     D*-------------CONSTANTES CON NOMBRE
     D CPYRIG          C                   CONST('Nuevo Banco de La Ri-
     D                                     oja S.A.            -
     D                                                         ')
     D SISNME          C                   CONST('SIGESCAP  ')
     D ATBOF           C                   CONST('Ya se encuentra al c-
     D                                     omienzo de la lista -
     D                                     ...')
     D ATEOF           C                   CONST('Ya se encuentra al f-
     D                                     inal de la lista')
     C*----------------------------------------------------------------
     C                   MOVEL(P)  CPYRIG        STSLNE           77
     C                   MOVE      *BLANKS       MSGTXT
     C                   EXSR      ANZSCP
     C                   EXSR      LODSFL
     C     *IN03         DOWEQ     *OFF
     C                   EXSR      PRCREQ
     C                   EXSR      LODSFL
     C                   ENDDO
     C                   EXSR      EXIT
     C*----------------------------------------------------------------
     C*---------------------
     C*>SUBNME:*INZSR=SUBRUTINA DE INICIALIZACION
     C*---------------------
     C     *INZSR        BEGSR
     C                   EXSR      GOTOP
     C     K0001         KLIST
     C                   KFLD                    FLD001
     C     K0002         KLIST
     C                   KFLD                    FLD001
     C                   KFLD                    FLD002
     C     K0003         KLIST
     C                   KFLD                    FLD001
     C                   KFLD                    FLD002
     C                   KFLD                    FLD003
     C     K0099         KLIST
     C                   KFLD                    FLD001
     C                   KFLD                    FLD002
     C                   KFLD                    FLD003
      *{INSKL}
      *{CONS}
     C                   Z-ADD     0             PF                2 0
     C                   CLEAR                   KP
     C                   CLEAR                   KC
     C                   CLEAR                   KN
     C                   EXSR      GOTOP
     C                   ENDSR
     C*---------------------
     C*>SUBNME:*GOTOP=IR AL PRINCIPIO DEL ARCHIVO
     C*---------------------
     C     GOTOP         BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    MASK              2
     C                   PARM                    FLC001          010
     C                   PARM                    FLC002          010
     C                   PARM                    FLC003          002
     C                   MOVE      FLC001        FLD001
     C                   MOVE      FLC002        FLD002
     C                   MOVE      FLC003        FLD003
     C                   ENDSR
     C*---------------------
     C*>SUBNME:GOLAST=IR AL FINAL
     C*---------------------
     C     GOLAST        BEGSR
     C     *HIVAL        SETGT     RTM696
     C                   EXSR      REDPEK
     C                   MOVE      KR            KC
     C                   ENDSR
     C*---------------------
     C*>SUBNME:LODSFL=CARGAR SUBFILE CON UNA PAGINA DE DATOS
     C*---------------------
     C     LODSFL        BEGSR
     C                   SETOFF                                       303126
     C                   Z-ADD     0             NRR               2 0
     C                   WRITE     C01
     C     K0099         SETLL     RTM696
     C                   EXSR      REDEQK
     C     *IN25         DOWEQ     *OFF
     C     *IN26         ANDEQ     *OFF
     C                   ADD       1             NRR
     C                   EXSR      GETREL
     C                   WRITE     D01                                  26
     C                   MOVE      KR            KN
     C                   EXSR      REDEQK
     C                   ENDDO
     C                   MOVE      *OFF          *IN33
     C   25
     CANN26              MOVE      *ON           *IN33
     C     NRR           COMP      *ZERO                              30
     C                   SETON                                        31
     C                   WRITE     P01
     C                   EXFMT     C01
     C  N30              CLEAR                   KR
     C                   MOVEL(P)  CPYRIG        STSLNE           77
     C   33              MOVEL(P)  ATEOF         STSLNE
     C                   EXSR      PRCAFU
     C                   ENDSR
     C*--------------------
     C*>SUBNME:PRCAFU=PROCESAR ACELERADORES DE TECLAS DE FUNCION
     C*--------------------
     C     PRCAFU        BEGSR
     C     PF            IFGT      *ZERO
     C     PF            ANDLE     24
     C                   MOVEA     '1'           *IN(PF)
     C                   ENDIF
     C                   SETOFF                                           99
     C     PF            IFEQ      99
     C                   SETON                                          9945
     C                   ENDIF
     C     PF            IFEQ      33
     C                   SETON                                          9946
     C                   ENDIF
     C     PF            IFEQ      88
     C                   SETON                                          9923
     C                   ENDIF
     C  N99              Z-ADD     *ZERO         PF
     C                   ENDSR
     C*--------------------
     C*>SUBNME:PRCREQ=PROCESAR REQUERIMIENTO
     C*--------------------
     C     PRCREQ        BEGSR
     C                   SELECT
     C     *IN45         WHENEQ    *ON
     C                   EXSR      PAGUP
     C     *IN46         WHENEQ    *ON
     C     *IN33         ANDEQ     *OFF
     C                   EXSR      PAGDN
     C     *IN06         WHENEQ    *ON
     C                   CALL      'ASPSE15R'
     C                   PARM      'AA'          EDTMDE            2
     C                   PARM                    PG696
     C                   PARM                    US696
     C                   PARM                    ID696
     C     *IN10         WHENEQ    *ON
     C                   EXSR      SCHREC
     C                   Z-ADD     *ZERO         PF
     C     *IN22         WHENEQ    *ON
     C                   EXSR      GOTOP
     C                   Z-ADD     *ZERO         PF
     C     *IN23         WHENEQ    *ON
     C                   EXSR      GOLAST
     C                   Z-ADD     *ZERO         PF
     C                   OTHER
     C                   EXSR      CHKOPT
     C                   ENDSL
     C                   ENDSR
     C*--------------------
     C*>SUBNME:PAGDN =AVANZA UNA PAGINA
     C*--------------------
     C     PAGDN         BEGSR
     C                   MOVE      KN            KC
     C                   ENDSR
     C*--------------------
     C*>SUBNME:PAGUP =RETROCEDER UNA PAGINA
     C*--------------------
     C     PAGUP         BEGSR
     C     K0099         SETLL     RTM696
     C                   DO        15
     C                   EXSR      REDPEK
     C  N25              MOVE      KR            KC
     C   25              LEAVE
     C                   ENDDO
     C   25              MOVEL(P)  ATBOF         STSLNE
     C   25              EXSR      GOTOP
     C                   ENDSR
     C*---------------------
     C*>SUBNME:CHKOPT=CHEQUEAR OPCIONES
     C*---------------------
     C     CHKOPT        BEGSR
     C                   READC     D01                                  2828
     C     *IN28         DOWEQ     *OFF
     C                   SELECT
     C     OPC           WHENEQ    '2'
     C                   CALL      'ASPSE15R'
     C                   PARM      'UU'          EDTMDE            2
     C                   PARM                    PG696
     C                   PARM                    US696
     C                   PARM                    ID696
     C     OPC           WHENEQ    '4'
     C                   CALL      'ASPSE15R'
     C                   PARM      'DD'          EDTMDE            2
     C                   PARM                    PG696
     C                   PARM                    US696
     C                   PARM                    ID696
     C     OPC           WHENEQ    '5'
     C                   CALL      'ASPSE15R'
     C                   PARM      'VV'          EDTMDE            2
     C                   PARM                    PG696
     C                   PARM                    US696
     C                   PARM                    ID696
     C                   ENDSL
     C                   READC     D01                                  2828
     C                   ENDDO
     C                   ENDSR
     C*---------------------
     C*>SUBNME:GETREL=OBTENER DATOS DE ARCHIVOS RELACIONADOS
     C*---------------------
     C     GETREL        BEGSR
     C*
     C                   MOVEL     PG696         WWPGMN           10
     C                   MOVE      *BLANKS       WWDESC
     C                   CALL      'ASPSE11C'
     C                   PARM                    WWPGMN
     C                   PARM                    WWDESC
     C*
     C                   ENDSR
     C*---------------------
     C*>SUBNME:REDEQK   =LEER CON CLAVE IGUAL
     C*---------------------
     C     REDEQK        BEGSR
     C   80              READ      RTM696                                 25
     C   81K0001         READE     RTM696                                 25
     C   82K0002         READE     RTM696                                 25
     C   83K0003         READE     RTM696                                 25
     C                   ENDSR
     C*---------------------
     C*>SUBNME:REDPEK  =LEER ANTERIOR CON CLAVE IGUAL
     C*---------------------
     C     REDPEK        BEGSR
     C   80              READP     RTM696                                 25
     C   81K0001         READPE    RTM696                                 25
     C   82K0002         READPE    RTM696                                 25
     C   83K0003         READPE    RTM696                                 25
     C                   ENDSR
     C*---------------------
     C*>SUBNME:ANZSCP = DETERMINAR QUE NIVEL DE CLAVE UTILIZAR
     C*---------------------
     C     ANZSCP        BEGSR
     C                   MOVE      MASK          MASKA             2 0
     C     80            ADD       MASKA         NI                2 0
     C                   MOVE      *ON           *IN(NI)
     C                   ENDSR
     C*---------------------
     C*>SUBNME:SCHREC= RUTINA PARA F10=UBICAR
     C*---------------------
     C     SCHREC        BEGSR
     C                   EXFMT     R01
     C                   EXSR      PRCAFU
     C  N12              MOVE      KR            KC
     C                   ENDSR
     C*---------------------
     C*>SUBNME:EXIT    = RUTINA DE TERMINACION DEL PROGRAMA
     C*---------------------
     C     EXIT          BEGSR
      *{EXIPGM}
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
