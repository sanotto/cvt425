*SRCMBRTXT:Operaciones Pasivas Operaciones No Regi
     H*****************************************************************
     H*                                                               *
     H*  SYSTEM NAME: SIDEBA -                                        *
     H*                                                               *
     H*  PROGRAM NAME: Operaciones Pasivas-OPERACIONES NO REGISTRADAS *
     H*                                                               *
     H*                                                               *
     H*  PROGRAM NO: BCPA00R9                                         *
     H*                                                               *
     H*  DATE:   23/11/2001                                           *
     H*                                                               *
     H*  AUTHOR: Sergio Cortes                                        *
     H*                                                               *
     H*****************************************************************
     H*  CONTROL DE MODIFICACIONES                                    *
     H*  ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨                                    *
     H*  Modifc.    Por  Fecha    Descripción                         *
     H*  ¨¨¨¨¨¨¨¨  ¨¨¨¨ ¨¨¨¨¨¨¨¨  ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ *
     H*****************************************************************
     H*  Tabla de subrutinas utilizadas                               *
     H*  ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨                               *
     H*  INICIO  - Subrutina de inicialización                        *
     H*  Dxxxxx  - Subrutinas utilizadas en cabeceras de corte control*
     H*  Pxxxxx  - Subrutinas utilizadas proceso                      *
     H*  Txxxxx  - Subrutinas utilizadas en totales de corte control  *
     H*  Lxxxxx  - Subrutinas utilizadas en LR                        *
     H*****************************************************************
     H*  Tabla de indicadores utilizados                              *
     H*  ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨                              *
     H*  01-19  - Identificadores de registros                        *
     H*   20    - Fin de ciclo                                        *
     H*  21-79  - Trabajo                                             *
     H*   80    - Accesos a archivos                                  *
     H*  81-91  - Overflow                                            *
     H*   99    - Inicio de programa                                  *
     H*                                                               *
      *  75 ON indica usuarios activos
      *  75 OFF indica usuarios dados de baja
     H*****************************************************************
     H*  Normas para nomenclatura de nombres de campos def.en el pgm. *
     H*  ¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨¨ *
     H*  DSxxxx - Campo utilizado en data estructure                  *
     H*  PAxxxx - Entrada de parámetros                               *
     H*  LX$xxx - Totales de importes para corte de control X         *
     H*  LXQxxx - Totales de cantidades para corte de control X       *
     H*  WAxxxx - Campos de trabajo alfanuméricos                     *
     H*  WNxxxx - Campos de trabajo numéricos                         *
     H*  @xxx   - Series                                              *
     H*  @KEYxx - Nombres de KLIST (desde 01 a 99)                    *
     H*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    AAAAMM            6
     C                   MOVE      AAAAMM        PERIOD            6 0
     C/EXEC SQL
     C+ DELETE FROM BCOPPA WHERE P1IFEC = :PERIOD AND P1DTXT LIKE
     C+ '%MANUAL%'
     C/END-EXEC
     C/EXEC SQL
     C+ INSERT INTO SDBFIL02/BCOPPA SELECT P2IFEC, P2ISUC, P2IPA2,
     C+ P2IMON, P2INCT, P2INCE, P2QDPL, P2FALT, P2FVEN, P2TTNA, P2PCOM,
     C+ P2IT01, P2CUI1, P2DNI1, P2IT02, P2CUI2, P2DNI2, P2IT03, P2CUI3,
     C+ P2DNI3, P2IT04, P2CUI4, P2DNI4, P2IT05, P2CUI5, P2DNI5, P2$CAP,
     C+ P2$IND, P2$PRM, P2IECP, P2ICCE, P2DTXT, P2TIPP, P2$MAJ, P2$IMP,
     C+ P2$BUU, P2$BUA, P2$P03, P2$A13, P2$A06, P2$A05 FROM BCOPPM WHERE
     C+ P2IFEC = :PERIOD
     C/END-EXEC
     C                   EVAL      *INLR=*ON
