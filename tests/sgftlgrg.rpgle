*SRCMBRTXT:Exit Point de Validacion de login      
     H DATEDIT(*YMD)
     FQATOCHOST IF   E           K DISK
     FSGFTUS    IF   E           K DISK
     FSGFTFL    IF   E           K DISK
     FSGlogf    IF A E           K DISK

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
      *--------------------------------------------------------------*
      *
      *  TCP/IP Application Server Logon Exit Point Interface
      *
      * *------------------------------------------------------------*
      * |  1 | Application identifier      | Input  | Binary(4)      |
      * |    |                             |        |                |
      * |    |  1 = FTP server program     |        |                |
      * |    |  2 = REXEC server program   |        |                |
      * |    |                             |        |                |
      * |----+------------+----------------+--------+----------------|
      * |  2 | User identifier             | Input  | Char(*)        |
      * |----+------------+----------------+--------+----------------|
      * |  3 | Length of user identifier   | Input  | Binary(4)      |
      * |----+------------+----------------+--------+----------------|
      * |  4 | Authentication string       | Input  | Char(*)        |
      * |----+------------+----------------+--------+----------------|
      * |  5 | Length of authentication    | Input  | Binary(4)      |
      * |    | string                      |        |                |
      * |----+------------+----------------+--------+----------------|
      * |  6 | Client IP address           | Input  | Char(*)        |
      * |----+------------+----------------+--------+----------------|
      * |  7 | Length of client IP address | Input  | Binary(4)      |
      * |----+------------+----------------+--------+----------------|
      * |  8 | Return code                 | Output | Binary(4)      |
      * |    |                             |        |                |
      * |    |  0 = Reject Logon           |        |                |
      * |    |  1 = Continue Logon         |        |                |
      * |    |  2 = Continue Logon,        |        |                |
      * |    |      override current       |        |                |
      * |    |      library                |        |                |
      * |    |  3 = Continue Logon,        |        |                |
      * |    |      override user prf,     |        |                |
      * |    |      password               |        |                |
      * |    |  4 = Continue Logon,        |        |                |
      * |    |      override user prf,     |        |                |
      * |    |      password, current      |        |                |
      * |    |      library                |        |                |
      * |    |  5 = Accept logon with      |        |                |
      * |    |      user prf returned      |        |                |
      * |    |  6 = Accept logon with      |        |                |
      * |    |      user prf returned,     |        |                |
      * |    |      override current       |        |                |
      * |    |      library                |        |                |
      * |    |                             |        |                |
      * |----+------------+----------------+--------+----------------|
      * |  9 | User profile                | Output | Char(10)       |
      * |----+------------+----------------+--------+----------------|
      * | 10 | Password                    | Output | Char(10)       |
      * |----+------------+----------------+--------+----------------|
      * | 11 | Initial current library     | Output | Char(10)       |
      * *------------------------------------------------------------*
      *
      *     Exit Point:  QIBM_QTMF_SVR_LOGON
      *                  QIBM_QTMX_SVR_LOGON
      *
     D AppId           S              9B 0
     D UserId          S            999
     D UserIdLen       S              9B 0
     D Authen          S            999
     D AuthenLen       S              9B 0
     D IpAddr          S             15
     D IpAddrLen       S              9B 0
     D RtnCode         S              9B 0
     D User            S             10
     D Password        S             10
     D Passwd          S             10
     D CurrLib         S             10

     D Email           S             30
     D FTPUser         S             10
     D Message         S             52
     D FullJob         S             28

     D RejectLogon     C                   CONST(0)
     D ContinueLogon   C                   CONST(1)
     D ConLogonOvrLib  C                   CONST(2)
     D ConLogonOvrUsr  C                   CONST(3)
     D ConLogonOvrUL   C                   CONST(4)
     D AcceptUsrRtn    C                   CONST(5)
     D AcceptUsOvrLib  C                   CONST(6)
     D Low             C                   CONST('abcdefghijklmn¦opqrstuvwxyz')
     D Upp             C                   CONST('ABCDEFGHIJKLMN#OPQRSTUVWXYZ')

     C*-------------------------------------------------------------------------
     C*
     C                   ExSr      CheckUser
     C                   ExSr      CheckIP
     C                   ExSr      GetCurLib
     C                   If        FUSUSR = *BLANKS or FUSUSR='*NONE'
     C                   Eval      RtnCode=1
     C                   Else
     C                   Eval      RtnCode=6
     C                   Eval      User=FUSUSR
     C                   Eval      Password=FUSUSR
     C                   Eval      LSTCOM='Sesion Iniciada:'+FtpUser
     C                   Exsr      LogEvent
     C                   Endif
     C                   ExSr      EndPgm

     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     CheckUser     BegSr
     C*
     C                   If        UserIdLen   > *Zeros
     C                   Eval      FtpUser = %Subst(UserId: 1: UserIdLen)
     C     Low:Upp       XLate     FtpUser       FtpUser
     C                   EndIf
     C*
     C     FtpUser       Chain     RESGFTUS                           99
     C                   If        *IN99 =*ON
     C                   Eval      RtnCode=RejectLogon
     C                   Eval      LSTCOM='Usuario No Autorizado'
     C                   Exsr      LogEvent
     C                   Exsr      EndPgm
     C                   Endif
     C*
     C                   If        FUSUSR <> *BLANKS and FUSUSR<>'*NONE'
     C                   Eval      Passwd = %Subst(Authen: 1: AuthenLen)
     C     Low:Upp       XLate     Passwd        Passwd
     C     Low:Upp       XLate     FUSUSR        FUSUSR
     C                   If        Passwd <> FUIFIL
     C                   Eval      LSTCOM='Contrase¦a Incorrecta'
     C                   Eval      RtnCode=RejectLogon
     C                   Exsr      LogEvent
     C                   Exsr      EndPgm
     C                   Endif
     C                   Endif
     C*
     C
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     CheckIP       BegSr
     C*
     C                   Eval      Internet    = %Subst(IpAddr: 1: IpAddrLen)
     C     Internet      Chain     QATOCHOST                          99
     C                   If        *IN99 = *ON
     C                   Eval      RtnCode=RejectLogon
     C                   Eval      LSTCOM='IP No Confiable'
     C                   Exsr      LogEvent
     C                   Exsr      EndPgm
     C                   Endif
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     GetCurLib     BegSr
     C     FtpUser       Chain     RESGFTFL                           99
     C  N99              MoveL(P)  FLIPGM        CurrLib
     C   99              MoveL(P)  'QGPL'        CurrLib
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     LogEvent      BegSr
     C*
     C                   Eval      LSFALT=*DATE
     C                   Time                    LSHALT
     C                   MoveL(P)  FtpUser       LSIUSR
     C                   Eval      LSIPGM='FTPSVR'
     C                   Eval      LSIFRI=Internet
     C                   Write     RESGLOGF
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     EndPgm        BegSr
     C*
     C                   Eval      *InLR       = *On
     C                   Return
     C*
     C                   EndSr
     C*-------------------------------------------------------------------------
     C*
     C*-------------------------------------------------------------------------
     C     *InzSr        BegSr
     C     *Entry        Plist
     C*
     C* Input parameters
     C                   Parm                    AppId
     C                   Parm                    UserId
     C                   Parm                    UserIdLen
     C                   Parm                    Authen
     C                   Parm                    AuthenLen
     C                   Parm                    IpAddr
     C                   Parm                    IpAddrLen
     C* Return parameters
     C                   Parm                    RtnCode
     C                   Parm                    User
     C                   Parm                    Password
     C                   Parm                    CurrLib
     C*
     C                   Move      *ON           *IN90
     C*
     C                   EndSr
