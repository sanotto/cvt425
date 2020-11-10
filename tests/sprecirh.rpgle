*SRCMBRTXT:Sube archivo desde IFS - Definiciones  
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
     D*  Flags for use in open()
     D*
     D* More than one can be used -- add them together.
     D**********************************************************************
     D*                                            Reading Only
     D O_RDONLY        C                   1
     D*                                            Writing Only
     D O_WRONLY        C                   2
     D*                                            Reading & Writing
     D O_RDWR          C                   4
     D*                                            Create File if not exist
     D O_CREAT         C                   8
     D*                                            Exclusively create
     D O_EXCL          C                   16
     D*                                            Truncate File to 0 bytes
     D O_TRUNC         C                   64
     D*                                            Append to File
     D O_APPEND        C                   256
     D*                                            Convert text by code-page
     D O_CODEPAGE      C                   8388608
     D*                                            Open in text-mode
     D O_TEXTDATA      C                   16777216


     D**********************************************************************
     D*      Mode Flags.
     D*         basically, the mode parm of open(), creat(), chmod(),etc
     D*         uses 9 least significant bits to determine the
     D*         file's mode. (peoples access rights to the file)
     D*
     D*           user:       owner    group    other
     D*           access:     R W X    R W X    R W X
     D*           bit:        8 7 6    5 4 3    2 1 0
     D*
     D* (This is accomplished by adding the flags below to get the mode)
     D**********************************************************************
     D*                                         owner authority
     D S_IRUSR         C                   256
     D S_IWUSR         C                   128
     D S_IXUSR         C                   64
     D S_IRWXU         C                   448
     D*                                         group authority
     D S_IRGRP         C                   32
     D S_IWGRP         C                   16
     D S_IXGRP         C                   8
     D S_IRWXG         C                   56
     D*                                         other people
     D S_IROTH         C                   4
     D S_IWOTH         C                   2
     D S_IXOTH         C                   1
     D S_IRWXO         C                   7

     D*--------------------------------------------------------------------
     D* Open a File
     D*
     D* int open(const char *path, int oflag, . . .);
     D*--------------------------------------------------------------------
     D open            PR            10I 0 ExtProc('open')
     D  filename                       *   value
     D  openflags                    10I 0 value
     D  mode                         10U 0 value options(*nopass)
     D  codepage                     10U 0 value options(*nopass)

     D*--------------------------------------------------------------------
     D* Read From a File
     D*
     D* ssize_t read(int handle, void *buffer, size_t bytes);
     D*--------------------------------------------------------------------
     D read            PR            10I 0 ExtProc('read')
     D  handle                       10i 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D*--------------------------------------------------------------------
     D* Write to a file
     D*
     D* ssize_t write(int fildes, const void *buf, size_t bytes)
     D*--------------------------------------------------------------------
     D write           PR            10I 0 ExtProc('write')
     D  handle                       10I 0 value
     D  buffer                         *   value
     D  bytes                        10U 0 value

     D*--------------------------------------------------------------------
     D* Close a file
     D*
     D* int close(int fildes)
     D*--------------------------------------------------------------------
     D close           PR            10I 0 ExtProc('close')
     D  handle                       10I 0 value
     ***************************************************************************
     ** PROTOTYPES
     ***************************************************************************
     D*-------------------------------------------------------------------------
     D*>DATASTRUCTURE : SYSERR
     D*>DESCRIPTION   : Data structure to receive error generated by OPM APIS
     D*>USE           : Retrieve OPM API Error codes
     D*>RELATEDFUNCT  : sysSndPgmMsg
     D*-------------------------------------------------------------------------
      /IF NOT DEFINED(SYSERR_DEFINED)
      /DEFINE SYSERR_DEFINED
     D SYSERR          DS
     D  dsECBytesP             1      4I 0 INZ(256)
     D  dsECBytesA             5      8I 0 INZ(0)
     D  dsECMsgID              9     15
     D  dsECReserv            16     16
     D  dsECMsgDta            17    256
      /ENDIF

     D Cvt2Asc         PR
     D  StrPtr                         *   value
     D  StringLen                     4S 0 value
     D*------------------------------------------------------------------------
     D*>DESCRIPTION   : Sends a program message
     D*>RETURNS       : A string containing the message key generated by the Sy
     D*>PARAMETER     : A string containig MSGI
     D*>PARAMETER     : A string containig MSGF LIB (OPT)
     D*>PARAMETER     : A string containig User data (OPT)
     D*>USAGE         : Eval      rc=sysSndPgmMsg('CPF9801':'QCPFMSG   *LIBL
     D*------------------------------------------------------------------------
     D  sysSndPgmMsg   PR             4A
     D   msgID                        7A   CONST
     D   msgF                        20A   CONST OPTIONS(*NOPASS)
     D   msgDta                     256A   CONST OPTIONS(*NOPASS)
     D   msgType                     10A   CONST OPTIONS(*NOPASS)
