*SRCMBRTXT:Control de Contraseña via API          
     D LC              C                   'abcdefghijklmnopqrstuvwxyz'
     D UC              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

     D GetProfile      PR                  ExtPgm('QSYGETPH')
     D User                          10a   const
     D Password                      10A   const
     D Handle                        12A
     D ErrorCode                  32766A   options(*varsize: *nopass)
     D PassLength                    10I 0 const options(*nopass)
     D PassCCSID                     10I 0 const options(*nopass)

     D ReleaseProfile  PR                  ExtPgm('QSYRLSPH')
     D Handle                        12A
     D ErrorCode                  32766A   options(*varsize: *nopass)
     D ErrDs           DS
     D BytesProvided           1      4I 0 INZ(256)
     D BytesAvail              5      8I 0 INZ(0)
     D ErrMsgID                9     15
     D Reserved               16     16
     D ErrMsgDta              17    256

     D Handle          S             12A

     D User            S             10A
     D Password        S             10A
     D ErrorID         S              7A
     c     *entry        plist
     C                   Parm                    User
     C                   Parm                    PassWord
     C                   Parm                    ErrorID

     C                   eval      ErrorID = ''

     C                   eval      User = %Xlate(LC:UC:User)
     C                   eval      Password = %Xlate(LC:UC:Password)
     C                   callp     GetProfile(User:
     C                                        Password:
     C                                        Handle:
     C                                        ErrDS:
     C                                        10:
     C                                        0)

     C                   if        ( BytesAvail > 0 )
     C                   eval      ErrorID = ErrMsgID
     C                   else
     C                   callp     ReleaseProfile(Handle)
     C                   endif
     C                   return
