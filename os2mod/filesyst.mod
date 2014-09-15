IMPLEMENTATION MODULE FileSystem;

(*************************************************************************
   OS/2 2.x  Modula-2 standard utility for file system management.
             09.03.94 00.46 : Bugs fixed for SetAccessMode()
             21.01.95 15.47 : Bug fixed: 'ExitOrder'
             03.05.95 20.08 : Bug fixed: NewTempName()

   Copyright (c) 1993 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)

IMPORT SYSTEM;
IMPORT Strings;
IMPORT Conversions;
FROM   OS2DEF      IMPORT CCHMAXPATH, APIRET, HFILE, PSZ;
FROM   DOSFILEMGR  IMPORT DosOpen, DosClose, DosRead, DosWrite;
FROM   DOSFILEMGR  IMPORT DosResetBuffer, DosSetFilePtr;
FROM   DOSFILEMGR  IMPORT DosDelete, DosMove, DosDupHandle;
FROM   DOSFILEMGR  IMPORT DosQueryFileInfo, DosQueryFHState;
FROM   DOSFILEMGR  IMPORT FILE_NORMAL;
FROM   DOSFILEMGR  IMPORT OPEN_ACTION_CREATE_IF_NEW;
FROM   DOSFILEMGR  IMPORT OPEN_ACTION_REPLACE_IF_EXISTS;
FROM   DOSFILEMGR  IMPORT OPEN_ACTION_FAIL_IF_NEW;
FROM   DOSFILEMGR  IMPORT OPEN_ACTION_OPEN_IF_EXISTS;
FROM   DOSFILEMGR  IMPORT OPEN_SHARE_DENYREADWRITE;
FROM   DOSFILEMGR  IMPORT OPEN_SHARE_DENYWRITE;
FROM   DOSFILEMGR  IMPORT OPEN_ACCESS_READONLY;
FROM   DOSFILEMGR  IMPORT OPEN_ACCESS_WRITEONLY;
FROM   DOSFILEMGR  IMPORT OPEN_ACCESS_READWRITE;
FROM   DOSFILEMGR  IMPORT FILESTATUS3;
FROM   DOSFILEMGR  IMPORT FILE_BEGIN, FILE_CURRENT;
FROM   DOSPROCESS  IMPORT DosExitList, DosSleep, EXLST_ADD, EXLST_EXIT;
FROM   DOSMISC     IMPORT DosScanEnv;
FROM   DOSDATETIME IMPORT DATETIME, DosGetDateTime;

CONST
  ExitOrder   = (80H-2)*256; (* 2 levels before OS/2 components *)
  OPEN_ACCESS = OPEN_ACCESS_READONLY OR
                OPEN_ACCESS_WRITEONLY OR
                OPEN_ACCESS_READWRITE;

VAR
  rc        : APIRET;

PROCEDURE AssignResult( VAR Result : LONGCARD; rc : APIRET );
BEGIN
  CASE rc OF
  | 0:
    Result := done;
  | permissionerror, callerror, unknownmedium, unknownfile, filenameerror,
    toomanyfiles, mediumfull, paramerror :
    Result := rc;
  ELSE
    Result := notdone;
  END;
END AssignResult;

(*
PROCEDURE AccessMode( VAR f : File ) : LONGCARD;
VAR
  i        : LONGCARD;
  rc       : APIRET;
BEGIN
  rc := DosQueryFHState( f.id, i );
  IF rc = 0 THEN
    RETURN i AND OPEN_ACCESS;
  ELSE
    AssignResult( f.res, rc );
    RETURN OPEN_ACCESS;
  END;
END AccessMode;
*)

PROCEDURE NewTempName( VAR FileName : ARRAY OF CHAR ) : BOOLEAN;
VAR
  rc          : APIRET;
  i           : LONGCARD;
  j           : LONGCARD;
  Clock       : DATETIME;
  ClockStr    : ARRAY [0..8] OF CHAR;
  TmpPath     : PSZ;
BEGIN
  j := 0;
  IF DosScanEnv( "TMP", TmpPath ) = 0 THEN
    Strings.Assign( TmpPath^, FileName );
    j := Strings.Length( FileName );
  END;
  IF j = 0 THEN
    Strings.Concat( ".", "", FileName );
    j := 1;
  END;
  rc := DosSleep( 10 );
  rc := DosGetDateTime( Clock );
  i :=     VAL( LONGCARD, Clock.hours      ) * 1000000;
  i := i + VAL( LONGCARD, Clock.minutes    ) * 10000;
  i := i + VAL( LONGCARD, Clock.seconds    ) * 100;
  i := i + VAL( LONGCARD, Clock.hundredths );
  IF Conversions.LongCardToStr( i, ClockStr ) THEN
    IF FileName[j-1] <> "\" THEN
      Strings.Append( "\", FileName );
    END;
    Strings.Append( ClockStr, FileName );
    Strings.Append( ".TMP", FileName );
    i := Strings.Length( FileName );
    IF Strings.Pos( ".TMP", FileName ) = i-4 THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END NewTempName;


PROCEDURE Create( VAR f : File; FileName : ARRAY OF CHAR );
VAR
  rc          : APIRET;
  ActionTaken : LONGCARD;
BEGIN
  f.eof := TRUE;
  Strings.Assign( FileName, f.name );
  IF Strings.Length( FileName ) > 0 THEN
    (* open a new permanent file *)
    f.tmp := FALSE;
    rc := DosOpen
    ( FileName,
      f.id,
      ActionTaken,
      0,
      FILE_NORMAL,
      OPEN_ACTION_CREATE_IF_NEW + OPEN_ACTION_REPLACE_IF_EXISTS,
      OPEN_SHARE_DENYREADWRITE + OPEN_ACCESS_READWRITE,
      NIL
    );
    AssignResult( f.res, rc );
  ELSE
    (* open a temporary file *)
    IF NewTempName( f.name ) THEN
      Create( f, f.name );
      f.tmp := TRUE;
    ELSE
      f.res := filenameerror;
    END;
  END;
END Create;


PROCEDURE Lookup( VAR f : File; FileName : ARRAY OF CHAR; new : BOOLEAN );
VAR
  ActionTaken : LONGCARD;
  rc          : APIRET;
BEGIN
  f.tmp := FALSE;
  f.name[0] := 0C;
  rc := DosOpen
  ( FileName,
    f.id,
    ActionTaken,
    0,
    FILE_NORMAL,
    OPEN_ACTION_FAIL_IF_NEW + OPEN_ACTION_OPEN_IF_EXISTS,
    OPEN_SHARE_DENYREADWRITE + OPEN_ACCESS_READWRITE,
    NIL
  );

  AssignResult( f.res, rc );
  IF f.res = done THEN
    f.eof := FALSE;
    Strings.Assign( FileName, f.name );
  ELSIF NOT new THEN
    f.res := notdone;
    f.eof := FALSE;
  ELSE
    Create( f, FileName );
  END;
END Lookup;


PROCEDURE Close( VAR f : File );
BEGIN
  Doio( f );
  IF f.res = done THEN
    AssignResult( f.res, DosClose( f.id ) );
    IF f.res = done THEN
      f.eof := FALSE;
      IF f.tmp THEN
        Delete( f );
      END;
      IF f.res = done THEN
        f.name[0] := 0C;
      END;
    END;
  END;
END Close;


PROCEDURE Delete( VAR f : File );
BEGIN
  AssignResult( f.res, DosDelete( f.name ) );
  IF f.res = done THEN
    f.name[ 0 ] := 0C;
    f.tmp := FALSE;
    f.eof := FALSE;
  END;
END Delete;


PROCEDURE Rename( VAR f : File; FileName : ARRAY OF CHAR );
VAR
  i             : LONGCARD;
  OldName       : ARRAY [0..CCHMAXPATH-1] OF CHAR;
  OldPos        : LONGCARD;
  SavedResult   : LONGCARD;
  OldTmp        : BOOLEAN;
  OldEof        : BOOLEAN;
BEGIN
  Strings.Assign( f.name, OldName );
  i := Strings.Length( FileName );
  IF (i = 0) AND f.tmp THEN
    RETURN;
  END;
  GetLongPos( f, OldPos );
  IF f.res <> done THEN
    RETURN;
  END;
  OldTmp := f.tmp;
  OldEof := f.eof;
  f.tmp := FALSE;
  Close( f );
  IF f.res <> done THEN
    f.tmp := OldTmp;
    f.eof := OldEof;
    RETURN;
  END;
  IF i > 0 THEN
    AssignResult( f.res, DosMove( OldName, FileName ) );
  ELSIF NewTempName( f.name ) THEN
    AssignResult( f.res, DosMove( OldName, f.name ) );
  ELSE
    f.res := notdone;
  END;
  IF f.res <> done THEN
    (* Rename has not been successful, reopen with old name *)
    SavedResult := f.res;
    Lookup( f, OldName, FALSE );
    IF f.res = done THEN
      f.tmp := OldTmp;
      SetLongPos( f, OldPos );
      IF f.res = done THEN
        f.eof := OldEof;
      END;
    END;
    f.res := SavedResult;
  ELSE
    (* Rename has been successful, reopen with new name *)
    Lookup( f, FileName, FALSE );
    IF f.res = done THEN
      SetLongPos( f, OldPos );
      IF f.res = done THEN
        f.eof := OldEof;
      END;
    END;
  END;
END Rename;


(*$XL-*)
PROCEDURE SetAccessMode( VAR f : File; Mode : LONGCARD );
(*$XL+*)
VAR
  FileHandle  : HFILE;
  ActionTaken : LONGCARD;
  FilePos     : LONGCARD;
  NewPos      : LONGCARD;
  rc          : APIRET;
BEGIN
  IF Mode = OPEN_ACCESS_READONLY THEN
    Mode := Mode + OPEN_SHARE_DENYWRITE;
  ELSE
    Mode := Mode + OPEN_SHARE_DENYREADWRITE;
  END;
  GetLongPos( f, FilePos );
  IF f.res <> done THEN
    RETURN;
  END;
  rc := DosOpen
  ( f.name,
    FileHandle,
    ActionTaken,
    0,
    FILE_NORMAL,
    OPEN_ACTION_FAIL_IF_NEW + OPEN_ACTION_OPEN_IF_EXISTS,
    Mode,
    NIL
  );
  IF rc = 0 THEN
    (* same file opened with a new handle with new acces mode *)
    rc := DosSetFilePtr( FileHandle, FilePos, FILE_BEGIN, NewPos );
    IF rc = 0 THEN
      IF NewPos = FilePos THEN
        rc := DosDupHandle( FileHandle, f.id );
        IF rc = 0 THEN
          (* close old handle and duplicate new handle into old one *)
          rc := DosClose( FileHandle );
          AssignResult( f.res, rc );
        ELSE
          AssignResult( f.res, rc );
          rc := DosClose( FileHandle );
        END;
      ELSE
        AssignResult( f.res, MAX( LONGCARD ) );
        rc := DosClose( FileHandle );
      END;
    ELSE
      AssignResult( f.res, rc );
      rc := DosClose( FileHandle );
    END;
  ELSE
    AssignResult( f.res, rc );
  END;
END SetAccessMode;


PROCEDURE SetRead( VAR f : File );
BEGIN
  SetAccessMode( f, OPEN_ACCESS_READONLY );
END SetRead;


PROCEDURE SetWrite( VAR f : File );
BEGIN
  SetAccessMode( f, OPEN_ACCESS_WRITEONLY );
END SetWrite;


PROCEDURE SetModify( VAR f : File );
BEGIN
  SetAccessMode( f, OPEN_ACCESS_READWRITE );
END SetModify;


PROCEDURE SetOpen( VAR f : File );
BEGIN
  AssignResult( f.res, DosResetBuffer( f.id ) );
END SetOpen;


PROCEDURE Doio( VAR f : File );
BEGIN
  AssignResult( f.res, DosResetBuffer( f.id ) );
END Doio;


PROCEDURE SetPos( VAR f : File ; HighPos, LowPos : CARDINAL );
VAR
  Pos     : LONGCARD;
BEGIN
  Pos := LONG( HighPos ) * (LONG( MAX( CARDINAL ) )+1) + LONG( LowPos );
  SetLongPos( f, Pos );
END SetPos;


PROCEDURE GetPos( VAR f : File; VAR HighPos, LowPos : CARDINAL );
VAR
  i : LONGCARD;
BEGIN
  GetLongPos( f, i );
  HighPos := SHORT( i DIV ( LONG( MAX( CARDINAL ) )+1 ) );
  LowPos := SHORT( i );
END GetPos;


PROCEDURE Length( VAR f : File; VAR HighPos, LowPos : CARDINAL );
VAR
  i : LONGCARD;
BEGIN
  LongLength( f, i );
  HighPos := SHORT( i DIV LONG( MAX( CARDINAL ) ) + 1 );
  LowPos := SHORT( i  );
END Length;


PROCEDURE SetLongPos( VAR f : File; Pos : LONGCARD );
VAR
  NewPos : LONGCARD;
BEGIN
  AssignResult( f.res, DosSetFilePtr( f.id, Pos, FILE_BEGIN, NewPos ) );
  IF f.res = done THEN
    IF NewPos <> Pos THEN
      f.res := notdone;
    END;
  END;
END SetLongPos;


PROCEDURE GetLongPos( VAR f : File; VAR Pos : LONGCARD );
BEGIN
  AssignResult( f.res, DosSetFilePtr( f.id, 0, FILE_CURRENT, Pos ) );
  IF f.res <> done THEN
    Pos := 0;
  END;
END GetLongPos;


PROCEDURE LongLength( VAR f : File; VAR LongPos : LONGCARD );
VAR
  FileInfo  : FILESTATUS3;
BEGIN
  AssignResult( f.res, DosQueryFileInfo( f.id, 1, FileInfo, SIZE( FileInfo ) ) );
  IF f.res <> done THEN
    LongPos := 0; RETURN;
  END;
  LongPos := FileInfo.FileSize;
END LongLength;


PROCEDURE Reset( VAR f : File );
BEGIN
  SetOpen( f );
  IF f.res = done THEN
    SetLongPos( f, 0 );
  END;
END Reset;


PROCEDURE Again( VAR f : File );
VAR
  Pos : LONGCARD;
BEGIN
  GetLongPos( f, Pos );
  IF (f.res = done) AND (Pos > 0) THEN
    SetLongPos( f, Pos-1 );
  END;
END Again;


PROCEDURE ReadByte( VAR f : File; VAR b : SYSTEM.BYTE );
VAR
  BytesRead : LONGCARD;
BEGIN
  AssignResult( f.res, DosRead( f.id, b, 1, BytesRead ) );
  IF f.res = done THEN
    IF BytesRead = 0 THEN
      f.eof := TRUE;
      b := 0;
    END;
  END;
END ReadByte;


PROCEDURE ReadWord( VAR f : File; VAR w : SYSTEM.WORD );
VAR
  BytesRead : LONGCARD;
BEGIN
  w := 0;
  AssignResult( f.res, DosRead( f.id, w, SIZE( w ), BytesRead ) );
  IF f.res = done THEN
    f.eof := (BytesRead <  SIZE( w ) );
  END;
END ReadWord;


PROCEDURE ReadLongWord( VAR f : File; VAR lw : SYSTEM.LONGWORD );
VAR
  BytesRead : LONGCARD;
BEGIN
  lw := 0;
  AssignResult( f.res, DosRead( f.id, lw, SIZE( lw ), BytesRead ) );
  IF f.res = done THEN
    f.eof := (BytesRead <  SIZE( lw ) );
  END;
END ReadLongWord;


PROCEDURE ReadChar( VAR f : File; VAR ch : CHAR );
BEGIN
  ReadByte( f, ch );
END ReadChar;


PROCEDURE ReadBlock( VAR f : File; VAR Block : ARRAY OF SYSTEM.BYTE );
VAR
  BytesRead : LONGCARD;
BEGIN
  AssignResult( f.res, DosRead( f.id, Block, HIGH( Block ), BytesRead ) );
  IF f.res = done THEN
    f.eof := (BytesRead <  HIGH( Block ) );
  END;
END ReadBlock;


PROCEDURE WriteChar( VAR f : File; ch : CHAR );
VAR
  BytesWritten : LONGCARD;
BEGIN
  AssignResult( f.res, DosWrite( f.id, ch, 1, BytesWritten ) );
  IF f.res = done THEN
    IF BytesWritten < 1 THEN
      f.res := notdone;
    END;
  END;
END WriteChar;


PROCEDURE WriteString( VAR f : File; s : ARRAY OF CHAR );
VAR
  BytesWritten : LONGCARD;
  i            : LONGCARD;
BEGIN
  i := Strings.Length( s );
  AssignResult( f.res, DosWrite( f.id, s, i, BytesWritten ) );
  IF f.res = done THEN
    IF BytesWritten < i THEN
      f.res := notdone;
    END;
  END;
END WriteString;


PROCEDURE WriteBlock( VAR f : File; VAR Block : ARRAY OF SYSTEM.BYTE );
VAR
  BytesWritten : LONGCARD;
BEGIN
  AssignResult( f.res, DosWrite( f.id, Block, HIGH( Block ), BytesWritten ) );
  IF f.res = done THEN
    IF BytesWritten < HIGH( Block ) THEN
      f.res := notdone;
    END;
  END;
END WriteBlock;


PROCEDURE WriteLn( VAR f : File );
CONST
  CR   = CHR( 13 );
  LF   = CHR( 10 );
VAR
  Str  : ARRAY [0..2] OF CHAR;
BEGIN
  Str[0] := CR;
  Str[1] := LF;
  Str[2] := 0C;
  WriteString( f, Str );
END WriteLn;


PROCEDURE ExitFileSystem();
VAR
  rc : APIRET;
BEGIN
  rc := DosResetBuffer( 0FFFFH );
END ExitFileSystem;


BEGIN
  rc := DosExitList( EXLST_ADD + ExitOrder, ExitFileSystem );
END FileSystem.
