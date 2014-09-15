IMPLEMENTATION MODULE InOut;

(*************************************************************************
   OS/2 2.x  Modula-2 standard utility for stream input/output.
             18.02.94 21.27 : Bugs fixed for Write...Hex() Write...Oct()
             06.01.95 20.55 : Bugs fixed for RedirectInput(), CloseInput()
             21.01.95 15.45 : Bug fixed: 'ExitOrder'
             01.05.95 23.06 : ReadString now accepts Ctrl-C

   Copyright (c) 1993 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)

IMPORT SYSTEM;
IMPORT Strings;
IMPORT Conversions;
IMPORT FileSystem;
IMPORT Keyboard;
FROM   OS2DEF     IMPORT CCHMAXPATH, APIRET;
FROM   DOSFILEMGR IMPORT HFILE, DosDupHandle, DosClose;
FROM   DOSPROCESS IMPORT DosExitList, EXLST_ADD, EXLST_EXIT;

CONST
  ExitOrder    = (80H-3)*256; (* 3 levels before OS/2 components *)
  BackSpace    = CHR( 8 );
  CtrlC        = CHR( 3 );

VAR
  rc           : APIRET;
  OutScreen    : FileSystem.File;  (* Saved before output redirection *)
  InKbd        : FileSystem.File;  (* Saved before input redirection *)
  NewInput     : BOOLEAN;          (* TRUE, if redirected input *)
  NewOutput    : BOOLEAN;          (* TRUE, if redirected output *)

PROCEDURE RedirectInput( from :ARRAY OF CHAR );
VAR
  OldIn        : FileSystem.File;
  rc           : APIRET;
BEGIN
  OldIn := in;
  FileSystem.Lookup( in, from, FALSE );
  IF in.res <> FileSystem.done THEN
    in := OldIn;
    Done := FALSE;
    RETURN;
  END;
  rc := DosDupHandle( in.id, OldIn.id );
  IF rc = 0 THEN
    rc := DosClose( in.id );
    in.id := OldIn.id;
    NewInput := Strings.Compare( in.name, InKbd.name ) <> 0;
    Done := TRUE;
  ELSE
    FileSystem.Close( in );
    in := OldIn;
    Done := FALSE;
  END;
END RedirectInput;

PROCEDURE RedirectOutput( to :ARRAY OF CHAR );
VAR
  OldOut  : FileSystem.File;
  rc      : APIRET;
BEGIN
  OldOut := out;
  FileSystem.Create( out, to );
  IF out.res <> FileSystem.done THEN
    out := OldOut;
    Done := FALSE;
    RETURN;
  END;
  rc := DosDupHandle( out.id, OldOut.id );
  IF rc = 0 THEN
    rc := DosClose( out.id );
    out.id := OldOut.id;
    NewOutput := Strings.Compare( out.name, OutScreen.name ) <> 0;
    Done := TRUE;
  ELSE
    FileSystem.Close( out );
    out := OldOut;
    Done := FALSE;
  END;
END RedirectOutput;

PROCEDURE OpenInput( defext : ARRAY OF CHAR );
VAR
  FileName    : ARRAY [0..CCHMAXPATH-1] OF CHAR;
  i,j,k       : LONGCARD;
BEGIN
  ReadString( FileName );
  IF Done THEN
    i := Strings.Length( FileName );
    IF i > 0 THEN
      IF FileName[ i-1 ] = '.' THEN
        j := 0;
        k := Strings.Length( defext );
        WHILE (j < 3) AND (j < k) AND (i+j < CCHMAXPATH-1) DO
          FileName[ i+j ] := defext[ j ];
          INC( j );
        END;
        FileName[ i+j ] := 0C;
      END;
      RedirectInput( FileName );
    ELSE
      Done := FALSE;
    END;
  END;
END OpenInput;

PROCEDURE OpenOutput( defext : ARRAY OF CHAR );
VAR
  FileName    : ARRAY [0..CCHMAXPATH-1] OF CHAR;
  i,j,k       : LONGCARD;
BEGIN
  ReadString( FileName );
  IF Done THEN
    i := Strings.Length( FileName );
    IF i > 0 THEN
      IF FileName[ i-1 ] = '.' THEN
        j := 0;
        k := Strings.Length( defext );
        WHILE (j < 3) AND (j < k) AND (i+j < CCHMAXPATH-1) DO
          FileName[ i+j ] := defext[ j ];
          INC( j );
        END;
        FileName[ i+j ] := 0C;
      END;
      RedirectOutput( FileName );
    ELSE
      Done := FALSE;
    END;
  END;
END OpenOutput;

PROCEDURE CloseInput();
BEGIN
  IF NewInput THEN
    RedirectInput( InKbd.name );
    IF Done THEN
      NewInput := FALSE;
    END;
  ELSE
    Done := TRUE;
  END;
END CloseInput;

PROCEDURE CloseOutput();
BEGIN
  IF NewOutput THEN
    RedirectOutput( OutScreen.name );
    IF Done THEN
      NewOutput := FALSE;
    END;
  ELSE
    Done := TRUE;
  END;
END CloseOutput;

PROCEDURE Read( VAR ch : CHAR );
BEGIN
  IF NewInput THEN
    FileSystem.ReadChar( in, ch );
    Done := (in.res = FileSystem.done) AND NOT in.eof;
  ELSE
    Keyboard.Read( ch );
    Done := (ch <> 0C);
  END;
  termCH := ch;
END Read;

PROCEDURE ScanCode():SHORTCARD;
BEGIN
  Done := TRUE;
  IF NewInput THEN
    RETURN 0;
  ELSE
    RETURN Keyboard.ScanCode();
  END;
END ScanCode;

PROCEDURE ReadString( VAR s : ARRAY OF CHAR );
VAR
  i,j,k     : LONGCARD;
  ch        : CHAR;
BEGIN
  i := 0;
  k := 0;
  Done := TRUE;
  ch := ' ';
  WHILE Done AND ((ch >= ' ') OR (ch = BackSpace)) DO
    Read( ch );
    IF (ch = CtrlC) AND NOT NewOutput THEN
      Write( '^' );
      Write( 'C' );
      HALT();
    END;
    IF (ch = ' ') AND (k < i) THEN
      (* Trailing blank stops input string *)
      ch := 0C;
    END;
    IF Done AND NOT NewInput THEN
      (* Display character on terminal screen *)
      IF ch >= ' ' THEN
        Write( ch );
      ELSIF (ch = BackSpace) AND (i > 0) THEN
        Write( ch );
        Write( ' ' );
        Write( ch );
      END;
    END;
    IF ch = BackSpace THEN
      IF i > 0 THEN
        DEC( i );
        IF i <= k THEN
          k := i;
        END;
      END;
    ELSIF ch >= ' ' THEN
      IF i < HIGH(s) THEN
        s[ i ] := ch;
      END;
      INC( i );
      IF ch = ' ' THEN
        INC( k );
      END;
    END;
  END;
  IF i >= HIGH(s) THEN
    i := HIGH(s);
  END;
  IF k >= i THEN
    s[0] := 0C;
  ELSIF k > 0 THEN
    j := k;
    WHILE j < i DO
      s[ j-k ] := s[ j ];
      INC( j );
    END;
    s[i-k] := 0C;
  ELSE
    s[i] := 0C;
  END;
END ReadString;

PROCEDURE ReadInt( VAR x : INTEGER );
VAR
  Buffer : ARRAY [0..255] OF CHAR;
BEGIN
  ReadString( Buffer );
  IF Done THEN
    Done := Conversions.StrToInt( Buffer, x );
  END;
END ReadInt;

PROCEDURE ReadShortInt( VAR x : SHORTINT );
VAR
  Buffer : ARRAY [0..255] OF CHAR;
BEGIN
  ReadString( Buffer );
  IF Done THEN
    Done := Conversions.StrToShortInt( Buffer, x );
  END;
END ReadShortInt;

PROCEDURE ReadLongInt( VAR x : LONGINT );
VAR
  Buffer : ARRAY [0..255] OF CHAR;
BEGIN
  ReadString( Buffer );
  IF Done THEN
    Done := Conversions.StrToLongInt( Buffer, x );
  END;
END ReadLongInt;

PROCEDURE ReadCard( VAR x : CARDINAL );
VAR
  Buffer : ARRAY [0..255] OF CHAR;
BEGIN
  ReadString( Buffer );
  IF Done THEN
    Done := Conversions.StrToCard( Buffer, x );
  END;
END ReadCard;

PROCEDURE ReadShortCard( VAR x : SHORTCARD );
VAR
  Buffer : ARRAY [0..255] OF CHAR;
BEGIN
  ReadString( Buffer );
  IF Done THEN
    Done := Conversions.StrToShortCard( Buffer, x );
  END;
END ReadShortCard;

PROCEDURE ReadLongCard( VAR x : LONGCARD );
VAR
  Buffer : ARRAY [0..255] OF CHAR;
BEGIN
  ReadString( Buffer );
  IF Done THEN
    Done := Conversions.StrToLongCard( Buffer, x );
  END;
END ReadLongCard;

PROCEDURE ReadWrd( VAR w : SYSTEM.WORD );
BEGIN
  ReadWord( w );
END ReadWrd;

PROCEDURE ReadByte( VAR b : SYSTEM.BYTE );
VAR
  ch : CHAR;
BEGIN
  ch := b;
  Read( ch );
  b := ch;
END ReadByte;

PROCEDURE ReadWord( VAR w : SYSTEM.WORD );
VAR
  i : ARRAY [0..1] OF CHAR;
BEGIN
  i[0] := 0C;
  i[1] := 0C;
  Read( i[0] );
  IF Done THEN
    Read( i[1] );
  END;
  w := i;
END ReadWord;

PROCEDURE ReadLongWord( VAR lw : SYSTEM.LONGWORD );
VAR
  i : ARRAY [0..1] OF CARDINAL;
BEGIN
  i[0] := 0;
  i[1] := 0;
  ReadWord( i[0] );
  IF Done THEN
    ReadWord( i[1] );
  END;
  lw := i;
END ReadLongWord;

PROCEDURE Write( ch : CHAR );
BEGIN
  FileSystem.WriteChar( out, ch );
  Done := out.res = FileSystem.done;
END Write;

PROCEDURE WriteLn();
CONST
  cr = CHR( 13 );
  lf = CHR( 10 );
BEGIN
  Write( cr );
  IF Done THEN
    Write( lf );
  END;
END WriteLn;

PROCEDURE WriteString( s : ARRAY OF CHAR );
BEGIN
  FileSystem.WriteString( out, s );
  Done := out.res = FileSystem.done;
END WriteString;

PROCEDURE WriteSizedString( VAR s : ARRAY OF CHAR; Size : LONGCARD );
VAR
  i : LONGCARD;
BEGIN
  Done := TRUE;
  i := Strings.Length( s );
  IF Size > i THEN
    i := Size - i;
    WHILE (i > 0) AND Done DO
      Write( ' ' );
      DEC( i );
    END;
  END;
  IF Done THEN
    WriteString( s );
  END;
END WriteSizedString;

PROCEDURE WriteInt( x : INTEGER; n : CARDINAL );
VAR
  Buffer : ARRAY [0..11] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.IntToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteInt;

PROCEDURE WriteShortInt( x : SHORTINT; n : CARDINAL );
VAR
  Buffer : ARRAY [0..11] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.ShortIntToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteShortInt;

PROCEDURE WriteLongInt( x : LONGINT; n : CARDINAL );
VAR
  Buffer : ARRAY [0..11] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.LongIntToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteLongInt;

PROCEDURE WriteCard( x : CARDINAL; n : CARDINAL );
VAR
  Buffer : ARRAY [0..11] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.CardToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteCard;

PROCEDURE WriteShortCard( x : SHORTCARD; n : CARDINAL );
VAR
  Buffer : ARRAY [0..11] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.ShortCardToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteShortCard;

PROCEDURE WriteLongCard( x : LONGCARD; n : CARDINAL );
VAR
  Buffer : ARRAY [0..11] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.LongCardToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteLongCard;

PROCEDURE WriteOct( x : CARDINAL; n : CARDINAL );
VAR
  Buffer : ARRAY [0..13] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.OctToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteOct;

PROCEDURE WriteShortOct( x : SHORTCARD; n : CARDINAL );
VAR
  Buffer : ARRAY [0..13] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.ShortOctToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteShortOct;

PROCEDURE WriteLongOct( x : LONGCARD; n : CARDINAL );
VAR
  Buffer : ARRAY [0..13] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.LongOctToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteLongOct;

PROCEDURE WriteHex( x : CARDINAL; n : CARDINAL );
VAR
  Buffer : ARRAY [0..10] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.HexToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteHex;

PROCEDURE WriteShortHex( x : SHORTCARD; n : CARDINAL );
VAR
  Buffer : ARRAY [0..10] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.ShortHexToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteShortHex;

PROCEDURE WriteLongHex( x : LONGCARD; n : CARDINAL );
VAR
  Buffer : ARRAY [0..10] OF CHAR;
BEGIN
  Done := TRUE;
  IF Conversions.LongHexToStr( x, Buffer ) THEN
    WriteSizedString( Buffer, n );
  ELSE
    Done := FALSE;
  END;
END WriteLongHex;

PROCEDURE WriteWrd( w : SYSTEM.WORD );
BEGIN
  WriteWord( w );
END WriteWrd;

PROCEDURE WriteWord( w : SYSTEM.WORD );
VAR
  b : ARRAY [0..1] OF SYSTEM.BYTE;
BEGIN
  b := w;
  WriteByte( b[0] );
  IF Done THEN
    WriteByte( b[1] );
  END;
END WriteWord;

PROCEDURE WriteByte( b : SYSTEM.BYTE );
BEGIN
  Write( b );
END WriteByte;

PROCEDURE WriteLongWord( lw : SYSTEM.LONGWORD );
VAR
  w : ARRAY [0..1] OF SYSTEM.WORD;
BEGIN
  w := lw;
  WriteWord( w[0] );
  IF Done THEN
    WriteWord( w[1] );
  END;
END WriteLongWord;

PROCEDURE ExitInOut();
VAR
  rc : APIRET;
BEGIN
  IF NewOutput THEN
    FileSystem.Close( out );
  END;
  IF NewInput THEN
    FileSystem.Close( in );
  END;
  rc := DosExitList( EXLST_EXIT, ExitInOut );
END ExitInOut;

BEGIN
  (* open default keyboard input and standard output *)
  in.id := DOSFILEMGR.STDIN;
  in.eof := FALSE;
  in.tmp := FALSE;
  in.name := "KBD$";
  InKbd := in;
  out.id := DOSFILEMGR.STDOUT;
  out.eof := FALSE;
  out.tmp := FALSE;
  out.name := "SCREEN$";
  OutScreen := out;
  NewInput := FALSE;
  NewOutput := FALSE;
  (* add ExitInOut to the exit list *)
  rc := DosExitList( EXLST_ADD + ExitOrder, ExitInOut );
END InOut.
