IMPLEMENTATION MODULE Terminal;

(*************************************************************************
   OS/2 2.x  Modula-2 standard utility for terminal input/output.
             13.04.95 13.05 : Now per process initialization/termination

   Copyright (c) 1993,1994 by Juergen Neuhoff
*************************************************************************)

(* $XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(* $A1        Byte alignment for record fields                           *)

(* $LINK
   LIBRARY TERMINAL INITINSTANCE TERMINSTANCE
   DATA    MULTIPLE
   CODE    PRELOAD EXECUTEREAD NONCONFORMING
*)

IMPORT Keyboard;
FROM OS2DEF      IMPORT APIRET, HFILE;
FROM DOSFILEMGR  IMPORT DosOpen, DosClose, DosWrite;
FROM DOSFILEMGR  IMPORT FILE_NORMAL, FILE_OPEN;
FROM DOSFILEMGR  IMPORT OPEN_ACCESS_WRITEONLY;
FROM DOSFILEMGR  IMPORT OPEN_SHARE_DENYNONE;
FROM DOSPROCESS  IMPORT DosExit, DosExitList, DosBeep;
FROM DOSPROCESS  IMPORT EXIT_PROCESS, EXLST_ADD, EXLST_EXIT;

CONST
  ExitOrder    = 0000AF00H; (* ExitOrder: just before OS/2 KBD is closed *)
  StdIn        = 0;
  StdOut       = 1;
  StdErr       = 2;

VAR
  ScreenHandle   : HFILE;
  rc             : APIRET;
  ActionTaken    : LONGCARD;
  ExistLookAhead : BOOLEAN;
  LookAhead      : ARRAY [0..1] OF CHAR;
  ExistLastRead  : BOOLEAN;
  LastRead       : ARRAY [0..1] OF CHAR;
  BytesWritten   : LONGCARD;
  Echo           : BOOLEAN;

PROCEDURE Read( VAR ch: CHAR );
VAR
  ScanCode : SHORTCARD;
BEGIN
  IF ExistLookAhead THEN
    ch := LookAhead[0];
    ScanCode := ORD( LookAhead[1] );
    ExistLookAhead := FALSE;
  ELSE
    Keyboard.Read( ch );
    ScanCode := Keyboard.ScanCode();
  END;
  LastRead[0] := ch;
  LastRead[1] := CHR( ScanCode );
  ExistLastRead := TRUE;
  IF Echo THEN
    IF LastRead[0] <> 0C THEN
      IF LastRead[0] = CHR( 0E0H ) THEN
        IF LastRead[1] = 0C THEN
          Write( ch );
        END;
      ELSIF LastRead[0] = CHR( 3 ) THEN
        Write( '^' );
        Write( 'C' );
        HALT();
      ELSE
        Write( ch );
      END;
    END;
  END;
END Read;

PROCEDURE BusyRead( VAR ch: CHAR );
BEGIN
  IF ExistLookAhead THEN
    ExistLookAhead := FALSE;
    ExistLastRead := TRUE;
    LastRead[0] := LookAhead[0];
    LastRead[1] := LookAhead[1];
    ch := LastRead[0];
  ELSE
    Keyboard.BusyRead( ch );
    LastRead[0] := ch;
    LastRead[1] := CHR( Keyboard.ScanCode() );
    ExistLastRead := (LastRead[0] <> 0C) OR (LastRead[1] <> 0C);
  END;
END BusyRead;

PROCEDURE ReadAgain();
BEGIN
  IF ExistLastRead THEN
    LookAhead[0] := LastRead[0];
    LookAhead[1] := LastRead[1];
    ExistLookAhead := TRUE;
    ExistLastRead := FALSE;
  END;
END ReadAgain;

PROCEDURE ScanCode():SHORTCARD;
BEGIN
  RETURN ORD( LastRead[1] );
END ScanCode;

PROCEDURE SetEcho( Flag: BOOLEAN );
BEGIN
  Echo := Flag;
END SetEcho;

PROCEDURE GetEcho( VAR Flag: BOOLEAN );
BEGIN
  Flag := Echo;
END GetEcho;

PROCEDURE Write( ch: CHAR );
VAR
  rc           : APIRET;
  BytesWritten : LONGCARD;
BEGIN
  rc := DosWrite( ScreenHandle, ch, 1, BytesWritten );
END Write;

PROCEDURE WriteLn();
CONST
  cr = CHR( 13 );
  lf = CHR( 10 );
VAR
  buffer       : ARRAY [0..1] OF CHAR;
  rc           : APIRET;
  BytesWritten : LONGCARD;
BEGIN
  buffer[ 0 ] := cr;
  buffer[ 1 ] := lf;
  rc := DosWrite( ScreenHandle, buffer, 2, BytesWritten );
END WriteLn;

PROCEDURE WriteString( s: ARRAY OF CHAR );
VAR
  i,j           : LONGCARD;
  rc            : APIRET;
  BytesWritten  : LONGCARD;
BEGIN
  i := 0;
  j := HIGH( s );
  IF j = 0 THEN
    j := MAX( LONGCARD );
  END;
  WHILE (i <= j) AND (s[i] <> 0C) DO
    INC( i );
  END;
  rc := DosWrite( ScreenHandle, s, i, BytesWritten );
END WriteString;

PROCEDURE ExitTerminal();
VAR
  rc : APIRET;
BEGIN
  rc := DosClose( ScreenHandle );
  rc := DosExitList( EXLST_EXIT, ExitTerminal );
END ExitTerminal;

BEGIN (* of Terminal *)
  rc := DosOpen
  ( "SCREEN$", ScreenHandle, ActionTaken, 0, FILE_NORMAL,
    FILE_OPEN, OPEN_ACCESS_WRITEONLY OR OPEN_SHARE_DENYNONE, NIL
  );
  IF (rc <> 0) THEN
    rc := DosWrite( StdErr, "SCREEN$ open error", 18, BytesWritten );
    rc := DosBeep( 1000, 1000 );
    DosExit( EXIT_PROCESS, 1 );
  END;
  rc := DosExitList( EXLST_ADD + ExitOrder, ExitTerminal );
  ExistLookAhead := FALSE;
  ExistLastRead := FALSE;
  LookAhead[0] := 0C;
  LookAhead[1] := 0C;
  LastRead[0] := 0C;
  LastRead[1] := 0C;
  Echo := FALSE;
END Terminal.
