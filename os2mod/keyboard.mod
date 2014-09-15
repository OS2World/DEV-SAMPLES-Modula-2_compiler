IMPLEMENTATION MODULE Keyboard;

(*************************************************************************
   OS/2 2.x  Modula-2 utility for keyboard input.

   Copyright (c) 1993,1995 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)

FROM SYSTEM      IMPORT ADR;
FROM OS2DEF      IMPORT APIRET, HFILE;
FROM DOSPROCESS  IMPORT DosExit, DosExitList, DosBeep;
FROM DOSPROCESS  IMPORT EXIT_PROCESS, EXLST_ADD, EXLST_EXIT;
FROM DOSFILEMGR  IMPORT DosWrite;
FROM DOSFILEMGR  IMPORT STDERR;
FROM KBD         IMPORT KbdCharIn, KbdGetStatus, KbdSetStatus;
FROM KBD         IMPORT APIRET16, KBDINFO, KBDKEYINFO, HKBD;

CONST
  ExitOrder    = 0000AF00H; (* ExitOrder: just before OS/2 KBD is closed *)


VAR
  KbdHandle      : HKBD;
  LastScanCode   : SHORTCARD;
  LastStatus     : SHORTCARD;
  LastState      : CARDINAL;
  rc             : APIRET;
  BytesWritten   : LONGCARD;



PROCEDURE Read( VAR ch: CHAR );
VAR
  CharIn   : KBDKEYINFO;
  rc16     : APIRET16;
  rc       : APIRET;
BEGIN
  rc16 := KbdCharIn( CharIn, 0, KbdHandle );
  ch := CharIn.CharCode;
  LastScanCode := CharIn.ScanCode;
  LastStatus := CharIn.Status;
  LastState := CharIn.State;
  IF rc16 <> 0 THEN
    rc := DosBeep( 1000, 1000 );
  END;
END Read;

PROCEDURE BusyRead( VAR ch: CHAR );
VAR
  CharIn   : KBDKEYINFO;
  rc16     : APIRET16;
BEGIN
  rc16 := KbdCharIn( CharIn, 1, KbdHandle );
  IF (CharIn.Status AND 40H) <> 0 THEN
    ch := CharIn.CharCode;
    LastScanCode := CharIn.ScanCode;
  ELSE
    ch := 0C;
    LastScanCode := 0;
  END;
END BusyRead;


PROCEDURE ScanCode():SHORTCARD;
BEGIN
  RETURN LastScanCode;
END ScanCode;

PROCEDURE Status() : SHORTCARD;
BEGIN
  RETURN LastStatus;
END Status;

PROCEDURE State() : CARDINAL;
BEGIN
  RETURN LastState;
END State;



VAR
  OldInfo        : KBDINFO;


PROCEDURE SetBinaryMode():BOOLEAN;
CONST
  BINARY         = 0004H;
  NOASCII        = 0FFF7H;
VAR
  rc16           : APIRET16;
  KbdInfo        : KBDINFO;
BEGIN
  KbdInfo.Size := SIZE( KbdInfo );
  rc16 := KbdGetStatus( KbdInfo, KbdHandle );
  IF rc16 <> 0 THEN
    RETURN FALSE;
  END;
  OldInfo := KbdInfo;
  KbdInfo.Mask := KbdInfo.Mask AND NOASCII;
  KbdInfo.Mask := KbdInfo.Mask OR BINARY;
  rc16 := KbdSetStatus( KbdInfo, KbdHandle );
  IF rc16 <> 0 THEN
    RETURN FALSE;
  END;
  RETURN TRUE;
END SetBinaryMode;


PROCEDURE SetOriginalMode();
VAR
  rc16 : APIRET16;
BEGIN
  rc16 := KbdSetStatus( OldInfo, KbdHandle );
END SetOriginalMode;


PROCEDURE ExitKeyboard();
VAR
  rc : APIRET;
BEGIN
  SetOriginalMode();
  rc := DosExitList( EXLST_EXIT, ExitKeyboard );
END ExitKeyboard;


BEGIN
  KbdHandle := 0;
  IF NOT SetBinaryMode() THEN
    rc := DosWrite( STDERR, "KBD$ cannot be set to binary mode", 33, BytesWritten );
    rc := DosBeep( 1000, 1000 );
    DosExit( EXIT_PROCESS, 1 );
  END;
  rc := DosExitList( EXLST_ADD + ExitOrder, ExitKeyboard );
  LastScanCode := 0;
  LastStatus := 0;
  LastState := 0;
END Keyboard.
