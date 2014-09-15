IMPLEMENTATION MODULE KBD32;

(*************************************************************************
  32-bit OS/2 thunking layer for 16-bit keyboard kernel API.

  This is still available for compatibility with older
  compiler versions. It only contains the 3 most important APIs
  of the keyboard kernel, thus keeping this thunking
  module small. For a full thunking interface of the keyboard
  kernel see 'KBD.DEF'!

  Copyright (c) 1995 by Juergen Neuhoff
**************************************************************************)


(*$XL+   Language extensions: '_' for symbol names                       *)

IMPORT SYSTEM;
IMPORT THUNK;
FROM   DOSMODULEMGR IMPORT DosLoadModule;
FROM   DOSMODULEMGR IMPORT HMODULE, APIRET;
FROM   DOSPROCESS   IMPORT DosExit, DosBeep;
FROM   DOSPROCESS   IMPORT EXIT_PROCESS;
FROM   DOSMEMMGR    IMPORT DosAllocMem, DosFreeMem, fALLOC;


CONST
  ORDCharIn    = 4;
  ORDGetStatus = 10;
  ORDSetStatus = 11;

VAR
  NoName         : POINTER TO ARRAY [0..0] OF CHAR;
  DLLHandle      : HMODULE;
  rc             : APIRET;
  pCharIn        : SYSTEM.LONGWORD;
  pGetStatus     : SYSTEM.LONGWORD;
  pSetStatus     : SYSTEM.LONGWORD;



PROCEDURE KbdCharIn
(
  VAR KeyInfo : KBDKEYINFO;
  IOWait      : CARDINAL;
  Handle      : HKBD
)             : APIRET16;
VAR
  pKeyInfo    : POINTER TO KBDKEYINFO;
  result      : CARDINAL;
  pESP        : SYSTEM.ADDRESS;
BEGIN
  pESP := THUNK.StackTop();
  THUNK.StackAlign2048();
  THUNK.StackGrow( SYSTEM.TSIZE( KBDKEYINFO ) );
  pKeyInfo := THUNK.Flat32ToFar16( THUNK.StackTop() );
  THUNK.StackAlignDWORD();
  THUNK.StackGrow( SYSTEM.TSIZE( THUNK.REGSAVE ) );
  THUNK.PushDWORD( pKeyInfo );
  THUNK.PushWORD( IOWait );
  THUNK.PushWORD( Handle );
  result := THUNK.CallFar16( pCharIn, 8 );
  pKeyInfo := THUNK.Far16ToFlat32( pKeyInfo );
  KeyInfo := pKeyInfo^;
  THUNK.StackAlign( pESP );
  RETURN result;
END KbdCharIn;


PROCEDURE KbdSetStatus
(
  VAR KbdInfo : KBDINFO;
  Handle      : HKBD
)             : APIRET16;
VAR
  pKbdInfo    : POINTER TO KBDINFO;
  result      : CARDINAL;
  pESP        : SYSTEM.ADDRESS;
BEGIN
  pESP := THUNK.StackTop();
  THUNK.StackAlign2048();
  SYSTEM.Push( SYSTEM.ADR( KbdInfo ), SYSTEM.TSIZE( KBDINFO ) );
  pKbdInfo := THUNK.Flat32ToFar16( THUNK.StackTop() );
  THUNK.StackAlignDWORD();
  THUNK.StackGrow( SYSTEM.TSIZE( THUNK.REGSAVE ) );
  THUNK.PushDWORD( pKbdInfo );
  THUNK.PushWORD( Handle );
  result := THUNK.CallFar16( pSetStatus, 6 );
  pKbdInfo := THUNK.Far16ToFlat32( pKbdInfo );
  KbdInfo := pKbdInfo^;
  THUNK.StackAlign( pESP );
  RETURN result;
END KbdSetStatus;



PROCEDURE KbdGetStatus
(
  VAR KbdInfo : KBDINFO;
  Handle      : HKBD
)             : APIRET16;
VAR
  pKbdInfo    : POINTER TO KBDINFO;
  result      : CARDINAL;
  pESP        : SYSTEM.ADDRESS;
BEGIN
  pESP := THUNK.StackTop();
  THUNK.StackAlign2048();
  SYSTEM.Push( SYSTEM.ADR( KbdInfo ), SYSTEM.TSIZE( KBDINFO ) );
  pKbdInfo := THUNK.Flat32ToFar16( THUNK.StackTop() );
  THUNK.StackAlignDWORD();
  THUNK.StackGrow( SYSTEM.TSIZE( THUNK.REGSAVE ) );
  THUNK.PushDWORD( pKbdInfo );
  THUNK.PushWORD( Handle );
  result := THUNK.CallFar16( pGetStatus, 6 );
  pKbdInfo := THUNK.Far16ToFlat32( pKbdInfo );
  KbdInfo := pKbdInfo^;
  THUNK.StackAlign( pESP );
  RETURN result;
END KbdGetStatus;


BEGIN
  NoName := NIL;
  rc := DosLoadModule( NoName^, 0, "KBDCALLS", DLLHandle );
  IF rc = 0 THEN
    THUNK.FindProc( pCharIn, ORDCharIn, DLLHandle );
    THUNK.FindProc( pGetStatus, ORDGetStatus, DLLHandle );
    THUNK.FindProc( pSetStatus, ORDSetStatus, DLLHandle );
  ELSE
    rc := DosBeep( 1000, 1000 );
    DosExit( EXIT_PROCESS, 1 );
  END;
END KBD32.
