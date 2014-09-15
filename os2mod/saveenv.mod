MODULE SaveEnv;

(**************************************************************************
   OS/2 2.x  Modula-2 utility for saving the current OS/2 environment.

   Copyright (c) 1993 by Juergen Neuhoff
**************************************************************************)

IMPORT SYSTEM;
IMPORT Strings;
IMPORT InOut;
IMPORT FileSystem;

TYPE
  STRING     = ARRAY [0..1024] OF CHAR;
  PSTRING    = POINTER TO STRING;
  PCHAR      = POINTER TO CHAR;
VAR
  OS2Env     : PCHAR;
  OutName    : PSTRING;
  ch         : PCHAR;
  Out        : FileSystem.File;


PROCEDURE CheckFileState( MyFile : FileSystem.File );
BEGIN
  IF MyFile.res <> FileSystem.done THEN
    InOut.WriteString( "SaveEnv: Write error for environment file " );
    InOut.WriteString( Out.name );
    FileSystem.Close( MyFile );
    HALT();
  END;
END CheckFileState;


PROCEDURE ShowUsage();
BEGIN
  InOut.WriteString( "SaveEnv: Usage is: 'SAVEENV <file-name>.CMD'" );
  InOut.WriteLn();
END ShowUsage;


BEGIN
  (*
     The initial stack layout under OS/2 2.x looks like this:

     EBP:     old EBP = 0
     EBP+4:   Return address to the routine that calls DosExecPgm
     EBP+8:   Module handle for the program module
     EBP+12:  0
     EBP+16:  Address of the environment data object
     EBP+20:  Offset to the command line in the environment data object

     This program uses the addresses at [EBP+16] and [EBP+20]
     for accessing the OS/2 environment and the command line.
  *)
  SYSTEM.INLINE
  (
    MOV EAX, DWORD PTR [EBP+16]
    MOV OS2Env, EAX
    MOV EAX, DWORD PTR [EBP+20]
    MOV OutName, EAX
  );

  (* get the environment file name from the command line *)
  OutName := SYSTEM.ADR( OutName^ ) + Strings.Size( OutName^ ) + 1;
  WHILE OutName^[0] = ' ' DO
    OutName := SYSTEM.ADR( OutName^ ) + 1;
  END;
  IF OutName^[0] = 0C THEN
    InOut.WriteString( "SaveEnv: Missing environment file name" );
    InOut.WriteLn();
    ShowUsage();
    RETURN;
  END;

  (* Open the environment file *)
  FileSystem.Lookup( Out, OutName^, TRUE );
  IF Out.res <> FileSystem.done THEN
    IF Out.res = FileSystem.filenameerror THEN
      InOut.WriteString( "SaveEnv: Invalid environment file name: " );
    ELSE
      InOut.WriteString( "SaveEnv: Open error for environment file: " );
    END;
    InOut.WriteString( Out.name );
    InOut.WriteLn();
    ShowUsage();
    RETURN;
  END;

  (* inform the user about saving the OS/2 2.x environment *)
  InOut.WriteString( "SaveEnv: Saving OS/2 environment into file " );
  InOut.WriteString( OutName^ );
  InOut.WriteLn();

  (* save all OS/2 environment entries, each one prepended with 'SET ' *)
  ch := SYSTEM.ADR( OS2Env^ );
  WHILE ch^ <> 0C DO
  (*InOut.WriteString( "SET " );*)
    FileSystem.WriteString( Out, "SET " );
    CheckFileState( Out );
    REPEAT
    (*InOut.Write( ch^ );*)
      FileSystem.WriteChar( Out, ch^ );
      CheckFileState( Out );
      ch := SYSTEM.ADR( ch^ ) + 1;
    UNTIL ch^ = 0C;
  (*InOut.WriteLn();*)
    FileSystem.WriteLn( Out );
    ch := SYSTEM.ADR( ch^ ) + 1;
  END;
  FileSystem.Close( Out );
END SaveEnv.
