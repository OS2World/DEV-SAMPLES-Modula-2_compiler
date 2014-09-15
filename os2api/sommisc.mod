IMPLEMENTATION MODULE SOMMISC;

(***************************************************************************
  OS/2 2.x/3.0 Miscellaneous development support for SOM.

  Copyright (c) 1994,1995 by Juergen Neuhoff
****************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

IMPORT SOM;
IMPORT SYSTEM;


PROCEDURE WriteString( str : ARRAY OF CHAR );
VAR
  i : LONGINT;
  j : SOM.INT;
BEGIN
  i := 0;
  WHILE (str[i] <> 0C) DO
    j := SOM.SOMOutCharRoutine( str[i] );
    INC(i);
  END;
END WriteString;


PROCEDURE WriteLn;
VAR
  j : SOM.INT;
BEGIN
  j := SOM.SOMOutCharRoutine( CHR( 13 ) );
  j := SOM.SOMOutCharRoutine( CHR( 10 ) );
END WriteLn;


PROCEDURE Debug( szClass, szMethod, szFile, szLine : ARRAY OF CHAR );
BEGIN
  somWriteString( szClass );
  somWriteString( "." );
  somWriteString( szMethod );
  somWriteString( " " );
  somWriteString( szFile );
  somWriteString( " " );
  somWriteString( szLine );
  somWriteLn();
END Debug;


PROCEDURE somString( s : ARRAY OF CHAR ) : SOM.ADDRESS;
BEGIN
  RETURN SYSTEM.ADR( s );
END somString;


BEGIN
  somDebug := Debug;
  somWriteString := WriteString;
  somWriteLn := WriteLn;
END SOMMISC.
