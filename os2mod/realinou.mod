IMPLEMENTATION MODULE RealInOut;

(**************************************************************************
   OS/2 2.x  Modula-2 standard utility for floating point input/output.

   Copyright (c) 1993 by Juergen Neuhoff
**************************************************************************)

IMPORT SYSTEM;
IMPORT InOut;
IMPORT RealConversions;
IMPORT Strings;


CONST
  IsLongReal = (SYSTEM.TSIZE( REAL ) = SYSTEM.TSIZE( LONGREAL ));
  DBLDIG     = 15;
  SGLDIG     = 6;


PROCEDURE ReadReal( VAR x : REAL );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
BEGIN
  InOut.ReadString( Buffer );
  IF InOut.Done THEN
    Done := RealConversions.StrToReal( Buffer, x );
  ELSE
    Done := FALSE;
  END;
END ReadReal;


PROCEDURE WriteFloat( IsLong : BOOLEAN; x : LONGREAL; n : INTEGER );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
  j,k    : INTEGER;
BEGIN
  j := 7; (* at least  digit "." "E" ["+"|"-"] digit digit digit  needed *)
  IF x < 0.0 THEN
    INC( j );
  END;
  IF n < j THEN
    Done := FALSE;
    RETURN;
  END;
  IF IsLong THEN
    k := DBLDIG-1;
  ELSE
    k := SGLDIG-1;
  END;
  IF n <= j+k THEN  (* j <= n <= j+k *)
    k := n - j;
  END;
  IF IsLong THEN
    Done := RealConversions.LongRealToStr( x, -k, Buffer );
  ELSE
    Done := RealConversions.ShortRealToStr( SHORT( x ), -k, Buffer );
  END;
  IF NOT Done THEN
    RETURN;
  END;
  k := SHORT( Strings.Length( Buffer ) );
  IF k > n THEN
    (* Buffer is larger than wanted size 'n' *)
    j := k - n;
    IF Buffer[ j-1 ] = ' ' THEN
      (* Buffer string truncated thru removal of leading blanks *)
      Strings.Delete( Buffer, 0, j );
    ELSE
      (* Buffer too large, use all blanks *)
      Done := FALSE;
      RETURN;
    END;
  ELSIF k < n THEN
    (* Buffer is smaller than wanted size, prepend blanks *)
    Strings.Fill( 0, n-k, ' ', Buffer );
  END;
  InOut.WriteString( Buffer );
  Done := InOut.Done;
END WriteFloat;


PROCEDURE WriteReal( x : REAL; n : INTEGER );
BEGIN
  WriteFloat( IsLongReal, LONG( x ), n );
END WriteReal;


PROCEDURE ReadLongReal( VAR x : LONGREAL );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
BEGIN
  InOut.ReadString( Buffer );
  IF InOut.Done THEN
    Done := RealConversions.StrToLongReal( Buffer, x );
  ELSE
    Done := FALSE;
  END;
END ReadLongReal;


PROCEDURE WriteLongReal( x : LONGREAL; n : INTEGER );
BEGIN
  WriteFloat( TRUE, x, n );
END WriteLongReal;


PROCEDURE ReadShortReal( VAR x : SHORTREAL );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
BEGIN
  InOut.ReadString( Buffer );
  IF InOut.Done THEN
    Done := RealConversions.StrToShortReal( Buffer, x );
  ELSE
    Done := FALSE;
  END;
END ReadShortReal;


PROCEDURE WriteShortReal( x : SHORTREAL; n : INTEGER );
BEGIN
  WriteFloat( FALSE, LONG( x ), n );
END WriteShortReal;


PROCEDURE WriteFixPt( x : REAL; n,k : INTEGER );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
BEGIN
  Done := RealConversions.RealToFixPt( x, n, k, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteFixPt;


PROCEDURE WriteLongFixPt( x : LONGREAL; n,k : INTEGER );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
BEGIN
  Done := RealConversions.LongRealToFixPt( x, n, k, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteLongFixPt;


PROCEDURE WriteShortFixPt( x : SHORTREAL; n,k : INTEGER );
VAR
  Buffer : ARRAY [0..40] OF CHAR;
BEGIN
  Done := RealConversions.ShortRealToFixPt( x, n, k, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteShortFixPt;


PROCEDURE WriteRealOct( x : REAL );
BEGIN
  IF IsLongReal THEN
    WriteLongRealOct( LONG( x ) );
  ELSE
    WriteShortRealOct( SHORT( x ) );
  END;
END WriteRealOct;


PROCEDURE WriteLongRealOct( x : LONGREAL );
CONST
  OctDigits = (SYSTEM.TSIZE( LONGREAL ) * 8) DIV 3 + 1;
VAR
  Buffer : ARRAY [0..OctDigits+1] OF CHAR;
BEGIN
  Done := RealConversions.LongRealToOctStr( x, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteLongRealOct;


PROCEDURE WriteShortRealOct( x : SHORTREAL );
CONST
  OctDigits = (SYSTEM.TSIZE( SHORTREAL ) * 8) DIV 3 + 1;
VAR
  Buffer : ARRAY [0..OctDigits+1] OF CHAR;
BEGIN
  Done := RealConversions.ShortRealToOctStr( x, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteShortRealOct;


PROCEDURE WriteRealHex( x : REAL );
BEGIN
  IF IsLongReal THEN
    WriteLongRealHex( LONG( x ) );
  ELSE
    WriteShortRealHex( SHORT( x ) );
  END;
END WriteRealHex;


PROCEDURE WriteLongRealHex( x : LONGREAL );
CONST
  HexDigits = SYSTEM.TSIZE( LONGREAL ) * 2 + 2;
VAR
  Buffer : ARRAY [0..HexDigits+1] OF CHAR;
BEGIN
  Done := RealConversions.LongRealToHexStr( x, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteLongRealHex;


PROCEDURE WriteShortRealHex( x : SHORTREAL );
CONST
  HexDigits = SYSTEM.TSIZE( SHORTREAL ) * 2 + 2;
VAR
  Buffer : ARRAY [0..HexDigits+1] OF CHAR;
BEGIN
  Done := RealConversions.ShortRealToHexStr( x, Buffer );
  IF Done THEN
    InOut.WriteString( Buffer );
    Done := InOut.Done;
  END;
END WriteShortRealHex;


END RealInOut.
