IMPLEMENTATION MODULE GPIDEF;

(************************************************************************
  OS/2 2.0 GPI general functions.

  Copyright (c) 1992 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

IMPORT SYSTEM;

PROCEDURE MAKEFIXED( intpart,fractpart:SYSTEM.WORD ):FIXED;
VAR
  Lo,Hi : INTEGER;
BEGIN
  Lo := intpart;
  Hi := fractpart;
  RETURN LONG( Lo ) + LONG( Hi ) SHL 16;
END MAKEFIXED;

PROCEDURE FIXEDFRAC( fx:SYSTEM.LONGWORD ):CARDINAL;
VAR
  i : LONGCARD;
BEGIN
  i := fx;
  RETURN SHORT( i );
END FIXEDFRAC;

PROCEDURE FIXEDINT( fx:SYSTEM.LONGWORD ):INTEGER;
VAR
  i : LONGCARD;
BEGIN
  i := fx;
  RETURN VAL( INTEGER, SHORT( i SHR 16 ) );
END FIXEDINT;

END GPIDEF.
