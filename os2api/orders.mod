IMPLEMENTATION MODULE ORDERS;

(************************************************************************
  OS/2 2.x Presentation Manager:
           Functions to find out GOCA order sizes for the GPI.

  Copyright (c) 1992, 1994 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(*$CDECL+    C-style procedures                                         *)
(*$A1        BYTE alignment for record fields                           *)

IMPORT SYSTEM;

PROCEDURE BYTE_ORDER( oc:SYSTEM.BYTE ):BOOLEAN;
VAR
  i : SHORTCARD;
BEGIN
  i := oc;
  RETURN (i=OCODE_GNOP1) OR (i=OCODE_GESD);
END BYTE_ORDER;

PROCEDURE SHORT_ORDER( oc:SYSTEM.BYTE ):BOOLEAN;
VAR
  i : SHORTCARD;
BEGIN
  i := oc;
  RETURN ((i XOR OCODE2_1) AND  OCODE2_2) = OCODE2_2;
END SHORT_ORDER;

PROCEDURE LONG_ORDER( oc:SYSTEM.BYTE ):BOOLEAN;
VAR
  i : SHORTCARD;
BEGIN
  i := oc;
  RETURN NOT ( (i=OCODE_VLONG) OR BYTE_ORDER( i ) OR SHORT_ORDER( i ) );
END LONG_ORDER;

PROCEDURE VLONG_ORDER( oc:SYSTEM.BYTE ):BOOLEAN;
VAR
  i : SHORTCARD;
BEGIN
  i := oc;
  RETURN i = OCODE_VLONG;
END VLONG_ORDER;

END ORDERS.
