IMPLEMENTATION MODULE WINDDE2;

(************************************************************************
  OS/2 2.0 Presentation Manager DDE helper functions.
           25.02.95 20.06 : bug fixed : DDES_PABDATA()

  Copyright (c) 1992 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

IMPORT SYSTEM;
FROM   OS2DEF  IMPORT PSZ;
FROM   WINDDE1 IMPORT DDESTRUCT, DDEINIT, PCONVCONTEXT;

PROCEDURE DDES_PSZITEMNAME( VAR DDEStruct:DDESTRUCT ):PSZ;
BEGIN
  RETURN SYSTEM.ADR( DDEStruct ) + DDEStruct.offszItemName;
END DDES_PSZITEMNAME;

PROCEDURE DDES_PABDATA( VAR DDEStruct:DDESTRUCT ):PBYTEARRAY;
BEGIN
  RETURN SYSTEM.ADR( DDEStruct ) + DDEStruct.offabData;
END DDES_PABDATA;

PROCEDURE DDEI_PCONVCONTEXT( VAR DDEInit:DDEINIT ):PCONVCONTEXT;
BEGIN
  RETURN SYSTEM.ADR( DDEInit ) + DDEInit.offConvContext;
END DDEI_PCONVCONTEXT;

END WINDDE2.
