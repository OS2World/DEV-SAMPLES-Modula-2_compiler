IMPLEMENTATION MODULE Keys;
(*
    Description : Symbolic constants for non-ASCII keyboard functions
    Author      : I.R. Matters (Ian.Matters@anu.edu.au)
    System      : Juergen Neuhoff's Modula-2 compiler on OS/2 v3.0
    Version     : 1.00
    Last Edit   : 19 June 1995

    Notes       : The values described in this table assume that the
                  second byte of the keyboard code is shifted by 128
                  decimal where possible.

                  Alt-Spacebar is specially mapped to 215C;
                  The Ctrl-2 key is remapped to an ASCII nul;
                  Ctrl-PrintScreen is only supported in full screen mode.
*)


FROM SYSTEM   IMPORT TSIZE;
FROM Terminal IMPORT GetEcho, SetEcho, Read;
FROM Keyboard IMPORT ScanCode;

(*$XL+  Modula-2 language extensions: allowed extended imports *)

IMPORT FROM KBD32;


PROCEDURE GetKeyChar(): CHAR;
(*
   Read a key as a CHAR.  If it's a special key,
   return then second byte with the high bit set.
*)
CONST ASCIInul     =   0C;
      ASCIIetx     =   3C;
      HighBit      =  80H;
      GrayControls = 340C;  (* Prefix for gray arrows and movement keys *)
VAR ch        : CHAR;
    shiftKeys : Modifiers;
    oldFlag   : BOOLEAN;
BEGIN
  GetEcho (oldFlag);
  SetEcho (FALSE);
  Read (ch);
  SetEcho (oldFlag);
  GetModifiers (shiftKeys);

  IF ((ch = ASCIInul) OR (ch = GrayControls)) THEN
    ch := CHR (ScanCode());
    IF (ch = ASCIIetx) THEN  (* Remap Ctrl-2 key to ASCIInul *)
      ch := ASCIInul;
    ELSE
      ch := CHR (ORD (ch) OR HighBit);
    END;
  END;  (* IF *)

  IF ((ch = ' ') AND (AltKey IN shiftKeys)) THEN  (* Map Alt-Spacebar *)
    ch := AltSpacebar;
  END;

  RETURN (ch);
END GetKeyChar;


PROCEDURE GetModifiers (VAR State: Modifiers);
(*
   Check the status of the various shift and lock keys
*)
VAR rc16    : APIRET16;
    kbdInfo : KBDINFO;
BEGIN
  kbdInfo.Size := TSIZE (KBDINFO);
  rc16 := KbdGetStatus (kbdInfo, 0);
  State := Modifiers (kbdInfo.State);
END GetModifiers;


END Keys.