IMPLEMENTATION MODULE CRT;
(*
    Title     : An adaption of the Borland's Turbo Pascal CRT Unit
    Author    : I.R. Matters (Ian.Matters@anu.edu.au)
    System    : Juergen Neuhoff's Modula-2 compiler on OS/2 v3.0
    Version   : 1.03
    Last Edit : 28 June 1995
*)

(*$XL+  Modula-2 language extensions: '_' allowed for symbol names *)

FROM InOut IMPORT WriteCard, WriteString, WriteLn;

IMPORT ASCII;
IMPORT Terminal;
IMPORT Strings;
IMPORT Conversions;

FROM SYSTEM             IMPORT BYTE, WORD, ADR, TSIZE;

FROM OS2DEF             IMPORT APIRET;

FROM DOSPROCESS         IMPORT DosSleep, DosBeep;

FROM VIO                IMPORT VioSetCurPos, VioGetCurPos, VioSetCurType,
			       VioGetCurType, VioSetAnsi, VioScrollDn,
                               VioScrollUp, VioScrollRt, VioScrollLf,
                               VioReadCharStr, VioGetMode, VioSetMode,
                               VioSetState, ANSI_ON, APIRET16,
                               VIOCURSORINFO, VIOMODEINFO, COLORS_16;

(* Local declarations *)


VAR MaxX, MaxY     : CARDINAL;
    rc16           : APIRET16;
    OrigScreenInfo : VIOMODEINFO;


PROCEDURE Top(): CARDINAL;
(*
   Return the top margin - integer based
*)
BEGIN
  RETURN (CARDINAL (ByteToWord (Hi (WindMin))) + 1);
END Top;


PROCEDURE Bottom(): CARDINAL;
(*
   Return the bottom margin - integer based
*)
BEGIN
  RETURN (CARDINAL (ByteToWord (Hi (WindMax))) + 1);
END Bottom;


PROCEDURE Left(): CARDINAL;
(*
   Return the left margin - integer based
*)
BEGIN
  RETURN (CARDINAL (ByteToWord (Lo (WindMin))) + 1);
END Left;


PROCEDURE Right(): CARDINAL;
(*
   Return the right margin - integer based
*)
BEGIN
  RETURN (CARDINAL (ByteToWord (Lo (WindMax))) + 1);
END Right;


PROCEDURE LogicalAND (n1, n2: CARDINAL): CARDINAL;
(*
   Perform a logical AND on 2 cardinals
*)
BEGIN
  RETURN (n1 AND n2);
END LogicalAND;


PROCEDURE FillAttr(): BYTE;
(*
   Calculate the fill attribute from the text attribute
*)
CONST Backgnd = 70H;  (* Mask for the background colors excl. blink *)
VAR temp : CARDINAL;
BEGIN

  (* Make the foreground color the bright inverse of the background *)

  temp := ByteToWord (TextAttr);
  temp := LogicalAND (temp, Backgnd) +
	  (7 - (LogicalAND (temp, Backgnd) DIV 16)) + 8;
  RETURN (Lo (temp));
END FillAttr;


PROCEDURE GetScreenInfo (VAR Info : VIOMODEINFO);
(*
   Recover the VIO screen information
*)
VAR temp : VIOMODEINFO;
BEGIN
  temp.Size  := 8;
  rc16       := VioGetMode (temp, 0);
  Info.Type  := temp.Type;
  Info.Color := temp.Color;
  Info.Col   := temp.Col;
  Info.Row   := temp.Row;
END GetScreenInfo;


PROCEDURE FindMode (Info : VIOMODEINFO): CARDINAL;
(*
   Determine the screen mode from the mode information
*)
VAR result : CARDINAL;
BEGIN
  WITH Info DO

    (* Crude - but it works *)

    IF ((Col = 40) AND (Row = 25) AND (Type = 5)) THEN
      result := BW40;
    ELSIF ((Col = 40) AND (Row = 25) AND (Type = 1)) THEN
      result := CO40;
    ELSIF ((Col = 80) AND (Row = 25) AND (Type = 5)) THEN
      result := BW80;
    ELSIF ((Col = 80) AND (Row = 25) AND (Type = 1)) THEN
      result := CO80;
    ELSIF ((Col = 132) AND (Row = 25) AND (Type = 5)) THEN
      result := BW132;
    ELSIF ((Col = 132) AND (Row = 25) AND (Type = 1)) THEN
      result := CO132;
    ELSIF ((Col = 80) AND (Row = 25) AND (Type = 0)) THEN
      result := Mono;
    ELSIF ((Col = 40) AND (Row = 50) AND (Type = 5)) THEN
      result := BW40 + Font8x8;
    ELSIF ((Col = 40) AND (Row = 50) AND (Type = 1)) THEN
      result := CO40 + Font8x8;
    ELSIF ((Col = 80) AND (Row = 50) AND (Type = 5)) THEN
      result := BW80 + Font8x8;
    ELSIF ((Col = 80) AND (Row = 50) AND (Type = 1)) THEN
      result := CO80 + Font8x8;
    ELSIF ((Col = 132) AND (Row = 43) AND (Type = 5)) THEN
      result := BW132 + Font8x8;
    ELSIF ((Col = 132) AND (Row = 43) AND (Type = 1)) THEN
      result := CO132 + Font8x8;
    ELSIF ((Col = 80) AND (Row = 50) AND (Type = 0)) THEN
      result := Mono + Font8x8;
    ELSE
      result := CO80;  (* Make it CO80 if we can't tell *)
    END;    
  END;  (* WITH *)

  RETURN (result);
END FindMode;


(* Procedure implementations *)

PROCEDURE KeyPressed(): BOOLEAN;
(*
   Has a character key been pressed on the keyboard?
*)
VAR ch     : CHAR;
    sc     : SHORTCARD;
    result : BOOLEAN;
BEGIN
  Terminal.BusyRead (ch);
  sc := Terminal.ScanCode();
  Terminal.ReadAgain;
  result := ((ch # 0C) OR (sc # 0));
  RETURN (result);
END KeyPressed;


PROCEDURE ReadKey(): CHAR;
(*
   Read the current key from the keyboard buffer without echo
*)
VAR ch      : CHAR;
    oldFlag : BOOLEAN;
BEGIN
  Terminal.GetEcho (oldFlag);
  Terminal.SetEcho (FALSE);
  Terminal.Read (ch);
  Terminal.SetEcho (oldFlag);
  RETURN (ch);
END ReadKey;


PROCEDURE TextMode (Mode: CARDINAL);
(*
   Select a new text screen mode
*)
VAR State    : RECORD
                 length,
                 type,
                 select : CARDINAL;
               END;

    ModeInfo : VIOMODEINFO;

    Extended : BOOLEAN;

  PROCEDURE ValidMode (VAR N : CARDINAL; VAR Ext : BOOLEAN) : BOOLEAN;
  (*
     Determine if it's a valid mode and if it's 43/50 line mode
  *)
  CONST bmMask = 07H;  (* Mask for basic modes *)
  VAR temp   : CARDINAL;
      result : BOOLEAN;
  BEGIN
    temp   := N;
    Ext    := (LogicalAND (temp, Font8x8) = Font8x8);
    temp   := LogicalAND (temp, bmMask);
    result := ((temp >= BW40) AND (temp <= CO132)) OR (temp = Mono);
    IF result THEN
      N := temp;
    END;
    RETURN (result);
  END ValidMode;

BEGIN  (* TextMode *)
  IF ValidMode (Mode, Extended) THEN

    WITH State DO
      length := 6;
      type   := 6;
      select := 0;
    END;
    rc16 := VioSetState (State, 0);

    WITH ModeInfo DO
      Size := 8;  (* Data structure is 8 bytes long *)

      CASE Mode OF

        BW40  : MaxX   := 40;
                IF Extended THEN
                  MaxY := 50;
                ELSE
                  MaxY := 25;
                END;
                Type   := 5;
                Color  := COLORS_16;
                Col    := MaxX;
                Row    := MaxY;      |

        CO40  : MaxX   := 40;
                IF Extended THEN
                  MaxY := 50;
                ELSE
                  MaxY := 25;
                END;
                Type   := 1;
                Color  := COLORS_16;
                Col    := MaxX;
                Row    := MaxY;      |

        BW80  : MaxX   := 80;
                IF Extended THEN
                  MaxY := 50;
                ELSE
                  MaxY := 25;
                END;
                Type   := 5;
                Color  := COLORS_16;
                Col    := MaxX;
                Row    := MaxY;      |

        CO80  : MaxX   := 80;
                IF Extended THEN
                  MaxY := 50;
                ELSE
                  MaxY := 25;
                END;
                Type   := 1;
                Color  := COLORS_16;
                Col    := MaxX;
                Row    := MaxY;      |

        BW132 : MaxX   := 132;
                IF Extended THEN
                  MaxY := 43;
                ELSE
                  MaxY := 25;
                END;
                Type   := 5;
                Color  := COLORS_16;
                Col    := MaxX;
                Row    := MaxY;      |

        CO132 : MaxX   := 132;
                IF Extended THEN
                  MaxY := 43;
                ELSE
                  MaxY := 25;
                END;
                Type   := 1;
                Color  := COLORS_16;
                Col    := MaxX;
                Row    := MaxY;      |

        Mono  : MaxX   := 80;
                IF Extended THEN
                  MaxY := 50;
                ELSE
                  MaxY := 25;
                END;
                Type   := 0;
                Color  := 0;
                Col    := MaxX;
                Row    := MaxY;

      END;  (* CASE *)
 
    END;  (* WITH *)  

    rc16 := VioSetMode (ModeInfo, 0);
    GetScreenInfo (ModeInfo);
    LastMode := FindMode (ModeInfo);
    Window (1, 1, MaxX, MaxY);
    Cursor (SmallCursor);
    NormVideo;
    ClrScr;  
  END;
END TextMode;


PROCEDURE Window (X1, Y1, X2, Y2: CARDINAL);
(*
   Set the current window location and size
*)
BEGIN
(**
  IF ((Y1 > 0) AND (Y1 <= MaxY)) THEN
    Top := Y1;
  END;  (* IF *)
  IF ((X1 > 0) AND (X1 <= MaxX)) THEN
    Left := X1;
  END;  (* IF *)
  IF ((Y2 > 0) AND (Y2 <= MaxY)) THEN
    Bottom := Y2;
  END;  (* IF *)
  IF ((X2 > 0) AND (X2 <= MaxX)) THEN
    Right := X2;
  END;  (* IF *)
**)
  WindMin := WORD (((Y1 - 1) * 256) + (X1 - 1));
  WindMax := WORD (((Y2 - 1) * 256) + (X2 - 1));
END Window;


PROCEDURE GotoXY (X, Y: CARDINAL);
(*
   Position the cursor relative to the current window
*)

  PROCEDURE InWindow (X, Y: CARDINAL): BOOLEAN;
  (*
     Is the X,Y location within the current window?
  *)
  VAR result : BOOLEAN;
  BEGIN
    result := (((X + Left()) > 1) AND ((X + Left()) <= (Right() + 1)) AND
	      ((Y + Top()) > 1) AND ((Y + Top()) <= (Bottom() + 1)));
    RETURN (result);
  END InWindow;

BEGIN  (* GotoXY *)
  IF InWindow (X, Y) THEN
    X := X + Left() - 1;
    Y := Y + Top() - 1;
    rc16 := VioSetCurPos (Y - 1, X - 1, 0);
  END;
END GotoXY;


PROCEDURE CursorXY (VAR X, Y: CARDINAL);
(*
   Read the current cursor position relative to the current window
*)
BEGIN
  rc16 := VioGetCurPos (Y, X, 0);
  X := (X + 2) - Left();
  Y := (Y + 2) - Top();
END CursorXY;


PROCEDURE WhereX(): CARDINAL;
(*
   Return the current horizontal cursor position in the current window
*)
VAR x, y: CARDINAL;
BEGIN
  CursorXY (x, y);
  RETURN (x);
END WhereX;


PROCEDURE WhereY(): CARDINAL;
(*
   Return the current vertical cursor position in the current window
*)
VAR x, y: CARDINAL;
BEGIN
  CursorXY (x, y);
  RETURN (y);
END WhereY;


PROCEDURE ClrScr;
(*
   Clear the current window and set the cursor home
*)
VAR cell : ARRAY [0..1] OF BYTE;
BEGIN
  cell [0] := 32; (* Fill character is a "space" *)
  cell [1] := FillAttr();
  rc16 := VioScrollDn (Top() - 1, Left() - 1, Bottom() - 1, Right() - 1,
		       Bottom() - Top() + 1, cell, 0);
  GotoXY (1, 1);
END ClrScr;


PROCEDURE ClrEOL;
(*
   Clear the current window to the end of the current line
*)
VAR cell : ARRAY [0..1] OF BYTE;
BEGIN
  cell [0] := 32; (* Fill character is a "space" *)
  cell [1] := FillAttr();
  rc16 := VioScrollRt (WhereY() + Top() - 2, WhereX() + Left() - 2,
		       WhereY() + Top() - 2, Right() - 1,
		       Right() - Left() + 1, cell, 0);
END ClrEOL;


PROCEDURE ClrEOS;
(*
   Clear the current window to the end of the screen
*)
VAR x, y, i : CARDINAL;
BEGIN
  ClrEOL;
  CursorXY (x, y);
  i := y + 1;
  WHILE (i <= MaxHeight() ) DO
    GotoXY (1, i);
    ClrEOL;
    INC (i);
  END;
  GotoXY (x, y);
END ClrEOS;


PROCEDURE InsLine;
(*
   Insert a blank line at the cursor position
*)
VAR cell : ARRAY [0..1] OF BYTE;
BEGIN
  cell [0] := 32; (* Fill character is a "space" *)
  cell [1] := FillAttr();
  rc16 := VioScrollDn (WhereY() + Top() - 2, Left() - 1,
                       Bottom() - 1, Right() - 1, 1, cell, 0);
END InsLine;


PROCEDURE DelLine;
(*
   Delete the line at the cursor position
*)
VAR cell : ARRAY [0..1] OF BYTE;
BEGIN
  cell [0] := 32; (* Fill character is a "space" *)
  cell [1] := FillAttr();
  rc16 := VioScrollUp (WhereY() + Top() - 2, Left() - 1,
                       Bottom() - 1, Right() - 1, 1, cell, 0);
END DelLine;


PROCEDURE TextColor (Color: CARDINAL);
(*
   Select the foreground character color
*)
CONST HiColor   = 08H;  (* Mask for the "bright" colors *)
      Foregnd   = 07H;  (* Mask for low foreground colors excl. blink *)
      Backgnd   = 70H;  (* Mask for the background colors excl. blink *)
      ValidMask = 8FH;  (* Mask for the valid range of colors *)
VAR s    : ARRAY [0..19] OF CHAR;
    temp : CARDINAL;
BEGIN
  Color := LogicalAND (Color, ValidMask);
  s := "[0";  (* Return to "normal" mode *)

  IF (LogicalAND (Color, HiColor) = HiColor) THEN
    Strings.Append (";1", s);  (* Enable "bold" mode *)
  END;

  IF (LogicalAND (Color, Blink) = Blink) THEN
    Strings.Append (";5", s);  (* Enable "blink" mode *)
  END;

  CASE (LogicalAND (Color, Foregnd)) OF

    Black        : Strings.Append (";30m", s); |

    Blue         : Strings.Append (";34m", s); |

    Green        : Strings.Append (";32m", s); |

    Cyan         : Strings.Append (";36m", s); |

    Red          : Strings.Append (";31m", s); |

    Magenta      : Strings.Append (";35m", s); |

    Brown        : Strings.Append (";33m", s); |

    LightGray    : Strings.Append (";37m", s);

  END;

  Terminal.Write (ASCII.esc);
  Terminal.WriteString (s);

  temp := ByteToWord (TextAttr);
  temp := LogicalAND (temp, Backgnd) + ORD (Color);
  TextAttr := Lo (temp);

  (* Now fix up the background *)

  TextBackground (LogicalAND (ByteToWord (TextAttr), Backgnd) DIV 16);
END TextColor;


PROCEDURE TextBackground (Color: CARDINAL);
(*
   Select the background color
*)
CONST Foregnd   = 0FH;  (* Mask for the foreground colors *)
      ValidMask = 07H;  (* Mask for the valid range of colors *)
VAR temp : CARDINAL;
BEGIN
  Color := LogicalAND (Color, ValidMask);
  CASE Color OF

    Black     : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[40m"); |

    Blue      : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[44m"); |

    Green     : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[42m"); |

    Cyan      : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[46m"); |

    Red       : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[41m"); |

    Magenta   : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[45m"); |

    Brown     : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[43m"); |

    LightGray : Terminal.Write (ASCII.esc);
		Terminal.WriteString ("[47m");

  END;

  temp := ByteToWord (TextAttr);
  temp := LogicalAND (temp, Foregnd) + (ORD (Color) * 16);
  TextAttr := Lo (temp);
END TextBackground;


PROCEDURE LowVideo;
(*
   Select low intensity character colors
*)
CONST fgMask = 87H;  (* Mask for low foreground and blink *)
BEGIN
  TextColor (LogicalAND (ByteToWord (TextAttr), fgMask));
END LowVideo;


PROCEDURE HighVideo;
(*
   Select high intensity character colors
*)
CONST fgMask  = 87H;  (* Mask for low foreground and blink *)
      HiColor = 08H;  (* Add-in value for the "bright" colors *)
BEGIN
  TextColor (LogicalAND (ByteToWord (TextAttr), fgMask) + HiColor);
END HighVideo;


PROCEDURE NormVideo;
(*
   Return the text attributes to the default setting
*)
BEGIN
  TextBackground (Black);
  TextColor (LightGray);
END NormVideo;


PROCEDURE Delay (mS: CARDINAL);
(*
  Delay for a number of milliseconds
*)
VAR rc : APIRET;
BEGIN
  rc := DosSleep (mS);
END Delay;


PROCEDURE Sound (Hz, mS : CARDINAL);
(*
  Produce a sound at the desired frequency for a number of milliseconds
*)
VAR rc : APIRET;
BEGIN
  rc := DosBeep (Hz, mS);
END Sound;


PROCEDURE Bell;
(*
  Make a pleasant bell sound
*)
BEGIN
  Sound (1175, 50);  (* D6 *)
  Sound (880, 25);   (* A5 *)
END Bell;


PROCEDURE Buzz;
(*
  Make a buzzing sound
*)
BEGIN
  Sound (100, 500);
END Buzz;


PROCEDURE ClearKB;
(*
   Flush the keyboard type-ahead buffer
*)
VAR ch : CHAR;
BEGIN
  WHILE KeyPressed() DO
    ch := ReadKey();
  END;
END ClearKB;


PROCEDURE Cursor (Mode: CursorModes);
(*
  Set the cursor to the requested mode 
*)
VAR cursor     : VIOCURSORINFO;
    smallCell,
    mode132    : BOOLEAN;
BEGIN
  smallCell := (LogicalAND (LastMode, Font8x8) = Font8x8);
  mode132   := (CARDINAL (LastMode) = BW132) OR (CARDINAL (LastMode) = CO132);

  WITH cursor DO
    Width := 0;

    CASE Mode OF

      NoCursor      : Start :=     0;
		      End   :=     0;
		      Attr  := CARDINAL (-1);
		      rc16  := VioSetCurType (cursor, 0); |

      SmallCursor   : IF mode132 THEN
                        Start :=  12;
		        End   :=  13;
                      ELSIF smallCell THEN
                        Start :=   6;
		        End   :=   7;
                      ELSE
                        Start :=  14;
                        End   :=  15;
                      END;
		      Attr  :=     0;
		      rc16  := VioSetCurType (cursor, 0); |

      LargeCursor   : Start :=     0;
                      IF mode132 THEN
                        End   :=  13;
                      ELSIF smallCell THEN
                        End   :=   7;
                      ELSE
		        End   :=  15;
                      END;
		      Attr  :=     0;
		      rc16  := VioSetCurType (cursor, 0);
    END;
  END;
END Cursor;


PROCEDURE CurrentCursor(): CursorModes;
(*
  Get the cursor mode 
*)
VAR cursor : VIOCURSORINFO;
    Mode   : CursorModes;
BEGIN
  rc16 := VioGetCurType (cursor, 0);
  WITH cursor DO
    IF Attr = CARDINAL (-1) THEN
      Mode := NoCursor;
    ELSIF Start = 0 THEN
      Mode := LargeCursor;
    ELSE
      Mode := SmallCursor;
    END;  (* IF *)
  END;  (* WITH *)
  RETURN (Mode);
END CurrentCursor;


PROCEDURE ByteToWord (B : BYTE): WORD;
(*
   Convert a byte to a word
*)
VAR card : CARDINAL;
BEGIN
  card := SHORTCARD (B);
  RETURN (WORD (card));
END ByteToWord;


PROCEDURE Lo (W : WORD): BYTE;
(*
   Extract the low order byte from a word
*)
VAR temp : ARRAY [0..1] OF BYTE;
BEGIN
  temp := W;
  RETURN (temp [0]);
END Lo;


PROCEDURE Hi (W : WORD): BYTE;
(*
   Extract the high order byte from a word
*)
VAR temp : ARRAY [0..1] OF BYTE;
BEGIN
  temp := W;
  RETURN (temp [1]);
END Hi;


PROCEDURE ScreenChar(): CHAR;
(*
   Read a character from the screen at the cursor position
*)
VAR CharStr : ARRAY [0..0] OF CHAR;
    Size    : CARDINAL;
BEGIN
  Size := 1;
  rc16 := VioReadCharStr (CharStr, Size, WhereY() + Top() - 2,
			  WhereX() + Left() - 2, 0);
  RETURN (CharStr [0]);
END ScreenChar;


PROCEDURE MaxWidth(): CARDINAL;
(*
   What is the full screen width?
*)
BEGIN
  RETURN (MaxX);
END MaxWidth;


PROCEDURE MaxHeight(): CARDINAL;
(*
   What is the full screen height?
*)
BEGIN
  RETURN (MaxY);
END MaxHeight;


PROCEDURE InsChar;
(*
   Insert a blank character at the cursor position
*)
VAR cell : ARRAY [0..1] OF BYTE;
BEGIN
  cell [0] := 32; (* Fill character is a "space" *)
  cell [1] := FillAttr();
  rc16 := VioScrollRt (WhereY() + Top() - 2, WhereX() + Left() - 2,
		       WhereY() + Top() - 2, Right() - 1, 1, cell, 0);
END InsChar;


PROCEDURE DelChar;
(*
   Delete the character at the cursor position
*)
VAR cell : ARRAY [0..1] OF BYTE;
BEGIN
  cell [0] := 32; (* Fill character is a "space" *)
  cell [1] := FillAttr();
  rc16 := VioScrollLf (WhereY() + Top() - 2, WhereX() + Left() - 2,
		       WhereY() + Top() - 2, Right() - 1, 1, cell, 0);
END DelChar;


PROCEDURE InitialMode(): CARDINAL;
(*
   What was the initial screen mode?
*)
BEGIN
  RETURN (FindMode (OrigScreenInfo));
END InitialMode;


PROCEDURE SetTextAttr (Attr : BYTE);
(*
   Set the screen colors to the required attributes
*)
CONST Backgnd = 70H;  (* Mask for the background colors excl. blink *)
      Foregnd = 8FH;  (* Mask for the foreground colors incl. blink *)
BEGIN
  TextAttr := Attr;
  TextColor (LogicalAND (ByteToWord (Attr), Foregnd));
  TextBackground (LogicalAND (ByteToWord (Attr), Backgnd) DIV 16);
END SetTextAttr;


BEGIN  (* Initialization *)
  rc16 := VioSetAnsi (ANSI_ON, 0);
  GetScreenInfo (OrigScreenInfo);
  TextAttr := LightGray;
  TextMode ( InitialMode() );
END CRT.
