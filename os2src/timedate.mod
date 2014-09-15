IMPLEMENTATION MODULE TimeDate;
(*
    Title       : A group of time and date utilities
    Limitations : Limited to dates 31 December 1899 to 4 June 2079
    Author      : I.R. Matters (Ian.Matters@anu.edu.au)
    System      : Juergen Neuhoff's Modula-2 compiler on OS/2 v3.0
    Version     : 1.00
    Last Edit   : 16 June 1995
*)


FROM Conversions IMPORT CardToStr, StrToCard;
FROM Strings     IMPORT Append, Assign, Concat, Copy, Length;
FROM OS2DEF      IMPORT APIRET;
FROM DOSDATETIME IMPORT DATETIME, DosGetDateTime;


CONST Leap         =     4;  (* Leap years every 4 years *)
      Century      =   100;  (* Years in a century       *)
      QuadCentury  =   400;  (* Years in four centuries  *)
      NormalYear   =   365;  (* Days in a non-leap year  *)
      FirstYear    =  1900;  (* Our first year           *)
      DaysIn4Years =  1461;  (* Including leap day       *)
      Minute       =    60;  (* Seconds on one minute    *)
      Hour         =  3600;  (* Seconds in one hour      *)
      Day          = 86400;  (* Seconds in one day       *)


(* Local declarations *)

PROCEDURE Str (C, W : CARDINAL; VAR S: ARRAY OF CHAR);
(*
   Convert a CARDINAL to a string with a designated field width
*)
VAR result : BOOLEAN;
BEGIN
  result := CardToStr (C, S);
  WHILE (Length (S) < W) DO
    Concat (" ", S, S);
  END; 
END Str;


(* Procedure implementations *)

PROCEDURE DateNow(): CARDINAL;
(*
   Returns the current date as a CARDINAL
   containing the date serial number - 1 January 1900 = 1.
*)
VAR DateTime : DATETIME;
    rc       : APIRET;
BEGIN
  rc := DosGetDateTime (DateTime);
  RETURN (DateToSerialNumber (DateTime.year, DateTime.month, DateTime.day));
END  DateNow;


PROCEDURE DateToStr (N: CARDINAL; F: DateFormats; VAR S: ARRAY OF CHAR);
(*
   Convert a date serial number to a "DD/MM/YYYY" or
   "MM-DD-YYYY" date string - day 1 = 1 January 1900
*)
VAR Y, M, D : CARDINAL;
    S1, S2  : ARRAY [0..10] OF CHAR;
BEGIN
  S2 [0] := 0C;
  Append ("00/00/0000", S2);
  IF (F = US) THEN
    S2 [2] := '-';
    S2 [5] := '-';
  END;
  SerialNumberToDate (N, Y, M, D);

  (* Now do the days *)

  Str (D, 2, S1);
  IF (F = US) THEN
    IF (S1 [0] > '0') THEN
      S2 [3] := S1 [0];
    END;
    S2 [4] := S1 [1];
  ELSE
    IF (S1 [0] > '0') THEN
      S2 [0] := S1 [0];
    END;
    S2 [1] := S1 [1];
  END;

  (* Now do the months *)

  Str (M, 2, S1);
  IF (F = US) THEN
    IF (S1 [0] > '0') THEN
      S2 [0] := S1 [0];
    END;
    S2 [1] := S1 [1];
  ELSE
    IF (S1 [0] > '0') THEN
      S2 [3] := S1 [0];
    END;
    S2 [4] := S1 [1];
  END;

  (* Now do the year *)

  Str (Y, 4, S1);
  IF (S1 [0] > '0') THEN
    S2 [6] := S1 [0];
  END;
  IF (S1 [1] > '0') THEN
    S2 [7] := S1 [1];
  END;
  IF (S1 [2] > '0') THEN
    S2 [8] := S1 [2];
  END;
  S2 [9] := S1 [3];

  Assign (S2, S);
END DateToStr;


PROCEDURE SerialNumberToDate (N: CARDINAL; VAR Y, M, D: CARDINAL);
(*
   Convert a date serial number to a date - 1 January 1900 = 1
*)
VAR Feb : CARDINAL;
BEGIN
  IF (N = 0) THEN
    Y := FirstYear - 1;
    M := 12;
    D := DaysInMonth (Y, M);
  ELSE
    Y := CARDINAL (((Leap * (LONGINT (N) - 1)) DIV DaysIn4Years) + FirstYear);
    M := 1;
    D := CARDINAL (LONGINT (N) - LONGINT (DateToSerialNumber (Y, M, 1)) + 1);
  END;

  IF (D > 31) THEN
    INC (M);
    DEC (D, 31);
  ELSE
    RETURN;
  END;

  IF LeapYear (Y) THEN
    Feb := 29
  ELSE
    Feb := 28;
  END;

  IF (D > Feb) THEN
    INC (M);
    DEC (D, Feb);
  ELSE
    RETURN;
  END;

  IF (D > 31) THEN
    INC (M);
    DEC (D, 31);
  ELSE 
    RETURN;
  END;

  IF (D > 30) THEN
    INC (M);
    DEC (D, 30);
  ELSE
    RETURN;
  END;

  IF (D > 31) THEN
    INC (M);
    DEC (D, 31);
  ELSE
    RETURN;
  END;

  IF (D > 30) THEN
    INC (M);
    DEC (D, 30);
  ELSE
    RETURN;
  END;

  IF (D > 31) THEN
    INC (M);
    DEC (D, 31);
  ELSE
    RETURN;
  END;

  IF (D > 31) THEN
    INC (M);
    DEC (D, 31);
  ELSE
    RETURN;
  END;

  IF (D > 30) THEN
    INC (M);
    DEC (D, 30);
  ELSE
    RETURN;
  END;

  IF (D > 31) THEN
    INC (M);
    DEC (D, 31);
  ELSE
    RETURN;
  END;

  IF (D > 30) THEN
    INC (M);
    DEC (D, 30);
  ELSE
    RETURN;
  END;

END SerialNumberToDate;


PROCEDURE DateToSerialNumber (Y, M, D: CARDINAL): CARDINAL;
(*
   Convert a date to a date serial number - 1 January 1900 = 1
*)
VAR Feb : CARDINAL;
    N   : LONGINT;
BEGIN
  N := (NormalYear * (LONGINT (Y) - FirstYear)) +
       ((LONGINT (Y) - FirstYear - 1) DIV Leap) + LONGINT (D) + 1;

  IF LeapYear (Y) THEN
    Feb := 29
  ELSE
    Feb := 28;
  END;

  IF (M >  1) THEN
    INC (N, 31);
  END;

  IF (M >  2) THEN
    INC (N, Feb);
  END;

  IF (M >  3) THEN
    INC (N, 31);
  END;

  IF (M >  4) THEN
    INC (N, 30);
  END;

  IF (M >  5) THEN INC
    (N, 31);
  END;

  IF (M >  6) THEN INC
    (N, 30);
  END;

  IF (M >  7) THEN INC
    (N, 31);
  END;

  IF (M >  8) THEN
    INC (N, 31);
  END;

  IF (M >  9) THEN INC
    (N, 30);
  END;

  IF (M > 10) THEN INC
    (N, 31);
  END;

  IF (M > 11) THEN INC
    (N, 30);
  END;

  RETURN (CARDINAL (N));
END DateToSerialNumber;


PROCEDURE TimeNow(): LONGCARD;
(*
   Returns the current time of day as a LONGCARD
   containing the number of seconds since midnight.
*)
VAR DateTime : DATETIME;
    rc       : APIRET;
BEGIN
  rc := DosGetDateTime (DateTime);
  RETURN (Hour * LONGCARD (DateTime.hours)) +
          (Minute * LONGCARD (DateTime.minutes)) +
          LONGCARD (DateTime.seconds);
END TimeNow;


PROCEDURE TimeToStr (N: LONGCARD; VAR S: ARRAY OF CHAR);
(*
   Convert a time serial number to a "HH:MM:SS" time string
*)
VAR H, M, Sec : CARDINAL;
    S1, S2    : ARRAY [1..9] OF CHAR;
BEGIN
  S2 [1] := 0C;
  Append ("00:00:00", S2);
  SerialNumberToTime (N, H, M, Sec);

  Str (H, 2, S1);
  IF (S1 [1] > '0') THEN
    S2 [1] := S1 [1];
  END;
  S2 [2] := S1 [2];

  Str (M, 2, S1);
  IF (S1 [1] > '0') THEN
    S2 [4] := S1 [1];
  END;
  S2 [5] := S1 [2];

  Str (Sec, 2, S1);
  IF (S1 [1] > '0') THEN
    S2 [7] := S1 [1];
  END;
  S2 [8] := S1 [2];

  S2 [9] := 0C;
  Assign (S2, S);
END TimeToStr;


PROCEDURE SerialNumberToTime (N: LONGCARD; VAR H, M, S: CARDINAL);
(*
   Convert a "HH:MM:SS" time serial number to hours, minutes and seconds
*)
BEGIN
  H := CARDINAL (N DIV Hour);
  M := CARDINAL ((N - (H * Hour)) DIV Minute);
  S := CARDINAL (N - (H * Hour) - (M * Minute));
END SerialNumberToTime;


PROCEDURE TimeToSerialNumber (H, M, S: CARDINAL): LONGCARD;
(*
   Convert a time to a time serial number - seconds since midnight
*)
BEGIN
  RETURN (Hour * LONGCARD (H)) + (Minute * LONGCARD (M)) + LONGCARD (S);
END TimeToSerialNumber;


PROCEDURE TimeStrToTime (TS: ARRAY OF CHAR; VAR H, M, S: CARDINAL);
(*
   Convert a time string to hours, minutes and seconds
*)
VAR ok   : BOOLEAN;
    Temp : ARRAY [1..9] OF CHAR;
BEGIN
  Copy (TS, 0, 2, Temp);
  ok := StrToCard (Temp, H);
  Copy (TS, 3, 2, Temp);
  ok := StrToCard (Temp, M);
  Copy (TS, 6, 2, Temp);
  ok := StrToCard (Temp, S);
END TimeStrToTime;


PROCEDURE LeapYear (Y: CARDINAL): BOOLEAN;
(*
   Is the year a leap year?
*)
VAR Result: BOOLEAN;
BEGIN
  Result := FALSE;
  IF (Y = (Y DIV Leap) * Leap) THEN
    Result := TRUE;
    IF (Y = (Y DIV Century) * Century) THEN
      Result := FALSE;
      IF (Y = (Y DIV QuadCentury) * QuadCentury) THEN
        Result := TRUE;
      END;
    END;
  END;
  RETURN (Result);
END LeapYear;


PROCEDURE DaysInMonth (Y, M: CARDINAL): CARDINAL;
(*
   How many days are in a given month?
*)
VAR Result: CARDINAL;
BEGIN
  IF (M = 2) THEN
    IF LeapYear (Y) THEN
      Result := 29;
    ELSE
      Result := 28;
    END;
  ELSIF (M IN {4, 6, 9, 11}) THEN
    Result := 30;
  ELSE
    Result := 31;
  END;

  RETURN (Result);
END DaysInMonth;


BEGIN  (* Initialization *)
END TimeDate.
