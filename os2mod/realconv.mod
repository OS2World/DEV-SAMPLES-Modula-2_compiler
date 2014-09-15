IMPLEMENTATION MODULE RealConversions;

(**************************************************************************
   OS/2 2.x  Modula-2 utility for float conversions to/from strings.

   Copyright (c) 1993 by Juergen Neuhoff
**************************************************************************)

(*$XL+       Modula-2 language extensions: allow typed constants         *)

FROM   SYSTEM   IMPORT ADR, TSIZE;
IMPORT Strings;
IMPORT Conversions;

CONST
  INFINITY         : ARRAY OF LONGCARD = [ 00000000H, 7FF00000H ];
  INDEFINITE       : ARRAY OF LONGCARD = [ 00000000H,0FFF80000H ];
  ShortINFINITY    =  7F800000H;
  ShortINDEFINITE  = 0FFC00000H;
  SGLDIG           = 6;        (* max # of significand dec. digits   *)
  DBLDIG           = 15;       (* max # of significand dec. digits   *)
  DIGMAX           = DBLDIG*2; (* max # of digits in string          *)
                               (* ( *2 is a good fudge factor)       *)
  DIGPREC          = DBLDIG+2; (* max # of significant digits        *)
                               (* (+2 for fractional part after min  *)
                               (*     significant digits)            *)
  IsLongReal       = (TSIZE( REAL ) = TSIZE( LONGREAL ));

CONST
  negtab  : ARRAY OF LONGREAL =
  [1.E-256, 1.E-128, 1.E-64, 1.E-32, 1.E-16, 1.E-8, 1.E-4, 1.E-2, 1.E-1, 1.0];
  postab  : ARRAY OF LONGREAL =
  [1.E+256, 1.E+128, 1.E+64, 1.E+32, 1.E+16, 1.E+8, 1.E+4, 1.E+2, 1.E+1];

TYPE
  CharSet          = SET OF CHAR;
  DIGITSTRING      = ARRAY [0..DIGMAX-1 +1 +1] OF CHAR;
                               (* DIGMAX digits in a string          *)
                               (* +1 for end of string               *)
                               (* +1 in case rounding adds           *)
  PDIGITSTRING     = POINTER TO DIGITSTRING;
  LongRealLayout   = RECORD
    CASE             : SHORTCARD OF
    | 1              :
      LongRealByte   : ARRAY [0..7] OF SHORTCARD;
    | 2              :
      LongRealWord   : ARRAY [0..3] OF CARDINAL;
    | 4              :
      LongRealDWord  : ARRAY [0..1] OF LONGCARD;
    ELSE
      LongRealVal    : LONGREAL;
    END;             END;
  ShortRealLayout  = RECORD
    CASE             : SHORTCARD OF
    | 1              :
      ShortRealByte  : ARRAY [0..3] OF SHORTCARD;
    | 2              :
      ShortRealWord  : ARRAY [0..1] OF CARDINAL;
    | 4              :
      ShortRealDWord : ARRAY [0..0] OF LONGCARD;
    ELSE
      ShortRealVal   : SHORTREAL;
    END;             END;


PROCEDURE IsLongNAN( r : LONGREAL ) : BOOLEAN;
VAR
  v : POINTER TO LongRealLayout;
BEGIN
  v := ADR( r );
  IF (v^.LongRealDWord[1] AND 7FFFFFFFH) <= INFINITY[1] THEN
    RETURN FALSE;
  END;
  RETURN TRUE;
END IsLongNAN;


PROCEDURE IsShortNAN( r : SHORTREAL ) : BOOLEAN;
VAR
  v : POINTER TO ShortRealLayout;
BEGIN
  v := ADR( r );
  RETURN (v^.ShortRealDWord[0] AND 7FFFFFFFH) > ShortINFINITY;
END IsShortNAN;


PROCEDURE IsLongINFINITY( r : LONGREAL ) : BOOLEAN;
VAR
  v : POINTER TO LongRealLayout;
BEGIN
  v := ADR( r );
  IF (v^.LongRealDWord[1] AND 7FFFFFFFH) = INFINITY[1] THEN
    IF v^.LongRealDWord[0] = INFINITY[0] THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END IsLongINFINITY;


PROCEDURE IsShortINFINITY( r : SHORTREAL ) : BOOLEAN;
VAR
  v : POINTER TO ShortRealLayout;
BEGIN
  v := ADR( r );
  RETURN (v^.ShortRealDWord[0] AND 7FFFFFFFH) = ShortINFINITY;
END IsShortINFINITY;


PROCEDURE IsLongINDEFINITE( r : LONGREAL ) : BOOLEAN;
VAR
  v : POINTER TO LongRealLayout;
BEGIN
  v := ADR( r );
  IF v^.LongRealDWord[1] = INDEFINITE[1] THEN
    IF v^.LongRealDWord[0] = INDEFINITE[0] THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END IsLongINDEFINITE;


PROCEDURE IsShortINDEFINITE( r : SHORTREAL ) : BOOLEAN;
VAR
  v : POINTER TO ShortRealLayout;
BEGIN
  v := ADR( r );
  RETURN v^.ShortRealDWord[0] = ShortINDEFINITE;
END IsShortINDEFINITE;


PROCEDURE floatcvt
( long        : BOOLEAN;
  cnvflag     : BOOLEAN;
  r           : LONGREAL;
  digits      : INTEGER;
  VAR decpt   : INTEGER;
  VAR sign    : BOOLEAN;
  VAR result  : DIGITSTRING
)             : BOOLEAN;
VAR
  i           : INTEGER;
  pow         : INTEGER;
  digstr      : DIGITSTRING;
  p           : INTEGER;
  n           : INTEGER;
  digprec     : INTEGER;
BEGIN
(*
   Convert a long real value to a string of decimal digits.
   Input:
     long    = up to (15 if TRUE; 6 if FALSE) digits are significand
     cnvflag = how to interpret wanted # of digits
     r       = long real value to be converted
     digits  = if cnvflag = FALSE:
                 # of digits in resulting string past the decimal point
               if cnvflag = TRUE:
                 # of digits in resulting string
   Returns:
     decpt  = position of decimal point from left of first digit
     sign   = TRUE if value was negative
     result = string with digits
   BUGS:
     This routine will hang if it is passed a NAN or INFINITY.
*)
  IF r < 0.0 THEN
    sign := TRUE;
    r := -r;
  ELSE
    sign := FALSE;
  END;
  IF digits < 0 THEN
    digits := 0;
  ELSIF digits >= DIGMAX THEN
    digits := DIGMAX;
  END;
  IF r = 0.0 THEN
    result[ 0 ] := 0C;
    Strings.Fill( 0, HIGH( result ), '0', result );
    decpt := 0;
    RETURN TRUE;
  END;

  (* Adjust things so that 1 <= r < 10 *)
  decpt := 1;
  pow := 256;
  i := 0;
  WHILE r < 1. DO
    WHILE r < negtab[ i+1 ] DO
      r := r * postab[ i ];
      decpt := decpt - pow;
    END;
    pow := pow DIV 2;
    i := i + 1;
  END;
  pow := 256;
  i := 0;
  WHILE r >= 10. DO
    WHILE r >= postab[ i ] DO
      r := r * negtab[ i ];
      decpt := decpt + pow;
    END;
    pow := pow DIV 2;
    i := i + 1;
  END;

  IF cnvflag THEN
    digits := digits + decpt;
    IF digits < 0 THEN
      digits := 0;
    ELSIF digits >= DIGMAX THEN
      digits := DIGMAX;
    END;
  END;

  (* Pick off digits 1 by 1 and stuff into digstr[] *)
  (* Do 1 extra digit for rounding purposes         *)
  IF long THEN
    digprec := DBLDIG+1;
  ELSE
    digprec := SGLDIG+1;
  END;
  FOR p := 0 TO digits BY 1 DO
    digstr[p] := 0C;
    IF p >= digprec THEN
      (* only 'digprec' digits are significand *)
      digstr[ p ] := '0';
    ELSE
      (* store digit before getting next one *)
      n := TRUNC( r );
      digstr[ p ] := CHR( n + VAL( INTEGER, ORD( '0' ) ) );
      r := (r - LONGFLOAT( n )) * 10.;
    END;
  END;

  IF digits < digprec THEN
    p := digits;
  ELSE
    p := digprec-1;
  (*p := digprec;*)
  END;
  IF digstr[ p ] >= '5' THEN
    (* we need to round *)
    LOOP
      IF p = 0 THEN (* if at start *)
        IF cnvflag THEN
          digits := digits + 1;
        END;
        decpt := decpt + 1; (* shift dec pnt *)
        digstr[ 0 ] := '1'; (* "100000..."   *)
        EXIT;
      END;
      digstr[ p ] := '0';
      p := p - 1;
      IF digstr[ p ] <> '9' THEN
        INC( digstr[ p ] );
        EXIT;;
      END;
    END;
  END;

  digstr[ digits ] := 0C;  (* terminate string *)
  Strings.Assign( digstr, result );
  RETURN TRUE;
END floatcvt;


PROCEDURE IsValid( r : LONGREAL; VAR s : ARRAY OF CHAR ) : BOOLEAN;
BEGIN
  IF IsLongNAN( r ) THEN
    (* it's not a valid number *)
    IF HIGH( s ) >= 3 THEN
      s[0] := 0C;
      Strings.Append( "NaN", s );
      Strings.Fill( 0, HIGH(s)-3, ' ', s );
    ELSE
      s[ 0 ] := 0C;
    END;
    RETURN FALSE;
  END;
  IF IsLongINFINITY( r ) THEN
    IF HIGH( s ) >= 2 THEN
      IF r >= 0.0 THEN
        s[0] := '+';
      ELSE
        s[0] := '-';
      END;
      s[1] := 'ì';
      s[2] := 0C;
      Strings.Fill( 0, HIGH(s)-2, ' ', s );
    ELSE
      s[0] := 0C;
    END;
    RETURN FALSE;
  END;
  RETURN TRUE;
END IsValid;


PROCEDURE FloatToFixPt
(
  long    : BOOLEAN;
  r       : LONGREAL;
  size    : INTEGER;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
VAR
  i       : INTEGER;
  p       : INTEGER;
  decpt   : INTEGER;
  sign    : BOOLEAN;
  buf     : DIGITSTRING;
  j       : INTEGER;
  ch      : CHAR;
  done    : BOOLEAN;
BEGIN
  (*
     Convert a long real value into a fixed floating point format:
     ["-"] digit {digit} "." {digit}
  *)

  IF NOT IsValid( r, s ) THEN
    (* it's not a valid number or it's an infinity *)
    RETURN FALSE;
  END;

  (* make sure 'size' and 'digits' are in proper ranges *)
  IF r < 0. THEN
    i := 1;
  ELSE
    i := 0;
  END;
  IF size > VAL( INTEGER, HIGH( s ) ) THEN
    (* size does not fit into string *)
    s[0] := 0C;
    RETURN FALSE;
  END;
  IF size < i + 2 THEN
    (* at least ["-"] digit "." needed for result string *)
    s[0] := 0C;
    RETURN FALSE;
  END;
  IF digits > DIGMAX THEN
    digits := DIGMAX;
  ELSIF digits < 0 THEN
    RETURN FALSE;
  END;
  IF size - i - 2 < digits THEN
    (* too many digits wanted after dec. point *)
    s[0] := 0C;
    RETURN FALSE;
  END;
  s[ size ] := 0C;

  (* store significand digits into a buffer *)
  done := floatcvt( long, TRUE, r, digits, decpt, sign, buf );

  p := 0;
  i := 0;
  IF i >= size THEN
    RETURN FALSE;
  END;
  IF sign THEN
    s[i] := '-';
    INC( i );
  END;
  IF i >= size THEN
    RETURN FALSE;
  END;

  IF decpt <= 0 THEN
    (* at least 1 digit before dec point *)
    s[i] := '0';
    INC( i );
  END;
  IF i >= size THEN
    RETURN FALSE;
  END;

  (* store all digits before dec. point into result string *)
  ch := buf[ p ];
  WHILE (decpt > 0) AND (i < size) AND (ch <> 0C) DO
    s[i] := buf[p];
    INC( i );
    INC( p );
    DEC( decpt );
    ch := buf[p];
  END;
  IF decpt > 0 THEN
    RETURN FALSE;
  END;

  (* store the dec. point itself into result string *)
  s[i] := '.';
  INC( i );
  IF i >= size THEN
    RETURN FALSE;
  END;

  (* now do the digits after the dec. point ... *)
  WHILE (decpt < 0) AND (digits > 0) AND (i < size) DO
    s[i] := '0';
    INC( i );
    INC( decpt );
    DEC( digits );
  END;
  ch := buf[p];
  WHILE (digits > 0) AND (i < size) AND (ch <> 0C) DO
    s[i] := ch;
    DEC( digits );
    INC( p );
    INC( i );
    ch := buf[p];
  END;
  WHILE (digits > 0) AND (i < size) DO
    s[i] := '0';
    DEC( digits );
    INC( i );
  END;
  IF digits > 0 THEN
    s[i] := 0C;
    RETURN FALSE;
  END;

  (* finally insert leading blanks as needed *)
  s[i] := 0C;
  IF i < size THEN
    j := size - i;
    Strings.Fill( 0, j, ' ', s );
  END;
  RETURN done;
END FloatToFixPt;


PROCEDURE FloatToStr
(
  long    : BOOLEAN;
  r       : LONGREAL;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
VAR
  i       : LONGCARD;
  p       : LONGCARD;
  decpt   : INTEGER;
  sign    : BOOLEAN;
  buf     : DIGITSTRING;
  ch      : CHAR;
  done    : BOOLEAN;
BEGIN
  IF digits >= 0 THEN
    (* Convert it into a fixed point format *)
    RETURN FloatToFixPt( long, r, SHORT( HIGH( s ) ), digits, s );
  ELSE
    digits := -digits;
  END;

  (* Convert it into a floating point format including exponent *)
  s[ HIGH(s) ] := 0C;
  IF NOT IsValid( r, s ) THEN
    (* it's not a valid number or it's an infinity *)
    RETURN FALSE;
  END;

  (* store significand digits into 'buf' *)
  done := floatcvt( long, FALSE, r, digits + 1, decpt, sign, buf );

  (* do the significand part: [-] digit . {digit} *)
  p := 0;
  i := 0;
  IF i >= HIGH( s ) THEN
    RETURN FALSE;
  END;
  IF sign THEN
    s[i] := '-';
    INC( i );
  END;
  IF i >= HIGH( s ) THEN
    RETURN FALSE;
  END;
  IF digits > 0 THEN
    s[i] := buf[p];
    INC( p );
  ELSE
    s[i] := '0';
  END;
  INC( i );
  IF i >= HIGH( s ) THEN
    RETURN FALSE;
  END;
  s[i] := '.';
  INC( i );
  IF i >= HIGH( s ) THEN
    RETURN FALSE;
  END;
  ch := buf[p];
  WHILE (digits > 0) AND (i < HIGH( s )) AND (ch > 0C) DO
    s[i] := ch;
    INC( i );
    INC( p );
    ch := buf[p];
    DEC( digits );
  END;
  IF digits > 0 THEN
    RETURN FALSE;
  END;

  (* now do the exponent part: E (+|-) digit digit digit *)
  IF r <> 0.0 THEN  (* avoid 0.00e-01 *)
    DEC( decpt );
  END;
  IF i+5 > HIGH( s ) THEN
    s[i] := 0C;
    RETURN FALSE;
  END;
  s[i] := 'E';
  INC( i );
  IF decpt < 0 THEN
    s[i] := '-';
    decpt := -decpt;
  ELSE
    s[i] := '+';
  END;
  INC( i );
  s[i] := CHR( decpt DIV 100 + VAL( INTEGER, ORD( '0' ) ) );
  INC( i );
  decpt := decpt MOD 100;
  s[i] := CHR( decpt DIV 10 + VAL( INTEGER, ORD( '0' ) ) );
  INC( i );
  decpt := decpt MOD 10;
  s[i] := CHR( decpt + VAL( INTEGER, ORD( '0' ) ) );
  INC( i );
  s[i] := 0C;
  RETURN done;
END FloatToStr;


PROCEDURE RealToStr
( r       : REAL;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  RETURN FloatToStr( IsLongReal, LONG( r ), digits, s );
END RealToStr;


PROCEDURE LongRealToStr
( r       : LONGREAL;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  RETURN FloatToStr( TRUE, r, digits, s );
END LongRealToStr;


PROCEDURE ShortRealToStr
( r       : SHORTREAL;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  RETURN FloatToStr( FALSE, LONG( r ), digits, s );
END ShortRealToStr;


PROCEDURE RealToFixPt
( r       : REAL;
  size    : INTEGER;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  RETURN FloatToFixPt( IsLongReal, LONG( r ), size, digits, s );
END RealToFixPt;


PROCEDURE LongRealToFixPt
( r       : LONGREAL;
  size    : INTEGER;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  RETURN FloatToFixPt( TRUE, r, size, digits, s );
END LongRealToFixPt;


PROCEDURE ShortRealToFixPt
( r       : SHORTREAL;
  size    : INTEGER;
  digits  : INTEGER;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  RETURN FloatToFixPt( FALSE, LONG( r ), size, digits, s );
END ShortRealToFixPt;


PROCEDURE StrToReal
( s       : ARRAY OF CHAR;
  VAR r   : REAL
)         : BOOLEAN;
VAR
  lval    : LONGREAL;
  sval    : SHORTREAL;
  ok      : BOOLEAN;
BEGIN
  (*
     Depending on the -R4 or -R8 compiler switch, the type size
     of REAL may be equal to that of SHORTREAL or LONGREAL.
     This procedure converts a string to real for both cases.
  *)
  IF IsLongReal THEN
    ok := StrToLongReal( s, lval );
    SYSTEM.MemCpy( ADR( r ), ADR( lval ), TSIZE( LONGREAL ) );
  ELSE
    ok := StrToShortReal( s, sval );
    SYSTEM.MemCpy( ADR( r ), ADR( sval ), TSIZE( SHORTREAL ) );
  END;
  RETURN ok;
END StrToReal;


PROCEDURE StrToLongReal
( s       : ARRAY OF CHAR;
  VAR r   : LONGREAL
)         : BOOLEAN;
VAR
  p       : LONGCARD;
  dval    : LONGREAL;
  exp     : INTEGER;
  msdec   : LONGCARD;
  lsdec   : LONGCARD;
  msscale : LONGCARD;
  dot     : BOOLEAN;
  sign    : BOOLEAN;
  subject : BOOLEAN;
  pow     : INTEGER;
  ch      : CHAR;
  sexp    : BOOLEAN;
  e       : INTEGER;
  u       : CARDINAL;
CONST
  MAX_EXP = 1024;
  HT      = CHR( 9  );
  LF      = CHR( 10 );
  VT      = CHR( 11 );
  FF      = CHR( 12 );
  CR      = CHR( 13 );
BEGIN
  s[ Strings.Length( s ) ] := 0C;
  p := 0;
  WHILE s[p] IN CharSet{ HT, LF, VT, FF, CR, ' ' } DO
    INC( p );
  END;
  sign := FALSE;              (* indicating + *)
  IF s[p] = '-' THEN
    sign := TRUE;
    INC( p );
  ELSIF s[p] = '+' THEN
    INC( p );
  END;
  subject := FALSE;
  dval := 0.0;
  dot := FALSE;               (* if decimal point has been seen *)
  exp := 0;
  msdec := 0;
  lsdec := 0;
  msscale := 1;
  LOOP
    ch := s[p];
    WHILE (('0' <= ch) AND (ch <= '9')) DO
      subject := TRUE; (* must have at least 1 digit *)
      IF msdec < (LONGCARD(MAX(LONGINT))-10) DIV 10 THEN
        msdec := msdec * 10 + VAL( LONGCARD, ORD( ch ) - ORD( '0' ) );
      ELSIF msscale < (LONGCARD(MAX(LONGINT))-10) DIV 10 THEN
        lsdec := lsdec * 10 + VAL( LONGCARD, ORD( ch ) - ORD( '0' ) );
        msscale := msscale * 10;
      ELSE
        INC( exp );
      END;
      IF dot THEN
        DEC( exp );
      END;
      INC( p );
      ch := s[p];
    END;
    IF (ch = '.') AND (NOT dot) THEN
      INC( p );
      dot := TRUE;
    ELSE
      EXIT;
    END;
  END;
  IF NOT subject THEN    (* if error (no digits seen) *)
    r := 0.0;            (* return 0.0 *)
    RETURN FALSE;
  END;
  IF s[p] = 'E' THEN
    sexp := FALSE;
    INC( p );
    IF s[p] = '-' THEN
      sexp := TRUE;
      INC ( p );
    ELSIF s[p] = '+' THEN
      INC( p );
    END;
    subject := FALSE;
    e := 0;
    ch := s[p];
    WHILE ('0' <= ch) AND (ch <= '9') DO
      IF e < MAX_EXP*2 THEN  (* prevent integer overflow *)
        e := e * 10 + VAL( INTEGER, ORD( ch ) - ORD( '0' ) );
      END;
      INC( p );
      subject := TRUE;
      ch := s[p];
    END;
    IF sexp THEN
      exp := exp - e;
    ELSE
      exp := exp + e;
    END;
    IF NOT subject THEN   (* if no digits in exponent *)
      r := 0.0;           (* return 0.0 *)
      RETURN FALSE;
    END;
  END;
  dval := FLOAT( VAL( LONGINT, msdec ) );
  IF msscale <> 1 THEN   (* if stuff was accumulated in lsdec *)
    dval := dval * FLOAT( VAL( LONGINT, msscale ) );
    dval := dval + FLOAT( VAL( LONGINT, lsdec ) );
  END;
  IF dval <> 0.0 THEN
    u := 0;
    pow := 256;
    WHILE exp > 0 DO
      WHILE exp >= pow DO
        dval := dval * postab[ u ];
        exp := exp - pow;
      END;
      pow := pow DIV 2;
      INC( u );
    END;
    WHILE exp < 0 DO
      WHILE exp <= -pow DO
        dval := dval * negtab[ u ];
        IF dval = 0.0 THEN
          r := dval;
          RETURN FALSE;
        END;
        exp := exp + pow;
      END;
      pow := pow DIV 2;
      INC( u );
    END;
    IF dval = LONGREAL( INFINITY ) THEN
      (* overflow occurred *)
      r := dval;
      RETURN FALSE;
    END;
  END;
  IF sign THEN
    r := -dval;
  ELSE
    r := dval;
  END;
  RETURN NOT IsLongNAN( dval ) AND NOT IsLongINFINITY( dval );
END StrToLongReal;


PROCEDURE StrToShortReal
( s       : ARRAY OF CHAR;
  VAR r   : SHORTREAL
)         : BOOLEAN;
VAR
  dval    : LONGREAL;
  ok      : BOOLEAN;
BEGIN
  ok := StrToLongReal( s, dval );
  r := SHORT( dval );
  RETURN ok AND NOT IsShortNAN( r ) AND NOT IsShortINFINITY( r );
END StrToShortReal;


PROCEDURE RealToOctStr
(
  r       : REAL;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  IF IsLongReal THEN
    RETURN LongRealToOctStr( LONG( r ), s );
  ELSE
    RETURN ShortRealToOctStr( SHORT( r ), s );
  END;
END RealToOctStr;


PROCEDURE LongRealToOctStr
(
  r         : LONGREAL;
  VAR s     : ARRAY OF CHAR
)           : BOOLEAN;
CONST
  OctDigits = (TSIZE( LONGCARD ) * 8) DIV 3 + 1;
VAR
  v         : LongRealLayout;
  s0        : ARRAY [0..OctDigits+1] OF CHAR;
  s1        : ARRAY [0..OctDigits+1] OF CHAR;
  len0      : LONGINT;
  len1      : LONGINT;
BEGIN
  s[0] := 0C;
  v.LongRealVal := r;
  IF Conversions.LongOctToStr( v.LongRealDWord[0], s0 ) THEN
    IF Conversions.LongOctToStr( v.LongRealDWord[1], s1 ) THEN
      len0 := Strings.Length( s0 );
      len1 := Strings.Length( s1 );
      IF s1[ len1-1 ] = 'B' THEN
        DEC( len1 );
        s1[ len1 ] := 0C;
        IF len1 < OctDigits THEN
          Strings.Fill( 0, OctDigits-len1, '0', s );
          len1 := OctDigits;
        END;
        Strings.Append( s1, s );
        IF len0 < OctDigits+1 THEN
          Strings.Fill( Strings.Length( s ), OctDigits+1-len0, '0', s );
          len0 := OctDigits+1;
        END;
        Strings.Append( s0, s );
        RETURN len0 + len1 = VAL( LONGINT, Strings.Length( s ) );
      END;
    END;
  END;
  RETURN FALSE;
END LongRealToOctStr;


PROCEDURE ShortRealToOctStr
(
  r         : SHORTREAL;
  VAR s     : ARRAY OF CHAR
)           : BOOLEAN;
CONST
  OctDigits = (TSIZE( LONGCARD ) * 8) DIV 3 + 1;
VAR
  v         : ShortRealLayout;
  s0        : ARRAY [0..OctDigits+1] OF CHAR;
  len0      : LONGINT;
BEGIN
  s[0] := 0C;
  v.ShortRealVal := r;
  IF Conversions.LongOctToStr( v.ShortRealDWord[0], s0 ) THEN
    len0 := Strings.Length( s0 );
    IF len0 < OctDigits+1 THEN
      Strings.Fill( 0, OctDigits+1-len0, '0', s );
    END;
    Strings.Append( s0, s );
    RETURN Strings.Length( s ) = OctDigits+1;
  END;
  RETURN FALSE;
END ShortRealToOctStr;


PROCEDURE RealToHexStr
(
  r       : REAL;
  VAR s   : ARRAY OF CHAR
)         : BOOLEAN;
BEGIN
  IF IsLongReal THEN
    RETURN LongRealToHexStr( LONG( r ), s );
  ELSE
    RETURN ShortRealToHexStr( SHORT( r ), s );
  END;
END RealToHexStr;


PROCEDURE LongRealToHexStr
(
  r         : LONGREAL;
  VAR s     : ARRAY OF CHAR
)           : BOOLEAN;
CONST
  HexDigits = TSIZE( LONGCARD ) * 2;
VAR
  s0        : ARRAY [0..HexDigits+2] OF CHAR;
  s1        : ARRAY [0..HexDigits+2] OF CHAR;
  v         : LongRealLayout;
  len1      : LONGINT;
  len0      : LONGINT;
BEGIN
  s[0] := 0C;
  v.LongRealVal := r;
  IF Conversions.LongHexToStr( v.LongRealDWord[0], s0 ) THEN
    IF Conversions.LongHexToStr( v.LongRealDWord[1], s1 ) THEN
      len1 := Strings.Length( s1 );
      len0 := Strings.Length( s0 );
      IF s1[ len1-1 ] = 'H' THEN
        DEC( len1 );
        s1[ len1 ] := 0C;
        IF len1 < HexDigits THEN
          Strings.Fill( 0, HexDigits-len1, '0', s );
          len1 := HexDigits;
        END;
        Strings.Append( s1, s );
        IF len0 < HexDigits+1 THEN
          Strings.Fill( Strings.Length( s ), HexDigits+1-len0, '0', s );
        ELSIF len0 > HexDigits+1 THEN
          Strings.Delete( s0, 0, len0-HexDigits-1 );
        END;
        len0 := HexDigits+1;
        Strings.Append( s0, s );
        RETURN len1 + len0 = VAL( LONGINT, Strings.Length( s ) );
      END;
    END;
  END;
  RETURN FALSE;
END LongRealToHexStr;


PROCEDURE ShortRealToHexStr
(
  r         : SHORTREAL;
  VAR s     : ARRAY OF CHAR
)           : BOOLEAN;
CONST
  HexDigits = TSIZE( LONGCARD ) * 2;
VAR
  v         : ShortRealLayout;
  s0        : ARRAY [0..HexDigits+2] OF CHAR;
  len0      : LONGINT;
BEGIN
  s[0] := 0C;
  v.ShortRealVal := r;
  IF Conversions.LongHexToStr( v.ShortRealDWord[0], s0 ) THEN
    len0 := Strings.Length( s0 );
    IF len0 < HexDigits+1 THEN
      Strings.Fill( 0, HexDigits+1-len0, '0', s );
      len0 := HexDigits+1;
    END;
    Strings.Append( s0, s );
    RETURN VAL( LONGINT, Strings.Length( s ) ) = len0;
  END;
  RETURN FALSE;
END ShortRealToHexStr;


END RealConversions.
