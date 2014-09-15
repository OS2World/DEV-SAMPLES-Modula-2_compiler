IMPLEMENTATION MODULE Conversions;

(**************************************************************************
   OS/2 2.x  Modula-2 utility for numeric conversions to/from strings.

   Copyright (c) 1993 by Juergen Neuhoff
**************************************************************************)

IMPORT SYSTEM;
IMPORT Strings;

TYPE
  CharSet = SET OF CHAR;

PROCEDURE ShortIntToStr
( i      : SHORTINT;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongIntToStr( i, s );
END ShortIntToStr;

PROCEDURE IntToStr
( i      : INTEGER;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongIntToStr( i, s );
END IntToStr;

PROCEDURE LongIntToStr
( i      : LONGINT;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
VAR
  j,k    : LONGCARD;
  n      : LONGINT;
BEGIN
  k := HIGH( s );
  j := 0;
  IF k = 0 THEN
    RETURN FALSE;
  END;
  IF i = 0 THEN
    s[ 0 ] := '0';
    s[ 1 ] := 0C;
    RETURN TRUE;
  END;
  IF i = MIN( LONGINT ) THEN
    IF k >= 11 THEN
      Strings.Copy( "-2147483648", 0, 11, s );
      RETURN TRUE;
    END;
    RETURN FALSE;
  END;
  IF i < 0 THEN
    s[ 0 ] := '-';
    j := 1;
    i := -i;
  END;
  s[ k ] := 0C;
  WHILE (k > j) AND (i > 0) DO
    DEC( k );
    n := i DIV 10;
    s[ k ] := CHR( ORD( '0' ) + ORD( i - n * 10 ) );
    i := n;
  END;
  IF i > 0 THEN
    RETURN FALSE;
  END;
  IF k > j THEN
    WHILE s[ k ] <> 0C DO
      s[ j ] := s[ k ];
      INC( j );
      INC( k );
    END;
    s[ j ] := 0C;
  END;
  RETURN TRUE;
END LongIntToStr;

PROCEDURE StrToShortInt
( s      : ARRAY OF CHAR;
  VAR i  : SHORTINT
)        : BOOLEAN;
VAR
  j      : LONGINT;
BEGIN
  IF StrToLongInt( s, j ) THEN
    IF j >= MIN( SHORTINT ) THEN
      IF j <= MAX( SHORTINT ) THEN
        i := SHORT( SHORT( j ) );
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END StrToShortInt;

PROCEDURE StrToInt
( s      : ARRAY OF CHAR;
  VAR i  : INTEGER
)        : BOOLEAN;
VAR
  j      : LONGINT;
BEGIN
  IF StrToLongInt( s, j ) THEN
    IF j >= MIN( INTEGER ) THEN
      IF j <= MAX( INTEGER ) THEN
        i := SHORT( j );
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END StrToInt;

PROCEDURE StrToLongInt
( s        : ARRAY OF CHAR;
  VAR i    : LONGINT
)          : BOOLEAN;
VAR
  j,k,m    : LONGCARD;
  neg      : BOOLEAN;
  overflow : BOOLEAN;
BEGIN
  k := Strings.Length( s );
  neg := FALSE;
  overflow := FALSE;
  j := 0;
  i := 0;
  WHILE (j < k) AND (s[ j ] = ' ') DO
    INC( j );
  END;
  IF s[ j ] = '-' THEN
    neg := TRUE;
    INC( j );
  ELSIF s[ j ] = '+' THEN
    INC( j );
  END;
  m := j;
  WHILE (j < k) AND (s[ j ] = '0') DO
    INC( j );
  END;
  WHILE (j < k) AND (s[ j ] IN CharSet{ '0'..'9' }) DO
    i := i * 10 + VAL( LONGINT, ORD( s[ j ] )-ORD( '0' ) );
    IF i < 0 THEN
      IF neg THEN
        IF i > MIN( LONGINT ) THEN
          overflow := TRUE;
        END
      ELSE
        overflow := TRUE;
      END;
    END;
    INC( j );
  END;
  IF neg THEN
    i := -i;
  END;
  RETURN (j > m) AND (j >= k) AND NOT overflow;
END StrToLongInt;

PROCEDURE ShortCardToStr
( c      : SHORTCARD;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongCardToStr( c, s );
END ShortCardToStr;

PROCEDURE CardToStr
( c      : CARDINAL;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongCardToStr( c, s );
END CardToStr;

PROCEDURE LongCardToStr
( c      : LONGCARD;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
VAR
  j,k,n  : LONGCARD;
BEGIN
  k := HIGH( s );
  IF k = 0 THEN
    RETURN FALSE;
  END;
  IF c = 0 THEN
    s[ 0 ] := '0';
    s[ 1 ] := 0C;
    RETURN TRUE;
  END;
  s[ k ] := 0C;
  WHILE (k > 0) AND (c > 0) DO
    DEC( k );
    n := c DIV 10;
    s[ k ] := CHR( ORD( '0' ) + ORD( c - n * 10 ) );
    c := n;
  END;
  IF c > 0 THEN
    RETURN FALSE;
  END;
  IF k > 0 THEN
    j := 0;
    WHILE s[ k ] <> 0C DO
      s[ j ] := s[ k ];
      INC( j );
      INC( k );
    END;
    s[ j ] := 0C;
  END;
  RETURN TRUE;
END LongCardToStr;

PROCEDURE StrToShortCard
( s      : ARRAY OF CHAR;
  VAR c  : SHORTCARD
)        : BOOLEAN;
VAR
  j      : LONGCARD;
BEGIN
  IF StrToLongCard( s, j ) THEN
    IF j <= MAX( SHORTCARD ) THEN
      c := SHORT( SHORT( j ) );
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END StrToShortCard;

PROCEDURE StrToCard
( s      : ARRAY OF CHAR;
  VAR c  : CARDINAL
)        : BOOLEAN;
VAR
  j      : LONGCARD;
BEGIN
  IF StrToLongCard( s, j ) THEN
    IF j <= MAX( CARDINAL ) THEN
      c := SHORT( j );
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END StrToCard;

PROCEDURE StrToLongCard
( s        : ARRAY OF CHAR;
  VAR c    : LONGCARD
)          : BOOLEAN;
VAR
  j,k,m,n  : LONGCARD;
  overflow : BOOLEAN;
BEGIN
  k := Strings.Length( s );
  j := 0;
  c := 0;
  overflow := FALSE;
  WHILE (j < k) AND (s[ j ] = ' ') DO
    INC( j );
  END;
  IF s[ j ] = '+' THEN
    INC( j );
  END;
  m := j;
  WHILE (j < k) AND (s[ j ] = '0') DO
    INC( j );
  END;
  WHILE (j < k) AND (s[ j ] IN CharSet{ '0'..'9' }) DO
    n := c;
    overflow := overflow OR (n > MAX(LONGCARD) DIV 10);
    c := c * 10 + VAL( LONGCARD, ORD( s[ j ] )-ORD( '0' ) );
    overflow := overflow OR (c < n);
    INC( j );
  END;
  RETURN (j >= k) AND (j > m) AND NOT overflow;
END StrToLongCard;

PROCEDURE ShortHexToStr
( h      : SYSTEM.BYTE;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongHexToStr( LONG( LONG( h ) ), s );
END ShortHexToStr;

PROCEDURE HexToStr
( h      : SYSTEM.WORD;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongHexToStr( LONG( h ), s );
END HexToStr;

PROCEDURE LongHexToStr
( h      : SYSTEM.LONGWORD;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
VAR
  j,k,c  : LONGCARD;
  m,n    : LONGCARD;
BEGIN
  c := h;
  k := HIGH( s );
  IF k < 2 THEN
    RETURN FALSE;
  END;
  IF c = 0 THEN
    s[ 0 ] := '0';
    s[ 1 ] := 'H';
    s[ 2 ] := 0C;
    RETURN TRUE;
  END;
  s[ k ] := 0C;
  DEC( k );
  s[ k ] := 'H';
  WHILE (k > 0) AND (c > 0) DO
    DEC( k );
    n := c DIV 16;
    m := ORD( c - n * 16 );
    IF m < 10 THEN
      s[ k ] := CHR( ORD( '0' ) + m );
    ELSE
      s[ k ] := CHR( ORD( 'A' ) + m - 10 );
    END;
    c := n;
  END;
  IF c > 0 THEN
    RETURN FALSE;
  END;
  IF NOT (s[ k ] IN CharSet{ '0'..'9' }) THEN
    IF k > 0 THEN
      DEC( k );
      s[ k ] := '0';
    ELSE
      RETURN FALSE;
    END;
  END;
  IF k > 0 THEN
    j := 0;
    WHILE s[ k ] <> 0C DO
      s[ j ] := s[ k ];
      INC( j );
      INC( k );
    END;
    s[ j ] := 0C;
  END;
  RETURN TRUE;
END LongHexToStr;

PROCEDURE StrToShortHex
( s      : ARRAY OF CHAR;
  VAR h  : SYSTEM.BYTE
)        : BOOLEAN;
VAR
  j      : LONGCARD;
BEGIN
  IF StrToLongHex( s, j ) THEN
    IF j <= MAX( SHORTCARD ) THEN
      h := SHORT( SHORT( j ) );
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END StrToShortHex;

PROCEDURE StrToHex
( s      : ARRAY OF CHAR;
  VAR h  : SYSTEM.WORD
)        : BOOLEAN;
VAR
  j      : LONGCARD;
BEGIN
  IF StrToLongHex( s, j ) THEN
    IF j <= MAX( CARDINAL ) THEN
      h := SHORT( j );
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END StrToHex;

PROCEDURE StrToLongHex
( s          : ARRAY OF CHAR;
  VAR h      : SYSTEM.LONGWORD
)            : BOOLEAN;
VAR
  j,k,c,m,n  : LONGCARD;
  overflow   : BOOLEAN;
BEGIN
  k := Strings.Length( s );
  j := 0;
  c := 0;
  overflow := FALSE;
  WHILE (j < k) AND (s[ j ] = ' ') DO
    INC( j );
  END;
  IF s[ j ] = '+' THEN
    INC( j );
  END;
  IF (j < k) AND NOT (s[ j ] IN CharSet{ '0'..'9' }) THEN
    RETURN FALSE;
  END;
  m := j;
  WHILE (j < k) AND (s[ j ] = '0') DO
    INC( j );
  END;
  WHILE (j < k) AND (s[ j ] IN CharSet{ '0'..'9', 'A'..'F' }) DO
    n := c;
    overflow := overflow OR (c > MAX(LONGCARD) DIV 16);
    c := c * 16;
    IF s[ j ] IN CharSet{ '0'..'9' } THEN
      c := c + ORD( s[ j ] ) - ORD( '0' );
    ELSE
      c := c + ORD( s[ j ] ) - ORD( 'A' ) + 10;
    END;
    overflow := overflow OR (c < n);
    INC( j );
  END;
  h := c;
  IF (j < k) AND (s[ j ] = 'H') THEN
    INC( j );
  ELSE
    RETURN FALSE;
  END;
  IF (j >= k) AND (j > m) AND NOT overflow THEN
    RETURN TRUE;
  END;
  RETURN FALSE;
END StrToLongHex;

PROCEDURE ShortOctToStr
( o      : SYSTEM.BYTE;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongOctToStr( LONG( LONG( o ) ), s );
END ShortOctToStr;

PROCEDURE OctToStr
( o      : SYSTEM.WORD;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
BEGIN
  RETURN LongOctToStr( LONG( o ), s );
END OctToStr;

PROCEDURE LongOctToStr
( o      : SYSTEM.LONGWORD;
  VAR s  : ARRAY OF CHAR
)        : BOOLEAN;
VAR
  j,k,c  : LONGCARD;
  n      : LONGCARD;
BEGIN
  c := o;
  k := HIGH( s );
  IF k < 2 THEN
    RETURN FALSE;
  END;
  IF c = 0 THEN
    s[ 0 ] := '0';
    s[ 1 ] := 'B';
    s[ 2 ] := 0C;
    RETURN TRUE;
  END;
  s[ k ] := 0C;
  DEC( k );
  s[ k ] := 'B';
  WHILE (k > 0) AND (c > 0) DO
    DEC( k );
    n := c DIV 8;
    s[ k ] := CHR( ORD( '0' ) + ORD( c - n * 8 ) );
    c := n;
  END;
  IF c > 0 THEN
    RETURN FALSE;
  END;
  IF k > 0 THEN
    j := 0;
    WHILE s[ k ] <> 0C DO
      s[ j ] := s[ k ];
      INC( j );
      INC( k );
    END;
    s[ j ] := 0C;
  END;
  RETURN TRUE;
END LongOctToStr;

PROCEDURE StrToShortOct
( s      : ARRAY OF CHAR;
  VAR o  : SYSTEM.BYTE
)        : BOOLEAN;
VAR
  j      : LONGCARD;
BEGIN
  IF StrToLongOct( s, j ) THEN
    IF j <= MAX( SHORTCARD ) THEN
      o := SHORT( SHORT( j ) );
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END StrToShortOct;

PROCEDURE StrToOct
( s      : ARRAY OF CHAR;
  VAR o  : SYSTEM.WORD
)        : BOOLEAN;
VAR
  j      : LONGCARD;
BEGIN
  IF StrToLongOct( s, j ) THEN
    IF j <= MAX( CARDINAL ) THEN
      o := SHORT( j );
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END StrToOct;

PROCEDURE StrToLongOct
( s          : ARRAY OF CHAR;
  VAR o      : SYSTEM.LONGWORD
)            : BOOLEAN;
VAR
  j,k,c,m,n  : LONGCARD;
  overflow   : BOOLEAN;
BEGIN
  k := Strings.Length( s );
  j := 0;
  c := 0;
  overflow := FALSE;
  WHILE (j < k) AND (s[ j ] = ' ') DO
    INC( j );
  END;
  IF s[ j ] = '+' THEN
    INC( j );
  END;
  IF (j < k) AND NOT (s[ j ] IN CharSet{ '0'..'7' }) THEN
    RETURN FALSE;
  END;
  m := j;
  WHILE (j < k) AND (s[ j ] = '0') DO
    INC( j );
  END;
  WHILE (j < k) AND (s[ j ] IN CharSet{ '0'..'7' }) DO
    n := c;
    overflow := overflow OR (c > MAX(LONGCARD) DIV 8);
    c := c * 8 + ORD( s[ j ] ) - ORD( '0' );
    overflow := overflow OR (c < n);
    INC( j );
  END;
  o := c;
  IF (j < k) AND (s[ j ] = 'B') THEN
    INC( j );
  ELSE
    RETURN FALSE;
  END;
  IF (j >= k) AND (j > m) AND NOT overflow THEN
    RETURN TRUE;
  END;
  RETURN FALSE;
END StrToLongOct;

END Conversions.
