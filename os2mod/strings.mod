IMPLEMENTATION MODULE Strings;

(**************************************************************************
   OS/2 2.x  Modula-2 utility for string handling.
             10.03.95 13.10 : Bug fixed : Strings.Length()

   Copyright (c) 1993 by Juergen Neuhoff
**************************************************************************)

FROM SYSTEM IMPORT MemCpy, ADR;

PROCEDURE Assign
( VAR Source       : ARRAY OF CHAR;
  VAR Destination  : ARRAY OF CHAR
);
VAR
  i,j              : LONGCARD;
BEGIN
  i := Length( Source );
  j := HIGH( Destination )+1;
  IF i > j THEN
    i := j;
  END;
  MemCpy( ADR( Destination ), ADR( Source ), i );
  IF i <= HIGH( Destination ) THEN
    Destination[ i ] := 0C;
  END;
END Assign;

PROCEDURE Insert
( SubStr           : ARRAY OF CHAR;
  VAR Str          : ARRAY OF CHAR;
  Index            : LONGCARD
);
VAR
  i,j,k            : LONGCARD;
BEGIN
  i := Length( SubStr );
  j := Length( Str );
  IF Index > j THEN
    Concat( Str, SubStr, Str );
  ELSE
    IF i > HIGH( Str ) - Index THEN
      i := HIGH( Str ) - Index;
      MemCpy( ADR( Str[ Index ] ), ADR( SubStr ), i );
      Str[ HIGH( Str ) ] := 0C;
    ELSE (* Index + Length( SubStr ) <= HIGH( Str ) *)
      k := j - Index;
      IF Index+i > HIGH( Str )-k THEN
        k := HIGH( Str ) - (Index + i);
      END;
      MemCpy( ADR( Str[ Index+i ] ), ADR( Str[ Index ] ), k );
      MemCpy( ADR( Str[ Index ] ), ADR( SubStr ), i );
      Str[ Index+i+k ] := 0C;
    END;
  END;
END Insert;

PROCEDURE Append
( SubStr           : ARRAY OF CHAR;
  VAR Str          : ARRAY OF CHAR
);
BEGIN
  Insert( SubStr, Str, Length( Str ) );
END Append;

PROCEDURE Delete
( VAR Str          : ARRAY OF CHAR;
  Index            : LONGCARD;
  Len              : LONGCARD
);
VAR
  i                : LONGCARD;
BEGIN
  i := Length( Str );
  IF Index < i THEN
    IF Index + Len > i THEN
      Len := i - Index;
    END;
    MemCpy( ADR( Str[ Index ] ), ADR( Str[ Index+Len ] ), i - (Index+Len) );
    Str[ i-Len ] := 0C;
  END;
END Delete;

PROCEDURE Pos
( SubStr           : ARRAY OF CHAR;
  Str              : ARRAY OF CHAR
)                  : LONGCARD;
VAR
  i,j,k,n          : LONGCARD;
BEGIN
  i := Length( SubStr );
  j := Length( Str );
  IF i > 0 THEN
    k := 0;
    WHILE k+i <= j DO
      n := k;
      WHILE (n < k+i) AND (Str[n] = SubStr[n-k]) DO
        INC( n );
      END;
      IF n >= k+i THEN
        RETURN k;
      END;
      INC( k );
    END;
  END;
  RETURN HIGH( Str ) + 1;
END Pos;

PROCEDURE Copy
( Str              : ARRAY OF CHAR;
  Index            : LONGCARD;
  Len              : LONGCARD;
  VAR Result       : ARRAY OF CHAR
);
VAR
  i                : LONGCARD;
BEGIN
  i := Length( Str );
  IF Index < i THEN
    IF Index + Len > i THEN
      Len := i - Index;
    END;
    IF Len > HIGH( Result ) THEN
      Len := HIGH( Result );
    END;
    MemCpy( ADR( Result ), ADR( Str[ Index ] ), Len );
    Result[ Len ] := 0C;
  ELSE
    Result[ 0 ] := 0C;
  END;
END Copy;

PROCEDURE Concat
( Str1             : ARRAY OF CHAR;
  Str2             : ARRAY OF CHAR;
  VAR Result       : ARRAY OF CHAR
);
VAR
  i,j,k            : LONGCARD;
BEGIN
  i := Length( Str1 );
  j := Length( Str2 );
  k := HIGH( Result );
  IF i > k THEN
    i := k;
  END;
  MemCpy( ADR( Result ), ADR( Str1 ), i );
  IF j > k-i THEN
    j := k-i;
  END;
  MemCpy( ADR( Result[i] ), ADR( Str2 ), j );
  Result[ i+j ] := 0C;
END Concat;

PROCEDURE Length
( VAR Str          : ARRAY OF CHAR
)                  : LONGCARD;
VAR
  i                : LONGCARD;
BEGIN
  i := 0;
  WHILE (Str[ i ] <> 0C) AND (i <= HIGH( Str )) DO
    INC( i );
  END;
  RETURN i;
END Length;

PROCEDURE Size
( VAR Str          : ARRAY OF CHAR
)                  : LONGCARD;
VAR
  i                : LONGCARD;
BEGIN
  i := 0;
  WHILE Str[ i ] <> 0C DO
    INC( i );
  END;
  RETURN i;
END Size;

PROCEDURE Compare
( Str1             : ARRAY OF CHAR;
  Str2             : ARRAY OF CHAR
)                  : INTEGER;
VAR
  i,j,k            : LONGCARD;
  ch1              : CHAR;
  ch2              : CHAR;
BEGIN
  i := Length( Str1 );
  j := Length( Str2 );
  k := 0;
  WHILE (k<i) AND (k<j) DO
    ch1 := Str1[k];
    ch2 := Str2[k];
    IF ch1 = ch2 THEN
      INC( k );
    ELSIF ch1 < ch2 THEN
      RETURN -1;
    ELSE
      RETURN 1;
    END;
  END;
  IF i=j THEN
    RETURN 0;
  END;
  IF i<j THEN
    RETURN -1;
  END;
  RETURN 1;
END Compare;

PROCEDURE Fill
( Pos              : LONGCARD;
  Size             : LONGCARD;
  Filler           : CHAR;
  VAR Str          : ARRAY OF CHAR
);
VAR
  i                : LONGCARD;
  j                : LONGCARD;
BEGIN
  i := Length( Str );
  IF Pos > i THEN
    RETURN;
  END;
  IF Pos = i THEN
    WHILE (i < HIGH( Str )) AND (Size > 0) DO
      Str[ i ] := Filler;
      INC( i );
      DEC( Size );
    END;
    Str[ i ] := 0C;
    RETURN;
  END;
  IF Pos + Size >= HIGH( Str ) THEN
    Size := HIGH( Str ) - Pos;
    WHILE Size > 0 DO
      Str[ Pos ] := Filler;
      INC( Pos );
      DEC( Size );
    END;
    Str[ Pos ] := 0C;
    RETURN;
  END;
  j := i - Pos;
  IF Pos + Size + j >= HIGH( Str ) THEN
    j := HIGH( Str ) - Pos - Size;
  END;
  i := Pos + Size + j;
  Str[ i ] := 0C;
  WHILE j > 0 DO
    DEC( j );
    DEC( i );
    Str[ i ] := Str[ i-Size ];
  END;
  i := Pos;
  WHILE i < Pos+Size DO
    Str[ i ] := Filler;
    INC( i );
  END;
END Fill;

END Strings.
