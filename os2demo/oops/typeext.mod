(*$XL+*)
MODULE TypeExt;

IMPORT Terminal, SYSTEM;
FROM   Storage IMPORT ALLOCATE, DEALLOCATE;

TYPE
  RecTyp =  RECORD
    x,y:      INTEGER;
            END;
  Rec0Typ = RECORD( RecTyp )
    z:        LONGINT;
            END;
  Rec1Typ = RECORD( RecTyp )
    ch:       CHAR;
            END;
  Rec20Typ = RECORD( Rec0Typ )
    n:        CARDINAL;
            END;
  RecP     = POINTER TO RecTyp;
  Rec0P    = POINTER TO Rec0Typ;
  Rec1P    = POINTER TO Rec1Typ;
  Rec20P   = POINTER TO Rec20Typ;

VAR
  i,j:       INTEGER;
  RecVar0:   RECORD( RecTyp )
    c        : CHAR;
             END;
  RecVar   : RecTyp;
  Rec0Var  : Rec0Typ;
  Rec1Var  : Rec1Typ;
  Rec20Var : Rec20Typ;
  RecPVar  : RecP;
  Rec20PVar: Rec20P;

PROCEDURE xyz( VAR RecVar:RecTyp );
BEGIN
  IF RecVar IS RecTyp THEN
    Terminal.WriteString( "It's a RecTyp " );
    Terminal.WriteLn();
  END;
  IF RecVar IS Rec0Typ THEN
    Terminal.WriteString( "It's a Rec0Typ( RecTyp ) " );
    Terminal.WriteLn();
  END;
  IF RecVar IS Rec1Typ THEN
    Terminal.WriteString( "It's a Rec1Typ( RecTyp ) " );
    Terminal.WriteLn();
  END;
  IF RecVar IS Rec20Typ THEN
    Terminal.WriteString( "It's a Rec20Typ( Rec0Typ( RecTyp ) ) " );
    Terminal.WriteLn();
    WITH RecVar:Rec20Typ DO
      RecVar.n := 9999H;
    END;
  END;
  Terminal.WriteLn();
  i := 4444H;
END xyz;

BEGIN
  i := 5555H;
  NEW( Rec20PVar );
  Rec20PVar^.x := 1;
  Rec20PVar^.y := 2;
  Rec20PVar^.z := 3;
  Rec20PVar^.n := 0;
  xyz( Rec20PVar^ );
  RecPVar := Rec20PVar;
  IF RecPVar IS RecP THEN
    Terminal.WriteString( "RecPVar IS RecP" );
    Terminal.WriteLn();
  END;
  IF RecPVar IS Rec0P THEN
    Terminal.WriteString( "RecPVar IS Rec0P( RecP )" );
    Terminal.WriteLn();
  END;
  IF RecPVar IS Rec1P THEN
    Terminal.WriteString( "RecPVar IS Rec1P( RecP )" );
    Terminal.WriteLn();
  END;
  IF RecPVar IS Rec20P THEN
    Terminal.WriteString( "RecPVar IS Rec20P( Rec0P( RecP ) )" );
    Terminal.WriteLn();
  END;
  Terminal.WriteLn();
  DISPOSE( Rec20PVar );
  RecVar0.y := 6666H;
  j := 7777H;
  xyz( RecVar );
  xyz( Rec0Var );
  xyz( Rec1Var );
  xyz( Rec20Var );
END TypeExt.
