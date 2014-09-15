(********************************************************************
  FILOSOOF.MOD    Cooperative multitasking sample program.

                  This program demonstrates how to use
                  Modula-2 coroutines to implement a simple
                  cooperative multitasking application.

                  Compile and link as follows:

                  MOD FILOSOOF -O -B -V
                  LINK386 @FILOSOOF.RSP


  Copyright (c) 1995 by Johan Terryn (CompuServe 100421,3024)
*********************************************************************)

MODULE Filosoof;
FROM   InOut       IMPORT   Write,WriteCard, WriteLn, WriteString;
FROM   Semaphore   IMPORT   Init, PSem, SIGNAL, StartP, VSem;

CONST ESC   = 33C;
      Delay = 100;

PROCEDURE GotoXY (colon, line : CARDINAL);
(* positions the cursor at the given colon and line *)
BEGIN
  Write (ESC); Write ('[');
  WriteCard (line,0);
  Write (';');
  WriteCard (colon,0);
  Write ('H');
END GotoXY;

PROCEDURE ClrScr;
(* the screen is erased and the cursor set in the top left corner *)
BEGIN
  Write (ESC); WriteString ('[2J');
  Write (ESC); WriteString ('[H');
END ClrScr;

CONST MaxAantal = 5;

TYPE Aantal = [1..MaxAantal];

VAR Vorken : ARRAY Aantal OF Aantal;
    Hongerig : ARRAY Aantal OF BOOLEAN;
    Begin, Eeuwigheid, OK, ETEN, DENKEN : SIGNAL;
    Nummer : CARDINAL;
    SigVork, SigFilosoof : ARRAY Aantal OF SIGNAL;
    Plaats : ARRAY Aantal,[1..2] OF CARDINAL;

PROCEDURE ZetVork(vork: Aantal; filosoof :Aantal);

BEGIN
  PSem(SigVork[vork]);
  Vorken[vork] := filosoof;
  VSem(SigVork[vork]);
END ZetVork;

PROCEDURE Vork(vork: Aantal):Aantal;

VAR aantal : Aantal;

BEGIN
  PSem(SigVork[vork]);
  aantal := Vorken[vork];
  VSem(SigVork[vork]);
  RETURN aantal
END Vork;


PROCEDURE KrijgtHonger(filosoof: Aantal; honger : BOOLEAN);

BEGIN
  PSem(SigFilosoof[filosoof]);
  Hongerig[filosoof] := honger;
  VSem(SigFilosoof[filosoof]);
END KrijgtHonger;

PROCEDURE HeeftHonger(filosoof: Aantal): BOOLEAN;

VAR honger : BOOLEAN;

BEGIN
  PSem(SigFilosoof[filosoof]);
  honger := Hongerig[filosoof];
  VSem(SigFilosoof[filosoof]);
  RETURN honger
END HeeftHonger;

PROCEDURE Philosoof;

VAR Ik, RechtsVanMij,LinksVanMij : Aantal;
    Tel ,i : CARDINAL;

BEGIN
  Ik := Nummer;
  KrijgtHonger(Ik,FALSE);
  RechtsVanMij := (Ik MOD MaxAantal) + 1;
  LinksVanMij  := ((Ik + (MaxAantal-2)) MOD MaxAantal) +1;
  ZetVork(Ik,Ik);
  VSem(OK);
  PSem(Begin);
  Tel := 0;
  LOOP
    INC(Tel);
    IF Tel = 100 THEN VSem(Eeuwigheid) END;
    KrijgtHonger(Ik,TRUE);
    WHILE HeeftHonger(LinksVanMij)  OR HeeftHonger(RechtsVanMij) DO
      WHILE  (HeeftHonger(LinksVanMij) AND (Vork(Ik) # Ik))
          OR (HeeftHonger(RechtsVanMij) AND (Vork(RechtsVanMij) # Ik))DO
        KrijgtHonger(Ik,FALSE);
      END (* while *);
      KrijgtHonger(Ik,TRUE);
    END (* while *);
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]);
    WriteString("(<.>) (<.>)");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+1);
    WriteString("     ^     ");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+2);
    WriteString("    ó=ò    ");
    VSem(ETEN);
    FOR  i := 1 TO Delay DO
      GotoXY(0,0);
    END (* for *);
    PSem(ETEN);
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]);
    WriteString("(<ø>) (<ø>)");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+1);
    WriteString(" (   ^   ) ");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+2);
    WriteString("     O     ");
    KrijgtHonger(Ik,FALSE);
    ZetVork(Ik,LinksVanMij);
    ZetVork(RechtsVanMij,RechtsVanMij);
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]);
    WriteString("(ÄÄÄ) (ÄÄÄ)");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+1);
    WriteString(" (   ^   ) ");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+2);
    WriteString("    ÄÄÄ    ");
    VSem(DENKEN);
    FOR  i := 1 TO Delay DO
      GotoXY(0,0);
    END (* for *);
    PSem(DENKEN);
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]);
    WriteString("(<÷>) (<÷>)");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+1);
    WriteString("   ) ^ (   ");
    GotoXY(Plaats[Ik][1],Plaats[Ik][2]+2);
    WriteString("     ÷     ");
  END (* loop *);
END Philosoof;

BEGIN
  Plaats[1][1] := 10; Plaats[1][2] := 5;
  Plaats[2][1] := 50; Plaats[2][2] := 5;
  Plaats[5][1] := 10; Plaats[5][2] := 13;
  Plaats[3][1] := 50; Plaats[3][2] := 13;
  Plaats[4][1] := 30; Plaats[4][2] := 21;
  Init(Begin);
  Init(ETEN);
  Init(DENKEN);
  Init(Eeuwigheid);
  Init(OK);
  ClrScr;
  FOR Nummer := 1 TO MaxAantal DO
    Init(SigVork[Nummer]);
    Init(SigFilosoof[Nummer]);
    VSem(SigVork[Nummer]);
    VSem(SigFilosoof[Nummer]);
  END; (* for*)
  FOR Nummer := 1 TO MaxAantal DO
     StartP(Philosoof,4096);
     PSem(OK);
  END; (* for*)
  FOR Nummer := 1 TO MaxAantal DO
     VSem(Begin);
  END; (* for*)
  PSem(Eeuwigheid);
END Filosoof.
