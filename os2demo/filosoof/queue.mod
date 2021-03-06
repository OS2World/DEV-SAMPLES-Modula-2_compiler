(********************************************************************
  QUEUE.MOD   

  Copyright (c) 1995 by Johan Terryn (CompuServe 100421,3024)
*********************************************************************)

IMPLEMENTATION MODULE Queue;
FROM   InOut     IMPORT   WriteString;
FROM   SYSTEM    IMPORT   ADDRESS;
FROM   Storage   IMPORT   ALLOCATE, DEALLOCATE;



CONST queuesize = 100;
      queuemax  = queuesize+1;

TYPE queuerange = [0..queuesize];
     queue = POINTER TO RECORD
       space : ARRAY queuerange OF ADDRESS;
       front, rear : queuerange;
     END;

PROCEDURE makeempty(VAR q : queue);

BEGIN
  IF q = NIL THEN NEW(q) END;
    q^.front := 0;
    q^.rear  := 0;
END makeempty;

PROCEDURE empty(q:queue):BOOLEAN;
(* Internal calls to this module fail, external calls work *)
BEGIN
  RETURN q^.front = q^.rear
END empty;

PROCEDURE full(q:queue):BOOLEAN;
BEGIN
  RETURN q^.front = (q^.rear+1) MOD queuemax
END full;

PROCEDURE insert(VAR q:queue; item :ADDRESS);
BEGIN
  IF NOT full(q) THEN
    q^.rear := (q^.rear+1) MOD queuemax;
    q^.space[q^.rear] := item
  ELSE
    WriteString("Overflow")
  END
END insert;

PROCEDURE remove(VAR q :queue; VAR item :ADDRESS);

BEGIN
  (* This one fails *)
  IF empty(q) THEN
  (* this one works
  IF q^.front = q^.rear THEN *)
    WriteString("Underflow")
  ELSE
    q^.front :=(q^.front+1) MOD queuemax;
    item := q^.space[q^.front]
  END;
END remove;

PROCEDURE define(VAR q :queue);
BEGIN
  q := NIL
END define;


PROCEDURE destroy(VAR q :queue);
BEGIN
  DISPOSE(q);
  q := NIL;
END destroy;

END Queue. 
