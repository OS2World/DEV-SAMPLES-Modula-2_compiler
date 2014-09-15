(********************************************************************
  SEMAPHOR.MOD   

  Copyright (c) 1995 by Johan Terryn (CompuServe 100421,3024)
*********************************************************************)

IMPLEMENTATION MODULE Semaphore;
FROM   InOut       IMPORT   WriteLn, WriteString;
FROM   Queue       IMPORT   define, empty, insert, makeempty,
                            queue, remove;
FROM   SYSTEM      IMPORT   ADDRESS, NEWPROCESS, TRANSFER;
FROM   Storage     IMPORT   ALLOCATE;

TYPE SIGNAL = POINTER TO semaphore;
     semaphore = RECORD
       value : CARDINAL;
       procs : queue
     END;
     ProcessId = POINTER TO ADDRESS;


VAR readyqueue : queue;
    activeprocess : CARDINAL;

PROCEDURE Init( VAR s:SIGNAL);

BEGIN
  NEW(s);
  s^.value := 0;
  define(s^.procs);
  makeempty(s^.procs);
END Init;

PROCEDURE VSem( VAR s:SIGNAL);
 VAR prevproc : ProcessId;

  BEGIN
    IF NOT empty(s^.procs) THEN
      insert(readyqueue,currentprocess);
      prevproc := currentprocess;
      remove(s^.procs,currentprocess);
      TRANSFER(prevproc^,currentprocess^)
    ELSE
      INC(s^.value);
      IF NOT empty(readyqueue) THEN
        insert(readyqueue,currentprocess);
        prevproc := currentprocess;
        remove(readyqueue,currentprocess);
        TRANSFER(prevproc^,currentprocess^)
      END
    END
END VSem;

PROCEDURE PSem( VAR s:SIGNAL);

 VAR prevproc : ProcessId;

BEGIN
  IF s^.value > 0 THEN
    DEC(s^.value)
  ELSIF NOT empty(readyqueue) THEN
    insert(s^.procs,currentprocess);
    prevproc := currentprocess;
    remove(readyqueue,currentprocess);
    TRANSFER(prevproc^,currentprocess^)
  ELSE
    WriteString("Deadlock !!!!");
    WriteLn;
  END
END PSem;

PROCEDURE Waiting( VAR s:SIGNAL): BOOLEAN;

BEGIN
  RETURN NOT empty(s^.procs)
END Waiting;

PROCEDURE StartP(p : PROC; wssize : CARDINAL);

VAR workspace : ADDRESS;
    prevproc  : ProcessId;

BEGIN
  ALLOCATE(workspace,wssize);
  INC(activeprocess);
  insert(readyqueue,currentprocess);
  prevproc := currentprocess;
  NEW(currentprocess);
  NEWPROCESS(p,workspace,wssize,currentprocess^);
  TRANSFER(prevproc^,currentprocess^);

END StartP;

PROCEDURE TermP;

VAR prevproc : ProcessId;
BEGIN
  DEC(activeprocess);
  IF (activeprocess=0) AND NOT empty(Idle^.procs) THEN
    remove(Idle^.procs,prevproc);
    insert(readyqueue,prevproc);
  END;
  IF NOT empty(readyqueue) THEN
    prevproc := currentprocess;
    remove(readyqueue,currentprocess);
    TRANSFER(prevproc^,currentprocess^);
  ELSE
    WriteString("Deadlock !!!!");WriteLn;
  END
END TermP;

PROCEDURE equal (p1,p2 : ProcessId):BOOLEAN;


 BEGIN
   RETURN p1= p2
END equal;

BEGIN
  Init(Idle);
  define(readyqueue);
  makeempty(readyqueue);
  Null := NIL;
  NEW(currentprocess);
  activeprocess := 0;
END Semaphore.
