IMPLEMENTATION MODULE Storage;

(*************************************************************************
   OS/2 2.x Modula-2 standard utility for local heap management.
            21.01.95 15.49 : Bugs fixed 'ExitOrder', 'ExitStorage()'
            07.06.95 23.51 : InitHeap() changed

   Copyright (c) 1993 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)

FROM SYSTEM     IMPORT ADDRESS;
FROM OS2DEF     IMPORT APIRET;
FROM DOSMEMMGR  IMPORT fPERM, fALLOC, OBJ_TILE, DOSSUB_INIT, DOSSUB_SPARSE_OBJ;
FROM DOSMEMMGR  IMPORT DOSSUB_SERIALIZE;
FROM DOSMEMMGR  IMPORT DosAllocMem, DosFreeMem, DosSubAllocMem, DosSubFreeMem;
FROM DOSMEMMGR  IMPORT DosSubSetMem, DosSubUnsetMem;
FROM DOSPROCESS IMPORT DosExitList, EXLST_ADD, EXLST_EXIT;

CONST
  ExitOrder    = (80H-1)*256; (* 1 level before OS/2 components *)
  PageSize     = 4096;        (* OS/2 2.0 physical memory page size *)
VAR
  Heap         : ADDRESS;
  rc           : APIRET;

PROCEDURE ALLOCATE
( VAR a  : ADDRESS;
  size   : LONGCARD
);
BEGIN
  IF Heap = NIL THEN
    a := NIL;
  ELSIF DosSubAllocMem( Heap, a, size ) <> 0 THEN
    a := NIL;
  END;
END ALLOCATE;

PROCEDURE DEALLOCATE
( VAR a  : ADDRESS;
  size   : LONGCARD
);
BEGIN
  IF Heap <> NIL THEN
    IF DosSubFreeMem( Heap, a, size ) = 0 THEN
      a := NIL;
    END;
  END;
END DEALLOCATE;

PROCEDURE Available
( size   : LONGCARD
)        : BOOLEAN;
VAR
  a      : ADDRESS;
BEGIN
  IF Heap = NIL THEN
    RETURN FALSE;
  END;
  ALLOCATE( a, size );
  IF a <> NIL THEN
    DEALLOCATE( a, size );
    RETURN TRUE;
  END;
  RETURN FALSE;
END Available;

PROCEDURE InitHeap();
VAR
  HeapSize : LONGCARD;
  rc       : APIRET;
BEGIN
  HeapSize := 512 * PageSize;
  WHILE HeapSize > 0 DO
    rc := DosAllocMem( Heap, HeapSize, OBJ_TILE+fPERM );
    IF rc = 0 THEN
      rc := DosSubSetMem
      ( Heap, DOSSUB_INIT+DOSSUB_SPARSE_OBJ+DOSSUB_SERIALIZE, HeapSize );
      IF rc = 0 THEN
        RETURN;
      ELSE
        rc := DosFreeMem( Heap );
      END;
    END;
    DEC( HeapSize, 4 * PageSize );
  END;
  Heap := NIL;
END InitHeap;

PROCEDURE ExitStorage();
VAR
  rc : APIRET;
BEGIN
  IF Heap <> NIL THEN
    rc := DosSubUnsetMem( Heap );
    rc := DosFreeMem( Heap );
    Heap := NIL;
  END;
  rc := DosExitList( EXLST_EXIT, ExitStorage );
END ExitStorage;

BEGIN
  InitHeap();
  rc := DosExitList( EXLST_ADD + ExitOrder, ExitStorage );
END Storage.
