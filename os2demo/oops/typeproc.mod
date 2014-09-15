MODULE TYPEPROC;

(*$XL+*)
(* type bound procedure demo *)


FROM Storage  IMPORT ALLOCATE, DEALLOCATE;
FROM InOut    IMPORT WriteString, WriteLn, WriteLongHex;
FROM SYSTEM   IMPORT ADDRESS;


TYPE
  TREE =  POINTER TO NODE;
  NODE =  RECORD
    key   : INTEGER;
    left  : TREE;
    right : TREE;
          END;

TYPE
  CENTERTREE = POINTER TO CENTERNODE;
  CENTERNODE = RECORD( NODE )
    width      : INTEGER;
    subnode    : TREE;
               END;

VAR
  CenterTree   : CENTERTREE;
  Tree         : TREE;


(* declaring method Insert for TREE *)
PROCEDURE( self : TREE ) Insert( node : TREE );
BEGIN
  InOut.WriteString( "TREE.Insert: called" );
  InOut.WriteLn();
END Insert;

(* overriding method Insert for extended CENTERTREE *)
PROCEDURE( self : CENTERTREE ) Insert( node : TREE );
BEGIN
  InOut.WriteString( "CENTERTREE.Insert: called" );
  InOut.WriteLn();
  (* now calling parent method bound to TREE *)
  self^.Insert^( node );
END Insert;

BEGIN
  WriteString( "TYPEPROC: This is a type-bound procedure demo ..." );
  WriteLn();
  WriteString( "TYPEPROC: Creating new 'CENTERTREE' object" );
  WriteLn();
  NEW( CenterTree );
  Tree := CenterTree;
  WriteString( "TYPEPROC: New 'CENTERTREE' object created = " );
  WriteLongHex( ADDRESS( Tree ), 12 );
  WriteLn();
  Tree^.Insert( CenterTree );
  WriteString( "TYPEPROC: Disposing 'CENTERTREE' object" );
  WriteLn();
  DISPOSE( CenterTree );
END TYPEPROC.
