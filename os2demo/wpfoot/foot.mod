IMPLEMENTATION MODULE FOOT;

(************************************************************************
  OS/2 2.x/3.0 implementation of Workplace Shell sample class 'Foot'.

  Copyright (c) 1995 by Juergen Neuhoff

  Entry Points:

     Class Methods:

        <none>

     Overridden Class Methods:

        <none>

     Instance Methods:

        <none>

     Overridden Instance Methods:

        Foot.wpOpen

     Non-Method Functions:


  How to compile:

     To create a class DLL run the following commands:

       MOD FOOT -o -m      <-- This Modula-2 compiler
       LINK386 @FOOT.RSP   <-- 32-bit OS/2 linker

*************************************************************************)

(*$XL+       Modula-2 extensions: '_' in symbol names, OOP facilities   *)
(*$XF+       Relaxed function designators                               *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

(*$LINK
  LIBRARY Foot INITINSTANCE
  PROTMODE
  DATA MULTIPLE NONSHARED LOADONCALL
*)



(*************************************************************************
   Common IMPORTs for a SOM-class implementation.
**************************************************************************)

IMPORT SOM;             (* basic SOM module, always needed *)
IMPORT WPABS;           (* module with parent class, always needed *)
IMPORT Conversions;     (* data conversion support *)
FROM   SOMMISC IMPORT somDebug;        (* debugging aid *)
FROM   SOMMISC IMPORT somWriteString;  (* debugging aid *)
FROM   SOMMISC IMPORT somWriteLn;      (* debugging aid *)
FROM   SYSTEM  IMPORT currentFile;     (* debugging aid *)
FROM   SYSTEM  IMPORT currentLine;     (* debugging aid *)
FROM   SYSTEM  IMPORT ADR;


(*************************************************************************
   This is Modula's equivalent for the language-neutral SOM-emitter's
   'passthru lines before/after'.

   It consists of further individual IMPORTs, to be followed by private
   types, constants, variables and/or procedures for the implementation,
**************************************************************************)

IMPORT OS2DEF;


(*************************************************************************
   Implementation header for the new SOM class 'Foot'
   (constants, types and variables)
**************************************************************************)

CONST
  Foot_MaxNoMethods = 0;         (* number of new methods *)
  FootDebug         = TRUE;      (* enable/disable method debugging *)

(*
 * Temporary class data structure used only in class creation
 *)
VAR
  FoottempClassData : SOM.somClassDataStructure;

(*
 * Internal instance data fields
 *)
TYPE
  FootData         = RECORD
                     END;
  PFootData        = POINTER TO FootData;


(*
 *   FootGetData function, gives access to the instance data, if any
 *)
PROCEDURE FootGetData( Self : PFoot ) : PFootData;
BEGIN
  RETURN SOM.somDataResolve( Self, FootCClassData.instanceDataToken );
END FootGetData;

(*
 *  SOM specific identifiers for all
 *  the new and also the overridden methods
 *)
VAR
  somId_wpOpen                   : SOM.somId;



(*************************************************************************
  apply- and redispatch- stubs for new methods introduced by class 'Foot'
*************************************************************************)


(* none for this sample class ... *)


(*************************************************************************
  Forward declared procedures for all newly introduced private methods
  and for privately overridden methods.
*************************************************************************)

PROCEDURE( Self : PFoot ) wpOpen
(
  hwndCnr       : OS2DEF.HWND;
  ulView        : LONGCARD;
  param         : LONGCARD
)               : OS2DEF.HWND;
FORWARD;


(*************************************************************************
    SOM-class creation procedures.
    Only the FootNewClass() procedure is publicly
    available for client programs.
**************************************************************************)

(*
 * class initialization
 *)
PROCEDURE FootsomInitializeClass;
VAR
  m  : Foot;     (* needed for static method references *)
  c  : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := FoottempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add the new methods, including apply and redispatch stubs,
     to the new SOM class
  *)
  (* none for this sample class ... *)

  (* Override inherited methods, if any *)
  c^.somOverrideSMethod( somId_wpOpen, m.wpOpen );

END FootsomInitializeClass;

(*
 *  class creation procedure
 *)
PROCEDURE FootsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  FoottempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "Foot",
    pClsObj,
    SIZE( FootData ),
    Foot_MaxNoMethods,
    Foot_MajorVersion,
    Foot_MinorVersion
  );
  FootCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  FootsomInitializeClass();
  FootCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( FootClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  FootClassData.classObject := classObject;
END FootsomCreateClass;

(*
 *   public NewClass-procedure
 *)
PROCEDURE FootNewClass
(
  majorVersion  : SOM.INTEGER4;
  minorVersion  : SOM.INTEGER4
)               : SOM.PSOMClass;
VAR
  pClsObj       : SOM.PSOMClass;
  mClsObj       : SOM.PSOMClass;
  line          : LONGCARD;
  b             : BOOLEAN;
BEGIN
  (* Check the version numbers *)
  IF ((majorVersion <> 0) AND (majorVersion <> Foot_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > Foot_MinorVersion)) THEN
    somWriteString( "FootNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF FootClassData.classObject <> NIL THEN
    RETURN FootClassData.classObject;
  END;

  (* Make sure the environment is initialized. *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
    IF SOM.SOMClassMgrObject = NIL THEN
      b := Conversions.StrToLongCard( currentLine(), line );
      SOM.SOMError( SOM.SOMERROR_CouldNotStartup, currentFile(), line );
    END;
    (* SOMClassMgrObject initialized... *)
  END;

  (* Get the parent class object. *)
  pClsObj := WPABS.WPAbstractNewClass( 0, 0 ); (* static *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "WPAbstract" ), 0, 0 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Get the metaclass object. *)
  (* using parent's metaclass: *)
  mClsObj := pClsObj^.mtab^.classObject;
  IF mClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoMetaClass, currentFile(), line );
  END;

  SOM.somConstructClass
  ( FootsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( FoottempClassData ) );

  RETURN FootClassData.classObject;
END FootNewClass;




(*************************************************************************
  Instance methods (possibly overridden) for class "Foot".
*************************************************************************)



(*
 *
 *   METHOD: wpOpen                                         ( ) PRIVATE
 *                                                          (X) PUBLIC
 *   DESCRIPTION:
 *
 *     Opens the car window.
 *
 *)

PROCEDURE( Self : PFoot ) wpOpen
(
  hwndCnr       : OS2DEF.HWND;
  ulView        : LONGCARD;
  param         : LONGCARD
)               : OS2DEF.HWND;
VAR
(*somThis       : PFootData;*)
BEGIN
(*somThis := FootGetData( Self );*)
  IF FootDebug THEN
    somDebug( "Foot", "wpOpen", currentFile(), currentLine() );
  END;
  RETURN Self^.wpOpen^( hwndCnr, ulView, param ); (* parent call *)
END wpOpen;




BEGIN (* of class module *)
  (* initialize some record fields for class-supporting structures *)
  FootCClassData.parentMtab := NIL;
  FootClassData.classObject := NIL;

  (* find the identifier tokens for all the new or overridden methods *)
  somId_wpOpen                 := SOM.somIdFromString( "wpOpen"                 );
END FOOT.
