IMPLEMENTATION MODULE ANIMAL;

(*$XL+       Modula-2 extenmsions: '_' in symbol names, OOP facilities  *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

(*$LINK
  LIBRARY ANIMAL INITINSTANCE
  DESCRIPTION 'Animal class DLL, compiled with Modula-2.  (c) Juergen Neuhoff'
  PROTMODE
  DATA MULTIPLE NONSHARED LOADONCALL
*)


IMPORT SOM;
FROM   SOMMISC IMPORT somDebug, somWriteString, somWriteLn;
FROM   SYSTEM  IMPORT ADR;


(*************************************************************************
   Passthru lines "before" if any
**************************************************************************)

IMPORT OS2DEF;
IMPORT Conversions;
IMPORT Strings;
FROM SYSTEM IMPORT currentFile;
FROM SYSTEM IMPORT currentLine;



(*************************************************************************
   Passthru lines "after"  if any
**************************************************************************)




(*************************************************************************
   Implementation header for SOM class Animal
**************************************************************************)

CONST
  Animal_MaxNoMethods = 5;
  AnimalDebug = FALSE;             (* enable/disable method debugging *)



(*
 * Declare the C specific class data structure
 *)
TYPE
  AnimalCClassDataStructure  = RECORD
    parentMtab                 : SOM.somMethodTabs;
    instanceDataToken          : SOM.somDToken;
                               END;
VAR
  AnimalCClassData           : AnimalCClassDataStructure;



(*
 * Temporary class data structure used only in class creation
 *)
VAR
  AnimaltempClassData        : SOM.somClassDataStructure;



(*
 * Internal instance data
 *)
TYPE
  AnimalData          = RECORD
    sound               : OS2DEF.PSZ;
    nsound              : LONGINT;
                        END;
  PAnimalData         = POINTER TO AnimalData;



(*
 *   <class>GetData function
 *)
PROCEDURE AnimalGetData( somSelf : PAnimal ) : PAnimalData;
BEGIN
  RETURN SOM.somDataResolve( somSelf, AnimalCClassData.instanceDataToken );
END AnimalGetData;




(*
 *  SOM identifiers for all the new and overridden methods
 *)
VAR
  somId_setSound           : SOM.somId;
  somId_getGenus           : SOM.somId;
  somId_getSpecies         : SOM.somId;
  somId_talk               : SOM.somId;
  somId_display            : SOM.somId;
  somId_somInit            : SOM.somId;
  somId_somUninit          : SOM.somId;
  somId_somDumpSelfInt     : SOM.somId;



(*
 *  Apply stubs for new methods
 *)
PROCEDURE somAP_setSound
(
  somSelf  : PAnimal;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
);
VAR
  mySound  : OS2DEF.PSZ;
BEGIN
  mySound := args[0];
  somSelf^.setSound( mySound );
END somAP_setSound;

PROCEDURE somAP_getGenus
(
  somSelf  : PAnimal;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
)          : OS2DEF.PSZ;
BEGIN
  RETURN somSelf^.getGenus();
END somAP_getGenus;

PROCEDURE somAP_getSpecies
(
  somSelf  : PAnimal;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
)          : OS2DEF.PSZ;
BEGIN
  RETURN somSelf^.getSpecies();
END somAP_getSpecies;

PROCEDURE somAP_talk
(
  somSelf  : PAnimal;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
);
BEGIN
  somSelf^.talk();
END somAP_talk;

PROCEDURE somAP_display
(
  somSelf  : PAnimal;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
);
BEGIN
  somSelf^.display();
END somAP_display;




(*
 *  Redispatch stubs for new methods
 *)
PROCEDURE somRD_setSound
(
  somSelf      : PAnimal;
  mySound      : OS2DEF.PSZ
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_setSound, args^ );
  RETURN;
END somRD_setSound;

PROCEDURE somRD_getGenus
(
  somSelf      : PAnimal
)              : OS2DEF.PSZ;
VAR
  retBuffer    : OS2DEF.PSZ;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_getGenus, args^ );
  RETURN retBuffer;
END somRD_getGenus;

PROCEDURE somRD_getSpecies
(
  somSelf      : PAnimal
)              : OS2DEF.PSZ;
VAR
  retBuffer    : OS2DEF.PSZ;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_getSpecies, args^ );
  RETURN retBuffer;
END somRD_getSpecies;

PROCEDURE somRD_talk
(
  somSelf      : PAnimal
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_talk, args^ );
  RETURN;
END somRD_talk;

PROCEDURE somRD_display
(
  somSelf      : PAnimal
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_display, args^ );
  RETURN;
END somRD_display;




(*
 *  forward declared private methods (not declared in any DEF-module)
 *)
PROCEDURE( Self : PAnimal ) somInit();
FORWARD;

PROCEDURE( Self : PAnimal ) somUninit();
FORWARD;

PROCEDURE( Self : PAnimal ) somDumpSelfInt
(
  level         : SOM.INT
);
FORWARD;




(*
 * class initialization
 *)
PROCEDURE AnimalsomInitializeClass;
VAR
  m : Animal;         (* needed for static method references *)
  c : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := AnimaltempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add new methods, if any, to SOM class Animal *)
  AnimalClassData.setSound := c^.somAddStaticMethod
  ( somId_setSound, md, m.setSound, somRD_setSound, somAP_setSound );
  AnimalClassData.getGenus := c^.somAddStaticMethod
  ( somId_getGenus, md, m.getGenus, somRD_getGenus, somAP_getGenus );
  AnimalClassData.getSpecies := c^.somAddStaticMethod
  ( somId_getSpecies, md, m.getSpecies, somRD_getSpecies, somAP_getSpecies );
  AnimalClassData.talk := c^.somAddStaticMethod
  ( somId_talk, md, m.talk, somRD_talk, somAP_talk );
  AnimalClassData.display := c^.somAddStaticMethod
  ( somId_display, md, m.display, somRD_display, somAP_display );

  (* Override inherited methods, if any *)
  c^.somOverrideSMethod( somId_somInit, m.somInit );
  c^.somOverrideSMethod( somId_somUninit, m.somUninit );
  c^.somOverrideSMethod( somId_somDumpSelfInt, m.somDumpSelfInt );

END AnimalsomInitializeClass;




(*
 *  class creation procedure
 *)
PROCEDURE AnimalsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  AnimaltempClassData.classObject := classObject;
  classObject^.somInitClass
  (
		"Animal",
		pClsObj,
    SIZE( AnimalData ),
		Animal_MaxNoMethods,
		Animal_MajorVersion,
    Animal_MinorVersion
  );
  AnimalCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  AnimalsomInitializeClass();
  AnimalCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( AnimalClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  AnimalClassData.classObject := classObject;
END AnimalsomCreateClass;




(*
 *   public NewClass-procedure
 *)
PROCEDURE AnimalNewClass
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
  IF ((majorVersion <> 0) AND (majorVersion <> Animal_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > Animal_MinorVersion)) THEN
    somWriteString( "AnimalNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF AnimalClassData.classObject <> NIL THEN
    RETURN AnimalClassData.classObject;
  END;

  (* Make sure the environment is initialized. *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
    IF SOM.SOMClassMgrObject = NIL THEN
      b := Conversions.StrToLongCard( currentLine(), line );
      SOM.SOMError( SOM.SOMERROR_CouldNotStartup, currentFile(), line );
    END;
    somWriteString( "AnimalNewClass: SOMClassMgrObject initalized..." );
    somWriteLn();
  END;

  (* Get the parent class object. *)
  pClsObj := SOM.SOMObjectNewClass( 1, 1 );       (* static reference *)
  SOM.SOMObjectClassData.classObject := pClsObj;  (* only for root class *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "SOMObject" ), 1, 1 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Explicit metaclass, so get it *)
  mClsObj := M_AnimalNewClass( 0, 0 );       (* static reference *)
  mClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "M_Animal" ), 0, 0 );
  IF mClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoMetaClass, currentFile(), line );
  END;

  SOM.somConstructClass
  ( AnimalsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( AnimaltempClassData ) );

  RETURN AnimalClassData.classObject;
END AnimalNewClass;



(*************************************************************************
   method implementations for SOM class Animal
**************************************************************************)

PROCEDURE( Self : PAnimal ) getGenus() : OS2DEF.PSZ;
CONST
  Genus     : ARRAY OF CHAR = "Unknown Genus";
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "getGenus", currentFile(), currentLine() );
  END;
  RETURN SYSTEM.ADR( Genus );
END getGenus;


PROCEDURE( Self : PAnimal ) getSpecies() : OS2DEF.PSZ;
CONST
  Species   : ARRAY OF CHAR = "Unknown Species";
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "getSpecies", currentFile(), currentLine() );
  END;
  RETURN SYSTEM.ADR( Species );
END getSpecies;


PROCEDURE( Self : PAnimal ) setSound
(
  mySound       : OS2DEF.PSZ
);
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "setSound", currentFile(), currentLine() );
  END;
  IF somThis^.sound <> NIL THEN
    SOM.SOMFree( somThis^.sound );
  END;
  somThis^.sound := SOM.SOMMalloc( Strings.Length( mySound^ )+1 );
  Strings.Assign( mySound^, somThis^.sound^ );
END setSound;


PROCEDURE( Self : PAnimal ) talk();
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "talk", currentFile(), currentLine() );
  END;
  somWriteString( somThis^.sound^ );
  somWriteLn();
END talk;


PROCEDURE( Self : PAnimal ) display();
VAR
  somThis   : PAnimalData;
  Genus     : OS2DEF.PSZ;
  Species   : OS2DEF.PSZ;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "display", currentFile(), currentLine() );
  END;
  somWriteString( "  Genus: " );
  Genus := Self^.getGenus();
  somWriteString( Genus^ );
  somWriteLn();
  Species := Self^.getSpecies();
  somWriteString( "  Species : " );
  somWriteString( Species^ );
  somWriteLn();
  somWriteString( "This Animal says" );
  somWriteLn();
  Self^.talk();
END display;


PROCEDURE( Self : PAnimal ) somInit();
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "somInit", currentFile(), currentLine() );
  END;
  Self^.somInit^();
  somThis^.sound := NIL;
END somInit;


PROCEDURE( Self : PAnimal ) somUninit();
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "somUninit", currentFile(), currentLine() );
  END;
  IF somThis^.sound <> NIL THEN
    SOM.SOMFree( somThis^.sound );
  END;
  Self^.somUninit^();
END somUninit;


PROCEDURE( Self : PAnimal ) somDumpSelfInt
(
  level         : SOM.INT
);
VAR
  somThis   : PAnimalData;
BEGIN
  somThis := AnimalGetData( Self );
  IF AnimalDebug THEN
    somDebug( "Animal", "somDumpSelfInt", currentFile(), currentLine() );
  END;
  Self^.display();
  Self^.somDumpSelfInt^( level );
END somDumpSelfInt;



(*************************************************************************
   Implementation header for SOM class M_Animal
**************************************************************************)

CONST
  M_Animal_MaxNoMethods = 1;
  M_AnimalDebug = FALSE;            (* enable/disable method debugging *)



(*
 * Declare the C specific class data structure
 *)
TYPE
  M_AnimalCClassDataStructure  = RECORD
    parentMtab                   : SOM.somMethodTabs;
    instanceDataToken            : SOM.somDToken;
                                 END;
VAR
  M_AnimalCClassData           : M_AnimalCClassDataStructure;




(*
 * Temporary class data structure used only in class creation
 *)
VAR
  M_AnimaltempClassData        : SOM.somClassDataStructure;



(*
 * Internal instance data
 *)
TYPE
  M_AnimalData          = RECORD
                          END;
  PM_AnimalData         = POINTER TO M_AnimalData;



(*
 *   <class>GetData function
 *)
PROCEDURE M_AnimalGetData( somSelf : PM_Animal ) : PM_AnimalData;
BEGIN
  RETURN NIL; (* no instance data *)
END M_AnimalGetData;




(*
 *  SOM identifiers for all the new and overridden methods
 *)
VAR
  somId_newAnimal         : SOM.somId;




(*
 *  Apply stubs for new methods
 *)
PROCEDURE somAP_newAnimal
(
  somSelf  : PM_Animal;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
)          : SYSTEM.ADDRESS;
VAR
  sound    : OS2DEF.PSZ;
BEGIN
  sound := args[0];
  RETURN somSelf^.newAnimal( sound );
END somAP_newAnimal;




(*
 *  Redispatch stubs for new methods
 *)
PROCEDURE somRD_newAnimal
(
  somSelf      : PAnimal;
  sound        : OS2DEF.PSZ
)              : SYSTEM.ADDRESS;
VAR
  retBuffer    : SYSTEM.ADDRESS;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_newAnimal, args^ );
  RETURN retBuffer;
END somRD_newAnimal;




(*
 *  forward declared private methods
 *)




(*
 * class initialization
 *)
PROCEDURE M_AnimalsomInitializeClass;
VAR
  m : M_Animal;         (* needed for static method references *)
  c : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := M_AnimaltempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add new methods, if any, to SOM class M_Animal *)
  M_AnimalClassData.newAnimal := c^.somAddStaticMethod
  ( somId_newAnimal, md, m.newAnimal, somRD_newAnimal, somAP_newAnimal );

  (* overwrite inherited methods, if any *)

END M_AnimalsomInitializeClass;




(*
 *  class creation procedure
 *)
PROCEDURE M_AnimalsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  M_AnimaltempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "M_Animal",
		pClsObj,
    SIZE( M_AnimalData ),
    M_Animal_MaxNoMethods,
    M_Animal_MajorVersion,
    M_Animal_MinorVersion
  );
  M_AnimalCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  M_AnimalsomInitializeClass();
  M_AnimalCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( M_AnimalClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  M_AnimalClassData.classObject := classObject;
END M_AnimalsomCreateClass;




PROCEDURE M_AnimalNewClass
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
  IF ((majorVersion <> 0) AND (majorVersion <> M_Animal_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > M_Animal_MinorVersion)) THEN
    somWriteString( "M_AnimalNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF M_AnimalClassData.classObject <> NIL THEN
    RETURN M_AnimalClassData.classObject;
  END;

  (* Make sure the environment is initialized. *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
    IF SOM.SOMClassMgrObject = NIL THEN
      b := Conversions.StrToLongCard( currentLine(), line );
      SOM.SOMError( SOM.SOMERROR_CouldNotStartup, currentFile(), line );
    END;
  END;

  (* Get the parent class object. *)
  pClsObj := SOM.SOMClassNewClass( 1, 1 ); (* static reference *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "SOMClass" ), 1, 1 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Use parent's metaclass *)
  mClsObj := pClsObj^.mtab^.classObject;

  SOM.somConstructClass
  ( M_AnimalsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( M_AnimaltempClassData ) );
  RETURN M_AnimalClassData.classObject;
END M_AnimalNewClass;




(*************************************************************************
   method implementations for SOM class M_Animal
**************************************************************************)

PROCEDURE( Self : PM_Animal ) newAnimal
(
  sound         : OS2DEF.PSZ
)               : SYSTEM.ADDRESS;
VAR
  somThis       : PM_AnimalData;
  newInstance   : PAnimal;
BEGIN
  (*
   *  Create an instance of an Animal with a specific sound.
   *)
  somThis := M_AnimalGetData( Self );
  IF M_AnimalDebug THEN
    somDebug( "M_Animal", "newAnimal", currentFile(), currentLine() );
  END;
  newInstance := Self^.somNew();
  newInstance^.setSound( sound );
  RETURN newInstance;
END newAnimal;



BEGIN
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
  END;

  AnimalCClassData.parentMtab := NIL;
  AnimalClassData.classObject := NIL;

  somId_setSound        := SOM.somIdFromString( "setSound"       );
  somId_getGenus        := SOM.somIdFromString( "getGenus"       );
  somId_getSpecies      := SOM.somIdFromString( "getSpecies"     );
  somId_talk            := SOM.somIdFromString( "talk"           );
  somId_display         := SOM.somIdFromString( "display"        );
  somId_somInit         := SOM.somIdFromString( "somInit"        );
  somId_somUninit       := SOM.somIdFromString( "somUninit"      );
  somId_somDumpSelfInt  := SOM.somIdFromString( "somDumpSelfInt" );

  M_AnimalCClassData.parentMtab := NIL;
  M_AnimalClassData.classObject := NIL;

  somId_newAnimal       := SOM.somIdFromString( "newAnimal"      );
END ANIMAL.
