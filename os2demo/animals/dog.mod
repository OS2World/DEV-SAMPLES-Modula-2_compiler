IMPLEMENTATION MODULE DOG;

(*$XL+       Modula-2 extenmsions: '_' in symbol names, OOP facilities  *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

(*$LINK
  LIBRARY DOG INITINSTANCE
  DESCRIPTION 'Dog class DLL, compiled with Modula-2.  (c) Juergen Neuhoff'
  PROTMODE
  DATA MULTIPLE NONSHARED LOADONCALL
*)


IMPORT SOM;      (* basic SOM module *)
IMPORT ANIMAL;   (* module with parent class *)
FROM   SOMMISC IMPORT somDebug, somWriteString, somWriteLn;


(*************************************************************************
   Passthru lines "before" if any
**************************************************************************)

IMPORT OS2DEF;
IMPORT Conversions;
IMPORT Strings;
FROM   SYSTEM IMPORT currentFile;
FROM   SYSTEM IMPORT currentLine;
FROM   SYSTEM IMPORT ADR;

TYPE PAnimal = ANIMAL.PAnimal;



(*************************************************************************
   Passthru lines "after"  if any
**************************************************************************)




(*************************************************************************
   Implementation header for SOM class Dog
**************************************************************************)

CONST
  Dog_MaxNoMethods = 4;                (* number of new methods *)
  DogDebug         = FALSE;            (* enable/disable method debugging *)



(*
 * Declare the C specific class data structure
 *)
TYPE
  DogCClassDataStructure  = RECORD
    parentMtab              : SOM.somMethodTabs;
    instanceDataToken       : SOM.somDToken;
                            END;
VAR
  DogCClassData           : DogCClassDataStructure;



(*
 * Temporary class data structure used only in class creation
 *)
VAR
  DogtempClassData        : SOM.somClassDataStructure;



(*
 * Internal instance data
 *)
TYPE
  DogData          = RECORD
    breed            : OS2DEF.PSZ;
    color            : OS2DEF.PSZ;
                     END;
  PDogData         = POINTER TO DogData;



(*
 *   <class>GetData function
 *)
PROCEDURE DogGetData( Self : PDog ) : PDogData;
BEGIN
  RETURN SOM.somDataResolve( Self, DogCClassData.instanceDataToken );
END DogGetData;




(*
 *  SOM identifiers for all the new and overridden methods
 *)
VAR
  somId_setBreed           : SOM.somId;
  somId_setColor           : SOM.somId;
  somId_getBreed           : SOM.somId;
  somId_getColor           : SOM.somId;
  somId_getGenus           : SOM.somId;
  somId_getSpecies         : SOM.somId;
  somId_display            : SOM.somId;
  somId_somInit            : SOM.somId;
  somId_somUninit          : SOM.somId;
  somId_somDumpSelfInt     : SOM.somId;



(*
 *  Apply stubs for new methods
 *)
PROCEDURE somAP_setBreed
(
  Self     : PDog;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
);
VAR
  myBreed  : OS2DEF.PSZ;
BEGIN
  myBreed := args[0];
  Self^.setBreed( myBreed );
END somAP_setBreed;

PROCEDURE somAP_setColor
(
  Self     : PDog;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
);
VAR
  myColor  : OS2DEF.PSZ;
BEGIN
  myColor := args[0];
  Self^.setColor( myColor );
END somAP_setColor;

PROCEDURE somAP_getBreed
(
  Self     : PDog;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
)          : OS2DEF.PSZ;
BEGIN
  RETURN Self^.getBreed();
END somAP_getBreed;

PROCEDURE somAP_getColor
(
  Self     : PDog;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
)          : OS2DEF.PSZ;
BEGIN
  RETURN Self^.getColor();
END somAP_getColor;




(*
 *  Redispatch stubs for new methods
 *)
PROCEDURE somRD_setBreed
(
  somSelf      : PDog;
  myBreed      : OS2DEF.PSZ
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_setBreed, args^ );
  RETURN;
END somRD_setBreed;

PROCEDURE somRD_setColor
(
  somSelf      : PDog;
  myColor      : OS2DEF.PSZ
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_setColor, args^ );
  RETURN;
END somRD_setColor;

PROCEDURE somRD_getBreed
(
  somSelf      : PDog
)              : OS2DEF.PSZ;
VAR
  retBuffer    : OS2DEF.PSZ;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_getBreed, args^ );
  RETURN retBuffer;
END somRD_getBreed;

PROCEDURE somRD_getColor
(
  somSelf      : PDog
)              : OS2DEF.PSZ;
VAR
  retBuffer    : OS2DEF.PSZ;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_getColor, args^ );
  RETURN retBuffer;
END somRD_getColor;




(*
 *  forward declared private methods (not declared in this DEF-module)
 *)
PROCEDURE( Self : PDog ) somInit();
FORWARD;

PROCEDURE( Self : PDog ) somUninit();
FORWARD;

PROCEDURE( Self : PDog ) somDumpSelfInt
(
  level         : SOM.INT
);
FORWARD;

PROCEDURE( Self : PDog ) getGenus() : OS2DEF.PSZ;
FORWARD;

PROCEDURE( Self : PDog ) getSpecies() : OS2DEF.PSZ;
FORWARD;

PROCEDURE( Self : PDog ) display();
FORWARD;




(*
 * class initialization
 *)
PROCEDURE DogsomInitializeClass;
VAR
  m : Dog;         (* needed for static method references *)
  c : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := DogtempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add new methods, if any, to SOM class Dog *)
  DogClassData.setBreed := c^.somAddStaticMethod
  ( somId_setBreed, md, m.setBreed, somRD_setBreed, somAP_setBreed );
  DogClassData.setColor := c^.somAddStaticMethod
  ( somId_setColor, md, m.setColor, somRD_setColor, somAP_setColor );
  DogClassData.getBreed := c^.somAddStaticMethod
  ( somId_getBreed, md, m.getBreed, somRD_getBreed, somAP_getBreed );
  DogClassData.getColor := c^.somAddStaticMethod
  ( somId_getColor, md, m.getColor, somRD_getColor, somAP_getColor );

  (* Override inherited methods, if any *)
  c^.somOverrideSMethod( somId_somInit, m.somInit );
  c^.somOverrideSMethod( somId_somUninit, m.somUninit );
  c^.somOverrideSMethod( somId_somDumpSelfInt, m.somDumpSelfInt );
  c^.somOverrideSMethod( somId_getGenus, m.getGenus );
  c^.somOverrideSMethod( somId_getSpecies, m.getSpecies );
  c^.somOverrideSMethod( somId_display, m.display );
END DogsomInitializeClass;




(*
 *  class creation procedure
 *)
PROCEDURE DogsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  DogtempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "Dog",
    pClsObj,
    SIZE( DogData ),
    Dog_MaxNoMethods,
    Dog_MajorVersion,
    Dog_MinorVersion
  );
  DogCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  DogsomInitializeClass();
  DogCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( DogClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  DogClassData.classObject := classObject;
END DogsomCreateClass;




(*
 *   public NewClass-procedure
 *)
PROCEDURE DogNewClass
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
  IF ((majorVersion <> 0) AND (majorVersion <> Dog_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > Dog_MinorVersion)) THEN
    somWriteString( "DogNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF DogClassData.classObject <> NIL THEN
    RETURN DogClassData.classObject;
  END;

  (* Make sure the environment is initialized. *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
    IF SOM.SOMClassMgrObject = NIL THEN
      b := Conversions.StrToLongCard( currentLine(), line );
      SOM.SOMError( SOM.SOMERROR_CouldNotStartup, currentFile(), line );
    END;
    somWriteString( "DogNewClass: SOMClassMgrObject initalized..." );
    somWriteLn();
  END;

  (* Get the parent class object. *)
  pClsObj := ANIMAL.AnimalNewClass( 0, 0 );       (* static reference *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "Animal" ), 0, 0 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Explicit metaclass, so get it *)
  mClsObj := M_DogNewClass( 0, 0 );       (* static reference *)
  mClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "M_Dog" ), 0, 0 );
  IF mClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoMetaClass, currentFile(), line );
  END;

  SOM.somConstructClass
  ( DogsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( DogtempClassData ) );

  RETURN DogClassData.classObject;
END DogNewClass;



(*************************************************************************
   method implementations for SOM class Dog
**************************************************************************)


PROCEDURE( Self : PDog ) getBreed() : OS2DEF.PSZ;
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "getBreed", currentFile(), currentLine() );
  END;
  RETURN somThis^.breed;
END getBreed;


PROCEDURE( Self : PDog ) getColor() : OS2DEF.PSZ;
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "getColor", currentFile(), currentLine() );
  END;
  RETURN somThis^.color;
END getColor;


PROCEDURE( Self : PDog ) setBreed( myBreed : OS2DEF.PSZ );
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "setBreed", currentFile(), currentLine() );
  END;
  IF somThis^.breed <> NIL THEN
    SOM.SOMFree( somThis^.breed );
  END;
  somThis^.breed := SOM.SOMMalloc( Strings.Length( myBreed^ )+1 );
  Strings.Assign( myBreed^, somThis^.breed^ );
END setBreed;


PROCEDURE( Self : PDog ) setColor( myColor : OS2DEF.PSZ );
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "setColor", currentFile(), currentLine() );
  END;
  IF somThis^.color <> NIL THEN
    SOM.SOMFree( somThis^.color );
  END;
  somThis^.color := SOM.SOMMalloc( Strings.Length( myColor^ )+1 );
  Strings.Assign( myColor^, somThis^.color^ );
END setColor;


PROCEDURE( Self : PDog ) somInit();
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "somInit", currentFile(), currentLine() );
  END;
  Self^.somInit^();
  somThis^.color := NIL;
  somThis^.breed := NIL;
END somInit;


PROCEDURE( Self : PDog ) somUninit();
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "somUninit", currentFile(), currentLine() );
  END;
  IF somThis^.color <> NIL THEN
    SOM.SOMFree( somThis^.color );
  END;
  IF somThis^.breed <> NIL THEN
    SOM.SOMFree( somThis^.breed );
  END;
  Self^.somUninit^();
END somUninit;


PROCEDURE( Self : PDog ) somDumpSelfInt
(
  level         : SOM.INT
);
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "somDumpSelfInt", currentFile(), currentLine() );
  END;
  Self^.somDumpSelfInt^( level );
END somDumpSelfInt;


PROCEDURE( Self : PDog ) getGenus() : OS2DEF.PSZ;
CONST
  Genus     : ARRAY OF CHAR = "Canis";
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "getGenus", currentFile(), currentLine() );
  END;
  RETURN SYSTEM.ADR( Genus );
END getGenus;


PROCEDURE( Self : PDog ) getSpecies() : OS2DEF.PSZ;
CONST
  Species   : ARRAY OF CHAR = "Familiaris";
VAR
  somThis   : PDogData;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "getSpecies", currentFile(), currentLine() );
  END;
  RETURN SYSTEM.ADR( Species );
END getSpecies;


PROCEDURE( Self : PDog ) display();
VAR
  somThis   : PDogData;
  Breed     : OS2DEF.PSZ;
  Color     : OS2DEF.PSZ;
BEGIN
  somThis := DogGetData( Self );
  IF DogDebug THEN
    somDebug( "Dog", "display", currentFile(), currentLine() );
  END;
  somWriteString( "  Breed: " );
  Breed := Self^.getBreed();
  somWriteString( Breed^ );
  somWriteLn();
  Color := Self^.getColor();
  somWriteString( "  Color : " );
  somWriteString( Color^ );
  somWriteLn();
  Self^.display^();
END display;




(*************************************************************************
   Implementation header for SOM class M_Dog
**************************************************************************)

CONST
  M_Dog_MaxNoMethods = 1;        (* number of new class factory methods *)
  M_DogDebug = FALSE;            (* enable/disable method debugging *)



(*
 * Declare the C specific class data structure
 *)
TYPE
  M_DogCClassDataStructure  = RECORD
    parentMtab                : SOM.somMethodTabs;
    instanceDataToken         : SOM.somDToken;
                              END;
VAR
  M_DogCClassData           : M_DogCClassDataStructure;




(*
 * Temporary class data structure used only in class creation
 *)
VAR
  M_DogtempClassData        : SOM.somClassDataStructure;



(*
 * Internal instance data
 *)
TYPE
  M_DogData          = RECORD END;
  PM_DogData         = POINTER TO M_DogData;



(*
 *   <class>GetData function
 *)
PROCEDURE M_DogGetData( Self : PM_Dog ) : PM_DogData;
BEGIN
  RETURN NIL; (* no instance data *)
END M_DogGetData;




(*
 *  SOM identifiers for all the new and overridden methods
 *)
VAR
  somId_newDog         : SOM.somId;




(*
 *  Apply stubs for new methods
 *)
PROCEDURE somAP_newDog
(
  Self  : PM_Dog;
  id       : SOM.somId;
  desc     : SOM.somId;
  VAR args : ARRAY OF SOM.DWORD
)          : SYSTEM.ADDRESS;
VAR
  sound    : OS2DEF.PSZ;
  breed    : OS2DEF.PSZ;
  color    : OS2DEF.PSZ;
BEGIN
  sound := args[0];
  breed := args[1];
  color := args[2];
  RETURN Self^.newDog( sound, breed, color );
END somAP_newDog;




(*
 *  Redispatch stubs for new methods
 *)
PROCEDURE somRD_newDog
(
  somSelf      : PDog;
  sound        : OS2DEF.PSZ;
  breed        : OS2DEF.PSZ;
  color        : OS2DEF.PSZ
)              : SYSTEM.ADDRESS;
VAR
  retBuffer    : SYSTEM.ADDRESS;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_newDog, args^ );
  RETURN retBuffer;
END somRD_newDog;




(*
 *  forward declared private methods
 *)




(*
 * class initialization
 *)
PROCEDURE M_DogsomInitializeClass;
VAR
  m : M_Dog;         (* needed for static method references *)
  c : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := M_DogtempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add new methods, if any, to SOM class M_Dog *)
  M_DogClassData.newDog := c^.somAddStaticMethod
  ( somId_newDog, md, m.newDog, somRD_newDog, somAP_newDog );

  (* overwrite inherited methods, if any *)

END M_DogsomInitializeClass;




(*
 *  class creation procedure
 *)
PROCEDURE M_DogsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  M_DogtempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "M_Dog",
    pClsObj,
    SIZE( M_DogData ),
    M_Dog_MaxNoMethods,
    M_Dog_MajorVersion,
    M_Dog_MinorVersion
  );
  M_DogCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  M_DogsomInitializeClass();
  M_DogCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( M_DogClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  M_DogClassData.classObject := classObject;
END M_DogsomCreateClass;




PROCEDURE M_DogNewClass
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
  IF ((majorVersion <> 0) AND (majorVersion <> M_Dog_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > M_Dog_MinorVersion)) THEN
    somWriteString( "M_DogNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF M_DogClassData.classObject <> NIL THEN
    RETURN M_DogClassData.classObject;
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
  pClsObj := ANIMAL.M_AnimalNewClass( 0, 0 ); (* static reference *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "M_Animal" ), 0, 0 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Use parent's metaclass *)
  mClsObj := pClsObj^.mtab^.classObject;

  SOM.somConstructClass
  ( M_DogsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( M_DogtempClassData ) );
  RETURN M_DogClassData.classObject;
END M_DogNewClass;




(*************************************************************************
   method implementations for SOM class M_Dog
**************************************************************************)

PROCEDURE( Self : PM_Dog ) newDog
(
  sound         : OS2DEF.PSZ;
  breed         : OS2DEF.PSZ;
  color         : OS2DEF.PSZ
)               : SYSTEM.ADDRESS;
VAR
  somThis       : PM_DogData;
  newInstance   : PAnimal;
BEGIN
  (*
   *  Create an instance of an Dog with a specific sound.
   *)
  somThis := M_DogGetData( Self );
  IF M_DogDebug THEN
    somDebug( "M_Dog", "newDog", currentFile(), currentLine() );
  END;
  newInstance := Self^.newAnimal( sound );
  WITH newInstance : PDog DO
    newInstance^.setBreed( breed );
    newInstance^.setColor( color );
    RETURN newInstance;
  END;
END newDog;




BEGIN
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
  END;

  DogCClassData.parentMtab := NIL;
  DogClassData.classObject := NIL;

  somId_setBreed        := SOM.somIdFromString( "setBreed"       );
  somId_setColor        := SOM.somIdFromString( "setColor"       );
  somId_getBreed        := SOM.somIdFromString( "getBreed"       );
  somId_getColor        := SOM.somIdFromString( "getColor"       );
  somId_getGenus        := SOM.somIdFromString( "getGenus"       );
  somId_getSpecies      := SOM.somIdFromString( "getSpecies"     );
  somId_display         := SOM.somIdFromString( "display"        );
  somId_somInit         := SOM.somIdFromString( "somInit"        );
  somId_somUninit       := SOM.somIdFromString( "somUninit"      );
  somId_somDumpSelfInt  := SOM.somIdFromString( "somDumpSelfInt" );

  M_DogCClassData.parentMtab := NIL;
  M_DogClassData.classObject := NIL;

  somId_newDog          := SOM.somIdFromString( "newDog"         );
END DOG.
