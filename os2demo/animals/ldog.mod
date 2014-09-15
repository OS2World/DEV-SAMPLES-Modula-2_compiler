IMPLEMENTATION MODULE LDOG;

(*$XL+       Modula-2 extenmsions: '_' in symbol names, OOP facilities  *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

(*$LINK
  LIBRARY
    INITINSTANCE
  DESCRIPTION
    'LittleDog class DLL, compiled with Modula-2.  (c) Juergen Neuhoff'
  PROTMODE
  DATA
    MULTIPLE NONSHARED LOADONCALL
*)


IMPORT SOM;      (* basic SOM module *)
IMPORT DOG;      (* module with parent class *)
FROM   SOMMISC IMPORT somDebug, somWriteString, somWriteLn;


(*************************************************************************
   Passthru lines "before" if any
**************************************************************************)

IMPORT OS2DEF;
IMPORT Conversions;
IMPORT Strings;
FROM   SYSTEM IMPORT currentFile;
FROM   SYSTEM IMPORT currentLine;

TYPE PDog = DOG.PDog;



(*************************************************************************
   Passthru lines "after"  if any
**************************************************************************)




(*************************************************************************
   Implementation header for SOM class LittleDog
**************************************************************************)

CONST
  LittleDog_MaxNoMethods = 0;                (* number of new methods *)
  LittleDogDebug         = FALSE;            (* enable/disable method debugging *)



(*
 * Declare the C specific class data structure
 *)
TYPE
  LittleDogCClassDataStructure  = RECORD
    parentMtab                    : SOM.somMethodTabs;
    instanceDataToken             : SOM.somDToken;
                                  END;
VAR
  LittleDogCClassData           : LittleDogCClassDataStructure;



(*
 * Temporary class data structure used only in class creation
 *)
VAR
  LittleDogtempClassData        : SOM.somClassDataStructure;



(*
 * Internal instance data
 *)
TYPE
  LittleDogData    = RECORD
                     END;
  PLittleDogData   = POINTER TO LittleDogData;



(*
 *   <class>GetData function
 *)
PROCEDURE LittleDogGetData( Self : PLittleDog ) : PLittleDogData;
BEGIN
  RETURN SOM.somDataResolve( Self, LittleDogCClassData.instanceDataToken );
END LittleDogGetData;




(*
 *  SOM identifiers for all the new and overridden methods
 *)
VAR
  somId_talk               : SOM.somId;



(*
 *  Apply stubs for new methods if any
 *)




(*
 *  Redispatch stubs for new methods if any
 *)




(*
 *  forward declared private methods (not declared in this DEF-module)
 *)




(*
 * class initialization
 *)
PROCEDURE LittleDogsomInitializeClass;
VAR
  m  : LittleDog;         (* needed for static method references *)
  c  : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := LittleDogtempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add new methods, if any, to SOM class LittleDog *)
  (* ---- *)

  (* Override inherited methods, if any *)
  c^.somOverrideSMethod( somId_talk, m.talk );

END LittleDogsomInitializeClass;




(*
 *  class creation procedure
 *)
PROCEDURE LittleDogsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  LittleDogtempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "LittleDog",
    pClsObj,
    SIZE( LittleDogData ),
    LittleDog_MaxNoMethods,
    LittleDog_MajorVersion,
    LittleDog_MinorVersion
  );
  LittleDogCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  LittleDogsomInitializeClass();
  LittleDogCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( LittleDogClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  LittleDogClassData.classObject := classObject;
END LittleDogsomCreateClass;




(*
 *   public NewClass-procedure
 *)
PROCEDURE LittleDogNewClass
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
  IF ((majorVersion <> 0) AND (majorVersion <> LittleDog_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > LittleDog_MinorVersion)) THEN
    somWriteString( "LittleDogNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF LittleDogClassData.classObject <> NIL THEN
    RETURN LittleDogClassData.classObject;
  END;

  (* Make sure the environment is initialized. *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
    IF SOM.SOMClassMgrObject = NIL THEN
      b := Conversions.StrToLongCard( currentLine(), line );
      SOM.SOMError( SOM.SOMERROR_CouldNotStartup, currentFile(), line );
    END;
    somWriteString( "LittleDogNewClass: SOMClassMgrObject initalized..." );
    somWriteLn();
  END;

  (* Get the parent class object. *)
  pClsObj := DOG.DogNewClass( 0, 0 );       (* static reference *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "Dog" ), 0, 0 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Use parent's metaclass *)
  mClsObj := pClsObj^.mtab^.classObject;

  SOM.somConstructClass
  ( LittleDogsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( LittleDogtempClassData ) );

  RETURN LittleDogClassData.classObject;
END LittleDogNewClass;



(*************************************************************************
   method implementations for SOM class LittleDog
**************************************************************************)

PROCEDURE( Self : PLittleDog ) talk();
VAR
  somThis   : PLittleDogData;
BEGIN
  somThis := LittleDogGetData( Self );
  IF LittleDogDebug THEN
    somDebug( "LittleDog", "talk", currentFile(), currentLine() );
  END;
  somWriteString( "woof woof" );
  somWriteLn();
  somWriteString( "woof woof" );
  somWriteLn();
END talk;




BEGIN
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
  END;

  LittleDogCClassData.parentMtab := NIL;
  LittleDogClassData.classObject := NIL;

  somId_talk            := SOM.somIdFromString( "talk"           );

END LDOG.
