IMPLEMENTATION MODULE BDOG;

(*$XL+       Modula-2 extenmsions: '_' in symbol names, OOP facilities  *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

(*$LINK
  LIBRARY
    INITINSTANCE
  DESCRIPTION
    'BigDog class DLL, compiled with Modula-2.  (c) Juergen Neuhoff'
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
   Implementation header for SOM class BigDog
**************************************************************************)

CONST
  BigDog_MaxNoMethods = 0;                (* number of new methods *)
  BigDogDebug         = FALSE;            (* enable/disable method debugging *)



(*
 * Declare the C specific class data structure
 *)
TYPE
  BigDogCClassDataStructure  = RECORD
    parentMtab                 : SOM.somMethodTabs;
    instanceDataToken          : SOM.somDToken;
                               END;
VAR
  BigDogCClassData           : BigDogCClassDataStructure;



(*
 * Temporary class data structure used only in class creation
 *)
VAR
  BigDogtempClassData        : SOM.somClassDataStructure;



(*
 * Internal instance data
 *)
TYPE
  BigDogData    = RECORD
                  END;
  PBigDogData   = POINTER TO BigDogData;



(*
 *   <class>GetData function
 *)
PROCEDURE BigDogGetData( Self : PBigDog ) : PBigDogData;
BEGIN
  RETURN SOM.somDataResolve( Self, BigDogCClassData.instanceDataToken );
END BigDogGetData;




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
PROCEDURE BigDogsomInitializeClass;
VAR
  m  : BigDog;         (* needed for static method references *)
  c  : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := BigDogtempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add new methods, if any, to SOM class BigDog *)
  (* ---- *)

  (* Override inherited methods, if any *)
  c^.somOverrideSMethod( somId_talk, m.talk );

END BigDogsomInitializeClass;




(*
 *  class creation procedure
 *)
PROCEDURE BigDogsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  BigDogtempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "BigDog",
    pClsObj,
    SIZE( BigDogData ),
    BigDog_MaxNoMethods,
    BigDog_MajorVersion,
    BigDog_MinorVersion
  );
  BigDogCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  BigDogsomInitializeClass();
  BigDogCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( BigDogClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  BigDogClassData.classObject := classObject;
END BigDogsomCreateClass;




(*
 *   public NewClass-procedure
 *)
PROCEDURE BigDogNewClass
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
  IF ((majorVersion <> 0) AND (majorVersion <> BigDog_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > BigDog_MinorVersion)) THEN
    somWriteString( "BigDogNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF BigDogClassData.classObject <> NIL THEN
    RETURN BigDogClassData.classObject;
  END;

  (* Make sure the environment is initialized. *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
    IF SOM.SOMClassMgrObject = NIL THEN
      b := Conversions.StrToLongCard( currentLine(), line );
      SOM.SOMError( SOM.SOMERROR_CouldNotStartup, currentFile(), line );
    END;
    somWriteString( "BigDogNewClass: SOMClassMgrObject initalized..." );
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
  ( BigDogsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( BigDogtempClassData ) );

  RETURN BigDogClassData.classObject;
END BigDogNewClass;



(*************************************************************************
   method implementations for SOM class BigDog
**************************************************************************)

PROCEDURE( Self : PBigDog ) talk();
VAR
  somThis   : PBigDogData;
BEGIN
  somThis := BigDogGetData( Self );
  IF BigDogDebug THEN
    somDebug( "BigDog", "talk", currentFile(), currentLine() );
  END;
  somWriteString( "WOOF WOOF" );
  somWriteLn();
  somWriteString( "WOOF WOOF" );
  somWriteLn();
END talk;




BEGIN
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
  END;

  BigDogCClassData.parentMtab := NIL;
  BigDogClassData.classObject := NIL;

  somId_talk            := SOM.somIdFromString( "talk"           );

END BDOG.
