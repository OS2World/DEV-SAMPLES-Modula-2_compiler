(*$XL+       Modula-2 extensions: '_' in symbol names, OOP facilities   *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

MODULE TESTANIMALS;

(************************************************************************
    Copyright (c) 1994,1995 by Juergen Neuhoff

    This is the Modula-2 version of the ANIMALS sample program
    from the IBM Developer's Toolkit 2.1 for OS/2.

    It demonstrates the implementation and usage of new SOM-classes.
    And it also shows the creation of dynamic link libraries for SOM.

    Enter the following commands to compile and run this demo:

    MOD TESTANIM.MOD -o -m
    LINK386 @ANIMAL.RSP
    LINK386 @DOG.RSP
    LINK386 @LDOG.RSP
    LINK386 @BDOG.RSP
    LINK386 @TESTANIM.RSP
    TESTANIM

    Recommended compiler switches:
      -o     Code optimization
      -m     Make for linker

*************************************************************************)


IMPORT SOM;
IMPORT ANIMAL;
IMPORT DOG;
IMPORT LDOG;
IMPORT BDOG;
IMPORT OS2DEF;
IMPORT Storage;
FROM   SOMMISC  IMPORT somWriteString, somWriteLn;
FROM   SYSTEM   IMPORT ADR;


VAR
  myAnimal       : ANIMAL.PAnimal;
  myDog          : DOG.PDog;
  myLittleDog    : LDOG.PLittleDog;
  myBigDog       : BDOG.PBigDog;
  classAnimal    : SOM.PSOMClass;
  classDog       : SOM.PSOMClass;
  classLittleDog : SOM.PSOMClass;
  classBigDog    : SOM.PSOMClass;
  NoRetValue     : SOM.somToken;
  ok             : BOOLEAN;


PROCEDURE newString( szVal : ARRAY OF CHAR ) : OS2DEF.PSZ;
BEGIN
  (* This C-style procedure uses caller's address of 'szVal' *)
  RETURN ADR( szVal );
END newString;


BEGIN

  (* create classes *)
  classAnimal := ANIMAL.AnimalNewClass
  ( ANIMAL.Animal_MajorVersion, ANIMAL.Animal_MinorVersion );
  classDog := DOG.DogNewClass
  ( DOG.Dog_MajorVersion, DOG.Dog_MinorVersion );
  classLittleDog := LDOG.LittleDogNewClass
  ( LDOG.LittleDog_MajorVersion, LDOG.LittleDog_MinorVersion );
  classBigDog := BDOG.BigDogNewClass
  ( BDOG.BigDog_MajorVersion, BDOG.BigDog_MinorVersion );


  (* create objects using constructors 'newAnimal' and 'newDog' *)
  myAnimal := classAnimal( ANIMAL.PM_Animal )^.newAnimal
  (
    newString( "Roar!!!" )
  );
  myDog := classDog( DOG.PM_Dog )^.newDog
  (
    newString( "Grrr" ),
    newString( "Retriever" ),
    newString( "Yellow" )
  );
  myLittleDog := classLittleDog( DOG.PM_Dog )^.newDog
  (
    newString( "unknown noise" ),
    newString( "French Poodle" ),
    newString( "Black" )
  );
  myBigDog := classBigDog( DOG.PM_Dog )^.newDog
  (
    newString( "unknown noise" ),
    newString( "German Shepherd" ),
    newString( "Brown")
  );


  (* display objects *)

  somWriteString( "=====================" );
  somWriteLn();
  somWriteString( "myAnimal:" );
  somWriteLn();
  myAnimal^.display();

  somWriteString( "=====================" );
  somWriteLn();
  somWriteString( "myDog:" );
  somWriteLn();
  myDog^.display();

  somWriteString( "=====================" );
  somWriteLn();
  somWriteString( "myLittleDog:" );
  somWriteLn();
  myLittleDog^.display();

  somWriteString( "=====================" );
  somWriteLn();
  somWriteString( "myBigDog:" );
  somWriteLn();
  myBigDog^.display();

  somWriteString( "=====================" );
  somWriteLn();
  somWriteString( "myBigDog again:" );
  somWriteLn();
  NoRetValue := NIL;
  ok := myBigDog^.somDispatch
  ( NoRetValue, SOM.somIdFromString( "display" ), myBigDog );

  (* free objects *)
  somWriteLn();
  myAnimal^.somFree();
  myDog^.somFree();
  myLittleDog^.somFree();
  myBigDog^.somFree();

  HALT();

END TESTANIMALS.
