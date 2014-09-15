MODULE TESTCAR;

(*************************************************************************
   TESTCAR     Workplace Shell class registration for 'Car'

   Copyright (c) 1995 by Juergen Neuhoff

   Description: This module performs the registration of the sample
   workplace class 'Car'. The class must be implemented in a module
   'CAR.DLL'.

   How to compile and link:
     MOD TESTCAR -o -m
     LINk386 @TESTCAR.RSP

**************************************************************************)

(*$XL+  language extensions: extended symbols, OOP features *)
(*$XF+  extended function desigantor usage *)

IMPORT SYSTEM;
IMPORT WINWORKPLACE;
IMPORT DOSPROCESS;
IMPORT OS2DEF;
IMPORT SOM;
IMPORT DOSFILEMGR;
IMPORT Conversions;
IMPORT Strings;
IMPORT DOSMODULEMGR;


(*****

PROCEDURE SOMError
(
  Code       : SOM.INT;
  FileName   : ARRAY OF CHAR;
  LineNumber : SOM.INT
);
VAR
  rc           : OS2DEF.APIRET;
  hLog         : DOSFILEMGR.HFILE;
  ActionTaken  : LONGCARD;
  BytesWritten : LONGCARD;
  LineStr      : ARRAY [0..12] OF CHAR;
  LogLocation  : LONGCARD;
BEGIN
  rc := DOSFILEMGR.DosOpen
  ( "TESTCAR.LOG",
    hLog,
    ActionTaken,
    0,
    DOSFILEMGR.FILE_NORMAL,
    DOSFILEMGR.OPEN_ACTION_CREATE_IF_NEW +
    DOSFILEMGR.OPEN_ACTION_OPEN_IF_EXISTS,
    DOSFILEMGR.OPEN_SHARE_DENYNONE +
    DOSFILEMGR.OPEN_ACCESS_READWRITE,
    NIL
  );
  IF rc = 0 THEN
    rc := DOSFILEMGR.DosSetFilePtr
    ( hLog,
      0,
      DOSFILEMGR.FILE_END,
      LogLocation
    );
    IF rc = 0 THEN
      rc := DOSFILEMGR.DosWrite
      ( hLog,
        "File : ",
        7,
        BytesWritten
      );
      IF rc = 0 THEN
        rc := DOSFILEMGR.DosWrite
        ( hLog,
          FileName,
          Strings.Size( FileName ),
          BytesWritten
        );
        IF rc = 0 THEN
          rc := DOSFILEMGR.DosWrite
          ( hLog,
            "  Line : ",
            9,
            BytesWritten
          );
          IF rc = 0 THEN
            Conversions.LongIntToStr( LineNumber, LineStr );
            rc := DOSFILEMGR.DosWrite
            ( hLog,
              LineStr,
              Strings.Length( LineStr ),
              BytesWritten
            );
          END;
        END;
      END;
    END;
  END;
  DOSFILEMGR.DosClose( hLog );
  IF rc <> 0 THEN
    DOSPROCESS.DosBeep( 1500, 500 );
  END;
END SOMError;

****)

VAR
  hobj             : WINWORKPLACE.HOBJECT;
  done             : BOOLEAN;
  rc               : OS2DEF.APIRET;
  somFindClsInFile : SOM.somMethodPtr;
  CarPath          : ARRAY [0..OS2DEF.CCHMAXPATHCOMP-1] OF CHAR;

BEGIN
(****
  (* for debugging only ... *)
  SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
  somFindClsInFile := SOM.somClassResolve
  ( SOM.SOMClassMgrClassData.classObject,
    SOM.SOMClassMgrClassData.somFindClsInFile
  );
  SOM.SOMError := SOMError;
  SOM.SOM_TraceLevel   := 2;
  SOM.SOM_WarnLevel    := 1;
  SOM.SOM_AssertLevel  := 2;
****)

  (* class registration *)
  rc := DOSFILEMGR.DosQueryPathInfo
  ( "CAR.DLL", DOSFILEMGR.FIL_QUERYFULLNAME, CarPath, SIZE( CarPath ) );
  IF rc = 0 THEN
    done := WINWORKPLACE.WinRegisterObjectClass( "Car", CarPath );
  ELSE
    done := FALSE;
  END;
  IF NOT done THEN
    rc := DOSPROCESS.DosBeep( 500, 500 );
  END;

(****
  (* object creation *)
  IF done THEN
    hobj := WINWORKPLACE.WinCreateObject
    (
      "Car",
      "MyCar",
      "",
      "C:\",
      WINWORKPLACE.CO_REPLACEIFEXISTS
    );
    IF hobj <> 0 THEN
      RETURN;
    END;
  END;
  rc := DOSPROCESS.DosBeep( 2000, 1500 );
****)

(****
  (* class deregistration *)
  done := WINWORKPLACE.WinDeregisterObjectClass( "Car" );
  IF NOT done THEN
    rc := DOSPROCESS.DosBeep( 500, 1500 );
  END;
  RETURN;
****)

END TESTCAR.
