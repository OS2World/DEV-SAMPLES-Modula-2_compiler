IMPLEMENTATION MODULE CAR;

(************************************************************************
  OS/2 2.x/3.0 implementation of Workplace Shell sample class 'Car'.

  This Modula-2 version is based upon a similar C-program
  sample class taken from IBM Developer's Toolkit 2.1.

  Copyright (c) 1995 by Juergen Neuhoff

  Entry Points:

     Class Methods:

        M_Car.QueryModuleHandle

     Overridden Class Methods:

        M_Car.wpclsInitData
        M_Car.wpclsQueryDefaultHelp
        M_Car.wpclsQueryDefaultView
        M_Car.wpclsQueryDetailsInfo
        M_Car.wpclsQueryIconData
        M_Car.wpclsQueryStyle
        M_Car.wpclsQueryTitle
        M_Car.wpclsUnInitData

     Instance Methods:

        Car.QueryBrakes
        Car.SetBrakes
        Car.QueryDuration
        Car.SetDuration
        Car.QueryHighTone
        Car.SetHighTone
        Car.QueryLowTone
        Car.SetLowTone
        Car.QuerySpeed
        Car.SetSpeed
        Car.BeepHorn
        Car.AddDashboardPage
        Car.AddHornBeepPage

     Overridden Instance Methods:

        Car.wpAddSettingsPages
        Car.wpFilterPopupMenu
        Car.wpInitData
        Car.wpMenuItemHelpSelected
        Car.wpMenuItemSelected
        Car.wpModifyPopupMenu
        Car.wpViewObject
        Car.wpQueryDetailsData
        Car.wpRestoreState
        Car.wpSaveState
        Car.wpUnInitData

     Non-Method Functions:

        CarInit
        CarWndProc
        DashBoardDlgProc
        HornBeepDlgProc

  How to compile:

     To create a class dynamic link library run the following commands:

       MOD CAR -o -m      <-- This Modula-2 compiler
       LINK386 @CAR.RSP   <-- 32-bit OS/2 linker
       RC CAR.RC CAR.DLL  <-- Resource compiler from OS/2 Developer's Toolkit

  Notes:
     This module is still being developed. It is not yet fully tested. 
     It may contain bugs. Hence the major parts are still placed in the
     commentary brackets (**** and ****).


*************************************************************************)

(*$XL+       Modula-2 extensions: '_' in symbol names, OOP facilities   *)
(*$XF+       Relaxed function designators                               *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

(*$LINK
  LIBRARY Car INITINSTANCE
  PROTMODE
  DATA MULTIPLE NONSHARED LOADONCALL
*)



(*************************************************************************
   Common IMPORTs for a SOM-class implementation.
**************************************************************************)

IMPORT SOM;             (* basic SOM module, always needed *)
IMPORT WPDATAF;         (* module with parent class, always needed *)
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

(********
IMPORT DOSEXCEPTIONS;
*********)
(*********
IMPORT DOSFILEMGR;
IMPORT DOSMODULEMGR;
IMPORT DOSPROCESS;
IMPORT OS2DEF;
IMPORT WIN;                 (* complete Win-API *)
IMPORT WINWORKPLACE;
IMPORT WPOBJECT;
IMPORT Strings;
*********)


(********

CONST
  ID_TITLE            = 100;
  ID_ICON             = 101;

CONST
  IDD_DASHBOARD       = 200;                  (* settings page (dialog) *)
  IDD_HORNBEEP        = 202;

(*
 *   The following user-defined Popup menu items (ID_xxx) should be higher
 *   than WPMENUID_USER.
 *
 *   The ID_OPENMENU will become a submenu of the system's popup open menu
 *   id, WPMENUID_OPEN.
 *)
CONST
  WPMENUID_USER       = WPOBJECT.WPMENUID_USER;
  ID_BEEPMENU         = (WPMENUID_USER+1);  (* menus added to popup menu *)
  ID_OPENMENU         = (WPMENUID_USER+2);
  ID_TRAPMENU         = (WPMENUID_USER+3);

CONST
  IDM_OPENCAR         = (WPMENUID_USER+4);  (* submenus of added menus *)
  IDM_BEEPHORN        = (WPMENUID_USER+5);
  IDM_TRAPCAR         = (WPMENUID_USER+6);

CONST
  IDMSG_ACCESS_VIOLATION   = 100;
  IDM_MSGBOX               = 999;

CONST
  ID_FRAME                 = 3000;          (* frame  window id *)
  ID_CLIENT                = 3001;          (* client window id *)

(*
 *   Set unique view ids.
 *)
CONST
  OPEN_CAR             = ID_OPENMENU;

CONST
  CAR_TIMER            = 1001;              (* timer id *)

(*
 *   IDs of dialog items in CAR.RC
 *)
CONST
  ID_UNDO              = 801;
  ID_DEFAULT           = 802;
  ID_HELP              = 803;
  ID_HITONE            = 804;
  ID_LOTONE            = 805;
  ID_SPEEDSLIDER       = 806;
  ID_STOP              = 807;
  ID_SPEEDDATA         = 808;
  ID_GO                = 809;

(*
 *  Keys for save-restore methods
 *)
CONST
  IDKEY_HITONE         = 1;
  IDKEY_LOTONE         = 2;
  IDKEY_DURATION       = 3;
  IDKEY_SPEED          = 4;
  IDKEY_BRAKEFLAG      = 5;

(*
 *   Default values of instance data items
 *)
CONST
  DEFAULT_DURATION     = 300;
  DEFAULT_HITONE       = 400;
  DEFAULT_LOTONE       = 400;
  DEFAULT_SPEED        = 50;
  DEFAULT_BRAKEFLAG    = FALSE;

(*
 *   Help IDs
 *
 *   NOTE:  At this time, no source file is provided to build help for this
 *   sample.  A pre-built help file, CAR.HLP, is provided containing the
 *   following help ids.
 *)
CONST
  ID_HELP_DEFAULT       = 256;
  ID_HELP_DASHBOARD     = 257;
  ID_HELP_OPENCAR       = 258;
  ID_HELP_HORNBEEP      = 259;
  ID_HELP_BEEPHORN      = 260;

(*
 *   Indexes into CARDETAILS information. (arrays)
 *)
CONST
  INDEX_MAKE            = 0;
  INDEX_MODEL           = 1;
  INDEX_COLOR           = 2;
  INDEX_SALE_DATE       = 3;
  INDEX_PRICE           = 4;

(*
 *   These defines are used as directional multipliers against x,y position.
 *)
CONST
  CAR_LEFT      = -1;
  CAR_RIGHT     =  1;
  CAR_DOWN      = -1;
  CAR_UP        =  1;

CONST
  ICON_WIDTH    = 32;      (* pels *)
  ICON_HEIGHT   = 32;      (* pels *)


(*
 *   CARDETAILS:  Structure used for details view
 *)
TYPE
  CARDETAILS    = RECORD
    pszMake       : OS2DEF.PSZ;      (* Manufacturer      *)
    pszModel      : OS2DEF.PSZ;      (* Model             *)
    pszColor      : OS2DEF.PSZ;      (* Color of car      *)
    cdateSale     : WIN.CDATE;       (* Date of sale      *)
    ulPrice       : LONGCARD;        (* Price in dollars  *)
                  END;
  PCARDETAILS   = POINTER TO CARDETAILS;

(*
 *   The following structures will be used to store window specific data
 *   and a pointer to the object that created the window/dialog.
 *
 *   They're allocated when the Client window is created.  This
 *   allows us to pass the *somSelf pointer and use it in our
 *   window and dialog procedures (the system only passes this
 *   pointer to methods).
 *)
TYPE
  WINDOWDATA    = RECORD
    cb            : CARDINAL;          (* size of this structure *)
    somSelf       : PCar;              (* pointer to this instance *)
    UseItem       : WPOBJECT.USEITEM;  (* global class usage information *)
    ViewItem      : WPOBJECT.VIEWITEM; (* global class view information *)
    x             : LONGINT;           (* x position of car in open view *)
    y             : LONGINT;           (* y position of car in open view *)
    xDir          : LONGINT;           (* x direction (CAR_LEFT or CAR_RIGHT) *)
    yDir          : LONGINT;           (* y direction (CAR_UP or CAR_DOWN) *)
                  END;
  PWINDOWDATA   = POINTER TO WINDOWDATA;

(*
 *   Window data for the Dashboard dialog (settings page)
 *)
TYPE
  DASHDLGDATA   = RECORD
    cb            : CARDINAL;   (* size of this structure           *)
    somSelf       : PCar;       (* pointer to this instance         *)
    PrevBrakes    : BOOLEAN;    (* indicates if car is stopped      *)
    PrevSpeed     : LONGCARD;   (* Speed car moves across screen    *)
                  END;
  PDASHDLGDATA  = POINTER TO DASHDLGDATA;

(*
 *   Window data for the Horn dialog (settings page)
 *)
TYPE
  HORNDLGDATA   = RECORD
    cb            : CARDINAL;   (* size of this structure *)
    somSelf       : PCar;       (* pointer to this instance         *)
    PrevDuration  : LONGCARD;   (* Duration of the car's horn beep  *)
    PrevHighTone  : LONGCARD;   (* The high note of the car beep    *)
    PrevLowTone   : LONGCARD;   (* The low note of the car beep     *)
                  END;
  PHORNDLGDATA  = POINTER TO HORNDLGDATA;

(*
 *   Private Debug helpers go here.  They can be enabled/disabled by
 *   defining DEBUG e.g. as a boolean identifier.
 *)
CONST DEBUG = TRUE;
PROCEDURE DebugBox
(
  title        : ARRAY OF CHAR;
  text         : ARRAY OF CHAR
);
BEGIN
  (*$IFDEF DEBUG*)
    WIN.WinMessageBox
    (
      WIN.HWND_DESKTOP,
      WIN.HWND_DESKTOP,
      text,
      title,
      20,
      WIN.MB_OK OR WIN.MB_INFORMATION OR WIN.MB_MOVEABLE
    );
  (*$ELSE*)
    (* do nothing if not debugging *)
  (*$ENDIF*)
END DebugBox;

VAR
  (*.....*)

**********)

(*************************************************************************
   Implementation header for the new SOM class 'Car'
   (constants, types and variables)
**************************************************************************)

CONST
(************
  Car_MaxNoMethods = 14;        (* number of new methods *)
************)
  Car_MaxNoMethods = 0;         (* number of new methods *)
  CarDebug         = TRUE;      (* enable/disable method debugging *)

(*
 * Temporary class data structure used only in class creation
 *)
VAR
  CartempClassData        : SOM.somClassDataStructure;

(*
 * Internal instance data fields
 *)
TYPE
  CarData          = RECORD
(********
    BrakeFlag        : BOOLEAN;
    duration         : LONGCARD;
    HighTone         : LONGCARD;
    LowTone          : LONGCARD;
    speed            : LONGCARD;
*********)
                     END;
  PCarData         = POINTER TO CarData;

(********

(*
 *   CarGetData function, gives access to the instance data, if any
 *)
PROCEDURE CarGetData( Self : PCar ) : PCarData;
BEGIN
  RETURN SOM.somDataResolve( Self, CarCClassData.instanceDataToken );
END CarGetData;

(*
 *  SOM specific identifiers for all
 *  the new and also the overridden methods
 *)
VAR
  somId_QueryBrakes              : SOM.somId;
  somId_SetBrakes                : SOM.somId;
  somId_QueryDuration            : SOM.somId;
  somId_SetDuration              : SOM.somId;
  somId_QueryHighTone            : SOM.somId;
  somId_SetHighTone              : SOM.somId;
  somId_QueryLowTone             : SOM.somId;
  somId_SetLowTone               : SOM.somId;
  somId_QuerySpeed               : SOM.somId;
  somId_SetSpeed                 : SOM.somId;
  somId_BeepHorn                 : SOM.somId;
  somId_AddDashboardPage         : SOM.somId;
  somId_AddHornBeepPage          : SOM.somId;
  somId_TrapTest                 : SOM.somId;
  somId_wpInitData               : SOM.somId;
  somId_wpUnInitData             : SOM.somId;
  somId_wpSaveState              : SOM.somId;
  somId_wpRestoreState           : SOM.somId;
  somId_wpAddSettingsPages       : SOM.somId;
  somId_wpFilterPopupMenu        : SOM.somId;
  somId_wpModifyPopupMenu        : SOM.somId;
  somId_wpMenuItemSelected       : SOM.somId;
  somId_wpMenuItemHelpSelected   : SOM.somId;
  somId_wpQueryDetailsData       : SOM.somId;
  somId_wpOpen                   : SOM.somId;
  somId_wpSetup                  : SOM.somId;

**********)


(*************************************************************************
  apply- and redispatch- stubs for new methods introduced by class 'Car'
*************************************************************************)


(***********

PROCEDURE somAP_QueryBrakes
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
)           : BOOLEAN;
BEGIN
  RETURN somSelf^.QueryBrakes( );
END somAP_QueryBrakes;

PROCEDURE somRD_QueryBrakes
(
  somSelf      : PCar
)              : BOOLEAN;
VAR
  retBuffer    : BOOLEAN;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_QueryBrakes, args^ );
  RETURN retBuffer;
END somRD_QueryBrakes;



PROCEDURE somAP_SetBrakes
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
);
VAR
  fBrake    : BOOLEAN;
BEGIN
  fBrake    := SHORT( SHORT( args[0] ) );
  somSelf^.SetBrakes( fBrake );
END somAP_SetBrakes;

(*
 *  redispatch stub for a new proper procedure method
 *)
PROCEDURE somRD_SetBrakes
(
  somSelf      : PCar;
  fBrake       : BOOLEAN
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_SetBrakes, args^ );
END somRD_SetBrakes;



PROCEDURE somAP_QueryDuration
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
)           : LONGCARD;
BEGIN
  RETURN somSelf^.QueryDuration( );
END somAP_QueryDuration;

PROCEDURE somRD_QueryDuration
(
  somSelf      : PCar
)              : LONGCARD;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_QueryDuration, args^ );
  RETURN retBuffer;
END somRD_QueryDuration;



PROCEDURE somAP_SetDuration
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
);
VAR
  ulDuration : LONGCARD;
BEGIN
  ulDuration := args[0];
  somSelf^.SetDuration( ulDuration );
END somAP_SetDuration;

(*
 *  redispatch stub for a new proper procedure method
 *)
PROCEDURE somRD_SetDuration
(
  somSelf      : PCar;
  ulDuration   : LONGCARD
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_SetDuration, args^ );
END somRD_SetDuration;



PROCEDURE somAP_QueryHighTone
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
)           : LONGCARD;
BEGIN
  RETURN somSelf^.QueryHighTone( );
END somAP_QueryHighTone;

PROCEDURE somRD_QueryHighTone
(
  somSelf      : PCar
)              : LONGCARD;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_QueryHighTone, args^ );
  RETURN retBuffer;
END somRD_QueryHighTone;



PROCEDURE somAP_SetHighTone
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
);
VAR
  ulTone    : LONGCARD;
BEGIN
  ulTone    := args[0];
  somSelf^.SetHighTone( ulTone );
END somAP_SetHighTone;

(*
 *  redispatch stub for a new proper procedure method
 *)
PROCEDURE somRD_SetHighTone
(
  somSelf      : PCar;
  ulTone       : LONGCARD
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_SetHighTone, args^ );
END somRD_SetHighTone;



PROCEDURE somAP_QueryLowTone
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
)           : LONGCARD;
BEGIN
  RETURN somSelf^.QueryLowTone( );
END somAP_QueryLowTone;

PROCEDURE somRD_QueryLowTone
(
  somSelf      : PCar
)              : LONGCARD;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_QueryLowTone, args^ );
  RETURN retBuffer;
END somRD_QueryLowTone;



PROCEDURE somAP_SetLowTone
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
);
VAR
  ulTone    : LONGCARD;
BEGIN
  ulTone    := args[0];
  somSelf^.SetLowTone( ulTone );
END somAP_SetLowTone;

(*
 *  redispatch stub for a new proper procedure method
 *)
PROCEDURE somRD_SetLowTone
(
  somSelf      : PCar;
  ulTone       : LONGCARD
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_SetLowTone, args^ );
END somRD_SetLowTone;



PROCEDURE somAP_QuerySpeed
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
)           : LONGCARD;
BEGIN
  RETURN somSelf^.QuerySpeed( );
END somAP_QuerySpeed;

PROCEDURE somRD_QuerySpeed
(
  somSelf      : PCar
)              : LONGCARD;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_QuerySpeed, args^ );
  RETURN retBuffer;
END somRD_QuerySpeed;



PROCEDURE somAP_SetSpeed
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
);
VAR
  ulSpeed    : LONGCARD;
BEGIN
  ulSpeed    := args[0];
  somSelf^.SetSpeed( ulSpeed );
END somAP_SetSpeed;

(*
 *  redispatch stub for a new proper procedure method
 *)
PROCEDURE somRD_SetSpeed
(
  somSelf      : PCar;
  ulSpeed      : LONGCARD
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_SetSpeed, args^ );
END somRD_SetSpeed;



PROCEDURE somAP_BeepHorn
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
)           : BOOLEAN;
BEGIN
  RETURN somSelf^.BeepHorn( );
END somAP_BeepHorn;

PROCEDURE somRD_BeepHorn
(
  somSelf      : PCar
)              : BOOLEAN;
VAR
  retBuffer    : BOOLEAN;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_BeepHorn, args^ );
  RETURN retBuffer;
END somRD_BeepHorn;



PROCEDURE somAP_AddDashboardPage
(
  somSelf      : PCar;
  id           : SOM.somId;
  desc         : SOM.somId;
  VAR args     : ARRAY OF SOM.DWORD
)              : LONGCARD;
VAR
  hwndNotebook : OS2DEF.HWND;
BEGIN
  hwndNotebook := args[0];
  RETURN somSelf^.AddDashboardPage( hwndNotebook );
END somAP_AddDashboardPage;

PROCEDURE somRD_AddDashboardPage
(
  somSelf      : PCar;
  hwndNotebook : OS2DEF.HWND
)              : LONGCARD;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_AddDashboardPage, args^ );
  RETURN retBuffer;
END somRD_AddDashboardPage;



PROCEDURE somAP_AddHornBeepPage
(
  somSelf      : PCar;
  id           : SOM.somId;
  desc         : SOM.somId;
  VAR args     : ARRAY OF SOM.DWORD
)              : LONGCARD;
VAR
  hwndNotebook : OS2DEF.HWND;
BEGIN
  hwndNotebook := args[0];
  RETURN somSelf^.AddHornBeepPage( hwndNotebook );
END somAP_AddHornBeepPage;

PROCEDURE somRD_AddHornBeepPage
(
  somSelf      : PCar;
  hwndNotebook : OS2DEF.HWND
)              : LONGCARD;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_AddHornBeepPage, args^ );
  RETURN retBuffer;
END somRD_AddHornBeepPage;



PROCEDURE somAP_TrapTest
(
  somSelf   : PCar;
  id        : SOM.somId;
  desc      : SOM.somId;
  VAR args  : ARRAY OF SOM.DWORD
);
BEGIN
  somSelf^.TrapTest();
END somAP_TrapTest;

PROCEDURE somRD_TrapTest
(
  somSelf      : PCar
);
VAR
  retBuffer    : SOM.somToken;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_TrapTest, args^ );
END somRD_TrapTest;

**********)



(*************************************************************************
  Forward declared procedures for all newly introduced private methods
  and for privately overridden methods.
*************************************************************************)

(*********

PROCEDURE( Self : PCar ) wpInitData( );
FORWARD;

PROCEDURE( Self : PCar ) wpUnInitData( );
FORWARD;

PROCEDURE( Self : PCar ) wpSaveState( ) : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpRestoreState
(
  ulReserved    : LONGCARD
)               : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpAddSettingsPages
(
  hwndNotebook  : OS2DEF.HWND
)               : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpFilterPopupMenu
(
  ulFlags       : LONGCARD;
  hwndCnr       : OS2DEF.HWND;
  fMultiSelect  : BOOLEAN
)               : LONGCARD;
FORWARD;

PROCEDURE( Self : PCar ) wpModifyPopupMenu
(
  hwndMenu      : OS2DEF.HWND;
  hwndCnr       : OS2DEF.HWND;
  iPosition     : LONGCARD
)               : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpMenuItemSelected
(
  hwndFrame     : OS2DEF.HWND;
  ulMenuId      : LONGCARD
)               : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpMenuItemHelpSelected
(
  MenuId        : LONGCARD
)               : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpQueryDetailsData
(
  ppDetailsData : OS2DEF.PPVOID;
  VAR cp        : LONGCARD
)               : BOOLEAN;
FORWARD;

PROCEDURE( Self : PCar ) wpOpen
(
  hwndCnr       : OS2DEF.HWND;
  ulView        : LONGCARD;
  param         : LONGCARD
)               : OS2DEF.HWND;
FORWARD;

PROCEDURE( Self : PCar ) wpSetup
(
  szSetupString : ARRAY OF CHAR
)               : BOOLEAN;
FORWARD;

*********)



(*************************************************************************
    SOM-class creation procedures.
    Only the CarNewClass() procedure is publicly
    available for client programs.
**************************************************************************)

(*
 * class initialization
 *)
PROCEDURE CarsomInitializeClass;
VAR
  m  : Car;     (* needed for static method references *)
  c  : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := CartempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add the new methods, including apply and redispatch stubs,
     to the new SOM class
  *)
(*******
  CarClassData.QueryBrakes := c^.somAddStaticMethod
  ( somId_QueryBrakes, md, m.QueryBrakes, somRD_QueryBrakes, somAP_QueryBrakes );
  CarClassData.SetBrakes := c^.somAddStaticMethod
  ( somId_SetBrakes, md, m.SetBrakes, somRD_SetBrakes, somAP_SetBrakes );
  CarClassData.QueryDuration := c^.somAddStaticMethod
  ( somId_QueryDuration, md, m.QueryDuration, somRD_QueryDuration, somAP_QueryDuration );
  CarClassData.SetDuration := c^.somAddStaticMethod
  ( somId_SetDuration, md, m.SetDuration, somRD_SetDuration, somAP_SetDuration );
  CarClassData.QueryHighTone := c^.somAddStaticMethod
  ( somId_QueryHighTone, md, m.QueryHighTone, somRD_QueryHighTone, somAP_QueryHighTone );
  CarClassData.SetHighTone := c^.somAddStaticMethod
  ( somId_SetHighTone, md, m.SetHighTone, somRD_SetHighTone, somAP_SetHighTone );
  CarClassData.QueryLowTone := c^.somAddStaticMethod
  ( somId_QueryLowTone, md, m.QueryLowTone, somRD_QueryLowTone, somAP_QueryLowTone );
  CarClassData.SetLowTone := c^.somAddStaticMethod
  ( somId_SetLowTone, md, m.SetLowTone, somRD_SetLowTone, somAP_SetLowTone );
  CarClassData.QuerySpeed := c^.somAddStaticMethod
  ( somId_QuerySpeed, md, m.QuerySpeed, somRD_QuerySpeed, somAP_QuerySpeed );
  CarClassData.SetSpeed := c^.somAddStaticMethod
  ( somId_SetSpeed, md, m.SetSpeed, somRD_SetSpeed, somAP_SetSpeed );
  CarClassData.BeepHorn := c^.somAddStaticMethod
  ( somId_BeepHorn, md, m.BeepHorn, somRD_BeepHorn, somAP_BeepHorn );
  CarClassData.AddDashboardPage := c^.somAddStaticMethod
  ( somId_AddDashboardPage, md, m.AddDashboardPage, somRD_AddDashboardPage, somAP_AddDashboardPage );
  CarClassData.AddHornBeepPage := c^.somAddStaticMethod
  ( somId_AddHornBeepPage, md, m.AddHornBeepPage, somRD_AddHornBeepPage, somAP_AddHornBeepPage );
  CarClassData.TrapTest := c^.somAddStaticMethod
  ( somId_TrapTest, md, m.TrapTest, somRD_TrapTest, somAP_TrapTest );
********)

  (* Override inherited methods, if any *)
(*******
  c^.somOverrideSMethod( somId_wpInitData, m.wpInitData );
  c^.somOverrideSMethod( somId_wpUnInitData, m.wpUnInitData );
  c^.somOverrideSMethod( somId_wpSaveState, m.wpSaveState );
  c^.somOverrideSMethod( somId_wpRestoreState, m.wpRestoreState );
  c^.somOverrideSMethod( somId_wpAddSettingsPages, m.wpAddSettingsPages );
  c^.somOverrideSMethod( somId_wpFilterPopupMenu, m.wpFilterPopupMenu );
  c^.somOverrideSMethod( somId_wpModifyPopupMenu, m.wpModifyPopupMenu );
  c^.somOverrideSMethod( somId_wpMenuItemSelected, m.wpMenuItemSelected );
  c^.somOverrideSMethod( somId_wpQueryDetailsData, m.wpQueryDetailsData );
  c^.somOverrideSMethod( somId_wpOpen, m.wpOpen );
  c^.somOverrideSMethod( somId_wpSetup, m.wpSetup );
*******)

END CarsomInitializeClass;

(*
 *  class creation procedure
 *)
PROCEDURE CarsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  CartempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "Car",
    pClsObj,
    SIZE( CarData ),
    Car_MaxNoMethods,
    Car_MajorVersion,
    Car_MinorVersion
  );
  CarCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  CarsomInitializeClass();
  CarCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( CarClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  CarClassData.classObject := classObject;
END CarsomCreateClass;

(*
 *   public NewClass-procedure
 *)
PROCEDURE CarNewClass
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
  (*
  SYSTEM.INLINE
  (
    MOV EAX, 55555555H
    INT 3                 ; hard break for debugging
  );
  *)

  (* Check the version numbers *)
  IF ((majorVersion <> 0) AND (majorVersion <> Car_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > Car_MinorVersion)) THEN
    somWriteString( "CarNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF CarClassData.classObject <> NIL THEN
    RETURN CarClassData.classObject;
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
  pClsObj := WPDATAF.WPDataFileNewClass( 0, 0 ); (* static *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "WPDataFile" ), 0, 0 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Get the metaclass object. *)
  (* If explicit metaclass, get it from there *)
    mClsObj := M_CarNewClass( 0, 0 );       (* static*)
    mClsObj := SOM.SOMClassMgrObject^.somFindClass
    ( SOM.somIdFromString( "M_Car" ), 0, 0 );
  (* else use parent's metaclass:
    mClsObj := pClsObj^.mtab^.classObject;
  *)
  IF mClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoMetaClass, currentFile(), line );
  END;

  SOM.somConstructClass
  ( CarsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( CartempClassData ) );

  RETURN CarClassData.classObject;
END CarNewClass;




(*************************************************************************
   Implementation header for the new SOM metaclass 'M_Car'
   (constants, types and variables)
**************************************************************************)

CONST
(************
  M_Car_MaxNoMethods = 1;         (* number of new methods *)
************)
  M_Car_MaxNoMethods = 0;         (* number of new methods *)
  M_CarDebug         = TRUE;      (* enable/disable method debugging *)

(*
 * Temporary class data structure used only in class creation
 *)
VAR
  M_CartempClassData        : SOM.somClassDataStructure;

(*
 * Internal instance data fields
 *)
TYPE
  M_CarData          = RECORD
                       END;
  PM_CarData         = POINTER TO M_CarData;

(******
(*
 *   M_CarGetData function, gives access to the instance data, if any
 *)
PROCEDURE M_CarGetData( Self : PM_Car ) : PM_CarData;
BEGIN
  RETURN NIL; (* no instance data fields to be dealt with *)
END M_CarGetData;
******)

(*
 *  SOM specific identifiers for all
 *  the new and also the overridden methods
 *)
VAR
(********
  somId_clsQueryModuleHandle     : SOM.somId;
  somId_wpclsInitData            : SOM.somId;
  somId_wpclsUnInitData          : SOM.somId;
  somId_wpclsQueryTitle          : SOM.somId;
  somId_wpclsQueryIconData       : SOM.somId;
  somId_wpclsQueryDefaultHelp    : SOM.somId;
  somId_wpclsQueryDefaultView    : SOM.somId;
  somId_wpclsQueryDetailsInfo    : SOM.somId;
  somId_wpclsQueryDetails        : SOM.somId;
  somId_wpclsQueryInstanceFilter : SOM.somId;
  somId_wpclsQueryStyle          : SOM.somId;
*********)


(*************************************************************************
  apply- and redispatch- stubs for new methods introduced
  by metaclass 'M_Car'
*************************************************************************)

(*********

PROCEDURE somAP_clsQueryModuleHandle
(
  somSelf      : PM_Car;
  id           : SOM.somId;
  desc         : SOM.somId;
  VAR args     : ARRAY OF SOM.DWORD
)              : OS2DEF.HMODULE;
BEGIN
  RETURN somSelf^.clsQueryModuleHandle( );
END somAP_clsQueryModuleHandle;

PROCEDURE somRD_clsQueryModuleHandle
(
  somSelf      : PM_Car
)              : OS2DEF.HMODULE;
VAR
  retBuffer    : LONGCARD;
  retValue     : SOM.somToken;
  args         : SOM.ADDRESS;
  dispatched   : BOOLEAN;
BEGIN
  retValue := ADR( retBuffer );
  args := ADR( somSelf ) + SIZE( somSelf );
  dispatched := somSelf^.somDispatch( retValue, somId_clsQueryModuleHandle, args^ );
  RETURN retBuffer;
END somRD_clsQueryModuleHandle;

*********)


(*************************************************************************
  Forward declared procedures for all newly introduced private methods
  and for privately overridden methods.
*************************************************************************)

(********

PROCEDURE( Self : PM_Car ) wpclsInitData( );
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsUnInitData( );
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryTitle( ) : OS2DEF.PSZ;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryIconData
(
  pIconInfo     : OS2DEF.PICONINFO
)               : LONGCARD;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryDefaultHelp
(
  VAR HelpPanelId   : LONGCARD;
  VAR szHelpLibrary : ARRAY OF CHAR
)                   : BOOLEAN;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryDefaultView( ) : LONGCARD;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryDetailsInfo
(
  ppClassFieldInfo : WPOBJECT.PPCLASSFIELDINFO;
  pSize            : WPOBJECT.PLONGCARD
)                  : LONGCARD;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryDetails( ) : WPOBJECT.PCLASSDETAILS;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryInstanceFilter( ) : OS2DEF.PSZ;
FORWARD;

PROCEDURE( Self : PM_Car ) wpclsQueryStyle( ) : LONGCARD;
FORWARD;

*********)


(*************************************************************************
    SOM-class creation procedures.
    Only the M_CarNewClass() procedure is publicly
    available for client programs.
**************************************************************************)


(*
 * class initialization
 *)
PROCEDURE M_CarsomInitializeClass;
VAR
  m  : M_Car;     (* needed for static method references *)
  c  : SOM.PSOMClass;
  md : SOM.somId;
BEGIN

  c := M_CartempClassData.classObject;
  md := SOM.somIdFromString( "----" );

  (* Add the new methods, including apply and redispatch stubs,
     to the new SOM class
  *)
(******
  M_CarClassData.clsQueryModuleHandle := c^.somAddStaticMethod
  ( somId_clsQueryModuleHandle, md, m.clsQueryModuleHandle, somRD_clsQueryModuleHandle, somAP_clsQueryModuleHandle );
******)

  (* Override inherited methods, if any *)
(******
  c^.somOverrideSMethod( somId_wpclsInitData, m.wpclsInitData );
  c^.somOverrideSMethod( somId_wpclsUnInitData, m.wpclsUnInitData );
  c^.somOverrideSMethod( somId_wpclsQueryTitle, m.wpclsQueryTitle );
  c^.somOverrideSMethod( somId_wpclsQueryIconData, m.wpclsQueryIconData );
  c^.somOverrideSMethod( somId_wpclsQueryDefaultHelp, m.wpclsQueryDefaultHelp );
  c^.somOverrideSMethod( somId_wpclsQueryDefaultView, m.wpclsQueryDefaultView );
  c^.somOverrideSMethod( somId_wpclsQueryDetailsInfo, m.wpclsQueryDetailsInfo );
  c^.somOverrideSMethod( somId_wpclsQueryDetails, m.wpclsQueryDetails );
  c^.somOverrideSMethod( somId_wpclsQueryInstanceFilter, m.wpclsQueryInstanceFilter );
  c^.somOverrideSMethod( somId_wpclsQueryStyle, m.wpclsQueryStyle );
*******)

END M_CarsomInitializeClass;


(*
 *  class creation procedure
 *)
PROCEDURE M_CarsomCreateClass
(
  pClsObj    : SOM.PSOMClass;
  mClsObj    : SOM.PSOMClass
);
VAR
  classObject : SOM.PSOMClass;
BEGIN
  classObject := mClsObj^.somNew();
  M_CartempClassData.classObject := classObject;
  classObject^.somInitClass
  (
    "M_Car",
    pClsObj,
    SIZE( M_CarData ),
    M_Car_MaxNoMethods,
    M_Car_MajorVersion,
    M_Car_MinorVersion
  );
  M_CarCClassData.instanceDataToken := classObject^.somGetInstanceToken();
  M_CarsomInitializeClass();
  M_CarCClassData.parentMtab := classObject^.somGetPClsMtab();
  classObject^.somSetClassData( SYSTEM.ADR( M_CarClassData ) );
  classObject^.somClassReady();
  (* make newly created class object visible *)
  M_CarClassData.classObject := classObject;
END M_CarsomCreateClass;


(*
 *   public NewClass-procedure
 *)
PROCEDURE M_CarNewClass
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
  (*
  SYSTEM.INLINE
  (
    MOV EAX, 66666666H
    INT 3                   ; hard break for debugging
  );
  *)
  (* Check the version numbers *)
  IF ((majorVersion <> 0) AND (majorVersion <> M_Car_MajorVersion)) OR
     ((minorVersion <> 0) AND (minorVersion > M_Car_MinorVersion)) THEN
    somWriteString( "M_CarNewClass: Error, bad version numbers." );
    somWriteLn();
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_BadVersion, currentFile(), line );
  END;

  (* Don't do anything if class object is already created. *)
  IF M_CarClassData.classObject <> NIL THEN
    RETURN M_CarClassData.classObject;
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
  pClsObj := WPDATAF.M_WPDataFileNewClass( 0, 0 ); (* static *)
  pClsObj := SOM.SOMClassMgrObject^.somFindClass
  ( SOM.somIdFromString( "M_WPDataFile" ), 0, 0 );
  IF pClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoParentClass, currentFile(), line );
  END;

  (* Get the metaclass object. *)
  (* Use parent's metaclass: *)
  mClsObj := pClsObj^.mtab^.classObject;
  IF mClsObj = NIL THEN
    b := Conversions.StrToLongCard( currentLine(), line );
    SOM.SOMError( SOM.SOMERROR_NoMetaClass, currentFile(), line );
  END;

  SOM.somConstructClass
  ( M_CarsomCreateClass, pClsObj, mClsObj, SYSTEM.ADR( M_CartempClassData ) );

  RETURN M_CarClassData.classObject;
END M_CarNewClass;




(**************************************************************************
    Non-Method forward procedures.

    They are fully specified later, after the method declarations,
    but are already introduced here, so that they can be referenced
    by the methods.

    Remember, this is a quick one-pass Modula-2 compiler!
***************************************************************************)

(*******

PROCEDURE CarInit( somSelf : PCar ) : OS2DEF.HWND;
FORWARD;

PROCEDURE DashBoardDlgProc
(
  hwndDlg   : OS2DEF.HWND;
  msg       : LONGCARD;
  mp1       : WIN.MPARAM;
  mp2       : WIN.MPARAM
)           : WIN.MRESULT;
FORWARD;

PROCEDURE HornBeepDlgProc
(
  hwndDlg   : OS2DEF.HWND;
  msg       : LONGCARD;
  mp1       : WIN.MPARAM;
  mp2       : WIN.MPARAM
)           : WIN.MRESULT;
FORWARD;

PROCEDURE CarWndProc
(
  hwnd      : OS2DEF.HWND;
  msg       : LONGCARD;
  mp1       : WIN.MPARAM;
  mp2       : WIN.MPARAM
)           : WIN.MRESULT;
FORWARD;

*******)

(********
PROCEDURE _Exception
(
  VAR arg           : DOSEXCEPTIONS.EXCEPTIONREPORTRECORD;
  VAR RegisRecord   : DOSEXCEPTIONS.EXCEPTIONREGISTRATIONRECORD;
  VAR ContextRecord : DOSEXCEPTIONS.CONTEXTRECORD;
  pvSpare           : OS2DEF.PVOID
)                   : LONGCARD;
FORWARD;
*******)


(*************************************************************************

              GLOBAL/STATIC (NON-INSTANCE) DATA SECTION

    This data shouldn't be changed by instance methods or it will
    effect all instances!  Any variables that are specific (unique
    values) for each instance of this object should be declared as
  instance data or dynamically allocated and stored as window data.

      This global data should be declared as class instance data
    if it will change after initialization.  In this case, it will
                  be accessed through class methods.

*************************************************************************)

(*********

TYPE
  CARCLASSTITLE       = ARRAY [0..OS2DEF.CCHMAXPATH-1] OF CHAR;

CONST
  LF                  = CHR( 10 );
  szCarWindowClass    : ARRAY OF CHAR = (*"CARSAMPPP";*) "CARSAMP";
  szCarInstanceFilter : ARRAY OF CHAR = "*.CAR";
  szHelpLib           : ARRAY OF CHAR = "car.hlp";
  szCarClassTitle     : CARCLASSTITLE = "";
  hmod                : OS2DEF.HMODULE = OS2DEF.NULLHANDLE;
  szDefaultText       : ARRAY OF CHAR =
  [
                                       LF,
    "                               ", LF,
    "     Bill of Sale              ", LF,
    "   =========================   ", LF,
    "                               ", LF,
    "   Make........ Toyota         ", LF,
    "                               ", LF,
    "   Model....... Camry          ", LF,
    "                               ", LF,
    "   Color....... Blue           ", LF,
    "                               ", LF,
    "   Sale Date... 3/31/92        ", LF,
    "                               ", LF,
    "   Price....... 14,995         ", LF,
    0C
  ];



(********
(*
 *   Globals required for Exception handling
 *
 *)
CONST
  szTrapMessage : ARRAY OF CHAR =
  [
    "A Memory Access Violation occurred.  The Car ",
    "sample's exception handler has transferred ",
    "control back to the cleanup code in the method ",
    "where the exception occurred.",
    LF
  ];
*******)



(*
 *   Statics required for FIELDINFO structures needed for DETAILS view are
 *   handled in the three functions:
 *     M_Car.wpclsInitData,
 *     Car.wpQueryDetailsData,
 *     M_Car.wpclsQueryDetailsInfo
 *
 *)
CONST
  NUM_CAR_FIELDS = 5;
VAR
  fieldinfo      : ARRAY [0..NUM_CAR_FIELDS-1] OF WPOBJECT.CLASSFIELDINFO;


TYPE
  CARCOLTITLES    = ARRAY [0..NUM_CAR_FIELDS-1] OF ARRAY [0..9] OF CHAR;
CONST
  aszCarColTitles : CARCOLTITLES =
  [
    [ "Make"      ],   (* details column 1 *)
    [ "Model"     ],   (* details column 2 *)
    [ "Color"     ],   (* details column 3 *)
    [ "Sale date" ],   (* details column 4 *)
    [ "Price ($)" ]    (* details column 5 *)
  ];

********)




(*************************************************************************

                       INSTANCE METHODS SECTION

              Do not put any code in this section unless
                   it is an object INSTANCE method

*************************************************************************)


(*********

(*
 *
 *  METHOD: QueryBrakes                                    ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Query state of car's brakes
 *
 *  RETURN:
 *
 *    TRUE	     Brake is on
 *    FALSE	     Brake is off
 *
 *)

PROCEDURE( Self : PCar ) QueryBrakes( ) : BOOLEAN;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "QueryBrakes", currentFile(), currentLine() );
  END;
  RETURN somThis^.BrakeFlag;
END QueryBrakes;



(*
 *
 *  METHOD: SetBrakes                                      ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Put on the brakes
 *
 *  RETURN:
 *
 *    None
 *)

PROCEDURE( Self : PCar ) SetBrakes
(
  fBrake        : BOOLEAN
);
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "SetBrakes", currentFile(), currentLine() );
  END;
  somThis^.BrakeFlag := fBrake;
END SetBrakes;



(*
 *
 *  METHOD: QueryDuration                                  ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Query Duration of horn beep
 *
 *  RETURN:
 *
 *    LONGCARD    Duration of beep
 *
 *)

PROCEDURE( Self : PCar ) QueryDuration( ) : LONGCARD;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "QueryDuration", currentFile(), currentLine() );
  END;
  RETURN somThis^.duration;
END QueryDuration;




(*
 *
 *  METHOD: SetDuration                                    ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Set Duration of horn beep
 *
 *  RETURN:
 *
 *    None
 *)

PROCEDURE( Self : PCar ) SetDuration
(
  ulDuration    : LONGCARD
);
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "SetDuration", currentFile(), currentLine() );
  END;
  somThis^.duration := ulDuration;
END SetDuration;




(*
 *
 *  METHOD: QueryHighTone                                  ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Query HighTone of horn
 *
 *  RETURN:
 *
 *    LONGCARD  High frequency of horn
 *
 *)

PROCEDURE( Self : PCar ) QueryHighTone( ) : LONGCARD;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "QueryHighTone", currentFile(), currentLine() );
  END;
  RETURN somThis^.HighTone;
END QueryHighTone;




(*
 *
 *  METHOD: SetHighTone                                    ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Set HighTone of horn
 *
 *  RETURN:
 *
 *    None
 *)

PROCEDURE( Self : PCar ) SetHighTone
(
  ulTone        : LONGCARD
);
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "SetHighTone", currentFile(), currentLine() );
  END;
  somThis^.HighTone := ulTone;
END SetHighTone;




(*
 *
 *  METHOD: QueryLowTone                                   ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Query LowTone of horn
 *
 *  RETURN:
 *
 *    LONGCARD   Low frequency of horn
 *
 *)

PROCEDURE( Self : PCar ) QueryLowTone( ) : LONGCARD;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "QueryLowTone", currentFile(), currentLine() );
  END;
  RETURN somThis^.LowTone;
END QueryLowTone;




(*
 *
 *  METHOD: SetLowTone                                     ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Set LowTone of horn
 *
 *  RETURN:
 *
 *    None
 *)

PROCEDURE( Self : PCar ) SetLowTone
(
  ulTone : LONGCARD
);
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "SetLowTone", currentFile(), currentLine() );
  END;
  somThis^.LowTone := ulTone;
END SetLowTone;




(*
 *
 *  METHOD: QuerySpeed                                     ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Query Speed of car
 *
 *  RETURN:
 *
 *    LONGCARD  car speed
 *
 *)

PROCEDURE( Self : PCar ) QuerySpeed( ) : LONGCARD;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "QuerySpeed", currentFile(), currentLine() );
  END;
  RETURN somThis^.speed;
END QuerySpeed;




(*
 *
 *  METHOD: SetSpeed                                       ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Set Speed of car
 *
 *  RETURN:
 *
 *    None
 *)

PROCEDURE( Self : PCar ) SetSpeed
(
  ulSpeed       : LONGCARD
);
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "SetSpeed", currentFile(), currentLine() );
  END;
  somThis^.speed := ulSpeed;
END SetSpeed;





(*
 *
 *  METHOD: BeepHorn                                       ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Beep the car's horn.
 *
 *  RETURN:
 *
 *    TRUE	     Successful
 *    FALSE	     Unsuccessful
 *
 *)

PROCEDURE( Self : PCar ) BeepHorn( ) : BOOLEAN;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "BeepHorn", currentFile(), currentLine() );
  END;
  DOSPROCESS.DosBeep( Self^.QueryHighTone(), Self^.QueryDuration() );
  DOSPROCESS.DosSleep( 100 );
  DOSPROCESS.DosBeep( Self^.QueryLowTone(), Self^.QueryDuration() );
  RETURN TRUE;
END BeepHorn;




(*
 *
 *  METHOD: AddDashboardPage                               ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    This method adds the dashboard page to the settings notebook.
 *
 *  RETURN:
 *
 *    0              Unsuccessful
 *    ulPageId       Identifier for the inserted page
 *
 *  HOW TO OVERRIDE:
 *
 *    Method should always be overridden in order to replace or remove
 *    the dashboard page from an object which is a descendent of Car.
 *    In most cases, an override of this method will not call the parent.
 *
 *)

PROCEDURE( Self : PCar ) AddDashboardPage
(
  hwndNotebook  : OS2DEF.HWND
)               : LONGCARD;
VAR
  pageinfo      : WINWORKPLACE.PAGEINFO;
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "AddDashboardPage", currentFile(), currentLine() );
  END;

  SYSTEM.MemSet( ADR( pageinfo ), 0, SIZE( pageinfo ) );
  pageinfo.cb                 := SIZE( WINWORKPLACE.PAGEINFO );
  pageinfo.hwndPage           := OS2DEF.NULLHANDLE;
  pageinfo.usPageStyleFlags   := WIN.BKA_MAJOR;
  pageinfo.usPageInsertFlags  := WIN.BKA_FIRST;
  pageinfo.pfnwp              := DashBoardDlgProc;
  pageinfo.resid              := hmod;
  pageinfo.dlgid              := IDD_DASHBOARD;
  pageinfo.pszName            := SOMMISC.somString( "Dashboard" );
  pageinfo.pCreateParams      := Self;
  pageinfo.idDefaultHelpPanel := ID_HELP_DASHBOARD;
  pageinfo.pszHelpLibraryName := ADR( szHelpLib );

  RETURN Self^.wpInsertSettingsPage( hwndNotebook, pageinfo );

END AddDashboardPage;



(*
 *
 *  METHOD: AddHornBeepPage                                ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    This method adds the horn beep page to the settings
 *    notebook.
 *
 *  RETURN:
 *
 *    0              Unsuccessful
 *    ulPageId       Identifier for the inserted page
 *
 *  HOW TO OVERRIDE:
 *
 *    Method should always be overridden in order to replace or remove
 *    the horn beep page from an object which is a descendent of Car.
 *    In most cases, an override of this method will not call the parent.
 *
 *
 *    Methods from the WPObject class
 *
 *)

PROCEDURE( Self : PCar ) AddHornBeepPage
(
  hwndNotebook  : OS2DEF.HWND
)               : LONGCARD;
VAR
  pageinfo      : WINWORKPLACE.PAGEINFO;
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "AddHornBeepPage", currentFile(), currentLine() );
  END;

  (* Insert the settings pages for a car *)
  SYSTEM.MemSet( ADR( pageinfo ), 0, SIZE( pageinfo ) );
  pageinfo.cb                 := SIZE( WINWORKPLACE.PAGEINFO );
  pageinfo.hwndPage           := OS2DEF.NULLHANDLE;
  pageinfo.usPageStyleFlags   := WIN.BKA_MAJOR;
  pageinfo.usPageInsertFlags  := WIN.BKA_FIRST;
  pageinfo.pfnwp              := HornBeepDlgProc;
  pageinfo.resid              := hmod;
  pageinfo.dlgid              := IDD_HORNBEEP;
  pageinfo.pszName            := SOMMISC.somString( "Horn Beep" );
  pageinfo.pCreateParams      := Self;
  pageinfo.idDefaultHelpPanel := ID_HELP_HORNBEEP;
  pageinfo.pszHelpLibraryName := ADR( szHelpLib );

  RETURN Self^.wpInsertSettingsPage( hwndNotebook, pageinfo );

END AddHornBeepPage;



(*
 *
 *  OVERRIDE: wpInitData                                   ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Initialize our state variables. Allocate any extra memory that
 *    we might need.
 *
 *)

PROCEDURE( Self : PCar ) wpInitData( );
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "wpInitData", currentFile(), currentLine() );
  END;

  (*
   *   We can initialize our instance data to 0's by using the somThis
   *   pointer and the size of the CarData structure created by SOM.
   *
   *   SOM stores instance data in a data structure named by prefixing
   *   the name "Data" with the class name, in this case, "Car".
   *)
   SYSTEM.MemSet( somThis, 0, SIZE( CarData ) );

   (*
    *  And/or we can explicitly initialize our instance variables.
    *)
   somThis^.HighTone  := DEFAULT_HITONE;
   somThis^.LowTone   := DEFAULT_LOTONE;
   somThis^.duration  := DEFAULT_DURATION;
   somThis^.speed     := DEFAULT_SPEED;
   somThis^.BrakeFlag := DEFAULT_BRAKEFLAG;

   Self^.wpInitData^(); (* parent wpInitData *)

END wpInitData;



(*
 *
 *  OVERRIDE: wpUnInitData                                 ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Clear up memory that was allocated on wpInitData.
 *
 *)

PROCEDURE( Self : PCar ) wpUnInitData( );
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpUnInitData", currentFile(), currentLine() );
  END;
  Self^.wpUnInitData^(); (* parent wpUnInitData *)
END wpUnInitData;




(*
 *
 *  METHOD: wpSaveState                                    ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Save our state variables (pitch and duration).
 *
 *)

PROCEDURE( Self : PCar ) wpSaveState( ) : BOOLEAN;
VAR
  somThis       : PCarData;
  ulBrakeFlag   : LONGCARD;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "wpSaveState", currentFile(), currentLine() );
  END;
  ulBrakeFlag := LONG( LONG( SOM.BYTE( somThis^.BrakeFlag ) ) );
  Self^.wpSaveLong( szCarClassTitle, IDKEY_HITONE,    somThis^.HighTone  );
  Self^.wpSaveLong( szCarClassTitle, IDKEY_LOTONE,    somThis^.LowTone   );
  Self^.wpSaveLong( szCarClassTitle, IDKEY_DURATION,  somThis^.duration  );
  Self^.wpSaveLong( szCarClassTitle, IDKEY_SPEED,     somThis^.speed     );
  Self^.wpSaveLong( szCarClassTitle, IDKEY_BRAKEFLAG, ulBrakeFlag );
  RETURN Self^.wpSaveState^(); (* parent wpSaveState *)
END wpSaveState;




(*
 *
 *  METHOD: wpRestoreState                                 ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Retrieve our saved state variables (pitch and duration).
 *
 *)

PROCEDURE( Self : PCar ) wpRestoreState( ulReserved : LONGCARD ) : BOOLEAN;
VAR
  somThis       : PCarData;
  ulBrakeFlag   : LONGCARD;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "wpRestoreState", currentFile(), currentLine() );
  END;

  Self^.wpRestoreLong( szCarClassTitle, IDKEY_HITONE,    somThis^.HighTone  );
  Self^.wpRestoreLong( szCarClassTitle, IDKEY_LOTONE,    somThis^.LowTone   );
  Self^.wpRestoreLong( szCarClassTitle, IDKEY_DURATION,  somThis^.duration  );
  Self^.wpRestoreLong( szCarClassTitle, IDKEY_SPEED,     somThis^.speed     );
  Self^.wpRestoreLong( szCarClassTitle, IDKEY_BRAKEFLAG, ulBrakeFlag );
  somThis^.BrakeFlag := SHORT( SHORT( SOM.LONGWORD( ulBrakeFlag ) ) );
  RETURN Self^.wpRestoreState^( ulReserved ); (* parent wpRestoreState *)
END wpRestoreState;



(*
 *
 *  METHOD: wpAddSettingsPages                             ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Add our own settings page to let the user alter the pitch
 *    and duration of the car's beep.
 *
 *)

PROCEDURE( Self : PCar ) wpAddSettingsPages
(
  hwndNotebook  : OS2DEF.HWND
)               : BOOLEAN;
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpAddSettingsPages", currentFile(), currentLine() );
  END;
  IF (Self^.wpAddSettingsPages^( hwndNotebook ))  AND
     (Self^.AddHornBeepPage( hwndNotebook ) <> 0)  AND
     (Self^.AddDashboardPage( hwndNotebook ) <> 0)
  THEN
    RETURN TRUE;
  ELSE
    DebugBox( "Car.wpAddSettingsPages", " Failed to add a settings page.");
    RETURN FALSE;
  END;
END wpAddSettingsPages;



(*
 *
 *  METHOD: wpFilterPopupMenu                              ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Filter out any options from the context that don't apply.
 *
 *  HOW TO OVERRIDE:
 *
 *    No restrictions.
 *
 *)

PROCEDURE( Self : PCar ) wpFilterPopupMenu
(
  ulFlags       : LONGCARD;
  hwndCnr       : OS2DEF.HWND;
  fMultiSelect  : BOOLEAN
)               : LONGCARD;
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpFilterPopupMenu", currentFile(), currentLine() );
  END;
  (*
   *   This method allows you to filter which menus to include in the
   *   popup menu.  Note: wpclsQueryStyle is overridden to disallow
   *   linking (creating shadow) as well.
   *)
   RETURN Self^.wpFilterPopupMenu^( ulFlags, hwndCnr, fMultiSelect )
          AND NOT WPOBJECT.CTXT_LINK;
END wpFilterPopupMenu;





(*
 *
 *  METHOD: wpModifyPopupMenu                              ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Add our extra option to the context menu to beep the horn
 *
 *)

PROCEDURE( Self : PCar ) wpModifyPopupMenu
(
  hwndMenu      : OS2DEF.HWND;
  hwndCnr       : OS2DEF.HWND;
  iPosition     : LONGCARD
)               : BOOLEAN;
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpModifyPopupMenu", currentFile(), currentLine() );
  END;

  (*
   *   Put in our special "open car" submenu item under the "open" menu
   *)
  Self^.wpInsertPopupMenuItems
  ( hwndMenu, 0, hmod, ID_OPENMENU, WPOBJECT.WPMENUID_OPEN );

  (*
   *   insert a "beep horn" menu item at the end of the list.
   *)
  Self^.wpInsertPopupMenuItems
  ( hwndMenu, iPosition, hmod, ID_BEEPMENU, 0 );

  (*
   *   insert a "TRAP-D" menu item at the end of the list.
   *)
  Self^.wpInsertPopupMenuItems
  ( hwndMenu, 0, hmod, ID_TRAPMENU, 0 );

  (* call parent *)
  RETURN Self^.wpModifyPopupMenu^( hwndMenu, hwndCnr, iPosition );

END wpModifyPopupMenu;



(*
 *
 *  METHOD: wpMenuItemSelected                             ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Process input from the extra menu option that we added.
 *
 *)

PROCEDURE( Self : PCar ) wpMenuItemSelected
(
  hwndFrame     : OS2DEF.HWND;
  ulMenuId      : LONGCARD
)               : BOOLEAN;
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpMenuItemSelected", currentFile(), currentLine() );
  END;
  CASE ulMenuId OF (* Which of our menu items was selected ? *)
  | IDM_OPENCAR:
    (*
     *   We could call wpOpen here, but, if the object is already opened,
     *   the following API determines whether the object should be
     *   resurfaced, or if multiple views are desired.
     *)
    Self^.wpViewObject( OS2DEF.NULLHANDLE, OPEN_CAR, 0 );
  | IDM_BEEPHORN:
    Self^.BeepHorn();
  | IDM_TRAPCAR:
    Self^.TrapTest();
  ELSE
    RETURN Self^.wpMenuItemSelected^( hwndFrame, ulMenuId ); (* parent call *)
  END;
  RETURN TRUE; (* we processed it *)
END wpMenuItemSelected;




(*
 *
 *  METHOD: wpMenuItemHelpSelected                         ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Process input from the extra menu option that we added.
 *
 *)

PROCEDURE( Self : PCar ) wpMenuItemHelpSelected
(
  MenuId        : LONGCARD
)               : BOOLEAN;
VAR
  somThis       : PCarData;
BEGIN
  somThis := CarGetData( Self );
  IF CarDebug THEN
    somDebug( "Car", "wpMenuItemHelpSelected", currentFile(), currentLine() );
  END;
  CASE MenuId OF (* Which of our menu items was selected ? *)
  | IDM_BEEPHORN:
    RETURN Self^.wpDisplayHelp( ID_HELP_BEEPHORN, szHelpLib );
  | IDM_OPENCAR:
    (* no help written at this time *)
    (* RETURN Self^.wpDisplayHelp( ID_HELP_OPENCAR, szHelpLib ); *)
  END;
  RETURN FALSE;
END wpMenuItemHelpSelected;




(*
 *
 *  METHOD: wpQueryDetailsData                             ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Returns the car specific data for the details view of this object.
 *    Sets the pointer ppDetailsData^ to the beginning of the buffer
 *    into which the data is written.
 *
 *)

PROCEDURE( Self : PCar ) wpQueryDetailsData
(
  ppDetailsData : OS2DEF.PPVOID;
  VAR cp        : LONGCARD
)               : BOOLEAN;
VAR
(*somThis       : PCarData;*)
  pCarDetails   : PCARDETAILS;
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpQueryDetailsData", currentFile(), currentLine() );
  END;
  Self^.wpQueryDetailsData^( ppDetailsData, cp ); (* parent call *)
  IF ppDetailsData <> NIL THEN (* query data *)
    pCarDetails                  := ppDetailsData^;
    pCarDetails^.pszMake         := SOMMISC.somString( "Toyota" );
    pCarDetails^.pszModel        := SOMMISC.somString( "Camry" );
    pCarDetails^.pszColor        := SOMMISC.somString( "BLUE" );
    pCarDetails^.cdateSale.day   := 24;
    pCarDetails^.cdateSale.month := 12;
    pCarDetails^.cdateSale.year  := 91;
    pCarDetails^.ulPrice         := 14000;
    (* point to buffer location after our details data *)
    ppDetailsData^ := ppDetailsData^ + SIZE( pCarDetails^ );
  ELSE (* query size of data *)
    (* caller is querying size of buffer *)
    cp := cp + SIZE( pCarDetails^ );
  END;
  RETURN TRUE;
END wpQueryDetailsData;




(*
 *
 *   METHOD: wpOpen                                         ( ) PRIVATE
 *                                                          (X) PUBLIC
 *   DESCRIPTION:
 *
 *     Opens the car window.
 *
 *)

PROCEDURE( Self : PCar ) wpOpen
(
  hwndCnr       : OS2DEF.HWND;
  ulView        : LONGCARD;
  param         : LONGCARD
)               : OS2DEF.HWND;
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpOpen", currentFile(), currentLine() );
  END;
  CASE ulView OF
  | OPEN_CAR:
    IF NOT Self^.wpSwitchTo( ulView ) THEN
      (*
       *   Create a basic Frame and Client window for this instance.
       *)
      RETURN CarInit( Self );
    END;
    RETURN OS2DEF.NULLHANDLE; (* not specified in C-version of 'Car' ... *)
  ELSE
    RETURN Self^.wpOpen^( hwndCnr, ulView, param ); (* parent call *)
  END;
END wpOpen;



(*
 *
 *  METHOD: wpSetup                                        ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Specify Setup strings and do some initialization.  This method is
 *    invoked once an object is completely created.
 *
 *  Note:  We're overriding this method to write some default data to the
 *         object's real filename.  This will give us something to look at
 *         if the user drag/drops us on an editor or selects the open/editor
 *         view.
 *)

PROCEDURE( Self : PCar ) wpSetup
(
  szSetupString : ARRAY OF CHAR
)               : BOOLEAN;
TYPE
  FILESTRING       = ARRAY [0..OS2DEF.CCHMAXPATH-1] OF CHAR;
VAR
(*somThis          : PCarData;*)
  cbBytesWritten   : LONGCARD;      (* pointer to receiving byte count *)
  rc               : OS2DEF.APIRET;
  fSuccess         : BOOLEAN;
  hf               : OS2DEF.HFILE;  (* variable for file handle *)
  ulAction         : LONGCARD;      (* variable for action taken *)
  szObjectFilename : FILESTRING;    (* buffer for wpQueryRealName() *)
  cb               : LONGCARD;
  ach              : ARRAY [0..9] OF CHAR;
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "wpSetup", currentFile(), currentLine() );
  END;

  cb := SIZE( szObjectFilename );

  (*
   *   Write an initial bill-of-sale to the object data file
   *)

  fSuccess := Self^.wpQueryRealName (* query full-pathname of object's file *)
  (
    szObjectFilename,               (* return buffer *)
    cb,                             (* size of buffer *)
    TRUE                            (* request fully qualified pathname *)
  );

  IF fSuccess THEN
    rc := DOSFILEMGR.DosOpen
    (
      szObjectFilename,
      hf,
      ulAction,
      0,                                  (* file size if created or truncated *)
      DOSFILEMGR.FILE_NORMAL,             (* file attribute *)
      DOSFILEMGR.FILE_OPEN OR             (* action taken upon exist/not exist *)
      DOSFILEMGR.FILE_CREATE,
      DOSFILEMGR.OPEN_ACCESS_READWRITE OR (* open mode of file *)
      DOSFILEMGR.OPEN_SHARE_DENYNONE,
      NIL                                 (* no extended attributes *)
    );
    IF rc <> 0 THEN
      Conversions.LongCardToStr( rc, ach );
      DebugBox( "Car.wpSetup: DosOpen failed rc =", ach );
    ELSE
      DOSFILEMGR.DosWrite
      ( hf, szDefaultText, SIZE( szDefaultText ), cbBytesWritten );
      DOSFILEMGR.DosClose
      ( hf );
    END;
  END;

  RETURN Self^.wpSetup^( szSetupString );  (* parent call *)
END wpSetup;



(*
 *
 *  METHOD: TrapTest                                       ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Specify Setup strings and do some initialization.  This method is
 *    invoked once an object is completely created.
 *
 *  Note:  Not yet implemented in Modula-2
 *)

PROCEDURE( Self : PCar ) TrapTest( );
VAR
(*somThis       : PCarData;*)
BEGIN
(*somThis := CarGetData( Self );*)
  IF CarDebug THEN
    somDebug( "Car", "TrapTest", currentFile(), currentLine() );
  END;
  DebugBox( "Car.TrapTest", "This method is not yet implemented" );
END TrapTest;

**********)


(*************************************************************************

                        CLASS METHODS SECTION

              Do not put any code in this section unless
                     it is an object CLASS method

**************************************************************************)


(*********

(*
 *
 *  METHOD: clsQueryModuleHandle                           ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    This method returns the module handle of this class.  If this is the
 *    first invocation, DosQueryModuleHandle is called to save the handle
 *    for future invocations.
 *
 *  RETURN:
 *
 *    0              Unsuccessful
 *    non-zero       module handle
 *
 *)

PROCEDURE( Self : PM_Car ) clsQueryModuleHandle( ) : OS2DEF.HMODULE;
VAR
(*somThis       : M_CarData;*)
  rc            : OS2DEF.APIRET;
  szPathName    : SOM.zString;
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "clsQueryModuleHandle", currentFile(), currentLine() );
  END;

  (*
   *   Make sure we already have module handle
   *)

  IF hmod = OS2DEF.NULLHANDLE THEN
    (*
     *   Retrieve registered pathname of our module (DLL) and query the
     *   module handle.
     *)
    szPathName := SOM.SOMClassMgrObject^.somLocateClassFile
    (
      SOM.somIdFromString( "CAR" ),
      Car_MajorVersion,
      Car_MinorVersion
    );
    rc := DOSMODULEMGR.DosQueryModuleHandle( szPathName^, hmod );
    IF rc <> 0 THEN
      DebugBox( "M_Car.clsQueryModuleHandle", "Failed to query module handle" );
      RETURN 0;
    END;
  END;

  RETURN hmod;

END clsQueryModuleHandle;




(*
 *
 *  METHOD: wpclsQueryStyle                                ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *     The wpclsQueryStyle method is called to allow the class object
 *     to specify the default object class style for its instances.
 *
 *  REMARKS:
 *
 *     This method can be called at any time in order to determine the
 *     default style for instances of this class.
 *
 *     This method should be overridden in order to modify the default
 *     object style for instances of this class.
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryStyle( ) : LONGCARD;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryStyle", currentFile(), currentLine() );
  END;
  (*
   *   Modify style bits as described in programming reference.  This
   *   particular style (link) is also disabled in wpFilterPopupMenu()
   *)
  RETURN Self^.wpclsQueryStyle^() OR WPOBJECT.CLSSTYLE_NEVERLINK;
END wpclsQueryStyle;




(*
 *
 *  METHOD: wpclsInitData                                  ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Initalize the class data
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsInitData( );
VAR
(*somThis       : M_CarData;*)
  i             : LONGCARD;
  pCFI          : WPOBJECT.PCLASSFIELDINFO;
  pClassTitle   : OS2DEF.PSZ;
  pCarDetails   : PCARDETAILS;
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsInitData", currentFile(), currentLine() );
  END;

  (*
   *   Call the parent class method first
   *)
  Self^.wpclsInitData^();

  (*
   *   Get class title
   *)
  IF WIN.WinLoadString
     (
       WIN.WinQueryAnchorBlock( WIN.HWND_DESKTOP ),
       Self^.clsQueryModuleHandle(),
       ID_TITLE,
       SIZE( szCarClassTitle ),
       szCarClassTitle
     ) = 0
  THEN (* Load string failed: use the parent's string *)
    pClassTitle := Self^.wpclsQueryTitle^();
    Strings.Assign( pClassTitle^, szCarClassTitle );
  END;


  (*
   *   Initialize everything needed for the CLASSFIELDINFO structures
   *   for the Car object class
   *)
  FOR i := 0 TO NUM_CAR_FIELDS-1 DO
    pCFI := ADR( fieldinfo[i] );
    SYSTEM.MemSet( pCFI, 0, SIZE( WPOBJECT.CLASSFIELDINFO ) );    (* zero's *)
    pCFI^.cb := SIZE( WPOBJECT.CLASSFIELDINFO );
    pCFI^.flData  := WIN.CFA_RIGHT OR
                     WIN.CFA_SEPARATOR OR
                     WIN.CFA_FIREADONLY;
    pCFI^.flTitle := WIN.CFA_CENTER OR
                     WIN.CFA_SEPARATOR OR
                     WIN.CFA_HORZSEPARATOR OR
                     WIN.CFA_STRING OR
                     WIN.CFA_FITITLEREADONLY;
    pCFI^.pNextFieldInfo := ADR( fieldinfo[i+1] ); (* next CLASSFIELDINFO *)
    pCFI^.pTitleData := ADR( aszCarColTitles[i] );
    pCFI^.flCompare  := WPOBJECT.COMPARE_SUPPORTED OR WPOBJECT.SORTBY_SUPPORTED;
    pCarDetails := NIL; (* only needed for computing field offsets *)
    CASE i OF
    | INDEX_MAKE:
        pCFI^.flData := pCFI^.flData OR WIN.CFA_STRING;
        pCFI^.offFieldData := ADR( pCarDetails^.pszMake );   (* field offset *)
        pCFI^.ulLenFieldData := SIZE( pCarDetails^.pszMake );
        pCFI^.DefaultComparison := WPOBJECT.CMP_EQUAL;
    | INDEX_MODEL:
        pCFI^.flData := pCFI^.flData OR WIN.CFA_STRING;
        pCFI^.offFieldData := ADR( pCarDetails^.pszModel );  (* field offset *)
        pCFI^.ulLenFieldData := SIZE( pCarDetails^.pszModel );
        pCFI^.DefaultComparison := WPOBJECT.CMP_EQUAL;
    | INDEX_COLOR:
        pCFI^.flData := pCFI^.flData OR WIN.CFA_STRING;
        pCFI^.offFieldData := ADR( pCarDetails^.pszColor );  (* field offset *)
        pCFI^.ulLenFieldData := SIZE( pCarDetails^.pszColor );
        pCFI^.DefaultComparison := WPOBJECT.CMP_EQUAL;
    | INDEX_SALE_DATE:
        pCFI^.flData := pCFI^.flData OR WIN.CFA_DATE;
        pCFI^.offFieldData := ADR( pCarDetails^.cdateSale ); (* field offset *)
        pCFI^.ulLenFieldData := SIZE( WIN.CDATE );
        pCFI^.ulLenCompareValue := SIZE( WIN.CDATE );
        pCFI^.DefaultComparison := WPOBJECT.CMP_GREATER;
    | INDEX_PRICE:
        pCFI^.flData := pCFI^.flData OR WIN.CFA_ULONG;
        pCFI^.offFieldData := ADR( pCarDetails^.ulPrice );   (* field offset *)
        pCFI^.ulLenFieldData := SIZE( LONGCARD );
        pCFI^.ulLenCompareValue := SIZE( LONGCARD );
        pCFI^.DefaultComparison := WPOBJECT.CMP_GREATER;
    END;
  END;

  (* terminate linked list *)
  fieldinfo[ NUM_CAR_FIELDS-1 ].pNextFieldInfo := NIL;

END wpclsInitData;




(*
 *
 *  METHOD: wpclsUnInitData                                ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Free any class data
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsUnInitData( );
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsUnInitData", currentFile(), currentLine() );
  END;
  Self^.wpclsUnInitData^(); (* parent call *)
END wpclsUnInitData;



(*
 *
 *  METHOD: wpclsQueryTitle                                ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Return the string "New car".
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryTitle( ) : OS2DEF.PSZ;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryTitle", currentFile(), currentLine() );
  END;

  (*
   *   Return the class title for a car
   *)
  IF szCarClassTitle[0] <> 0C THEN
    RETURN ADR( szCarClassTitle );
  ELSE
    RETURN Self^.wpclsQueryTitle^();  (* parent call *)
  END;
END wpclsQueryTitle;





(*
 *
 *  METHOD: wpclsQueryIconData                             ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Return the class icon
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryIconData
(
  pIconInfo     : OS2DEF.PICONINFO
)               : LONGCARD;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryIconData", currentFile(), currentLine() );
  END;
  IF pIconInfo <> NIL THEN
    (*
     *   fill in icon information
     *)
    pIconInfo^.fFormat := OS2DEF.ICON_RESOURCE;
    pIconInfo^.hmod := Self^.clsQueryModuleHandle();
    pIconInfo^.resid := ID_ICON;
  END;
  RETURN SIZE( OS2DEF.ICONINFO );
END wpclsQueryIconData;




(*
 *
 *  METHOD: wpclsQueryDefaultHelp                          ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Process input from the extra menu option that we added.
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryDefaultHelp
(
  VAR HelpPanelId   : LONGCARD;
  VAR szHelpLibrary : ARRAY OF CHAR
)                   : BOOLEAN;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryDefaultHelp", currentFile(), currentLine() );
  END;
  IF ADR( HelpPanelId ) <> NIL THEN
    HelpPanelId := ID_HELP_DEFAULT;
  END;
  IF ADR( szHelpLibrary ) <> NIL THEN  (* copy help filename *)
    SYSTEM.MemCpy( ADR( szHelpLibrary ), ADR( szHelpLib ), SIZE( szHelpLib ) );
  END;
  RETURN TRUE;
END wpclsQueryDefaultHelp;




(*
 *
 *  METHOD: wpclsQueryDefaultView                          ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Returns the default view for a new instance of this object.
 *
 *  REMARKS:
 *
 *    Tell the system what our default open view is...
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryDefaultView( ) : LONGCARD;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryDefaultHelp", currentFile(), currentLine() );
  END;
  RETURN OPEN_CAR;
END wpclsQueryDefaultView;



(*
 *
 *  METHOD: wpclsQueryDetailsInfo                          ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *    Appends the car specific chain of FIELDINFO structures describing the
 *    details data of this object to ClassFieldInfo (if pClassFieldInfo
 *    is NON-NIL).  In this case it also sets ClassFieldInfo to the
 *    head of the linked list.
 *
 *    Adds the number of bytes required by the details data for car to Size
 *    (if pSize is NON-NIL).
 *
 *  REMARKS:
 *
 *    Add details data for this object.
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryDetailsInfo
(
  ppClassFieldInfo : WPOBJECT.PPCLASSFIELDINFO;
  pSize            : WPOBJECT.PLONGCARD
)                  : LONGCARD;
VAR
(*somThis          : M_CarData;*)
  cParentColumns   : LONGCARD;
  pCFI             : WPOBJECT.PCLASSFIELDINFO;
  i                : LONGCARD;
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryDetailsInfo", currentFile(), currentLine() );
  END;

  (*
   *   Always call the parent method first to retrieve number of details
   *   columns and parent's data already defined in details buffer.
   *)
  cParentColumns := Self^.wpclsQueryDetailsInfo^( ppClassFieldInfo, pSize );

  (*
   *   If pSize is non-NIL, we must add the size of our deatils column
   *   data structure.
   *)
  IF pSize <> NIL THEN
    pSize^ := pSize^ + SIZE( CARDETAILS );  (* adjust size *)
  END;

  (*
   *   If the request was for the chained fieldinfo structures
   *   (ppClassFieldInfo is non-NIL), link them in
   *
   *   eventually the chain will look like
   *
   *   Grandad - Dad - Me - Kid - Grandkid
   *
   *   I will be getting the pointer to the beginning of the chain
   *
   *   If the beginning of the chain is 0, I will assign the address
   *   of my first CLASSFIELDINFO structure to ppClassFieldInfo^.
   *   Otherwise pp^ points to the first column description in the
   *   chain.  We need to walk the chain and link our CLASSFIELDINFO
   *   structures at the end.
   *)

  IF ppClassFieldInfo <> NIL THEN
    (*
     *   Find the last link in the chain;  Then link our CLASSFIELDINFO
     *   structures to the chain.
     *)
    IF ppClassFieldInfo^ <> NIL THEN
      pCFI := ppClassFieldInfo^;
      FOR i:=0 TO cParentColumns-1 DO
        IF pCFI^.pNextFieldInfo <> NIL THEN
          pCFI := pCFI^.pNextFieldInfo;
        END;
      END;
      pCFI^.pNextFieldInfo := ADR( fieldinfo );
    ELSE
      ppClassFieldInfo^ := ADR( fieldinfo );
    END;
  END;
  RETURN cParentColumns + NUM_CAR_FIELDS;
END wpclsQueryDetailsInfo;




(*
 *
 *  METHOD: wpclsQueryInstanceFilter                       ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *     The wpclsQueryInstanceFilter method is called to allow the class
 *     object to specify the file title filters for instances of its
 *     class.
 *
 *  REMARKS:
 *
 *     A pointer to a string containing file title filter(s).  This
 *     string can contain several file title filters separated by a
 *     comma.  Example: "*.TXT, *.DOC"
 *
 *  Note:  Overriding this method will cause any data file with the extension
 *         ".CAR" to become a data object of the class "Car."
 *
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryInstanceFilter( ) : OS2DEF.PSZ;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryInstanceFilter", currentFile(), currentLine() );
  END;
  RETURN ADR( szCarInstanceFilter );
END wpclsQueryInstanceFilter;



(*
 *
 *  METHOD: wpclsQueryDetails                              ( ) PRIVATE
 *                                                         (X) PUBLIC
 *  DESCRIPTION:
 *
 *     Specify default details to display in details view
 *
 *  REMARKS:
 *
 *     A pointer details structure is modified and returned.
 *     Note: this is purely virtual at the moment (parent class doesn't
 *     do anything).
 *)

PROCEDURE( Self : PM_Car ) wpclsQueryDetails( ) : WPOBJECT.PCLASSDETAILS;
VAR
(*somThis       : M_CarData;*)
BEGIN
(*somThis := M_CarGetData( Self );*)
  IF M_CarDebug THEN
    somDebug( "M_Car", "wpclsQueryDetails", currentFile(), currentLine() );
  END;
  RETURN Self^.wpclsQueryDetails^();  (* parent call *)
END wpclsQueryDetails;


*********)



(*************************************************************************

                        ORDINARY CODE SECTION

                  Any non-method code should go here.

**************************************************************************)


(*********


(*
 *
 *       ROUTINE:    CarInit ( )
 *
 *       DESCRIPTION:    Car Initialization
 *
 *       RETURNS:    Handle of car frame window, NIL if error
 *
 *)

PROCEDURE CarInit( somSelf : PCar ) : OS2DEF.HWND;
VAR
  hab                   : OS2DEF.HAB;             (* PM anchor block handle *)
  hwndFrame             : OS2DEF.HWND;            (* Frame window handle *)
  hwndClient            : OS2DEF.HWND;
  pWindowData           : PWINDOWDATA;
  fSuccess              : BOOLEAN;
  flFrameCtlData        : WIN.FRAMECDATA;         (* Frame Ctl Data *)
  pszTitle              : OS2DEF.PSZ;             (* Frame window title *)
  pPresData             : OS2DEF.PVOID;           (* Frame window presentation *)
  swp                   : WIN.SWP;
BEGIN
  hwndFrame := OS2DEF.NULLHANDLE;
  hwndClient := OS2DEF.NULLHANDLE;

  hab := WIN.WinQueryAnchorBlock( WIN.HWND_DESKTOP );
  IF NOT WIN.WinRegisterClass
         (
           hab,
           szCarWindowClass,
           CarWndProc,
           WIN.CS_SIZEREDRAW OR WIN.CS_SYNCPAINT,
           SIZE( pWindowData )
          )
  THEN
    DebugBox( "CarInit", "Failure in WinRegisterClass" );
    RETURN OS2DEF.NULLHANDLE;
  END;

  (*
   *   Allocate some instance specific data in Window words of Frame window.
   *   This will ensure our window procedure can use this object's methods
   *   (our window proc isn't passed a somSelf pointer).
   *)
  pWindowData := somSelf^.wpAllocMem( SIZE( pWindowData^ ), NIL );
  IF pWindowData = NIL THEN
    DebugBox( "CarInit", "wpAllocMem failed to allocate pWindowData" );
    RETURN OS2DEF.NULLHANDLE;
  END;

  SYSTEM.MemSet( pWindowData, 0, SIZE( pWindowData^ ) );
  pWindowData^.cb := SIZE( pWindowData^ ); (* first field := size *)
  pWindowData^.somSelf := somSelf;

  (*
   *  Create a frame window
   *)
  flFrameCtlData.cb := SIZE( flFrameCtlData );
  flFrameCtlData.flCreateFlags :=
    WIN.FCF_SIZEBORDER OR WIN.FCF_TITLEBAR OR WIN.FCF_SYSMENU OR WIN.FCF_MINMAX;
  flFrameCtlData.hmodResources := SHORT( hmod );
  flFrameCtlData.idResources := ID_ICON;
  pszTitle := somSelf^.wpQueryTitle();
  pPresData := NIL;
  hwndFrame := WIN.WinCreateWindow (* create frame window *)
  (
    WIN.HWND_DESKTOP,           (* parent-window handle      *)
    WIN.WC_FRAME^,              (* registered class name     *)
    pszTitle^,                  (* window text               *)
    0,                          (* window style              *)
    0, 0, 0, 0,                 (* position of window        *)
    OS2DEF.NULLHANDLE,          (* owner-window handle       *)
    WIN.HWND_TOP,               (* handle to sibling window  *)
    ID_FRAME,                   (* window identifier         *)
    flFrameCtlData,             (* control data              *)
    pPresData^                  (* presentation data         *)
  );
  IF hwndFrame = OS2DEF.NULLHANDLE THEN
    DebugBox( "CarInit", "Failure in WinCreateWindow" );
    RETURN OS2DEF.NULLHANDLE;
  END;

  (*
   *  Create a client area inside parent frame window using
   *  WinCreateWindow so we can pass presentation parameters
   *
   *)
  pszTitle := NIL;
  pPresData := NIL;
  hwndClient := WIN.WinCreateWindow
  (
    hwndFrame,                 (* parent-window handle       *)
    szCarWindowClass,          (* registered class name      *)
    pszTitle^,                 (* window text                *)
    0,                         (* window style               *)
    0, 0, 0, 0,                (* position of window         *)
    hwndFrame,                 (* owner-window handle        *)
    WIN.HWND_TOP,              (* handle to sibling window   *)
    WIN.FID_CLIENT,            (* window identifier          *)
    pWindowData^,              (* buffer                     *)
    pPresData^                 (* presentation data          *)
  );
  IF hwndClient = OS2DEF.NULLHANDLE THEN
    WIN.WinDestroyWindow( hwndFrame );
    RETURN OS2DEF.NULLHANDLE;
  END;

  WIN.WinSendMsg
  ( hwndFrame, WIN.WM_SETICON, WIN.MPFROMP( somSelf^.wpQueryIcon() ), NIL );
  pszTitle := somSelf^.wpQueryTitle();
  WIN.WinSetWindowText
  ( WIN.WinWindowFromID( hwndFrame, WIN.FID_TITLEBAR ), pszTitle^ );

  (*
   * Restore the Window Position
   *)
  pszTitle := somSelf^.wpQueryTitle();
  fSuccess := WINWORKPLACE.WinRestoreWindowPos
  (
    szCarClassTitle,       (* class title *)
    pszTitle^,             (* object title *)
    hwndFrame
  );
  IF NOT fSuccess THEN
    (* Get the dimensions and the shell's suggested
     * location for the window
     *)
    WIN.WinQueryTaskSizePos( hab, 0, swp );
    (* Set the frame window position
     *)
    swp.fl := WIN.SWP_SIZE OR WIN.SWP_MOVE OR WIN.SWP_RESTORE OR WIN.SWP_ZORDER;
    WIN.WinSetWindowPos
    ( hwndFrame, WIN.HWND_TOP, swp.x, swp.y, swp.cx, swp.cy, swp.fl );
  END;

  WIN.WinShowWindow( hwndFrame, TRUE );
  WIN.WinStartTimer( hab, hwndClient, CAR_TIMER, 100 );

  RETURN hwndFrame;      (* success *)

END CarInit;




(*
 *
 *   CarWndProc()
 *
 *   DESCRIPTION: Car Window Procedure
 *
 *)

PROCEDURE CarWndProc
(
  hwnd          : OS2DEF.HWND;
  msg           : LONGCARD;
  mp1           : WIN.MPARAM;
  mp2           : WIN.MPARAM
)               : WIN.MRESULT;
VAR
  pWindowData   : PWINDOWDATA;
  hwndFrame     : OS2DEF.HWND;
  fSuccess      : BOOLEAN;
  pszTitle      : OS2DEF.PSZ;
  rectl         : OS2DEF.RECTL;
  hps           : OS2DEF.HPS;
  hab           : OS2DEF.HAB;
BEGIN
   hwndFrame := WIN.WinQueryWindow( hwnd, WIN.QW_PARENT );

   CASE msg OF

   | WIN.WM_CREATE:

     pWindowData := PWINDOWDATA( mp1 );

     IF pWindowData = NIL THEN
       DebugBox( "CarWndProc:WM_CREATE", "couldn't get window words" );
       RETURN WIN.MRESULT( FALSE );
     END;
     (*
      *   Fill in the class view/usage details and window specific data
      *   for this instance.
      *)
     pWindowData^.UseItem.type    := WPOBJECT.USAGE_OPENVIEW;
     pWindowData^.ViewItem.view   := OPEN_CAR;
     pWindowData^.ViewItem.handle := hwndFrame;
     pWindowData^.x               := 10;
     pWindowData^.y               := 10;
     pWindowData^.xDir            := CAR_RIGHT;
     pWindowData^.yDir            := CAR_UP;

     (*
      *   Set window pointer with object pointer and instance view info.
      *   Then add view to the in-use list so wpSwitchTo works.
      *)
     WIN.WinSetWindowPtr( hwnd, WIN.QWL_USER, pWindowData );
     pWindowData^.somSelf^.wpAddToObjUseList( pWindowData^.UseItem );
     pszTitle := pWindowData^.somSelf^.wpQueryTitle();
     pWindowData^.somSelf^.wpRegisterView( hwndFrame, pszTitle^ );
     WIN.WinSetFocus( WIN.HWND_DESKTOP, hwndFrame );

   | WIN.WM_COMMAND:

     (* nothing *)

   | WIN.WM_TIMER:

     pWindowData := PWINDOWDATA( WIN.WinQueryWindowPtr( hwnd, WIN.QWL_USER ) );

     IF pWindowData = NIL THEN
       DebugBox( "CarWndProc:WM_TIMER", "couldn't get window words" );
       RETURN WIN.MRESULT( FALSE );
     ELSE
       (*
        *   If the car's brakes are off, we move the car by modifying it's
        *   x,y position.  Direction (xDir, yDir) changes when the car's
        *   position reaches a border of the window.  The distance it
        *   moves is based on the speed contained in an instance variable.
        *)

       IF NOT pWindowData^.somSelf^.QueryBrakes() THEN
         WIN.WinQueryWindowRect( hwnd,rectl );

         IF pWindowData^.x <= 0 THEN              (* at left border? *)
           pWindowData^.xDir := CAR_RIGHT;        (* mult. by  1 *)
         ELSIF pWindowData^.x >= (rectl.xRight - ICON_WIDTH)  THEN
           pWindowData^.xDir := CAR_LEFT;         (* mult. by -1 *)
         END;

         IF pWindowData^.y <= 0 THEN              (* at bottom border? *)
           pWindowData^.yDir := CAR_UP;           (* mult. by  1 *)
         ELSIF pWindowData^.y >= (rectl.yTop - ICON_HEIGHT) THEN
           pWindowData^.yDir := CAR_DOWN;         (* mult. by -1 *)
         END;

         pWindowData^.x := pWindowData^.x +
           pWindowData^.xDir * VAL( LONGINT, pWindowData^.somSelf^.QuerySpeed() );

         pWindowData^.y := pWindowData^.y +
           pWindowData^.yDir * VAL( LONGINT, pWindowData^.somSelf^.QuerySpeed() );

         WIN.WinInvalidateRect( hwnd, rectl, TRUE );  (* invalidate car region *)
       END;
     END;

   | WIN.WM_PAINT:

     pWindowData := PWINDOWDATA( WIN.WinQueryWindowPtr( hwnd, WIN.QWL_USER ) );

     IF pWindowData = NIL THEN
       DebugBox( "CarWndProc:WM_PAINT", "couldn't get window words" );
       RETURN WIN.MRESULT( FALSE );
     ELSE
       hps := WIN.WinBeginPaint( hwnd, OS2DEF.HPS( OS2DEF.NULLHANDLE ), rectl );
       WIN.WinFillRect( hps, rectl, WIN.SYSCLR_WINDOW );
       WIN.WinDrawPointer
       ( hps,
         pWindowData^.x,
         pWindowData^.y,
         pWindowData^.somSelf^.wpQueryIcon(),
         WIN.DP_NORMAL
       );
       WIN.WinEndPaint( hps );
     END;

   | WIN.WM_CLOSE:

     hab := WIN.WinQueryAnchorBlock( WIN.HWND_DESKTOP );
     WIN.WinStopTimer( hab, hwnd, CAR_TIMER );

     pWindowData := PWINDOWDATA( WIN.WinQueryWindowPtr( hwnd, WIN.QWL_USER ) );

     IF pWindowData = NIL THEN
       DebugBox( "CarWndProc:WM_CLOSE", "couldn't get window words" );
       RETURN WIN.MRESULT( FALSE );
     END;
     pszTitle := pWindowData^.somSelf^.wpQueryTitle();
     fSuccess := WINWORKPLACE.WinStoreWindowPos
     ( szCarClassTitle, pszTitle^, hwndFrame );
     pWindowData^.somSelf^.wpDeleteFromObjUseList( pWindowData^.UseItem );
     pWindowData^.somSelf^.wpFreeMem( pWindowData );

     WIN.WinPostMsg( hwnd, WIN.WM_QUIT, 0, 0 );
     WIN.WinDestroyWindow ( hwndFrame ) ;

   ELSE
     RETURN WIN.WinDefWindowProc( hwnd, msg, mp1, mp2 );
   END;

   RETURN WIN.MRESULT( FALSE );

END CarWndProc;





(*
 *
 *   DashBoardDlgProc()
 *
 *   DESCRIPTION: Dialog Procedure for Dashboaard settings page
 *
 *)

PROCEDURE DashBoardDlgProc
(
  hwndDlg       : OS2DEF.HWND;
  msg           : LONGCARD;
  mp1           : WIN.MPARAM;
  mp2           : WIN.MPARAM
)               : WIN.MRESULT;
VAR
  pDashDlgData  : PDASHDLGDATA;
  somThis       : PCarData;
  acBuffer      : ARRAY [0..9] OF CHAR;
  usCount       : CARDINAL;
  SliderData    : WIN.SLDCDATA;
  wprm          : WIN.WNDPARAMS;
  somSelf       : PCar;
  temp          : LONGCARD;
BEGIN

  CASE msg OF

  | WIN.WM_INITDLG:

    (*
     *   Store some instance specific data in Window words of this dialog.
     *   This will ensure our dialog procedure can access this objects
     *   data (our dialog proc isn't always passed a somSelf^ pointer).
     *)
    somSelf := mp2;
    pDashDlgData :=  somSelf^.wpAllocMem( SIZE( pDashDlgData^ ), NIL );
    IF pDashDlgData = NIL THEN
      DebugBox( "DashboardDlgProc:WM_INITDLG", "Couldn't allocate window words" );
    ELSE
      SYSTEM.MemSet( pDashDlgData, 0, SIZE( pDashDlgData^ ) );
      pDashDlgData^.cb := SIZE( pDashDlgData^ );
      pDashDlgData^.somSelf := mp2; (* pointer to this object *)
      pDashDlgData^.PrevBrakes := pDashDlgData^.somSelf^.QueryBrakes();
      pDashDlgData^.PrevSpeed  := pDashDlgData^.somSelf^.QuerySpeed();
      WIN.WinSetWindowPtr( hwndDlg, WIN.QWL_USER, pDashDlgData );
      SliderData.cbSize := SIZE( WIN.SLDCDATA );
      SliderData.usScale1Increments := 10;
      SliderData.usScale1Spacing := 20;
      SliderData.usScale2Increments := 10;
      SliderData.usScale2Spacing := 20;
      wprm.fsStatus := WIN.WPM_CTLDATA;
      wprm.cchText := 0;
      wprm.cbPresParams := 0;
      wprm.cbCtlData := 0;
      wprm.pCtlData := ADR( SliderData );
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_SPEEDSLIDER,
        WIN.WM_SETWINDOWPARAMS,
        WIN.MPARAM( ADR( wprm ) ),
        WIN.MPARAM( 0 )
      );
      FOR usCount := 0 TO 9 DO
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_SPEEDSLIDER,
          WIN.SLM_SETTICKSIZE,
          WIN.MPFROM2SHORT( usCount, 5 ),
          WIN.MPARAM( 0 )
        );
        Conversions.CardToStr( usCount*10, acBuffer );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_SPEEDSLIDER,
          WIN.SLM_SETSCALETEXT,
          WIN.MPFROMSHORT( usCount ),
          WIN.MPFROMP( ADR( acBuffer ) )
        );
      END;
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_SPEEDSLIDER,
        WIN.SLM_SETSLIDERINFO,
        WIN.MPFROM2SHORT( WIN.SMA_SLIDERARMDIMENSIONS, 0 ),
        WIN.MPFROM2SHORT( 20, 40 )
      );
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_SPEEDSLIDER,
        WIN.SLM_SETSLIDERINFO,
        WIN.MPFROM2SHORT( WIN.SMA_SLIDERARMPOSITION, WIN.SMA_INCREMENTVALUE ),
        WIN.MPFROMSHORT( SHORT( pDashDlgData^.somSelf^.QuerySpeed() ) DIV 10 )
      );
      Conversions.LongIntToStr( pDashDlgData^.somSelf^.QuerySpeed(), acBuffer );
      WIN.WinSetDlgItemText( hwndDlg, ID_SPEEDDATA, acBuffer );
      IF NOT pDashDlgData^.somSelf^.QueryBrakes() THEN
        WIN.WinSendDlgItemMsg
        ( hwndDlg, ID_GO, WIN.BM_SETCHECK, WIN.MPARAM( 1 ), WIN.MPVOID );
      ELSE
        WIN.WinSendDlgItemMsg
        ( hwndDlg, ID_STOP, WIN.BM_SETCHECK, WIN.MPARAM( 1 ), WIN.MPVOID );
      END;
      RETURN WIN.MRESULT( TRUE );
    END;

  | WIN.WM_CLOSE:

    pDashDlgData := WIN.WinQueryWindowPtr( hwndDlg, WIN.QWL_USER );
    IF pDashDlgData = NIL THEN
      DebugBox( "DashboardDlgProc:WM_DESTROY", "couldn't get window words" );
    ELSE
      pDashDlgData^.somSelf^.wpFreeMem( pDashDlgData );
      RETURN WIN.WinDefDlgProc( hwndDlg, msg, mp1, mp2 );
    END;

  | WIN.WM_COMMAND:

    pDashDlgData := WIN.WinQueryWindowPtr( hwndDlg, WIN.QWL_USER );
    somThis := CarGetData( pDashDlgData^.somSelf );
    IF pDashDlgData = NIL THEN
      DebugBox( "DashboardDlgProc:WM_COMMAND", "couldn't get window words" );
    ELSE
      CASE WIN.SHORT1FROMMP( mp1 ) OF
      | ID_UNDO:
        pDashDlgData^.somSelf^.SetBrakes( pDashDlgData^.PrevBrakes );
        pDashDlgData^.somSelf^.SetSpeed( pDashDlgData^.PrevSpeed );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_SPEEDSLIDER,
          WIN.SLM_SETSLIDERINFO,
          WIN.MPFROM2SHORT( WIN.SMA_SLIDERARMPOSITION, WIN.SMA_INCREMENTVALUE ),
          WIN.MPFROMSHORT( SHORT( pDashDlgData^.somSelf^.QuerySpeed() ) DIV 10 )
        );
        Conversions.LongCardToStr( pDashDlgData^.somSelf^.QuerySpeed(), acBuffer );
        WIN.WinSetDlgItemText( hwndDlg, ID_SPEEDDATA, acBuffer );
        IF NOT pDashDlgData^.somSelf^.QueryBrakes() THEN
          WIN.WinSendDlgItemMsg
          ( hwndDlg, ID_GO, WIN.BM_SETCHECK, WIN.MPARAM( 1 ), WIN.MPVOID );
        ELSE
          WIN.WinSendDlgItemMsg
          ( hwndDlg, ID_STOP, WIN.BM_SETCHECK, WIN.MPARAM( 1 ), WIN.MPVOID );
        END;
      | ID_DEFAULT:
        (*
         *   preserve previous values
         *)
        pDashDlgData^.PrevBrakes := pDashDlgData^.somSelf^.QueryBrakes();
        pDashDlgData^.PrevSpeed  := pDashDlgData^.somSelf^.QuerySpeed();
        pDashDlgData^.somSelf^.SetBrakes( DEFAULT_BRAKEFLAG );
        pDashDlgData^.somSelf^.SetSpeed( DEFAULT_SPEED );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_SPEEDSLIDER,
          WIN.SLM_SETSLIDERINFO,
          WIN.MPFROM2SHORT( WIN.SMA_SLIDERARMPOSITION, WIN.SMA_INCREMENTVALUE ),
          WIN.MPFROMSHORT( SHORT( pDashDlgData^.somSelf^.QuerySpeed() ) DIV  10 )
        );
        Conversions.LongCardToStr( pDashDlgData^.somSelf^.QuerySpeed(), acBuffer );
        WIN.WinSetDlgItemText
        ( hwndDlg, ID_SPEEDDATA, acBuffer );
        WIN.WinSendDlgItemMsg
        ( hwndDlg, ID_GO, WIN.BM_SETCHECK, WIN.MPARAM( 1 ), WIN.MPVOID );
      | ID_HELP:
        (* nothing *)
      END;
      RETURN WIN.MRESULT( TRUE );
    END;

  | WIN.WM_CONTROL:

    pDashDlgData := WIN.WinQueryWindowPtr( hwndDlg, WIN.QWL_USER );
    somThis := CarGetData( pDashDlgData^.somSelf );
    IF pDashDlgData = NIL THEN
      DebugBox( "DashboardDlgProc:WM_CONTROL", "couldn't get window words" );
    ELSE
      CASE WIN.SHORT1FROMMP( mp1 ) OF
      | ID_GO:
        IF WIN.SHORT2FROMMP( mp1 ) = WIN.BN_CLICKED THEN
          pDashDlgData^.PrevBrakes := pDashDlgData^.somSelf^.QueryBrakes();
          pDashDlgData^.somSelf^.SetBrakes( FALSE );
        END;
      | ID_STOP:
        IF WIN.SHORT2FROMMP( mp1 ) = WIN.BN_CLICKED THEN
          pDashDlgData^.PrevBrakes := pDashDlgData^.somSelf^.QueryBrakes();
          pDashDlgData^.somSelf^.SetBrakes( TRUE );
        END;
      | ID_SPEEDSLIDER:
        IF WIN.SHORT2FROMMP( mp1 ) = WIN.SLN_CHANGE THEN
          pDashDlgData^.PrevSpeed  := pDashDlgData^.somSelf^.QuerySpeed();
          temp := WIN.WinSendDlgItemMsg
          ( hwndDlg,
            ID_SPEEDSLIDER,
            WIN.SLM_QUERYSLIDERINFO,
            WIN.MPFROM2SHORT( WIN.SMA_SLIDERARMPOSITION, WIN.SMA_INCREMENTVALUE ),
            WIN.MPARAM( 0 )
          );
          temp := temp * 10;
          Conversions.LongCardToStr( temp, acBuffer );
          WIN.WinSetDlgItemText( hwndDlg, ID_SPEEDDATA, acBuffer );
          pDashDlgData^.somSelf^.SetSpeed( temp );
        END;
      END;
      RETURN WIN.MRESULT( TRUE );
    END;

  END; (* CASE msg OF *)

  RETURN WIN.WinDefDlgProc( hwndDlg, msg, mp1, mp2 );

END DashBoardDlgProc;






(*
 *
 *   HornBeepDlgProc()
 *
 *   DESCRIPTION:  Dialog Procedure for Horn Beep settings page
 *
 *)

PROCEDURE HornBeepDlgProc
(
  hwndDlg       : OS2DEF.HWND;
  msg           : LONGCARD;
  mp1           : WIN.MPARAM;
  mp2           : WIN.MPARAM
)               : WIN.MRESULT;
VAR
  pHornDlgData  : PHORNDLGDATA;
  somSelf       : PCar;
  temp          : LONGCARD;
BEGIN
  CASE msg OF

  | WIN.WM_INITDLG:

    (*
     *   Store some instance specific data in Window words of this dialog.
     *   This will ensure our dialog procedure can access this objects
     *   data (our dialog proc isn't always passed a somSelf^ pointer).
     *)
    somSelf := mp2;
    pHornDlgData := somSelf^.wpAllocMem( SIZE( pHornDlgData^ ), NIL );
    IF pHornDlgData = NIL THEN
       DebugBox( "HornBeepDlgProc", "Couldn't allocate window words" );
    ELSE
      SYSTEM.MemSet( pHornDlgData, 0, SIZE( pHornDlgData^ ) );
      pHornDlgData^.cb := SIZE( pHornDlgData^ );
      pHornDlgData^.somSelf := mp2;  (* pointer to this object *)
      pHornDlgData^.PrevDuration := pHornDlgData^.somSelf^.QueryDuration();
      pHornDlgData^.PrevHighTone := pHornDlgData^.somSelf^.QueryHighTone();
      pHornDlgData^.PrevLowTone  := pHornDlgData^.somSelf^.QueryLowTone();
      WIN.WinSetWindowPtr( hwndDlg, WIN.QWL_USER, pHornDlgData );
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_HITONE,
        WIN.SPBM_SETLIMITS,
        WIN.MPFROMLONG( 1000 ),
        WIN.MPFROMLONG( 0 )
      );
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_HITONE,
        WIN.SPBM_SETCURRENTVALUE,
        WIN.MPFROMLONG( pHornDlgData^.somSelf^.QueryHighTone() ),
        WIN.MPFROMLONG( 0 )
      );
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_LOTONE,
        WIN.SPBM_SETLIMITS,
        WIN.MPFROMLONG( 1000 ),
        WIN.MPFROMLONG( 0 )
      );
      WIN.WinSendDlgItemMsg
      ( hwndDlg,
        ID_LOTONE,
        WIN.SPBM_SETCURRENTVALUE,
        WIN.MPFROMLONG( pHornDlgData^.somSelf^.QueryLowTone() ),
        WIN.MPFROMLONG( 0 )
      );
      RETURN WIN.MRESULT( TRUE );
    END;

  | WIN.WM_CLOSE:

    pHornDlgData := WIN.WinQueryWindowPtr( hwndDlg, WIN.QWL_USER );
    IF pHornDlgData = NIL THEN
      DebugBox( "HornBeepDlgProc", "Couldn't get window words" );
    ELSE
      pHornDlgData^.somSelf^.wpFreeMem( pHornDlgData );
      RETURN WIN.WinDefDlgProc( hwndDlg, msg, mp1, mp2 );
    END;

  | WIN.WM_COMMAND:

    pHornDlgData := WIN.WinQueryWindowPtr( hwndDlg, WIN.QWL_USER );
    IF pHornDlgData = NIL THEN
      DebugBox( "HornBeepDlgProc", "Couldn't get window words" );
    ELSE
      CASE WIN.SHORT1FROMMP( mp1 ) OF
      | ID_UNDO:
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_HITONE,
          WIN.SPBM_SETCURRENTVALUE,
          WIN.MPFROMLONG( pHornDlgData^.PrevHighTone ),
          WIN.MPFROMLONG( 0 )
        );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_LOTONE,
          WIN.SPBM_SETCURRENTVALUE,
          WIN.MPFROMLONG( pHornDlgData^.PrevLowTone ),
          WIN.MPFROMLONG( 0 )
        );
      | ID_DEFAULT:
        (*
         *   preserve previous values
         *)
        pHornDlgData^.PrevHighTone := pHornDlgData^.somSelf^.QueryHighTone();
        pHornDlgData^.PrevLowTone := pHornDlgData^.somSelf^.QueryLowTone();
        pHornDlgData^.somSelf^.SetHighTone( DEFAULT_HITONE );
        pHornDlgData^.somSelf^.SetLowTone( DEFAULT_LOTONE );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_HITONE,
          WIN.SPBM_SETCURRENTVALUE,
          WIN.MPFROMLONG( DEFAULT_HITONE ),
          WIN.MPFROMLONG( 0 )
        );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_LOTONE,
          WIN.SPBM_SETCURRENTVALUE,
          WIN.MPFROMLONG( DEFAULT_LOTONE ),
          WIN.MPFROMLONG( 0 )
        );
      | ID_HELP:
        (* nothing *)
      END;
      RETURN WIN.MRESULT( TRUE );
    END;

  | WIN.WM_CONTROL:
    pHornDlgData := WIN.WinQueryWindowPtr( hwndDlg, WIN.QWL_USER );
    IF pHornDlgData = NIL THEN
      DebugBox( "HornBeepDlgProc", "Couldn't get window words" );
    ELSE
      (*
       * When the value of either of the tones change, set the new values
       * in the car object after saving the current settings for
       * "undo" purposes
       *)
      CASE WIN.SHORT2FROMMP( mp1 ) OF
      | WIN.SPBN_ENDSPIN:
        (*
         *   preserve previous values
         *)
        pHornDlgData^.PrevHighTone := pHornDlgData^.somSelf^.QueryHighTone();
        pHornDlgData^.PrevLowTone := pHornDlgData^.somSelf^.QueryLowTone();
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_HITONE,
          WIN.SPBM_QUERYVALUE,
          WIN.MPARAM( ADR( temp ) ),
          WIN.MPFROM2SHORT( 0, WIN.SPBQ_UPDATEIFVALID )
        );
        pHornDlgData^.somSelf^.SetHighTone( temp );
        WIN.WinSendDlgItemMsg
        ( hwndDlg,
          ID_LOTONE,
          WIN.SPBM_QUERYVALUE,
          WIN.MPARAM( ADR( temp ) ),
          WIN.MPFROM2SHORT( 0, WIN.SPBQ_UPDATEIFVALID )
        );
        pHornDlgData^.somSelf^.SetLowTone( temp );
      END;
      RETURN WIN.MRESULT( TRUE );
    END;

  END; (* CASE msg OF *)

  RETURN WIN.WinDefDlgProc( hwndDlg, msg, mp1, mp2 );

END HornBeepDlgProc;







(*
 *
 *   _Exception()
 *
 *   DESCRIPTION:  Exception handler routine for this object.
 *
 *   PURPOSE: To notify user when an illegal memory access is made
 *
 *   METHOD:  Whenever a memory protection exception occurs, a message
 *            box is put on the screen to inform the user.
 *
 *   RETURNS: Returns HANDLED if memory exception,
 *            otherwise, returns NOT_HANDLED
 *
 *)


(*

PROCEDURE _Exception
(
  VAR arg           : DOSEXCEPTIONS.EXCEPTIONREPORTRECORD;
  VAR RegisRecord   : DOSEXCEPTIONS.EXCEPTIONREGISTRATIONRECORD;
  VAR ContextRecord : DOSEXCEPTIONS.CONTEXTRECORD;
  pvSpare           : OS2DEF.PVOID
)                   : LONGCARD;
VAR
  rc                : LONGCARD;
BEGIN

  CASE arg.ExceptionNum OF

  | DOSEXCEPTIONS.XCPT_ACCESS_VIOLATION:
    WIN.WinAlarm( WIN.HWND_DESKTOP, WIN.WA_ERROR );
    rc := WIN.WinMessageBox
    (
      WIN.HWND_DESKTOP,
      WIN.HWND_DESKTOP,
      "Memory Access Violation, abort program",
      "Car Error",
      0,
      WIN.MB_YESNO OR WIN.MB_QUERY OR WIN.MB_MOVEABLE
    );
    IF rc = WIN.MBID_NO THEN
      RETURN DOSEXCEPTIONS.XCPT_CONTINUE_EXECUTION;
    ELSE
      RETURN DOSEXCEPTIONS.XCPT_CONTINUE_SEARCH;
    END;

  | DOSEXCEPTIONS.XCPT_PROCESS_TERMINATE:
    RETURN DOSEXCEPTIONS.XCPT_CONTINUE_SEARCH;

  | DOSEXCEPTIONS.XCPT_ASYNC_PROCESS_TERMINATE:
    RETURN DOSEXCEPTIONS.XCPT_CONTINUE_SEARCH;

  | DOSEXCEPTIONS.XCPT_SIGNAL:
    RETURN DOSEXCEPTIONS.XCPT_CONTINUE_SEARCH;

  ELSE
    (*
     *   For more exceptions, see the header files or the programming
     *   reference(s).
     *)
    RETURN DOSEXCEPTIONS.XCPT_CONTINUE_SEARCH;
  END;

END _Exception;

*)


**************)



(*************************************************************************
   Per-process initialization of module.
**************************************************************************)



BEGIN (* of class module *)
(********
  (* intialize SOM's environment, if not yet active *)
  IF SOM.SOMClassMgrObject = NIL THEN
    SOM.SOMClassMgrObject := SOM.somEnvironmentNew();
  END;
********)
  (* initialize some record fields for class-supporting structures *)
  CarCClassData.parentMtab := NIL;
  CarClassData.classObject := NIL;

(*******
  (* find the identifier tokens for all the new or overridden methods *)
  somId_QueryBrakes            := SOM.somIdFromString( "QueryBrakes"            );
  somId_SetBrakes              := SOM.somIdFromString( "SetBrakes"              );
  somId_QueryDuration          := SOM.somIdFromString( "QueryDuration"          );
  somId_SetDuration            := SOM.somIdFromString( "SetDuration"            );
  somId_QueryHighTone          := SOM.somIdFromString( "QueryHighTone"          );
  somId_SetHighTone            := SOM.somIdFromString( "SetHighTone"            );
  somId_QueryLowTone           := SOM.somIdFromString( "QueryLowTone"           );
  somId_SetLowTone             := SOM.somIdFromString( "SetLowTone"             );
  somId_QuerySpeed             := SOM.somIdFromString( "QuerySpeed"             );
  somId_SetSpeed               := SOM.somIdFromString( "SetSpeed"               );
  somId_BeepHorn               := SOM.somIdFromString( "BeepHorn"               );
  somId_AddDashboardPage       := SOM.somIdFromString( "AddDashboardPage"       );
  somId_AddHornBeepPage        := SOM.somIdFromString( "AddHornBeepPage"        );
  somId_TrapTest               := SOM.somIdFromString( "TrapTest"               );
  somId_wpInitData             := SOM.somIdFromString( "wpInitData"             );
  somId_wpUnInitData           := SOM.somIdFromString( "wpUnInitData"           );
  somId_wpSaveState            := SOM.somIdFromString( "wpSaveState"            );
  somId_wpRestoreState         := SOM.somIdFromString( "wpRestoreState"         );
  somId_wpAddSettingsPages     := SOM.somIdFromString( "wpAddSettingsPages"     );
  somId_wpFilterPopupMenu      := SOM.somIdFromString( "wpFilterPopupMenu"      );
  somId_wpModifyPopupMenu      := SOM.somIdFromString( "wpModifyPopupMenu"      );
  somId_wpMenuItemSelected     := SOM.somIdFromString( "wpMenuItemSelected"     );
  somId_wpMenuItemHelpSelected := SOM.somIdFromString( "wpMenuItemHelpSelected" );
  somId_wpQueryDetailsData     := SOM.somIdFromString( "wpQueryDetailsData"     );
  somId_wpOpen                 := SOM.somIdFromString( "wpOpen"                 );
  somId_wpSetup                := SOM.somIdFromString( "wpSetup"                );
*********)


  (* initialize some record fields for metaclass-supporting structures *)
  M_CarCClassData.parentMtab := NIL;
  M_CarClassData.classObject := NIL;

(********
  (* get identifier tokens for all the new or overridden metaclass methods *)
  somId_clsQueryModuleHandle     := SOM.somIdFromString( "clsQueryModuleHandle"     );
  somId_wpclsInitData            := SOM.somIdFromString( "wpclsInitData"            );
  somId_wpclsUnInitData          := SOM.somIdFromString( "wpclsUnInitData"          );
  somId_wpclsQueryTitle          := SOM.somIdFromString( "wpclsQueryTitle"          );
  somId_wpclsQueryIconData       := SOM.somIdFromString( "wpclsQueryIconData"       );
  somId_wpclsQueryDefaultHelp    := SOM.somIdFromString( "wpclsQueryDefaultHelp"    );
  somId_wpclsQueryDefaultView    := SOM.somIdFromString( "wpclsQueryDefaultView"    );
  somId_wpclsQueryDetailsInfo    := SOM.somIdFromString( "wpclsQueryDetailsInfo"    );
  somId_wpclsQueryDetails        := SOM.somIdFromString( "wpclsQueryDetails"        );
  somId_wpclsQueryInstanceFilter := SOM.somIdFromString( "wpclsQueryInstanceFilter" );
  somId_wpclsQueryStyle          := SOM.somIdFromString( "wpclsQueryStyle"          );
*********)

END CAR.
