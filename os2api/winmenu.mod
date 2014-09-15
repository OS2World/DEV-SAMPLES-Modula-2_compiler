IMPLEMENTATION MODULE WINMENU;

(************************************************************************
  OS/2 2.0 implementation for additional menu functions.

  Copyright (c) 1992 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

IMPORT SYSTEM;
FROM   OS2DEF        IMPORT HWND;
FROM   WINDEF        IMPORT MPARAM, MPFROM2SHORT, MPFROMLONG, MPFROMP;
FROM   WINMENUS      IMPORT MIA_DISABLED, MIA_CHECKED;
FROM   WINMENUS      IMPORT MM_SETITEMATTR, MM_QUERYITEMATTR;
FROM   WINMENUS      IMPORT MM_SETITEMTEXT, MM_ISITEMVALID;
FROM   WINMESSAGEMGR IMPORT WinSendMsg;


PROCEDURE WinCheckMenuItem
( MenuWinHandle        : HWND;
  ItemId               : CARDINAL;
  Check                : BOOLEAN
)                      : BOOLEAN;
VAR
  MParam1              : MPARAM;
  MParam2              : MPARAM;
  MResult              : LONGCARD;
BEGIN
  MParam1 := MPFROM2SHORT( ItemId, LONG( SHORTCARD( TRUE ) ) );
  IF Check THEN
    MParam2 := MPFROM2SHORT( MIA_CHECKED, MIA_CHECKED );
  ELSE
    MParam2 := MPFROM2SHORT( MIA_CHECKED, 0 );
  END;
  MResult := WinSendMsg( MenuWinHandle, MM_SETITEMATTR, MParam1, MParam2 );
  RETURN (MResult <> 0);
END WinCheckMenuItem;


PROCEDURE WinIsMenuItemChecked
( MenuWinHandle        : HWND;
  ItemId               : CARDINAL
)                      : BOOLEAN;
VAR
  MParam1              : MPARAM;
  MParam2              : MPARAM;
  MResult              : LONGCARD;
BEGIN
  MParam1 := MPFROM2SHORT( ItemId, LONG( SHORTCARD( TRUE ) ) );
  MParam2 := MPFROMLONG( MIA_CHECKED );
  MResult := WinSendMsg( MenuWinHandle, MM_QUERYITEMATTR, MParam1, MParam2 );
  RETURN (MResult <> 0);
END WinIsMenuItemChecked;


PROCEDURE WinEnableMenuItem
( MenuWinHandle        : HWND;
  ItemId               : CARDINAL;
  Enable               : BOOLEAN
)                      : BOOLEAN;
VAR
  MParam1              : MPARAM;
  MParam2              : MPARAM;
  MResult              : LONGCARD;
BEGIN
  MParam1 := MPFROM2SHORT( ItemId, LONG( SHORTCARD( TRUE ) ) );
  IF Enable THEN
    MParam2 := MPFROM2SHORT( MIA_DISABLED, 0 );
  ELSE
    MParam2 := MPFROM2SHORT( MIA_DISABLED, MIA_DISABLED );
  END;
  MResult := WinSendMsg( MenuWinHandle, MM_SETITEMATTR, MParam1, MParam2 );
  RETURN (MResult <> 0);
END WinEnableMenuItem;


PROCEDURE WinIsMenuItemEnabled
( MenuWinHandle        : HWND;
  ItemId               : CARDINAL
)                      : BOOLEAN;
VAR
  MParam1              : MPARAM;
  MParam2              : MPARAM;
  MResult              : LONGCARD;
BEGIN
  MParam1 := MPFROM2SHORT( ItemId, LONG( SHORTCARD( TRUE ) ) );
  MParam2 := MPFROMLONG( MIA_DISABLED );
  MResult := WinSendMsg( MenuWinHandle, MM_QUERYITEMATTR, MParam1, MParam2 );
  RETURN (MResult <> 0);
END WinIsMenuItemEnabled;


PROCEDURE WinSetMenuItemText
( MenuWinHandle        : HWND;
  ItemId               : CARDINAL;
  Text                 : ARRAY OF CHAR
)                      : BOOLEAN;
VAR
  MParam1              : MPARAM;
  MParam2              : MPARAM;
  MResult              : LONGCARD;
BEGIN
  MParam1 := MPFROMLONG( LONG( ItemId ) );
  MParam2 := MPFROMP( SYSTEM.ADR( Text ) );
  MResult := WinSendMsg( MenuWinHandle, MM_SETITEMTEXT, MParam1, MParam2 );
  RETURN (MResult <> 0);
END WinSetMenuItemText;


PROCEDURE WinIsMenuItemValid
( MenuWinHandle        : HWND;
  ItemId               : CARDINAL
)                      : BOOLEAN;
VAR
  MParam1              : MPARAM;
  MParam2              : MPARAM;
  MResult              : LONGCARD;
BEGIN
  MParam1 := MPFROM2SHORT( ItemId, LONG( SHORTCARD( TRUE ) ) );
  MParam2 := MPFROMLONG( LONG( LONG( SHORTCARD( FALSE ) ) ) );
  MResult := WinSendMsg( MenuWinHandle, MM_ISITEMVALID, MParam1, MParam2 );
  RETURN (MResult <> 0);
END WinIsMenuItemValid;

END WINMENU.
