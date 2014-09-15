IMPLEMENTATION MODULE WINLISTBOXES;

(************************************************************************
  OS/2 2.0 functions for Presentation Manager List box controls.

  Copyright (c) 1992 by Juergen Neuhoff
*************************************************************************)

(*$XL+       Modula-2 language extensions: '_' allowed for symbol names *)
(*$CDECL+    C-style procedures                                         *)
(*$A         default alignment for record fields                        *)

IMPORT SYSTEM;
FROM   OS2DEF        IMPORT HWND;
FROM   WINDEF        IMPORT MPARAM, MPFROMLONG, MPFROMP;
FROM   WINDEF        IMPORT MPFROM2SHORT, MPFROMSHORT;
FROM   WINMESSAGEMGR IMPORT WinSendMsg;


PROCEDURE WinDeleteLboxItem
( ListBoxWinHandle       : HWND;
  Index                  : LONGINT
)                        : LONGINT;
VAR
  MResult                : LONGCARD;
  MParam1                : MPARAM;
  MParam2                : MPARAM;
BEGIN
  MParam1 := MPFROMLONG( Index );
  MParam2 := NIL;
  MResult := WinSendMsg( ListBoxWinHandle, LM_DELETEITEM, MParam1, MParam2 );
  RETURN VAL( LONGINT, MResult );
END WinDeleteLboxItem;


PROCEDURE WinInsertLboxItem
( ListBoxWinHandle       : HWND;
  Index                  : LONGINT;
  Text                   : ARRAY OF CHAR
)                        : LONGINT;
VAR
  MResult                : LONGCARD;
  MParam1                : MPARAM;
  MParam2                : MPARAM;
BEGIN
  MParam1 := MPFROMLONG( Index );
  MParam2 := MPFROMP( SYSTEM.ADR( Text ) );
  MResult := WinSendMsg( ListBoxWinHandle, LM_INSERTITEM, MParam1, MParam2 );
  RETURN VAL( LONGINT, MResult );
END WinInsertLboxItem;


PROCEDURE WinQueryLboxCount
( ListBoxWinHandle       : HWND
)                        : LONGINT;
VAR
  MResult                : LONGCARD;
BEGIN
  MResult := WinSendMsg( ListBoxWinHandle, LM_QUERYITEMCOUNT, NIL, NIL );
  RETURN VAL( LONGINT, MResult );
END WinQueryLboxCount;


PROCEDURE WinQueryLboxItemText
( ListBoxWinHandle       : HWND;
  Index                  : LONGINT;
  VAR Text               : ARRAY OF CHAR;
  TextMax                : LONGINT
)                        : LONGINT;
VAR
  MResult                : LONGCARD;
  MParam1                : MPARAM;
  MParam2                : MPARAM;
BEGIN
  MParam1 := MPFROM2SHORT( SHORT( Index ), SHORT( TextMax ) );
  MParam2 := MPFROMP( SYSTEM.ADR( Text ) );
  MResult := WinSendMsg( ListBoxWinHandle, LM_QUERYITEMTEXT, MParam1, MParam2 );
  RETURN VAL( LONGINT, MResult );
END WinQueryLboxItemText;


PROCEDURE WinQueryLboxItemTextLength
( ListBoxWinHandle       : HWND;
  Index                  : LONGINT
)                        : INTEGER;
VAR
  MResult                : LONGCARD;
  MParam1                : MPARAM;
BEGIN
  MParam1 := MPFROMSHORT( SHORT( Index ) );
  MResult := WinSendMsg( ListBoxWinHandle, LM_QUERYITEMTEXTLENGTH, MParam1, NIL );
  RETURN SHORT( VAL( LONGINT, MResult ) );
END WinQueryLboxItemTextLength;


PROCEDURE WinSetLboxItemText
( ListBoxWinHandle       : HWND;
  Index                  : LONGINT;
  Text                   : ARRAY OF CHAR
)                        : BOOLEAN;
VAR
  MResult                : LONGCARD;
  MParam1                : MPARAM;
  MParam2                : MPARAM;
BEGIN
  MParam1 := MPFROMLONG( Index );
  MParam2 := MPFROMP( SYSTEM.ADR( Text ) );
  MResult := WinSendMsg( ListBoxWinHandle, LM_SETITEMTEXT, MParam1, MParam2 );
  RETURN (VAL( SHORTCARD, VAL( LONGCARD, MResult ) ) <> 0);
END WinSetLboxItemText;


PROCEDURE WinQueryLboxSelectedItem
( ListBoxWinHandle       : HWND
)                        : LONGINT;
VAR
  MResult                : LONGCARD;
  MParam1                : MPARAM;
BEGIN
  MParam1 := MPFROMLONG( LIT_FIRST );
  MResult := WinSendMsg( ListBoxWinHandle, LM_QUERYSELECTION, MParam1, NIL );
  RETURN VAL( LONGINT, MResult );
END WinQueryLboxSelectedItem;


END WINLISTBOXES.
