IMPLEMENTATION MODULE THUNK;

(*************************************************************************
  32-bit OS/2 thunking layer support for 16-bit OS/2 kernel APIs.

  Copyright (c) 1995 by Juergen Neuhoff
**************************************************************************)


(*$XL+   Language extensions: '_' for symbol names, bitwise operators *)
(*$A1    Byte alignment for record fields                             *)


IMPORT SYSTEM;
IMPORT OS2DEF;
FROM   DOSMODULEMGR IMPORT DosQueryProcAddr, DosQueryProcType;
FROM   DOSMODULEMGR IMPORT PT_16BIT, PT_32BIT;
FROM   DOSPROCESS   IMPORT DosExit, DosBeep;
FROM   DOSPROCESS   IMPORT EXIT_PROCESS;


PROCEDURE Flat32ToFar16( Flat32 : SYSTEM.ADDRESS ) : SYSTEM.LONGWORD;
BEGIN
  (* Transform a flat 0:32 pointer into a far 16:16 pointer
     using the OS/2 LDT tiling mechanism
  *)
  RETURN
  (((Flat32 AND 0FFFF0000H) SHL 3)) + (7 SHL 16) + (Flat32 AND 0000FFFFH);
END Flat32ToFar16;


PROCEDURE Far16ToFlat32( Far16 : SYSTEM.DWORD ) : SYSTEM.ADDRESS;
VAR
  p : LONGCARD;
BEGIN
  (* Transform a far 16:16 pointer into a flat 0:32 pointer
     using the OS/2 LDT tiling mechanism
  *)
  p := Far16;
  RETURN ((p SHR 3) AND 0FFFF0000H) + (p AND 0000FFFFH)
END Far16ToFlat32;



PROCEDURE FindProc
(
  VAR PFunc : SYSTEM.LONGWORD;
  ORDFunc   : LONGCARD;
  DLLHandle : HMODULE
);
VAR
  rc        : OS2DEF.APIRET;
  TFunc     : LONGCARD;
BEGIN
  rc := DosQueryProcAddr( DLLHandle, ORDFunc, "", PFunc );
  IF rc = 0 THEN
    rc := DosQueryProcType( DLLHandle, ORDFunc, "", TFunc );
    IF rc = 0 THEN
      IF TFunc = PT_16BIT THEN
        PFunc := Flat32ToFar16( PFunc );
        RETURN;
      END;
    END;
  END;
  rc := DosBeep( 1000, 1000 );
  DosExit( EXIT_PROCESS, 1 );
END FindProc;


PROCEDURE StackAlign( pESP : SYSTEM.ADDRESS );
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    POP   EAX
    POP   ESP
    JMP   EAX
  );
END StackAlign;



PROCEDURE StackAlign2048();
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    POP   EAX
    AND   ESP, 0FFFFF800H
    JMP   EAX
  );
END StackAlign2048;



PROCEDURE StackAlignWORD();
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    POP   EAX
    AND   ESP, 0FFFFFFFEH
    JMP   EAX
  );
END StackAlignWORD;



PROCEDURE StackAlignDWORD();
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    POP   EAX
    AND   ESP, 0FFFFFFFCH
    JMP   EAX
  );
END StackAlignDWORD;



PROCEDURE StackGrow( Count : LONGCARD );
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    POP    EAX
    SUB    ESP, DWORD PTR [ ESP ]
    ADD    ESP, 4
    JMP    EAX
  );
END StackGrow;



PROCEDURE StackTop() : SYSTEM.ADDRESS;
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    MOV   EAX, ESP
    ADD   EAX, 4
    RETN
  );
END StackTop;



PROCEDURE PushDWORD( StackItem : SYSTEM.DWORD );
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    RETN
  );
END PushDWORD;



PROCEDURE PushWORD( StackItem : SYSTEM.WORD );
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    MOV   AX, WORD PTR [ESP+4]
    MOV   WORD PTR [ESP+6], AX
    POP   EAX
    ADD   ESP, 2
    JMP   EAX
  );
END PushWORD;


PROCEDURE Stack32To16();
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    PUSH  ESP
    CALL  Flat32ToFar16
    PUSH  EBX
    SUB   EAX, 4
    PUSH  EAX
    POP   BX
    POP   AX
    MOV   SS, AX
    MOVZX ESP, BX
    POP   EBX
    AND   EBP, 0000FFFFH
    RETN
  );
END Stack32To16;


PROCEDURE Stack16To32();
BEGIN
  SYSTEM.INLINE
  (
    LEAVE
    PUSH   EAX
    PUSH   EBX
    ROL    EBP, 16
    MOV    BP, SS
    SHR    BP, 3
    ROL    EBP, 16
    MOV    AX, SP
    SHL    EAX, 16
    MOV    AX, SS
    SHR    AX, 3
    ROL    EAX, 16
    MOV    BX, DS
    MOV    SS, BX
    MOV    ESP, EAX
    POP    EBX
    POP    EAX
    RETN
  );
END Stack16To32;



PROCEDURE CallFar16
(
  Target   : SYSTEM.DWORD;
  ParmSize : LONGCARD
)          : CARDINAL;
BEGIN
  SYSTEM.INLINE
  (
    ; First transform stack and registers as follows:
    ;
    ; < lo EBP caller  > <--EBP
    ; < hi EBP caller  >
    ; < lo EIP caller  >
    ; < hi EIP caller  >
    ; < lo ParmSize    >   ==============>  < IP Target       >
    ; < hi ParmSize    >                    < CS Target       >
    ; < IP Target      >                    < IP resume16     >
    ; < CS Target      >                    < CS resume16     >
    ; < Target param n >                    < Target param n  >
    ;         :                                     :
    ; < Target param 0 >                    < Target param 0  >
    ; <      _         >                    < lo EIP resume32 >
    ; <      _         >                    < hi EIP resume32 >
    ; <      _         >                    < CS     resume32 >
    ; <      _         >                    < 0000H           >
    ; <      _         >                    < lo EBX caller   >
    ; <      _         >                    < hi EBX caller   >
    ; <      _         >                    < lo ECX caller   >
    ; <      _         >                    < hi ECX caller   >
    ; <      _         >                    < lo EDX caller   >
    ; <      _         >                    < hi EDX caller   >
    ; <      _         >                    < lo EDI caller   >
    ; <      _         >                    < hi EDI caller   >
    ; <      _         >                    < lo ESI caller   >
    ; <      _         >                    < hi ESI caller   >
    ; <      _         >                    < lo EBP caller   >
    ; <      _         >                    < hi EBP caller   >
    ; <      _         >                    < lo EIP caller   >
    ; <      _         >                    < hi EIP caller   >
    ;
    ; Then make the transfer to the 16-bit target API using a RETF.
    ; Upon return (still in 16-bit mode) revert to 32-bit mode.
    ; Restore caller's registers and return to caller.
    ;

    ; switch to a 16-bit stack
    CALL  Stack32To16

    ; Save caller's registers, because target API might destroy them.
    MOV   ESP, EBP
    ADD   EBP, ParmSize[ EBP ]
    ADD   EBP, 16
    PUSH  ESI
    PUSH  EDI
    PUSH  EDX
    PUSH  ECX
    PUSH  EBX
    POP   DWORD PTR [ EBP+08 ]
    POP   DWORD PTR [ EBP+12 ]
    POP   DWORD PTR [ EBP+16 ]
    POP   DWORD PTR [ EBP+20 ]
    POP   DWORD PTR [ EBP+24 ]
    POP   DWORD PTR [ EBP+28 ]
    POP   DWORD PTR [ EBP+32 ]
    MOV   WORD PTR  [ EBP+06 ], 0000H
    MOV   WORD PTR  [ EBP+04 ], CS
    CALL  loadresume
    MOV   DWORD PTR [ EBP ], EAX
    ADD   ESP, 4

    ; transfer to 16:16 target API
    transfer:
    CALL  next
    next:
    POP   EAX                  ;1
    ADD   EAX, 31              ;5
    TEST  AL, 1                ;2    find a 16:16 resume-address with
    JZ    transfer2            ;6    at least 2 contiguous bytes not
    ADD   EAX, 3               ;5    crossing a 64 KB boundary
    transfer2:                 ;0
    PUSH  EAX                  ;1
    CALL  Flat32ToFar16        ;5
    XCHG  EAX, DWORD PTR [ESP] ;3
    PUSH  EAX                  ;1
    DB    66H                  ;1
    RETF                       ;1

    ; After 16:16 API call resume at one of the following 2 RETF instructions
    resume16:
    DB    66H                  ;1
    RETF                       ;1    transfer to resume32
    NOP                        ;1
    DB    66H                  ;1    transfer to resume32
    RETF                       ;1

    loadresume:
    CALL next2
    next2:
    POP  EAX                   ;1
    ADD  EAX,7                 ;5
    RETN                       ;1

    ;now back in flat 32-bit mode after 16:16 API call, restore regs, return
    resume32:
    CALL Stack16To32
    POP  EBX
    POP  ECX
    POP  EDX
    POP  EDI
    POP  ESI
    POP  EBP
    RETN
  );
END CallFar16;



END THUNK.
