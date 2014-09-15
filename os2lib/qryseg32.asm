_MSGSEG32	segment dword use32 public 'CODE'	;size is 47
FLAT	group	
	extrn	DOS32IQUERYMESSAGECP
	extrn	sig32

	public	DOS32QUERYMESSAGECP,Dos32QueryMessageCP,DOSQUERYMESSAGECP,DosQueryMessageCP
_MSGSEG32	segment
DOS32QUERYMESSAGECP:
Dos32QueryMessageCP:
DOSQUERYMESSAGECP:
DosQueryMessageCP:
		mov	EAX,[ESP]
		push	EAX
		push	EBP
		mov	EBP,ESP
		mov	EAX,0Ch[EBP]
		mov	8[EBP],EAX
		mov	EAX,010h[EBP]
		mov	0Ch[EBP],EAX
		mov	EAX,014h[EBP]
		mov	010h[EBP],EAX
		mov	EAX,018h[EBP]
		mov	014h[EBP],EAX
		lea	EAX,sig32
		mov	018h[EBP],EAX
		pop	EBP
		jmp	DOS32IQUERYMESSAGECP
		inc	dword ptr [EAX]
_MSGSEG32	ends
	end
