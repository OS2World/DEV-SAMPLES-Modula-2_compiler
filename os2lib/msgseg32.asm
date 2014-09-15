_MSGSEG32	segment dword use32 public 'CODE'	;size is 48
FLAT	group	
	extrn	DOS32TRUEGETMESSAGE
	extrn	DOS32IQUERYMESSAGECP

	public	DOSGETMESSAGE,DosGetMessage,DOS32GETMESSAGE,Dos32GetMessage
	public	sig32
_MSGSEG32	segment
sig32:
		dec	dword ptr 053h[EBP]
		inc	EDI
		push	EBX
		inc	EBP
		inc	EDI
		xor	ESI,[EDX]
		add	[ECX],AL
		add	byte ptr [EAX],0
		add	[EAX],AL
		add	[EAX],AL
DOSGETMESSAGE:
DosGetMessage:
DOS32GETMESSAGE:
Dos32GetMessage:
		lea	EAX,sig32
		push	EAX
		push	EBP
		mov	EBP,ESP
		mov	EAX,4[EBP]
		xchg	EAX,8[EBP]
		mov	4[EBP],EAX
		pop	EBP
		jmp	DOS32TRUEGETMESSAGE
		inc	dword ptr [EAX]
		add	BH,BH
		inc	dword ptr [EAX]
_MSGSEG32	ends
	end
