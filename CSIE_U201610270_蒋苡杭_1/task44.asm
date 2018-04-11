.386
STACK	SEGMENT	USE16	STACK
		DB	200	DUP(0)
STACK	ENDS
DATA	SEGMENT	USE16
XUEHAO	DB	4 DUP(0)
DATA	ENDS
CODE	SEGMENT	USE16
		ASSUME CS: CODE, DS: DATA, SS:STACK
start:  MOV AX,DATA
        MOV DX,AX
		mov DI,OFFSET XUEHAO
        MOV BYTE PTR [SI],30H       ;立即寻址
		INC 	SI
        MOV BYTE PTR [SI],32H       ;立即寻址
        INC 	SI
        MOV BYTE PTR [SI],37H       ;立即寻址
        INC 	SI
        MOV BYTE PTR [SI],30H       ;立即寻址
        INC 	SI
        MOV		AH, 4CH
        INT		21H
CODE	ENDS
		END		START