.386
stack   segment use16   stack
        db  500 dup(0)
stack   ends
data segment use16
ctrl 	db  0ah, 0dh, '$'	;换行
info1   db  'please input username:$'  
info2   db  'please input password:$'  
info3   db  'wrong username or password!Input again!$'
info4   db  'enter the name of goods:$' 
info5   db  'fail to find goods!$' 
info6   db  'characters that are not a~z or A~Z are not allowed!$'
info7   db  ':price should not be negtive$'
info8	db	':imported goods should be more than sold goods$'
in_name db  10
		db  ?
		db  10 dup(0)
in_pwd  db  10
		db  ?
		db  10 dup(0)
in_good db  10
		db  ?
		db  10 dup(0)
bname	db	'yihang', 4 dup(0)		;字符串超过分配空间，十号调用会卡住	
bpass	db	'test', 6 dup(0)	
n		equ		15
s1		db	'shop1', 0		
ga1		db	'pen', 7 dup(0)	
		dw	35, 56, 10000, 1,?
ga2		db	'book', 6 dup(0)	;
		dw	15, 28, 45, 40,?
ga3		db	'pencil', 4 dup(0)	
		dw	8, 10, 102, 23,?
gan     db  n-3 dup('temp-value',15,0,20,0,30,0,2,0,?,?)
s2		db	'shop2', 0		
gb1		db	'book', 6 dup(0)	
		dw	8, 36, 56, 46,?
gb2		db	'pen', 7 dup(0)
		dw	13, 23, 2, 1,?
gb3		db	'pencil', 4 dup(0)	
		dw	12, 13, 108, 15,?
gbn     db  n-3 dup('temp-value',15,0,20,0,30,0,2,0,?,?)
auth    db  0	;身份证明变量，1为老板，0为客户
num		db  0
pro1	dd  0
pro2	dd  0
apr     dd  0
m       dw  ?
data ends

code 	segment		use16
		assume		CS:code, DS:data, SS:stack
start:	mov	ax, data
		mov ds, ax
		mov cx, 0
		mov bp, 0
;检查第一个商店数据的合理性
		mov si, offset ga1
		add si, 10
goods1:	
		mov ax, [si]      ;将进货价存入ax
		cmp ax, 0
		jl	warn3
        mov ax, [si+2]    ;销售价存入ax
		cmp ax, 0
		jl  warn3
		mov bx, [si+4]
		mov ax, [si+6]
		cmp bx, ax
		jl  warn4
		inc cx
		add si, 20
		cmp cx, n
		jnz goods1
		
;检查第二个商店数据的合理性		
		mov cx, 0
		mov si, offset gb1
		add si, 10
goods2:	mov ax, [si]      ;将进货价存入ax
		cmp ax, 0
		jl	warn3
        mov ax, [si+2]    ;进货总数存入ax
		cmp ax, 0
		jl  warn3
		mov bx, [si+4]
		mov ax, [si+6]
		cmp bx, ax
		jl  warn4
		inc cx
		add si, 20
		cmp cx, n
		jnz goods2
		
		mov cx, 0
		mov bp, 0
func1:	mov edx, 0		;006A
		mov pro1, edx
		mov pro2, edx
		mov auth, dl
		mov num, dl		;将num和auth初始化
		lea dx, info1	;提示用户输入用户名
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		
		lea dx, in_name
		mov ah, 10
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		
        mov cl, in_name+1	;获得字符串长度错误：cx的话变量类型不匹配
		mov ch, 0 
        cmp cx, 0			;用户只是输入了回车
		jz  func3			
		cmp BYTE PTR[in_name+2], 71H
		jz	quit
		mov di, 0			;用来比较姓名字符串时的计数
		mov si, 0
		jmp back
			
back:	lea dx, info2
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h

		lea dx, in_pwd
		mov ah, 10
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		mov di, 0
		mov si, 0
		mov cl, in_name+1
		mov ch, 0
		jmp func2
	
func2:  mov dl, [bname+di]	    ;变址寻址方式
		mov bl, [di+in_name+2]	;直接寻址
		cmp bl, 97
		jl  char1
		jmp char2
back2:	cmp bl, dl
		jnz warn1				;一个字符不同就进入提示部分
		inc di
		cmp cx, di				;cs0076
		jnz	func2				;如果不为0，说明字符串还没比较完
		;inc di
		cmp BYTE PTR[bname+di], 0	;比较网点定义的下一个个字符是否为0
		jnz warn1
		
		mov cl, in_pwd+1
		mov ch, 0
pass:	mov dl, [bpass+si]
		mov bl, [si+in_pwd+2]
		cmp bl, dl
		jnz warn1
		inc si
		cmp cx, si					;直接寻址和立即寻址
		jnz pass
		;inc si
		cmp BYTE PTR[bpass+si], 0	;比较最后是否为0
		jnz	warn1
		mov auth, 1
		mov bx, 0
		jmp func3	;00bc
		
warn1:	lea dx, info3	;提示登录失败
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		jmp func1
		
func3:	lea dx, info4
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		lea dx, in_good			;输入待查找的商品名称00cd
        mov ah, 10
        int 21h
		mov ax, 0
		call TIMER
		mov  cx, 2000
f3_2:	call locate				;0329
		mov	dx, n				;将dx置为商品个数用于循环
		mov si, offset ga1
f3_1:	call fresh	;0377
		dec  dx
		add  si, 20
		cmp  dx, 0
		jnz  f3_1
		dec	 cx
		cmp  cx, 0
		jz	 end1
		jmp	 f3_2

quit:	mov cl, in_name+1	;只有一个字符的情况下说明输入的就是q
		cmp cl, 1
		jz 	end1
		jmp back
		
end1:   mov  ax,1
		call TIMER
		mov ah,4ch
        int 21h
quit2:	lea dx , info5
        mov ah , 9
        int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		mov bx, 0
        jmp func3
		
;判断输入姓名的合法性		
char1:  cmp	bl, 90
		jg	warn2
		cmp	bl, 65
		jl  warn2
		jmp back2
char2:  cmp	bl, 122
		jg  warn2
		jmp back2
warn2:  lea dx, info6
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		jmp func1
		
ewarn3: inc bp
		jmp fwarn3
warn3:	sub si, 10
fwarn3:	cmp BYTE PTR[si+ds:[bp]], 0	
		jnz ewarn3
		mov BYTE PTR[si+ds:[bp]], '$'
		lea dx, [si]
		mov ah, 9
		int 21h
		lea dx, info7
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		mov BYTE PTR[si+ds:[bp]], 0		;恢复原状
		jmp end1
		
ewarn4: inc bp
		jmp fwarn4
warn4:	sub si, 10
fwarn4:	cmp BYTE PTR[si+ds:[bp]], 0
		jnz ewarn4
		mov BYTE PTR[si+ds:[bp]], '$'
		lea dx, [si]
		mov ah, 9
		int 21h
		lea dx, info8
		mov ah, 9
		int 21h
		lea dx, ctrl
		mov ah, 9
		int 21h
		mov BYTE PTR[si+ds:[bp]], 0   ;恢复原状
		jmp end1
;传入参数:si指向待寻找的那个商品
;带出参数:数据区的销售量增加1
;功能:检查进货量和销售量，销售量加一
goods	proc
		push ax
		push bx
		push si
		add si, 10;si指向数据区
		mov bx, [si+4]	;进货总量
		mov ax, [si+6]	;已售数量
		cmp ax, bx
		jnl  warn4
		inc WORD PTR[si+6]
		pop	si
		pop bx
		pop ax
		ret
		goods	endp
;利用寄存器di传出待寻找的那个商品的地址，传入的参数si中应该包括商品1或者商品2首地址
;根据商品一，找到商品二
find	proc
		push	bx
		push	ax
		mov bx, 0	;先用bx算出需要比较的字符数
		mov	bp, 0	;bp用于带出字符串长度
_f1:	inc bx
		mov	bp, bx
		cmp BYTE PTR[si+bx], 0
		jnz	_f1
_f2:	mov al, BYTE PTR[di+bx]
		mov ah, BYTE PTR[si+bx]
		cmp al, ah
		jnz	_f3	
		dec bx	
		cmp bx, 0
		jnl  _f2
		pop	ax
		pop bx
		ret
_f3:	add di, 20
		mov bx, bp
		jmp	_f2
		find	endp
;传入参数:无
;带出参数:无
;功能:从第一商店定位到寻找商品，为goods函数做铺垫
locate	proc
		push	ax
		push	cx
		push	bp
		push	dx
		push	bx
		mov	si, offset ga1	    ;在第一个网店寻找查询的商品名称
		mov bp, 0				;存储查找过的商品数目
		mov dl, in_good+1		;存放输入的商品名称的长度
		mov dh, 0
		mov WORD PTR bx, 0
		cmp dx, 0			;只输入回车，回到功能1   0180
		jz  func1
l_1:	mov cl, [si+bx]
		mov ch, 0
		mov al, BYTE PTR [bx+in_good+2]	;010B
		mov ah, 0
		cmp cx, ax
		jnz l_2
		inc bx
		cmp bx, dx
		jnz	l_1
		cmp BYTE PTR[si+bx], 0	;比较最后是否为0
		jnz l_2
		jmp	l_3
l_2:	add si, 20
		inc bp
		cmp bp, n
		jz  quit2
		mov bx, 0
		jmp l_1
l_3:	
		;si必然找到待寻找的那个商品
		call goods	;判断商品合法性
		pop	bx
		pop	dx
		pop	bp
		pop	cx
		pop	ax
		ret
		locate	endp
fresh  	proc	;更新si指向的商品的平均利润率
		push eax
		push ebx
		push ecx
		push edx
		push di
		add si, 10
		mov ax, [si+6]    ;已售数量
        cwde
        mov ecx, eax	  ;已售数量存入ecx  
        mov ax, [si+2]	  ;销售价
        cwde
        imul ecx, eax     ;销售价*已售数量
		imul ecx, 100
        mov ax, [si]      ;将进货价存入ax
        cwde			  ;扩展ax内的数据为32位
        mov ebx, eax      ;将进货价存入ebx中
        mov ax, [si+4]    ;进货总数存入ax
        cwde
        imul ebx, eax     ;进货价*进货总数
		cdq
		mov eax, ecx	;为除法做准备
        idiv ebx     	;算出利润放入eax中
        sub eax ,100	;简化运算过程
        mov pro1, eax
		sub si, 10		;保证si指向的是商品字符段
		mov di, offset	gb1
		call find		;di此时指向商店二中的同名商品
		add di, 10
		mov ax, [di+6]    ;已售数量
        cwde
        mov ecx, eax	  ;已售数量存入ecx  
        mov ax, [di+2]	  ;销售价
        cwde
        imul ecx, eax     ;销售价*已售数量
		imul ecx, 100
        mov ax, [di]      ;将进货价存入ax
        cwde			  ;扩展ax内的数据为32位
        mov ebx, eax      ;将进货价存入ebx中
        mov ax, [di+4]    ;进货总数存入ax
        cwde
        imul ebx, eax     ;进货价*进货总数
		cdq
		mov eax, ecx	;为除法做准备
        idiv ebx     	;算出利润放入eax中
        sub eax ,100	;简化运算过程
        mov pro2, eax
		;下面计算平均利润率
		mov eax,pro1	;商品1的利润
		add eax,pro2	;商品2的利润
		cdq				;注意扩充
		mov ebx, 2
		idiv ebx
		mov ecx, eax	;保存eax，一定要注意
		sub di, 10
		push	si
		mov si, di
		mov di, offset  ga1
		call find
		pop		si
		mov  [di+18], ax;di为重定位到shop1中的商品地址
		pop di
		pop edx
		pop ecx
		pop ebx
		pop eax
		ret
		fresh	endp
TIMER	PROC
	PUSH  DX
	PUSH  CX
	PUSH  BX
	MOV   BX, AX
	MOV   AH, 2CH
	INT   21H	     ;CH=hour(0-23),CL=minute(0-59),DH=second(0-59),DL=centisecond(0-100)
	MOV   AL, DH
	MOV   AH, 0
	IMUL  AX,AX,1000
	MOV   DH, 0
	IMUL  DX,DX,10
	ADD   AX, DX
	CMP   BX, 0
	JNZ   _T1
	MOV   CS:_TS, AX
_T0:	POP   BX
	POP   CX
	POP   DX
	RET
_T1:	SUB   AX, CS:_TS
	JNC   _T2
	ADD   AX, 60000
_T2:	MOV   CX, 0
	MOV   BX, 10
_T3:	MOV   DX, 0
	DIV   BX
	PUSH  DX
	INC   CX
	CMP   AX, 0
	JNZ   _T3
	MOV   BX, 0
_T4:	POP   AX
	ADD   AL, '0'
	MOV   CS:_TMSG[BX], AL
	INC   BX
	LOOP  _T4
	PUSH  DS
	MOV   CS:_TMSG[BX+0], 0AH
	MOV   CS:_TMSG[BX+1], 0DH
	MOV   CS:_TMSG[BX+2], '$'
	LEA   DX, _TS+2
	PUSH  CS
	POP   DS
	MOV   AH, 9
	INT   21H
	POP   DS
	JMP   _T0
_TS	DW    ?
 	DB    'Time elapsed in ms is '
_TMSG	DB    12 DUP(0)
TIMER   ENDP
code	ends
		end	start