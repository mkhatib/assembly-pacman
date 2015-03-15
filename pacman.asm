;	Pacman Assembly 8086 Project 2008
;	Done By : Mohammad Na'em Khatib - @mkhatib7
.Model 	Small
.Stack 1000
.Data
;=====================================================================================================
;=					Program Variables , Very Easy Configuration							=
;=====================================================================================================
; Speed Of The Game  ( Change This if it's too slow decrease it , else if it's too fast increase it )
	;speed dw 12000   	; this was for the earlier delay that depends on the computer ( not recomended ) . i replaced it with the new one, which depends only on the system clock !
	pacmanSpeed db 10
; The Score at which the user WINS !
	finalScore db 30 ; 30 dots to win ;)
; Current Postion of the pacman
	bmLocY db 02h
	bmLocX db 19h
; The key Controls
	right db 77
	left db 75
	up db 72
	down db 80
	quit db 1bh;'q'
	restart db 'r'
; The Eatable Points LookLike 3h for heart
	dot db 3h ; '.'
	mine db 15

; For Timer ; this is like flags for timer to be working on... don't play with them
	t1 db 0
	t2 db 0
	td db 0
; Strings To Be Displayed for the user
	helpTitle db "Help"
	help1 db "1.Use Arrows To Move",0
	help1Len dw 21
	help2 db "2.Point for Each Dot",0
	help2Len dw 21
	help3 db "3.'r' Repeate Game.",0
	help3Len dw 19
	help4 db "4.ESC to Exit",0
	help4Len dw 14
	scoreMsg db "Score  0000",0
	scoreMsgLen dw 11
	timeMsg db 	"Time   00:00",0
	timeSeperater db ':'
	timeMsgLen dw 12
	exitMsg db "      Thanks For Playing!",10,13
	exitMsgLen dw 27
	doneByMsg db "Done By MohaMMad Na'eM khatib ",1h,10,13,10,13,10,13,10,13,10,13,10,13,10,13,10,13,10,13,10,13,10,13,10,13,10,13
	doneByMsgLen dw 55
	levelCompleteMsg db "!!! CONGRATULATIONS !!!"
	levelCompleteMsgLen dw 23
	livesMsg db "Lives   ", 01,00,01,00,01
	livesMsgLen dw 8
	gameOverMsg db "     !!! GAME OVER !!!"
	gameOverMsgLen dw 22
	pressAnyKeyMsg db "Press Any Key To Continue..."
	pressAnyKeyMsgLen dw 28
; Level Positions
	level1Positions dw 1030h,113CH,061AH,0737h,0549h,1149h
					dw 1019h,1122h,0623h,0727h,034Ch,1242h
					dw 0519h,0936h,101Fh,062AH,0949h,074Ch
					dw 021DH,0924h,1035h,063AH,072Fh,0943h
					dw 021Fh,0530h,092DH,061DH,071CH,0C42h
; Double Number of the dots
	doubleNumOfDots dw 60;192
; Level Mines & a temp copy for repositioning them
	level1Mines			dw 1045h,0330H,071FH,0732h,0242h
						dw 0824h,1327h,0F1Ch,0D4Dh,0233h
	level1MinesCopy		dw 1045h,0330H,071FH,0732h,0242h
						dw 0824h,1327h,0F1Ch,0D4Dh,0233h
; Double Number of mines
	level1MinesNum dw 20
; this is the range allowed to be added !
	;boardBeginingXY dw 			0219H	   		024DH
	;boardEndingXY dw 			1419H			144DH
; Level Blocks the begining of the block , to the end of the block
	level1BeginingBlocks	dw 0240h,0D40h,0C19h,0425h,0B25h,1125h,0425h,082Dh,0919h
	level1EndingBlocks		dw 0941h,1441h,0C36h,0B26h,1126h,1136h,043Bh,0832h,091Fh
; Double Number of the blocks array elements
	level1BlocksNum	dw	18
; A flag to indecate the ending of the game
	finishFlag db 0

; A flag to indicate the Current Direction
	currentDirection db 0
; Initial Position For the Pacman ( here the top left most of the play yard)
	initialbmLocY db 02h
	initialbmLocX db 19h
; For Scoring Counting ; Four Bits
	score db 48,48,48,48
	scoreInteger db 0 ; the score in integer
; For Timing Counting ; Four Bits MM:SS
	time db 48,48,48,48
; Begining and Ending of Areas for easy configrubale
; ========== Game Theme ===================
; Packman Face :)
	pacManFace db 1h ; :) smily face
	pacManColor db 0Eh ; Yellow
	dotsColor db 00Bh ; LightBlue
	minesColor db 0Ch ; Red
	grase db 176 ; The Green Grase
	graseL dw 1
	graseColor db 0Ah ; Green
	borderColor db 10h ; Blue

	helpAreaColor db 08FH
	boardBeginingXY dw 0118H
	boardEndingXY dw 154EH
	scoreAreaBeginingXY dw 0100H
	scoreAreaEndingXY dw 0315H
	livesAreaBeginingXY dw 0500H
	livesAreaEndingXY dw 0715H
	timeAreaBeginingXY dw 0900H
	timeAreaEndingXY dw 0B15H
	helpAreaBeginingXY dw 0D00H
	helpAreaEndingXY dw 1515H
; Lives 51 - 48 = 3 Lives for the user
	lives db 51
	scoreAreaColor db ?
	timeAreaColor db ?

; For Keeping A Copy Of the level positions
	levelTmpMines			dw	(?)
	levelTmpPositions 		dw 	(?)
; ============================================ End Of Variables Area ===============================================
	;=====================================================================================================
	;=							Start Of The Code  								=
	;=====================================================================================================
.Code
.startup
; Hiding The Text Cursor
	mov     ah, 1
	mov     ch, 2bh
	mov     cl, 0bh
	int     10h
;wait Any Key
    CALL cls 				; clear the screen
	CALL buildHelpArea 		;  Help Area
	call buildInitialScreen

	mov ah, 00h
	int 16h
; Copying the position for later use
	CALL copyPositions
	restartGame:
		mov lives, 51
	begining:
        CALL cls 				; clear the screen
		CALL buildBoard  		; Main Area & Border
		call putBlocks			; Blocks
		CALL buildScoreArea 	; Score Area
		CALL buildLivesArea 	;  Help Area
		CALL buildTimeArea 		; Timer Area
		CALL buildHelpArea 		;  Help Area
		call fillBoard			; Put Dots
		call putMines			; Mines ( BOMBS)
		CALL putPacMan			; pica at last :)
	waitActionMain:
 		MOV AH,06h
		MOV DL,0FFh
		INT 21h
		jz waitActionMain
		CALL changeDirection
exit:
	CALL exitArea
	MOV AH,4CH
	INT 21h

;====================================================================================================
;================================		buildInitialScreen	 	==========================================
proc buildInitialScreen
	MOV AH,06H
	MOV AL,00H
	MOV BH,0C4H
	MOV CX,0D1AH
	MOV DX,1549H
	INT 10H

	MOV AH,06H
	MOV AL,00H
	MOV BH,0E0H
	MOV CX,0E1BH
	MOV DX,1448H
	INT 10H
	MOV BX,CX
	add BX,0101h
	CALL setXY
	MOV DX, offset DoneByMsg
	MOV CX, DoneByMsgLen
	CALL printLine
	CALL setXY
	MOV DX, offset exitMsg
	MOV CX, exitMsgLen
	CALL printLine
	add BX,0211h
	CALL setXY
	MOV DX, offset pressAnyKeyMsg
	MOV CX, pressAnyKeyMsgLen
	CALL printLine
	RET
buildInitialScreen endp
;====================================================================================================
;=================================	Copy Positions  	=============================================
Proc copyPositions
	push di
	push bx
	SUB DI,DI
	@@copyLoop:
		MOV  BX,[level1Positions+DI]
		MOV [levelTmpPositions+DI],BX
		ADD DI, 2
		CMP DI,doubleNumOfDots
		JNE @@copyLoop
	pop bx
	pop di
	RET
copyPositions EndP
Proc reCopyPositions
	push di
	push bx
	SUB DI,DI
	@@copyLoop2:
		MOV  BX,[leveltmpPositions+DI]
		MOV [level1Positions+DI],BX
		ADD DI, 2
		CMP DI,doubleNumOfDots
		JNE @@copyLoop2
	pop bx
	pop di
	RET
reCopyPositions EndP
Proc reCopyMines
	push di
	push bx
	SUB DI,DI
	@@copyLoopMines2:
		MOV  BX,[level1MinesCopy+DI];[levelTmpMines+DI]
		MOV [level1Mines+DI],BX
		ADD DI, 2
		CMP DI,level1MinesNum
		JNE @@copyLoopMines2
	pop bx
	pop di
	RET
reCopyMines EndP
;====================================================================================================
;================================		Fill Board	 	==========================================
proc fillBoard
	sub si,si ; si = 0
@@putDots:
		MOV BX,[level1Positions+SI] ; get the position from the array
		CALL setXY	; set the corosponding cursor position
		MOV CX,BX
		MOV DX,BX
		MOV AH,06H      ; Scroll Pageup
		MOV AL,00H      ; Lines To Scroll
		MOV BH,dotsColor
		INT 10H
		ADD SI,2
		MOV AH,06h
		MOV DL ,dot
		INT 21h		; print a dot
		CMP SI,doubleNumOfDots
		JNE @@putDots	; if there is more dots repeat
		ret
endp
;====================================================================================================
;====================================		Put Mines	 	=======================================
proc putMines
	sub si,si
@@putMines:
		MOV BX,[level1Mines+SI]
		CALL setXY
		MOV CX,BX
		MOV DX,BX
		MOV AH,06H      ; Scroll Pageup
		MOV AL,00H      ; Lines To Scroll
		MOV BH,minesColor
		INT 10H
		ADD SI,2
		MOV AH,06h
		MOV DL ,mine
		INT 21h 	; print a mine
		CMP SI,level1MinesNum
		JNE @@putMines	; if there is more Mines repeat
		ret
endp putMines
;====================================================================================================
;====================================		Put Blocks	 	==========================================
proc putBlocks
	sub si,si
@@putBlocks:
		MOV CX,[level1BeginingBlocks+SI]	; bring the start xy for the block
		MOV DX,[level1EndingBlocks+SI]		; the end xy of the block
		MOV AH,06H      ;scroll pageup
		MOV AL,00H      ;lines to scroll
		MOV BH,borderColor
		INT 10H		; draw block from begining to end !
		ADD SI,2
		CMP SI,level1BlocksNum
		JNE @@putBlocks
	ret
endp
;====================================================================================================
;====================================		Wait Action  	==========================================
proc waitActionToMove
@@waitAction:
		call incTimer
		MOV AH,06h
		MOV DL,0FFh
		INT 21h
		ret
waitActionToMove endp
;====================================================================================================
;====================================		Move Right  	==========================================
Proc moveRight
@@moveRight2:
		call canMoveRight? ; Check if u can move right returns 1 in dx if false
		cmp dx,1
		JE @@dontMoveRight
		CALL pmRemove	; clear the current position for the pacman
		INC bmLocX		; increment it's x position
		CALL search		; search if the new position is a DOT position OR a MINE position and increment the score if dot , or decrement lives if mine
		CALL putPacMan	; print the pacman in the new position
		call delay		; for slowing the pacman motion
		CALL incTimer	; increment timer depending on the timer flags if set then it will be incremented by one
		MOV AH,06h		; seek for an input
		MOV DL,0FFh
		INT 21h
		JZ @@moveRight2	; No input, Keep Moving Right
		cmp al,quit		; if pressed the quit button .. Exit
		je @@exitRight
@@dontMoveRight:
		call waitActionToMove ; ask for an input
@@return1:
		RET
@@exitRight:
	jmp exit
moveRight EndP
;====================================================================================================
;====================================		Can Move Right	  	==================================
proc canMoveRight?
		sub dx,dx				; dx = 0
		MOV BL, bmLocX
		mov AX, boardEndingXY
		sub al,2
		CMP BL, al				; is it out of bounds
		JG 	@@canNotMoveRight	; if yes then you cant move right any more set dx to 1 and return
		sub di,di
	; if the position is going to be inside a block, don't move else , keep moving
@@checkRightBlocks:
		mov ax,[level1BeginingBlocks+di]
		mov dx,[level1EndingBlocks+di]
		sub al,2
		cmp BL,al
		JLE 	@@ContinueCheckRight
		cmp BL,dl
		JG	 	@@ContinueCheckRight
		cmp bmLocY,ah
		JNGE	@@ContinueCheckRight
		cmp bmLocY,dh
		JNLE 	@@ContinueCheckRight
		jmp @@canNotMoveRight
	@@ContinueCheckRight:
		add di,2
		cmp di,level1BlocksNum
		jne @@checkRightBlocks
	@@canMoveRight:
		sub dx,dx
		ret
	@@canNotMoveRight:
		mov dx,1
		ret
canMoveRight? endp
;====================================================================================================
;====================================		Move Left	  	==========================================
Proc moveLeft
@@moveLeft2:
		call canMoveLeft?
		cmp dx,1
		je @@dontMoveLeft
		CALL pmRemove
		DEC bmLocX
		CALL search
		CALL putPacMan
		CALL delay
		CALL incTimer
		MOV AH,06h
		MOV DL,0FFh
		INT 21h
		JZ @@moveLeft2
		cmp al,quit
		je @@exitLeft
@@dontMoveLeft:
	call waitActionToMove
@@return2:
		RET
@@exitLeft:
	jmp exit
moveLeft EndP
;====================================================================================================
;====================================		Can Move LEft	  	==========================================
proc canMoveLeft?
		sub dx,dx
		MOV BL, bmLocX
		mov AX, boardBeginingXY
		add al,2
		cmp bl,al
		JL @@canNotMoveLeft
		sub di,di
@@checkLeftBlocks:
		mov ax,[level1BeginingBlocks+di]
		mov dx,[level1EndingBlocks+di]
		add dl,1
		cmp BL,dl
		JG 	@@ContinueCheckLeft
		cmp BL,al
		JL 	@@ContinueCheckLeft
		cmp bmLocY,ah
		JNGE	@@ContinueCheckLeft
		cmp bmLocY,dh
		JNLE 	@@ContinueCheckLeft
		jmp @@canNotMoveLeft
	@@ContinueCheckLeft:
		add di,2
		cmp di,level1BlocksNum
		jne @@checkLeftBlocks
	@@canMoveLeft:
		sub dx,dx
		ret
	@@canNotMoveLeft:
		mov dx,1
		ret
canMoveLeft? endp

;====================================================================================================
;====================================		Move UP	 	==========================================
Proc moveUp
@@moveUp2:
		call canMoveUp?
		cmp dx,1
		je @@dontMoveUp
		CALL pmRemove
		DEC bmLocY
		CALL search
		CALL putPacMan
		CALL delay
		CALL incTimer
		MOV AH,06h
		MOV DL,0FFh
		INT 21h
		JZ @@moveUp2
		cmp al,quit
		je @@exitUp
@@dontMoveUp:
	call waitActionToMove
@@return3:
		RET
@@exitUp:
	jmp exit
moveUp EndP
;====================================================================================================
;====================================		Can Move Up	  	=================================
proc canMoveUp?
		sub dx,dx
		MOV BH, bmLocY
		mov ax, boardBeginingXY
		add ah,2
		CMP BH, ah
		JL @@canNotMoveUp
		sub di,di
@@checkUpBlocks:
		mov ax,[level1BeginingBlocks+di]
		mov dx,[level1EndingBlocks+di]
		add dh,1
		cmp bmLocY,dh
		JG	@@ContinueCheckUp
		cmp bmLocY,ah
		JL	@@ContinueCheckUp
		cmp bmLocX,al
		JL	@@ContinueCheckUp
		cmp bmLocX,dl
		JG 	@@ContinueCheckUp
		jmp @@canNotMoveUp
	@@ContinueCheckUp:
		add di,2
		cmp di,level1BlocksNum
		jne @@checkUpBlocks
	@@canMoveUp:
		sub dx,dx
		ret
	@@canNotMoveUp:
		mov dx,1
		ret
canMoveUp? endp
;====================================================================================================
;====================================		Move Down  	=======================================
Proc moveDown
@@moveDown2:
		call canMoveDown?
		cmp dx,1
		je @@dontMoveDown
		CALL pmRemove
		INC bmLocY
		CALL search
		CALL putPacMan
		call delay
		CALL incTimer
		MOV AH,06h
		MOV DL,0FFh
		INT 21h
		JZ @@moveDown2
		cmp al,quit
		je @@exitDown
@@dontMoveDown:
	call waitActionToMove
@@return:
		RET
@@exitDown:
	jmp exit
moveDown EndP
;====================================================================================================
;====================================		Can Move Down	  	=================================
proc canMoveDown?
		sub dx,dx
		MOV BH, bmLocY
		mov ax, boardEndingXY
		sub ah,2
		CMP BH, ah
		JG @@canNotMoveDown
		sub di,di
@@checkDownBlocks:
		mov ax,[level1BeginingBlocks+di]
		mov dx,[level1EndingBlocks+di]
		sub ah,1
		cmp bmLocY,ah
		JL	@@ContinueCheckDown
		cmp bmLocY,dh
		JG	@@ContinueCheckDown
		cmp bmLocX,al
		JL	@@ContinueCheckDown
		cmp bmLocX,dl
		JG 	@@ContinueCheckDown
		jmp @@canNotMoveDown
	@@ContinueCheckDown:
		add di,2
		cmp di,level1BlocksNum
		jne @@checkDownBlocks
	@@canMoveDown:
		sub dx,dx
		ret
	@@canNotMoveDown:
		mov dx,1
		ret
canMoveDown? endp
;====================================================================================================
;=========================================		initialize	 ======================================
; this method is called before restarting the game , for reinitiallizing the game to its initial conditions and positions
proc initialize
	sub di,di
@@zeroScore:
	mov [score+di],48	; zero the score
	mov [time+di],48	; zero the timer
	inc di
	cmp di, 4
	jne @@zeroScore
	mov scoreInteger,0		; zero the score integer
	mov dl,initialbmLocY 	; return the pacman to it's inital position
	mov dh,initialbmLocX
	mov bmLocY,dl
	mov bmLocX,dh
	mov currentDirection,0	; clear the current direction flag ( the pacman is not moving)
	CALL reCopyPositions	; Copy the positions for redistributing them
	CALL reCopyMines		; copy mines
	ret
initialize endp
;====================================================================================================
;=========================================	Change Direction 	==========================================
Proc changeDirection
@@WAIT:
		CMP AL,restart
		Je @@restartGame
		CMP AL,quit
		JE @@exit
		CMP AL,right
		JE @@changeToRight
		CMP AL,left
		JE @@changeToLeft
		CMP AL,up
		JE @@changeToUp
		CMP AL,down
		JE @@changeToDown
		cmp currentDirection,0
		je @@stillTheFirstMove
		MOV al,currentDirection
		JMP @@wait
@@restartGame:
	call initialize
	jmp restartGame
	ret
@@begining:
	call initialize
	jmp begining
	ret
@@stillTheFirstMove:
	call waitActionToMove
	jmp @@WAIT
@@changeToLeft:
	MOV currentDirection,AL
	CALL moveLeft
	jmp @@WAIT
	RET
@@changeToRight:
	MOV currentDirection,AL
	CALL moveRight
	jmp @@WAIT
	RET
@@changeToUp:
	MOV currentDirection,AL
	CALL moveUp
	jmp @@WAIT
	RET
@@changeToDown:
	MOV currentDirection,AL
	CALL moveDown
	jmp @@WAIT
	RET
@@doNth2:
	SUB AL,AL
	RET
@@exit:
	CALL exitArea
	MOV AH,4CH
	INT 21h
	RET
changeDirection EndP
;====================================================================================================
;=========================================	Put Pacman ===============================================
Proc putPacMan
	MOV BH,bmLocY
	MOV BL,bmLocX
	CALL setXY
	MOV AH,06H
	MOV AL,00H
	MOV BH,pacManColor
	mov ch,bmLocY	; put the pacman on it's position
	mov dh,bmLocY
	mov cl,bmLocX
	mov dl,bmLocX
	INT 10H
	MOV DL ,pacManFace
	MOV AH,06h ; print it
	INT 21h
	RET
putPacMan EndP
;====================================================================================================
;=========================================	Search	===============================================
Proc search
	SUB DI,DI
	MOV BH,bmLocY
	MOV BL,bmLocX
	@@searchLoopForDots:	; search if it's a dot , increment the score if found
		CMP  BX,[level1Positions+DI]
		JE @@eatDot
		ADD DI, 2
		CMP DI,doubleNumOfDots
		JNE @@searchLoopForDots
	SUB DI,DI
	@@searchLoopForMines:	; search if it's a mine , decrement lives and repeat the game if found
		CMP  BX,[level1Mines+DI]
		JE @@eatMine
		ADD DI, 2
		CMP DI,level1MinesNum
		JNE @@searchLoopForMines

	JMP @@doNth ; if nothing has been found
	@@eatDot:
		MOV [level1Positions+DI],0
		CALL incScore
		ret
	@@eatMine:
		MOV [level1Mines+DI],0
		CALL decLives
	@@doNth:
		RET
	RET
search EndP
;====================================================================================================
;=========================================	Score 	=============================================
Proc incScore
	MOV DI,4
@@incProcess:
	DEC DI
	CMP [score+DI],'9'
	JE @@zeroCurrent
	JMP @@incCurrent
@@zeroCurrent:
	MOV [score+DI],'0'
	JMP @@incProcess
@@incCurrent:
	INC [score+DI]
	MOV BX, scoreAreaBeginingXY
	add bx,0108h
	CALL setXY
	MOV DX, offset score
	MOV CX, 4
	CALL printLine
	inc scoreInteger
	mov dh,finalScore
	cmp scoreInteger,dh
	je @@levelComplete
	RET
@@levelComplete:
	call levelComplete
	;mov finishFlag,1
	RET
incScore EndP
;====================================================================================================
;=========================================	Lives	===============================================
Proc decLives
	dec lives
	cmp lives,48 	; if lives is zero game is over
	je @@gameOver
	call initialize
	jmp begining	; repeate the game
@@gameOver:
	call gameOver
	RET
decLives EndP
;====================================================================================================
;=========================================	Timer	 ===============================================
Proc incTimer
	cmp finishFlag,1 	; if the game is already finished
	je @@finishFlag		; go and exit the game
working:
	call oneSecond		; is it one second passed yet
	cmp td,1			; it will be if td is set to one
	jne @@dontIncrement1	; if not , don't increment .. if yes , increment timer
	MOV BX, 0508h
	CALL setXY
	MOV DI,4
@@incProcess2:
	DEC DI
	CMP DI,2
	JE @@CMP6
	CMP DI,0
	JE @@CMP6
@@CMP9:
	CMP [time+DI],'9'
	JE 	@@zeroCurrent2
	JMP @@incCurrent2
	JMP @@cont
@@CMP6:
	CMP [time+DI],'5'
	JE @@zeroCurrent2
	JMP @@incCurrent2
@@cont:
@@zeroCurrent2:
	MOV [time+DI],'0'
	JMP @@incProcess2
@@incCurrent2:
	CALL oneSecond
	INC [time+DI]
	MOV BX, timeAreaBeginingXY
	add bx, 0108h
	CALL setXY
	MOV DX, offset time
	MOV CX, 2
	CALL printLine
	MOV DX , offset timeSeperater
	MOV CX, 1
	CALL printLine
	MOV DX, offset [time+2]
	MOV CX, 2
	CALL printLine
@@dontIncrement1:
	RET
@@finishFlag:
	mov ah,4ch
	int 21h
ret
incTimer EndP
;====================================================================================================
;=========================================	Remove Pacman  ===========================================
Proc pmRemove
	MOV BH,bmLocY
	MOV BL,bmLocX
	CALL setXY
	MOV AH,06h
	MOV DL,' '	; print a space to clear the pacman previous position
	INT 21h
	RET
pmRemove EndP
;====================================================================================================
;=========================================	Build Board	 ===============================================
Proc buildBoard
	MOV AH,06H
	MOV AL,00H
	MOV BH,borderColor
	MOV CX , boardBeginingXY    ;the most up left corner
	MOV DX,boardEndingXY    	; the most down right corner
	INT 10H
	MOV AH,06H
	MOV AL,00H
	MOV BH,graseColor
	add cx, 0101h
	sub dx, 0101h
	INT 10H
	mov bx, boardBeginingXY
	add bx, 0101h
putGrase:
	call setXY
	add bx, 0100h
	mov dx, offset grase
	mov cx, graseL
	call printLine
	mov ax,boardEndingXY
	cmp bh, ah
	jl putGrase
	mov bh,02h
	add bx, 0001h
	mov ax,boardEndingXY
	cmp bl, al
	jl putGrase
	RET
buildBoard EndP
;====================================================================================================
;=========================================	Print Line	 ===============================================
; changes nothing
;	dx: offset message want to printed
;	cx: length of message to be printed
Proc printLine param:word
	PUSH BX
    MOV AH,40H
    MOV BX,0
    INT 21H
	POP BX
	RET
printLine EndP
;========================================================================================================
;======================================== SetXY =========================================================
; The BX holds the position where you want to put the cursor
Proc setXY
	PUSH BX
	MOV AH, 02H ; request set cursor
	MOV DX, BX
	MOV BH, 00 ; page number 0
	INT 10H ; call interrupt
	POP BX
	RET
setXY EndP
;========================================================================================================
;========================================= Clear Screen ===================================================
Proc cls
	MOV AH, 06h
	MOV BH, 7
	MOV CX, 0000h
	MOV DX, 184fh
	INT 10h
	RET
cls EndP
;========================================================================================================
;=========================================	Center Area  	======================================================
proc centerBoard
	MOV AH,06H
	MOV AL,00H
	MOV BH,004H
	MOV CX,071FH
	MOV DX,0E41H
	INT 10H

	MOV AH,06H
	MOV AL,00H
	MOV BH,0A0H
	MOV CX,0820H
	MOV DX,0D40H
	INT 10H
	MOV BX,CX
	add BX,0101h
	CALL setXY
	ret
centerBoard endp
;========================================================================================================
;=========================================	Exit Area  	======================================================
Proc exitArea
	call centerBoard
	MOV DX, offset exitMsg
	MOV CX, exitMsgLen
	CALL printLine
	INC BH
	INC BH
	CALL setXY
	MOV DX, offset DoneByMsg
	MOV CX, DoneByMsgLen
	CALL printLine
	RET
exitArea EndP
;========================================================================================================
;========================================== Level Complete =======================================================
proc levelComplete
	call centerBoard
	MOV BX,CX
	add BX,0203h
	CALL setXY
	MOV DX, offset levelCompleteMsg
	MOV CX, levelCompleteMsgLen
	CALL printLine
	add BX,0200h
	CALL setXY
	MOV DX, offset pressAnyKeyMsg
	MOV CX, pressAnyKeyMsgLen
	CALL printLine
	sub al,al
@@pressAnyKey:
	mov ah,06h
	mov dl,0FFh
	int 21h
	jz @@pressAnyKey
	cmp al, quit
	je @@exitGame
	call initialize
	jmp restartGame
	RET
@@exitGame:
	jmp exit
	ret
levelComplete endp
;========================================================================================================
;========================================== gameOver =======================================================
proc gameOver
	call centerBoard
	MOV BX,CX
	add BX,0203h
	CALL setXY
	MOV DX, offset gameOverMsg
	MOV CX, gameOverMsgLen
	CALL printLine
	add BX,0200h
	CALL setXY
	MOV DX, offset pressAnyKeyMsg
	MOV CX, pressAnyKeyMsgLen
	CALL printLine

	sub al,al
@@pressAnyKey2:
	mov ah,06h
	mov dl,0FFh
	int 21h
	jz @@pressAnyKey2
	cmp al, quit
	je @@exitGame2
	call initialize
	jmp restartGame
	RET
@@exitGame2:
	jmp exit
	ret
gameOver endp
;========================================================================================================
;==========================================Help Area=======================================================
Proc buildHelpArea
	MOV AH,06H
	MOV AL,00H
	MOV BH,helpAreaColor
	MOV CX,helpAreaBeginingXY
	MOV DX,helpAreaEndingXY
	INT 10H
	MOV BX,helpAreaBeginingXY
	add bx, 0101h
	CALL setXY

	MOV DX, offset helpTitle
	MOV CX, 4
	CALL printLine
	ADD BH,2
	CALL setXY
	MOV DX, offset help1
	MOV CX, help1Len
	CALL printLine
	ADD BH,1
	CALL setXY
	MOV DX, offset help2
	MOV CX, help2Len
	CALL printLine
	ADD BH,1
	CALL setXY
	MOV DX, offset help3
	MOV CX, help3Len
	CALL printLine
	ADD BH,1
	CALL setXY
	MOV DX, offset help4
	MOV CX, help4Len
	CALL printLine
	RET
buildHelpArea EndP
;========================================================================================================
;========================================== 	Score Area 	=========================================
Proc buildScoreArea
	MOV AH,06H
	MOV AL,00H
	MOV BH,4FH
	MOV CX,scoreAreaBeginingXY
	MOV DX,scoreAreaEndingXY
	INT 10H
	MOV BX, cx
	add bx,0101h
	CALL setXY

	MOV DX, offset scoreMsg
	MOV CX, scoreMsgLen
	CALL printLine
	RET
buildScoreArea EndP
;========================================================================================================
;========================================== 	Lives Area 	=========================================
Proc buildLivesArea
	MOV AH,06H
	MOV AL,00H
	MOV BH,3EH
	MOV CX,livesAreaBeginingXY
	MOV DX,livesAreaEndingXY
	INT 10H
	MOV BX, cx
	add bx,0101h
	CALL setXY
	MOV DX, offset livesMsg
	MOV CX, livesMsgLen
	CALL printLine
	mov ax,48
@@putLivesFaces:
	inc ax
	mov dx, offset  pacmanFace
	mov cx,1
	push ax
	CALL printLine
	pop ax
	cmp al,lives
	jl @@putLivesFaces
	RET
buildLivesArea EndP
;=================================================================================================
;========================================== 	Time Area   	=========================================
Proc buildTimeArea
	MOV AH,06H
	MOV AL,00H
	MOV BH,5FH
	MOV CX,timeAreaBeginingXY
	MOV DX,timeAreaEndingXY
	INT 10H
	MOV BX, cx
	add bx,0101h
	CALL setXY
	MOV DX, offset timeMsg
	MOV CX, timeMsgLen
	CALL printLine
	RET
buildTimeArea EndP
;========================================================================================================
;========================================== 	     The OldDelay 		=====================================
; delay22 Proc
    ; MOV CX,speed
    ; lup_del:
		; PUSH CX
		; MOV CX,speed
		; lup_del2:
		; LOOP lup_del2
		; POP CX
	; LOOP lup_del
; RET
; delay22 EndP
;========================================================================================================
;====== 	Delay Depends on the System Time only, so it will work at the same speed on any computer 		=========
delay Proc
	MOV     AH,2CH
	INT    	21h 	; get system time
	mov [si],dl	; dl contains 1/100 of the second
@@myDelay:
	INT 21h
	sub dl,[si]
	cmp dl,pacmanSpeed
	jb @@myDelay
	RET
delay EndP
;======================================================================================================
;========================================== 	     One Second 		===================================
Proc oneSecond
	PUSH 	CX BX AX DX
	MOV     BH,01h
	MOV     AH,2CH
	INT    	21h
	cmp 	t1,0		; if t1 is not 0 it's contains the seconds at specific moments ( keep the value and jump to label1)
	jne 	@@Label1	; else put in it the seconds of this specific moment
	MOV     td,0		; it's not indicated a second passed yet
	mov 	t1,dh		; t1, t2 = the seconds now
	MOV    	t2,DH
@@Label1:
	INT     21h			; take anothe time value
	SUB     DH,t2
	CMP     DH,BH		; if we don't have a second passed yet
	JB  	@@Label2	; return
	mov 	td,1		; else set the td flag thet there's a second passed and thus incrementing the timer
	mov 	t2,0		; zero both t2,t1 for seeking for a new second
	mov 	t1,0
@@Label2:
	POP 	DX AX BX CX
	RET
oneSecond EndP
;========================================================================================================
End
