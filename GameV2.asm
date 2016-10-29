IDEAL
MODEL small
STACK 100h

DATASEG
;Bitmap drawing system: load bitmap image data into memory and draw to VGA memory from data memory

;File info and graphics
;=-=-=-=-=-=-=-=-=-=-=-=
playerFile db 'llamav2.bmp', 0 ;nullbyte to terminate file name string
terrainFile db 'terrain2.bmp', 0
cactusFile db 'cactus2.bmp', 0
retryFile db 'retry.bmp', 0
retryButton db 34*60 dup (0) ;retry button graphic
player db 29*60 dup(0) ;player graphic
cactus db 60*49 dup(0) ;cactus graphic
terrain db 320 * 37 dup(0) ;terrain graphic
tmpHeader db 54 dup (0) ;bitmap header, temporary for bitmap initialisation
Palette db 1024 dup (0) ;bitmap palette buffer, all files use the same palette, no need for duplicates

;Player dimensions 
;=-=-=-=-=-=-=-=-=
playerHeight dw 29
playerWidth dw 60

;Player boundary box
;=-=-=-=-=-=-=-=-=-=
playerBoundXMin dw 0
playerBoundYMin dw 150
playerBoundXMax dw 6
playerBoundYMax dw 121

;Cactus boundary box
;=-=-=-=-=-=-=-=-=-=
cactusBoundXMax dw 300
cactusBoundXMin dw 290
cactusX dw 310
cactusY dw 124

;Terrain dimensions
;=-=-=-=-=-=-=-=-=-=
terrainX dw 0
terrainY dw 163

;Game info
;=-=-=-=-=
difficulty db 0 ;Difficulty level: changes cactus speed
canAddScore db 0
speed dw 10 ;cactus speed
errormsg db 'ERROR OPENING FILE$' ;error message that is displayed when file loading fails
timeout dw 1 ;Code halt time
score db 0 ;Game score
asciiScore db 4 dup(0) ;Score graphics
scoreLen db 0 ;Score ASCII length

CODESEG
;Opens file
;=-=-=-=-=-=-=-=-=-=-=-=-=-
; dx = address to file name
;=-=-=-=-=-=-=-=-=-=-=-=-=-
proc OpenFile
	 mov ah, 3Dh
	 xor al, al
	 int 21h
	 jc openerror
	 jmp openFinish
	openerror:
	 mov ax, 0
	openFinish:
  ret
endp OpenFile

;Closes file
;=-=-=-=-=-=-=-=-=
; bx = file handle
;=-=-=-=-=-=-=-=-=
proc CloseFile
	  push ax
	  push bx
	  push dx
	  mov ah,3Eh
	  int 21h
	  pop dx
	  pop bx
	  pop ax
	  ret
endp CloseFile

;Reads the bitmap's header
;=-=-=-=-=-=-=-=-=-=-=-=-=
; bx = file handler
; dx = bitmap header buffer
;=-=-=-=-=-=-=-=-=-=-=-=-=
proc ReadBMPHeader
	push ax
	push bx
	push cx
	push dx
	mov ah,3fh
	mov cx,54
	int 21h
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp ReadBMPHeader

;Reads the bitmap's palette
;=-=-=-=-=-=-=-=-=-=-=-=-=-=
; bx = file handler
; dx = palette buffer address
;=-=-=-=-=-=-=-=-=-=-=-=-=-=
proc ReadBMPPalette
	  push ax
	  push bx
	  push cx
	  push dx
	  mov ah, 3fh
	  mov cx, 400h
	  int 21h
	  pop dx
	  pop cx
	  pop bx
	  pop ax
	  ret
endp ReadBMPPalette

;Copies palette buffer to VGA memory
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; si = palette buffer address
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
proc CopyBMPPalette
	push ax
	push bx
	push cx
	push dx
	mov cx, 256
	mov dx, 3C8h
	xor al, al
	;Copy starting color to port 3C8h
	out dx, al
	;Copy palette itself to port 3C9h
	inc dx
	PalCopy:
	;Colours in a bitmap file are saved as BGR values rather than RGB.
	mov al,[si+2] ; Get red value.
	;Max. is 255, but video palette maximal
	;value is 63. Therefore dividing by 4.
	shr al,2 
	out dx,al ; Send it.
	mov al,[si+1] ; Get green value.
	shr al,2
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2
	out dx,al ; Send it.
	add si,4 ; Point to next color.
	;There is a null byte after every color
	loop PalCopy
	pop dx
	pop cx
	pop bx
	pop ax
    ret
endp CopyBMPPalette

;Copies bitmap image data to memory
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; dx = memory buffer address
; cx = image height
; ax = image width
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
proc CopyBMPToMemory
	push ax
	push bx
	push cx
	push dx
	push ax ;width
	push dx ;buffer
	mul cx
	pop dx ; dx = buffer
	add dx, ax ; dx = end of buffer
	pop di ; di = width
	sub dx, di ; start of last line
	mov bp, cx ; bp = height
	mov cx, di ; cx = width
	;Bitmap file saves image data upside down, so we'll be copying data to the graphic's
	;buffer "width" bytes at a time from the file, by beginning at the end of the buffer
	;and subtracting the width every time. The loop will occur "height" times
	ReadLine:
		mov ax, 03F00h
	    int 21h; Read from file. BX = file handle, CX = number of bytes to read, DX = buffer
	    sub dx, cx
	    dec bp
	    cmp bp, 0
	    jne ReadLine
	    pop dx
	    pop cx
	    pop bx
	    pop ax
	    ret
endp CopyBMPToMemory

;Draws to VGA memory from DATA memory
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; STACK PARAMS:
; ss:bp + 4 = image y cord
; ss:bp + 6 = image x cord
; ss:bp + 8 = image height
; ss:bp + 10 = image width
; ss:bp + 12 = graphic buffer address
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
proc DrawFromMemory
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push sp
	push bp
	push si
	push di
	mov ax, 0A000h ;VGA memory address
	mov es, ax
	;In order to calculate byte index we'll use the following equation:
	;index = screenWidth * y + x
	mov ax, [bp + 4] ; start y
	mov bx, 320 ;screen width
	mul bx ;ax = start of the line
	mov di, ax ; di = start of the length
	add di, [bp + 6] ; add startX
	mov si, [bp + 12] ; si = buffer
	mov cx, [bp + 8] ;height
	;Pixel plotting loop
	;Draws row to screen "height" times
	CPLoop:
		push cx
		mov cx, [bp + 10]; width
		push ax
		xor ax, ax
		;copy pixel from data memory to VGA memory
		;if pixel is white, don't plot pixel (alpha channel)
		copyLine:
		mov al, [ds:si]
		cmp al, 0FFh
		je alpha
		mov [es:di], al
		alpha:
		inc si
		inc di
		loop copyLine
		pop ax
		pop cx
		sub di, [bp + 10] ;sub the width from the buffer
		add di, 320 ;go to next row
	loop CPLoop
	pop di
	pop si
	pop bp
	pop sp
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 10
endp DrawFromMemory

;Draws cactuses to screen
;Decreases cactus' X after every call
proc DrawCactus
	push offset cactus
	push 60 ; width
	push 49 ; height
	push [cactusX] ; x
	push [cactusY] ; y
	call DrawFromMemory
	;Reset cord
	cmp [cactusX], -10
	jle reset
	push ax
	mov ax, [speed]
	add al, [difficulty]
	sub [cactusX], ax
	sub [cactusBoundXMax], ax
	sub [cactusBoundXMin], ax
	pop ax
	ret
	reset:
	mov [cactusX], 310
	mov [cactusBoundXMax], 310
	mov [cactusBoundXMin], 290

	;Every 10 points, increase difficulty
	push ax
	push bx
	mov al, [score]
	mov bl, 10
	div bl
	cmp ah, 0
	pop bx
	pop ax
	je levelUp
	ret
	levelUp:
	inc [difficulty]
	ret
endp DrawCactus

;Simple proc the draws the player on the screen
;Good for idle mode
proc DrawPlayer
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 150 ; y
	call DrawFromMemory
	ret
endp DrawPlayer

;Jumping animation
;Cleans the screen, and after every frame
;draws objects on the screen according to their
;layer order
proc Jump
	;Jump sound effect
	;turn on speakers
	push ax
	in al, 61h
	or al, 00000011b
	out 61h, al
	
	;get frequency change access
	mov al, 0B6h
	out 43h, al
	pop ax
	
	;play sound
	mov ax, 17C7h
	out 42h, al
	mov al, ah
	out 42h, al
	
	call YieldCode
	
	;close speaker
	in al, 61h
	and al, 11111100b
	out 61h, al
	
	call DrawBackground
	call DrawTerrain
	call DrawScore
	;frame 1
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 100 ; y
	call DrawFromMemory
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	mov [canAddScore], 1
	;frame 2
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 70 ; y
	call DrawFromMemory
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 2.5
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 60 ; y
	call DrawFromMemory
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 3
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 55 ; y
	call DrawFromMemory
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 3.5
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 55 ; y
	call DrawFromMemory
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 4
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 57 ; y
	call DrawFromMemory
	call YieldCode
	;call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 5
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 58 ; y
	call DrawFromMemory
	call YieldCode
	;call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 6
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 70 ; y
	call DrawFromMemory
	sub [playerBoundYMin], 80
	sub [playerBoundYMax], 80
	call CollisionCheck
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 7
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 100 ; y
	call DrawFromMemory
	add [playerBoundYMin], 30
	add [playerBoundYMax], 30
	call CollisionCheck
	call YieldCode
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call AddScore
	call DrawScore
	;frame 8
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 150 ; y
	call DrawFromMemory
	add [playerBoundYMin], 50
	add [playerBoundYMax], 50
	call CollisionCheck
	call AddScore
	call DrawScore
	;mov [canAddScore], 0
	ret
endp Jump

;Clears screen
proc DrawBackground
	mov ah, 06h
	xor al, al
	xor cx, cx
	mov dx, 184Fh
	mov bh, 0FFh
	int 10h
	ret
endp DrawBackground

;Draws terrain on screen
;Decreases terrain X on every call
proc DrawTerrain
	sub [terrainX], 3
	push offset terrain
	push 320 ; width
	push 37 ; height
	push [terrainX] ; x
	push [terrainY] ; y
	call DrawFromMemory
	ret
endp DrawTerrain

;Halts the code for a specific amount of time
proc YieldCode
	push ax
	mov ah, 86h
	mov cx, [timeout]
	int 15h
	pop ax
	ret
endp YieldCode

;Checks if player collided with cactus
proc CollisionCheck
	push ax
	;is playerBoundXMax bigger than cactusBoundXMin
	mov ax, [playerBoundXMax]
	cmp ax, [cactusBoundXMin]
	jg pass1
	jmp noCollision
	pass1:
	;is playerBoundXMin smaller than cactusBoundXMax
	mov ax, [playerBoundXMin]
	cmp ax, [cactusBoundXMax]
	jb pass2
	jmp noCollision
	pass2:
	;is cactusMaxY bigger than playerMinY
	mov ax, 75
	cmp ax, [playerBoundYMin]
	jb pass3
	jmp noCollision
	pass3:
	;mov [collided], 1
	pop ax
	call GameOver
	ret
	noCollision:
	;mov [collided], 0
	pop ax
	ret
endp CollisionCheck

proc GameOver
	;Game over sound
	;turn on speakers
	push ax
	in al, 61h
	or al, 00000011b
	out 61h, al
	
	;get frequency change access
	mov al, 0B6h
	out 43h, al
	pop ax
	
	;play game over sound
	mov ax, 1C3Fh
	out 42h, al
	mov al, ah
	out 42h, al
	
	call YieldCode
	
	mov ax, 1FB4h
	out 42h, al
	mov al, ah
	out 42h, al 
	
	call YieldCode
	
	mov ax, 2394h
	out 42h, al
	mov al, ah
	out 42h, al
	
	call YieldCode
	
	;close speaker
	in al, 61h
	and al, 11111100b
	out 61h, al

	;Draws retry graphic on screen
	push offset retryButton
	push 60 ; width
	push 34 ; height
	push 146 ; x
	push 83 ; y
	call DrawFromMemory
	WaitForKey:
	in al, 64h
	cmp al, 10b
	je WaitForKey
	in al, 60h
	cmp al, 15h ;did the user enter y
	je resetGame
	cmp al, 31h ;did the user enter n
	je exitGame
	jmp WaitForKey
	resetGame:
	call DrawBackground
	call DrawTerrain
	;mov [collided], 0
	mov [cactusX], 310
	mov [cactusY], 124
	mov [cactusBoundXMax], 310
	mov [cactusBoundXMin], 290
	mov [score], 0
	mov [difficulty], 0
	ret
	exitGame:
	mov ax, 4c00h
	int 21h
	ret
endp GameOver

;Draws score on screen
;Takes digits, converts to ascii
;and adds it to ASCII score array
proc DrawScore
	push ax
	push bx
	push cx
	push dx
	push di

	xor ax, ax
	xor bx, bx
	mov al, [score]
	whileNotZero:
	mov cl, 10
	div cl
	mov bl, al
	mov al, ah
	mov ah, 0
	push ax
	mov al, bl
	mov ah, 0
	inc [scoreLen]
	cmp al, 0
	je endCalc
	jmp whileNotZero

	endCalc:
	xor cx, cx
	mov cl, [scoreLen]
	xor di, di
	InsertArray:
	pop dx
	add dx, 30h
	mov [asciiScore + di], dl
	inc di
	loop InsertArray

	push ax
	xor bx, bx
	mov al, 1
	mov bh, 1
	mov bl, 0e4h
	mov cx ,[word ptr scoreLen]
	mov dl, 19
	mov dh, 2
	push ds
	pop es
	mov bp, offset asciiScore
	mov ah, 13h
	int 10h
	pop ax 

	mov [scoreLen], 0
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp DrawScore

;Adds game score after every successful jump
proc AddScore
	push ax
	push bx
	cmp [cactusX], 5
	jnbe endScoreCalc
	isAdd:
	cmp [canAddScore], 1
	jne endScoreCalc
	inc [score]
	mov [canAddScore], 0
	endScoreCalc:
	pop bx
	pop ax
	ret
endp AddScore

start:
	mov ax, @data
	mov ds, ax
	
	;Starts graphic mode 13h
	mov ax, 13h
	int 10h
	
	;Paints screen to white
	call DrawBackground
	
	;Draw terrain
	lea dx, [terrainFile]
	call OpenFile
	cmp ax, 0
	jz readError
	;Read BMP header
	mov bx, ax ; AX is the file handle from OpenFile
	mov dx, offset tmpHeader ; 54 bytes on memory
	call ReadBMPHeader
	
	lea dx, [Palette]
	call ReadBMPPalette
	
	mov si, dx
	call CopyBMPPalette
	
	mov dx, offset terrain
	mov cx, 37 ; height
	mov ax, 320 ; width
	call CopyBMPToMemory
	
	call CloseFile
	
	push offset terrain
	push 320 ; width
	push 37 ; height
	push 0 ; x
	push 163 ; y
	call DrawFromMemory
	
	jmp skipError
	readError:
	lea dx, [errormsg]
	mov ah, 9h
	int 21h
	jmp finish
	skipError:
	
	;draw player
	lea dx, [playerFile]
	call OpenFile
	cmp ax, 0
	jz readError
	;Read BMP header
	mov bx, ax ; AX is the file handle from OpenFile
	mov dx, offset tmpHeader ; 54 bytes on memory
	call ReadBMPHeader
	
	lea dx, [Palette]
	call ReadBMPPalette
	
	mov si, dx
	call CopyBMPPalette
	
	mov dx, offset player
	mov cx, [playerHeight] ; height
	mov ax, [playerWidth] ; width
	call CopyBMPToMemory
	
	call CloseFile
	
	push offset player
	push [playerWidth] ; width
	push [playerHeight] ; height
	push 17 ; x
	push 150 ; y
	call DrawFromMemory
	
	;draw cactus
	lea dx, [cactusFile]
	call OpenFile
	cmp ax, 0
	jz readError
	;Read BMP header
	mov bx, ax ; AX is the file handle from OpenFile
	mov dx, offset tmpHeader ; 54 bytes on memory
	call ReadBMPHeader
	
	lea dx, [Palette]
	call ReadBMPPalette
	
	mov si, dx
	call CopyBMPPalette
	
	mov dx, offset cactus
	mov cx, 49 ; height
	mov ax, 60 ; width
	call CopyBMPToMemory
	
	call CloseFile
	
	push offset cactus
	push 60 ; width
	push 49 ; height
	push [cactusX] ; x
	push [cactusY] ; y
	call DrawFromMemory
	
	;Load retry button
	lea dx, [retryFile]
	call OpenFile
	cmp ax, 0
	;Read BMP header
	mov bx, ax ; AX is the file handle from OpenFile
	mov dx, offset tmpHeader ; 54 bytes on memory
	call ReadBMPHeader
	
	lea dx, [Palette]
	call ReadBMPPalette
	
	mov si, dx
	call CopyBMPPalette
	
	mov dx, offset retryButton
	mov cx, 34 ; height
	mov ax, 60 ; width
	call CopyBMPToMemory
	call CloseFile
	
	;Draws score
	call DrawScore

	;Main loop that checks if player jumps or not
	WaitForKeyPress:
	in al, 60h
	cmp al, 39h
	jne NoJump
	call Jump
	jmp WaitForKeyPress
	NoJump:
	call DrawBackground
	call DrawTerrain
	call DrawCactus
	call DrawPlayer
	call CollisionCheck
	call DrawScore
	call YieldCode
	jmp WaitForKeyPress
	
	finish:
	;wait for user input
	call GameOver
	jmp WaitForKeyPress
exit:
	mov ax, 4c00h
	int 21h
END start