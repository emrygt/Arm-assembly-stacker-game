Stack_Size       EQU     0x400;
	
				 AREA    STACK, NOINIT, READWRITE, ALIGN=3
Stack_Mem        SPACE   Stack_Size
__initial_sp

				 AREA    RESET, DATA, READONLY
                 EXPORT  __Vectors
                 EXPORT  __Vectors_End

__Vectors        DCD     __initial_sp               ; Top of Stack
                 DCD     Reset_Handler              ; Reset Handler
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	0
				 DCD	Button_Handler					 
__Vectors_End    

				 AREA    |.text|, CODE, READONLY
Reset_Handler    PROC
                 EXPORT  Reset_Handler
				 ldr	 r0, =0xE000E100
				 movs	 r1, #1	
				 str	 r1,[r0]						 
			     CPSIE	 i					 
                 LDR     R0, =__main
                 BX      R0
                 ENDP

				 AREA	 button, CODE, READONLY
Button_Handler	 PROC
				 EXPORT	 Button_Handler

				 PUSH{R7,r5}
				 movs r6, #27	;arbitrary number to check if button has been pressed
				 movs r5, #40
				 cmp r11, r2		;check if floors are aligned
				 beq skip			;if they are, go on
				 b gameover			;if not, game ends
skip			cmp r9, r5			;check if we reached the top floor
				beq branch			;if so, you win
				ldr	 r0, =0x40010010
				 mov r11, r2
				 ldr	 r1,[r0]				
				 movs    r7, r1
				 movs	 r2,#0xFF
				 ands	 r1,r1,r2
				 cmp	 r1,#0
				 beq	 release
				 str	 r7,[r0]
				 mov r3, r10
				 SUBS R3, #1		;move one stage up
				 mov r10, r3
				 POP {R7, r5}
				 bx		 lr
release			 str	 r7,[r0]
				 POP {R7,r5}				
			    bx      lr
			  



				 
                 AREA    main, CODE, READONLY
                 EXPORT	 __main			 ;make __main visible to linker
				 IMPORT	image3
				 IMPORT	firstright3
				 IMPORT imagegover	
				 IMPORT imagewon
					 
                 ENTRY
__main           PROC
                 LDR R0, =image3			 ; get the first adrress of the image
				 movs r3, #5		;outer row counter (for the position of the upper edge of the image) 5. stages 0,1...4,5   there are 6 stages
				 mov r10, r3
mainnext		 mov r3, r10
				 movs r2, #0				;outer column counter (position of the left edge of the image)
				 MOVS R7, R0             ; copy of the first adrress of the image

paintfirst		PROC
				PUSH{R0,r4,r5,r6,R3,R2}				
				movs r6, #0
				LDR R0, =firstright3
				movs r4, #40		;height of image
				muls r3, r4, r3		;select stage
				movs r5, r3
				adds r3, r3, #40 	;row limit
				mov r9, r3
				ldr r4, =0x40010000
firstloop1		cmp r5, r9			 ;check if we reached the last row 	
                bhs firstreached1				
firstloop2		cmp r6, #128			 	 ;check if we reached the last column				 
				bhs firstreached2 
				str r5, [r4] 
				str r6, [r4, #0x4]
				bl colorswap
				str R1, [R4, #0x8]
				adds R6, R6, #1         ; next column 
				adds R0, R0, #4		 ; next color values
				b firstloop2
firstreached2	adds R5, R5, #1		 ; next row	
				movs r6, #0
                b firstloop1
firstreached1	bl refresh
				bl delay
				POP {R0,r4,r5,r6,R3,R2}
				ENDP
				

				 
paintright		PROC	
				PUSH{r3}
				ldr r5, =320													
				movs r4, #40		;height of image
				muls r3, r4, r3		;select floor
				movs r5, r3						  
			    adds r3, r3, #40
			    mov r9, r3			;row limit
				movs r3, #240
				mov r8, r3
			    movs r3, #160				;column limit		
				LDR R0, =image3			   
			    ldr r4, =0x40010000     ; lcd row register				             
loop1		    cmp r5, r9			 ;check if we reached the last row 	
                bhs reached1	
				cmp r9, r8			;check if we are on the first floor
				blo skipo			
				mov r11, r2			;if we are, keep two counters equal so that, it doesn't trigger gameover subroutine
skipo            movs r6, r2 			 ; column number 
loop2		    cmp r6, r3			 	 ;check if we reached the last column
                bhs reached2				
				str r5, [r4]
				str r6, [r4, #0x4]		
				bl colorswap
                str R1, [R4, #0x8]				 				 
                adds R6, R6, #1         ; next column 
				adds R0, R0, #4		 ; next color values
				b loop2
reached2		adds R5, R5, #1		 ; next row		 
                b loop1
reached1		bl refresh				
				bl delay
				cmp r6, #27			;if the button has been pressed recently, go back to the start
				beq mainnext		;after we are done with painting the current image
				subs r5, #40
				LDR R0, =image3  	;go back to first value of image colors							
				adds r3, #32			;increse column limit for next stage
				adds r2, #32			;increment outer column counter								
				ldr r7, =320
				subs r7, #96
				cmp r2, r7
				blo loop1
				POP{R3}		
				cmp r2, r7
				bhs paintleft			 
				ENDP
					
				b paintleft
branch			b youwon

paintleft		PROC				
				PUSH{r3}
				subs r2, r2, #64
				movs r7, r2
				adds r7, #32
				movs r4, #40			;height of image
				muls r3, r4, r3
				movs r4, #240
				mov r8, r4
				
				ldr r4, =320					
				movs r5, r3			
				adds r3, r3, #40
				mov r9, r3
				movs r3, r4  			
				LDR R0, =image3
				ldr r4, =0x40010000     ; lcd row register
leftloop1		cmp r5, r9				;check if we reached the last row 	
				bhs leftreached1
				cmp r9, r8				;check if we are on the first floor
				blo skipo2
				mov r11, r2				;if we are, keep two counters equal so that, it doesn't trigger gameover subroutine
skipo2			movs R6, r2 			 ; column number 
leftloop2	    cmp r6, r3				;check if we reached the last column
				bhs leftreached2	
				str r5, [r4]
				str r6, [r4, #0x4]
				bl colorswap	
				str r1, [r4, #0x8]
				adds r6, r6, #1         ; next column 
				adds r0, r0, #4		 ; next word
				b leftloop2
leftreached2	adds r5, r5, #1		 ; next row	
				b leftloop1
leftreached1	bl refresh
				bl delay
				cmp r6, #27			;if the button has been pressed recently, go back to the start
				beq mainnext		;after we are done with painting the current image
				subs r5, #40				
				LDR R0, =image3			;reset image
				subs r3, #32			;decrease column limit for next stage
				cmp r2, #0
				beq dontsub
				subs r2, #32		;decrement outer column counter
dontsub			subs r7, #32 
				cmp r7, #0			;check if we reached the left edge of the screen
				bhi leftloop1
				POP{R3}							
				bl paintfirst
				 ENDP
				 	 					 												
				
				
refresh			PROC
	            PUSH {R6,r5}
				movs r6, #1		
				movs r5, #2
				str r6, [r4, #0xC]   ;refresh the screen				
				POP{R6,r5}
				BX LR
				ENDP
					 			
					 
colorswap		PROC
				PUSH {R5}
				ldr r1, [r0] 			 ; get the value in argb format 
				rev r1, r1				 ; reverse and get bgra  
				movs r5, #8			 
				rors r1, r1, r5		 ; rotate and get abgr 
				POP {R5}
				BX LR
				ENDP
	
delay			PROC				
				PUSH{r0}
				ldr  r0, =3000000    ; delay
ag				subs  r0, r0, #1  
				bne   ag          	
				POP{r0}
				bx lr
				ENDP
					
youwon		PROC
	        PUSH {R6, r5,r3,r4}
				str r6, [r4, #0xC]   ;refresh the screen
				LDR R0, =imagewon
				movs r5, #95
				movs r6, #135
				ldr r4, =0x40010000
wonloop1		cmp r5, #145			 ;check if we reached the last row 	
                bhs wonreached1				
wonloop2		cmp r6, #185			 	 ;check if we reached the last column				 
				bhs wonreached2 
				str r5, [r4] 
				str r6, [r4, #0x4]
				bl colorswap
				str R1, [R4, #0x8]
				adds R6, R6, #1         ; next column 
				adds R0, R0, #4		 ; next color values
				b wonloop2
wonreached2	adds R5, R5, #1		 ; next row	
				movs r6, #135
                b wonloop1
wonreached1 	bl refresh
				POP {R6, r5,r3,r4}
stop			b stop
				endp
					
gameover	PROC
	        PUSH {R6, r5,r3,r4}
				str r6, [r4, #0xC]   ;refresh the screen
				LDR R0, =imagegover
				ldr r3, =285
				movs r5, #10
				movs r6, #35
				ldr r4, =0x40010000
goverloop1		cmp r5, #210			 ;check if we reached the last row 	
                bhs goverreached1				
goverloop2		cmp r6, r3			 	 ;check if we reached the last column				 
				bhs goverreached2 
				str r5, [r4] 
				str r6, [r4, #0x4]
				bl colorswap
				str R1, [R4, #0x8]
				adds R6, R6, #1         ; next column 
				adds R0, R0, #4		 ; next color values
				b goverloop2
goverreached2	adds R5, R5, #1		 ; next row	
				movs r6, #35
                b goverloop1
goverreached1	bl refresh
				POP {R6, r5,r3,r4}
				endp
		
			
				END