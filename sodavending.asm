
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

org 100h



; add your code here 

;
;  we  are using short name for different chocolates 
;  tray button, tray status and chocolate cost
; ------------------------------------------------------
;   Full Name/Description             Short Name Used
; ------------------------------------------------------
;   pepsi select button   ptb
;   fanta select button   ftb
;   dew select button     dtb
;
;   perk tray empty status               ptes
;   five star tray empty status          ftes
;   dairy milk tray empty status         dtes
;
;   small cost          sdc
;   medium cost          mdc
;   large cost            ldc
;
;--------------------------------------------------------------
;	TO DO
; while checking for empty check for all levels
; initialise variables to get the status
; after checking for type of cola get volume
;reset before infinte loop, not done in this code
;here led glows if choc empty we need opp, glow when filled
;-----------------------------------------------------------------                 
.model tiny
.data
  ; chocolate tray button pressing status variable
  ptb db 0    ; initially button not pressed
  ftb db 0    ; initially button not pressed
  dtb db 0   ; initially button not pressed

  sml db 0
  med db 0
  lrg db 0   
  
  
  peh db 1	;level indicators
  pem db 1
  pes db 1

  fah db 1
  fam db 1
  fas db 1

  deh db 1
  dem db 1
  des db 1 
  
  RES db 01h
  
  pushButton1 db 0  ; Bit2-Dairy Milk, Bit1-Five Star, Bit0-Perk
  pushButton2 db 0

  ; chocolate tray empty status variable
  ptes  db 0    ; initially not empty
  ftes  db 0    ; initially not empty
  dtes  db 0    ; initially not empty
  
  ; quantity cost in term of number of 5 rupees coins
  sdc EQU 1   ; cost Rs  5  , so 1 coin required
  mdc EQU 2   ; cost Rs 10  , so 2 coins required
  ldc EQU 3   ; cost Rs 15  , so 3 coins required
  
  ; defining address of ports for first 8255A (CHIP-1)
  PORTA1 EQU 00H
  PORTB1 EQU 02H
  PORTC1 EQU 04H 
  CR1    EQU 06H
  
  ; defining address of ports for second 8255A (CHIP-2)
  PORTA2 EQU 08H
  PORTB2 EQU 0AH 
  PORTC2 EQU 0CH 
  CR2    EQU 0EH
  
  PORTA3 EQU 10H
  PORTB3 EQU 12H
  PORTC3 EQU 14H
  CR3    EQU 16H
  
  ValidCoin db 00H   ; initially set not valid coin                   
  NoOfCoins dw 0000h 
.code
.startup
  MOV AX,0000H
  MOV DS,AX				;initializing data segment to 0000H

  MOV AX,0800H
  MOV SS,AX				;initializing stack segment to 0800H
  
  ; initializing control word for first 8255A CHIP-1 
  MOV DX,CR1       ; address of Control Register 1
  MOV AL,10001010B	; PortA, PortC output and PortB input mode 0
  OUT DX,AL
 
  ; initializing control word for second 8255A CHIP-2 
  MOV DX,CR2       ; address of Control Register 2
  MOV AL,10010010B	; portA, PortB input and PortC output mode 0 
  OUT DX,AL  
  
  ; initializing control word for second 8255A CHIP-3 
  MOV DX,CR3       ; address of Control Register 3
  MOV AL,10000000B	; portA, PortB input and PortC output mode 0 
  OUT DX,AL
  
  

  ;;                M A I N      P R O G R A M

  
  mainLoop: mov bl,0
        	mov ptb,bl
			mov ftb,bl
			mov dtb,bl
			mov lrg,bl
			mov sml,bl
			mov med,bl
			mov ptes,bl
			mov ftes,bl
			mov dtes,bl
            mov peh,1
            mov pem,1
            mov pes,1
            mov fah,1
            mov fam,1
            mov fas,1
            mov deh,1
            mov dem,1
            mov des,1
  
  
            CALL QuantityCheck		;Check the amount of drink left in each tank
  
			CALL ReadPushButton   ; read Push Button status of all soda
            
            ; check pepsi button & tank empty status
            MOV AL, ptb   ; pepsi button status examine
            CMP AL, 01H   ; 00H-not pressed,    01H-pressed
            JNZ nextMain1
            
           
            
    pepsiLargeCheck:	cmp peh,1
						jz sizeselect

	pepsiMediumCheck:	cmp pem,1
						jnz pepsiSmallCheck
						cmp lrg,1
						jz errorLight
						cmp med,1
						jz nextsize1
						cmp sml,1
						jz sizeselect

	pepsiSmallCheck:	cmp pes,1
						jnz errorLight
						cmp lrg,1
						jz errorLight
						cmp med,1
						jz errorLight
						cmp sml,1
						jz sizeselect
	       



            ; check fanta button & tank empty status 
  nextMain1:MOV AL, ftb   ; fanta button status examine
            CMP AL, 01H   ; 00H-not pressed,    01H-pressed
            JNZ nextMain2

   
   
    fantaLargeCheck:
						cmp fah,1
						jz sizeselect

	fantaMediumCheck:	cmp fam,1
						jnz fantaSmallCheck
						cmp lrg,1
						jz errorLight
						cmp med,1
						jz sizeselect
						cmp sml,1
						jz sizeselect

	fantaSmallCheck:	cmp fas,1
						jnz errorLight
						cmp lrg,1
						jz errorLight
						cmp med,1
						jz errorLight
						cmp sml,1
						jz sizeselect 
						
						
            
            ; check dew button & tank empty status 
  nextMain2:MOV AL, dtb   ; dew button status examine
            CMP AL, 01H   ; 00H-not pressed,    01H-pressed
            JNZ mainLoop

    dewLargeCheck:		cmp deh,1
						jz sizeSelect

	dewMediumCheck:		cmp dem,1
						jnz dewSmallCheck
						cmp lrg,1
						jz errorLight
						cmp med,1
						jz sizeselect
						cmp sml,1
						jz sizeselect

	dewSmallCheck:	    cmp des,1
						jnz errorLight
						cmp lrg,1
						jz errorLight
						cmp med,1
						jz errorLight
						cmp sml,1
						jz sizeselect
            
           


;Check the user size selection for soda drink

sizeSelect:CALL ReadPushButton2
	MOV AL,sml
            CMP AL,01H
            JNZ nextsize1
            
            MOV CX, sdc   ; number of coin require for small Size
            JMP coinLoop
               
   nextsize1: MOV AL,med
              CMP AL,01H
              JNZ nextsize2
              MOV CX, mdc   ; number of coin require for Medium Size      
              JMP coinLoop

   nextsize2: MOV AL,lrg
              CMP AL,01H
              JNZ sizeSelect
              MOV CX, ldc   ; number of coin require for Large Size 
              JMP coinLoop





            ; check number of valid coin inserted to dispense chocolate
   coinLoop:MOV NoOfCoins,CX
            CALL PrintAmount
            CALL ReadCoinInsertion
			CALL CloseDisplay
            MOV AL, ValidCoin
            CMP AL,01H
            
            JNZ mainLoop    ; not valid restart the process
            
            CALL DispenseSoda  ; dispense demanded chocolate
            CALL StopDispense	;stop dispense when required amount dispensed

  RESLOOP: MOV DX,PORTC3
           IN al,DX
           AND AL,01H
           CMP AL,RES
           JNZ RESLOOP
           NOT AL
           AND AL,01
           MOV RES,AL
           MOV AL,0
           out PORTA1,AL
           JMP mainLoop  
            
            
            
   errorLight:  mov dx,PORTA1
			    mov al,01h
				out dx,al
				mov bl,0h
				mov ptb,bl
				mov ftb,bl
				mov dtb,bl
				mov lrg,bl
				mov sml,bl
				mov med,bl
				mov ptes,bl
				mov ftes,bl
				mov dtes,bl
				mov bl,01h
				mov peh,bl
				mov pem,bl
				mov pes,bl
				mov deh,bl
				mov dem,bl
				mov des,bl
				mov fas,bl
				mov fam,bl
				mov fah,bl
				jmp mainLoop  
  
  
  
  
  
  
  

  ;						 S U B R O U T I N E S

  QuantityCheck PROC NEAR
	
		PUSHF
		PUSH	DX
		PUSH	CX
		PUSH	AX

		;porta3 contains data on pepsi and fanta
		IN		AL,PORTA3

		;for pepsi
		MOV		AH,AL
		AND		AH,04H	;pepsi large
		JNZ		NEXTC
		MOV		peh,0		
		MOV		AH,AL
		AND		AH,02H	;pepsi medium
		JNZ		NEXTC
		MOV		pem,0
    	MOV		AH,AL
		AND		AH,01H	;pepsi small
		JNZ		NEXTC
		MOV		pes,0

		;for fanta
NEXTC:	MOV		AH,AL
		AND		AH,40H	;fanta large
		JNZ		NEXTF
		MOV		fah,0
    	MOV		AH,AL
		AND		AH,20H	;fanta medium
		JNZ		NEXTF
		MOV		fam,0
    	MOV		AH,AL
		AND		AH,10H	;fanta small
		JNZ		NEXTF
		MOV		fas,0

		;portb3 for dew
NEXTF:	IN		AL,PORTB3	
		MOV		AH,AL
		AND		AH,04H	;dew small
		JNZ		NEXTI
		MOV		deh,0
    	MOV		AH,AL
		AND		AH,02H	;dew medium
		JNZ		NEXTI
		MOV		dem,0
    	MOV		AH,AL
		AND		AH,01H	;dew large
		JNZ		NEXTI
		MOV		des,0

NEXTI:
		POP		AX
		POP		CX
		POP		DX
		POPF
		RET
	QuantityCheck ENDP


  ; Procedure for Reading Push button pressing status 

  ReadPushButton PROC NEAR
          PUSHF
             
          MOV DX, PORTB1   ; load address of PORTB1
          IN  AL, DX       ; get status of all push button
          MOV pushButton1, AL  ; Store pushbutton status as
                              ; Bit(2)-Dew, Bit(1)-Fanta, Bit(0)-Pepsi
          AND AL, 01H
          JZ nextBit1
          MOV ptb, 01H    ; pepsi demand button pressed
          jmp nextBit3
  nextBit1:MOV AL, pushButton1  ; reload all button status
          ROR AL, 01H
          AND AL, 01H
          JZ nextBit2
          MOV ftb, 01H    ; fanta demand button pressed
          jmp nextBit3
  nextBit2:MOV AL, pushButton1  ; reload all button status 
          ROR AL, 02H
          AND AL, 01H
          JZ nextBit3
          MOV dtb, 01H   ; dew demand button pressed 
  nextBit3:POPF
          RET
  ReadPushButton ENDP





  ; Procedure for Reading Quantity button pressing Status

  
  ReadPushButton2 PROC NEAR USES AX, DX
          PUSHF
             
          MOV DX, PORTB1   ; load address of PORTB1
          IN  AL, DX       ; get stus of all push button
          MOV pushButton2, AL  ; Store pushbutton status as
                              ; Bit(6)-Large, Bit(5)-Medium, Bit(4)-Small
          AND AL, 10H
          JZ nextBit11
          MOV sml, 01H    ; small quantity button pressed
  nextBit11:MOV AL, pushButton2  ; reload all button status
          ROR AL, 01H
          AND AL, 10H
          JZ nextBit21
          MOV med, 01H    ; medium quantity button pressed
  nextBit21:MOV AL, pushButton2  ; reload all button status 
          ROR AL, 02H
          AND AL, 10H
          JZ nextBit31
          MOV lrg, 01H   ; large quantity button pressed 
  nextBit31:POPF
          RET
  ReadPushButton2 ENDP



  ; Procedure for Checking Inserted 5 Rupees Coin Validity  

      
  
  
  ReadCoinInsertion PROC NEAR
          
          PUSHF
          
          
  X6:      MOV DX,PORTC2
           MOV AL,00000011B ; Bit(2)-Pressure Transducer(PT), Bit(1)-START, Bit(0)-ALE
           OUT DX,AL  
           
           MOV DX,PORTC2
           MOV AL,00000001B ; Bit(2)-Pressure Transducer(PT), Bit(1)-START, Bit(0)-ALE
           OUT DX,AL
           ; Check EOC Status of ADC
           
           MOV DX,PORTB2
          
 X1:       IN AL,DX
           AND AL,01H
           JZ X1
           
           MOV DX,PORTA2
           IN AL,DX
 
           CMP sml,01h
           JZ X2
           
           CMP med,01h
           JZ X3
           
           CMP lrg,01h
           JZ X4
     
 X2:   
           CMP AL,01011110b
           JB  X6       
           JA  X5
           MOV ValidCoin,01h
           JMP Break
           
                     
 X3:       
           CMP AL,01011110b
           JB  X6
           JA  X5
           MOV ValidCoin,01h                     
           JMP Break
           
 X4:       
           CMP AL,01011110b
           JB  X6
           JA  X5
           MOV ValidCoin,01h
           JMP Break
           
 X5:       MOV ValidCoin,00h          
 Break:   
           POPF
           RET
 ReadCoinInsertion ENDP 

  ; Procedure for Dispensing Soda   

  DispenseSoda PROC NEAR
          PUSHF
          MOV AL, 00H
          CMP ptb, 01H
          JNZ nextDC1
          OR AL,01H   ; set Bit(0)
  nextDC1:CMP ftb, 01H
          JNZ nextDC2
          OR AL, 02H  ; set Bit(1)
  nextDC2:CMP dtb, 01H
          JNZ nextDC3
          OR AL, 04H  ; set Bit(2)
  nextDC3:MOV DX, PORTC1
          OUT DX, AL  ; operate demanded tray relay and dispense chocolate
          POPF
          RET
  DispenseSoda ENDP
  
  StopDispense PROC NEAR USES AX, DX
	PUSHF
RE:	in al,PORTC1
	cmp sml,01H
	jnz nextM
	and al,00010000b
	jnz EX
	jmp RE
	
nextM:  cmp med,01H
	in al,PORTC1
	jnz nextL
	and al,00100000b
	jnz EX
	jmp nextM	  

nextL:  cmp lrg,01H
	in al,PORTC1
	jnz nextL
	and al,01000000b
	jnz EX
	jmp nextL

EX: mov al,00000000b
	out portc1,al
	popf
	ret
StopDispense endp  
  
  
  
  
  
  
; start delay
Delay PROC NEAR
	
	PUSHF
	PUSH 	CX
	MOV CX, 1325			;1325*15.085 usec = 20 msec
	W1: 
		NOP
		NOP
		NOP
		NOP
		NOP
	LOOP W1
	POP 	CX
	POPF

	RET
Delay ENDP	
; end delay

PrintAmount PROC NEAR		;NoOfCoins stores the number of coins required
	PUSHF
	PUSH 	DX
	PUSH 	AX
	PUSH 	CX
    PUSH    BX
    
	MOV 	CX,NoOfCoins
    MOV     BX,200
	MOV 	DX,PORTA1		;portA1 for reading


	CMP 	CX,1			;check what to display
	JZ 		FIVE

	CMP 	CX,2
	JZ 		TEN

	CMP 	CX,3
	JZ  	FIFTEEN

FIVE:									;infinite loop --- change accordingly
		MOV 	AL,02H		;0 in dispA
		OUT 	PORTA1,AL
		;CALL 	DELAY
		MOV 	AL,54H		;5 in dispB
		OUT		PORTA1,AL
		;CALL 	DELAY
		DEC     BX
		CMP		BX,0
		JNZ		FIVE
		JMP     end

TEN: 
		MOV 	AL,12H		;1 in dispA
		OUT 	PORTA1,AL
		CALL 	DELAY
		MOV 	AL,04H		;0 in dispB
		OUT		PORTA1,AL
		CALL 	DELAY
		DEC     BX
		CMP		BX,0
		JNZ		TEN
		JMP     end	

FIFTEEN: 
		MOV 	AL,12H		;1 in dispA
		OUT 	PORTA1,AL
		CALL 	DELAY
		MOV 	AL,54H		;5 in dispB
		OUT		PORTA1,AL
		CALL 	DELAY
		DEC     BX
		CMP		BX,0
		JNZ		FIFTEEN
		
end:
    
    POP     BX
	POP 	CX
	POP 	AX
	POP 	DX
	POPF

	RET
PrintAmount ENDP  
  
 
 CloseDisplay proc near

	pushf
	push dx
	push ax

	mov dx,porta1

	mov al,0
	out dx,al

	pop ax
	pop dx
	popf
	ret
CloseDisplay endp


  
.exit
end




ret




