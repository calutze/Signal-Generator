;ME 305 - Lab 4 Function Generator
;November 13, 2014
;Chris Lutze, Jenny Reitmajer

;Assembler Equates

; Interrupt Service Routine
INTERVAL   =$0320

TIOS	   =$0080			; Timer IC/OC Select Register
CH0	       =0b00000001		; Setting channel 0 to output configured

TCTL2       =$0089			; Timer Control Register 2
OL0        =0b00000001		; Output Mode Bit for channel 0
OM0        =0b00000010		; Output Level Bit for channel 0

TMSK1       =$008C			; Timer Mask Register 1
C0I        =0b00000001		; Channel 0 maskable interrupt requests enabled

TFLG1       =$008E			; Timer Flag Register 1
C0F        =0b00000001		; Clear channel 0 flag

TSCR       =$0086			; Timer System Control Register
TEN        =0b10000000		; Timer Enable Bit - timer enabled
TSBCK      =0b00100000		; Timer Stop in Background Mode Bit - 
							;    timer disabled in background mode
TFFCA      =0b00010000		; Timer Fast Flag Clear-All Bit - set

TCNTH      =$0084			; Timer Counter Register High

TC0H	   =$0090			; Timer Channel 0 Register - contains output
							;    compare value
; Digital Analog Converter						
PORTJ          =     $0028				 ; Port J bit location
DDRJ           =     $0029				 ; DDR J bit location
DACALSB        =     $0300               ; CHA LSB DAC address
DACAMSB        =     $0301               ; CHA MSB DAC address
DACBLSB 	   =	 $0302				 ; CHB LSB DAC address
DACBMSB        =     $0303               ; CHB MSB DAC address
pin5           =     0b00010000          ; tied to notLDAC, the DAC latch

;RAM area
.area bss

BS_FLG::	   .blkb 1			   ; backspace requested flag
bsstate::	   .blkb 1			   ; backspace sub-state variable
BUFFER::       .blkb 5             ; stores ASCII characters as entered from keypad
CINT::		   .blkb 1			   ; counts number of interrupts
COUNT::        .blkb 1             ; number of valid digits entered into keypad
CSEG::		   .blkb 1			   ; TASK_5 state
DMESS2::       .blkb 1             ; message 2 flag
DMESS3::       .blkb 1             ; message 3 flag
DMESS4::       .blkb 1             ; message 4 flag
DMESS5::       .blkb 1             ; message 5 flag
DMESS6::	   .blkb 1			   ; message 6 flag
DMESS7::	   .blkb 1			   ; message 7 flag
DMESS10::      .blkb 1             ; message 10 flag
DMESS11::      .blkb 1             ; message 11 flag
DMESS12::      .blkb 1             ; message 12 flag
DMESS13::      .blkb 1             ; message 13 flag
DMESS14::      .blkb 1             ; message 14 flag
DMESS15::      .blkb 1             ; message 15 flag
DMESS16::      .blkb 1             ; message 16 flag
DPRMPT::	   .blkb 1			   ; 
DWAVE::		   .blkb 1			   ; Display Wave flag
DPTR::         .blkb 2             ; address of next character to be displayed
Echo::		   .blkb 1			   ; print digit flag
ENTER_DIGITS:: .blkb 1			   ; stores COUNT at enter key press
ENTER_FLG::    .blkb 1             ; flag indicating that enter is finished
ERROR_DELAY::  .blkb 2			   ; 150ms delay
ERROR_LARGE::  .blkb 1			   ; magnitude overflow flag
ERROR_NOD::	   .blkb 1			   ; no digits error flag
ERROR_ZERO::   .blkb 1			   ; zero magnitude flag
FIRSTCH::      .blkb 1             ; first character boolean variable
KEY_BUFF::     .blkb 1             ; stores keypad ASCII character
LSEG::		   .blkb 1			   ; TASK_5 state
MASTERKEY::    .blkb 1             ; flag that keypad is waiting with a key
NEWBTI::	   .blkb 1			   ; flag for when a new BTI is set
NINT::		   .blkb 1			   ; period for each interrups
NINT_OK::	   .blkb 1			   ; flag for when valid NINT has been entered
POINTER::      .blkb 2             ; pointer for buffer
RESULT::	   .blkb 2			   ; binary result from keypad converted from BUFFER
RUN::		   .blkb 1			   ; 
SEGPTR:	 	   .blkb 2			   ; TASK_5 state
SEGINC::	   .blkb 1			   ; TASK_5 state
t1state::      .blkb 1			   ; TASK_1 state
t2state::      .blkb 1			   ; TASK_2 state
t3state::      .blkb 1			   ; TASK_3 state
t4state::      .blkb 1			   ; TASK_4 state
t5state::      .blkb 1			   ; TASK_5 state
VALUE::		   .blkb 2			   ; TASK_5 state
WAVEPTR::	   .blkb 2			   ; address of 1st line of data table for current wave
WAVE::		   .blkb 1             ; stores type of waveform to display

;code area
.area text
;
;
;======================================================================
_main::
		clr    t1state
		clr    t2state
		clr    t3state
		clr    t4state
		clr    t5state

			   
top:   
		jsr    TASK_1
		bgnd
		jsr    TASK_2
		jsr    TASK_3
		bgnd
		jsr	   TASK_4
		jsr    TASK_5
		bra    top

; end_main
;
;=============================================================================
;
;    Subroutine TASK_1          ; TC0 INTERRUPTS 

TASK_1: ldaa   t1state			; get current t1state and branch accordingly
        beq    t1state0
		rts
;------------------------------------------------------------------------------	
t1state0:						; initialize
; Set up Timer Output Compare Interrupts to occur every 0.1 msec
		bset   TIOS, CH0	  	 ; set timer channel 0 for output toggle
		bset   TCTL2, OL0		 ; set TCTL to Toggle OC0 output line
		bclr   TCTL2, OM0		 ; (continues to set TCTL) 
		bset   TMSK1, C0I		 ; set timer channel 0 timer output compare flag
		cli						 ; clear I bit to enable maskable interrupts
		bset   TFLG1, C0F		 ; enable timer channel 0 output compare interrupts
		
		bset   TSCR, TEN		 ; start Pulse Accumulator counter
		bset   TSCR, TSBCK		 ; set counter not to run in BGND mode
        ldd    TCNTH			 ; read current PA number
		addd   #INTERVAL		 ; add INTERVAL to current PA number
		std    TC0H				 ; store offset PA number into TC0H
        movb   #$01,t1state
        rts
;=============================================================================
;
;    Subroutine TASK_2          ; MasterMind

TASK_2: ldaa   t2state          ; get current t2state and branch accordingly
        beq    t2state0
		deca
		beq    t2state1
		deca
		beq    t2state2
		deca
		lbeq   t2state3
		deca
		lbeq   t2state4
		deca
		lbeq   t2state5
		deca
		lbeq   t2state6
		deca
		lbeq   t2state7
		deca
		rts
;------------------------------------------------------------------------------	
t2state0:						; initialize
		clr    ERROR_NOD
		clr    ERROR_ZERO
		clr    ERROR_LARGE
		clr    WAVE
		clr    COUNT
		clr    bsstate
	    movw   #BUFFER, POINTER
		movw   #$5FFF, ERROR_DELAY
		movb   #$01,ENTER_FLG
        movb   #$01,DMESS2
        movb   #$01,t2state
        rts
;------------------------------------------------------------------------------	
t2state1:						; waiting for prompts
        tst    DMESS2
		lbeq   t2s1_done
		lbra   t2s1_end

t2s1_done:
        movb   #$02,t2state
t2s1_end:
        rts
;------------------------------------------------------------------------------	
t2state2:						; checking for a key
        tst    MASTERKEY
		lbeq   t2s2_exit
		clr    MASTERKEY
		ldaa   KEY_BUFF
		
        movb   #$05,t2state 		 ; set next state to ENT
		ldab   COUNT
		stab   ENTER_DIGITS			 ; number of digits to be converted in enter
		cmpa   #$0A					 ; compare ascii character with Enter
		lbeq   t2s2_exit			 ; exit if equal	
		
		movb   #$06,t2state			 ; set next state to BS
		cmpa   #$08					 ; compare ascii character with Backspace
		lbeq   t2s2_exit			 ; exit if equal
		
        movb   #$02,t2state			 ; set back to current state
		cmpa   #$30					 ; compare ascii character with #$30	
		lblt   t2s2_exit			 ; not a digit if <$30, exit
		cmpa   #$39					 ; compare ascii character with #$39						
		lbhi   t2s2_exit			 ; not a digit if >$39, exit
		
        movb   #$04,t2state			 ; set next state to NINT
		tst    COUNT
		lbne   t2s2_exit			 ; exit if not zero

		movb   #$02,t2state          ; set back to current state
	    cmpa   #$31                  ; compare ascii character with #$31
		lblt   t2s2_exit             ; non-valid range for wave selection 
		cmpa   #$33                  ; compare ascii character with #$33
		lbhi   t2s2_exit             ; non-valid range for wave selection
		movb   #$03,t2state			 ; set next state to type for wave
		lbra   t2s2_exit			 ; exit
		
t2s2_exit:
		rts
;------------------------------------------------------------------------------	
t2state3:						; digit for type of wave
		tst    ENTER_FLG
		lbeq   t2s3_exit
		movw   #BUFFER,POINTER		 ; move first address of BUFFER into POINTER
		inc    COUNT				 ; increment COUNT
		ldab   KEY_BUFF				 ; load accumulator B with KEY_BUFF
		movb   #$01,DMESS16

		movb   #$01,DMESS3			 ; set DMESS3 flag
		movb   #$01,WAVE
		cmpb   #$31
		lbeq   t2s3_exit			 ; exit if KEY_BUFF = $31  
		clr    DMESS3				 ; clear DMESS3
		
		movb   #$01,DMESS4			 ; set DMESS4
		movb   #$02,WAVE
		cmpb   #$32
		lbeq   t2s3_exit			 ; exit if KEY_BUFF = $32
		clr    DMESS4				 ; clear DMESS4
		
		movb   #$01,DMESS5			 ; set DMESS5
		movb   #$03,WAVE
		cmpb   #$33
		lbeq   t2s3_exit			 ; exit if KEY_BUFF = $33
		
t2s3_exit:
        movb   #$01,DMESS6
		movb   #$01,DMESS7
		movb   #$01,DMESS10
		movb   #$02,t2state			 ; set t2state to 2
		clr    ENTER_FLG
		clr    RUN
		rts
;------------------------------------------------------------------------------	
t2state4:						; digit for NINT
		ldaa   COUNT
		cmpa   #$04					 ; test COUNT
		lbge   t2s4_exit			 ; if COUNT=>4 branch
		cmpa   #$01				 	 ; test COUNT
		lblt   t2s4_exit	    	 ; if COUNT=!0 branch						

		ldab   KEY_BUFF				 ; load accumulator B with KEY_BUFF
		ldx    POINTER				 ; load index register X with POINTER
		stab   0,X					 ; store contents of accumulator B at location X
		inc    COUNT				 ; increment COUNT
		inx							 ; increment X
		stx    POINTER				 ; store X into POINTER
		inc	   DPTR					 ; increment DPTR
		movb   #$01,Echo			 ; set Echo to 1
		lbra   t2s4_exit
		
t2s4_exit:
		movb   #$02,t2state			 ; set t2state to 2
		rts
;------------------------------------------------------------------------------	
t2state5:						; ENTER
	    tst    ENTER_FLG
		lbne   t2s5_exit
		ldab   ENTER_DIGITS
		cmpb   #$01
		lbeq   noD_error			 ; compare ENTER_DIGITS $01 and branch if equal
		cmpb   COUNT			  	 ; compare ENTER_DIGITS to COUNT
		lbne   hex_conversion		 ; if COUNT=!ENTER_DIGITS the branch directly to hex_conversion
		
		movw   #$0000,RESULT		 ; zero result on each first pass through t2state5
		movw   #BUFFER,POINTER		 ; load index register X with BUFFER
		
hex_conversion:
        ldx    POINTER
		ldab   0,X
		clra
		subb   #$30                  ; subtract #$30 from B to convert from ASCII to BCD
		pshd   						 ; push accumulator D onto the stack
		
		ldd    RESULT				 ; load accumulator A with RESULT
		ldy    #$000A				 ; load index register Y with 10
		emul   						 ; multiply accumulator D with Y
		cpd    #$00FF
		bhs    large_error1
		std    RESULT                ; convert BCD to binary

		puld   						 ; pull the stack onto accumulator D
		addd   RESULT				 ; add BCD number to RESULT
		cpd    #$00FF
		bhs    large_error
		std    RESULT				 ; store addition back into RESULT
		inx
		stx    POINTER				 
		dec    COUNT				 ; decrement COUNT
		ldaa   COUNT
		cmpa   #$01
		lbne   t2s5_loop			 ; branch if COUNT =! 1

;MAGNITUDE TESTING
		ldd    RESULT
		cpd    #$0000				 ; check that RESULT is not 0
		lbeq   zero_error
		stab   NINT
		bra    t2s5_exit
		
t2s5_loop:
		rts
		
large_error1:						; stack must be incremented twice before to have
		ins							; correct return address on the top
		ins
		
large_error:
		movb   #$01,ERROR_LARGE		; set ERROR_LARGE flag
		movb   #$01,COUNT
		movb   #$07,t2state
		rts	

zero_error:
		movb   #$01,ERROR_ZERO		; set ERROR_ZERO flag
		movb   #$01,COUNT
		movb   #$07,t2state
		rts
		
noD_error:
		movb   #$01,ERROR_NOD		; set ERROR_NOD flag
		movb   #$01,COUNT
		movb   #$07,t2state
		rts

t2s5_exit:
		movb   #$01,ENTER_FLG  
		clr    COUNT
		movb   #$02,t2state
		rts
;------------------------------------------------------------------------------	
t2state6:						; Backspace
        ldaa   COUNT
		cmpa   #$01
		lbls   t2s6_exit    
		movb   #$01,BS_FLG		    ; set BS_FLG to 1
		
t2s6_exit:
		movb   #$02,t2state		    ; set t1state to 2
		rts
;------------------------------------------------------------------------------	
t2state7:						; Error
        ldy    ERROR_DELAY			; check if ERROR_DELAY is 1500
		cpy    #$5FFF
		lbne   t2s7_decrement
		tst    ERROR_ZERO	
		bne    zeros				; check ERROR_ZERO flag and branch
		tst    ERROR_LARGE
		bne    too_large			; branch if ERROR_LARGE flag set
		tst    ERROR_NOD
        bne    no_digits			; branch if ERROR_NOD flag set
		tst    RESULT
		beq    zeros
	    lbra   t2s7_decrement
		
no_digits:
		movb   #$01,DMESS10 	    ; short blank	
		movb   #$01,DMESS11         ; long blank
		movb   #$01,DMESS12         ; message states 'ERROR! NO DIGITS'
		clr    ERROR_NOD		    ; clear ERROR_NOD
		lbra   t2s7_decrement
		
too_large:
		movb   #$01,DMESS10 	    ; short blank	
		movb   #$01,DMESS11         ; long blank
		movb   #$01,DMESS14         ; message states 'ERROR! MAGNITUTE TOO LARGE'
		clr    ERROR_LARGE		    ; clear ERROR_LARGE
		lbra   t2s7_decrement

zeros:
		movb   #$01,DMESS10 	    ; short blank	
		movb   #$01,DMESS11         ; long blank
		movb   #$01,DMESS13         ; message states 'ERROR! ZERO MAGNITUDE'
		clr    ERROR_ZERO		    ; clear ERROR_ZERO
		lbra   t2s7_decrement
		
t2s7_reset:
        movw   #$5FFF, ERROR_DELAY  ; reset ERROR_DELAY count
		bra    t2s7_exit
		
t2s7_decrement:
		cpy    #$0000			    ; check if ERROR_DELAY is zero
		beq    t2s7_reset		    ; branch if zero
	    dey							
		sty    ERROR_DELAY		    ; decrement ERROR_DELAY
		rts
		
t2s7_exit:						    ; reset prompts for bottom line
		movb   #$01,DMESS6
		movb   #$01,DMESS7
		movb   #$01,DMESS11
		movb   #$02,t2state
		rts

;=============================================================================
;
;    Subroutine TASK_3			; Keypad

TASK_3: ldaa   t3state             ; get current t3state and branch accordingly
        beq    t3state0
		deca
		beq    t3state1
		deca
		beq    t3state2
		deca
		rts
;------------------------------------------------------------------------------	
t3state0:
        movb   #$01,t3state
        rts
;------------------------------------------------------------------------------	
t3state1:                          ; Grabs key when pressed, stores in KEY_BUFF
        ; and sets MASTERKEY=1 to alert Mastermind that a key is ready
        tst    L$KEY_FLG           
		bne    t3s1_exit           ; if there is no key, exit
		jsr    GETCHAR             ; grab key from keypad, stores in accumulator b
		stab   KEY_BUFF            ; store character into KEY_BUFF
		movb   #$02,t3state
		movb   #$01,MASTERKEY
		bra    t3s1_exit
t3s1_exit:
        rts
		
;------------------------------------------------------------------------------	
t3state2:
		tst    MASTERKEY
		lbne   t3s2_exit
		movb   #$01,t3state
		lbra   t3s2_exit
		
t3s2_exit:
        rts
;=============================================================================
;
;    Subroutine TASK_4          ; LCD Module Display

TASK_4: ldaa   t4state			  ; get current t4state and branch accordingly
        lbeq   t4state0
		deca
		lbeq   t4state1
		deca
		lbeq   t4state2
		deca
		lbeq   t4state3
		deca
		lbeq   t4state4
		deca
		lbeq   t4state5
		deca
		lbeq   t4state6
		deca
		lbeq   t4state7
		deca
		lbeq   t4state8
		deca
		lbeq   t4state9
		deca
		lbeq   t4state10
		deca
		lbeq   t4state11
		deca
		lbeq   t4state12
		deca
		lbeq   t4state13
		deca
		lbeq   t4state14
		deca
		lbeq   t4state15
		deca
		lbeq   t4state16
		rts
;------------------------------------------------------------------------------	
t4state0:
	    jsr    INITLCD           ; initialize the LCD
        jsr    INITKEY			 ; initialize the keypad
		jsr    FLUSH_BFR	     ; flush the input buffer
		jsr    CURSOR			 ; turn on cursor
		clr    DPTR
		clr    Echo
        movb   #$01,t4state
        rts
;------------------------------------------------------------------------------	
t4state1:						; Hub state - waiting for display flag

		movb   #$10,t4state
		movb   #$01,FIRSTCH
		tst    DMESS16
		lbne   t4s1_wait   		  	  ; Display long blank for waveform clear
		
		movb   #$0A,t4state
		movb   #$01,FIRSTCH
		tst    DMESS10
		lbne   t4s1_wait   		  	  ; Display short blank for clearing NINT
		
		movb   #$0B,t4state
		movb   #$01,FIRSTCH
		tst    DMESS11
		lbne   t4s1_wait   		  	  ; Display long blank for error messages 
		
		movb   #$02,t4state
		movb   #$01,FIRSTCH
		tst    DMESS2
		lbne   t4s1_wait   		  	  ; Waveform selection display   		
		
		movb   #$03,t4state
		movb   #$01,FIRSTCH
		tst    DMESS3
		lbne   t4s1_wait   		  	  ; Display "SAWTOOTH WAVE"   	
		
		movb   #$04,t4state
		movb   #$01,FIRSTCH
		tst    DMESS4
		lbne   t4s1_wait   		  	  ; Display "7-SEGMENT SINE WAVE"  	
		
		movb   #$05,t4state
		movb   #$01,FIRSTCH
		tst    DMESS5
		lbne   t4s1_wait   		  	  ; Display "SQUARE WAVE" 		
		
		movb   #$06,t4state
		movb   #$01,FIRSTCH
		tst    DMESS6
		lbne   t4s1_wait   		  	  ; Display "NINT :"   	
		
		movb   #$07,t4state
		movb   #$01,FIRSTCH
		tst    DMESS7
		lbne   t4s1_wait   		  	  ; Display " [1-->255]"  
		 	
		movb   #$08,t4state
		movb   #$01,FIRSTCH
		tst    Echo
		lbne   t4s1_wait   		  	  ; Echo for digit display
		  	
		movb   #$09,t4state
		movb   #$01,FIRSTCH
		tst    BS_FLG
		lbne   t4s1_wait   		  	  ; Backspace  	
		 	
		movb   #$0C,t4state
		movb   #$01,FIRSTCH
		tst    DMESS12
		lbne   t4s1_wait   		  	  ; Display "NO DIGITS ENTERED"  
			
		movb   #$0D,t4state
		movb   #$01,FIRSTCH
		tst    DMESS13
		lbne   t4s1_wait   		  	  ; Display "ZERO MAGNITUDE"
		   	
		movb   #$0E,t4state
		movb   #$01,FIRSTCH
		tst    DMESS14
		lbne   t4s1_wait   		  	  ; Display "MAGNITUDE TOO LARGE"
		  	
		movb   #$0F,t4state
		movb   #$01,FIRSTCH
		tst    DMESS15
		lbne   t4s1_wait   		  	  ; Display "15-SEGMENT SINE WAVE"  
		
		movb   #$01,t4state
		bra    t4s1_wait

t4s1_wait:
        rts
		
;------------------------------------------------------------------------------	
t4state2:						; Initial prompts, '1: SAW; 2: SINE-7; 3: SQUARE'
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s2_write_disp    ; if zero, branch to t4s2_write_disp
		
t4s2_1st_disp:
	    ldaa   #$00				  ; starting address for message
		ldx    #MESS2             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s2_bottom
		
t4s2_write_disp:
        jsr    DISPLAY
		bra    t4s2_bottom
		
t4s2_bottom:
        tst    FIRSTCH
		beq    t4s2_skip
		clr    DMESS2
		movb   #$01,t4state
		
t4s2_skip:
		rts
		
;------------------------------------------------------------------------------	
t4state3:						; Sawtooth Wave
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s3_write_disp    ; if zero, branch to t4s3_write_disp
		
t4s3_1st_disp:
	    ldaa   #$40				  ; starting address for message
		ldx    #MESS3             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s3_bottom
		
t4s3_write_disp:
        jsr    DISPLAY
		bra    t4s3_bottom
		
t4s3_bottom:
        tst    FIRSTCH
		beq    t4s3_skip
		clr    DMESS3
		movb   #$01,t4state
		
t4s3_skip:
		rts
;------------------------------------------------------------------------------	
t4state4:						; 7-Segment Sine Wave
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s4_write_disp    ; if zero, branch to t4s4_write_disp
		
t4s4_1st_disp:
	    ldaa   #$40				  ; starting address for message
		ldx    #MESS4             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s4_bottom
		
t4s4_write_disp:
        jsr    DISPLAY
		bra    t4s4_bottom
		
t4s4_bottom:
        tst    FIRSTCH
		beq    t4s4_skip
		clr    DMESS4
		movb   #$01,t4state
		
t4s4_skip:
		rts
;------------------------------------------------------------------------------	
t4state5:						; Square Wave
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s5_write_disp    ; if zero, branch to t4s5_write_disp
		
t4s5_1st_disp:
	    ldaa   #$40				  ; starting address for message
		ldx    #MESS5             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s5_bottom
		
t4s5_write_disp:
        jsr    DISPLAY
		bra    t4s5_bottom
		
t4s5_bottom:
        tst    FIRSTCH
		beq    t4s5_skip
		clr    DMESS5
		movb   #$01,t4state
		
t4s5_skip:
		rts
;------------------------------------------------------------------------------	
t4state6:						; NINT
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s6_write_disp    ; if zero, branch to t4s6_write_disp
		
t4s6_1st_disp:
	    ldaa   #$54				  ; starting address for message
		ldx    #MESS6             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s6_bottom
		
t4s6_write_disp:
        jsr    DISPLAY
		bra    t4s6_bottom
		
t4s6_bottom:
        tst    FIRSTCH
		beq    t4s6_skip
		clr    DMESS6
		movb   #$01,t4state
		
t4s6_skip:
		rts
;------------------------------------------------------------------------------	
t4state7:						; Range [1-->255]
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s7_write_disp    ; if zero, branch to t4s7_write_disp
		
t4s7_1st_disp:
	    ldaa   #$5E				  ; starting address for message
		ldx    #MESS7             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s7_bottom
		
t4s7_write_disp:
        jsr    DISPLAY
		bra    t4s7_bottom
		
t4s7_bottom:
        tst    FIRSTCH
		beq    t4s7_skip
		clr    DMESS7
		movb   #$005A,DPTR			 ; set DPTR for NINT start address
		movb   #$01,t4state
		
t4s7_skip:
		rts
;------------------------------------------------------------------------------	
t4state8:						; Echo
		ldaa   DPTR			      ; load accumulator A with DPTR
		ldab   KEY_BUFF		      ; load accumulator B with KEY_BUFF
		jsr    OUTCHAR_AT
		clr    Echo				  ; clears Echo
		movb   #$01,t4state		  ; set t4state to 1
		rts
;------------------------------------------------------------------------------	
t4state9:						; Backspace
		ldaa   bsstate            ; get current bsstate and branch accordingly
        lbeq   bsstate0
        deca
        lbeq   bsstate1
        deca
        lbeq   bsstate2
		deca
		lbeq   bsstate3
        deca
		rts
		
bsstate0:
		movb   #$01,bsstate
		rts
		
bsstate1:		
        ldaa   DPTR               ; load DPTR into accumulator A
		staa   DPTR
		jsr    SETADDR			  ; Set LCD address
		movb   #$02,bsstate
		rts
		
bsstate2:
		ldaa   DPTR
		ldab   #$20               ; load accumulator B with an ASCII "space"
		jsr    OUTCHAR_AT         ; print character at
		movb   #$03,bsstate
		rts
		
bsstate3:
		ldaa   DPTR
		deca
		staa   DPTR				  ; store cursor address back into DPTR
		jsr    SETADDR			  ; move cursor on DISPLAY
		clr    BS_FLG			  ; clears BS_FLG
		dec    COUNT			  ; decrement COUNT
		ldx    POINTER
		dex
		stx    POINTER
		movb   #$01,bsstate
		movb   #$01,t4state
		rts
;------------------------------------------------------------------------------	
t4state10:						; Short Blank
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s10_write_disp    ; if zero, branch to t4s10_write_disp
		
t4s10_1st_disp:
	    ldaa   #$5A				  ; starting address for message
		ldx    #MESS10             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s10_bottom
		
t4s10_write_disp:
        jsr    DISPLAY
		bra    t4s10_bottom
		
t4s10_bottom:
        tst    FIRSTCH
		beq    t4s10_skip
		clr    DMESS10
		movb   #$01,t4state
		
t4s10_skip:
		rts
;------------------------------------------------------------------------------	
t4state11:						; Long Blank - Error Message
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s11_write_disp    ; if zero, branch to t4s11_write_disp
		
t4s11_1st_disp:
	    ldaa   #$54				  ; starting address for message
		ldx    #MESS11             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s11_bottom
		
t4s11_write_disp:
        jsr    DISPLAY
		bra    t4s11_bottom
		
t4s11_bottom:
        tst    FIRSTCH
		beq    t4s11_skip
		clr    DMESS11
		movb   #$01,t4state
		
t4s11_skip:
		rts
;------------------------------------------------------------------------------	
t4state12:						; No Digits
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s12_write_disp    ; if zero, branch to t4s12_write_disp
		
t4s12_1st_disp:
	    ldaa   #$54				  ; starting address for message
		ldx    #MESS12             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s12_bottom
		
t4s12_write_disp:
        jsr    DISPLAY
		bra    t4s12_bottom
		
t4s12_bottom:
        tst    FIRSTCH
		beq    t4s12_skip
		clr    DMESS12
		movb   #$01,t4state
		
t4s12_skip:
		rts
;------------------------------------------------------------------------------	
t4state13:						; Invalid Magnitude
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s13_write_disp    ; if zero, branch to t4s13_write_disp
		
t4s13_1st_disp:
	    ldaa   #$54				  ; starting address for message
		ldx    #MESS13             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s13_bottom
		
t4s13_write_disp:
        jsr    DISPLAY
		bra    t4s13_bottom
		
t4s13_bottom:
        tst    FIRSTCH
		beq    t4s13_skip
		clr    DMESS13
		movb   #$01,t4state
		
t4s13_skip:
		rts
;------------------------------------------------------------------------------	
t4state14:						; Magnitude too large
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s14_write_disp    ; if zero, branch to t4s14_write_disp
		
t4s14_1st_disp:
	    ldaa   #$54				  ; starting address for message
		ldx    #MESS14             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s14_bottom
		
t4s14_write_disp:
        jsr    DISPLAY
		bra    t4s14_bottom
		
t4s14_bottom:
        tst    FIRSTCH
		beq    t4s14_skip
		clr    DMESS14
		movb   #$01,t4state
		
t4s14_skip:
		rts
;------------------------------------------------------------------------------	
t4state15:						; 15-Segment Sine Wave
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s15_write_disp    ; if zero, branch to t4s15_write_disp
		
t4s15_1st_disp:
	    ldaa   #$40				  ; starting address for message
		ldx    #MESS15             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s15_bottom
		
t4s15_write_disp:
        jsr    DISPLAY
		bra    t4s15_bottom
		
t4s15_bottom:
        tst    FIRSTCH
		beq    t4s15_skip
		clr    DMESS15
		movb   #$01,t4state
		
t4s15_skip:
		rts
;------------------------------------------------------------------------------	
t4state16:						; Long Blank - Wave type
        tst    FIRSTCH            ; check FIRSTCH
		beq    t4s16_write_disp    ; if zero, branch to t4s16_write_disp
		
t4s16_1st_disp:
	    ldaa   #$40				  ; starting address for message
		ldx    #MESS16             ; message to be written
		jsr    DISPLAY_1ST
		bra    t4s16_bottom
		
t4s16_write_disp:
        jsr    DISPLAY
		bra    t4s16_bottom
		
t4s16_bottom:
        tst    FIRSTCH
		beq    t4s16_skip
		clr    DMESS16
		movb   #$01,t4state
		
t4s16_skip:
		rts
;=============================================================================
;
;    Subroutine TASK_5			; Function Generator

TASK_5: ldaa   t5state			; get current t5state and branch accordingly
        lbeq   t5state0
		deca
		lbeq   t5state1
		deca
		lbeq   t5state2
		deca
		lbeq   t5state3
		deca
		lbeq   t5state4
		rts
;------------------------------------------------------------------------------	
t5state0:
		clr    DPRMPT
		clr    WAVE
		movb   #$01,NEWBTI
        movb   #$01,t5state
		bset   DDRJ, pin5		; make pin5 of PORTJ an output
		bset   PORTJ, pin5		;take not LDAC high
        rts
;------------------------------------------------------------------------------	
t5state1:
		ldaa   WAVE

		movb   #$01,DWAVE       ; if WAVE = 1, set DWAVE
		movw   #SAW,WAVEPTR											
		cmpa   #$01
		lbeq   t5s1_exit
		
		movb   #$01,DWAVE       ; if WAVE = 2, set DWAVE
		movw   #SINE_7,WAVEPTR
		cmpa   #$02
		lbeq   t5s1_exit
		
		movb   #$01,DWAVE       ; if WAVE = 3, set DWAVE	
		movw   #SQUARE,WAVEPTR		
		cmpa   #$03
		lbeq   t5s1_exit
		
;		movb   #$01,DWAVE
;		movw   #SINE_15,WAVEPTR
;		cmpa   #$04
;		lbeq   t5s1_exit
		
		movb   #$00,DWAVE		
		rts
		
t5s1_exit:
		movb   #$02,t5state
		clr    WAVE
        rts
;------------------------------------------------------------------------------	
t5state2:
        tst    DWAVE            ; wait for display of wave message					;need?
        beq    t5s2a
        ldx    WAVEPTR          ; point to start of data for wave
        movb   0,X, CSEG        ; get number of wave segments
        movw   1,X, VALUE       ; get initial value for DAC
        movb   3,X, LSEG       	; load segment length
		inc    LSEG				; inc LSEC on first wave period
        movw   4,X, SEGINC     	; load segment increment
        inx                    	; inc SEGPTR to next segment
        inx
        inx
        inx
        inx
        inx
        stx    SEGPTR           ; store incremented SEGPTR for next segment
        movb   #$01, DPRMPT     ; set flag for display of NINT prompt				;need?
        movb   #$03, t5state    ; set next state
		
t5s2a:  rts
;------------------------------------------------------------------------------	
t5state3:
		tst    ENTER_FLG
		beq    t5s3_exit		; if ENTER_FLG is zero, exit state												
		tst    NINT
		beq    t5s3_exit		; if NINT is zero, exit state
		movb   #$01,NINT_OK		; set NINT_OK										;need?   			 						;don't need?
		movb   #$01,RUN			; set RUN
		movb   #$04,t5state
		movw   NINT,CINT
		 
t5s3_exit:
		rts
;------------------------------------------------------------------------------	
t5state4:
        tst    RUN
        beq    t5s4c            ; do not update function generator if RUN=0
        tst    NEWBTI
        beq    t5s4e            ; do not update function generator if NEWBTI=0
        dec    LSEG             ; decrement segment length counter
        bne    t5s4b            ; if not at end, simply update DAC output
        dec    CSEG             ; if at end, decrement segment counter
        bne    t5s4a            ; if not last segment, skip reinit of wave
        ldx    WAVEPTR          ; point to start of data for wave
        movb   0,X, CSEG        ; get number of wave segments
        inx                     ; inc SEGPTR to start of first segment
        inx
        inx
        stx    SEGPTR           ; store incremented SEGPTR
		
t5s4a:  ldx    SEGPTR           ; point to start of new segment
        movb   0,X, LSEG        ; initialize segment length counter
        movw   1,X, SEGINC      ; load segment increment
        inx                     ; inc SEGPTR to next segment
        inx
        inx
        stx    SEGPTR           ; store incremented SEGPTR
		
t5s4b:  ldd    VALUE            ; get current DAC input value
        addd   SEGINC           ; add SEGINC to current DAC input value
        std    VALUE            ; store incremented DAC input value
        bra    t5s4d
		
t5s4c:  movb   #$01, t5state    ; set next state

t5s4d:  clr    NEWBTI

t5s4e:  rts
;=============================================================================
;
;    Subroutines DISPLAY 

DISPLAY_1ST:						; sets address for first character
        stx    DPTR					; store DPTR in register X
		jsr    SETADDR				; move cursor
		clr    FIRSTCH				; clear FIRSTCH
		rts

DISPLAY:							; displays the next character	
        ldx    DPTR					
		ldab   0,X
		beq    DONE
		inx								
		stx    DPTR				
		jsr    OUTCHAR				; displays character at next address
		rts
		
DONE:   movb   #$01,FIRSTCH
        rts 
;=============================================================================
;
;    Interrupt Subroutine

INTERRUPT::		
        ldd    TC0H			; read current interrupt output compare value
        addd   #INTERVAL	; add INTERVAL to that TC0H
		std    TC0H			; store new value back into TC0H
		bset   TFLG1, C0F	; Clear timer channel 0 timer output compare interrupt flag
		tst    RUN
		beq    exit_interrupt
		dec    CINT
		bne    exit_interrupt
		ldd    VALUE
		jsr    OUTDACB
		movb   NINT,CINT
		movb   #$01,NEWBTI
exit_interrupt:		
		rti					; return from interrupt
;=============================================================================
;
;    OUTDACA Subroutine					 ; output to DAC A

OUTDACA::
        staa   DACAMSB                   ; output high byte of DAC value
        stab   DACALSB                   ; output low byte of DAC value
        bclr   PORTJ, pin5               ; toggle DAC latch 
        bset   PORTJ, pin5
		rts		
;=============================================================================
;
;    OUTDACB Subroutine					 ; output to DAC B

OUTDACB::
        staa   DACBMSB                   ; output high byte of DAC value
        stab   DACBLSB                   ; output low byte of DAC value
        bclr   PORTJ, pin5               ; toggle DAC latch 
        bset   PORTJ, pin5
		rts		
;======================================================================
; FLASH GORDON					; flash storage area

SAW::
		.byte   2               ; number of segments for SAW
        .word   0            	; initial DAC input value
        .byte   19              ; length for segment_1
		.word   173             ; increment for segment_1
		.byte	1               ; length for segment_2
		.word   -3287           ; increment for segment_2
		
SQUARE::
		.byte	4				; number of segments for SQUARE
		.word	0				; initial DAC input value
		.byte	1				; length for segment_1
		.word	3287			; increment for segment_1
		.byte	9				; length for segment_2
		.word	0				; increment for segment_2
		.byte	1				; length for segment_3
		.word	-3287			; increment for segment_3
		.byte	9				; length for segment_4
		.word	0				; increment for segment_4
		
SINE_7::
		.byte	7				; number of segments for 7 segment sine
		.word	2048			; initial DAC input value
		.byte	25              ; length for segment_1
		.word	36              ; increment for segment_1
		.byte	50              ; length for segment_2
		.word	9               ; increment for segment_2
		.byte	50              ; length for segment_3
		.word	-9              ; increment for segment_3
		.byte	50              ; length for segment_4
		.word	-36             ; increment for segment_4
		.byte	50              ; length for segment_5
		.word	-9              ; increment for segment_5
		.byte	50              ; length for segment_6
		.word	9               ; increment for segment_6
		.byte	25              ; length for segment_7
		.word	36              ; increment for segment_7
		
SINE_15::
        .byte   15              ; number of segments for SINE
        .word   2048            ; initial DAC input value
        .byte   10              ; length for segment_1
        .word   41              ; increment for segment_1
        .byte   21              ; length for segment_2
        .word   37              ; increment for segment_2
        .byte   21              ; length for segment_3
        .word   25              ; increment for segment_3
        .byte   21              ; length for segment_4
        .word   9               ; increment for segment_4
        .byte   21              ; length for segment_5
        .word   -9              ; increment for segment_5
        .byte   21              ; length for segment_6
        .word   -25             ; increment for segment_6
        .byte   21              ; length for segment_7
        .word   -37             ; increment for segment_7
        .byte   20              ; length for segment_8
        .word   -41             ; increment for segment_8
        .byte   21              ; length for segment_9
        .word   -37             ; increment for segment_9
        .byte   21              ; length for segment_10
        .word   -25             ; increment for segment_10
        .byte   21              ; length for segment_11
        .word   -9              ; increment for segment_11
        .byte   21              ; length for segment_12
        .word   9               ; increment for segment_12
        .byte   21              ; length for segment_13
        .word   25              ; increment for segment_13
        .byte   21              ; length for segment_14
        .word   37              ; increment for segment_14
        .byte   10              ; length for segment_15
        .word   41              ; increment for segment_15

MESS2::	.asciz '1: SAW; 2: SINE-7; 3: SQUARE;'
MESS3:: .asciz 'SAWTOOTH WAVE'
MESS4:: .asciz '7-SEGMENT SINE WAVE'
MESS5:: .asciz 'SQUARE WAVE'
MESS6::	.asciz ' NINT: '
MESS7::	.asciz ' [1-->255]'
MESS10::	.asciz '   '   	   		  		 ; Clear line for digits, short blank
MESS11::	.asciz '                    ' 	 ; Clear line for errors, long blank
MESS12::	.asciz 'NO DIGITS ENTERED'
MESS13::	.asciz 'ZERO MAGNITUDE'
MESS14::	.asciz 'MAGNITUDE TOO LARGE'
MESS15::	.asciz '15-SEGMENT SINE WAVE'
MESS16::    .asciz '                                        '; Clear entire bottom line

;======================================================================
;
.area interrupt_vectors(abs)
      .org      $FFEE		; at interrupt vector location
	  .word     INTERRUPT	; load INTERRUPT starting address
	  .org      $FFFE		; at reset vector location
	  .word     __start		; load starting address


