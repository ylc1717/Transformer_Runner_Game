#include p18f87k22.inc

    global  LCD_Setup
    global  Inner_counter_delay, Outer_counter_Speed_up, ad_delay_const,Restart_C
    global  my_higher_array, my_lower_array
    global  obs_gen,space_gen
    
    extern  setup
    extern  ran_lo, random_obs,random_space
    extern  Dino_status,Keypad_decoding

acs0    udata_acs   ; named variables in access ram
LCD_cnt_l   res 1   ; reserve 1 byte for variable LCD_cnt_l
LCD_cnt_h   res 1   ; reserve 1 byte for variable LCD_cnt_h
LCD_cnt_ms  res 1   ; reserve 1 byte for ms counter
LCD_tmp	    res 1   ; reserve 1 byte for temporary use
LCD_counter res 1   ; reserve 1 byte for counting through nessage
rotate_counter res 1 ; reserve 1 byte for rotate_counter to write out obs
my_lower_array res 1 
my_higher_array res 1 
LCD_second_line_counter res 1 
long_delay_counter  res 1
 
Score_counter	    res 1   ; reserve one byte for a counter variable
Score_store_1 res 1 ; reserve one byte for multiply 16bits with 16bits 
Score_store_2 res 1 ; reserve one byte for multiply 8bits with 8bits 
Score_store_3 res 1 ; reserve one byte for multiply 8bits with 8bits 
Score_store_4 res 1 ; reserve one byte for multiply 8bits with 8bits
Score_store_temp res 1 ; reserve one byte for multiply 8bits with 8bits 
Score_low     res 1 ;store the score when it is below 256
Score_high    res 1 ;store the score when it is above 256

acs_ovr	access_ovr
LCD_hex_tmp res 1   ; reserve 1 byte for variable LCD_hex_tmp	

map_carry   res 1   ;store the carry bit in my_higher_array 
map_rotate_counter res 1   ; rotate our map, bit by bi, so it back to its own position   
Inner_counter_delay res 1  ; determines the # of the 16-bit delay loop, refreshed by the outer counter
Outer_counter_Speed_up res 1;Decrease by one every loop, sothe map rotate faster every loop
ad_delay_counter  res 1	    ;refreshed by the ad_delay_const
ad_delay_const res 1	    ;adjust the difference between ground & sky delay period 


Ground_test res 1	; Test variable for measurements
Sky_test    res 1	; Test variable for measurements
H_del	    res 1
L_del	    res 1
Restart_C   res 1	; Use to restart the game in  Colide check function
	constant    LCD_E=5	; LCD enable bit
    	constant    LCD_RS=4	; LCD register select bit

LCD	code
    
LCD_Setup
	clrf    LATB
	movlw   b'11000000'	    ; RB0:5 all outputs
	movwf	TRISB
	movlw   .40
	call	LCD_delay_ms	; wait 40ms for LCD to start up properly
	movlw	b'00110000'	; Function set 4-bit
	call	LCD_Send_Byte_I
	movlw	.10		; wait 40us
	call	LCD_delay_x4us
	movlw	b'00101000'	; 2 line display 5x8 dot characters
	call	LCD_Send_Byte_I
	movlw	.10		; wait 40us
	call	LCD_delay_x4us
	movlw	b'00101000'	; repeat, 2 line display 5x8 dot characters
	call	LCD_Send_Byte_I
	movlw	.10		; wait 40us
	call	LCD_delay_x4us
	movlw	b'00001111'	; display on, cursor on, blinking on
	call	LCD_Send_Byte_I
	movlw	.10		; wait 40us
	call	LCD_delay_x4us
	movlw	b'00000001'	; display clear
	call	LCD_Send_Byte_I
	
	movlw	.2		; wait 2ms
	call	LCD_delay_ms
	movlw	b'00000110'	; entry mode incr by 1 no shift
	call	LCD_Send_Byte_I
	movlw	.10		; wait 40us
	call	LCD_delay_x4us
	return

obs_gen
	
	call	LCD_w22		    ; LCD write on the second line second digit
	
	bsf	STATUS, C	    ; set carry bit to 1
	rlcf	my_lower_array,1    ; the LS bit is always one
	rlcf	my_higher_array,1   ; transfer the carry bit to higher arrray
	rlcf	map_carry,1	    ; carry bit stored in the map_carry for later use
	call	LCD_show_array
	
	    
	movlw	.0		; Load zero to test
	cpfseq	Dino_status	; if = 0, skip next.
	call	Sky_diving
	movlw	.0		; Load five to test
	cpfsgt	Dino_status	; Dino status greater than 0. skip, carry on otherwise
	call	Ground_delay_check
	
	; If Dino is not on the ground, it must in the sky, excute the next line
	movlw	.0		; Load zero to test
	cpfseq	Dino_status	; Skip if =0, otherwise 
	decf	Dino_status	; Decrease Dino status by one
	
	call	LCD_Setup	; LCD display clear, move to 1st line 1st digit
	bcf	STATUS, C
	incf	Score_low	; Score added one
	rlcf	Score_high	; Move the Carry bit of Score_low to Score_high,
	decfsz	random_obs	; the LS bit is always one
	bra	obs_gen
	return

space_gen
	
	call	LCD_w22		  ; LCD write in the second line 2 2nd digit
	bcf	STATUS, C	  ;set carry bit to 0
	rlcf	my_lower_array,1  ;the LS bit is always zero, rotate it and store back in my_lower_array
	rlcf	my_higher_array,1
	rlcf	map_carry,1
	call	LCD_show_array
	
	movlw	.0		    ; Load zero to test
	cpfseq	Dino_status
	call	Sky_diving
	movlw	.0		    ; Load five to test
	cpfsgt	Dino_status
	call	Ground_delay_check
	; If Dino is not on the ground, it must in the sky, excute the next line
	movlw	.0		    ; Load zero to test
	cpfseq	Dino_status
	decf	Dino_status
	
	call	LCD_Setup
	incf	Score_low	; Score added one
	rlcf	Score_high	; Move the Carry bit of Score_low to Score_high,
	decfsz	random_space	    ; the LS bit is always one
	bra	space_gen
	return
	
;************************    Above is Main Code      ********************************************
LCD_show_array
	movlw	0x08
	movwf	map_rotate_counter
LCD_show_higher_array		    ;Function used to show the array bit by bit
	btfsc   my_higher_array,7   ;Bit 7 test, skip if zero
	call	LCD_write_obs
	btfss   my_higher_array,7   ;Bit 7 test, skip if one
	call	LCD_wtire_space
	rlncf	my_higher_array	    ;Rotate the 8 bits array
	decfsz  map_rotate_counter  ;A counter whose initial value is .8
	bra	LCD_show_higher_array
	;return
	movlw	0x08
	movwf	map_rotate_counter
LCD_show_lower_array		    ;Samilar function used to show the other array
	btfsc   my_lower_array,7    ;Bit 7 test, skip if zero
	call	LCD_write_obs
	btfss   my_lower_array,7    ;Bit 7 test, skip if one
	call	LCD_wtire_space
	rlncf	my_lower_array	    ;Rotate the 8 bits array
	decfsz  map_rotate_counter  ;A counter whose initial value is .8
	bra	LCD_show_lower_array
	return
	
Ground_delay_check
	incf	Ground_test	    ;Used to show the rotate speed on the Scope, When the transformer is on the ground
	movff	Ground_test,PORTH   ;Move the value to PORTH, the vlotage of bit 0 is tested. 
				    ;The rotating speed is proportional to the The frequency of the voltage.
	call	Colide_check	    ;Check if the transformer is colide with the obstical.
	movff	Outer_counter_Speed_up,Inner_counter_delay
				    ;Import an counter which controls the rotate speed
				    
	movlw 0xff		    ; Set up the 16 bits Delay loop
	movwf H_del		    ; FR H_del
	movlw 0x20		    
	movwf L_del		    ; and FR L_del. The L_del controls the delay time
sub_Ground
	
	movlw 0x00 ; W=0
	
cnd_loop    decf H_del,f	    ; no carry when 0x00 -> 0xff
	    subwfb L_del,f	    ; no carry when 0x00 -> 0xff
	    cpfsgt  Dino_status	    ; Skip if Dino_status is not zero/ the transfomer is not on the ground
	    call    Keypad_decoding 
	    call    Jump_check	    ; Show if transfomer is jump or not
	    bc cnd_loop		    ; if carry, then loop again
	
	decfsz	Inner_counter_delay ;The Initial value ofInner_counter_delay controls the delay speed
	bra	sub_Ground
	return			    ; carry not set so return
	
Sky_diving
	incf	Sky_test	    ;Used to show the rotate speed on the Scope, When the transformer is on the sky.
	movff	Sky_test,PORTJ	    ;Move the value to PORTJ, the vlotage of bit 0 is tested. 
				    ;The rotating speed is proportional to the The frequency of the voltage.
	call	Jumped		    ;Function that shows the transformer on the sky
	movff	Outer_counter_Speed_up,Inner_counter_delay
	movlw 0xff		    
	movwf H_del		    
	movlw 0x20
	movwf L_del		    
sub_long	
	movlw 0x00; W=0
	
dloop	decf H_del,f		    ; no carry when 0x00 -> 0xff
	subwfb L_del,f		    ; no carry when 0x00 -> 0xff
	movff ad_delay_const,ad_delay_counter
	call adjust_delay	    ; Call a delay loop to reduce the run time difference between Ground_delay_check & Sky_diving
	bc dloop		    ; if carry, then loop again

	decfsz	Inner_counter_delay
	bra	sub_long
	
	return			    ; carry not set so return
	

Jump_check	;Dino_status can only = to 5 or 0
	btfsc	Dino_status,0	    ; if equal to zero skip next
	call	Jumped
	btfss	Dino_status,0	    ; if equal to 1 skip next
	call	Didnot_jump
	return
	
	
Colide_check
	btfsc   map_carry,0	    ;Bit 0 test, skip if zero
	call	LCD_wtire_game_over_and_Score
	
	btfsc   map_carry,0	    ;Bit 0 test, skip if zero
	goto	setup
	return
	
	
Didnot_jump	
	call	LCD_Setup
	call	LCD_w21		    ; Write on the 2 line, first bit
	call	LCD_write_tank	    ; Write tank
	call	LCD_show_array	    
	return
Jumped	
	call	LCD_Setup
	call	LCD_write_jet
	call	LCD_w20		    ; Write on the 2 line, first bit
				    ; As the trans is on the 1 line, first bit, we reduce the cursor by 1
	btfss	map_carry,0
	call	LCD_wtire_space
	btfsc	map_carry,0
	call	LCD_write_obs
	call	LCD_show_array
	return

adjust_delay 
	decfsz ad_delay_counter ; decrement until zero
	bra adjust_delay
	return

;************************************** Below are the functions used to write on the LCD ******************
	
LCD_write_obs
	movlw	0xec
	call	LCD_Send_Byte_D
	return
	
LCD_write_jet
	movlw	0xbc
	call	LCD_Send_Byte_D
	return
	
LCD_write_tank
	movlw	0xaa
	call	LCD_Send_Byte_D
	return
	
LCD_wtire_space
	movlw	0x2e
	call	LCD_Send_Byte_D
	return
	
LCD_w20	; Shift cursor 39 times to the second line first digit
	;(The transformer will be appear on the first line first digit)
	movlw	.39
	movwf	LCD_second_line_counter
LCD_shift_cursor_2	
	movlw	b'00010100'	; shift cursor to the second line
	call	LCD_Send_Byte_I
	movlw	.10
	call	LCD_delay_x4us
	decfsz	LCD_second_line_counter
	bra	LCD_shift_cursor_2
	return
	
LCD_w21	; Shift cursor 40 times to the second line first digit
	movlw	.40
	movwf	LCD_second_line_counter
LCD_shift_cursor_1	
	movlw	b'00010100'	; shift cursor to the second line
	call	LCD_Send_Byte_I
	movlw	.10
	call	LCD_delay_x4us
	decfsz	LCD_second_line_counter
	bra	LCD_shift_cursor_1
	return
	
LCD_w22; Shift cursor 41 times to the second line second digit
	movlw	.41
	movwf	LCD_second_line_counter
LCD_shift_cursor	
	movlw	b'00010100'	; shift cursor to the second line
	call	LCD_Send_Byte_I
	movlw	.10
	call	LCD_delay_x4us
	decfsz	LCD_second_line_counter
	bra	LCD_shift_cursor
	return
	
LCD_wtire_game_over_and_Score
	call	LCD_Setup
	movlw	.3
	movwf	LCD_second_line_counter
LCD_shift_cursor_4	
	movlw	b'00010100'	; shift cursor to the second line
	call	LCD_Send_Byte_I
	movlw	.10
	call	LCD_delay_x4us
	decfsz	LCD_second_line_counter
	bra	LCD_shift_cursor_4
	
	movlw	0x47
	call	LCD_Send_Byte_D
	movlw	0x41
	call	LCD_Send_Byte_D
	movlw	0x4d
	call	LCD_Send_Byte_D
	movlw	0x45
	call	LCD_Send_Byte_D
	movlw	0x20
	call	LCD_Send_Byte_D
	movlw	0x4f
	call	LCD_Send_Byte_D
	movlw	0x56
	call	LCD_Send_Byte_D
	movlw	0x45
	call	LCD_Send_Byte_D
	movlw	0x52
	call	LCD_Send_Byte_D
	
	movlw	.31
	movwf	LCD_second_line_counter
LCD_shift_cursor_5	
	movlw	b'00010100'	; shift cursor to the second line
	call	LCD_Send_Byte_I
	movlw	.10
	call	LCD_delay_x4us
	decfsz	LCD_second_line_counter
	bra	LCD_shift_cursor_5
	
	call	Write_Score
	call	Delay_game_over
	return
Delay_game_over	
	movlw	.50
	movwf	long_delay_counter	
sub_long_game_over	
	movlw 0x00 ; W=0	
d_loop	decf 0x11,f ; no carry when 0x00 -> 0xff
	subwfb 0x10,f ; no carry when 0x00 -> 0xff
	bc d_loop ; if carry, then loop again	
	decfsz	long_delay_counter
	bra	sub_long_game_over
	return ; carry not set so return

Write_Score
	movlw	b'01010011'	; S
	call	LCD_Send_Byte_D
	movlw	b'01000011'	; C
	call	LCD_Send_Byte_D
	movlw	b'01001111'	; O
	call	LCD_Send_Byte_D
	movlw	b'01010010'	; R
	call	LCD_Send_Byte_D
	movlw	b'01000101'	; E
	call	LCD_Send_Byte_D
	movlw	b'00111010'	; :
	call	LCD_Send_Byte_D
	
Mul_k	movf	Score_low,W	; mult the lower bytes with 8A
	mullw	0x8a
	movff	PRODH,Score_store_2 ;Store PRODH in store_2
	movff	PRODL,Score_store_1 ;Store PRODH in store_1
	
	movf	Score_high,W	;  mult the higher bytes with 8A
	mullw	0x8a
	movf	PRODL,W		
	addwf	Score_store_2,1	;  PRODL add with store_2
	
	movf	PRODH,W
	movwf	Score_store_3	; Store the third byte in store_3
	movlw	0x00
	addwfc	Score_store_3,1	; clear carry bit and store it in PRODH,
	
	movf	Score_low,W	;mult the number with 41
	mullw	0x41
	
	movf	PRODL,W
	addwf	Score_store_2,1	;without  carry bit***
	movf	PRODH,W
	addwfc	Score_store_3,1 ;  add with carry bit***
	
	movf	Score_high,W
	mullw	0x41
	movf	PRODH,W
	movwf	Score_store_4
	movlw	0x00
	addwfc	Score_store_4,1	; clear the carry bit from the third bit***, This works
	
	movf	PRODL,W
	addwf	Score_store_3,1
	movlw	0x00
	addwfc	Score_store_4,1 ; clear the carry bit from the third bit***
	
	movf	Score_store_4,W
	call	LCD_Write_low_nibble	 ; print the highest byte, this is decimal

	movlw	.03	    ;loop mul_ten 3 times
  	movwf	Score_counter	
Mul_ten	
	movf	Score_store_1,W
	mullw	0x0a
	movff	PRODL,Score_store_1 ;Store PRODH in store_1
	movff	PRODH,Score_store_temp ;Store PRODH in store_2
	
	movf	Score_store_2,W
	mullw	0x0a
	movf	PRODL,W		
	addwf	Score_store_temp,0	;  PRODL add with temp
	movwf	Score_store_2		; Store it in store 2
	movf	PRODH,W
	movwf	Score_store_temp
	movlw	0x00
	addwfc	Score_store_temp,1	; clear carry bit and store it in temp, 
		
	movf	Score_store_3,W
	mullw	0x0a
	movf	PRODL,W		
	addwf	Score_store_temp,0	;  PRODL add with store_3
	movwf	Score_store_3
	movf	PRODH,W
	movwf	Score_store_4
	movlw	0x00
	addwfc	Score_store_4,1		;clear carry bit
	
	movf	Score_store_4,W
	call	LCD_Write_low_nibble	 ; print the highest byte, this is decimal
	
	decfsz	Score_counter
	bra Mul_ten
	
	movlw	0x44
	movwf	Restart_C
restart
	call	Keypad_decoding
	cpfseq	Restart_C
	bra	restart
	movlw	0x00
	movwf	Score_low
	movwf	Score_high
	return


	
;******************** Essential LCD Writing Functions************************************
	
LCD_Send_Byte_I		    ; Transmits byte stored in W to instruction reg
	movwf   LCD_tmp
	swapf   LCD_tmp,W   ; swap nibbles, high nibble goes first
	andlw   0x0f	    ; select just low nibble
	movwf   LATB	    ; output data bits to LCD
	bcf	LATB, LCD_RS	; Instruction write clear RS bit
	call    LCD_Enable  ; Pulse enable Bit 
	movf	LCD_tmp,W   ; swap nibbles, now do low nibble
	andlw   0x0f	    ; select just low nibble
	movwf   LATB	    ; output data bits to LCD
	bcf	LATB, LCD_RS; Instruction write clear RS bit
        call    LCD_Enable  ; Pulse enable Bit 
	return

LCD_Send_Byte_D		    ; Transmits byte stored in W to data reg
	movwf   LCD_tmp
	swapf   LCD_tmp,W   ; swap nibbles, high nibble goes first
	andlw   0x0f	    ; select just low nibble
	movwf   LATB	    ; output data bits to LCD
	bsf	LATB, LCD_RS	; Data write set RS bit
	call    LCD_Enable  ; Pulse enable Bit 
	movf	LCD_tmp,W   ; swap nibbles, now do low nibble
	andlw   0x0f	    ; select just low nibble
	movwf   LATB	    ; output data bits to LCD
	bsf	LATB, LCD_RS    ; Data write set RS bit	    
        call    LCD_Enable  ; Pulse enable Bit 
	movlw	.10	    ; delay 40us
	call	LCD_delay_x4us
	return
LCD_Write_low_nibble
	movwf	LCD_hex_tmp
	movf	LCD_hex_tmp,W	; then low nibble
	call	LCD_Hex_Nib
	return
	
LCD_Hex_Nib	    ; writes low nibble as hex character
	andlw	0x0F
	movwf	LCD_tmp
	movlw	0x0A
	cpfslt	LCD_tmp
	addlw	0x07	; number is greater than 9 
	addlw	0x26
	addwf	LCD_tmp,W
	call	LCD_Send_Byte_D ; write out ascii
	return
LCD_Enable	    ; pulse enable bit LCD_E for 500ns
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	bsf	    LATB, LCD_E	    ; Take enable high
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	bcf	    LATB, LCD_E	    ; Writes data to LCD
	return
    
; ** a few delay routines below here as LCD timing can be quite critical ****
LCD_delay_ms		    ; delay given in ms in W
	movwf	LCD_cnt_ms
lcdlp2	movlw	.250	    ; 1 ms delay
	call	LCD_delay_x4us	
	decfsz	LCD_cnt_ms
	bra	lcdlp2
	return
    
LCD_delay_x4us		    ; delay given in chunks of 4 microsecond in W
	movwf	LCD_cnt_l   ; now need to multiply by 16
	swapf   LCD_cnt_l,F ; swap nibbles
	movlw	0x0f	    
	andwf	LCD_cnt_l,W ; move low nibble to W
	movwf	LCD_cnt_h   ; then to LCD_cnt_h
	movlw	0xf0	    
	andwf	LCD_cnt_l,F ; keep high nibble in LCD_cnt_l
	call	LCD_delay
	return

LCD_delay			; delay routine	4 instruction loop == 250ns	    
	movlw 	0x00		; W=0
lcdlp1	decf 	LCD_cnt_l,F	; no carry when 0x00 -> 0xff
	subwfb 	LCD_cnt_h,F	; no carry when 0x00 -> 0xff
	bc 	lcdlp1		; carry, then loop again
	return			; carry reset so return

    end

