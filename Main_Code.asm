#include p18f87k22.inc

    ;extern	UART_Setup, UART_Transmit_Message  ; external UART subroutines
    extern  LCD_Setup	    ; external LCD subroutines
    extern  Keypad_decoding, Keypad_set_up,Dino_status		    ;functions from Keypad
    extern  RANDOM,ran_lo, random_obs, random_space,random_temp	    ;functions from Random_gen
    extern  obs_gen,space_gen					    ;functions from LCD
    extern  Inner_counter_delay, Outer_counter_Speed_up, ad_delay_const	    ; Global variables from LCD
    extern  my_higher_array, my_lower_array	    ; Global variables from LCD
    
    global  setup

acs0	udata_acs    ; reserve data space in access ram
counter	    res 1    ; reserve one byte for a counter variable
delay_count res 1    ; reserve one byte for counter in the delay routine

 
rst	code	0    ; reset vector
	goto	setup
	
main	code
	; ******* Programme FLASH read Setup Code ***********************
setup	bcf	EECON1, CFGS	; point to Flash program memory  
	bsf	EECON1, EEPGD 	; access Flash program memory
	call	Keypad_set_up
	call	LCD_Setup	; setup LCD
	movlw	0x5
	movwf	RANDOM		; Seed the random generator
	movlw	0x00
	movwf	my_lower_array
	movwf	my_higher_array
	movlw	.20	; set up the increasing speed of the game
	movwf	Outer_counter_Speed_up
	movlw	0x00
	movwf	TRISH,ACCESS	; Output PORTH for measurements
	movlw	0x00
	movwf	TRISJ,ACCESS	; Output PORTJ for measurements
	movlw	.1
	movwf	ad_delay_const	; Adjustment delay to fix the speed difference between the ground and the sky
	goto	moving_map
	
moving_map
	
	call	ran_lo		    ; Random number generator function
	movlw	0x00
	cpfseq	random_obs	    ; if random_obs = 0, call space_gen. otherwise obs_gen then space_gen
	call	obs_gen		    ; Obstacles generating function
	call	space_gen	    ; Empty space generating function
	movlw	0x01		    ; Use .1 rather than .0 prevent the decf instruction "restriction"
	cpfseq	Outer_counter_Speed_up	    ;Skip until Outer_counter_Speed_up dec to one
	decf	Outer_counter_Speed_up
	bra	moving_map


	
	end