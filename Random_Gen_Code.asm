
#include p18f87k22.inc	
	global	ran_lo, random_obs, random_space,RANDOM,random_temp
	
acs0		udata_acs   ; named variables in access ram
RANDOM		res 1   ; Ramdom number
random_temp	res 1   ; Store the temp middle random bit 0 
random_obs	res 1   ; reserve one byte for the length of obstacle
random_space	res 1  ; reserve one byte for the space between obstacle
	
random  code 	
ran_lo	

	rlncf	RANDOM,W          ;Rotate left without carry bit
        btfsc   RANDOM,4          ;Bit 4 test, skip if zero
        xorlw   1                 ;NOT gate of W
        btfsc   RANDOM,5          ;Bit 5 test, skip if zero
        xorlw   1                 ;NOT gate of W
        btfsc   RANDOM,3          ;Bit 3 test, skip if zero
        xorlw   1                 ;NOT gate of W
        movwf   RANDOM 
	
	movlw	.0                ;Set bit 0 zero
	movwf	random_obs
	btfsc	RANDOM,0          ;Bit 0 test, skip if zero
	call	write_1_2bits       ;Write zero in bit 0
	
	movlw	.0                ;Set bit 1 zero
	movwf	random_temp       
	btfsc	RANDOM,1          ;Bit 1 test, skip if zero
	call	write_2_temp       ;Write zero in bit 1
	
	movf    random_temp,W		;********
	addwf   random_obs      ;Add two results to get lowest two bits
	                          ;(gives range 0-3)

	;***********************************************************;
	
	movlw	.00               ;Set bit 0 zero
	movwf	random_space
	btfsc	RANDOM,2          ;Bit 0 test, skip if zero
	call	write_1_space      ;Write 1 in bit 0
	
	
	movlw	.0                ;Set bit 1 zero
	movwf	random_temp       
	btfsc	RANDOM,3          ;Bit 1 test, skip if zero
	call	write_2_temp       ;Write 1 in bit 1
	
	movf    random_temp,W
	addlw	0x04
	addwf   random_space      ;Add two results to get lowest two bits
	                          ;(gives range 0-3)
        return   

write_1_2bits
	movlw	0x01               
	movwf	random_obs
	return
	
write_2_temp
	movlw	0x02
	movwf	random_temp
	return
	
write_1_space
	movlw	0x01 
	movwf	random_space
	return	
	
	end