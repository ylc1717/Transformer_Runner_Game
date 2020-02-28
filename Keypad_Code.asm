#include p18f87k22.inc
    global  Keypad_decoding, Keypad_set_up, Dino_status
    
Keypad_ram	udata_acs   ; named variables in access ram
Keypad_value	res 1   ;constant    Keypad = 0x20    ?????????
myprotD		res 1   ;constant    myprotD = 0x40
mydelay1	res 1   ;constant    mydelay1 = 0x60
mydelay		res 1   
Dino_status	res 1	;define the status of Dino, should only use 0 and 1.
	
    constant    Null = 0xff ;Decoding the keypad
    constant    Zero = 0xbe 
    constant    Clear = 0xee
	
Keypad  code 
 
Keypad_set_up
    setf TRISE ; Tri-state PortE
    banksel PADCFG1 ; PADCFG1 is not in Access Bank!!
    bsf PADCFG1, REPU, BANKED ; PortE pull-ups on
    movlb 0x00 ; set BSR back to Bank 0
    clrf LATE ; Write 0s to the LATE register 
    return
    
Keypad_decoding
  
    movlw 0x0f
    movwf TRISE, ACCESS ;PORTE 4-7 as outputs and  PORTE 0-3 as  inputs
    movlw 0xff
    movwf mydelay1
    call  delay1        ;Delay for the voltage on PORTE pins to settle
   
    movf PORTE, W    
    movwf Keypad_value, ACCESS

    movlw 0xf0
    movwf TRISE, ACCESS ;PORTE 4-7 as Inpur and  PORTE 0-3 as  output
    
    movlw 0x00
    movwf TRISD, ACCESS ; set PROT D as output to check the keyboard
  
    movlw 0xff
    movwf mydelay1
    call  delay1        ;Delay for the voltage on PORTE pins to settle
    
    movf Keypad_value, W   
    addwf PORTE, W      ;Add W to Keypad,
    movwf Keypad_value  

    movf Keypad_value,W   
    movwf myprotD, ACCESS
    movff myprotD, PORTD ;check the keyboard use PORT D
    
test_null
    movlw   Null
    cpfseq  Keypad_value
    goto    test_zero
    retlw   0x00    ; If you do nothing, Dino shound stay at the original position
test_zero
    movlw Zero
    cpfseq Keypad_value
    goto test_clear
    movlw   0x05
    movwf   Dino_status
    return 
test_clear
    movlw   Clear
    cpfseq  Keypad_value
    retlw   0x44
    

delay 
    decfsz mydelay, F, ACCESS
   
                   
delay1 
    decfsz mydelay1, F, ACCESS
    bra delay1
    return

    end
