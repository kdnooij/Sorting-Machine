;
;  2012.12.06:  Rob Hoogerwoord
;
;               Test program to check all functions of the PP2 Board, as follows:
;
;               -- Copies the Push Buttons to the corresponding Outputs;
;                  VERSION to test the Output Protection circuits: PB0 activates
;                  outputs 0..3 and PB1 activates outputs 4..7
;               -- Copies the slide switches to the corresponding three LEDs;
;               -- Displays the values of A/D-converter inputs 0 and 1 on the
;                  7-segments display: A/D-0 on digits 0..2 and A/D-1 on
;                  digits 3..5
;               -- The values read from the A/D-converters are numbers in the
;                  range [0..255] ; the analogue inputs have voltages in the
;                  range 0..5 Volt. By appropriate scaling the values shown on the
;                  are the actual voltages; the decimal points are used
;               -- The Serial Port is NOT tested: if the program can be loaded at
;                  all, the Serial Port is working all right anyhow.
;               -- To test the individual segments, including the decimal points,
;                  of the 7-segments display, pressing a push button switches off
;                  the display of the A/D-converters but lights the corresponding
;                  segment instead.
;
@DATA
       Digits      DS     6  ;  bit patterns for display elements 0..5
       DigNr       DS     1  ;  number of the "next" digit to be shown
       DigSel      DS     1  ;  bit pattern to select the "next" digit
                             ;  INVARIANT:  DigSel = 2 ^ DigNr  AND  DigNr IN [0..5]
;
@CODE
;
      IO_AREA     EQU   -16  ;  base address of the I/O-Area
       MEMSIZ     EQU    15  ;  (constant) register holding the size of memory
        TIMER     EQU    13  ;  timer register
       OUTPUT     EQU    11  ;  the 8 outputs
         LEDS     EQU    10  ;  the 3 LEDs above the 3 slide switches
       DSPDIG     EQU     9  ;  register selecting the active display element
       DSPSEG     EQU     8  ;  register for the pattern on the active display element
        INPUT     EQU     7  ;  the 8 inputs
      ADCONVS     EQU     6  ;  the outputs, concatenated, of the 2 A/D-converters
       RXDATA     EQU     5  ;  (input) register holding the last character received
       RXSTAT     EQU     4  ;  status of the serial receiver
       TXDATA     EQU     3  ;  (output) register for the character to be sent
       TXSTAT     EQU     2  ;  status of the serial sender
        BDSEL     EQU     1  ;  (output) register for the baud rate
       SWITCH     EQU     0  ;  the 3 slide switches
;
     TIM001ms     EQU    10  ;  1ms equals 10 TIMER steps
;
;    Initialization:
;
 PP2BoardTest :  LOAD  R5  IO_AREA       ; R5 := "base address of I/O area"
                 LOAD  R0  TIM001ms      ;
                  SUB  R0  [R5+TIMER]    ;
                 STOR  R0  [R5+TIMER]    ; initialise timer
                 LOAD  R0  0             ;
                 STOR  R0  [GB+DigNr]    ; DigNr := 0
                 STOR  R0  [GB+Digits+0] ; Digits[0] := 0 , i.e. "blank"
                 STOR  R0  [GB+Digits+1] ; Digits[1] := 0
                 STOR  R0  [GB+Digits+2] ; Digits[2] := 0
                 STOR  R0  [GB+Digits+3] ; Digits[3] := 0
                 STOR  R0  [GB+Digits+4] ; Digits[4] := 0
                 STOR  R0  [GB+Digits+5] ; Digits[5] := 0
                 LOAD  R0  1             ;
                 STOR  R0  [GB+DigSel]   ; DigSel := 1
;
;    Main Loop:
;
      ForEver :  LOAD  R0  [R5+TIMER]    ; 1ms interval elapsed?
                  BPL  ForEver           ; IF not THEN skip ELSE...
                 LOAD  R0  TIM001ms      ;
                 STOR  R0  [R5+TIMER]    ; bump timer by 1ms
;
                 LOAD  R0  [R5+INPUT]
                 LOAD  R1  R0
                  AND  R0  %00001        ; R0 := PB[0]
                  AND  R1  %00010
                  DIV  R1  2             ; R1 := PB[1]
                 MULS  R0  %000001111    ; copy PB[0] into R0[0..3]
                 MULS  R1  %011110000    ; copy PB[1] into R1[4..7]
                   OR  R0  R1            ; combine the results
                 STOR  R0  [R5+OUTPUT]   ; OUTPUT := R0
;
                 LOAD  R0  [R5+SWITCH]
                 STOR  R0  [R5+LEDS]     ; LEDS := SWITCH
;
                 LOAD  R0  [R5+ADCONVS]  ; R0 :=  AD1 ++ AD0
                 DVMD  R0  256           ; R0,R1 := AD1,AD0
                 PUSH  R0                ; save AD1 , R1 = AD0
                 LOAD  R0  0             ; x := 0
                 MULS  R1  100           ; scale measured value to a voltage:
                  ADD  R1    4           ; add a small correction first, then:
                  DIV  R1   51           ; R1 := R1 * (500/255)
                  BRS  Nat2Dec3          ; convert AD0 to Digits[2..0]
                 PULL  R1                ; R1 := AD1 (pulled from stack)
                 LOAD  R0  3             ; x := 3
                 MULS  R1  100           ; scale measured value to a voltage:
                  ADD  R1    4           ; add a small correction first, then:
                  DIV  R1   51           ; R1 := R1 * (500/255)
                  BRS  Nat2Dec3          ; convert AD0 to Digits[5..3]
;
                 LOAD  R0  [R5+INPUT]    ; IF no input button is pressed
                  BEQ  Display           ; THEN skip ELSE ...
                 STOR  R0  [GB+Digits+0] ; Digits[0] := INPUT
                 STOR  R0  [GB+Digits+1] ; Digits[1] := INPUT
                 STOR  R0  [GB+Digits+2] ; Digits[2] := INPUT
                 STOR  R0  [GB+Digits+3] ; Digits[3] := INPUT
                 STOR  R0  [GB+Digits+4] ; Digits[4] := INPUT
                 STOR  R0  [GB+Digits+5] ; Digits[5] := INPUT
;
      Display :   BRS  UpdateDisp        ; update display
;
                  BRA  ForEver           ; continue
;
;    Routine Nat2Dec3 converts a number in the range [0..500] to 3 decimal digits
;    and stores these in array segment Digits[x..x+2].
;    R1 : upon entry, contains the number to be converted
;    R0 : upon entry, contains x ; precondition:  x = 0  OR x = 3
;    Registers R2..4 are used for local purposes.
;
     Nat2Dec3 :  LOAD  R4  R0 
                  ADD  R4  Digits        ; R4 := offset(Digits[x])
                 LOAD  R2  R1
                 DVMD  R2  10            ; R2,R3  :=  R2 DIV 10 , R2 MOD 10
                 LOAD  R0  R3
                  BRS  Hex2Seg7          ; R1 := "7-segment pattern for" R3
                 STOR  R1  [GB+R4]       ; Digits[x] := "next decimal digit"
                  ADD  R4  1             ; R4 := offset(Digits[x+1])
                 DVMD  R2  10            ; R2,R3  :=  R2 DIV 10 , R2 MOD 10
                 LOAD  R0  R3
                  BRS  Hex2Seg7          ; R1 := "7-segment pattern for" R3
                 STOR  R1  [GB+R4]       ; Digits[x+1] := "next decimal digit"
                  ADD  R4  1             ; R4 := offset(Digits[x+2])
                 DVMD  R2  10            ; R2,R3  :=  R2 DIV 10 , R2 MOD 10
                 LOAD  R0  R3
                  BRS  Hex2Seg7          ; R1 := "7-segment pattern for" R3
                   OR  R1  %10000000     ; set decimal point
                 STOR  R1  [GB+R4]       ; Digits[x] := "next decimal digit"
                  RTS
;
;    Routine UpdateDisp updates the 7-segments display.
;    Registers R0 and R1 are used for local purposes.
;
   UpdateDisp :  LOAD  R0  [GB+DigNr]
                  CMP  R0  5             ; right-most digit?
                  BEQ  UpdateDig5        ; IF so THEN proceed with digit 5 ELSE
   UpdateDg04 :  LOAD  R1  R0            ; proceed with digits 0..4
                  ADD  R1  Digits        ; R1 := offset(Digits[DigNr])
                 LOAD  R1  [GB+R1]       ; R1 := Digits[DigNr]
                 STOR  R1  [R5+DSPSEG]   ; display the pattern on the next digit
                  ADD  R0  1
                 STOR  R0  [GB+DigNr]    ; DigNr := DigNr + 1
                 LOAD  R0  [GB+DigSel]
                 STOR  R0  [R5+DSPDIG]   ; activate next display element
                  ADD  R0  R0
                 STOR  R0  [GB+DigSel]   ; DigSel := DigSel * 2
                  RTS
   UpdateDig5 :  LOAD  R1  [GB+Digits+5] ; special case: digit 5
                 STOR  R1  [R5+DSPSEG]   ; display the pattern on the next digit
                 LOAD  R0  0
                 STOR  R0  [GB+DigNr]    ; DigNr := 0
                 LOAD  R0  [GB+DigSel]
                 STOR  R0  [R5+DSPDIG]   ; activate next display element
                 LOAD  R0  1
                 STOR  R0  [GB+DigSel]   ; DigSel := 1
    UpdateEnd :   RTS
;
;      Routine Hex2Seg7 maps a number in the range [0..15] to its hexadecimal
;      representation pattern for the 7-segment display. Values exceeding 15 are
;      displayed as "blank".
;      R0 : upon entry, contains the number
;      R1 : upon exit,  contains the resulting pattern
;
 Hex2Seg7     :   BRS  Hex2Seg7_bgn ; push address(tbl) onto stack and proceed at "bgn"
 Hex2Seg7_tbl :  CONS  %01111110    ;  7-segment pattern for '0'
                 CONS  %00110000    ;  7-segment pattern for '1'
                 CONS  %01101101    ;  7-segment pattern for '2'
                 CONS  %01111001    ;  7-segment pattern for '3'
                 CONS  %00110011    ;  7-segment pattern for '4'
                 CONS  %01011011    ;  7-segment pattern for '5'
                 CONS  %01011111    ;  7-segment pattern for '6'
                 CONS  %01110000    ;  7-segment pattern for '7'
                 CONS  %01111111    ;  7-segment pattern for '8'
                 CONS  %01111011    ;  7-segment pattern for '9'
                 CONS  %01110111    ;  7-segment pattern for 'A'
                 CONS  %00011111    ;  7-segment pattern for 'b'
                 CONS  %01001110    ;  7-segment pattern for 'C'
                 CONS  %00111101    ;  7-segment pattern for 'd'
                 CONS  %01001111    ;  7-segment pattern for 'E'
                 CONS  %01000111    ;  7-segment pattern for 'F'
                 CONS  %00000000    ;  7-segment pattern for 'blank'
 Hex2Seg7_bgn :   CMP  R0  16       ; IF R0 < 16
                  BCS  Hex2Seg7_fi  ; THEN skip
                 LOAD  R0  16       ; ELSE R0 := 16
 Hex2Seg7_fi  :  LOAD  R1  [SP++]   ; R1 := address(tbl) (pulled from stack)
                 LOAD  R1  [R1+R0]  ; R1 := tbl[R0]
                  RTS
;
;  That's all folks!
;
@END
