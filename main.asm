;
;      2011.10.11:  author:  Rob Hoogerwoord
;
;      2013.10.29:  removed errors in the annotation [RH]
;
;      This routine continuously reads the intput buttons and copies them to the
;      LED outputs. In addition, if Button 0 is pressed this increases a modulo
;      16 counter which is displayed at the right-most digit of the display,
;      and, similarly, if Button 1 is pressed this increases a modulo 16 counter
;      which is displayed at the second right-most digit of the display.
;
;
@DATA
BlackCount     DS     1
WhiteCount     DS     1
SortDelay      DS     1
    State      DS     1

@CODE

   IOAREA      EQU  -16  ;  address of the I/O-Area, modulo 2^18
    INPUT      EQU    7  ;  position of the input buttons (relative to IOAREA)
  ADCONVS      EQU    6  ;  position of the analogue inputs
   OUTPUT      EQU   11  ;  relative position of the power outputs
   DSPDIG      EQU    9  ;  relative position of the 7-segment display's digit selector
   DSPSEG      EQU    8  ;  relative position of the 7-segment display's segments
    TIMER      EQU    13 ;  relative position of the timer
    
; COLOR DETECTION
    B_VAL      EQU    10
    W_VAL      EQU    25
    WHITE      EQU    2
    BLACK      EQU    1
    EMPTY      EQU    0

; STATES
   NORMAL      EQU    0
   W_SORT      EQU    1
   B_SORT      EQU    2

; TIMING
SORT_TIME      EQU    20000

  begin :      BRA  main         ;  skip subroutine Hex7Seg
;  
;      Routine Hex7Seg maps a number in the range [0..15] to its hexadecimal
;      representation pattern for the 7-segment display.
;      R0 : upon entry, contains the number
;      R1 : upon exit,  contains the resulting pattern
;
Hex7Seg     :  BRS  Hex7Seg_bgn  ;  push address(tbl) onto stack and proceed at "bgn"
Hex7Seg_tbl : CONS  %01111110    ;  7-segment pattern for '0'
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
Hex7Seg_bgn:   AND  R0  %01111   ;  R0 := R0 MOD 16 , just to be safe...
              LOAD  R1  [SP++]   ;  R1 := address(tbl) (retrieve from stack)
              LOAD  R1  [R1+R0]  ;  R1 := tbl[R0]
               RTS
;
;      The body of the main program
;
      main:   LOAD  R5  IOAREA 
              LOAD  R0  NORMAL      ;  Load state to 0 in R0
              STOR  R0  [GB+State]  ;  Store state in RAM
              

;
;  main loop of program
;
      loop:    BRS  check_buttons   ;  check if buttons are pressed
              LOAD  R4  R1            
               CMP  R4  1           ;  if button 0 is pressed
               BEQ  power_off       ;  stop the program

               BRS  check_input
              LOAD  R4  R1          ;  the color of the puck is stored in R4
               CMP  R4  EMPTY       ;  if empty
               BEQ  exec_state      ;  change state accordingly
               CMP  R4  WHITE       ;  if white
               BEQ  w_detected      ;  change state accordingly
               CMP  R4  BLACK       ;  if black
               BEQ  b_detected      ;  change state accordingly
               BRA  detection_err   ;  ERROR

w_detected:   LOAD  R0  W_SORT
              STOR  R0  [GB+State]
              LOAD  R0  [R5+TIMER]
               SUB  R0  SORT_TIME
              STOR  R0  [GB+SortDelay]
               BRA  exec_state
              
b_detected:   LOAD  R0  B_SORT
              STOR  R0  [GB+State]
              LOAD  R0  [R5+TIMER]
               SUB  R0  SORT_TIME
              STOR  R0  [GB+SortDelay]
               BRA  exec_state

exec_state:   LOAD  R0  [GB+State]
               CMP  R0  NORMAL
               BEQ  e_sort
               CMP  R0  W_SORT
               BEQ  w_sort
               CMP  R0  B_SORT
               BEQ  b_sort

    w_sort:   LOAD  R0  [GB+SortDelay]
               CMP  R0  [R5+TIMER]
               BHI  normal_state
              LOAD  R0  %0001
              STOR  R0  [R5+OUTPUT]
               BRA  exec_end
    
    b_sort:   LOAD  R0  [GB+SortDelay]
               CMP  R0  [R5+TIMER]
               BHI  normal_state
              LOAD  R0 %0010
              STOR  R0  [R5+OUTPUT]
               BRA  exec_end

normal_state: LOAD  R0  NORMAL
              STOR  R0  [GB+State]
               BRA  e_sort

    e_sort:   LOAD  R0  %0100
              STOR  R0  [R5+OUTPUT]
               BRA  exec_end

  exec_end:    BRA  loop
               


;
;  ERRORS
;

detection_err: BRA  power_off

;
;  check_input: checks the color of the puck (0=NONE, 1=BLACK, 2=WHITE) using B_VAL and W_VAL as thresholds
;      @return: R1 - puck color
;

check_input:  
              LOAD  R1  [R5+ADCONVS]  ;  read from analogue inputs
               DIV  R1  255           ;  shift input 8 bits to the right to select second analogue input
               CMP  R1  W_VAL         ;  compare input level to the black value
               BHI  w_input           ;  if it's smaller a black disk is detected
              LOAD  R0  [R5+INPUT]
               CMP  R0  2         ;  compare input level to the white value
               BNE  b_input           ;  if it's smaller a white disk is detected
      e_input:LOAD  R1  EMPTY         ;  if the input level is higher than the white one, nothing is detected
               BRA  check_input_end   ;  go to the end of the function
    b_input:  LOAD  R1  BLACK         ;  the disk is black, so the function returns 1
               BRA  check_input_end   ;  go to the end of the function
    w_input:  LOAD  R0  [R5+INPUT]
               CMP  R0  2
               BEQ  e_input
              LOAD  R1  WHITE         ;  the disk is white, so the function returns 2
check_input_end:
               RTS                    ;  return

check_buttons:
              LOAD  R1  [R5+INPUT]
               RTS

  power_off:  LOAD  R1  0
              STOR  R1  [R5+OUTPUT]   ;  turn off motors

@END
