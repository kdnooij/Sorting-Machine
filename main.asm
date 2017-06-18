@DATA
BlackCount     DS     1
WhiteCount     DS     1
    State      DS     1
  Display      DS     16
DispLength     DS     1
   DispDgt     DS     1
   DispPos     DS     1
 DispDelay0    DS     1
 DispDelay     DS     1
  PosDelay0    DS     1
  PosDelay     DS     1
  PwmPhase     DS     1
  PrevInput    DS     1
  ConvDelay0   DS     1
  ConvDelay    DS     1
  Conveyer     DS     1
  Output       DS     1
  Detected     DS     1
  Timeout0     DS     1
  Timeout      DS     1
  ErrorState   DS     2
  Error        DS     1


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
    W_VAL      EQU    95
    WHITE      EQU    2
    BLACK      EQU    1
    EMPTY      EQU    0

; STATES
   NORMAL      EQU    0
   W_SORT      EQU    1
   B_SORT      EQU    2
   STARTING    EQU    3

; TIMING
SORT_TIME      EQU    10000

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
              CONS  %01110111    ;  7-segment pattern for 'A' 10 
              CONS  %00011111    ;  7-segment pattern for 'b' 11
              CONS  %01001110    ;  7-segment pattern for 'C' 12
              CONS  %00111101    ;  7-segment pattern for 'd' 13
              CONS  %01001111    ;  7-segment pattern for 'E' 14
              CONS  %01000111    ;  7-segment pattern for 'F' 15
              CONS  %01011110    ;  7-segment pattern for 'G' 16
              CONS  %00110111    ;  7-segment pattern for 'H' 17
              CONS  %00000110    ;  7-segment pattern for 'I' 18
              CONS  %00111000    ;  7-segment pattern for 'J' 19
              CONS  %00110111    ;  7-segment pattern for 'K' 20
              CONS  %00001110    ;  7-segment pattern for 'L' 21
              CONS  %01110110    ;  7-segment pattern for 'M' 22
              CONS  %00011001    ;  7-segment pattern for 'N' 23
              CONS  %01111110    ;  7-segment pattern for 'O' 24
              CONS  %01100111    ;  7-segment pattern for 'P' 25
              CONS  %01110011    ;  7-segment pattern for 'Q' 26
              CONS  %01110111    ;  7-segment pattern for 'R' 27
              CONS  %01011011    ;  7-segment pattern for 'S' 28
              CONS  %00001111    ;  7-segment pattern for 'T' 29
              CONS  %00011100    ;  7-segment pattern for 'U' 30 
              CONS  %00111110    ;  7-segment pattern for 'V' 31
              CONS  %00111110    ;  7-segment pattern for 'W' 32
              CONS  %00110111    ;  7-segment pattern for 'X' 33
              CONS  %00111011    ;  7-segment pattern for 'Y' 34
              CONS  %01101101    ;  7-segment pattern for 'Z' 35
              CONS  %00000000    ;  7-segment pattern for ' ' 36 
              CONS  %10000000    ;  7-segment pattern for '.' 37
Hex7Seg_bgn:   MOD  R0  %0100110 ;  R0 := R0 MOD 38 , just to be safe...
              LOAD  R1  [SP++]   ;  R1 := address(tbl) (retrieve from stack)
              LOAD  R1  [R1+R0]  ;  R1 := tbl[R0]
               RTS
;
;      The body of the main program
;
      main:   LOAD  R5  IOAREA 
              LOAD  R0  STARTING    ;  Load state in R0
              STOR  R0  [GB+State]  ;  Store state in RAM
              LOAD  R0  25          ;  P
               BRS  Hex7Seg
              STOR  R1  [GB+Display+0]
              LOAD  R0  27          ;  R
               BRS  Hex7Seg
              STOR  R1  [GB+Display+1]
              LOAD  R0  14          ;  E
               BRS  Hex7Seg
              STOR  R1  [GB+Display+2]
              LOAD  R0  28          ;  S
               BRS  Hex7Seg
              STOR  R1  [GB+Display+3]
              LOAD  R0  28          ;  S
               BRS  Hex7Seg
              STOR  R1  [GB+Display+4]
              LOAD  R0  36          ;   
               BRS  Hex7Seg
              STOR  R1  [GB+Display+5]
              LOAD  R0  28          ;  S
               BRS  Hex7Seg
              STOR  R1  [GB+Display+6]
              LOAD  R0  29          ;  T
               BRS  Hex7Seg
              STOR  R1  [GB+Display+7]
              LOAD  R0  10          ;  A
               BRS  Hex7Seg
              STOR  R1  [GB+Display+8]
              LOAD  R0  27          ;  R
               BRS  Hex7Seg
              STOR  R1  [GB+Display+9]
              LOAD  R0  29          ;  T
               BRS  Hex7Seg
              STOR  R1  [GB+Display+10]
              LOAD  R0  36          ;  0
               BRS  Hex7Seg
              STOR  R1  [GB+Display+11]

              LOAD  R0  12
              STOR  R0  [GB+DispLength]

              LOAD  R0  0
              STOR  R0  [GB+DispDgt]
              STOR  R0  [GB+DispPos]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+DispDelay0]
               SUB  R0  100
              STOR  R0  [GB+DispDelay]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+PosDelay0]
               SUB  R0  10000
              STOR  R0  [GB+PosDelay]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+ConvDelay0]
               SUB  R0  10000
              STOR  R0  [GB+ConvDelay]
              LOAD  R0  %01
              STOR  R0  [GB+Conveyer]

              LOAD  R0  0
              STOR  R0  [GB+Detected]

              STOR  R0  [GB+WhiteCount]
              STOR  R0  [GB+BlackCount]
;
;  main loop of program
;
      loop:    BRS  display
               BRS  check_buttons   ;  check if buttons are pressed
              LOAD  R4  R1
               AND  R4  %010000000            
               CMP  R4  %010000000  ;  if button 0 is pressed
               BEQ  power_off       ;  stop the program

              LOAD  R0  [GB+State]
               CMP  R0  STARTING
               BEQ  starting

               BRS  check_input
              LOAD  R2  [GB+Detected]
               CMP  R2  EMPTY
               BNE  change_det
              LOAD  R4  R1          ;  the color of the puck is stored in R4
               CMP  R4  EMPTY       ;  if empty
               BEQ  exec_state      ;  change state accordingly
  check_w:     CMP  R4  WHITE       ;  if white
               BNE  check_b         ;  change state accordingly
              LOAD  R2  2
              STOR  R2  [GB+Detected]
               BRA  exec_state
  check_b:     CMP  R4  BLACK       ;  if black
               BNE  detection_err   ;  ERROR
              LOAD  R2  1
              STOR  R2  [GB+Detected]
               BRA  exec_state

change_det:   LOAD  R4  R1          ;  the color of the puck is stored in R4
              STOR  R4  [GB+Detected]
               CMP  R4  EMPTY       ;  if empty
               BNE  exec_state
               CMP  R2  WHITE       ;  if white
               BEQ  w_detected      ;  change state accordingly
               CMP  R2  BLACK       ;  if black
               BEQ  b_detected



  starting:    BRS  display
               BRS  check_buttons
               AND  R1  %01000
               CMP  R1  %01000
               BEQ  normal_state
               BRA  loop

w_detected:   LOAD  R0  W_SORT
              STOR  R0  [GB+State]

              LOAD  R0  32          ;  W
               BRS  Hex7Seg
              STOR  R1  [GB+Display+0]
              LOAD  R0  17          ;  H
               BRS  Hex7Seg
              STOR  R1  [GB+Display+1]
              LOAD  R0  18          ;  I
               BRS  Hex7Seg
              STOR  R1  [GB+Display+2]
              LOAD  R0  29          ;  T
               BRS  Hex7Seg
              STOR  R1  [GB+Display+3]
              LOAD  R0  14          ;  E
               BRS  Hex7Seg
              STOR  R1  [GB+Display+4]
              LOAD  R0  [GB+WhiteCount]
               BRS  Hex7Seg
              STOR  R1  [GB+Display+5]

              LOAD  R0  7
              STOR  R0  [GB+DispLength]

              LOAD  R0  0
              STOR  R0  [GB+DispDgt]
              STOR  R0  [GB+DispPos]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+DispDelay0]
               SUB  R0  100
              STOR  R0  [GB+DispDelay]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+PosDelay0]
               SUB  R0  10000
              STOR  R0  [GB+PosDelay]

              LOAD  R0  %0100010
              STOR  R0  [GB+Output]

              LOAD  R0  EMPTY
              STOR  R0  [GB+Detected]

              LOAD R0 [R5+TIMER]
              STOR R0 [GB+Timeout0]
              SUB R0 30000                  ;This is the time till it goes to emergency
              STOR R0 [GB+Timeout]

               BRA  exec_state
              
b_detected:   LOAD  R0  B_SORT
              STOR  R0  [GB+State]

              LOAD  R0  11          ;  B
               BRS  Hex7Seg
              STOR  R1  [GB+Display+0]
              LOAD  R0  21          ;  L
               BRS  Hex7Seg
              STOR  R1  [GB+Display+1]
              LOAD  R0  10          ;  A
               BRS  Hex7Seg
              STOR  R1  [GB+Display+2]
              LOAD  R0  12          ;  C
               BRS  Hex7Seg
              STOR  R1  [GB+Display+3]
              LOAD  R0  20          ;  K
               BRS  Hex7Seg
              STOR  R1  [GB+Display+4]
              LOAD  R0  [GB+BlackCount]
               BRS  Hex7Seg
              STOR  R1  [GB+Display+5]

              LOAD  R0  7
              STOR  R0  [GB+DispLength]

              LOAD  R0  0
              STOR  R0  [GB+DispDgt]
              STOR  R0  [GB+DispPos]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+DispDelay0]
               SUB  R0  100
              STOR  R0  [GB+DispDelay]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+PosDelay0]
               SUB  R0  10000
              STOR  R0  [GB+PosDelay]

              LOAD  R0  %010001
              STOR  R0  [GB+Output]

              LOAD  R0  EMPTY
              STOR  R0  [GB+Detected]

              LOAD R0 [R5+TIMER]
              STOR  R0  [GB+Timeout0]
              SUB R0 30000                  ;This is the time till it goes to emergency
              STOR R0 [GB+Timeout]

               BRA  exec_state

exec_state:   LOAD  R0  [GB+State]
               CMP  R0  NORMAL
               BEQ  e_sort
               CMP  R0  W_SORT
               BEQ  w_sort
               CMP  R0  B_SORT
               BEQ  b_sort

    w_sort:   LOAD  R1  [GB+Timeout0]
              LOAD  R2  [GB+Timeout]
               BRS  check_timer
               CMP  R0  0
               BNE  Err_23_5

              LOAD  R0  [R5+INPUT]
              LOAD  R1  [GB+PrevInput]
              STOR  R0  [GB+PrevInput]
               AND  R0  %0100
               AND  R1  %0100
               CMP  R0  %0000
               BNE  w_sort_end
               CMP  R1  %0100
               BNE  w_sort_end

              LOAD  R0  [GB+WhiteCount]
               ADD  R0  1
              STOR  R0  [GB+WhiteCount]

              LOAD  R0  EMPTY
              STOR  R0  [GB+State]

              LOAD  R0  [GB+Output]
               AND  R0  %1011101
              STOR  R0  [GB+Output]

 w_sort_end:   BRA  e_sort
    
    b_sort:   LOAD  R1  [GB+Timeout0]
              LOAD  R2  [GB+Timeout]
               BRS  check_timer
               CMP  R0  0
               BNE  Err_23_5

              LOAD  R0  [R5+INPUT]
              LOAD  R1  [GB+PrevInput]
              STOR  R0  [GB+PrevInput]
               AND  R0  %0001
               AND  R1  %0001
               CMP  R0  %0000
               BNE  b_sort_end
               CMP  R1  %0001
               BNE  b_sort_end

              LOAD  R0  [GB+BlackCount]
               ADD  R0  1
              STOR  R0  [GB+BlackCount]

              LOAD  R0  EMPTY
              STOR  R0  [GB+State]

              LOAD  R0  [GB+Output]
               AND  R0  %1101110
              STOR  R0  [GB+Output]

 b_sort_end:   BRA  e_sort

normal_state: LOAD  R0  NORMAL
              STOR  R0  [GB+State]

              LOAD  R0  [GB+Output]
              LOAD  R2  %0000100
                OR  R0  R2
              STOR  R0  [GB+Output]

              LOAD  R0  14          ;  E
               BRS  Hex7Seg
              STOR  R1  [GB+Display+0]
              LOAD  R0  22          ;  M
               BRS  Hex7Seg
              STOR  R1  [GB+Display+1]
              LOAD  R0  25          ;  P
               BRS  Hex7Seg
              STOR  R1  [GB+Display+2]
              LOAD  R0  29          ;  T
               BRS  Hex7Seg
              STOR  R1  [GB+Display+3]
              LOAD  R0  34          ;  Y
               BRS  Hex7Seg
              STOR  R1  [GB+Display+4]

              LOAD  R0  7
              STOR  R0  [GB+DispLength]

              LOAD  R0  0
              STOR  R0  [GB+DispDgt]
              STOR  R0  [GB+DispPos]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+DispDelay0]
               SUB  R0  100
              STOR  R0  [GB+DispDelay]

              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+PosDelay0]
               SUB  R0  10000
              STOR  R0  [GB+PosDelay]

               BRA  e_sort

    e_sort:   LOAD  R1  [GB+ConvDelay0]
              LOAD  R2  [GB+ConvDelay]
               BRS  check_timer
               CMP  R0  0
               BEQ  e_cont

    e_switch: LOAD  R0  [GB+Conveyer]
               CMP  R0  %01
               BEQ  e_sort_off
              LOAD  R0  %01
              STOR  R0  [GB+Conveyer]
              LOAD  R3  [R5+TIMER]
              STOR  R3  [GB+ConvDelay0]
               SUB  R3  500
              STOR  R3  [GB+ConvDelay]
              LOAD  R1  [GB+Output]
              LOAD  R0  %0000100
                OR  R1  R0
              STOR  R1  [GB+Output]
               BRA  e_cont

e_sort_off:   LOAD  R0  %00
              STOR  R0  [GB+Conveyer]
              LOAD  R3  [R5+TIMER]
              STOR  R3  [GB+ConvDelay]
               SUB  R3  5000
              STOR  R3  [GB+ConvDelay]
              LOAD  R1  [GB+Output]
              LOAD  R0  %1111011
               AND  R1  R0
              STOR  R1  [GB+Output]

e_cont:        BRS  pwm

               BRS  display
               BRA  exec_end

  exec_end:    BRA  loop
               


;
;  ERRORS
;


Err_23_5:           LOAD  R0  2
                    STOR  R0  [GB+ErrorState]
                    LOAD  R0  3
                    STOR  R0  [GB+ErrorState+1]
                    LOAD  R0  5
                    STOR  R0  [GB+Error]
                    BRA  Error

Err_21_1:           LOAD  R0  2
                    STOR  R0  [GB+ErrorState]
                    LOAD  R0  1
                    STOR  R0  [GB+ErrorState+1]
                    LOAD  R0  1
                    STOR  R0  [GB+Error]
                    BRA  Error

Err_22_1:           LOAD  R0  2
                    STOR  R0  [GB+ErrorState]
                    LOAD  R0  2
                    STOR  R0  [GB+ErrorState+1]
                    LOAD  R0  1
                    STOR  R0  [GB+Error]
                    BRA  Error

Err_22_2:           LOAD  R0  2
                    STOR  R0  [GB+ErrorState]
                    LOAD  R0  2
                    STOR  R0  [GB+ErrorState+1]
                    LOAD  R0  2
                    STOR  R0  [GB+Error]
                    BRA  Error

Error:              LOAD R0 [GB+ErrorState]  ;Screen display E
                    BRS Hex7Seg
                    STOR R1 [GB+Display+0]
                    LOAD R0 [GB+ErrorState+1]  ;               R
                    BRS Hex7Seg
                    STOR R1 [GB+Display+1]
                    LOAD R0 [GB+BlackCount]    ; 
                    BRS Hex7Seg ;               R
                    STOR R1 [GB+Display+2]
                    LOAD R0 [GB+WhiteCount]            ;O
                    BRS Hex7Seg
                    STOR R1 [GB+Display+3]      ;R
                    LOAD R0 36  ;               R
                    BRS Hex7Seg
                    STOR R1 [GB+Display+4]
                    LOAD R0 [GB+Error]  ;               R
                    BRS Hex7Seg
                    STOR R1 [GB+Display+5]

                    LOAD  R0  7
                    STOR  R0  [GB+DispLength]

                    LOAD  R0  0
                    STOR  R0  [GB+DispDgt]
                    STOR  R0  [GB+DispPos]

                    LOAD R1 0
                    STOR R1 [GB+Output]
                    STOR R1 [R5+OUTPUT]
                    ;LOAD R2 NORMAL               ;Give state empty
                    ;STOR R2 [GB+State]
                    BRA err_loop

      err_loop:     BRS check_buttons
                    AND R1 %01000
                    CMP R1 %01000
                    BEQ reset
                    BRS display
                    BRA err_loop

detection_err: BRA  power_off


;
;  DISPLAY FUNCTION
;

display:      LOAD  R0  [GB+DispPos]
              LOAD  R1  [GB+DispDgt]
               ADD  R0  R1
               MOD  R0  [GB+DispLength]
               ADD  R0  Display

              LOAD  R0  [GB+R0]
              STOR  R0  [R5+DSPSEG]
              
              LOAD  R2  %0100000
               CMP  R1  0
               BEQ  stor_disp
              LOAD  R2  %010000
               CMP  R1  1
               BEQ  stor_disp
              LOAD  R2  %01000
               CMP  R1  2
               BEQ  stor_disp
              LOAD  R2  %0100
               CMP  R1  3
               BEQ  stor_disp
              LOAD  R2  %010
               CMP  R1  4
               BEQ  stor_disp
              LOAD  R2  %01
               CMP  R1  5
               BEQ  stor_disp
              
stor_disp:    STOR  R2  [R5+DSPDIG]

              LOAD  R1  [GB+DispDelay0]
              LOAD  R2  [GB+DispDelay]  
               BRS  check_timer
               CMP  R0  0
               BEQ  disp_pos

               ADD  R1  1
               MOD  R1  6
              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+DispDelay0]
               SUB  R0  10
              STOR  R0  [GB+DispDelay]

              STOR  R1  [GB+DispDgt]

disp_pos:     LOAD  R0  [GB+DispLength]
               CMP  R0  7
               BLS  disp_end

              LOAD  R1  [GB+PosDelay0]
              LOAD  R2  [GB+PosDelay]
               BRS  check_timer
               CMP  R0  0
               BEQ  disp_end
               
              LOAD  R1  [GB+DispPos]
               ADD  R1  1
               MOD  R1  [GB+DispLength]
              LOAD  R0  [R5+TIMER]
              STOR  R0  [GB+PosDelay0]
               SUB  R0  10000
              STOR  R0  [GB+PosDelay]

              STOR  R1  [GB+DispPos]
              
disp_end:      RTS


reset:        LOAD  R0  0
              STOR  R0  [GB+BlackCount]
              STOR  R0  [GB+WhiteCount]
              STOR  R0  [GB+State]
              STOR  R0  [GB+DispLength]
              STOR  R0  [GB+DispDgt]
              STOR  R0  [GB+DispPos]
              STOR  R0  [GB+DispDelay]
              STOR  R0  [GB+PosDelay]
              STOR  R0  [GB+PwmPhase]
              STOR  R0  [GB+PrevInput]
              STOR  R0  [GB+ConvDelay]
              STOR  R0  [GB+Conveyer]
              STOR  R0  [GB+Output]
              STOR  R0  [GB+Detected]
              STOR  R0  [GB+Timeout]

               BRA  main
;
;  check_input: checks the color of the puck (0=NONE, 1=BLACK, 2=WHITE) using B_VAL and W_VAL as thresholds
;  RETURN
;           R1: puck color
;

check_input:  
              LOAD  R0  [R5+INPUT]
               AND  R0  %010
               CMP  R0  2
               BEQ  e_input
              LOAD  R1  [R5+ADCONVS]  ;  read from analogue inputs
               DIV  R1  255           ;  shift input 8 bits to the right to select second analogue input
               CMP  R1  W_VAL         ;  compare input level to the black value
               BHI  w_input           ;  if it's smaller a black disk is detected
               BRA  b_input           ;  if it's smaller a white disk is detected
      e_input:LOAD  R1  EMPTY         ;  if the input level is higher than the white one, nothing is detected
               BRA  check_input_end   ;  go to the end of the function
    b_input:  ;LOAD  R2  [R5+TIMER]
              ;LOAD  R3  %0000
              ;STOR  R3  [R5+OUTPUT]
               ;SUB  R2  2000
     ;b_back:   CMP  R2  [R5+TIMER]
               ;BLS  b_back
              LOAD  R1  BLACK         ;  the disk is black, so the function returns 1
               BRA  check_input_end   ;  go to the end of the function
    w_input:  ;LOAD  R2  [R5+TIMER]
              ;LOAD  R3  %0000
              ;STOR  R3  [R5+OUTPUT]
               ;SUB  R2  2000
     ;w_back:   CMP  R2  [R5+TIMER]
     ;          BLS  w_back
              LOAD  R1  WHITE         ;  the disk is white, so the function returns 2
check_input_end:
               RTS                    ;  return


;
; CHECK TIMER FUNCTION
; R1: initial timer (x)
; R2: (x - deltaT)
;
; RETURN=  R0: 1 (fire event) OR 2 (continue)

check_timer:  LOAD  R0  R1
               CMP  R0  0
               BGE  t_else
              LOAD  R0  R2
               CMP  R0  0
               BLE  t_else
               CMP  R0  [R5+TIMER]
               BLE  t_cont
              LOAD  R0  0
               CMP  R0  [R5+TIMER]
               BGE  t_cont
               BRA  t_fire

    t_else:   LOAD  R0  R2
               CMP  R0  [R5+TIMER]
               BLE  t_cont
    
     t_fire:   LOAD  R0  1
               RTS

    t_cont:   LOAD  R0  0
               RTS




check_buttons:
              LOAD  R1  [R5+INPUT]
               RTS

        pwm:  LOAD  R1  [GB+PwmPhase]
               CMP  R1  0
               BEQ  pwm_off
              LOAD  R2  [GB+Output]
               BRA  pwm_stor
    pwm_off:  LOAD  R2  0
   pwm_stor:   ADD  R1  1
               MOD  R1  3
              STOR  R1  [GB+PwmPhase]
              STOR  R2  [R5+OUTPUT]
               RTS

  power_off:  LOAD  R1  0
              STOR  R1  [R5+OUTPUT]   ;  turn off motors
@END
