;
; nxtNoughtsAndCrosses - 2 player networked game for ZX Spectrum Next
;   uses Next Mailbox Protocol 0.1
;
;

; request:
; protocol maj=0 min=1
; 26 chars is min len of valid request
;
; pos:   |  0        | 2    |  3   |  4     | 25           | 46      |
; size:  |  2        | 1    |  1   |  20    | 20           | 255     |
; field: |  protocol | cmd  |  app | userid | param1:      | message |
;        |           |      |      |        | nickname / * |         |
;        |           |      |      |        | or msgid     |         |
;        |           |      |      |        | or poolid    |         |
;

                        ;   zeusemulate "48K"
                        zeusemulate "Next", "RAW"       ; RAW prevents Zeus from adding some BASIC emulator-friendly
                        zoLogicOperatorsHighPri = false ; data like the stack and system variables. Not needed because
                        zoSupportStringEscapes = true   ; this only runs on the Next, and everything is already present.
                        zxAllowFloatingLabels = false   ;

; NextZXOS APIs
IDE_MODE                equ $01d5                       ; used to set the characters per line

; Spectrum ROM routines
ROM_KEYTABLE            equ $0205                       ; convert from keycode to ascii
ROM_KEY_SCAN            equ $028e                       ;
ROM_CHAN_OPEN           equ $1601                       ; to allow us to print to the upper screen
ROM_PR_STRING           equ $203c                       ;

; Next registers
NXREG_TURBO_CTL         equ $07                         ; set CPU speed
CPU_28                  equ 11b                         ; 11b = 28MHz

; Next Mailbox Protocol
MBOX_CMD_REGISTER       equ 1                           ;
MBOX_CMD_CHECK_REGISTERED_NICKNAME equ 2                ;
MBOX_CMD_SEND_MESSAGE   equ 3                           ;
MBOX_CMD_MESSAGE_COUNT  equ 4                           ;
MBOX_CMD_GET_MESSAGE    equ 5                           ;
MBOX_CMD_JOIN_POOL      equ 6                           ;
MBOX_CMD_GET_POOL       equ 7                           ;

MBOX_STATUS_OK          equ 0                           ;
MBOX_STATUS_INVALID_PROTOCOL equ 1                      ;
MBOX_STATUS_INVALID_CMD equ 2                           ;
MBOX_STATUS_INVALID_APP equ 3                           ;
MBOX_STATUS_INVALID_USERID equ 4                        ;
MBOX_STATUS_INVALID_LENGTH equ 5                        ;
MBOX_STATUS_INTERNAL_ERROR equ 6                        ;
MBOX_STATUS_MISSING_NICKNAME equ 7                      ;
MBOX_STATUS_MISSING_MESSAGE equ 8                       ;
MBOX_STATUS_UNIMPLEMENTED equ 9                         ;
MBOX_STATUS_MISSING_MESSAGE_ID equ 10                   ;
MBOX_STATUS_MISSING_POOL_SIZE equ 11                    ;
MBOX_STATUS_MISSING_POOL_ID equ 12                      ;

MBOX_STATUS_USER_ALREADY_REGISTERED equ 101             ;
MBOX_STATUS_UNREGISTERED_NICKNAME equ 102               ;
MBOX_STATUS_UNKNOWN_USERID equ 103                      ;
MBOX_STATUS_UNREGISTERED_USERID equ 104                 ;
MBOX_STATUS_UNKNOWN_POOL_ID equ 105                     ;

MBOX_STATUS_REGISTER_OK equ 51                          ;
MBOX_STATUS_COUNT_OK    equ 52                          ;
MBOX_STATUS_GET_MESSAGE_OK equ 53                       ;
MBOX_STATUS_INVALID_MESSAGE_ID equ 54                   ;
MBOX_STATUS_JOINED_POOL equ 55                          ;
MBOX_STATUS_INVALID_POOLSIZE equ 56                     ;
MBOX_STATUS_ALREADY_JOINED_POOL equ 57                  ;
MBOX_STATUS_POOL_UNFILLED equ 58                        ;
MBOX_STATUS_POOL_FILLED equ 59                          ;


org                     $8000                           ; This should keep our code clear of NextBASIC sysvars
                        ;                                 (Necessary for making NextZXOS API calls);

;    connect
;    get msg count  (todo reg load/save)
;    if not reg
;      reg
;      join pool
;    else if no msgs (because unfilled)
;      get pool (ie return the pool we're in)
;      if unfilled
;        show waiting for player
;    else if msg shows game finished
;      show finished
;      ask to restart/join pool
;    else if not finished
;      draw state
;      if our move
;        get move
;        send move
;      else
;        show waiting for player
;

;
; main loop
;
Main                    proc                            ;
                        di                              ;
                        nextreg NXREG_TURBO_CTL, CPU_28 ; Next Turbo Control Register to set cpu speed
                        call MakeCIPStart               ; setup comms to server
                        call SetupScreen                ;

Loop                    call HandleCount                ; count messages, setting carry if unregistered
                        jp c, NotRegistered             ; if carry set, we're unregistered
                        ld a, (MSG_COUNT)               ;
                        cp 0                            ;
                        jp z, NoMessages                ;
                        call ProcessLatestMessage       ;
                        ld a, (IS_GAME_FINISHED)        ;
                        jp z, Finished                  ;
                        call ShowState                  ;
                        ld a, (IS_OUR_TURN)             ;
                        cp 1                            ;
                        jp nz, ShowWaitingForMove       ;
                        call GetMove                    ;
                        call SendMove                   ;
                        call ShowState                  ;
                        call CheckWin                   ;
                        jp Loop                         ;

NotRegistered           call HandleRegister             ;
                        jp Loop                         ;

NoMessages              call JoinPool                   ; returns poolid with every call
                        call GetPool                    ; returns unfilled or nick + orders
                        PrintLine(0,8,MSG_WAITING_FOR_POOL, MSG_WAITING_FOR_POOL_LEN);
                        jp Loop                         ;

Finished                call ShowFinished               ;
                        call ShowState                  ;
                        call JoinPool                   ;
                        call GetPool                    ;
                        call PressKeyToContinue         ;
                        jp Loop                         ;

ShowWaitingForMove      PrintLine(0,8,MSG_WAITING_FOR_MOVE, MSG_WAITING_FOR_MOVE_LEN);
                        call PressKeyToContinue         ;
                        jp Loop                         ;
pend

;
;
;
ProcessLatestMessage    call GetLatestMessage           ;
                        call ProcessMessage             ;
                        ret                             ;

;
;
;
GetLatestMessage        ld hl, (MSG_COUNT)              ;
                        ld (MBOX_MSG_ID), hl            ;
                        call HandleGetMessage           ;
                        ret                             ;

;
;
;
ShowFinished            PrintLine(0,10,MSG_FINISHED, MSG_FINISHED_LEN);
                        ld a, (WE_WON)                  ;
                        cp 1                            ;
                        jp nz,TheyWon                   ;
                        PrintLine(0,11,MSG_YOU_WON, MSG_YOU_WON_LEN);
                        ret                             ;
TheyWon                 PrintLine(0,11,MSG_YOU_LOST, MSG_YOU_LOST_LEN);
                        ret                             ;

;
;
;
ShowState               proc                            ;
                        ld b, 0                         ;
                        ld c, 8                         ;
                        ld a, 'o'                       ;
                        ld ix, MOVE_HISTORY-1           ;
Loop                    push af                         ;
                        ld de, BOARD                    ;
                        ld hl, MOVE_TABLE               ;
                        inc ix                          ;
                        ld a, (ix)                      ; grid value for movenum
                        cp 0                            ;
                        ret z                           ; no more moves to process
                        push bc                         ;
                        ld c, a                         ;
                        add hl, bc                      ; add a to move_table to get board offset
                        ld a, (hl)                      ; a is board offset
                        ld c, a                         ;
                        push hl                         ;
                        push de                         ;
                        pop hl                          ;
                        pop de                          ;
                        add hl, bc                      ; update board inline based on offset
                        pop bc                          ;
                        pop af                          ;
                        ld (hl), a                      ;
                        push hl                         ;
                        push de                         ;
                        pop hl                          ;
                        pop de                          ;
                        call FlipToken                  ; change player's token
                        dec c                           ;
                        jp nz, Loop                     ;
                        PrintLine(0,15,BOARD, 7)        ;
                        PrintLine(0,16,BOARD+8, 7)      ;
                        PrintLine(0,17,BOARD+15,7)      ;
                        ret                             ;
pend

;
;
;
GetMove                 proc                            ;
                        PrintLine(0,4,MSG_PRESS_MOVE_KEY, MSG_PRESS_MOVE_KEY_LEN);
                        call HandleMoveChoice           ;
                        ret                             ;
pend

;
;
;
HandleMoveChoice        call ROM_KEY_SCAN               ;
                        inc d                           ; no shiftkey = ff
                        jp z, HandleMoveChoice          ; ignore shifted key combos
                        ld a,e                          ; a: = key code of key pressed (ff if none).
                        cp '1'                          ; between 1-9?
                        jp c,NotNum                     ;
                        cp '9'+1                        ;
                        jp nc,NotNum                    ;
                        call CheckUniqueMove            ;
                        jp z,HandleMoveChoice           ;
                        ld (MOVE_POSITION), a           ;
                        ld hl, MOVE_HISTORY             ;
                        push af                         ;
                        ld a, 0                         ;
                        ld b, 0                         ;
                        ld c, 8                         ;
                        cpir                            ;
                        pop af                          ;
                        ld (hl), a                      ; append move to move_history
                        ret                             ;
NotNum                  jp HandleMoveChoice             ;
;
;
;
SendMove                proc                            ;
                        call BuildOpponentMessage       ;
                        ld a, MBOX_CMD_SEND_MESSAGE     ; send:   0 1 3 1 98 97 104 111 106 115 105 98 111 102 108 111 98 117 116 115 117 106 97 114 115 116 117 97 114 116 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 116 104 101 32 113 117 105 99 107 32 98 114 111 119 110 32 102 111 120 0
                        call BuildOpponentMsgRequest    ;
                        ld h, 0                         ; result: 0
                        ld l, 2+1+1+20+20+200           ; proto+cmd+app+userid+targetnick+message
                        ld de, REQUESTBUF               ;
                        call MakeCIPSend                ;
                        call ProcessSendResponse        ;

                        call BuildOurMessage            ;
                        ld a, MBOX_CMD_SEND_MESSAGE     ; send:   0 1 3 1 98 97 104 111 106 115 105 98 111 102 108 111 98 117 116 115 117 106 97 114 115 116 117 97 114 116 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 116 104 101 32 113 117 105 99 107 32 98 114 111 119 110 32 102 111 120 0
                        call BuildOurMsgRequest         ;
                        ld h, 0                         ; result: 0
                        ld l, 2+1+1+20+20+200           ; proto+cmd+app+userid+targetnick+message
                        ld de, REQUESTBUF               ;
                        call MakeCIPSend                ;
                        call ProcessSendResponse        ;
                        ret                             ;
pend

ProcessSendResponse     ld hl, (ResponseStart)          ;
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_OK               ;
                        jp nz, PrintProblemSend         ;
                        ret                             ;
PrintProblemSend        PrintLine(15,15, MSG_ERR_SENDING,MSG_ERR_SENDING_LEN);
                        call PressKeyToContinue         ;
                        ret                             ;

BuildOpponentMsgRequest ld (MBOX_CMD), a                ;
                        ld de, REQUESTBUF               ; entire server request string
                        WriteString(MBOX_PROTOCOL_BYTES, 2);
                        WriteString(MBOX_CMD, 1)        ;
                        WriteString(MBOX_APP_ID, 1)     ; 1=nextmail
                        WriteString(MBOX_USER_ID,20)    ; userid
                        WriteString(OPPONENT_NICK,20)   ;
                        WriteString(OUT_MESSAGE,200)    ;
                        ret                             ;

BuildOurMsgRequest      ld (MBOX_CMD), a                ;
                        ld de, REQUESTBUF               ; entire server request string
                        WriteString(MBOX_PROTOCOL_BYTES, 2);
                        WriteString(MBOX_CMD, 1)        ;
                        WriteString(MBOX_APP_ID, 1)     ; 1=nextmail
                        WriteString(MBOX_USER_ID,20)    ; userid
                        WriteString(MBOX_NICK,20)       ;
                        WriteString(OUT_MESSAGE,200)    ;
                        ret                             ;

; byte 1: move number
;   0=no move yet
;   1-9 move 1-9
;   10 finished
; byte 2: our move?
;   1=yes 0=no
; bytes 3-11: move history
;   eg   900000000
;   then 940000000
;   then 942000000
;   then 942100000 where number matches grid below
;
;       o|x|o    1|2|3
;      --+-+--  --+-+--
;       o|x|x    4|5|6
;      --+-+--  --+-+--
;       x|x|o    7|8|9
BuildOpponentMessage    proc                            ;
                        ld hl, OUT_MESSAGE              ;
                        ld a, (MOVE_NUMBER)             ;
                        ld (hl), a                      ;
                        inc hl                          ;
                        ld a, 1                         ;
                        ld (hl), a                      ;
                        inc hl                          ;
                        ld d,h                          ;
                        ld e,l                          ;
                        ld hl, MOVE_HISTORY             ;
                        ld b,0                          ;
                        ld c,9                          ;
                        ldir                            ;
                        ret                             ;
pend

BuildOurMessage         proc                            ;
                        ld hl, OUT_MESSAGE              ;
                        ld a, (MOVE_NUMBER)             ;
                        ld (hl), a                      ;
                        inc hl                          ;
                        ld a, 0                         ;
                        ld (hl), a                      ;
                        inc hl                          ;
                        ld d,h                          ;
                        ld e,l                          ;
                        ld hl, MOVE_HISTORY             ;
                        ld b,0                          ;
                        ld c,9                          ;
                        ldir                            ;
                        ret                             ;
pend

;
; EXIT: carry set if won
;       and A contains token
;
; move_history: 92416700
; xmove: 946
; omove: 217
; win_table: 1,2,3, 1,4,7, 4,5,6, 7,8,9, 3,6,9, 1,5,9, 3,5,7
; move_position: 7
;
;       o|x|o    1|2|3
;      --+-+--  --+-+--
;       o|x|x    4|5|6
;      --+-+--  --+-+--
;       x|x|o    7|8|9
;
; for 1-7
;    visit win triple
;       check token at each
;       if same, win
;       else next triple
CheckWin                proc                            ;
                        ld hl, MOVE_HISTORY             ;
                        ld b, 0                         ;
                        ld c, 8                         ;
                        ld a, 0                         ;
                        cpir                            ;
                        ld a, c                         ; 5+ moves needed for win (o,x,o,x,o etc)
                        cp 5                            ;
                        ret nc                          ; not enough moves for a win
                        ld c, 0                         ;
                        ld hl, WIN_TABLE                ;
Loop                    ld a, (hl)                      ;
                        inc hl                          ;
                        ld b, (hl)                      ;
                        cp b                            ;
                        inc hl                          ;
                        jp nz, NoWin                    ;
                        ld b, (hl)                      ;
                        cp b                            ;
                        jp nz, NoWin                    ;
                        scf                             ;
                        ret                             ;
NoWin                   inc c                           ;
                        ld a, 9                         ;
                        cp c                            ;
                        jp nc, Loop                     ;
                        scf                             ;
                        ccf                             ;
                        ret                             ;
pend

;
;
;
TokenAtPosition         proc                            ;

                        ret                             ;
pend


;
; ENTRY: A holds grid number
; EXIT: z if move has happened before
;
CheckUniqueMove         proc                            ;
                        ld hl, MOVE_HISTORY             ;
                        ld b, 0                         ;
                        ld c, 8                         ;
                        cpir                            ;
                        ret                             ;
pend

;
; ENTRY
;   A = 'o' or 'x'
; EXIT
;   A = 'x' or 'o'
FlipToken               cp 'o'                          ;
                        jp z, MakeCross                 ;
                        ld a, 'o'                       ;
                        ret                             ;
MakeCross               ld a, 'x'                       ;
                        ret                             ;

; byte 1: move number
;   0=no move yet
;   1-9 move 1-9
;   10 finished
; byte 2: our move?
;   1=yes 0=no
; bytes 3-11: move history
;   eg   900000000
;   then 940000000
;   then 942000000
;   then 942100000 where number matches grid below
;
;       o|x|o    1|2|3
;      --+-+--  --+-+--
;       o|x|x    4|5|6
;      --+-+--  --+-+--
;       x|x|o    7|8|9
;
ProcessMessage          ld hl, IN_MESSAGE               ; byte 1: move number
                        ld a, (hl)                      ;
                        ld (MOVE_NUMBER), a             ;
                        cp 10                           ;
                        jp z, NotFinished               ;
                        ld a, 1                         ; finished!
                        ld (IS_GAME_FINISHED), a        ;
                        ret                             ;
NotFinished             ld a, 0                         ;
                        ld (IS_GAME_FINISHED), a        ;
                        inc hl                          ; byte 2 - our move?
                        ld a, (hl)                      ;
                        ld (IS_OUR_TURN), a             ;
                        inc hl                          ; bytes 3-11 - move history
                        ld de, MOVE_HISTORY             ;
                        ld b,0                          ;
                        ld c,9                          ; copy 9 moves into move_history
                        ldir                            ;
                        ret                             ;
pend                    ; main loop                     ;


;
; end of main
;

;
; join pool
;
JoinPool                ld a, MBOX_CMD_JOIN_POOL        ;
                        call BuildJoinPoolRequest       ;
                        ld de, REQUESTBUF               ;
                        ld h, 0                         ;
                        ld l, 2+1+1+20+1                ; proto+cmd+app+userid+poolsize
                        call MakeCIPSend                ;
                        call ProcessJoinPoolResponse    ;
                        ret                             ;
; BuildStandardRequest
;
; ENTRY
;  A = MBOX CMD
; EXIT
;  REQUESTBUF is populated ready for CIPSEND
;
BuildJoinPoolRequest    ld (MBOX_CMD), a                ;
                        ld de, REQUESTBUF               ; entire server request string
                        WriteString(MBOX_PROTOCOL_BYTES, 2);
                        WriteString(MBOX_CMD, 1)        ;
                        WriteString(MBOX_APP_ID, 1)     ; 1=nextmail
                        WriteString(MBOX_USER_ID,20)    ; userid
                        WriteString(MBOX_POOL_SIZE, 1)  ;
                        ret                             ;

ProcessJoinPoolResponse proc                            ;
                        ld hl, (ResponseStart)          ;
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_UNREGISTERED_USERID ; 104
                        jp z, Problem                   ;
                        cp MBOX_STATUS_JOINED_POOL      ; 205
                        jp z, Joined                    ;
                        cp MBOX_STATUS_ALREADY_JOINED_POOL ; 207
                        jp z, Joined                    ;
                        jp Problem                      ;
Joined                  inc hl                          ; move past status
                        ld a, (hl)                      ; get count
                        ld (MBOX_POOL_ID), a            ; store
                        inc hl                          ; get 2nd byte
                        ld a, (hl)                      ;
                        ld (MBOX_POOL_ID+1), a          ; store 2nd
                        ret                             ;

Problem                 PrintLine(0,7,MSG_JOIN_FAIL, MSG_JOIN_FAIL_LEN);
                        call PressKeyToContinue         ;
                        ret                             ;
pend

;
; get pool
;
GetPool                 ld a, MBOX_CMD_GET_POOL         ;
                        call BuildGetPoolRequest        ;
                        ld de, REQUESTBUF               ;
                        ld h, 0                         ;
                        ld l, 2+1+1+20+2                ; proto+cmd+app+userid+poolid
                        call MakeCIPSend                ;
                        call ProcessGetPoolResponse     ;
                        ret                             ;
; BuildStandardRequest
;
; ENTRY
;  A = MBOX CMD
; EXIT
;  REQUESTBUF is populated ready for CIPSEND
;
BuildGetPoolRequest     ld (MBOX_CMD), a                ;
                        ld de, REQUESTBUF               ; entire server request string
                        WriteString(MBOX_PROTOCOL_BYTES, 2);
                        WriteString(MBOX_CMD, 1)        ;
                        WriteString(MBOX_APP_ID, 1)     ; 1=nextmail
                        WriteString(MBOX_USER_ID,20)    ; userid
                        WriteString(MBOX_POOL_ID, 2)    ;
                        ret                             ;

ProcessGetPoolResponse  proc                            ;
                        ld hl, (ResponseStart)          ;
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_UNREGISTERED_USERID ; 104
                        jp z, Problem                   ;
                        cp MBOX_STATUS_MISSING_POOL_ID  ; 205
                        jp z, Problem                   ;
                        cp MBOX_STATUS_POOL_UNFILLED    ; 208
                        jp z, Done                      ;
                        cp MBOX_STATUS_POOL_FILLED      ; 209
                        jp Filled                       ;
                        jp Problem                      ;

Filled                  inc hl                          ; move past status
                        ld a, (hl)                      ; get count of nicks
                        cp 2                            ;
                        jp nz, Problem                  ; must be 2 - us and them
                        inc hl                          ; move past nick count
                        ld a, (hl)                      ; a holds nick1 len
                        ld b, 0                         ;
                        ld c, a                         ;
                        ld (NICK1_LEN), a               ;
                        inc hl                          ; move to nick contents
                        ld de, NICK1                    ;
                        ldir                            ; populate NICK1 with message contents. hl at next nick len
                        call CheckIfNickIsOurs          ;
                        jp nz, TheyGoFirst              ; nick 1 is the opponent, so they go first
                        ld a, (hl)                      ; nick1 is us. get nick2 len
                        ld b, 0                         ;
                        ld c, a                         ;
                        ld (OPPONENT_NICK_LEN), a       ;
                        inc hl                          ; move to nick2
                        ld de, OPPONENT_NICK            ;
                        ldir                            ;
                        ld a, 1                         ;
                        ld (WE_GO_FIRST), a             ; set WE_GO_FIRST to true
                        jp Done                         ;

TheyGoFirst             ld hl, NICK1                    ;
                        ld de, (OPPONENT_NICK)          ; populate OPPONENT_NICK with NICK1
                        ld a, (NICK1_LEN)               ;
                        ld (OPPONENT_NICK_LEN), a       ;
                        ld b, 0                         ;
                        ld c, a                         ;
                        ldir                            ;
                        ld a, 0                         ;
                        ld (WE_GO_FIRST), a             ; set WE_GO_FIRST to false

Done                    ret                             ;

Problem                 PrintLine(0,7,MSG_JOIN_FAIL, MSG_JOIN_FAIL_LEN);
                        call PressKeyToContinue         ;
                        ret                             ;
pend

;
; compares a nick with our own (MBOX_NICK) with length (MBOX_NICK_LEN)
; ENTRY
;   (NICK1)=candidate nick
;   (NICK1_LEN)=length of nick
; EXIT
;   Z set if a match with our nick
;
CheckIfNickIsOurs       proc                            ;
                        ld a, (NICK1_LEN)               ;
                        ld b, a                         ;
                        ld a, (MBOX_NICK_LEN)           ;
                        cp b                            ;
                        ret nz                          ; if nicks are not the same length

                        ld hl, MBOX_NICK-1              ; nicks are same length
                        ld de, NICK1-1                  ;
Loop                    push bc                         ; current count
                        inc hl                          ;
                        inc de                          ;
                        ld b, (hl)                      ;
                        ld a, (de)                      ;
                        cp b                            ;
                        pop bc                          ;
                        ret nz                          ; stop looking if not match
                        dec b                           ;
                        jp nz,Loop                      ;
                        ret                             ;
pend
;
; setup screen
;
SetupScreen             Border(7)                       ; 7=white
                        OpenOutputChannel(2)            ; ROM: Open channel to upper screen (channel 2)
SetLayer1_1             ld a, 1                         ; set layer via IDE_MODE using M_P3DOS (needs bank 7)
                        ld b, 1                         ; if A=1, change mode to:
                        ld c, 1                         ;   B=layer (0,1,2)
                        M_P3DOS(IDE_MODE,7)             ;   C=sub-mode (if B=1): 0=lo-res, 1=ula, 2=hi-res, 3=hi-col                                                        ;
ClearScreen             ld a,14                         ; 'clear window control code' (for layers 1+) (see IDE_MODE docs)
                        rst $10                         ;  ROM print a char
SetFontWidth            PrintChar(30)                   ; set char width in pixels
                        PrintChar(5)                    ; to 5 (51 chars wide)
                        ret                             ;

;
; clear centre panel of any text
;
ClearCentre             PrintAt(0,7)                    ;
                        ld bc, 51*13                    ; 52 cols * 13 rows.
ClearLoop               PrintChar(' ')                  ;
                        dec bc                          ;
                        ld a,c                          ;
                        or b                            ;
                        jp nz ClearLoop                 ;
                        ret                             ;

HandleRegister          PrintLine(0,7,REG_PROMPT, REG_PROMPT_LEN) ;
                        PrintLine(0,8,PROMPT, PROMPT_LEN) ;
                        call WipeUserId                 ;
                        call HandleUserIdInput          ;
                        jp c, RegBreak                  ; back to menu - input was cancelled by break
                        call PopulateMboxUserId         ;
                        call RegisterUserId             ;
                        call PressKeyToContinue         ;
RegBreak                call ClearCentre                ;
                        ret                             ;
;
; copy buffer into fixed location
;
PopulateMboxUserId      ld hl, USER_ID_BUF              ; source
                        ld de, MBOX_USER_ID             ;
                        ld bc, 20                       ;
                        ldir                            ;
                        ret                             ;
;
; register
;

; 1. register user for app
;
; response:
;
; pos:       |  0      | 1              |
; size:      |  1      | 20             |
; field:     | status  | nickname       |
; condition: |         | status=101/201 |

RegisterUserId:         ld a, MBOX_CMD_REGISTER         ;
                        call BuildStandardRequest       ; send:     0   1  1   1  98  97 104 111 106 115 105 98 111 102 108 111 98 117 116 115 117 106 97 114
                        ld de, REQUESTBUF               ; result: 201 115 116 117 97 114 116   0   0   0   0   0  0   0   0   0
                        ld h, 0                         ;
                        ld l, 2+1+1+20                  ; proto+cmd+app+userid
                        call MakeCIPSend                ;
                        call ProcessRegResponse         ;
                        ret                             ;
;
; set all 20 chars of user id to spaces
;
WipeUserId              ld hl, USER_ID_BUF              ;   fill nick with spaces (0s cause problems when printing to screen)
                        ld d,h                          ;
                        ld e,l                          ;
                        inc de                          ;
                        ld (hl), ' '                    ;
                        ld bc, 19                       ;
                        ldir                            ;
                        ret                             ;
;
; process registration response
;
ProcessRegResponse      ld hl, (ResponseStart)          ;
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_USER_ALREADY_REGISTERED ; already? no problem
                        jp z, PrintNickname             ;
                        cp MBOX_STATUS_REGISTER_OK      ; ok? cool
                        jp z, PrintNickname             ;
                        ld a, 0                         ;
BadUser                 ld (CONNECTED), a               ;
                        ret                             ;
PrintNickname           ld a, 1                         ;
                        ld (CONNECTED), a               ;
                        ld de, MBOX_NICK                ;
                        ld hl, (ResponseStart)          ;
                        inc hl                          ; move past status
                        ld bc, 20                       ; nicks/userids are 20
                        ldir                            ;
                        ld hl, MBOX_NICK                ;
                        ld de, MBOX_NICK_LEN            ;
                        call CalcNickLength             ;
                        ret                             ;

;
; fetch number of messages for user
;

; 3. get message count
;
; response:
;
; pos:      | 0      | 1            |
; size:     | 1      | 1            |
; field:    | status | messageCount |
; condition |        | status=202   |

HandleCount             ld a, MBOX_CMD_MESSAGE_COUNT    ;
                        call BuildStandardRequest       ; send:     0   1  1   1  98  97 104 111 106 115 105 98 111 102 108 111 98 117 116 115 117 106 97 114
                        ld de, REQUESTBUF               ; result: 201 115 116 117 97 114 116   0   0   0   0   0  0   0   0   0
                        ld h, 0                         ;
                        ld l, 2+1+1+20                  ; proto+cmd+app+userid
                        call MakeCIPSend                ;
                        call ProcessMsgCountResponse    ;
                        ret                             ;

ProcessMsgCountResponse proc                            ;
                        ld hl, (ResponseStart)          ;
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_UNREGISTERED_USERID ;
                        jp z, Problem                   ;
                        cp MBOX_STATUS_COUNT_OK         ;
                        jp nz, Problem                  ;
                        inc hl                          ; move past status
                        ld a, (hl)                      ; get count
                        ld (MSG_COUNT), a               ; store
                        inc hl                          ; get 2nd byte
                        ld a, (hl)                      ;
                        ld (MSG_COUNT+1), a             ; store 2nd
                        scf                             ;
                        ccf                             ; carry is clear on exit when ok
                        ret                             ;
Problem                 scf                             ; carry is set on exit when not ok
                        ret                             ;
pend

; 4. getMessage
;
; response:
;
; pos:      | 0      | 1          | 21         | 23         |
; size:     | 1      | 20         | 2          | n          |
; field:    | status | senderNick | messagelen | message    |
; condition |        |              status=203              |
;
HandleGetMessage        ld a, MBOX_CMD_GET_MESSAGE      ;
                        call BuildGetMsgRequest         ;
                        ld h, 0                         ;
                        ld l, 2+1+1+20+2                ; 2x_proto+cmd+app+userid+2x_msg_id
                        ld de, REQUESTBUF               ;
                        call MakeCIPSend                ;
                        call ProcessGetResponse         ;

                        ret                             ;


BuildGetMsgRequest      ld (MBOX_CMD), a                ;
                        ld de, REQUESTBUF               ; entire server request string
                        WriteString(MBOX_PROTOCOL_BYTES, 2);
                        WriteString(MBOX_CMD, 1)        ; cmd is get message by id
                        WriteString(MBOX_APP_ID, 1)     ; 1=nextmail
                        WriteString(MBOX_USER_ID,20)    ; userid
                        WriteString(MBOX_MSG_ID,2)      ; param 1 is msg id
                        ret                             ;

ProcessGetResponse      ld hl, (ResponseStart)          ;  status byte
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_GET_MESSAGE_OK   ; is it ok?
                        jp nz, PrintBadMsgId            ; no - show error
                        inc hl                          ; yes - move past status byte into sender's nick
                        ld de, IN_NICK                  ; will hold our copy of the msg sender's nick
                        ld bc, 20                       ;
                        ldir                            ;
                        push hl                         ; hl pointing after nick
                        ld de, IN_NICK_LEN              ;
                        ld hl, IN_NICK                  ;
                        call CalcNickLength             ;
                        pop hl                          ; pointing at msg len
                        ld a, (hl)                      ; this is msg len
                        ld (IN_MSG_LEN), a              ;
                        ld de, IN_MESSAGE               ; populate in_messge with contents of response
                        inc hl                          ; move past len byte into start of msg
                        ld bc, (IN_MSG_LEN)             ;
                        ldir                            ;
                        ret                             ;

PrintBadMsgId           PrintLine(0,15,BAD_MSG_ID,BAD_MSG_ID_LEN) ;
                        ret                             ;

;                                                              ;
; calc the len of the user's nick (20 bytes)
; nicks are 1+ characters with trailing $00s
;
; ENTRY: HL address of nick
;        DE address of nick_len (2 bytes)
; EXIT: (DE) contains nick len
;
CalcNickLength          ld a, $00                       ;
                        ld bc, 20                       ; nick max len
                        cpir                            ; find first $00 or bc == 0
                        jp nz, LenIsMax                 ; z only set if match found
                        inc c                           ; back up the counter
                        ld a, 20                        ; no: calc len of 20 - bc
                        sub c                           ; if bc max is 20, b is 0
                        ld (de), a                      ;
                        ret                             ;

LenIsMax                ld a, 20                        ;
                        ld (de), a                      ;
                        ret                             ;


; BuildStandardRequest
;
; ENTRY
;  A = MBOX CMD
; EXIT
;  REQUESTBUF is populated ready for CIPSEND
;
BuildStandardRequest    ld (MBOX_CMD), a                ;
                        ld de, REQUESTBUF               ; entire server request string
                        WriteString(MBOX_PROTOCOL_BYTES, 2);
                        WriteString(MBOX_CMD, 1)        ;
                        WriteString(MBOX_APP_ID, 1)     ; 1=nextmail
                        WriteString(MBOX_USER_ID,20)    ; userid
                        ret                             ;

PressKeyToContinue      PrintLine(10,17, MSG_PRESS_KEY, MSG_PRESS_KEY_LEN);
KeyLoop                 call ROM_KEY_SCAN               ; d=modifier e=keycode or $ff
                        ld a,d                          ; do we have a key modifier? (ss CS etc)
                        cp $ff                          ; ff=no
                        ret nz                          ; yes, return
                        ld a,e                          ;
                        cp $ff                          ; ff=no
                        ret nz                          ; yes, return
                        jp KeyLoop                      ; otherwise continue to check for input

BAD_MSG_ID              defb "bad message id error for get msg" ;
BAD_MSG_ID_LEN          equ $-BAD_USER_MSG              ;
BAD_USER_MSG            defb "<no user registered>"     ;
BAD_USER_MSG_LEN        equ $-BAD_USER_MSG              ;
BOARD                   defb "  | |  "                  ;
                        defb "--+-+--"                  ;
                        defb "  | |  "                  ;
                        defb "--+-+--"                  ;
                        defb "  | |  "                  ;
Buffer:                 ds 256                          ;
BufferLen               equ $-Buffer                    ;
BUFLEN                  defs 1                          ;
CONNECTED               defb 00                         ;
CONNECTED_TO            defb "Connected to "            ;
CONNECTED_TO_LEN        equ $-CONNECTED_TO              ;
HYPHEN                  defb '-'                        ;
IN_MESSAGE              defs 200                        ;
IN_MSG_LEN              defb 0,0                        ; 2 because we'll point BC at it for ldir
IN_NICK                 defs 20                         ;
IN_NICK_LEN             defb 0,0                        ;
IS_GAME_FINISHED        defb 1                          ;
IS_OUR_TURN             defb 1                          ;
MboxHost                defb "nextmailbox.spectrum.cl"  ;
MboxHostLen             equ $-MboxHost                  ;
MboxPort:               defb "8361"                     ;
MboxPortLen:            equ $-MboxPort                  ;
MBOX_APP_ID             defb $02                        ; nxtnoughts is app 2 in db
MBOX_BLANK_NICK         defs 20,' '                     ;
MBOX_CMD                defb $01                        ;
MBOX_MSG_ID             defb 0,0                        ; 2 bytes for 0-65535
MBOX_NICK               defs 20                         ; our nick
MBOX_NICK_LEN           defb 00,00                      ;
MBOX_POOL_ID            defb 00,00                      ;
MBOX_POOL_SIZE          defb 2                          ;
MBOX_PROTOCOL_BYTES     defb $00, $01                   ;
MBOX_USER_ID            defs 20                         ; the one used for transmission to allow the working buffer to be reset
MOVE_HISTORY            defb 9                          ;
MOVE_NUMBER             defb 1                          ;
MOVE_POSITION           defb 1                          ;
MOVE_TABLE              defb 2,4,6,13,15,17,27,19,31    ; offset from board for piece position
MSG_COUNT               defb 0,0                        ;
MSG_ERR_SENDING         defb "Error sending message"    ;
MSG_ERR_SENDING_LEN     equ $-MSG_ERR_SENDING           ;
MSG_FINISHED            defb "Game finished"            ;
MSG_FINISHED_LEN        equ $-MSG_FINISHED              ;
MSG_JOIN_FAIL           defb "Failed to join pool"      ;
MSG_JOIN_FAIL_LEN       equ $-MSG_JOIN_FAIL             ;
MSG_PRESS_KEY           defb "Press any key to continue";
MSG_PRESS_KEY_LEN       equ $-MSG_PRESS_KEY             ;
MSG_PRESS_MOVE_KEY      defb "Press 1-9 to place token" ;
MSG_PRESS_MOVE_KEY_LEN  equ $-MSG_PRESS_MOVE_KEY        ;
MSG_NICK                defb "Nick: "                   ;
MSG_NICK_LEN            equ $-MSG_NICK                  ;
MSG_YOU_LOST            defb "You lost!"                ;
MSG_YOU_LOST_LEN        equ $-MSG_YOU_LOST              ;
MSG_YOU_WON             defb "You won!"                 ;
MSG_YOU_WON_LEN         equ $-MSG_YOU_WON               ;
MSG_WAITING_FOR_MOVE    defb "Waiting for opponent to move";
MSG_WAITING_FOR_MOVE_LEN equ $-MSG_WAITING_FOR_MOVE     ;
MSG_WAITING_FOR_POOL    defb "Waiting for opponent to join";
MSG_WAITING_FOR_POOL_LEN equ $-MSG_WAITING_FOR_POOL     ;
NICK1                   ds 20                           ;
NICK1_LEN               equ $-NICK1                     ;
OPPONENT_NICK           ds 20                           ;
OPPONENT_NICK_LEN       defb 1                          ;
OUT_MESSAGE             ds 200,$09                      ; gets printed so fill with tab (not 0s and not space because users use space)
PROMPT                  defb "> "                       ;
PROMPT_LEN              equ $-PROMPT                    ;
REG_PROMPT              defb "Enter your Next Mailbox Id (then enter)";
REG_PROMPT_LEN          equ $-REG_PROMPT                ;
REQUESTBUF              ds 256                          ;
SENDBUF                 defb 255                        ;
USER_ID_BUF             defs 20, ' '                    ; our input buffer
VERSION                 defb "nxtNoughts v0.1 2020 Tadaguff";
VERSION_LEN             equ $-VERSION                   ;
WE_WON                  defb 0                          ;
WE_GO_FIRST             defb 0                          ;
WIN_TABLE               defb 1,2,3,1,4,7,4,5,6,7,8,9,3,6,9,1,5,9,3,5,7;

                        include "esp.asm"               ;
                        include "constants.asm"         ;
                        include "msg.asm"               ;
                        include "parse.asm"             ;
                        include "macros.asm"            ;
                        include "esxDOS.asm"            ;
                        include "cip.asm"               ;
                        include "keys.asm"              ;
                        include "zeus.asm"              ; syntax highlighting


; Raise an assembly-time error if the expression evaluates false
                        zeusassert zeusver<=81, "Upgrade to Zeus v4.00 (TEST ONLY) or above, available at http://www.desdes.com/products/oldfiles/zeustest.exe";
; zeusprint               zeusver                         ;
; Generate a NEX file                                   ; Instruct the .NEX loader to write the file handle to this
                        ;        output_z80 "NxtMail.z80",$FF40, Main ;
                        output_nex "nxtnoughts.nex", $FF40, Main ; Generate the file, with SP argument followed PC
                        ; Zeus "just knows" which 16K banks to include in the .NEX file,
                        ; making generation a one-liner if you don't want loading screens
                        ; or external palette files. See History/Documentation in Zeus
                        ; for complete instructions and syntax.
