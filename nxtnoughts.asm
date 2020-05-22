;
; nxtNoughtsAndCrosses - 2 player networked game for ZX Spectrum Next
;   uses Next Mailbox Protocol 0.1
;
;
;       o|x|o  
;      --+-+--
;       o|x|x
;      --+-+--
;       o|x|o

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
MBOX_STATUS_OK          equ 0                           ;
MBOX_STATUS_INV_PROTO   equ 1                           ;
MBOX_STATUS_INV_CMD     equ 2                           ;
MBOX_STATUS_INV_APP     equ 3                           ;
MBOX_STATUS_INV_USERID  equ 4                           ;
MBOX_STATUS_INV_LENGTH  equ 5                           ;
MBOX_STATUS_INT_ERR     equ 6                           ;
MBOX_STATUS_MISS_NICK   equ 7                           ;
MBOX_STATUS_MISS_MSG    equ 8                           ;
MBOX_STATUS_UNIMPL      equ 9                           ;
MBOX_STATUS_MISS_MSG_ID equ 10                          ;

MBOX_STATUS_USR_ALR_REG equ 101                         ;
MBOX_STATUS_UNREG_NICK  equ 102                         ;
MBOX_STATUS_UNK_USERID  equ 103                         ;
MBOX_STATUS_UNREG_USER  equ 104                         ;

MBOX_STATUS_REGISTER_OK equ 201                         ;
MBOX_STATUS_COUNT_OK    equ 202                         ;
MBOX_STATUS_GET_MSG_OK  equ 203                         ;
MBOX_STATUS_INV_MSG_ID  equ 204                         ;

MBOX_CMD_REGISTER       equ 1                           ;
MBOX_CMD_CHECK_REG_NICK equ 2                           ;
MBOX_CMD_SEND_MESSAGE   equ 3                           ;
MBOX_CMD_MESSAGE_COUNT  equ 4                           ;
MBOX_CMD_GET_MESSAGE    equ 5                           ;
MBOX_CMD_GET_RAND_USERS equ 6                           ; # ?
MBOX_CMD_AWAIT_USERS    equ 7                           ; # session / group?



org                     $8000                           ; This should keep our code clear of NextBASIC sysvars
                        ;                                 (Necessary for making NextZXOS API calls);
;
; main loop
;
Main                    proc                            ;
                        di                              ;
                        nextreg NXREG_TURBO_CTL, CPU_28 ; Next Turbo Control Register to set cpu speed
                        call MakeCIPStart               ; setup comms to server

                        call SetupScreen                ;
                        call LoadFile                   ; obtain any previously saved userid and register userid with server
                        call DisplayMenu                ;
                        call DisplayStatus              ;
MainLoop                call HandleMenuChoice           ;

                        jp MainLoop                     ;
pend

;
; end of main
;



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

;
; display main menu
;
DisplayMenu             call DrawMenuBox                ;
                        PrintLine(1,1,MENU_LINE_1,MENU_LINE_1_LEN) ;
                        ret                             ;
;
; show connected, nick, message counts
;
DisplayStatus           PrintLine(0,21,BLANK_ROW,51)    ;
                        PrintLine(0,22,BLANK_ROW,51)    ;
                        PrintLine(0,23,BLANK_ROW,51)    ;
                        PrintLine(0,21,CONNECTED_TO, CONNECTED_TO_LEN);
                        PrintLine(0+CONNECTED_TO_LEN,21,MboxHost,MboxHostLen) ;
                        ld a, (CONNECTED)               ;
                        cp 1                            ;
                        jp z, PrintConnected            ;
                        PrintLine(MboxHostLen+1,18,OFFLINE,OFFLINE_LEN);
                        ret                             ;   bail because we're offline
PrintConnected          PrintLine(0,22,MSG_NICK,MSG_NICK_LEN) ;
                        PrintLineLenVar(0+MSG_NICK_LEN,22,MBOX_NICK, MBOX_NICK_LEN) ;
                        PrintLine(0,23,MESSAGES,MESSAGES_LEN);
                        PrintLine(49-VERSION_LEN,23,VERSION,VERSION_LEN);
                        ld hl,MSG_COUNT                 ;
                        inc (hl)                        ;
                        dec (hl)                        ; 
                        jp z, PrintZeroMessages         ; don't convert message count to ascii if zero (ldir uses len in BC)
                        ld hl, (MSG_COUNT)              ;
                        call ConvertWordToAsc           ; otherwise convert number to ascii: text in WordStart, length in WordLen
                        ld bc, (WordLen)                ; fill MSG_COUNT_BUF with the ascii number
                        ld de, MSG_COUNT_BUF            ;
                        ld hl, (WordStart)              ;
                        ldir                            ;
                        PrintLineLenVar(0+MESSAGES_LEN,23,MSG_COUNT_BUF,WordLen) ;
                        ret                             ;
PrintZeroMessages       PrintLine(0+MESSAGES_LEN,23,MSG_COUNT_ZERO,1);
                        ret                             ;


; HandleMenuChoice
;
HandleMenuChoice        call ROM_KEY_SCAN               ;
                        inc d                           ; no shiftkey = ff
                        ret nz                          ; ignore shifted key combos
                        ld a,e                          ; a: = key code of key pressed (ff if none).
                        cp $24                          ; check for 1 key
                        jp z, HandleRegister            ;
                        cp $1c                          ; check for 2 key
                        jp z, HandleSend                ;
                        cp $14                          ; check for 3 key
                        jp z, HandleViewMessage         ;
                        cp $0c                          ; check for 4 key
                        jp z, HandleCount               ;
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
                        call HandleCount                ; also displays status
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
                        cp MBOX_STATUS_USR_ALR_REG      ; already? no problem
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
                        call SaveFile                   ;
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
                        call DisplayStatus              ;

                        ret                             ;

ProcessMsgCountResponse ld hl, (ResponseStart)          ;
                        ld a, (hl)                      ;
                        cp MBOX_STATUS_COUNT_OK         ;
                        jp nz, PrintProblem             ;
                        inc hl                          ; move past status
                        ld a, (hl)                      ; get count
                        ld (MSG_COUNT), a               ; store
                        inc hl                          ; get 2nd byte
                        ld a, (hl)                      ;
                        ld (MSG_COUNT+1), a             ; store 2nd
                        ret                             ;
PrintProblem            PrintLine(6,21,BAD_USER_MSG, BAD_USER_MSG_LEN) ;
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

BAD_MSG_ID              defb "bad message number"       ;
BAD_MSG_ID_LEN          equ $-BAD_MSG_ID                ;
BAD_USER_MSG            defb "<no user registered>"     ;
BAD_USER_MSG_LEN        equ $-BAD_USER_MSG              ;
Buffer:                 ds 256                          ;
BufferLen               equ $-Buffer                    ;
BUFLEN                  defs 1                          ;
CONNECTED               defb 00                         ;
CONNECTED_TO            defb "Connected to "            ;
CONNECTED_TO_LEN        equ $-CONNECTED_TO              ;
DIR_NAME                defb "/nxtMail2",0              ;
FILEBUF                 defs 128                        ;
FILE_NAME               defb "/nxtMail2/nxtMail.dat",0  ;
HYPHEN                  defb '-'                        ;
IN_MESSAGE              defs 200                        ;
IN_MSG_LEN              defb 0,0                        ; 2 because we'll point BC at it for ldir
IN_NICK                 defs 20                         ;
IN_NICK_LEN             defb 0,0                        ;
MboxHost                defb "nextmailbox.spectrum.cl"  ;
MboxHostLen             equ $-MboxHost                  ;
MboxPort:               defb "8361"                     ;
MboxPortLen:            equ $-MboxPort                  ;
MBOX_APP_ID             defb $01                        ; nxtmail is app 1 in db
MBOX_BLANK_NICK         defs 20,' '                     ;
MBOX_CMD                defb $01                        ;
MBOX_MSG_ID             defb 0,0                        ; 2 bytes for 0-65535
MBOX_NICK               defs 20                         ;
MBOX_NICK_LEN           defb 00,00                      ;
MBOX_PROTOCOL_BYTES     defb $00, $01                   ;
MBOX_USER_ID            defs 20                         ; the one used for transmission to allow the working buffer to be reset
MENU_LINE_1             defb "1. Connect/Register userId" ;
MENU_LINE_1_LEN         equ $-MENU_LINE_1               ;
MSG_ERR_SENDING         defb "Error sending message"    ;
MSG_ERR_SENDING_LEN     equ $-MSG_ERR_SENDING           ;
MSG_GET_MSG_PROMPT      defb "Message body: (200 max. Enter to end)";
MSG_GET_MSG_PROMPT_LEN  equ $-MSG_GET_MSG_PROMPT        ;
MSG_FROM                defb "From: "                   ;
MSG_FROM_LEN            equ $-MSG_FROM                  ;
MSG_ID_BUF              defs 5,' '                      ; '1'-'65535'
MSG_ID_BUF_LEN          defb 0                          ; length of the digits entered 1-5
MSG_ID_PROMPT           defb "Message number (1-65535. Enter to end)" ;
MSG_ID_PROMPT_LEN       equ $-MSG_ID_PROMPT             ;
MSG_PRESS_KEY           defb "Press any key to continue";
MSG_PRESS_KEY_LEN       equ $-MSG_PRESS_KEY             ;
MSG_UNREG_NICK          defb "Nick is unregistered with NxtMail";
MSG_UNREG_NICK_LEN      equ $-MSG_UNREG_NICK            ;
MSG_NICK                defb "Nick: "                   ;
MSG_NICK_LEN            equ $-MSG_NICK                  ;
OUT_MESSAGE             ds 200,$09                      ; gets printed so fill with tab (not 0s and not space because users use space)
PROMPT                  defb "> "                       ;
PROMPT_LEN              equ $-PROMPT                    ;
REG_PROMPT              defb "Enter your Next Mailbox Id (then enter)";
REG_PROMPT_LEN          equ $-REG_PROMPT                ;
REQUESTBUF              ds 256                          ;
SENDBUF                 defb 255                        ;
USER_ID_BUF             defs 20, ' '                    ; our input buffer
VERSION                 defb "nxtMail v0.4 2020 Tadaguff";
VERSION_LEN             equ $-VERSION                   ;

T
                        include "esp.asm"               ;
                        include "constants.asm"         ;
                        include "msg.asm"               ;
                        include "parse.asm"             ;
                        include "macros.asm"            ;
                        include "esxDOS.asm"            ;
                        include "cip.asm"               ;
                        include "file.asm"              ;
                        include "keys.asm"              ;
                        include "zeus.asm"              ; syntax highlighting


; Raise an assembly-time error if the expression evaluates false
                        zeusassert zeusver<=78, "Upgrade to Zeus v4.00 (TEST ONLY) or above, available at http://www.desdes.com/products/oldfiles/zeustest.exe";
; zeusprint               zeusver                         ;
; Generate a NEX file                                   ; Instruct the .NEX loader to write the file handle to this
                        ;        output_z80 "NxtMail.z80",$FF40, Main ;
                        output_nex "nxtnoughts.nex", $FF40, Main ; Generate the file, with SP argument followed PC
                        ; Zeus "just knows" which 16K banks to include in the .NEX file,
                        ; making generation a one-liner if you don't want loading screens
                        ; or external palette files. See History/Documentation in Zeus
                        ; for complete instructions and syntax.
