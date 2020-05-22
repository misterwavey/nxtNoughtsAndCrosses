MakeCIPStart:
                        ld de, Buffer                   ;
                        WriteString(Cmd.CIPSTART1, Cmd.CIPSTART1Len);
                        WriteString(MboxHost, MboxHostLen) ;
                        WriteString(Cmd.CIPSTART2, Cmd.CIPSTART2Len);
                        WriteString(MboxPort, MboxPortLen) ;
                        WriteString(Cmd.Terminate, Cmd.TerminateLen);

InitialiseESP:
                        PrintLine(0,13, Buffer, 51)     ;
                        PrintLine(0,14,Msg.InitESP,20)  ; "Initialising WiFi..."

                        PrintAt(0,15)                   ;
                        PrintMsg(Msg.SetBaud1)          ; "Using 115200 baud, "
                        NextRegRead(Reg.VideoTiming)    ;
                        and %111                        ;
                        push af                         ;
                        ld d, a                         ;
                        ld e, 5                         ;
                        mul                             ;
                        ex de, hl                       ;
                        add hl, Timings.Table           ;
                        call PrintRst16                 ; "VGA0/../VGA6/HDMI"
                        PrintMsg(Msg.SetBaud2)          ; " timings"
                        pop af                          ;
                        add a,a                         ;
                        ld hl, Baud.Table               ;
                        add hl, a                       ;
                        ld e, (hl)                      ;
                        inc hl                          ;
                        ld d, (hl)                      ;
                        ex de, hl                       ; HL now contains the prescalar baud value
                        ld (Prescaler), hl              ;
                        ld a, %x0x1 x000                ; Choose ESP UART, and set most significant bits
                        ld (Prescaler+2), a             ; of the 17-bit prescalar baud to zero,
                        ld bc, UART_Sel                 ; by writing to port 0x143B.
                        out (c), a                      ;
                        dec b                           ; Set baud by writing twice to port 0x143B
                        out (c), l                      ; Doesn't matter which order they are written,
                        out (c), h                      ; because bit 7 ensures that it is interpreted correctly.
                        inc b                           ; Write to UART control port 0x153B

;                        ld a, (Prescaler+2)             ; Print three bytes written for debug purposes
;                        call PrintAHexNoSpace
;                        ld a, (Prescaler+1)
;                        call PrintAHexNoSpace
;                        ld a, (Prescaler)
;                        call PrintAHexNoSpace
;                        ld a, CR
;                        rst 16                        ;

                        ESPSend("ATE0")                 ; * Until we have absolute frame-based timeouts, send first AT
                        call ESPReceiveWaitOK           ; * cmd twice to give it longer to respond to one of them.
                        ESPSend("ATE0")                 ;
                        ErrorIfCarry(Err.ESPComms1)     ; Raise ESP error if no response
                        call ESPReceiveWaitOK           ;
                        ErrorIfCarry(Err.ESPComms2)     ; Raise ESP error if no response
                        ; * However... the UART buffer probably needs flushing here now!
                        ESPSend("AT+CIPCLOSE")          ; Don't raise error on CIPCLOSE
                        call ESPReceiveWaitOK           ; Because it might not be open
                        ; ErrorIfCarry(Err.ESPComms)    ; We never normally want to raise an error after CLOSE
                        ESPSend("AT+CIPMUX=0")          ;
                        ErrorIfCarry(Err.ESPComms3)     ; Raise ESP error if no response
                        call ESPReceiveWaitOK           ;
                        ErrorIfCarry(Err.ESPComms4)     ; Raise ESP error if no response
Connect:
                        PrintMsg(Msg.Connect1)          ;
                        ; Print(MboxHost, MboxHost) ;
                        PrintMsg(Msg.Connect2)          ;
                        ESPSendBuffer(Buffer)           ; This is AT+CIPSTART="TCP","<server>",<port>\r\n
                        ErrorIfCarry(Err.ESPConn1)      ; Raise ESP error if no connection
                        call ESPReceiveWaitOK           ;
                        ErrorIfCarry(Err.ESPConn2)      ; Raise ESP error if no response
                        PrintMsg(Msg.Connected)         ;
                        ret                             ;
;
; MakeCIPSend
; Entry
;   DE = request string: proto,cmd,app,userid,(optionally) nick or * or msg num, (optionally) msg
;   HL = request length (note: without the cipsend cr lf)
; Exit
;   ResponseStart is pointer to buffer containing response
;

MakeCIPSend:
                        ld (RequestLen), hl             ; store length of the request we'll be sending to the server
                        inc hl                          ; add space for CR
                        inc hl                          ; add space for LF
                        ld (RequestLenAddr), hl         ; store this version of the length for the CIPSend total
                        ex de, hl                       ; need hl to store buffer below
                        ld (RequestBufAddr), hl         ; store addr of actual request to send
                        ex de, hl                       ; restore hl
                        call ConvertWordToAsc           ; ie 26d becomes 2 ascii bytes for '2' and '6'

PopulateCipSend         ld de, MsgBuffer                ; cipsend buffer
                        WriteString(Cmd.CIPSEND, Cmd.CIPSENDLen);    AT+CIPSEND=
                        WriteBuffer(WordStart, WordLen) ;                n
                        WriteString(Cmd.Terminate, Cmd.TerminateLen);  cr lf

PopulateServerRequest   ld de, Buffer                   ; actual request for server
                        WriteBuffer(RequestBufAddr, RequestLen) ;
                        WriteString(Cmd.Terminate, Cmd.TerminateLen); )
SendRequest:
                        ESPSendBuffer(MsgBuffer)        ; >>> send CIPSEND string to ESP
                        call ESPReceiveWaitOK           ;
                        ErrorIfCarry(Err.ESPComms5)     ; Raise wifi error if no response
                        call ESPReceiveWaitPrompt       ;
                        ErrorIfCarry(Err.ESPComms6)     ; Raise wifi error if no prompt
                        ESPSendBufferLen(Buffer, RequestLenAddr); >>> send request string to server
                        ErrorIfCarry(Err.ESPConn3)      ; Raise connection error

ReceiveResponse:
                        call ESPReceiveBuffer           ;
                        call ParseIPDPacket             ;
                        ErrorIfCarry(Err.ESPConn4)      ; Raise connection error if no IPD packet
                        ret                             ;


RequestLenAddr:         dw $0000                        ;
RequestBufAddr:         dw $0000                        ;
RequestLen              defb 0,0                        ;
ResponseStart:          dw $0000                        ;
ResponseLen:            dw $0000                        ;
Prescaler:              ds 3                            ;
MsgBuffer:              ds 256                          ;
MsgBufferLen            equ $-MsgBuffer                 ;

