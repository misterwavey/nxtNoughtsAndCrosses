; msg.asm

;  Copyright 2019-2020 Robin Verhagen-Guest
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

Msg                     proc                            ;
InitESP:                db "Initialising WiFi...", CR, 0;

InitDone:               db "Initialised", CR, 0         ;
Connect1:               db "Connecting to ", 0          ;
Connect2:               db "...", CR, 0                 ;
Connected:              db "Connected", CR, 0           ;
Sending1:               db "Sending ", 0                ;
Sending2:               db " chars...", CR, 0           ;
Received:               db "Received ", 0               ;
SetBaud1:               db "Using 115200 baud, ", 0     ;
SetBaud2:               db " timings", CR, 0            ;
pend

Err                     proc                            ;
                        ;  "<-Longest valid erro>", 'r'|128
HostLen:                db "1 HOSTNAME too lon", 'g'|128;
ESPComms1:              db "2 WiFi comms erro", 'r'|128 ;
ESPComms2:              db "3 WiFi comms erro", 'r'|128 ;
ESPComms3:              db "4 WiFi comms erro", 'r'|128 ;
ESPComms4:              db "5 WiFi comms erro", 'r'|128 ;
ESPComms5:              db "6 WiFi comms erro", 'r'|128 ;
ESPComms6:              db "7 WiFi comms erro", 'r'|128 ;
ESPConn1:               db "8 Server conn erro", 'r'|128;
ESPConn2:               db "9 Server conn erro", 'r'|128;
ESPConn3:               db "A Server conn erro", 'r'|128;
ESPConn4:               db "B Server conn erro", 'r'|128;
ZoneLen:                db "C ZONE too lon", 'g'|128    ;
Break:                  db "D BREAK - CONT repeat", 's'|128;
NotNext:                db "E Next require", 'd'|128    ;
ESPTimeout:             db "F WiFi/server timeou", 't'|128;
DirCreate:              db "G Error creating di", 'r'|128;
FileCreate:             db "H Error creating fil", 'e'|128;
FileWrite:              db "I Error writing fil", 'e'|128 ;
FileRead:               db "J Error reading fil", 'e'|128 ;
FileClose:              db "K Error closing fil", 'e'|128 ;
BadResp21:              db "c Invalid respons", 'e'|128 ;
CoreMin:                db "Core 3.00.04 require", 'd'|128;
pend

Timings:                proc Table:                     ;
                        ;   Text   Index  Notes
                        db "VGA0", 0                    ; 0  Timing 0
                        db "VGA1", 0                    ; 1  Timing 1
                        db "VGA2", 0                    ; 2  Timing 2
                        db "VGA3", 0                    ; 3  Timing 3
                        db "VGA4", 0                    ; 4  Timing 4
                        db "VGA5", 0                    ; 5  Timing 5
                        db "VGA6", 0                    ; 6  Timing 6
                        db "HDMI", 0                    ; 7  Timing 7
pend

PrintRst16              proc                            ;
                        ei                              ;
Loop:                   ld a, (hl)                      ;
                        inc hl                          ;
                        or a                            ;
                        jr z, Return                    ;
                        rst 16                          ;
                        jr Loop                         ;
Return:                 di                              ;
                        ret                             ;
pend

PrintRst16Error         proc                            ;
                        ei                              ;
Loop:                   ld a, (hl)                      ;
                        ld b, a                         ;
                        and %1 0000000                  ;
                        ld a, b                         ;
                        jp nz, LastChar                 ;
                        inc hl                          ;
                        rst 16                          ;
                        jr Loop                         ;
Return:                 di                              ;
                        ret                             ;
LastChar                and %0 1111111                  ;
                        rst 16                          ;
                        jr Return                       ;
pend


PrintBufferProc         proc                            ;
                        ld de, MsgBuffer                ;
                        ldir                            ;
                        xor a                           ;
                        ld (de), a                      ;
                        inc de                          ;
                        ld hl, MsgBuffer                ;
                        call PrintRst16                 ;
                        ret                             ;
pend

PrintBufferLen          proc                            ;
                        ld a, (hl)                      ;
                        ei                              ;
                        rst 16                          ;
                        di                              ;
                        inc hl                          ;
                        dec bc                          ;
                        ld a, b                         ;
                        or c                            ;
                        jr nz, PrintBufferLen           ;
                        ret                             ;
pend

PrintAHexNoSpace        proc                            ;
                        ld b, a                         ;
                        and $F0                         ;
                        swapnib                         ;
                        call Print                      ;
                        ld a, b                         ;
                        and $0F                         ;
                        call Print                      ;
                        ret                             ;
Print:                  cp 10                           ;
                        ld c, '0'                       ;
                        jr c, Add                       ;
                        ld c, 'A'-10                    ;
Add:                    add a, c                        ;
                        rst 16                          ;
                        ret                             ;
pend

