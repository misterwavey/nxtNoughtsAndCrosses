; macros.asm

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


Border                  macro(Colour)                   ;
        if Colour=0                                     ;
                          xor a                         ;
        else                                            ;
                          ld a, Colour                  ;
        endif                                           ;
                          out (ULA_PORT), a             ;
        if Colour=0                                     ;
                          xor a                         ;
        else                                            ;
                          ld a, Colour*8                ;
        endif                                           ;
                          ld (23624), a                 ;
                          mend                          ;

Freeze                  macro(Colour1, Colour2)         ;
Loop:                     Border(Colour1)               ;
                          Border(Colour2)               ;
                          jr Loop                       ;
                          mend                          ;


CpHL                    macro(Register)                 ;
                          or a                          ;
                          sbc hl, Register              ;
                          add hl, Register              ;
                          mend                          ;

PrintMsg                macro(Address)                  ;
                          ld hl, Address                ;
                          call PrintRst16               ;
                          mend                          ;

PrintBuffer             macro(StartAddr, LengthAddr)    ;
                          ld hl, (StartAddr)            ;
                          ld bc, (LengthAddr)           ;
                          call PrintBufferProc          ;
                          mend                          ;

PrintMsgRetToBASIC      macro(MsgAddr)                  ;
                          ld hl, MsgAddr                ;
                          call PrintRst16               ;
                          jp Return.ToBasic             ;
                          mend                          ;

ErrorIfCarry            macro(ErrAddr)                  ;
                          jp nc, Continue               ;
                          ld hl, ErrAddr                ;
                          jp CheckESPTimeout.HandleError;
Continue:
                          mend                          ;

ErrorIfNonZero          macro(ErrAddr)                  ;
                          jp z, Continue                ;
                          ld hl, ErrAddr                ;
                          jp CheckESPTimeout.HandleError;
Continue:
                          mend                          ;

WriteString             macro(StringAddr, StringLen)    ;
                          ld hl, StringAddr             ;
                          ld bc, StringLen              ;
                          ldir                          ;
                          mend                          ;

WriteByte               macro(byte)                     ;
                          ld a, byte                    ;
                          ex de, hl                     ;
                          ld (hl), a                    ;
                          ex de, hl                     ;
                          inc de                        ;
                          mend                          ;

WriteBuffer             macro(CommandAddrAddr, CommandLenAddr);
                          ld hl, (CommandAddrAddr)      ;
                          ld bc, (CommandLenAddr)       ;
                          ldir                          ;
                          mend                          ;

AddHL                   macro (WordValue)               ; Next-only opcode
                          noflow                        ;
                          db $ED, $34                   ;
                          dw WordValue                  ;
                          mend                          ;

MirrorA                 macro()                         ;
                          noflow                        ;
                          db $ED, $24                   ;
                          mend                          ;

FillLDIR                macro(SourceAddr, Size, Value)  ;
                          ld a, Value                   ;
                          ld hl, SourceAddr             ;
                          ld (hl), a                    ;
                          ld de, SourceAddr+1           ;
                          ld bc, Size-1                 ;
                          ldir                          ;
                          mend                          ;

NextRegRead             macro(Register)                 ;
                          ld bc, Port.NextReg           ; Port.NextReg = $243B
                          ld a, Register                ;
                          call NextRegReadProc          ;
                          mend                          ;

DecodeDecimal           macro(Buffer, DigitCount)       ;
                          ld a, (DigitCount)            ;
                          ld hl, Buffer                 ;
                          dec hl                        ;
                          ld (DecodeDecimalProc.DecimalBuffer), hl;
                          ld b, a                       ;
                          call DecodeDecimalProc        ;
                          mend                          ;

Rst8                    macro(Command)                  ;
                          rst $08                       ;
                          noflow                        ;
                          db Command                    ;
                          mend                          ;

CopyLDIR                macro(SourceAddr, DestAddr, Size);
                          ld hl, SourceAddr             ;
                          ld de, DestAddr               ;
                          ld bc, Size                   ;
                          ldir                          ;
                          mend                          ;

WaitFrames              macro(FrameCount)               ;
                          ld b, FrameCount              ;
                          call WaitFramesProc           ;
                          mend                          ;

PrintLine               macro(X, Y, string, len)        ;
                          push de                       ;
                          push bc                       ;
                          PrintAt(X,Y)                  ;
                          ld de, string                 ; address of string
                          ld bc, len                    ; length of string to print
                          ei                            ;
                          call ROM_PR_STRING            ;
                          di                            ;
                          pop bc                        ;
                          pop de                        ;
                          mend                          ;

PrintLineLenVar         macro(X, Y, string, len)        ;
                          push de                       ;
                          push bc                       ;
                          PrintAt(X,Y)                  ;
                          ld de, string                 ; address of string
                          ld bc, (len)                  ; length of string to print
                          ei                            ;
                          call ROM_PR_STRING            ;
                          di                            ;
                          pop bc                        ;
                          pop de                        ;
                          mend                          ;

OpenOutputChannel       macro(Channel)                  ; Semantic macro to call a 48K ROM routine
                          ld a, Channel                 ; 2 = upper screen
                          ei                            ;
                          call ROM_CHAN_OPEN            ;
                          di                            ;
                          mend                          ;

PrintChar               macro(Char)                     ; Semantic macro to call a 48K ROM routine
                          ld a, Char                    ;
                          rst $10                       ; ROM 0010: THE 'PRINT A CHARACTER' RESTART
                          mend                          ;

PrintAt                 macro(X, Y)                     ; Semantic macro to call a 48K ROM routine
                          PrintChar(22)                 ;
                          PrintChar(Y)                  ; X and Y are reversed order, i.e.
                          PrintChar(X)                  ; PRINT AT Y, X
                          mend                          ;

Border                  macro(Colour)                   ; Semantic macro to call a 48K ROM routine
        if Colour=0                                     ;
                          xor a                         ;
        else                                            ;
                          ld a, Colour                  ;
        endif                                           ;
                          out ($FE), a                  ; Change border colour immediately
        if Colour<>0                                    ;
                          ld a, Colour*8                ;
        endif                                           ;
                          ld (23624), a                 ; Makes the ROM respect the new border colour
                          mend                          ;

esxDOS                  macro(Command)                  ; Semantic macro to call an esxDOS routine
                          rst $08                       ; rst $08 is the instruction to call an esxDOS API function.
                          noflow                        ; Zeus normally warns when data might be executed, suppress.
                          db Command                    ; For esxDOS API calls, the data byte is the command number.
                          mend                          ;

M_P3DOS                 macro(Command, Bank)            ; Semantic macro to call an NextZXOS routine via the esxDOS API
                          exx                           ; M_P3DOS: See NextZXOS_API.pdf page 37
                          ld de, Command                ; DE=+3DOS/IDEDOS/NextZXOS call ID
                          ld c, Bank                    ; C=RAM bank that needs to be paged (usually 7, but 0 for some calls)
                          esxDOS($94)                   ; esxDOS API: M_P3DOS ($94)
                          mend                          ;


