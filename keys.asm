
; $028e.  5  KEY_SCAN   {001} the keyboard scanning subroutine
; On returning from $028e KEY_SCAN the DE register and the Zero flag indicate
; which keys are being pressed.
;
; . The Zero flag is reset if pressing more than two keys, or pressing two
;  keys and neither is a shift key; DE identifies two of the keys.
; . The Zero flag is set otherwise, and DE identifies the keys.
; . If pressing just the two shift keys then DE = $2718.
; . If pressing one shift key and one other key, then D identifies the shift
;   key and E identifies the other key.
; . If pressing any one key, then D=$ff and E identifies the key.
; . If pressing no key, then DE=$ffff.
;
; The key codes returned by KEY_SCAN are shown below.
;
; KEY_SCAN key codes: hex, decimal, binary
; ? hh dd bbbbbbbb   ? hh dd bbbbbbbb   ? hh dd bbbbbbbb   ? hh dd bbbbbbbb
; 1 24 36 00100011   Q 25 37 00100101   A 26 38 00100110  CS 27 39 00100111
; 2 1c 28 00011100   W 1d 29 00011101   S 1e 30 00011110   Z 1f 31 00011111
; 3 14 20 00010100   E 15 21 00010101   D 16 22 00010110   X 17 23 00010111
; 4 0c 12 00001100   R 0d 13 00001101   F 0e 14 00001110   C 0f 15 00001111
; 5 04  4 00000100   T 05  5 00000101   G 06  6 00000110   V 07  7 00000111
; 6 03  3 00000011   Y 02  2 00000010   H 01  1 00000001   B 00  0 00000000
; 7 0b 11 00001011   U 0a 10 00001010   J 09  9 00001001   N 08  8 00001000
; 8 13 19 00010011   I 12 18 00010010   K 11 17 00010001   M 10 16 00010000
; 9 1b 27 00011011   O 1a 26 00011010   L 19 25 00011001  SS 18 24 00011000
; 0 23 35 00100011   P 22 34 00100010  EN 21 33 00100001  SP 20 32 00100000
;
; SS + 0 = delete    SS+SP = break

;
; handle user id input
;

HandleUserIdInput       ld b, 20                        ; collect 20 chars for userId
                        ld c, $31                       ; used to debounce ('1' from menu press)
                        ld hl, USER_ID_BUF              ; which buffer to store chars
InputLoop               PrintLine(3,8,USER_ID_BUF, 5)   ; show current buffer contents
                        PrintLine(3+5,8,HYPHEN,1)       ;
                        PrintLine(3+6,8,USER_ID_BUF+5, 5);
                        PrintLine(3+6+5,8,HYPHEN,1)     ;
                        PrintLine(3+6+6,8,USER_ID_BUF+5+5, 5);
                        PrintLine(3+6+6+5,8,HYPHEN,1)   ;
                        PrintLine(3+6+6+6,8,USER_ID_BUF+5+5+5, 5);
                        push hl                         ;
                        push bc                         ;
                        call ROM_KEY_SCAN               ; d=modifier e=keycode or $ff
                        pop bc                          ;
                        pop hl                          ;
                        ld a,d                          ; do we have a key modifier? (ss CS etc)
                        cp $ff                          ;
                        jp nz, ShiftCheck               ; yes
                        jp NoShiftPressed               ; no

ShiftCheck              cp $27                          ; $27=CS - check if caps shift is pressed (CS + 0 = delete)
                        jp nz, NoShiftPressed           ; no
                        ld a,e                          ; yes. check 2nd char
                        cp $23                          ; $23=0 - is 2nd char 0 key? (CS + 0 = delete)
                        jp z, Delete                    ; yes
                        cp $20                          ; no. is 2nd char SPACE? (CS+SP=break)
                        scf                             ; set carry so we can tell it was a break
                        ret z                           ; yes back to menu
                        jp nz, InputLoop                ; no. collect another char

Delete                  ld a,b                          ; let's see if we've got any chars to delete
                        cp 20                           ;
                        jp z, InputLoop                 ; no. collect another char
                        ld a,e                          ; yes. obtain keypress again
                        cp c                            ; is this key same as last keypress?
                        jp z, InputLoop                 ; yes. = debounce
                        ld c, a                         ; no. store key for next debounce check
                        dec hl                          ; and reposition buffer pointer
                        ld (hl), ' '                    ; blank current char
                        inc b                           ; and collected char count
                        jp InputLoop                    ; collect another char

NoShiftPressed          ld a,e                          ; do we have a key pressed?
                        cp $ff                          ;
                        jp z, NoKeyPressed              ; no
                        cp $21                          ; enter?
                        jr nz, NotEnter                 ; no
                        ld a, b                         ; yes. see if we've got enough chars
                        cp 0                            ; got all our chars?
                        ret z                           ; we've got all our chars and enter was pressed
NotEnter                ld a, b                         ; can we allow more chars?
                        cp 0                            ; got all our chars?
                        ld a,e                          ;   place key into a again
                        jp z, NoKeyPressed              ; not allowed any more chars until delete is pressed
                        push bc                         ; we have a keypress without shift
                        push hl                         ;
                        ld b, 0                         ;
                        ld c, a                         ;  bc = keycode value
                        ld hl, ROM_KEYTABLE             ;  hl = code to ascii lookup table
                        add hl, bc                      ;  find ascii given keycode
;                        add bc, $20                     ; find lower case ascii
;                        add hl, bc                      ;
                        ld a, (hl)                      ; ascii
                        pop hl                          ;
                        pop bc                          ;
                        cp $20                          ; check if >= 32 (ascii space)
                        jp c,InputLoop                  ; no, ignore
                        cp $7f                          ; check if <= 126 (ascii ~)
                        jp nc,InputLoop                 ; no, ignore
                        cp c                            ; does key = last keypress?
                        jp z, InputLoop                 ; yes - debounce
                        ld c, a                         ; no - store char in c for next check
                        ld (hl),a                       ; no - store char in buffer
                        inc hl                          ;
                        dec b                           ; one less char to collect
                        jp InputLoop                    ; no

NoKeyPressed            cp c                            ; is current keycode same as last?
                        jp z, InputLoop                 ; yes - just loop again
                        ld c, a                         ; no, update c to show change
                        jp InputLoop                    ;
