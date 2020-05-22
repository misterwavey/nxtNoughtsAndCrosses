; esxDOS.asm

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

; NOTE: File paths use the slash character (‘/’) as directory separator (UNIX style)

esxDOS                  proc                            ;

M_GETSETDRV             equ $89                         ;
F_OPEN                  equ $9a                         ;
F_CLOSE                 equ $9b                         ;
F_READ                  equ $9d                         ;
F_WRITE                 equ $9e                         ;
F_SEEK                  equ $9f                         ;
F_GET_DIR               equ $a8                         ;
F_SET_DIR               equ $a9                         ;
F_MKDIR                 equ $aa                         ;
F_SYNC                  equ $9c                         ;
F_STAT                  equ $ac                         ;

FA_READ                 equ $01                         ;
FA_WRITE                equ $02                         ;
FA_APPEND               equ $06                         ; ?
FA_CREATE               equ $08                         ;
FA_OVERWRITE            equ $0C                         ;
M_GETDATE               equ $8E                         ;

; errors

esx_ok                  equ 0                           ; 0x0
esx_eok                 equ 1                           ; 0x1
esx_nonsense            equ 2                           ; 0x2
esx_estend              equ 3                           ; 0x3
esx_ewrtype             equ 4                           ; 0x4
esx_enoent              equ 5                           ; 0x5
esx_eio                 equ 6                           ; 0x6
esx_einval              equ 7                           ; 0x7
esx_eacces              equ 8                           ; 0x8
esx_enospc              equ 9                           ; 0x9
esx_enxio               equ 10                          ; 0xa
esx_enodrv              equ 11                          ; 0xb
esx_enfile              equ 12                          ; 0xc
esx_ebadf               equ 13                          ; 0xd
esx_enodev              equ 14                          ; 0xe
esx_eoverflow           equ 15                          ; 0xf
esx_eisdir              equ 16                          ; 0x10
esx_enotdir             equ 17                          ; 0x11
esx_eexist              equ 18                          ; 0x12
esx_epath               equ 19                          ; 0x13
esx_esys                equ 20                          ; 0x14
esx_enametoolong        equ 21                          ; 0x15
esx_enocmd              equ 22                          ; 0x16
esx_einuse              equ 23                          ; 0x17
esx_erdonly             equ 24                          ; 0x18
esx_everify             equ 25                          ; 0x19
esx_eloadingko          equ 26                          ; 0x1a
esx_edirinuse           equ 27                          ; 0x1b
esx_emapramactive       equ 28                          ; 0x1c
esx_edrivebusy          equ 29                          ; 0x1d
esx_efsunknown          equ 30                          ; 0x1e
esx_edevicebusy         equ 31                          ; 0x1f

esx_seek_set            equ $00                         ; set the fileposition to BCDE
esx_seek_fwd            equ $01                         ; add BCDE to the fileposition
esx_seek_bwd            equ $02                         ; subtract BCDE from the fileposition

DefaultDrive            db '$'                          ; Because we're only opening dot commands, pre-load default as system drive
Handle                  db 255                          ;

; Function:             Open file
; In:                   HL = pointer to file name (ASCIIZ) (IX for non-dot commands)
;                       B  = open mode
;                       A  = Drive
; Out:                  A  = file handle
;                       On error: Carry set
;                         A = 5   File not found
;                         A = 7   Name error - not 8.3?
;                         A = 11  Drive not found
;
fOpenForRead:
                        ld a, (DefaultDrive)            ; get drive we're on
                        ld b, FA_READ                   ; b = open mode
                        Rst8(esxDOS.F_OPEN)             ; open read mode
                        ld (Handle), a                  ;
                        ret                             ; Returns a file handler in 'A' register.

fOpenForWrite:
                        ld a, (DefaultDrive)            ; get drive we're on
                        ld b, FA_WRITE                   ; b = open mode
                        Rst8(esxDOS.F_OPEN)             ; open read mode
                        ld (Handle), a                  ;
                        ret                             ; Returns a file handler in 'A' register.
fCreate:
                        ld b, FA_WRITE+FA_OVERWRITE        ;
                        Rst8(esxDOS.F_OPEN)             ;
                        ld (Handle), a                  ;
                        ret                             ;

fWrite:                 ld a, (Handle)                  ;
                        Rst8(esxDOS.F_WRITE)            ;
                        ret                             ;

; Function:             Read bytes from a file
; In:                   A  = file handle
;                       HL = address to load into (IX for non-dot commands)
;                       BC = number of bytes to read
; Out:                  Carry flag is set if read fails.
fRead:                  
                        ld a, (Handle)                  ; file handle
                        Rst8(esxDOS.F_READ)             ; read file
                        ret                             ;

; Function:             Close file
; In:                   A  = file handle
; Out:                  Carry flag active if error when closing
fClose:                 
                        ld a, (Handle)                  ;
                        Rst8(esxDOS.F_CLOSE)            ; close file
                        ret                             ;

fStat:                  Rst8(esxDOS.F_STAT)             ;
                        ret                             ;

fMkdir                  Rst8(esxDOS.F_MKDIR)            ; make directory
                        ret                             ;
pend

