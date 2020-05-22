;
; save
;

SaveFile                ld a, (esxDOS.DefaultDrive)     ; set drive to system with $
                        ld ix, FILE_NAME                ;
                        call esxDOS.fOpenForWrite       ; do we have an existing file?
                        jp nc, DoSave                   ; yes
DoCreate                ld a, (esxDOS.DefaultDrive)     ; no - create file
                        ld ix, FILE_NAME                ;
                        call esxDOS.fCreate             ;
                        jp nc, DoSave                   ; save if create worked
                        PrintLine(0,5,Err.FileCreate, 20) ; otherwise show error
                        ret                             ;
DoSave                  ld ix, MBOX_USER_ID                ; save userid
                        ld bc, 20                       ; with trailing 0s
                        call esxDOS.fWrite              ; write to file
                        jp nc, CloseSaved               ; if it worked, close

                        push af                         ;  else show error
                        PrintAt(0,5)                    ;
                        pop af                          ;
                        call PrintAHexNoSpace           ;
                        PrintLine(0, 4, Err.FileWrite, 20);
                        ret                             ;

CloseSaved              call esxDOS.fClose              ;
                        ret nc                          ;  close ok?
                        PrintLine(0,4,Err.FileClose,20) ;  no
                        ret                             ;
;
; load
;

LoadFile                ld a, (esxDOS.DefaultDrive)     ;
                        ld ix, FILE_NAME                ;
                        call esxDOS.fOpenForRead        ;
                        jp nc, ReadFile                 ; if open ok
                        cp esxDOS.esx_enoent            ; else was erro no such file?
                        ret z                           ; yes - we'll save one later
                        cp esxDOS.esx_enotdir           ; no, was error 'no such directory'?
                        jp z, Mkdir                     ; yes go make one
                        push af                         ; no, display error
                        PrintAt(0,10)                   ;
                        pop af                          ;
                        call PrintAHexNoSpace           ;
Fepd                    jp Fepd                         ; loop forever

Mkdir                   ld a, (esxDOS.DefaultDrive)     ;
                        ld ix, DIR_NAME                 ;
                        call esxDOS.fMkdir              ; create it
                        ret nc                          ; if it worked return
                        push af                         ; if it didn't
                        PrintAt(10,15)                  ;
                        pop af                          ;
                        call PrintAHexNoSpace           ;
                        jp Fepd                         ; loop forever

ReadFile                ld ix, FILEBUF                  ;
                        ld bc, 20                       ;
                        call esxDOS.fRead               ;
                        jp nc, CloseFile                ;
                        PrintLine(0,4,Err.FileRead,20)  ;
                        push af                         ;
                        PrintAt(10,15)                  ;
                        pop af                          ;
                        call PrintAHexNoSpace           ;
                        jp Fepd                         ; loop forever

CloseFile               call esxDOS.fClose              ;
                        jp nc, ProcessFileBuf           ;
                        PrintLine(0,4,Err.FileClose,20) ;
                        push af                         ;
                        PrintAt(10,15)                  ;
                        pop af                          ;
                        call PrintAHexNoSpace           ;
                        jp Fepd                         ; loop forever

ProcessFileBuf          ld hl, FILEBUF                  ;
                        ld de, MBOX_USER_ID                ;
                        ld bc, 20                       ;
                        ldir                            ;
                        ld hl, CONNECTED                ;   set not connected by default
                        ld (hl), 0                      ;
                        call RegisterUserId             ;
                        ld a, (CONNECTED)               ;  did we get connected?
                        cp 1                            ;
                        call z, HandleCount             ;  yay
                        ret                             ;
