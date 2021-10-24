;quazar surround soundcard

;-------------------------------------------------------------------------------

properties.quazar:

    defw @init
    defb @init.len
    defw @out
    defb @out.len
    defw @buffer.init
    defb @buffer.init.len
    defw 0x06d0                 ; bc
    defb 0x06                   ; e - +1 for OUTI
    defb 0                      ; d - unused
    defw @timing.ram
    defw @timing.megabyte
    defb 8                      ; bits per channel

;-------------------------------------------------------------------------------

@init:

    ld bc,0x06d0
    in a,(c)            ; mode 1

    ld a,%10000000
    dec b
    out (c),a           ; rear right
    dec b
    out (c),a           ; rear left
    dec b
    out (c),a           ; front right
    dec b
    out (c),a           ; front left

    @init.len: equ $ - @init

;-------------------------------------------------------------------------------

@out:

    ld b,e          ;  4   1
    outi            ; 20   2
    outi            ; 20   2
    outi            ; 20   2
    outi            ; 20   2  = 21

    @out.len: equ $ - @out

;-------------------------------------------------------------------------------

@buffer.init:
    ld c,8                      ; 2 buffers, 4 channels per buffer
    ld hl,bp.audio_buffer.1
@buffers:
    ld b,128                    ; 2^(bits-1)
    ld a,0
@loop:
    ld (hl),a
    inc hl
    inc a
    djnz @-loop

    ld b,bp.audio_buffer.bytes - 128
@loop:
    ld (hl),a
    inc hl
    djnz @-loop

    dec c
    jr nz,@-buffers

@buffer.init.len: equ $ - @buffer.init

;-------------------------------------------------------------------------------

@timing.ram:

    defb      32
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 123,122
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,122
    defb 121,121
    defb 121,109
    defb  81, 78
    defb  81, 80
    defb  80, 81
    defb  79, 80
    defb  80, 78
    defb  81, 80
    defb  80, 81
    defb  79, 80
    defb  80, 78
    defb  81, 80
    defb  80, 81
    defb  79, 80
    defb  80, 78
    defb  81, 80
    defb  80, 81
    defb  79, 93
    defb   0

    defb 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1

    defb 83

    assert $ - @timing.ram == 130

;-------------------------------------------------------------------------------

@timing.megabyte:

    ; bpio 4d0 if dline >= 0n258

    defb      34    ; 259:006 260:198
    defb 121,121    ; 262:006 263:198
    defb 121,121    ; 265:006 266:198
    defb 121,121    ; 268:006 269:198
    defb 121,121    ; 271:006 272:198
    defb 121,121    ; 274:006 275:198
    defb 121,121    ; 277:006 278:198
    defb 121,121    ; 280:006 281:198
    defb 121,121    ; 283:006 284:198
    defb 121,121    ; 286:006 287:198
    defb 121,121    ; 289:006 290:198
    defb 121,121    ; 292:006 293:198
    defb 121,121    ; 295:006 296:198
    defb 124,121    ; 298:006 299:198
    defb 121,121    ; 301:006 302:198
    defb 121,121    ; 304:006 305:198
    defb 121,121    ; 307:006 308:198
    defb 121,121    ; 310:006 311:198
    defb 121,121    ; 001:006 002:198
    defb 121,121    ; 004:006 005:198
    defb 121,121    ; 007:006 008:198
    defb 121,121    ; 010:006 011:198
    defb 121,121    ; 013:006 014:198
    defb 121,121    ; 016:006 017:198
    defb 121,121    ; 019:006 020:198
    defb 121,121    ; 022:006 023:198
    defb 126,121    ; 025:006 026:198
    defb 121,121    ; 028:006 029:198
    defb 121,121    ; 031:006 032:198
    defb 121,121    ; 034:006 035:198
    defb 121,121    ; 037:006 038:198
    defb 121,121    ; 040:006 041:198
    defb 121,121    ; 043:006 044:198
    defb 121,121    ; 046:006 047:198
    defb 121,121    ; 049:006 050:198
    defb 121,121    ; 052:006 053:198
    defb 121,121    ; 055:006 056:198
    defb 121,121    ; 058:006 059:198
    defb 121,123    ; 061:006 062:198
    defb 121,121    ; 064:006 065:198
    defb 121,109    ; 067:006 068:194 ! 4 t-states early, +1 -> 4 t-states late
    defb  84, 84    ; 070:006 071:194 !
    defb  84, 84    ; 073:006 074:194 !
    defb  84, 84    ; 076:006 077:194 !
    defb  84, 84    ; 079:006 080:194 !
    defb  84, 84    ; 082:006 083:194 !
    defb  84, 84    ; 085:006 086:194 !
    defb  84, 84    ; 088:006 089:194 !
    defb  84, 84    ; 091:006 092:194 !
    defb  84, 84    ; 094:006 095:194 !
    defb  84, 84    ; 097:006 098:194 !
    defb  84, 84    ; 100:006 101:194 !
    defb  84, 84    ; 103:006 104:194 !
    defb  84, 84    ; 106:006 107:194 !
    defb  84, 84    ; 109:006 110:194 !
    defb  83, 81    ; 112:006 113:194 !
    defb 0

    defb -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1

    defb 80         ; t-states to 1.5 from line interrupt

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------

