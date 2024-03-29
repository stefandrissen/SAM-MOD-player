; dac on printer port 1 or 2
;
; timing also used by blue alpha

;-------------------------------------------------------------------------------

include "../ports/printer.i"

;-------------------------------------------------------------------------------

properties.dac.1:

    defw 0                      ; init
    defb 0
    defw dac.out
    defb dac.out.len
    defw dac.buffer.init
    defb dac.buffer.init.len
    defw port.printer_1.data    ; bc
    defw 0                      ; de - unused
    defw dac.timing.ram
    defw dac.timing.megabyte
    defb 6                      ; bits per channel

properties.dac.2:

    defw 0
    defb 0
    defw dac.out
    defb dac.out.len
    defw dac.buffer.init
    defb dac.buffer.init.len
    defw port.printer_2.data
    defw 0
    defw dac.timing.ram
    defw dac.timing.megabyte
    defb 6                      ; bits per channel

;-------------------------------------------------------------------------------

dac.out:

    ; playback is mixing channels
    ; c1 -> c4 -> buffer / c2 -> c3 -> buffer

    ; could / should be mixed directly
    ; c1 -> c2 -> c3 -> c4 -> buffer
    ; this /could/ allow 15.6KHz playback for mono dac

    ld e,a          ;  4   1
    ld a,(hl)       ;  8   1
    inc hl          ;  8   1
    add (hl)        ;  8   1
    inc hl          ;  8   1
    out (c),a       ; 12   2
    ld a,e          ;  4   1

    dac.out.len: equ $ - dac.out

;-------------------------------------------------------------------------------

dac.buffer.init:
    ld c,4                      ; 2 buffers, 2 channels per buffer
@buffers:
    ld b,32                     ; 2^(bits-1)
    ld hl,bp.audio_buffer.1
    ld a,0
@loop.2:
    ld c,b
    ld b,6
@loop.1:
    ld (hl),a
    inc hl
    djnz@-loop.1
    inc a
    ld b,c
    djnz @-loop.2

    ld b,bp.audio_buffer.bytes - ( 32 * 6 )
@loop:
    ld (hl),a
    inc hl
    djnz @-loop

    dec c
    jr nz,@-buffers

dac.buffer.init.len: equ $ - dac.buffer.init

;-------------------------------------------------------------------------------

dac.timing.ram:

    defb      40
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,130
    defb 129,130
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,129
    defb 129,118
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  84, 88
    defb  84, 85
    defb  83, 89
    defb 0

    defb 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1  ; second 0?

    defb 93

    assert $ - dac.timing.ram == 130

;-------------------------------------------------------------------------------

dac.timing.megabyte:

    ; bpio 7f if dline >= 0n258

    defb      42    ; 259:011 260:203
    defb 129,129    ; 262:011 263:203
    defb 129,129    ; 265:011 266:203
    defb 129,129    ; 268:011 269:203
    defb 129,129    ; 271:011 272:203
    defb 129,129    ; 274:011 275:203
    defb 129,129    ; 277:011 278:203
    defb 129,129    ; 280:011 281:203
    defb 129,129    ; 283:011 284:203
    defb 129,129    ; 286:011 287:203
    defb 129,129    ; 289:011 290:203
    defb 129,129    ; 292:011 293:203
    defb 129,131    ; 295:011 296:203
    defb 129,129    ; 298:011 299:203
    defb 129,129    ; 301:011 302:203
    defb 129,129    ; 304:011 305:203
    defb 129,129    ; 307:011 308:203
    defb 129,129    ; 310:011 311:203
    defb 129,129    ; 001:011 002:203
    defb 129,129    ; 004:011 005:203
    defb 129,129    ; 007:011 008:203
    defb 129,129    ; 010:011 011:203
    defb 129,129    ; 013:011 014:203
    defb 129,129    ; 016:011 017:203
    defb 129,129    ; 019:011 020:203
    defb 129,130    ; 022:011 023:203
    defb 134,129    ; 025:011 026:203
    defb 129,129    ; 028:011 029:203
    defb 129,129    ; 031:011 032:203
    defb 129,129    ; 034:011 035:203
    defb 129,129    ; 037:011 038:203
    defb 129,129    ; 040:011 041:203
    defb 129,129    ; 043:011 044:203
    defb 129,129    ; 046:011 047:203
    defb 129,129    ; 049:011 050:203
    defb 129,129    ; 052:011 053:203
    defb 129,129    ; 055:011 056:203
    defb 129,129    ; 058:011 059:203
    defb 131,129    ; 061:011 062:203
    defb 129,129    ; 064:011 065:203
    defb 129,115    ; 067:011 068:203
    defb  86, 87    ; 070:011 071:203
    defb  92, 87    ; 073:011 074:203
    defb  90, 88    ; 076:011 077:203
    defb  86, 87    ; 079:011 080:203
    defb  92, 86    ; 082:011 083:203
    defb  90, 88    ; 085:011 086:203
    defb  85, 86    ; 088:011 089:203
    defb  90, 87    ; 091:011 092:203
    defb  89, 91    ; 094:011 095:203
    defb  87, 85    ; 097:011 098:203
    defb  90, 88    ; 100:011 101:203
    defb  86, 87    ; 103:011 104:203
    defb  92, 86    ; 106:011 107:203
    defb  90, 88    ; 109:011 110:203
    defb  85, 86    ; 112:011 113:203
    defb 0

    defb -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1

    defb 90         ; t-states to 1.5 from line interrupt

    assert $ - dac.timing.megabyte == 130

;-------------------------------------------------------------------------------
