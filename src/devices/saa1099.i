; internal philips saa1099 sound chip

;-------------------------------------------------------------------------------

include "../ports/saa1099.i"

;-------------------------------------------------------------------------------

properties.saa1099:

    defw @init
    defb @init.len
    defw @out
    defb @out.len
    defw @buffer.init
    defb @buffer.init.len
    defw port.sound.address         ; bc
    defb saa.register.amplitude_5   ; e
    defb saa.register.amplitude_2   ; d
    defw @timing.ram
    defw @timing.megabyte
    defb 3                          ; bits per channel

;-------------------------------------------------------------------------------

@init:
;   ld a,%10001000    ;A=silence value ???
    ld bc,port.sound.address
;   ld de,32 * 256 + 31
;   xor a
;bp.res.saa:
;   out (c),e
;   out (port.sound.data),a
;   dec e
;   dec d
;   jr nz,bp.res.saa

if 1 > 0

    ; !!! size too large for jr

    ld e,saa.register.sound_enable
    out (c),e
    dec b
    ld e,saa.se.channels.enabled
    out (c),e

    ld e,saa.envelope.enabled | saa.envelope.mode.maximum
    ld d,saa.register.envelope_generator_0
    inc b
    out (c),d
    dec b
    out (c),e

    ld d,saa.register.envelope_generator_1
    inc b
    out (c),d
    dec b
    out (c),e

else

    ld hl,bp.saa.init
    ld b,6
    otir

endif

    @init.len: equ $ - @init

;-------------------------------------------------------------------------------

@out:

    out (c),e       ; 16   2
    outi            ; 24   2
    inc b           ;  4   1
    out (c),d       ; 16   2
    outi            ; 24   2
    inc b           ;  4   1  = 26

    @out.len: equ $ - @out

;-------------------------------------------------------------------------------

@buffer.init:
    ld e,4                      ; 2 buffers, 2 channels per buffer
    ld hl,bp.audio_buffer.1
@buffers:
    ld b,8                      ; 2^(bits-1)
    ld a,0
@loop.2:
    ld c,b
    ld b,26
@loop.1:
    ld (hl),a
    inc hl
    djnz @-loop.1
    add %00010001
    ld b,c
    djnz @-loop.2

    dec e
    jr nz,@-buffers

@buffer.init.len: equ $ - @buffer.init

;-------------------------------------------------------------------------------

; timing tables for sound devices - (130 bytes)
; last byte in table is delay during line interrupt to get to 1.5
;
; 208 bytes per frame, one byte per 1.5 line -> 312 lines of which 192 screen
; and 120 in border area -> 80 bytes in border area
;
; border area is uncontended, so more instructions can be executed than during
; screen area:

; if -1 reached -> error, double 0 indicates something went wrong in timing

;-------------------------------------------------------------------------------

@timing.ram:

    ; bpio ff if peek(pc+2)==51 and dline >= 0n258

    defb      29    ; 259:006 260:198
    defb 119,119    ; 262:006 263:198
    defb 119,119    ; 265:006 266:198
    defb 119,119    ; 268:006 269:198
    defb 119,119    ; 271:006 272:198
    defb 119,119    ; 274:006 275:198
    defb 119,119    ; 277:006 278:198
    defb 119,119    ; 280:006 281:198
    defb 119,119    ; 283:006 284:198
    defb 119,119    ; 286:006 287:198
    defb 119,119    ; 289:006 290:198
    defb 119,119    ; 292:006 293:198
    defb 119,119    ; 295:006 296:198
    defb 119,119    ; 298:006 299:198
    defb 119,119    ; 301:006 302:198
    defb 119,119    ; 304:006 305:198
    defb 119,119    ; 307:006 308:198
    defb 119,119    ; 310:006 311:198
    defb 119,119    ; 001:006 002:198
    defb 119,119    ; 004:006 005:198
    defb 119,119    ; 007:006 008:198
    defb 119,119    ; 010:006 011:198
    defb 119,119    ; 013:006 014:198
    defb 119,119    ; 016:006 017:198
    defb 119,119    ; 019:006 020:198
    defb 119,119    ; 022:006 023:198
    defb 119,119    ; 025:006 026:198
    defb 119,121    ; 028:006 029:198
    defb 119,119    ; 031:006 032:198
    defb 119,119    ; 034:006 035:198
    defb 119,119    ; 037:006 038:198
    defb 119,119    ; 040:006 041:198
    defb 119,119    ; 043:006 044:198
    defb 119,119    ; 046:006 047:198
    defb 119,119    ; 049:006 050:198
    defb 119,119    ; 052:006 053:198
    defb 119,119    ; 055:006 056:198
    defb 119,119    ; 058:006 059:198
    defb 119,119    ; 061:006 062:198
    defb 119,119    ; 064:006 065:198
    defb 119,106    ; 067:006 068:198
    defb  80, 77    ; 070:006 071:198
    defb  77, 75    ; 073:006 074:198
    defb  76, 75    ; 076:006 077:198
    defb  76, 74    ; 079:006 080:198
    defb  77, 74    ; 082:006 083:198
    defb  77, 74    ; 085:006 086:190 ! 75 -> 086:206
    defb  80, 75    ; 088:006 089:198
    defb  79, 77    ; 091:006 092:198
    defb  78, 77    ; 094:006 095:198
    defb  77, 76    ; 097:006 098:198
    defb  77, 75    ; 100:006 101:198
    defb  77, 74    ; 103:006 104:198
    defb  78, 74    ; 106:006 107:190 ! 75 -> 107:206
    defb  79, 75    ; 109:006 110:198
    defb  80, 77    ; 112:006 113:198
    defb  79, 77    ; 115:006 116:198
    defb  77, 75    ; 118:006 119:198
    defb  77, 74    ; 121:006 122:198
    defb  78, 73    ; 124:006 125:198
    defb  79, 75    ; 127:006 128:198
    defb  80, 77    ; 130:006 131:198
    defb  79, 76    ; 133:006 134:190 ! 77 -> 134:206
    defb 0

    defb -1,-1,-1

    defb 78 ; delay for line interrupt

    assert $ - @timing.ram == 130

;-------------------------------------------------------------------------------

@timing.megabyte:

    ; bpio ff if peek(pc+2)==51 and dline >= 0n258

    defb      30    ; 259:006 260:198
    defb 119,118    ; 262:006 263:198
    defb 119,118    ; 265:006 266:198
    defb 119,118    ; 268:006 269:198
    defb 119,118    ; 271:006 272:198
    defb 119,118    ; 274:006 275:198
    defb 119,118    ; 277:006 278:198
    defb 119,118    ; 280:006 281:198
    defb 119,118    ; 283:006 284:198
    defb 119,118    ; 286:006 287:198
    defb 119,118    ; 289:006 290:198
    defb 119,118    ; 292:006 293:198
    defb 119,118    ; 295:006 296:198
    defb 119,120    ; 298:006 299:198
    defb 119,118    ; 301:006 302:198
    defb 119,118    ; 304:006 305:198
    defb 119,118    ; 307:006 308:198
    defb 119,118    ; 310:006 311:198
    defb 119,118    ; 001:006 002:198
    defb 119,118    ; 004:006 005:198
    defb 119,118    ; 007:006 008:198
    defb 119,118    ; 010:006 011:198
    defb 119,118    ; 013:006 014:198
    defb 119,118    ; 016:006 017:198
    defb 119,118    ; 019:006 020:198
    defb 119,118    ; 022:006 023:198
    defb 119,118    ; 025:006 026:198
    defb 119,118    ; 028:006 029:198
    defb 119,121    ; 031:006 032:198
    defb 119,118    ; 034:006 035:198
    defb 119,118    ; 037:006 038:198
    defb 119,118    ; 040:006 041:198
    defb 119,118    ; 043:006 044:198
    defb 119,118    ; 046:006 047:198
    defb 119,118    ; 049:006 050:198
    defb 119,118    ; 052:006 053:198
    defb 119,118    ; 055:006 056:198
    defb 119,118    ; 058:006 059:198
    defb 119,118    ; 061:006 062:198
    defb 119,118    ; 064:006 065:198
    defb 119,105    ; 067:006 068:198
    defb  83, 78    ; 070:006 071:198
    defb  77, 77    ; 073:006 074:190 ! 78 -> 074:206
    defb  82, 76    ; 076:006 077:198
    defb  83, 82    ; 079:006 080:198
    defb  79, 78    ; 082:006 083:198
    defb  79, 77    ; 085:006 086:198
    defb  82, 78    ; 088:006 089:198
    defb  83, 80    ; 091:006 092:198
    defb  80, 78    ; 094:006 095:198
    defb  79, 76    ; 097:006 098:198
    defb  81, 78    ; 100:006 101:198
    defb  83, 80    ; 103:006 104:198
    defb  79, 78    ; 106:006 107:198
    defb  79, 76    ; 109:006 110:198
    defb  82, 78    ; 112:006 113:198
    defb  83, 80    ; 115:006 116:198
    defb  79, 78    ; 118:006 119:198
    defb  79, 76    ; 121:006 122:198
    defb  82, 78    ; 124:006 125:198
    defb  83, 80    ; 127:006 128:198
    defb  80, 78    ; 130:006 131:198
    defb  79, 77    ; 133:006 134:198
    defb 0

    defb -1,-1,-1

    defb 78         ; t-states to 1.5 from line interrupt

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------
