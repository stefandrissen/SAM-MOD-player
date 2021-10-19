; internal philips saa1099 sound chip

;-------------------------------------------------------------------------------

include "../ports/saa1099.i"

;-------------------------------------------------------------------------------

properties.saa1099:

    defw @init,@init.len
    defw @out, @out.len
    defw port.sound.address
    defb saa.register.amplitude_5
    defb saa.register.amplitude_2
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

    defb  28,119,119,119,119,119,119,119    ;   0
    defb 119,119,119,119,119,119,119,119    ;   8
    defb 119,119,119,119,119,119,119,119    ;  16
    defb 119,119,119,119,119,119,119,119    ;  24
    defb 119,119,119,119,119,119,119,119    ;  32
    defb 119,119,119,119,119,119,119,119    ;  40
    defb 119,119,119,119,119,119,121,119    ;  48
    defb 119,119,119,119,119,119,119,119    ;  56
    defb 119,119,119,119,119,119,119,119    ;  64
    defb 119,119,119,119,119,119,119,119    ;  72
    defb 107, 76, 81, 75, 80, 74, 79, 73    ;  80
    defb  78, 75, 77, 75, 79, 76, 81, 75    ;  88
    defb  80, 74, 79, 73, 78, 75, 77, 75    ;  96
    defb  79, 76, 81, 75, 80, 74, 79, 73    ; 104
    defb  78, 75, 77, 75, 79, 76, 81, 75    ; 112
    defb  80, 74, 79, 73, 80, 89, 95        ; 120
    defb 0,-1

    defb 78 ; delay for line interrupt

    assert $ - @timing.ram == 130

;-------------------------------------------------------------------------------

@timing.megabyte:

    ; bpio ff w if b==0 and dline >= 0n258

    defb      28    ; 259:006 260:190
    defb 121,117    ; 262:006 263:190
    defb 121,117    ; 265:006 266:190
    defb 121,117    ; 268:006 269:190
    defb 121,117    ; 271:006 272:190
    defb 121,117    ; 274:006 275:190
    defb 121,117    ; 277:006 278:190
    defb 121,117    ; 280:006 281:190
    defb 121,117    ; 283:006 284:190
    defb 121,117    ; 286:006 287:190
    defb 121,117    ; 289:006 290:190
    defb 121,117    ; 292:006 293:190
    defb 121,117    ; 295:006 296:190
    defb 121,118    ; 298:006 299:190
    defb 121,117    ; 301:006 302:190
    defb 121,117    ; 304:006 305:190
    defb 121,117    ; 307:006 308:190
    defb 121,117    ; 310:006 311:190
    defb 121,117    ; 001:006 002:190
    defb 121,117    ; 004:006 005:190
    defb 121,117    ; 007:006 008:190
    defb 121,117    ; 010:006 011:190
    defb 121,117    ; 013:006 014:190
    defb 121,117    ; 016:006 017:190
    defb 121,117    ; 019:006 020:190
    defb 121,117    ; 022:006 023:190
    defb 121,117    ; 025:006 026:190
    defb 121,117    ; 028:006 029:190
    defb 122,118    ; 031:006 032:190
    defb 121,117    ; 034:006 035:190
    defb 121,117    ; 037:006 038:190
    defb 121,117    ; 040:006 041:190
    defb 121,117    ; 043:006 044:190
    defb 121,117    ; 046:006 047:190
    defb 121,117    ; 049:006 050:190
    defb 121,117    ; 052:006 053:190
    defb 121,117    ; 055:006 056:190
    defb 121,117    ; 058:006 059:190
    defb 121,117    ; 061:006 062:190
    defb 121,117    ; 064:006 065:190
    defb 121,107    ; 067:006 068:190
    defb  83, 75    ; 070:006 071:190
    defb  83, 80    ; 073:006 074:190
    defb  84, 77    ; 076:006 077:190
    defb  79, 76    ; 079:006 080:190
    defb  83, 77    ; 082:006 083:190
    defb  83, 79    ; 085:006 086:190
    defb  80, 77    ; 088:006 089:190
    defb  81, 75    ; 091:006 092:190
    defb  82, 77    ; 094:006 095:190
    defb  83, 79    ; 097:006 098:190
    defb  79, 77    ; 100:006 101:190
    defb  81, 75    ; 103:006 104:190
    defb  83, 80    ; 106:006 107:190
    defb  85, 77    ; 109:006 110:190
    defb  78, 76    ; 112:006 113:190
    defb  81, 75    ; 115:006 116:190
    defb  83, 80    ; 118:006 119:190
    defb  85, 77    ; 121:006 122:190
    defb  78, 77    ; 124:006 125:190
    defb  81, 75    ; 127:006 128:190
    defb  82, 77    ; 130:006 131:190
    defb  83, 75    ; 133:006 134:190
    defb 0

    defb -1,-1,-1

    defb 78         ; t-states to 1.5 from line interrupt

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------
