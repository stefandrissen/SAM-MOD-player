; internal philips saa1099

;-------------------------------------------------------------------------------

include "../ports/saa1099.i"

;-------------------------------------------------------------------------------

properties.saa1099:

    defw @init,@init.len
    defw @out, @out.len
    defw port.sound.address
    defb saa.register.amplitude_5
    defb saa.register.amplitude_2
    defw @timing.contended
    defw @timing.uncontended
    defb 23 * 3 + 2
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


; lets try actual t-states 384 states per line, with a counter for when contended memory is encountered

;-------------------------------------------------------------------------------

@timing.contended:

    defb  28,119,119,119,119,119,119,119,119,119  ; 1
    defb 119,119,119,119,119,119,119,119,119,119  ; 2
    defb 119,119,119,119,119,119,119,119,119,119  ; 3
    defb 119,119,119,119,119,119,119,119,119,119  ; 4
    defb 119,119,119,119,119,119,119,119,119,119  ; 5
    defb 119,119,119,119,121,119,119,119,119,119  ; 6
    defb 119,119,119,119,119,119,119,119,119,119  ; 7
    defb 119,119,119,119,119,119,119,119,119,119  ; 8
    defb 107, 76, 81, 75, 80, 74, 79, 73, 78, 75  ; 9
    defb  77, 75, 79, 76, 81, 75, 80, 74, 79, 73  ;10
    defb  78, 75, 77, 75, 79, 76, 81, 75, 80, 74  ;11
    defb  79, 73, 78, 75, 77, 75, 79, 76, 81, 75  ;12
    defb  80, 74, 79, 73, 80, 89, 95,  0, -1, 78  ;13

;-------------------------------------------------------------------------------

@timing.uncontended:

    defb  28,119,119,119,119,119,119,119,119,119  ; 1
    defb 119,119,119,119,119,119,119,119,119,119  ; 2
    defb 119,119,119,119,119,119,119,119,119,119  ; 3
    defb 119,119,119,119,119,119,119,119,119,119  ; 4
    defb 119,119,119,119,119,119,119,119,119,119  ; 5
    defb 119,119,119,119,121,119,119,119,119,119  ; 6
    defb 119,119,119,119,119,119,119,119,119,119  ; 7
    defb 119,119,119,119,119,119,119,119,119,119  ; 8
    defb 107, 76, 81, 75, 80, 74, 79, 73, 78, 75  ; 9
    defb  77, 75, 79, 76, 81, 75, 80, 74, 79, 73  ;10
    defb  78, 75, 77, 75, 79, 76, 81, 75, 80, 74  ;11
    defb  79, 73, 78, 75, 77, 75, 79, 76, 81, 75  ;12
    defb  80, 74, 79, 73, 80, 89, 95,  0, -1, 78  ;13

;-------------------------------------------------------------------------------
