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


; lets try actual t-states 384 states per line, with a counter for when contended memory is encountered

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

@timing.megabyte:   ; !!! incorrect

    defb      28
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,121
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,119
    defb 119,107
    defb  76, 81
    defb  75, 80
    defb  74, 79
    defb  73, 78
    defb  75, 77
    defb  75, 79
    defb  76, 81
    defb  75, 80
    defb  74, 79
    defb  73, 78
    defb  75, 77
    defb  75, 79
    defb  76, 81
    defb  75, 80
    defb  74, 79
    defb  73, 78
    defb  75, 77
    defb  75, 79
    defb  76, 81
    defb  75, 80
    defb  74, 79
    defb  73, 80
    defb  89, 95
    defb 0

    defb -1

    defb 78         ; t-states to 1.5 from line interrupt

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------
