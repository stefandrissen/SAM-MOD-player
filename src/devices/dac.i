; dac on printer port 1 or 2
;
; timing also used by blue alpha

;-------------------------------------------------------------------------------

include "../ports/printer.i"

;-------------------------------------------------------------------------------

properties.dac.1:

    defw 0,0                    ; init
    defw out.dac,out.dac.len
    defw port.printer_1.data
    defw 0
    defw timing.dac.ram
    defw timing.dac.megabyte
    defb 6                      ; bits per channel

properties.dac.2:

    defw 0,0
    defw out.dac,out.dac.len
    defw port.printer_2.data
    defw 0
    defw timing.dac.ram
    defw timing.dac.megabyte
    defb 6                      ; bits per channel

;-------------------------------------------------------------------------------

out.dac:

    ld e,a          ;  4   1
    ld a,(hl)       ;  8   1
    inc hl          ;  8   1
    add (hl)        ;  8   1
    inc hl          ;  8   1
    out (c),a       ; 12   2
    ld a,e          ;  4   1

    out.dac.len: equ $ - out.dac

;-------------------------------------------------------------------------------

timing.dac.ram:

    defb  40,129,129,129,129,129,129,129    ;   0
    defb 129,129,129,129,129,129,129,129    ;   8
    defb 129,129,129,129,129,129,129,129    ;  16
    defb 129,129,129,129,129,129,129,129    ;  24
    defb 129,129,129,129,129,129,129,129    ;  32
    defb 129,129,129,129,129,129,129,129    ;  40
    defb 129,129,130,129,130,129,129,129    ;  48
    defb 129,129,129,129,129,129,129,129    ;  56
    defb 129,129,129,129,129,129,129,129    ;  64
    defb 129,129,129,129,129,129,129,129    ;  72
    defb 118, 84, 85, 84, 88, 84, 85, 84    ;  80
    defb  88, 84, 85, 84, 88, 84, 85, 84    ;  88
    defb  88, 84, 85, 84, 88, 84, 85, 84    ;  96
    defb  88, 84, 85, 84, 88, 84, 85, 83    ; 104
    defb  89                                ; 112
    defb 0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1  ; second 0?

    defb 93

    assert $ - timing.dac.ram == 130

;-------------------------------------------------------------------------------

timing.dac.megabyte:    ; !!! incorrect

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

    assert $ - timing.dac.megabyte == 130

;-------------------------------------------------------------------------------
