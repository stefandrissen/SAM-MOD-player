; blue alpha sound sampler

;-------------------------------------------------------------------------------

include "../ports/blue_alpha.i"

;-------------------------------------------------------------------------------

properties.blue_alpha:

    defw @init,@init.len
    defw out.dac,out.dac.len
    defw 124 * 256 + 127
    defw 0
    defw timing.dac.contended
    defw timing.dac.uncontended
    defb 17 * 3 + 2
    defb 6                      ; bits per channel

;-------------------------------------------------------------------------------

@init:

    ld bc,port.blue_alpha.control
    ld a,255
    out (c),a
    ld bc,125   ; port.blue_alpha.b
    ld a,253
    out (c),a

    @init.len: equ $ - @init

;-------------------------------------------------------------------------------
