; blue alpha sound sampler

;-------------------------------------------------------------------------------

include "../ports/blue_alpha.i"

;-------------------------------------------------------------------------------

properties.blue_alpha:

    defw @init,@init.len
    defw out.dac,out.dac.len
    defw port.blue_alpha.a      ; bc
    defw 0                      ; de - unused
    defw timing.dac.ram
    defw timing.dac.megabyte
    defb 6                      ; bits per channel

;-------------------------------------------------------------------------------

@init:

    ld bc,port.blue_alpha.control
    ld a,0xff
    out (c),a
    ld b,port.blue_alpha.b \ 0x100
    ld a,%11111101  ; 'documentation' indicates that this is adc?
    out (c),a

    @init.len: equ $ - @init

;-------------------------------------------------------------------------------
