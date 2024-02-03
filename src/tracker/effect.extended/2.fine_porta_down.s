 ;---------------------------------------------------------------
 ; Extended Effect 2 - Fine Porta Down

    ld a,(tick)
    or a
    ret nz

    ld a,0x0f
 r1.121:
    ld (porta_down.mask),a
 r1.122:
    jp effect.portamento_down

