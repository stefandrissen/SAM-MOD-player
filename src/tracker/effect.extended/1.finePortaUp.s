 ;---------------------------------------------------------------
 ; Extended Effect 1 - Fine Porta Up

    ld a,(song.tick)
    or a
    ret nz

    ld a,0x0f
 r1.119:
    ld (porta_up.mask),a
 r1.120:
    jp effect.portaUp
