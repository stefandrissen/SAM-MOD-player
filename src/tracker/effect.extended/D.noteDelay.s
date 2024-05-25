 ;---------------------------------------------------------------
 ; Extended Effect D - Note Delay

 r1.147:
    ld a,(parameter)
    and 0x0f
    ld b,a
    ld a,(song.tick)
    cp b
    ret nz

    or a
    ret z
    jr do.retrig
