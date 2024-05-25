 ;---------------------------------------------------------------
 ; Extended Effect E - Pattern Delay

    ld a,(song.tick)
    or a
    ret nz

 r1.148:
    ld a,(parameter)
    and 0x0f
    ld b,a
    ld a,(pattern_delay.counter)
    or a
    ret nz              ; still delaying pattern

    ld a,b              ; so do not reset tick
    inc b
    ld (pattern_delay.flag),a

    ret
