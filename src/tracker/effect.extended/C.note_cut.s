 ;---------------------------------------------------------------
 ; Extended Effect C - Note Cut

 r1.144:
    ld a,(parameter)
    and 0x0f
    ld b,a
    ld a,(tick)
    cp b
    ret nz

    xor a
 r1.145:
    ld (volume),a
 r1.146:
    jp bp.volume
