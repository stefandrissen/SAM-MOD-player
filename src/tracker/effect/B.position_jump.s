 ;---------------------------------------------------------------
 ; Effect B - Position Jump
 ;---------------------------------------------------------------
    ld a,(disable.pos)
    or a
    ret nz
 r1.111:
    ld a,(parameter)
    dec a
    ld (song.position),a
 pj2:
    xor a
    ld (pattern_break.row),a
    inc a
    ld (position_jump.flag),a

    ret
