 ;---------------------------------------------------------------
 ; Effect B - Position Jump
 ;---------------------------------------------------------------
    ld a,(disable.pos)
    or a
    ret nz
 r1.111:
    ld a,(parameter)
    dec a
    ld (song.pos),a
 pj2:
    xor a
    ld (pbreak.pos),a
    inc a
    ld (posjump.flag),a

    ret
