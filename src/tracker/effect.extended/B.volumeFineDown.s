 ;---------------------------------------------------------------
 ; Extended Effect B - Volume Fine Down

    ld a,(song.tick)
    or a
    ret nz

 r1.142:
    ld a,(parameter)
    and 0x0f
 r1.143:
    jp volume_slide.down
