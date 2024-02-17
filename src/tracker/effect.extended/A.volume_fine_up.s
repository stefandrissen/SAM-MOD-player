 ;---------------------------------------------------------------
 ; Extended Effect A - Volume Fine Up

    ld a,(tick)
    or a
    ret nz

 r1.140:
    ld a,(parameter)
    and 0x0f
 r1.141:
    jp volume_slide.up
