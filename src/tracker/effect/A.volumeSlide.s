 ;---------------------------------------------------------------
 ; Effect A - Volume Slide
 ;------------------------------------------------------------
 r1.102:
    call period.nop

 volume_slide:
 r1.103:
    ld a,(parameter)
    and 0xf0
    jr z,volume_slide.down

    rrca
    rrca
    rrca
    rrca

 volume_slide.up:
    ld b,a
 r1.104:
    ld a,(volume)
    add b
    cp 0x40
    jr c,$+4
    ld a,0x3f
 r1.105:
    ld (volume),a
 r1.106:
    jp bp.volume

 volume_slide.down:
 r1.107:
    ld a,(parameter)
    and 0x0f
    ld b,a
 r1.108:
    ld a,(volume)
    sub b
    jr nc,$+3
    xor a
 r1.109:
    ld (volume),a

 r1.110:
    jp bp.volume
