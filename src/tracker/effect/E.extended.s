 ;---------------------------------------------------------------
 ; Effect E - Extended
 ;            x = extended effect
 ;            y = parameter
 ;---------------------------------------------------------------
 r1.117:
    ld a,(parameter)
    and 0xf0
    rrca
    rrca
    rrca
 r1.118:
    ld hl,list.effects.extended
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a

    jp (hl)
