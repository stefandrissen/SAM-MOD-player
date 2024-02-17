 ;---------------------------------------------------------------
 ; Effect 7 - Tremolo
 ;---------------------------------------------------------------
 r1.081:
    call period.nop
 tremolo:
 r1.082:
    ld a,(parameter)
    or a
    jr z,tremolo2

 trem.cmnd: equ $+1
    ld b,0
    and 0x0f
    jr z,treskip

    ld c,a
    ld a,b
    and 0xf0
    or c
    ld b,a
 treskip:
 r1.083:
    ld a,(parameter)
    and 0xf0
    jr z,treskip2

    ld c,a
    ld a,b
    and 0x0f
    or c
    ld b,a
 treskip2:
    ld a,b
 r1.084:
    ld (trem.cmnd),a
 tremolo2:
 trem.pos: equ $+1
    ld a,0
    rrca
    rrca
    and 0x1f
    ld b,a
 r1.085:
    ld a,(wav.cntrl)
    rrca
    rrca
    rrca
    rrca
    and 0x03
    jr z,@sine

    sla b
    sla b
    sla b
    dec a
    jr z,@ramp

    ld e,255
    jr @set

 @ramp:
 r1.086:
    ld a,(trem.pos)
    bit 7,a
    jr nz,@ramp2

    ld a,255
    sub b
    ld e,a
    jr @set

 @ramp2:
    ld e,b
    jr @set

 @sine:
    ld h,table.vibrato / 0x100
    ld l,b
    set 5,l                 ;table offset 32
    ld e,(hl)
 @set:
    ld hl,0
 r1.087:
    ld a,(trem.cmnd)
    and 0x0f
    jr z,@skip

    ld b,a
    ld d,0
 @multiply:
    add hl,de
    djnz @-multiply

 @skip:
    sla l
    rl h
 r1.088:
    ld a,(trem.pos)
    bit 7,a
 r1.089:
    ld a,(volume)
    jr nz,@negative
    add h
    jr nc,$+4
    ld a,63

    cp 64
    jr c,$+4
    ld a,63

    jr tremolo3

 @negative:
    sub h
    jr nc,$+3
    xor a
 tremolo3:
    ld c,a
 r1.090:
    ld a,(volume)
    ld b,a
    ld a,c

 r1.091:
    ld (volume),a

 r1.092:
    call bp.volume

    ld a,b
 r1.093:
    ld (volume),a

 r1.094:
    ld a,(trem.cmnd)
    rrca
    rrca
    and %00111100
 r1.095:
    ld hl,trem.pos
    add (hl)
    ld (hl),a

    ret
