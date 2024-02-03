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

 trem.cmnd:
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
    ld (trem.cmnd+1),a
 tremolo2:
 trem.pos:
    ld a,0
    rrca
    rrca
    and 0x1f
    ld b,a
 r1.085:
    ld a,(wav.cntrl+1)
    rrca
    rrca
    rrca
    rrca
    and 0x03
    jr z,tre.sine

    sla b
    sla b
    sla b
    dec a
    jr z,tre.ramp

    ld e,255
    jr tre.set

 tre.ramp:
 r1.086:
    ld a,(trem.pos+1)
    bit 7,a
    jr nz,tre.ramp2

    ld a,255
    sub b
    ld e,a
    jr tre.set

 tre.ramp2:
    ld e,b
    jr tre.set

 tre.sine:
    ld h,table.vibrato / 0x100
    ld l,b
    set 5,l                 ;table offset 32
    ld e,(hl)
 tre.set:
    ld hl,0
 r1.087:
    ld a,(trem.cmnd+1)
    and 0x0f
    jr z,skiptremul

    ld b,a
    ld d,0
 tre.mul:
    add hl,de
    djnz tre.mul

 skiptremul:
    sla l
    rl h
 r1.088:
    ld a,(trem.pos+1)
    bit 7,a
 r1.089:
    ld a,(volume)
    jr nz,trem.neg
    add h
    jr nc,$+4
    ld a,63

    cp 64
    jr c,$+4
    ld a,63

    jr tremolo3

 trem.neg:
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
    ld a,(trem.cmnd+1)
    rrca
    rrca
    and %00111100
 r1.095:
    ld hl,trem.pos+1
    add (hl)
    ld (hl),a
    ret
