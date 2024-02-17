;---------------------------------------------------------------
 ; Effect 4 - Vibrato
 ;---------------------------------------------------------------

 r1.066:
    ld a,(parameter)
    or a
    jr z,vibrato2

 vibr.cmnd: equ $+1
    ld b,0
    and 0x0f
    jr z,@skip

    ld c,a
    ld a,b
    and 0xf0
    or c
    ld b,a
 @skip:
 r1.067:
    ld a,(parameter)
    and 0xf0
    jr z,@skip2
    ld c,a
    ld a,b
    and 0x0f
    or c
    ld b,a
 @skip2:
    ld a,b
 r1.068:
    ld (vibr.cmnd),a
 vibrato2:
 vibr.pos: equ $+1
    ld a,0
    rrca
    rrca
    and 0x1f
    ld b,a
 r1.069:
    ld a,(wav.cntrl)
    and 0x03
    jr z,@sine

    sla b
    sla b
    sla b
    dec a
    jr z,@ramp
                        ;                 _ _ _
    ld e,255            ; square waveform   _ _ _
    jr @set

 @ramp:
 r1.070:
    ld a,(vibr.pos)
    bit 7,a
    jr nz,@ramp2

    ld a,255            ; rampdown waveform \ \ \ \
    sub b               ;                   \ \ \ \
    ld e,a
    jr @set

 @ramp2:
    ld e,b
    jr @set

 @sine:
    ld h,table.vibrato / 0x100
    ld l,b              ; sine waveform  /\  /\
    set 5,l             ; table offset 32  \/  \/
    ld e,(hl)

 @set:
    ld hl,0
 r1.071:
    ld a,(vibr.cmnd)
    and 0x0f
    jr z,@skip.mul

    ld b,a
    ld d,0
 @mul:
    add hl,de
    djnz @-mul
 @skip.mul:
    sla l
    rl h
    ld b,h
 r1.072:
    ld hl,(period)
 r1.073:
    ld a,(vibr.pos)
    bit 7,a
    jr nz,@negative

    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h

    jr @vibrato3

 @negative:
    ld a,l
    sub b
    ld l,a
    jr nc,$+3
    dec h

 @vibrato3:
    ex de,hl
 r1.074:
    call period.nop.de
 r1.075:
    ld a,(vibr.cmnd)
    rrca
    rrca
    and %00111100
 r1.076:
    ld hl,vibr.pos
    add (hl)
    ld (hl),a

    ret
