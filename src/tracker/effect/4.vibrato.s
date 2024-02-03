;---------------------------------------------------------------
 ; Effect 4 - Vibrato
 ;---------------------------------------------------------------

 r1.066:
    ld a,(parameter)
    or a
    jr z,vibrato2

 vibr.cmnd:
    ld b,0
    and 0x0f
    jr z,vibskip

    ld c,a
    ld a,b
    and 0xf0
    or c
    ld b,a
 vibskip:
 r1.067:
    ld a,(parameter)
    and 0xf0
    jr z,vibskip2
    ld c,a
    ld a,b
    and 0x0f
    or c
    ld b,a
 vibskip2:
    ld a,b
 r1.068:
    ld (vibr.cmnd+1),a
 vibrato2:
 vibr.pos:
    ld a,0
    rrca
    rrca
    and 0x1f
    ld b,a
 r1.069:
    ld a,(wav.cntrl+1)
    and 0x03
    jr z,vib.sine

    sla b
    sla b
    sla b
    dec a
    jr z,vib.ramp     ;                 _ _ _

    ld e,255          ;square waveform   _ _ _
    jr vib.set

 vib.ramp:
 r1.070:
    ld a,(vibr.pos+1)
    bit 7,a
    jr nz,vib.ramp2

    ld a,255                    ;rampdown waveform \ \ \ \
    sub b                       ;                   \ \ \ \
    ld e,a
    jr vib.set

 vib.ramp2:
    ld e,b
    jr vib.set

 vib.sine:
    ld h,table.vibrato / 0x100
    ld l,b                      ;sine waveform  /\  /\
    set 5,l                 ;table offset 32  \/  \/
    ld e,(hl)
 vib.set:
    ld hl,0
 r1.071:
    ld a,(vibr.cmnd+1)
    and 0x0f
    jr z,skip.mul

    ld b,a
    ld d,0
 vib.mul:
    add hl,de
    djnz vib.mul
 skip.mul:
    sla l
    rl h
    ld b,h
 r1.072:
    ld hl,(period)
 r1.073:
    ld a,(vibr.pos+1)
    bit 7,a
    jr nz,vibr.neg

    ld a,l
    add b
    ld l,a
    jr nc,$+3
    inc h

    jr vibrato3

 vibr.neg:
    ld a,l
    sub b
    ld l,a
    jr nc,$+3
    dec h

 vibrato3:
    ex de,hl
 r1.074:
    call period.nop.de
 r1.075:
    ld a,(vibr.cmnd+1)
    rrca
    rrca
    and %00111100
 r1.076:
    ld hl,vibr.pos+1
    add (hl)
    ld (hl),a
    ret
