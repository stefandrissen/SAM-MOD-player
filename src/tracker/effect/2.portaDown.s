;---------------------------------------------------------------
 ; Effect 2 - Portamento Down
 ;---------------------------------------------------------------
 r1.048:
    ld hl,(period)
 r1.049:
    ld a,(parameter)
 porta_down.mask: equ $+1
    and 0xff            ; changed to 0x0f by fine porta
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld a,0xff
 r1.050:
    ld (porta_down.mask),a
 period.max: equ $+1
    ld bc,856           ; C-1 maximum Amiga period
    or a
    sbc hl,bc
    jr c,@skip

    ld hl,0
 @skip:
    add hl,bc
 r1.051:
    ld (period),hl
    ex de,hl

 r1.052:
    jp period.nop.de
