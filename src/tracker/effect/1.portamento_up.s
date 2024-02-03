;---------------------------------------------------------------
 ; Effect 1 - Portamento Up
 ;---------------------------------------------------------------
 r1.043:
    ld hl,(period)
 r1.044:
    ld a,(parameter)
 porta_up.mask: equ $+1
    and 0xff            ; changed to 0x0f for fine porta
    ld c,a
    ld b,0
    ld a,0xff
 r1.045:
    ld (porta_up.mask),a
    sbc hl,bc
 period.min: equ $+1
    ld bc,113           ; B-3 minimum Amiga period
    jr c,porttoofar
    sbc hl,bc
    jr nc,portauskip
 porttoofar:
    ld hl,0
 portauskip:
    add hl,bc
 r1.046:
    ld (period),hl
    ex de,hl
 r1.047:
    jp period.nop.de
