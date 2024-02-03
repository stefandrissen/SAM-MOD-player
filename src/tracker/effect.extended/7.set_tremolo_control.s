 ;---------------------------------------------------------------
 ; Extended Effect 7 - Set Tremolo Control

 r1.133:
    ld a,(parameter)
    and 0x0f
    rlca
    rlca
    rlca
    rlca
    ld b,a
 r1.134:
    ld hl,wav.cntrl+1
    ld a,(hl)
    and 0x0f
    or b
    ld (hl),a
    ret
