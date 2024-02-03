 ;---------------------------------------------------------------
 ; Extended Effect 4 - Set Vibrato Control

 r1.125:
    ld a,(parameter)
    and 0x0f
    ld b,a
 r1.126:
    ld hl,wav.cntrl+1
    ld a,(hl)
    and 0xf0
    or b
    ld (hl),a
    ret
