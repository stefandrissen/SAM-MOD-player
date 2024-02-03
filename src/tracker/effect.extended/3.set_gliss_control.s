 ;---------------------------------------------------------------
 ; Extended Effect 3 - Set Gliss Control

 r1.123:
    ld a,(parameter)
    and 0x0f
 r1.124:
    ld (gliss+1),a
    ret
