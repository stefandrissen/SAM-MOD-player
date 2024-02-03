 ;---------------------------------------------------------------
 ; Extended Effect 5 - Set Fine Tune

 r1.127:
    ld a,(parameter)
    and 0x0f
 r1.128:
    ld (finetune),a

    ret
