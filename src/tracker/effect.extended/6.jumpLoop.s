;---------------------------------------------------------------
 ; Extended Effect 6 - Jump Loop

    ld a,(song.tick)
    or a
    ret nz

 r1.129:
    ld a,(parameter)
    and 0x0f
    jr z,@setloop

    ld b,a
   jump_loop.loop_counter: equ $+1
    ld a,0
    or a
    jr z,@continue

    dec a
 r1.130:
    ld (jump_loop.loop_counter),a
    ret z

 @jump_loop:
   jump_loop.pattern.row: equ $+1
    ld a,0
    ld (pattern_break.row),a
    cpl                         ; -> a = always non-zero
    ld (pattern_break.flag),a
    ret

 @continue:
    ld a,b
 r1.131:
    ld (jump_loop.loop_counter),a
    jr @jump_loop

 @setloop:
    ld a,(pattern.row)
 r1.132:
    ld (jump_loop.pattern.row),a

    ret
