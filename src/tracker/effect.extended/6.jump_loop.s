;---------------------------------------------------------------
 ; Extended Effect 6 - Jump Loop

    ld a,(tick)
    or a
    ret nz

 r1.129:
    ld a,(parameter)
    and 0x0f
    jr z,@setloop
    ld b,a
 loopcount: equ $+1
    ld a,0
    or a
    jr z,@continue

    dec a
 r1.130:
    ld (loopcount),a
    ret z

 jmploop:
 pattpos: equ $+1
    ld a,0
    ld (pbreak.pos),a
    ld a,1
    ld (pbreak.flag),a
    ret

 @continue:
    ld a,b
 r1.131:
    ld (loopcount),a
    jr jmploop

 @setloop:
    ld a,(pattern.pos)
 r1.132:
    ld (pattpos),a

    ret
