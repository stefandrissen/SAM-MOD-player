;---------------------------------------------------------------
 ; Extended Effect 6 - Jump Loop

    ld a,(tick)
    or a
    ret nz

 r1.129:
    ld a,(parameter)
    and 0x0f
    jr z,setloop
    ld b,a
 loopcount:
    ld a,0
    or a
    jr z,jump.cnt

    dec a
 r1.130:
    ld (loopcount+1),a
    ret z

 jmploop:
 pattpos:
    ld a,0
    ld (pbreak.pos+1),a
    ld a,1
    ld (pbreak.flag+1),a
    ret

 jump.cnt:
    ld a,b
 r1.131:
    ld (loopcount+1),a
    jr jmploop

 setloop:
    ld a,(pattern.pos)
 r1.132:
    ld (pattpos+1),a
    ret
