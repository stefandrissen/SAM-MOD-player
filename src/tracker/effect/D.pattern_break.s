  ;---------------------------------------------------------------
  ; Effect D - Pattern Break
  ;
  ; parameter is decimal -> consider fixing when installing mod
  ;---------------------------------------------------------------
 r1.115:
    ld a,(parameter)
    ld e,a
    and 0xf0
    rrca
    rrca
    rrca
    rrca
    ld c,a
    add a,a
    add a,a
    add a,c
    add a,a   ;*10
    ld c,a
    ld a,e
    and 0x0f
    add c
    cp 0x40
    jr nc,pj2

    ld (pattern_break.row),a
    ld a,1
    ld (position_jump.flag),a

    ret
