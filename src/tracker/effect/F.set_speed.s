 ;---------------------------------------------------------------
 ; Effect F - Set Speed
 ;            Sort of handles BPM alterations (not 100% accurate)
 ;---------------------------------------------------------------
 r1.116:
    ld a,(parameter)
    or a
    ret z

    cp 32                   ; speed <32
    jr nc,@setbpm           ; so this is BPM
    ld (speed),a
    ; xor a
    ; ld (tick),a
    ret

  @setbpm:
    ld h,table.bpm / 0x100
    add a,a
    jr nc,$+3
    inc h
    ld l,a
    ld e,(hl)
    inc l
    ld d,(hl)
    ld (tempo),de
    ret
