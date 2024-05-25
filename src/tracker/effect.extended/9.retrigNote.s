 ;---------------------------------------------------------------
 ; Extended Effect 9 - Retrig Note

 r1.135:
    ld a,(parameter)
    and 0x0f
    ret z

    ld hl,table.retrig
    bit 3,a
    res 3,a
    jr z,$+3
    inc h
    rrca
    rrca
    rrca
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld a,(song.tick)
    add a,l
    ld l,a            ;can't overflow
    ld a,(hl)
    or a
    ret z

 do.retrig:
 r1.136:
    ld a,(instrument.new)
 r1.137:
    ld (instrument.current),a
 r1.138:
    ld hl,(cx.sample.offset)
 r1.139:
    ld a,(cx.sample.page)
 bp.page.8:
    ld (0),a
 bp.offset.8:
    ld (0),hl
    ret