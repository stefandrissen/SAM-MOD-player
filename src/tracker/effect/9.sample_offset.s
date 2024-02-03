 ;---------------------------------------------------------------
 ; Effect 9 - Sample Offset  if offset too large -> start of loop
 ;
 ; - effect 9 without offset       -> use previous sample offset
 ; - note triggered without sample -> use previous sample offset
 ;
 ; more info - https://github.com/libxmp/libxmp/blob/master/docs/tracker_notes.txt
 ;           - https://github.com/steffest/BassoonTracker/blob/7e24c34b59c9c52304a90c42ccb6e8ed542c274e/script/src/tracker.js#L1028
 ;---------------------------------------------------------------
 r1.096:
    ld a,(parameter)
    or a
    jr z,@sample_offset.zero

 r1.097:
    ld (cx.effect.sample.offset),a

 @sample_offset.zero:
 cx.effect.sample.offset: equ $+1
    ld a,0
 r1.098:
    ld hl,(cx.sample.offset)
    ld b,a
    and %11000000       ; %01000000 = 0x40 -> 0x4000 = 16384 = 1 page
    rlca
    rlca
    ld c,a              ; offset in pages
    ld a,b
    and %00111111       ; offset in bytes excluding pages
    add a,h
    ld h,a
 r1.099:
    ld a,(cx.sample.page)
    add c
    bit 6,h             ; if pointer in bank D, move pointer down to bank C
    res 6,h
    jr z,$+3
    inc a               ; increasing page

    ex de,hl
    ld c,a
 r1.100:
    ld a,(sample_end.page)
    sub c
    jr nc,@+ok
 r1.101:
    ld hl,(sample_end.offset)
    or a
    sbc hl,de
    jr nc,@+ok
    add c               ; -> a = (sample_end.page )
    add hl,de           ; -> hl = (sample_end.offset)
    jr bp.page.7
 @ok:
    ld a,c
    ex de,hl
 bp.page.7:
    ld (0),a
 bp.offset.7:
    ld (0),hl

    ret
