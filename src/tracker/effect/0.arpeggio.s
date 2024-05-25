 ;---------------------------------------------------------------
 ; Effect 0 - Arpeggio
 ;---------------------------------------------------------------
    ld a,(song.tick)            ; [1-31]
    ld h,table.arpeggio / 0x100 ; table on 256 boundary
    ld l,a
    ld a,(hl)                   ; -> a = tick mod 3
    or a
    jr z,@arpeggio.0

    cp 2
    jr z,@arpeggio.2

 r1.040:
    ld a,(parameter)
    and 0xf0                    ; 1
    rrca
    rrca
    rrca
    rrca
    jr @arpeggio.calc.period

 @arpeggio.2:
 r1.041:
    ld a,(parameter)
    and 0x0f                    ; 2
    jr @arpeggio.calc.period

 @arpeggio.0:
 r2.009:
    ld de,(period)             ; 0
    jr @arpeggio.got.period

 @arpeggio.calc.period:
    add a,a
 note.number: equ $+1
    add 0
    ret c               ;note number unknown

    cp 2*36
    ret nc              ;new note too high

    ld l,a
 r1.042:
    ld a,(finetune)
    srl a
    jr nc,$+4
    set 7,l
    add a,finelist / 0x100
    ld h,a
    ld e,(hl)
    inc l
    ld d,(hl)

 @arpeggio.got.period:

    jr period.nop.de
