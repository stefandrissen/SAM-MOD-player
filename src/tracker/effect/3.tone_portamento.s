 ;---------------------------------------------------------------
 ; Effect 3 - Tone Portamento
 ;---------------------------------------------------------------
 r1.058:
    ld a,(parameter)
    or a
    jr z,@use.last.parameter

 r1.059:
    ld (tonespeed),a        ; incorrect when BPM not 125 - see Nearly There by Jogeir (BPM 128)
    xor a
 r1.060:
    ld (parameter),a

 tonenochng:
 @use.last.parameter:
 wanted.per: equ $+1
    ld de,0
    ld a,d
    or e
    ret z

 tonespeed: equ $+1
    ld bc,0

 r1.061:
    ld hl,(period)

 tone.portamento.direction: equ $+1
    ld a,0
    or a
    jr nz,@up

 @down:
    add hl,bc
    sbc hl,de
    jr c,@tone.set.period

    jr @reset.tone.portamento

 @up:
    sbc hl,bc
    jr c,@reset.tone.portamento

    sbc hl,de
    jr nc,@tone.set.period

 @reset.tone.portamento:
    ld hl,0
 r1.062:
    ld (wanted.per),hl

 @tone.set.period:
    add hl,de

 gliss: equ $+1
    ld a,0
    or a
    jr z,@skip

    ex de,hl

    ld c,0
 r1.063:
    ld a,(finetune)
    srl a
    jr nc,$+4
    set 7,c
    add finelist / 256
    ld b,a
 @loop:
    ld a,(bc)
    inc c
    ld l,a
    ld a,(bc)
    inc c
    ld h,a
    or a
    sbc hl,de
    jr c,@found

    ld a,c
    and %01111111
    cp 2*36
    jr c,@-loop

 @found:
    dec c
    ld a,(bc)
    ld h,a
    dec c
    ld a,(bc)
    ld l,a
 @skip:
 r1.064:
    ld (period),hl
    ex de,hl

 r1.065:
    jp period.nop.de
