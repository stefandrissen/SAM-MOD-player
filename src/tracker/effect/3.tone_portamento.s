 ;---------------------------------------------------------------
 ; Effect 3 - Tone Portamento
 ;---------------------------------------------------------------
 r1.058:
    ld a,(parameter)
    or a
    jr z,@use.last.parameter

 r1.059:
    ld (tonespeed+1),a      ; TODO: needs correction when BPM not 125 - see Nearly There by Jogeir (BPM 128) use tempo
    xor a
 r1.060:
    ld (parameter),a

 tonenochng:
 @use.last.parameter:
 wanted.per:
    ld de,0
    ld a,d
    or e
    ret z

 tonespeed:
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
    ld (wanted.per+1),hl

 @tone.set.period:
    add hl,de

 gliss:
    ld a,0
    or a
    jr z,glissskip

    ex de,hl

    ld c,0
 r1.063:
    ld a,(finetune)
    srl a
    jr nc,$+4
    set 7,c
    add finelist / 256
    ld b,a
 glissloop:
    ld a,(bc)
    inc c
    ld l,a
    ld a,(bc)
    inc c
    ld h,a
    or a
    sbc hl,de
    jr c,glissfound

    ld a,c
    and %01111111
    cp 2*36
    jr c,glissloop

 glissfound:
    dec c
    ld a,(bc)
    ld h,a
    dec c
    ld a,(bc)
    ld l,a
 glissskip:
 r1.064:
    ld (period),hl
    ex de,hl
 r1.065:
    jp period.nop.de
