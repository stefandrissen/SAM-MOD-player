show.burst:

 ; F6 show values of burst player pointers

    ld hl,(int.routine)
    ld de,@burst.interrupt
    or a
    sbc hl,de
    jr nz,@sb.no.inc    ; only increase channel if in this mode already

    ld hl,@burst.channel + 1
    ld a,(hl)
    inc a
    and 3
    ld (hl),a

 @sb.no.inc:

    ld a,-1
    ld (int.rtn.pag),a

    call cls

    ld ix,@text.burst.colour
    ld a,8
    call colour.scrn

    ld de,@text.burst
    ld hl,video.memory.24.rows * 0 + 0 + video.memory.high
    ld b,32
    call print.de.b

    ld hl,video.memory.24.rows * 1 + 0 + video.memory.high
    ld b,23
    call print.de.b

    ld hl,video.memory.24.rows * 3 + 0 + video.memory.high
    ld b,8
    call print.de.b

 @burst.channel:
    ld a,0
    add "1"
    call print.chr

    ld hl,video.memory.24.rows * 4 + 0 + video.memory.high
    ld b,21
    call print.de.b

    ld hl,bp.pointers.sample - bp.pointers.length
    ld de,bp.pointers.length
    ld a,(@burst.channel + 1)
    inc a
    ld b,a
    @loop:
        add hl,de
        djnz @-loop

    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (@bi.page+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (@bi.offs2+1),de
    inc de
    ld (@bi.offs1+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (@bi.vol+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    inc l
    ld (@bi.slo+1),de
    ld e,(hl)
    inc l
    ld d,(hl)
    ld (@bi.shi+1),de

    ld hl,@burst.interrupt
    ld (int.routine),hl

    in a,(port.hmpr)
    ld (int.rtn.pag),a

    ld a,-1
    ret

;-------------------------------------------------------------------------------
@text.burst:

    defm "SAM MOD player             "
    include "../constants/text.version.i"
    include "../constants/text.copyright.i"
    defm "CHANNEL "
    defm "Page Offs Vol SLo SHi"

@text.burst.colour:
    defb 3,2,1,4,1,5,19,1

;-------------------------------------------------------------------------------
@burst.interrupt:

    ld a,(counter)
    or a
    ret nz

  @burst.print.pos:
    ld hl,video.memory.24.rows * 5 + 2 + video.memory.high
    ld b,32
  @bi.page:
    ld a,(0)
    call print.hex
    inc l
 @bi.offs1:
    ld a,(0)
    call print.hex
 @bi.offs2:
    ld a,(0)
    call print.hex
    inc l
    inc l

 @bi.vol:
    ld a,(0)
    call print.hex
    inc l
    inc l

 @bi.slo:
    ld a,(0)
    call print.hex
    inc l
    inc l

 @bi.shi:
    ld a,(0)
    call print.hex

    ld hl,@burst.print.pos+2
    ld a,(hl)
    inc a
    cp 24 + 128
    jr nz,$+4

    ld a,5 + 128

    ld (hl),a

    ret
