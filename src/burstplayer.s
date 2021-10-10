; SAM MOD player - MAKE burstplayer

; (C) 1995-2021 Stefan Drissen

; 384 t-states per line, 312 lines = 119808 t-states per frame
; 6MHz / 119808 = 50.08 Hz
;

;---------------------------------------------------------------

include "memory.i"
include "ports/internal.i"
include "ports/megabyte.i"
include "opcodes.i"

;---------------------------------------------------------------

; samdac routine uses most memory!
; quazar needs largest playback buffers

count:      equ 255     ; number of outs before border change

;---------------------------------------------------------------

    org 0x8000

;---------------------------------------------------------------

burstplayer.create:
    jp @go.burst

;---------------------------------------------------------------

burstplayer.device: defb 0  ; [0-7]

    device.clut:        equ 0
    device.saa:         equ 1
    device.samdac.1:    equ 2
    device.samdac.2:    equ 3
    device.dac.1:       equ 4
    device.dac.2:       equ 5
    device.bluealpha:   equ 6
    device.quazar:      equ 7

    defb 0 ; burstplayer.port

burstplayer.amiga:      defb 0  ; [0-1]

    pal:        equ 0       ; use which Amiga to calculate sample
    ntsc:       equ 1       ; speeds

burstplayer.external.ram:   defb 0  ; [0-4]

burstplayer.page:           defb page.burstplayer

    defm "                         "
    defm "MAKEBURST "
    include "txt.copyright.i"
    defm "Thanks to Edwin Blink for the   "
    defm "original burst idea and code...."

;---------------------------------------------------------------
@go.burst:
;---------------------------------------------------------------
    di
    in a,(port.lmpr)
    ld (@lmpr+1),a

    ld a,(burstplayer.page)
    or low.memory.ram.0
    out (port.lmpr),a
    ld (@sp+1),sp
    ld sp,0xc000
    call maker
@lmpr:
    ld a,0
    out (port.lmpr),a
@sp:
    ld sp,0
    ; ei
    ret

if defined (debug)

    ;---------------------------------------------------------------
    debug.assert.hl_7_bit_positive:    ; ensure jr does not overflow
    ;---------------------------------------------------------------

        push af
        ld a,h
        or a
        jr nz,@error
        bit 7,l
        jr nz,@error
        pop af
        ret

    @error:

        ld a,r
        and %110
        out (port.border),a
        jr @error

endif

;---------------------------------------------------------------
maker:
;---------------------------------------------------------------
    ld a,count
    ld (poke.count+1),a
    xor a
    ld (@counter+1),a

    ld hl,0
    ld de,1
    ld bc,0x7fff
    ld (hl),l
    ldir

    ld hl,playtab1 + ( 2 * 208 )
    ld bc,2 * 208
    ld a,(burstplayer.device)
    cp device.quazar            ; if QSS -> playtab2 higher
    jr nz,not.qss.pt
    add hl,bc
    ld bc,4 * 208
not.qss.pt:
    ex af,af'
    ld (qs.playtab2+1),hl
    add hl,bc
    ld (no.function+1),hl
    ex af,af'

    ld hl,list.device.properties
    add a,a
    add a,l
    ld l,a
    jr nc,$+3
    inc h
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl

    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld c,(hl)
    inc hl
    ld b,(hl)
    inc hl
    ld (sound.driver.reset.address+1),de
    ld (sound.driver.reset.length+1),bc

    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld c,(hl)
    inc hl
    ld b,(hl)
    inc hl
    ld (sound.driver.address+1),de
    ld (sound.driver.length+1),bc

    ld c,(hl)
    inc hl
    ld b,(hl)
    inc hl
    ld (sample.port+1),bc

    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld (sample.ctrl+1),de

    ld a,(burstplayer.external.ram)
    or a
    jr z,@no.megabyte

    inc hl
    inc hl

@no.megabyte:

    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl

    jr nz,@megabyte

    inc hl
    inc hl

@megabyte:

    ld (mk.timing+2),de     ; pointing to either timing.contended or timing.uncontended

    ex de,hl
    ld bc,129
    add hl,bc
    ld a,(hl)               ; line interrupt timing byte
    ld (no.func.wait+1),a

    sub 15  ;+1 for jr being done
            ;+2 for ld a,jr
            ;+4 for ld (jr+1),a
            ;+4 for ld (cp+1),a

    ld (no.func.wait2+1),a

    ex de,hl

    ld a,(hl)
    inc hl
    ld (ras.start.1+1),a
    ld (ras.start.2+1),a

    ld a,(hl)
    ld (output.bits+1),a

    ld hl,mk.movecode
    ld de,0
    ld bc,mk.mv.end
    ldir


;---------------------------------------------------------------
; interrupt routine (0x0038)

interrupt:

    ld hl,0x0038

    ld (hl),opcode.ex_af_af
    inc hl

    ld (hl),opcode.exx
    inc hl

    call insert.outs

    ld (hl),opcode.cp_n
    inc hl
    ld (hl),191 - 3             ; 0xbc - second to last line interrupt
    inc hl

    ld (hl),opcode.jr_z_n
    inc hl
    ld (mk.sto1+1),hl           ; jr z,prepare.border.player
    ld (mk.sto1.1+1),hl
    ld (mk.recjradd+1),hl
    inc hl

    ld (hl),opcode.add_a_n
    inc hl
    ld (hl),3
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.line_interrupt
    inc hl

    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto2+1),hl           ; jp no.function
    inc hl
    inc hl

mk.sto1.1:
    ld de,0
    push hl
    scf
    sbc hl,de
if defined (debug)
    call debug.assert.hl_7_bit_positive
endif
    ld a,l
    pop hl
    ld (mk.stojr188+1),a

    ld (hl),opcode.ld_a_n
    inc hl
    ld (mk.recjr191+1),hl
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
mk.recjradd:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (jr+1),a
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ld (hl),191         ; screen.last.line
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
    dec de
    dec de
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (cp191+1),a
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.line_interrupt
    inc hl

    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto2.1+1),hl ; jp nofunction2
    inc hl
    inc hl

;---------------------------------------------------------------
; reset output device

    ld de,sound.driver.reset
sound.driver.reset.address:
    ld hl,0
sound.driver.reset.length:
    ld bc,0
    ld a,b
    or c
    jr z,@ro.no.silence
    ldir
@ro.no.silence:
    ex de,hl
    ld (hl),opcode.ret
    inc hl

;---------------------------------------------------------------
; select border player

mk.sto1:
    ld de,0
    push hl
    scf
    sbc hl,de
if defined (debug)
    call debug.assert.hl_7_bit_positive
endif
    ex de,hl
    ld (hl),e
mk.recjr191:
    ld hl,0
    ld (hl),e
    pop hl             ; prepare.border.player:

    ld (hl),opcode.ld_a_n
    inc hl
mk.stojr188:
    ld (hl),0           ; ld a,jr188
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.recjradd+1)
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (jradd+1),a
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ld (hl),191 - 3     ; second to last line interrupt
    inc hl

    dec de
    dec de
    ld (hl),opcode.ld_nn_a
    inc hl
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (cp188+1),a
    inc hl

    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
    ld (mk.sto3+1),hl           ; ld (prog.p+1),a
    inc hl
    inc hl

    ld (hl),opcode.ex_af_af
    inc hl

    ld (hl),opcode.exx
    inc hl

    ld (hl),opcode.push_af
    inc hl

    ld (hl),opcode.push_bc
    inc hl

    ld (hl),opcode.push_de
    inc hl

    ld (hl),opcode.push_hl
    inc hl

    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.push_ix
    inc hl

    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.ld_nn_sp
    inc hl
    ld (mk.sto4+1),hl           ; ld (prog.sp+1),sp
    inc hl
    inc hl

    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.rec2+1),hl           ; playerselect:
    ld (mk.sto5+1),hl           ; jp border.player.1
    inc hl
    inc hl

;---------------------------------------------------------------

output.bits:
    ld b,0

    call populate.volume.table

    call populate.pitch.table

;---------------------------------------------------------------
; get pattern data routine

; copies pattern from mod (AHL) to mod.current.row

    ld hl,get.pattern


    ld a,(burstplayer.external.ram)
    or a
    jr z,@no.megabyte.1

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.xmpr.c
    inc hl

    ld (hl),opcode.inc_a
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.xmpr.d
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ld (hl),high.memory.external
    inc hl

    ld de,1

    jr @continue.1

@no.megabyte.1:

    ld de,8

@continue.1:

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_de_nn
    inc hl
    ld (hl),mod.current.row \ 256
    inc hl
    ld (hl),mod.current.row / 256
    inc hl

    ld b,15
@mk.gp.blp:
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.ldi
    inc hl
    djnz @mk.gp.blp

    ld (hl),opcode.ld_a_hl
    inc hl

    ld (hl),opcode.ld_de_a
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ld (hl),page.sequencer
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ret

    add hl,de

get.pattern.size:   equ 49

;---------------------------------------------------------------
; call far routine, C=page, HL=address

    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,12
    ex de,hl
    add hl,de
    ex de,hl
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl

    ld (hl),opcode.ld_a_c
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_nn_hl
    inc hl
    ld de,3
    ex de,hl
    add hl,de
    ex de,hl
    ld (hl),e
    inc hl
    ld (hl),d                               ; ld (call+1),hl
    inc hl

    ld (hl),opcode.call_nn
    inc hl
    inc hl                                  ; call hl
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ; ld (hl),page.sequencer
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ret
    inc hl

far.call.size:  equ 19

;---------------------------------------------------------------
; ldir.from.far:

; copies C bytes (max 128) to local buffer

; B  = source page
; HL = source address

    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
    ld (@ldir.from.far.page+1),hl
    inc hl
    inc hl
    ld (hl),opcode.ld_a_b
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_de_nn
    inc hl
    ld (hl),ldir.far.buffer \ 256
    inc hl
    ld (hl),ldir.far.buffer / 256
    inc hl

    ld (hl),opcode.ld_b_n
    inc hl
    inc hl

    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.ldir
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ex de,hl
@ldir.from.far.page:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ret

ldir.from.far.size: equ 19

;---------------------------------------------------------------
; ldir.to.far:

; copies C bytes (max 128) from buffer to
; B  = target page,
; DE = target address

    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_nn_a
    inc hl
    ld (@ldir.to.far.page+1),hl
    inc hl
    inc hl
    ld (hl),opcode.ld_a_b
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (hl),ldir.far.buffer \ 256
    inc hl
    ld (hl),ldir.far.buffer / 256
    inc hl

    ld (hl),opcode.ld_b_n
    inc hl
    inc hl

    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.ldir
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ex de,hl
@ldir.to.far.page:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.ret
    inc hl

ldir.to.far.size:   equ 19

;---------------------------------------------------------------

; play tables (208*2*2)    if QSS -> 208*4*2

;---------------------------------------------------------------
; interrupt function
no.function:
    ld de,0           ; after playtab2 (QSS or not)
mk.sto2:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

no.func.wait:
    ld a,0

ins.nf.nop:
    cp 3
    jr c,ins.nf.notjr
    ld (hl),opcode.jr_n
    inc hl
    inc hl
    sub 3
    jr ins.nf.nop
ins.nf.notjr:
    or a
    jr z,ins.nf.all
ins.nf.lp:
    ld (hl),opcode.nop
    inc hl
    dec a
    jr nz,ins.nf.lp
ins.nf.all:

    call insert.outs
    ld (hl),opcode.exx
    inc hl
    ld (hl),opcode.ex_af_af
    inc hl
    ld (hl),opcode.ei
    inc hl
    ld (hl),opcode.ret
    inc hl

no.function2:
    ex de,hl
mk.sto2.1:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

no.func.wait2:
    ld a,0

ins.nf.nop2:
    cp 3
    jr c,ins.nf.notjr2
    ld (hl),opcode.jr_n
    inc hl
    inc hl
    sub 3
    jr ins.nf.nop2
ins.nf.notjr2:
    or a
    jr z,ins.nf.all2
ins.nf.lp2:
    ld (hl),opcode.nop
    inc hl
    dec a
    jr nz,ins.nf.lp2
ins.nf.all2:

    call insert.outs
    ld (hl),opcode.exx
    inc hl
    ld (hl),opcode.ex_af_af
    inc hl
    ld (hl),opcode.ei
    inc hl
    ld (hl),opcode.halt
    inc hl
    ld (hl),opcode.ret
    inc hl

;---------------------------------------------------------------
border.player.1:

; all get data routines included in here
;---------------------------------------------------------------

mk.timing:
    ld ix,0                 ; IX = timing.<device>

    ld a,opcode.nop         ; reset error on out counter
    ld (insert.xout),a

    ld a,(ix)               ; A = number of operations until sample out
    inc ix

    ex de,hl                ; borderplay1:
mk.sto5:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    ld (mk.rec32+1),hl
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto10+1),hl      ; ld ix,bord.pl11
    inc hl
    inc hl
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto24+1),hl
    inc hl                  ; jp get.c1.data
    inc hl

;=====----- get channel 1 data
get.c1.data:

; hla = sample pointer (hl = address, a = fraction)
; spc = speed (sp = bytes, c = fraction)
; d   = volume table high byte

; result from channel 1 put into channel 2 player which
; "mixes" (adds 7 bit + 7 bit in case of samdac) and puts into playback buffer

    ; !!! a (t-state counter) has no meaningful value here


    ld (mk.get.c1.data+1),hl
    ex de,hl
mk.sto24:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan1.page),hl  ; c1.page:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (bp.chan1.offs),hl  ; c1.offset:
    inc hl
    inc hl

    call select.page

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_d_n           ; d -> volume.table
    inc hl
    ld (bp.chan1.vol),hl ; c1.tab:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_c_n
    inc hl
    ld (bp.chan1.speedlo),hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_sp_nn
    inc hl
    ld (bp.chan1.speedhi),hl ; c1.speedhi:
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan1.sp.frct),hl ; c1.sp.frct:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.jp_ix
    inc hl

;=====----- end get channel 1 data

    ld (mk.ix1+2),ix
    ld (mk.a1+1),a
    ld e,a
    ld a,(@counter+1)
    ld (mk.c1+1),a
    ld a,e

mk.sto10:
    ld de,0
    ex de,hl
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    call mk.bp1.chan1

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto11+1),hl  ; ld ix,bord.pl14
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto25+1),hl
    inc hl              ; jp get.c4.data
    inc hl

; =====----- start get channel 4 data

; fetch channel 4 data

    ld (mk.getc4data+1),hl
    ex de,hl
mk.sto25:
    ld hl,0             ; get.c4.data:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan4.page),hl  ; c4.pag:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (bp.chan4.offs),hl  ; c4.off:
    inc hl
    inc hl

    call select.page

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_d_n
    inc hl
    ld (bp.chan4.vol),hl   ; c4.tab:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_c_n
    inc hl
    ld (bp.chan4.speedlo),hl   ; c4.speedlo:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_sp_nn
    inc hl
    ld (bp.chan4.speedhi),hl   ; c4.speedhi:
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan4.sp.frct),hl   ; c4.sp.frct:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.jp_ix
    inc hl

;=====----- end get channel 4 data

    ld (mk.ix4+2),ix
    ld (mk.a4+1),a
    ld e,a
    ld a,(@counter+1)
    ld (mk.c4+1),a
    ld a,e

mk.sto11:
    ld de,0
    ex de,hl
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    call mk.bp1.chan4

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto12+1),hl      ; ld ix,bordpl1f
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.paltabsel+1),hl
    inc hl                  ; jp paltabselect
    inc hl

; =====----- start paltabselect

; frame palette and screen select

    ld (mk.paltabselr+1),hl
    ex de,hl
mk.paltabsel:
    ld hl,0             ; paltabselect
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (hl),( frame.palette + 15 ) \ 0x100
    inc hl
    ld (hl),( frame.palette + 15 ) / 0x100
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_bc_nn
    inc hl
    ld (hl),port.clut
    inc hl
    ld (hl),16                  ; ld bc,16*256+port.clut
    inc hl

    ld b,16

    push af
    ld a,(burstplayer.device)
    cp device.clut
    jr nz,@not.clut
    dec b                       ; palette 0 = sample output, so do not set

@not.clut:
    pop af

@mk.outd:

    cp 5
    call c,insert.xout
    sub 5
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.outd         ; outd (16*)
    inc hl

    djnz @mk.outd

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.vmpr
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ld_e_a
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_a_nn
    inc hl
    ld (hl),frame.screen \ 256
    inc hl
    ld (hl),frame.screen / 256
    inc hl

    cp 1+3
    call c,insert.xout
    sub 1+3
    ld (hl),opcode.and_a
    inc hl
    ld (hl),opcode.jr_nz_n
    inc hl
    ld (hl),1               ; jr nz,$+3
    inc hl
    ld (hl),opcode.ld_a_e
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.vmpr
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.jp_ix
    inc hl

;=====----- end paltabselect

    ld (mk.ixp+2),ix
    ld (mk.ap+1),a
    ld e,a
    ld a,(@counter+1)
    ld (mk.cp+1),a
    ld a,e

    ex de,hl
mk.sto12:
    ld hl,0             ; bordpl1f:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto13+1),hl  ; ld ix,bord.pl12
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto26+1),hl
    inc hl              ; jp get.c2.data
    inc hl

;=====----- start get channel 2 data

; fetch channel 2 data

    ld (mk.getc2data+1),hl
    ex de,hl
mk.sto26:
    ld hl,0             ; get.c2.data:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan2.page),hl  ; c2.pag:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (bp.chan2.offs),hl  ; c2.off:
    inc hl
    inc hl

    call select.page

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_d_n
    inc hl
    ld (bp.chan2.vol),hl   ; c2.tab:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_c_n
    inc hl
    ld (bp.chan2.speedlo),hl ; c2.speedlo:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_sp_nn
    inc hl
    ld (bp.chan2.speedhi),hl   ; c2.speedhi:
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan2.sp.frct),hl   ; c2.sp.frct:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.jp_ix
    inc hl

;=====----- end get channel 2 data

    ld (mk.ix2+2),ix
    ld (mk.a2+1),a
    ld e,a
    ld a,(@counter+1)
    ld (mk.c2+1),a
    ld a,e

    ex de,hl
mk.sto13:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    call mk.bp1.chan2

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto14+1),hl ;ld ix,bord.pl13
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto27+1),hl
    inc hl              ;jp get.c3.data
    inc hl

;=====----- start get channel 3 data

;fetch channel 3 data

    ld (mk.getc3data+1),hl
    ex de,hl
mk.sto27:
    ld hl,0            ;get.c3.data:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan3.page),hl  ; c3.pag:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (bp.chan3.offs),hl  ; c3.off:
    inc hl
    inc hl

    call select.page

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_d_n
    inc hl
    ld (bp.chan3.vol),hl   ; c3.tab:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_c_n
    inc hl
    ld (bp.chan3.speedlo),hl   ; c3.speedlo:
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_sp_nn
    inc hl
    ld (bp.chan3.speedhi),hl   ; c3.speedhi:
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.chan3.sp.frct),hl   ; c3.sp.frct:
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.jp_ix
    inc hl

;=====----- end get channel 3 data

    ld (mk.ix3+2),ix
    ld (mk.a3+1),a
    ld e,a
    ld a,(@counter+1)
    ld (mk.c3+1),a
    ld a,e

    ex de,hl
mk.sto14:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    call mk.bp1.chan3

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (mk.sto15+1),hl ;ld hl,borderplay2
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_de_nn
    inc hl
qs.playtab2:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d           ; ld de,playtab2
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto16+1),hl  ; jp player.rejoin
    inc hl
    inc hl

;---------------------------------------------------------------
border.play.2:

    ld a,(maker+1)
    ld (poke.count+1),a
    xor a
    ld (@counter+1),a

    ld a,opcode.nop
    ld (insert.xout),a

    ld ix,(mk.timing+2)
    ld a,(ix)
    inc ix

    ex de,hl
mk.sto15:
    ld hl,0            ;borderplay2:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto17+1),hl ;ld ix,bord.pl21
    inc hl
    inc hl
    ld (hl),opcode.jp_nn
    inc hl
mk.get.c1.data:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d       ; jp get.c1.data
    inc hl
mk.sto17:
    ld de,0
    ex de,hl
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

mk.c1:
    ld a,0
    ld (@counter+1),a
mk.ix1:
    ld ix,0
mk.a1:
    ld a,0

    call mk.bp2.chan1

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto18+1),hl ;ld ix,bord.pl24
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
mk.getc4data:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d          ;jp get.c4.data
    inc hl
mk.sto18:
    ld de,0
    ex de,hl
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

mk.c4:
    ld a,0
    ld (@counter+1),a
mk.ix4:
    ld ix,0
mk.a4:
    ld a,0

    call mk.bp2.chan4

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto19+1),hl ;ld ix,bordpl2f
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
mk.paltabselr:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d          ;jp paltabselect
    inc hl

mk.cp:
    ld a,0
    ld (@counter+1),a
mk.ixp:
    ld ix,0
mk.ap:
    ld a,0


    ex de,hl
mk.sto19:
    ld hl,0            ;bordpl2f:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto20+1),hl ;ld ix,bord.pl22
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
mk.getc2data:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d          ;jp get.c2.data
    inc hl
    ex de,hl
mk.sto20:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

mk.c2:
    ld a,0
    ld (@counter+1),a
mk.ix2:
    ld ix,0
mk.a2:
    ld a,0

    call mk.bp2.chan2

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.ld_ix_nn
    inc hl
    ld (mk.sto21+1),hl ;ld ix,bord.pl23
    inc hl
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
mk.getc3data:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d          ;jp get.c3.data
    inc hl
    ex de,hl
mk.sto21:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

mk.c3:
    ld a,0
    ld (@counter+1),a
mk.ix3:
    ld ix,0
mk.a3:
    ld a,0

    call mk.bp2.chan3

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_hl_nn
    inc hl
mk.rec32:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d          ;ld hl,borderplay1
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_de_nn
    inc hl
    ld (hl),playtab1 \ 256
    inc hl
    ld (hl),playtab1 / 256
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.jp_nn
    inc hl
    ld (mk.sto22+1),hl ;jp player.rejoin
    inc hl
    inc hl

;---------------------------------------------------------------
player.rejoin:

    ex de,hl
mk.sto16:
    ld hl,0            ;player.rejoin:
    ld (hl),e
    inc hl
    ld (hl),d
mk.sto22:
    ld hl,0
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl

    cp 5
    call c,insert.xout
    sub 5
    ld (hl),opcode.ld_nn_hl
    inc hl
mk.rec2:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d          ;ld (playerselect+1),hl
    inc hl

    cp 6
    call c,insert.xout
    sub 6
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.ld_nn_de
    inc hl
    ld (mk.sto23+1),hl ;ld (playtabselect+1),de
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.pointer.page.sequencer),hl
    ; ld (hl),page.sequencer
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    cp 3
    call c,insert.xout
    sub 3
    ld (hl),opcode.ld_sp_nn
    inc hl
    ex de,hl
mk.sto4:
    ld hl,0            ;prog.sp:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl           ;ld sp,0000
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
ras.start.1:
    ld (hl),0          ;ld a,ras.start
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.line_interrupt
    inc hl

    call insert.xout
    cp 255
    call nz,insert.xout

    ld (hl),opcode.exx
    inc hl

    ld (hl),opcode.ex_af_af
    inc hl

    ld (hl),opcode.ld_hl_nn
    inc hl
    ex de,hl
mk.sto23:
    ld hl,0            ;playtabselect:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl           ;ld hl,00000
    inc hl
    inc hl
    ld (hl),opcode.exx
    inc hl

    ld (hl),opcode.ei
    inc hl

    ld (hl),opcode.call_nn
    inc hl
    ld (bp.pointer.addr.sequencer),hl
    inc hl
    inc hl

    ld (hl),opcode.xor_a
    inc hl
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.clut
    inc hl
    ld (hl),opcode.ix
    inc hl
    ld (hl),opcode.pop_ix
    inc hl
    ld (hl),opcode.pop_hl
    inc hl
    ld (hl),opcode.pop_de
    inc hl
    ld (hl),opcode.pop_bc
    inc hl
    ld (hl),opcode.ld_a_n
    inc hl
    ex de,hl
mk.sto3:
    ld hl,0            ;prog.p:
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl           ;ld a,0
    inc hl
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl
    ld (hl),opcode.pop_af
    inc hl
    ld (hl),opcode.ret
    inc hl

;---------------------------------------------------------------
;enable burstplayer
enable:
    ld (bp.pointer.addr.enable),hl      ; enableplayer:

    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.status
    inc hl

    ld (hl),opcode.and_n
    inc hl
    ld (hl),frame.interrupt
    inc hl

    ld (hl),opcode.jr_nz_n
    inc hl
    ld (hl),-6         ;jr nz,$-4
    inc hl

    ld (hl),opcode.in_a_n
    inc hl
    ld (hl),port.status
    inc hl

    ld (hl),opcode.and_n
    inc hl
    ld (hl),frame.interrupt
    inc hl

    ld (hl),opcode.jr_z_n
    inc hl
    ld (hl),-6         ;jr z,$-4
    inc hl

    ld (hl),opcode.ld_hl_nn
    inc hl
    ld de,(mk.rec32+1)
    ld (hl),e
    inc hl
    ld (hl),d          ;ld hl,borderplay1
    inc hl

    ld (hl),opcode.ld_nn_hl
    inc hl
    ld de,(mk.rec2+1)
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (playerselect+1),hl
    inc hl

    ld (hl),opcode.ex_af_af
    inc hl

    ld (hl),opcode.exx
    inc hl

    ld (hl),opcode.ld_bc_nn
    inc hl
sample.port:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d
    inc hl

    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (hl),playtab1 \ 256
    inc hl
    ld (hl),playtab1 / 256 ;ld hl,playtab1
    inc hl

    ld (hl),opcode.ld_de_nn
    inc hl
sample.ctrl:
    ld de,1
    ld (hl),e
    inc hl
    ld (hl),d           ; ld de,sample.ctrl
    inc hl
    ld (hl),opcode.ld_a_n
    inc hl
ras.start.2:
    ld (hl),0           ; ld a,ras.start
    inc hl
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.line_interrupt
    inc hl
    ld (hl),opcode.exx
    inc hl
    ld (hl),opcode.ex_af_af
    inc hl
    ld (hl),opcode.ei
    inc hl
    ld (hl),opcode.ret
    inc hl

;---------------------------------------------------------------
set.silence:

    ld (mk.rec37+1),hl      ; set.silence
    ld (hl),opcode.ld_hl_nn
    inc hl
    ld (hl),playtab1 \ 256
    inc hl
    ld (hl),playtab1 / 256 ; ld hl,playtab1
    inc hl
    ld (hl),opcode.ld_d_h
    inc hl
    ld (hl),opcode.ld_e_l
    inc hl
    ld (hl),opcode.inc_de
    inc hl
    ld (hl),opcode.ld_hl_n
    inc hl
    ld a,(burstplayer.device)
    cp device.saa
    ld a,%10001000      ; saa
    jr z,@is.saa
    ld a,%10000000      ; others
@is.saa:
    ld (hl),a
    inc hl
    ld (hl),opcode.ld_bc_nn
    inc hl
    ld de,8 * 208 - 1
    ld a,(burstplayer.device)
    cp device.quazar
    jr z,$+5
    ld de,4 * 208 - 1
    ld (hl),e
    inc hl
    ld (hl),d         ;ld bc,4*208-1 (8* = QSS)
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.ldir
    inc hl
    ld (hl),opcode.ret
    inc hl

;---------------------------------------------------------------
;test
;
;   call sound.driver.reset
;   call set.silence
;   ld a,page.sequencer
;   out (port.hmpr),a
;   jp addr.demo

    ld (run.program+1),hl
    ld (hl),opcode.call_nn
    inc hl
    ld (hl),sound.driver.reset \ 256
    inc hl
    ld (hl),sound.driver.reset / 256  ;call reset sound dev
    inc hl

    ld (hl),opcode.call_nn
    inc hl
mk.rec37:
    ld de,0
    ld (hl),e
    inc hl
    ld (hl),d               ; call set.silence
    inc hl

    ld (hl),opcode.ld_a_n
    inc hl
    ld (bp.pointer.page.demo),hl
    inc hl

    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.hmpr
    inc hl

    ld (hl),opcode.jp_nn
    inc hl
    ld (bp.pointer.addr.demo),hl
    inc hl
    inc hl

;---------------------------------------------------------------
;swap channel 3 and 4 addresses if device is SAA

    ld a,(burstplayer.device)
    ld (bp.device),a
    cp device.saa
    ret nz

    ld hl,bp.chan3.page
    ld de,bp.chan4.page
    ld b,12
mk.swap.lp:
    ld c,(hl)
    ld a,(de)
    ld (hl),a
    ld a,c
    ld (de),a
    inc hl
    inc de
    djnz mk.swap.lp

    ret                 ;!!!!!!!!!!!!!!!!!!

;---------------------------------------------------------------
select.page:

    push af
    ld a,(burstplayer.external.ram)
    or a
    jr z,@no.megabyte.4

    pop af

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.xmpr.c
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.inc_a
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.xmpr.d
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_n
    inc hl
    ld (hl),high.memory.external
    inc hl

    ret

@no.megabyte.4:

    pop af

    ret

;---------------------------------------------------------------

include "volume.i"


;---------------------------------------------------------------
populate.pitch.table:

; populate pitch table for Amiga -> SAM sample rate
;---------------------------------------------------------------
    ld ix,pitch.table+4

    ; ld b,4
    ; pitch.clp:    ; ld (ix),0         ;for nocalc on lowest two div
    ; inc ix
    ; djnz pitch.clp

    ld hl,43544       ;PAL  -> chl = 7093789.2
    ld a,(burstplayer.amiga)
    cp pal
    jr z,not.ntsc
    ld hl,45152       ;NTSC -> chl = 7159090.5
not.ntsc:
    ld (lo.amiga+1),hl

    ld de,2
    ld c,0

; divide cde by cde'
; put result in (ix+0), (ix+1)

pitch.loop:
    exx

lo.amiga:
    ld de,43544     ; chl = 7093789.2 * 128 / 10400
    ld c,2          ; value in table = hl / offs * 2 for rounding at end
    ld b,24
    exx
; divide:
    ld b,0
    ld hl,0
    exx
mp.divlp1:
    rl e
    rl d
    rl c
    exx
    adc hl,hl
    ld a,b
    adc a,a
    ld b,a
    sbc hl,de
    ld a,b
    sbc a,c
    ld b,a
    jr nc,mp.divskip1
    add hl,de
    ld a,b
    adc a,c
    ld b,a
mp.divskip1:
    ccf
    exx
    djnz mp.divlp1

    ld hl,0
    adc hl,de

    ld (ix),l
    ld (ix+1),h
    inc ix
    inc ix
    exx

    inc de

    ld a,d
    cp 1024 / 256
    jr nz,pitch.loop
    ret

;---------------------------------------------------------------
insert.outs:

; insert output commands for sound device

    ex de,hl
    push bc
sound.driver.address:
    ld hl,0
sound.driver.length:
    ld bc,0
    ldir
    pop bc
    ex de,hl
    ret

; ------------------------------------------------------------------------------
insert.xout:

; insert output commands for sound device, but with timing padding and exx

    nop                     ; set to ret when error

    cp 3
    jr c,ins.notjr
    ld (hl),opcode.jr_n     ; a = 3 or 4 or 5
    inc hl
    ld (hl),0               ; jr $+2
    inc hl
    sub 3
    jr insert.xout

ins.notjr:
    or a
    jr z,ins.allnop

ins.noplp:
    ld (hl),opcode.nop
    inc hl
    dec a
    jr nz,ins.noplp

ins.allnop:
    ld a,(maker+1)
    and 1
    call nz,time

    ld (hl),opcode.exx
    inc hl

    call insert.outs

    ld (hl),opcode.exx
    inc hl

    ld a,(maker+1)
    and 1
    call z,time

    ld a,(ix)
    or a
    jr nz,@nz

    push af
    ld a,opcode.ret
    ld (insert.xout),a  ; stop output when 0 encountered
    pop af

@nz:
    inc a
    jr z,@bad.timing
    dec a

    inc ix
    scf
    ret

@bad.timing:

    ld a,r
    out (port.border),a
    jr @bad.timing


time:
    push af
@counter:
    ld a,0
    inc a
poke.count:
    cp 0
    jr nz,notborder

    ld (hl),opcode.ex_af_af
    inc hl
    ld (hl),opcode.ld_a_n
    inc hl
border.col:
    ld (hl),0x70        ; ld a,3
    inc hl
    ld (hl),opcode.out_n_a
    inc hl
    ld (hl),port.clut
    inc hl
    ld (hl),opcode.ex_af_af
    inc hl

notborder:
    ld (@counter+1),a
    pop af
    ret

;---------------------------------------------------------------
; make channel 1 for burstplayer 1

; there are /two/ burstplayers - while one is playing, the other is being filled

mk.bp1.chan1:
    ld de,playtab1 + ( 2 * 208 )
    ld (mk.playtab),de

    ld de,(bp.chan1.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan1.page)
    ld (mk.gd.page),de

    ld de,(bp.chan1.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get1st

    ld de,playtab1 + ( 4 * 208 ) + 2  ;left
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 4 for burstplayer 1

mk.bp1.chan4:
    ld de,(bp.chan4.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan4.page)
    ld (mk.gd.page),de

    ld de,(bp.chan4.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get2nd

    ld de,playtab1 + 0 + ( 4 * 208 )  ;left
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 2 for burstplayer 1

mk.bp1.chan2:
    ld de,playtab1 + 1 + ( 2 * 208 )   ;playtab2
    ld (mk.playtab),de

    ld de,(bp.chan2.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan2.page)
    ld (mk.gd.page),de

    ld de,(bp.chan2.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get1st

    ld de,playtab1 + 3 + ( 4 * 208 )  ;right
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 3 for burstplayer 1

mk.bp1.chan3:
    ld de,(bp.chan3.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan3.page)
    ld (mk.gd.page),de

    ld de,(bp.chan3.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get2nd

    ld de,playtab1 + 1 + ( 4 * 208 )  ;right
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 1 for burstplayer 2

mk.bp2.chan1:
    ld de,playtab1 + 0
    ld (mk.playtab),de

    ld de,(bp.chan1.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan1.page)
    ld (mk.gd.page),de

    ld de,(bp.chan1.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get1st

    ld de,playtab1 + 2  ;left
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 4 for burstplayer 2

mk.bp2.chan4:
    ld de,(bp.chan4.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan4.page)
    ld (mk.gd.page),de

    ld de,(bp.chan4.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get2nd

    ld de,playtab1+0  ;left
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 2 for burstplayer 2

mk.bp2.chan2:
    ld de,playtab1 + 1
    ld (mk.playtab),de

    ld de,(bp.chan2.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan2.page)
    ld (mk.gd.page),de

    ld de,(bp.chan2.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get1st

    ld de,playtab1 + 3  ;right
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make channel 3 for burstplayer 2

mk.bp2.chan3:
    ld de,(bp.chan3.sp.frct)
    ld (mk.gd.spfr),de

    ld de,(bp.chan3.page)
    ld (mk.gd.page),de

    ld de,(bp.chan3.offs)
    ld (mk.gd.offs),de

    push af
    ld a,(burstplayer.device)
    cp device.quazar
    jp nz,mk.bp.get2nd

    ld de,playtab1+1  ;right
    ld (mk.playtab),de
    jp mk.qss.get

;---------------------------------------------------------------
;make get first sample byte routine

;   d  = volume.table

;   hl = sample pointer
;   a  = sample pointer fraction

;   sp = pitch
;   c  = pitch fraction

;   ld e,(hl)   get byte
;   add a,c
;   adc hl,sp
;   ld b,(hl)   get byte
;   add a,c
;   adc hl,sp
;   ex af,af'
;   ld a,(de)   apply volume
;   ld (nn),a   save for channel mix
;   ld e,b
;   ld a,(de)   apply volume
;   ld (nn),a   save for channel mix
;   ld e,(hl)   get byte
;   ld a,(de)   apply volume
;   ld (nn),a
;   ex af,af'
;   add a,c
;   adc hl,sp

;   REPEAT 68 times ( 3 x 68 = 204 )

;   ld e,hl
;   add a,c
;   adc hl,sp
;   ld (nn),a
;   ld a,(de)
;   ld (nn),a
;   in a,(hmpr)         <- !!!
;   bit 6,h
;   jr z,+3
;   inc a
;   ld (nn),a           <-
;   res 6,h
;   ld (nn),hl

; https://simonowen.com/sam/timings/
;
; cp / call c / sub counters are based on t-states / 4
; a | b = t-states border | screen

mk.bp.get1st:
    pop af
    ld iy,mk.store

    ld b,208 / 3                ; 208 bytes per frame, each iteration outputs 3 sample bytes
@loop1.1:
    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl      ; 8 | 16
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4                     ; combined -> carry flag modified by xout
    ld (hl),opcode.add_a_c      ; 4 | 8
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp    ; 16 | 24
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_b_hl      ; 8 | 16
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c      ; 4 | 8
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp    ; 16 | 24
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ex_af_af     ; 4 | 8
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de      ; 8 | 16
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a      ; 16 | 32
    inc hl
    ld (iy+0),l
    ld (iy+1),h
    inc iy
    inc iy
    inc hl
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ld_e_b       ; 4 | 8
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de      ; 8 | 16
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a      ; 16 | 32
    inc hl
    ld (iy+0),l
    ld (iy+1),h
    inc iy
    inc iy
    inc hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl      ; 8 | 16
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de      ; 8 | 16
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a      ; 16 | 32
    inc hl
    ld (iy+0),l
    ld (iy+1),h
    inc iy
    inc iy
    inc hl
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ex_af_af     ; 4 | 8
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c      ; 4 | 8
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp    ; 16 | 24
    inc hl

    dec b
    jp nz,@loop1.1

;now get last byte ( 208 / 3 = 69 * 3 = 207 ) and store sample pointer

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl      ; 8 | 16
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c      ; 4 | 8
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp    ; 16 | 24
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a      ; 16 | 32
    inc hl
    ld bc,(mk.gd.spfr)
    ld (hl),c
    inc hl
    ld (hl),b
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de      ; 8 | 16
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a      ; 16 | 32
    inc hl
    ld (iy+0),l
    ld (iy+1),h
    inc iy
    inc iy
    inc hl
    inc hl

    jp @update.page.sample

;-------------------------------------------------------------------------------
@update.page.sample:

    push af
    ld a,(burstplayer.external.ram)
    or a
    jr z,@no.megabyte.5

    pop af

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_a_nn      ; 16 | 32
    inc hl
    ld bc,(mk.gd.page)
    ld (hl),c
    inc hl
    ld (hl),b
    inc hl

    jr @continue.5

@no.megabyte.5:

    pop af

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.in_a_n       ; 16 | 24
    inc hl
    ld (hl),port.hmpr
    inc hl

@continue.5:

    cp 2+3
    call c,insert.xout
    sub 2+3
    ld (hl),opcode.cb
    inc hl
    ld (hl),opcode.bit_6_h      ; 8 | 16
    inc hl
    ld (hl),opcode.jr_z_n       ; 12/8 | 16/16 !!! contended timing effected by data
    inc hl
    ld (hl),1                   ; jr z,$+3
    inc hl
    ld (hl),opcode.inc_a        ; 4 | 8
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a      ; 16 | 32
    inc hl
    ld bc,(mk.gd.page)
    ld (hl),c
    inc hl
    ld (hl),b                   ; ld (samplepage+1),a
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.cb
    inc hl
    ld (hl),opcode.res_6_h      ; 8 | 16
    inc hl

    cp 5
    call c,insert.xout
    sub 5
    ld (hl),opcode.ld_nn_hl     ; 20 | 40
    inc hl
    ld bc,(mk.gd.offs)
    ld (hl),c
    inc hl
    ld (hl),b                   ; ld (sample.offs+1),hl
    inc hl

    ret

;---------------------------------------------------------------
;make get second sample byte and add to first routine

;   ld e,(hl)
;   add a,c
;   adc hl,sp
;   ld b,(hl)
;   add a,c
;   adc hl,sp
;   ex af,af'
;   ld a,(de)
;   add a,n     <- from make first sample
;   ld (nn),a   -> buffer
;   ld e,b
;   ld a,(de)
;   add a,n     <- from make first sample
;   ld (nn),a   -> buffer
;   ld e,(hl)
;   ld a,(de)
;   add a,n     <- from make first sample
;   ld (nn),a   -> buffer
;   ex af,af'
;   add a,c
;   adc hl,sp

;   REPEAT 68 times

;   ld e,(hl)
;   add a,c
;   adc hl,sp
;   ld (nn),a   -> store sample pointer fraction
;   ld a,(de)
;   add a,n
;   ld (nn),a   -> buffer
;   in a,(hmpr) !!!
;   bit 6,h
;   jr z,+1
;   inc a
;   ld (nn),a   ->
;   res 6,h
;   ld (nn),a   ->


mk.bp.get2nd:
    pop af
    ld iy,mk.store

    ld b,208 / 3        ; bytes per frame
blp1.4:
    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_b_hl
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ex_af_af
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.add_a_n
    inc hl
    ex de,hl
    ld l,(iy+0)
    ld h,(iy+1)
    inc iy
    inc iy
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (2*x+playtab2),a
    inc hl
    inc de
    inc de
    ld (mk.playtab),de

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ld_e_b
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.add_a_n
    inc hl
    ex de,hl
    ld l,(iy+0)
    ld h,(iy+1)
    inc iy
    inc iy
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (2*x+playtab2),a
    inc hl
    inc de
    inc de
    ld (mk.playtab),de

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.add_a_n
    inc hl
    ex de,hl
    ld l,(iy+0)
    ld h,(iy+1)
    inc iy
    inc iy
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d         ;ld (2*x+playtab2),a
    inc hl
    inc de
    inc de
    ld (mk.playtab),de

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ex_af_af
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    dec b
    jp nz,blp1.4

;now get last byte and store sample pointer

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld bc,(mk.gd.spfr)
    ld (hl),c
    inc hl
    ld (hl),b       ; ld (speedfract+1),a
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.add_a_n
    inc hl
    ex de,hl
    ld l,(iy+0)
    ld h,(iy+1)
    inc iy
    inc iy
    ld (hl),e
    inc hl
    ld (hl),d
    ex de,hl
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d           ; ld (2*x+playtab2),a
    inc hl
    inc de
    inc de
    ld (mk.playtab),de

    jp @update.page.sample

;---------------------------------------------------------------
;make get sample byte for QSS - no mixing -> same routine 4*

mk.qss.get:
    pop af

    ld b,208 / 3        ; bytes per frame
q.blp1.4:
    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_b_hl
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ex_af_af
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d         ;ld (4*x+playtabx),a
    inc hl
    inc de
    inc de
    inc de
    inc de
    ld (mk.playtab),de

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ld_e_b
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d         ; ld (4*x+playtabx),a
    inc hl
    inc de
    inc de
    inc de
    inc de
    ld (mk.playtab),de

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d         ; ld (4*x+playtabx),a
    inc hl
    inc de
    inc de
    inc de
    inc de
    ld (mk.playtab),de

    cp 1
    call c,insert.xout
    sub 1
    ld (hl),opcode.ex_af_af
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    dec b
    jp nz,q.blp1.4

; now get last byte and store sample pointer

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_e_hl
    inc hl

    cp 1+4
    call c,insert.xout
    sub 1+4
    ld (hl),opcode.add_a_c
    inc hl
    ld (hl),opcode.ed
    inc hl
    ld (hl),opcode.adc_hl_sp
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld bc,(mk.gd.spfr)
    ld (hl),c
    inc hl
    ld (hl),b         ; ld (speedfract+1),a
    inc hl

    cp 2
    call c,insert.xout
    sub 2
    ld (hl),opcode.ld_a_de
    inc hl

    cp 4
    call c,insert.xout
    sub 4
    ld (hl),opcode.ld_nn_a
    inc hl
    ld de,(mk.playtab)
    ld (hl),e
    inc hl
    ld (hl),d         ; ld (4*x+playtabx),a
    inc hl

    jp @update.page.sample

;---------------------------------------------------------------
; memory needed for mk.bp routines

mk.playtab: defw 0
mk.gd.spfr: defw 0
mk.gd.page: defw 0
mk.gd.offs: defw 0

mk.store:   defs 208 * 2    ; stores adresses

;===============================================================
list.device.properties:

    defw properties.clut
    defw properties.saa1099
    defw properties.samdac.1
    defw properties.samdac.2
    defw properties.dac.1
    defw properties.dac.2
    defw properties.blue_alpha
    defw properties.quazar

    include "devices/clut.i"
    include "devices/saa1099.i"
    include "devices/samdac.i"
    include "devices/dac.i"
    include "devices/blue_alpha.i"
    include "devices/quazar.i"

;===============================================================
mk.movecode:
    org 0

    di
    in a,(port.vmpr)
;   and %00111111
    push af
    ld (bp.stsp + 1 + 0x8000),sp

    in a,(port.lmpr)
    ld (bp.stlmpr + 1 + 0x8000),a

    in a,(port.hmpr)
    and high.memory.page.mask
    ld (bp.exitpage + 1 + 0x8000),a

    or low.memory.ram.0
    out (port.lmpr),a

    ld sp,0x8000

run.program:
    jp 0        ; test

bp.exit:
    di
    ld a,0xff
    out (port.line_interrupt),a
bp.exitpage:
    ld a,0
    out (port.hmpr),a
    jp bp.stlmpr + 0x8000

bp.stlmpr:
    ld a,0
    out (port.lmpr),a
bp.stsp:
    ld sp,0
    pop af
    out (port.vmpr),a
;   ei
    ret

bp.id:

    defm "BUR"          ; ID code (at 00053)

;---------------------------------------------------------------
    defs 102 - $        ; NMI - corrupts player

    jp bp.exit

; pointers to variables in generated burstplayer code


bp.device:          defb 0

bp.pointers:

bp.pointer.addr.sequencer:  defw 0
bp.pointer.page.sequencer:  defw 0
bp.pointer.addr.demo:       defw 0
bp.pointer.page.demo:       defw 0

bp.pointer.addr.enable:     defw 0
bp.pointer.addr.exit:       defw bp.exit

bp.pointers.sample:

bp.chan1.page:              defw 0
bp.chan1.offs:              defw 0
bp.chan1.vol:               defw 0
bp.chan1.speedlo:           defw 0
bp.chan1.speedhi:           defw 0
bp.chan1.sp.frct:           defw 0

;bp.pointers.length: equ $ - bp.pointers.sample

bp.chan2.page:              defw 0
bp.chan2.offs:              defw 0
bp.chan2.vol:               defw 0
bp.chan2.speedlo:           defw 0
bp.chan2.speedhi:           defw 0
bp.chan2.sp.frct:           defw 0

bp.chan3.page:              defw 0
bp.chan3.offs:              defw 0
bp.chan3.vol:               defw 0
bp.chan3.speedlo:           defw 0
bp.chan3.speedhi:           defw 0
bp.chan3.sp.frct:           defw 0

bp.chan4.page:              defw 0
bp.chan4.offs:              defw 0
bp.chan4.vol:               defw 0
bp.chan4.speedlo:           defw 0
bp.chan4.speedhi:           defw 0
bp.chan4.sp.frct:           defw 0


if 0 > 1
; set saa for samples
bp.saa.init:    defb saa.register.sound_enable        , saa.se.enabled
                defb saa.register.envelope_generator_1, saa.envelope.enabled | saa.envelope.mode.maximum
                defb saa.register.envelope_generator_0, saa.envelope.enabled | saa.envelope.mode.maximum
endif

mk.mv.end:
sound.driver.reset:

length: equ  mk.movecode - 0x8000 + sound.driver.reset

if defined( testing )

;   testing will test burstplayer only with dummy sequencer and demo

    include "ports/keyboard.i"

;===============================================================================

; rasterline = 384 T-states / 4 = 96 * 1.5 = 144
; bytes per frame = 10400 Hz / 50 = 208
; 192 screen lines
; 120 border lines (68 top, 52 bottom)
; 312 total lines / 208 = 1.5

;===============================================================================

    autoexec

    org $ + mk.movecode

    print $                 ; set breakpoint at this address if needed

;-------------------------------------------------------------------------------

    ld a,device.samdac.1
    ld (burstplayer.device),a

    ld a,1
    ld (burstplayer.external.ram),a

    call burstplayer.create

    in a,(port.hmpr)
    and high.memory.page.mask
    or low.memory.ram.0
    out (port.lmpr),a

    jp @test.low

;-------------------------------------------------------------------------------

    org $ - 0x8000

@test.low:

    in a,(port.hmpr)
    ld c,a

    ld a,page.burstplayer
    out (port.hmpr),a

    ; set sequencer

    ld hl,(bp.pointer.page.sequencer + 0x8000)
    set 7,h
    ld (hl),c

    ld hl,(bp.pointer.addr.sequencer + 0x8000)
    set 7,h
    ld (hl),@test.sequencer \ 256
    inc hl
    ld (hl),@test.sequencer / 256

    ; set demo

    ld hl,(bp.pointer.page.demo + 0x8000)
    set 7,h
    ld (hl),c

    ld hl,(bp.pointer.addr.demo + 0x8000)
    set 7,h
    ld (hl),@test.demo \ 256
    inc hl
    ld (hl),@test.demo / 256

    ld hl,frame.palette
    set 7,h
    ex de,hl
    ld hl,@test.palette
    ld bc,0x10
    ldir

    ; start

    call burstplayer.start

    halt

    ret

;-------------------------------------------------------------------------------
@test.palette:
    ;     GRB!grb         pen
    defb %0000000   ;     0

    defb %0011101   ; 3 1 1 BLUE + green
    defb %1011001   ; 3 2 2
    defb %1011101   ; 3 3 3

    defb %0101110   ; 3 1 4 RED + green
    defb %1101010   ; 3 2 5
    defb %1101110   ; 3 3 6

    defb %1001101   ; 3 1 7 GREEN + blue

    defb %0000000   ;     8 bright background

    defb %1011100   ; 3 2 9
  ; defb %1011101   ; 3 3   same as pen 3

    defb %0101011   ; 3 1 A RED + blue
    defb %0111010   ; 3 2 B
    defb %0111011   ; 3 3 C

    defb %1001110   ; 3 1 D GREEN + red
    defb %1101100   ; 3 2 E
  ; defb %1101110   ; 3 3   same as pen 6

    defb %1110111 ;    F

;-------------------------------------------------------------------------------

    org $ + 0x8000

@test.demo:

    call @enable.burst

@loop:
    inc a
    and %1111
    out (port.border),a
    jr @loop

@enable.burst:
    ld hl,(bp.pointer.addr.enable)
    jp (hl)

;-------------------------------------------------------------------------------

@test.sequencer:

    ; set sample pointers to 0x8000

    ld hl,(bp.chan1.offs)
    ld (hl),0x00
    inc hl
    ld (hl),0x80

    ld hl,(bp.chan2.offs)
    ld (hl),0x00
    inc hl
    ld (hl),0x80

    ld hl,(bp.chan3.offs)
    ld (hl),0x00
    inc hl
    ld (hl),0x80

    ld hl,(bp.chan4.offs)
    ld (hl),0x00
    inc hl
    ld (hl),0x80

    ret

endif
