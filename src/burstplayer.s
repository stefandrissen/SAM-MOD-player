;SAM MOD player - MAKE burstplayer 

;(C) 1995-2019 Stefan Drissen

;---------------------------------------------------------------

include "memory.i"
include "ports.i"
include "saa1099.i"
include "opcodes.i"

;---------------------------------------------------------------

;samdac routine uses most memory!
;quazar needs largest playback buffers

count:		equ 255		;number of outs before border change
linetest:	equ 0

;---------------------------------------------------------------

	org &8000

;---------------------------------------------------------------

burstplayer.create:	jp @go.burst

;---------------------------------------------------------------

burstplayer.device:		defb 0	; [0-7]

	device.clut:		equ 0
	device.saa:			equ 1
	device.samdac.1:	equ 2
	device.samdac.2:	equ 3
	device.dac.1:		equ 4
	device.dac.2:		equ 5
	device.bluealpha:	equ 6
	device.quazar:		equ 7
 
burstplayer.speed:		defb 0	; [0-1]

	pal:		equ 0		;use which Amiga to calculate sample
	ntsc:		equ 1		;speeds

burstplayer.external.ram:	defb 0	; [0-4]

burstplayer.page:		defb page.burstplayer

	defm "                         "
	defm "MAKEBURST (C)2019 Stefan Drissen"
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
	ld sp,49152
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
	ld bc,32767
	ld (hl),l
	ldir

	ld hl,playtab1 + ( 2 * 208 )
	ld bc,2 * 208
	ld a,(burstplayer.device)
	cp device.quazar			; if QSS -> playtab2 higher
	jr nz,not.qss.pt
	add hl,bc
	ld bc,4 * 208
not.qss.pt:
	ex af,af'
	ld a,l
	ld (qs.playtab2.1+1),a
	ld a,h
	ld (qs.playtab2.2+1),a
	add hl,bc
	ld (no.function+1),hl
	ex af,af'

	ld hl,device.list	
	add a,a
	ld e,a
	add a,a
	add a,a
	add a,a
	add a,e		; * 18
	add a,l
	ld l,a
	jr nc,$+3
	inc h
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
	ld e,(hl)
	inc hl
	ld d,(hl)
	jr @ok
		
@no.megabyte:
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	inc hl
@ok:	
	inc hl
	ld (mk.timing+2),de
	
	ex de,hl
	ld bc,129
	add hl,bc
	ld a,(hl)
	ld (no.func.wait+1),a
	
	sub 15	;+1 for jr being done
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
;interrupt routine (00056)
interrupt:
	ld hl,00056
	ld (hl),opcode.ex_af_af
	inc hl
	ld (hl),opcode.exx
	inc hl
	call insert.outs
	ld (hl),opcode.cp_n
	inc hl
	ld (hl),191 - 3		
	inc hl
	ld (hl),opcode.jr_z_n
	inc hl
	ld (mk.sto1+1),hl		;jr z,prep.bord.play
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
	ld (mk.sto2+1),hl		;jp no.function
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
	ld (hl),d         ;ld (jr+1),a
	inc hl
	ld (hl),opcode.ld_a_n
	inc hl
	ld (hl),191
	inc hl
	ld (hl),opcode.ld_nn_a
	inc hl
	dec de
	dec de
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (cp191+1),a
	inc hl

	ld (hl),opcode.out_n_a
	inc hl
	ld (hl),port.line_interrupt
	inc hl
	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.sto2.1+1),hl ;jp nofunction2
	inc hl
	inc hl

;---------------------------------------------------------------
;reset output device

	ld de,sound.driver.reset
sound.driver.reset.address:
	ld hl,0
sound.driver.reset.length:
	ld bc,0
	ld a,b
	or c
	jr z,ro.no.silence
	ldir
ro.no.silence:
	ex de,hl
	ld (hl),opcode.ret
	inc hl

;---------------------------------------------------------------
;select border player

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
	pop hl             ;prep.bord.play:
	ld (hl),opcode.ld_a_n
	inc hl
mk.stojr188:
	ld (hl),0		;ld a,jr188
	inc hl
	ld (hl),opcode.ld_nn_a
	inc hl
	ld de,(mk.recjradd+1)
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (jradd+1),a
	inc hl
	ld (hl),opcode.ld_a_n
	inc hl
	ld (hl),188
	inc hl
	dec de
	dec de
	ld (hl),opcode.ld_nn_a
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (cp188+1),a
	inc hl

	ld (hl),opcode.in_a_n
	inc hl
	ld (hl),port.hmpr
	inc hl
	ld (hl),opcode.ld_nn_a
	inc hl
	ld (mk.sto3+1),hl			;ld (prog.p+1),a
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
	ld (mk.sto4+1),hl			;ld (prog.sp+1),sp
	inc hl
	inc hl
	ld (mk.rec2+1),hl ;playerselect:
	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.sto5+1),hl ;jp borderplay1
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
	ld (hl),mod.current.row // 256
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
	
get.pattern.size:	equ 49

;---------------------------------------------------------------
;call far routine, C=page, HL=address

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
	ld (hl),d								;ld (call+1),hl
	inc hl
	
	ld (hl),opcode.call_nn
	inc hl
	inc hl									;call hl
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

far.call.size:	equ 19

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

	ld a,(burstplayer.external.ram)
	or a
	jr z,@no.megabyte.2

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
	
	ld bc,1

	jr @continue.2
		
@no.megabyte.2:

	ld bc,8
	
@continue.2: 

	ld (hl),opcode.out_n_a
	inc hl
	ld (hl),port.hmpr
	inc hl

	ld (hl),opcode.ld_de_nn
	inc hl
	ld (hl),ldir.far.buffer \ 256
	inc hl
	ld (hl),ldir.far.buffer // 256
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
	
	add hl,bc

ldir.from.far.size:	equ 27

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
	
	ld a,(burstplayer.external.ram)
	or a
	jr z,@no.megabyte.3

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
	
@no.megabyte.3:

	ld (hl),opcode.out_n_a
	inc hl
	ld (hl),port.hmpr
	inc hl

	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (hl),ldir.far.buffer \ 256
	inc hl
	ld (hl),ldir.far.buffer // 256
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
	
ldir.to.far.size:	equ 27

;---------------------------------------------------------------

;play tables (208*2*2)    if QSS -> 208*4*2

;---------------------------------------------------------------
;interrupt function
no.function:
	ld de,0           ;after playtab2 (QSS or not)
mk.sto2:
	ld hl,0
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl

no.func.wait:
	ld e,0

	ld a,linetest
	or a
	ld a,0
	jr z,$+4
	ld a,-4
	add a,e

ins.nf.nop:
	cp 3
	jr c,ins.nf.notjr
	ld (hl),opcode.jr_n
	inc hl
	; ld (hl),0		;jr $+2
	inc hl
	sub 3
	jr   ins.nf.nop
ins.nf.notjr:
	or a
	jr z,ins.nf.all
ins.nf.lp:
	; ld (hl),opcode.nop
	inc hl
	dec a
	jr nz,ins.nf.lp
ins.nf.all:

	; ld a,linetest
    ; or a
    ; call nz,timeline

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
	ld e,0

	ld a,linetest
	or a
	ld a,0
	jr z,$+4
	ld a,-4
	add a,e

ins.nf.nop2:
	cp 3
	jr c,ins.nf.notjr2
	ld (hl),opcode.jr_n
	inc hl
	; ld (hl),0		;jr $+2
	inc hl
	sub 3
	jr   ins.nf.nop2
ins.nf.notjr2:
	or a
	jr z,ins.nf.all2
ins.nf.lp2:
	; ld (hl),opcode.nop
	inc hl
	dec a
	jr nz,ins.nf.lp2
ins.nf.all2:

	; ld a,linetest
	; or a
	; call nz,timeline

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
;border player 1  - all get data routines included in here
border.play.1:

mk.timing:
	ld ix,0					; IX = timing.<device>
	
	ld a,opcode.nop
	ld (insert.xout),a
	
	ld a,(ix)				; A = number of operations until sample out
	inc ix

	ex de,hl				; borderplay1:
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
	ld (mk.sto10+1),hl		; ld ix,bord.pl11
	inc hl
	inc hl
	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.sto24+1),hl
	inc hl					; jp get.c1.data
	inc hl

;=====----- get channel 1 data
get.c1.data:

; hla = sample pointer (hl = address, a = fraction)
; spc = speed (sp = bytes, c = fraction)
; d = volume table high byte

; result from channel 1 put into channel 2 player which 
; "mixes" (adds 7 bit + 7 bit in case of samdac) and puts into playback buffer

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
	ld (bp.c1.page),hl	;c1.page:
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (bp.c1.offs),hl	;c1.off:
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

	ld (hl),opcode.ld_d_n			; d -> volume.table
	inc hl
	ld (bp.c1.vol),hl ;c1.tab:
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_c_n
	inc hl
	ld (bp.c1.speedlo),hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_sp_nn
	inc hl
	ld (bp.c1.speedhi),hl ;c1.speedhi:
	inc hl
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.c1.sp.frct),hl ;c1.sp.frct:
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

	call mk.bp11         ;inc "bp11"

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode.ix
	inc hl
	ld (hl),opcode.ld_ix_nn
	inc hl
	ld (mk.sto11+1),hl ;ld ix,bord.pl14
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.sto25+1),hl
	inc hl              ;jp get.c4.data
	inc hl

;=====----- start get channel 4 data

;fetch channel 4 data

	ld (mk.getc4data+1),hl
	ex de,hl
mk.sto25:
	ld hl,0            ;get.c4.data:
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.c4.page),hl ;c4.pag:
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (bp.c4.offs),hl ;c4.off:
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
	ld (bp.c4.vol),hl ;c4.tab:
	; ld (hl),32         ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_c_n
	inc hl
	ld (bp.c4.speedlo),hl ;c4.speedlo:
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_sp_nn
	inc hl
	ld (bp.c4.speedhi),hl ;c4.speedhi:
	inc hl
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.c4.sp.frct),hl ;c4.sp.frct:
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

	call mk.bp14         ;inc "bp14"

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode.ix
	inc hl
	ld (hl),opcode.ld_ix_nn
	inc hl
	ld (mk.sto12+1),hl ;ld ix,bordpl1f
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.paltabsel+1),hl
	inc hl              ;jp paltabselect
	inc hl

;=====----- start paltabselect

;frame palette and screen select

	ld (mk.paltabselr+1),hl
	ex de,hl
mk.paltabsel:
	ld hl,0            ;paltabselect
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (hl),( frame.palette + 15 ) \ 256
	inc hl
	ld (hl),( frame.palette + 15 ) // 256
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_bc_nn
	inc hl
	ld (hl),port.clut
	inc hl
	ld (hl),16					; ld bc,16*256+port.clut
	inc hl

	ld b,16

	push af
	ld a,(burstplayer.device)
	cp device.clut
	jr nz,@not.clut
	dec b						; palette 0 = sample output, so do not set
	 
@not.clut:
	pop af		
		
@mk.outd:

	cp 6
	call c,insert.xout
	sub 6

	ld (hl),opcode.ed
	inc hl
	ld (hl),opcode.outd			; outd (16*)
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
	ld (hl),frame.screen // 256
	inc hl

	cp 1+3
	call c,insert.xout
	sub 1+3

	ld (hl),opcode.and_a
	inc hl
	ld (hl),opcode.jr_nz_n
	inc hl
	ld (hl),1				;jr nz,$+3
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
	ld hl,0            ;bordpl1f:
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
	ld (mk.sto13+1),hl ;ld ix,bord.pl12
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.sto26+1),hl
	inc hl              ;jp get.c2.data
	inc hl

;=====----- start get channel 2 data

;fetch channel 2 data

	ld (mk.getc2data+1),hl
	ex de,hl
mk.sto26:
	ld hl,0            ;get.c2.data:
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.c2.page),hl ;c2.pag:
	; ld (hl),4          ;ld a,4
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (bp.c2.offs),hl ;c2.off:
	; ld (hl),0
	inc hl
	; ld (hl),128        ;ld hl,32768
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
	ld (bp.c2.vol),hl ;c2.tab:
	; ld (hl),32         ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_c_n
	inc hl
	ld (bp.c2.speedlo),hl ;c2.speedlo:
	; ld (hl),128        ;ld c,128
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_sp_nn
	inc hl
	ld (bp.c2.speedhi),hl ;c2.speedhi:
	; ld (hl),1
	inc hl
	; ld (hl),0          ;ld sp,1
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.c2.sp.frct),hl ;c2.sp.frct:
	; ld (hl),0          ;ld a,0
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

	call mk.bp12         ;inc "bp12"

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
	ld (bp.c3.page),hl ;c3.pag:
	; ld (hl),4          ;ld a,4
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (bp.c3.offs),hl ;c3.off:
	; ld (hl),0
	inc hl
	; ld (hl),128        ;ld hl,32768
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
	ld (bp.c3.vol),hl ;c3.tab:
	; ld (hl),32         ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_c_n
	inc hl
	ld (bp.c3.speedlo),hl ;c3.speedlo:
	; ld (hl),128        ;ld c,128
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.ld_sp_nn
	inc hl
	ld (bp.c3.speedhi),hl ;c3.speedhi:
	; ld (hl),1
	inc hl
	; ld (hl),0          ;ld sp,1
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.c3.sp.frct),hl ;c3.sp.frct:
	; ld (hl),0          ;ld a,0
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

	call mk.bp13         ;inc "bp13"

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
qs.playtab2.1:
	ld (hl),0
	inc hl
qs.playtab2.2:
	ld (hl),0			;ld de,playtab2
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode.jp_nn
	inc hl
	ld (mk.sto16+1),hl ;jp player.rejoin
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
	ld (hl),d		; jp get.c1.data
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

	call mk.bp21         ;inc "bp21"

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

	call mk.bp24         ;inc "bp24"

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

	call mk.bp22         ;inc "bp22"

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

	call mk.bp23         ;inc "bp23"

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
	ld (hl),playtab1 // 256
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
	inc de
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
	; ld (hl),0
	inc hl
    ; ld (hl),0          ;call sequencer
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
	ld (bp.pointer.addr.enable),hl		;enableplayer:

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
	inc de
	ld (hl),e
	inc hl
	ld (hl),d          ;ld (playerselect+1),hl
	inc hl
	ld (hl),opcode.ex_af_af
	inc hl
	ld (hl),opcode.exx
	inc hl
	ld (hl),opcode.ld_bc_nn
	inc hl
sample.port:
	ld de,232			; unnecessary default?
	ld (hl),e
	inc hl
	ld (hl),d          ;ld bc,sample.port
	inc hl
	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (hl),playtab1 \ 256
	inc hl
	ld (hl),playtab1 // 256 ;ld hl,playtab1
	inc hl
	ld (hl),opcode.ld_de_nn
	inc hl
sample.ctrl:
	ld de,1
	ld (hl),e
	inc hl
	ld (hl),d          ;ld de,sample.ctrl
	inc hl
	ld (hl),opcode.ld_a_n
	inc hl
ras.start.2:
	ld (hl),0			; ld a,ras.start
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

	ld (mk.rec37+1),hl		; set.silence
	ld (hl),opcode.ld_hl_nn
	inc hl
	ld (hl),playtab1 \ 256
	inc hl
	ld (hl),playtab1 // 256	; ld hl,playtab1
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
	ld a,%10001000		; saa
	jr z,@is.saa
	ld a,%10000000		; others		
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
;	call sound.driver.reset
;	call set.silence
;	ld a,page.sequencer
;	out (port.hmpr),a
;	jp addr.demo

	ld (run.program+1),hl
	ld (hl),opcode.call_nn
	inc hl
	ld (hl),sound.driver.reset \ 256
	inc hl
	ld (hl),sound.driver.reset // 256  ;call reset sound dev
	inc hl
	ld (hl),opcode.call_nn
	inc hl
mk.rec37:
	ld de,0
	ld (hl),e
	inc hl
	ld (hl),d				;call set.silence
	inc hl

	ld (hl),opcode.ld_a_n
	inc hl
	ld (bp.pointer.page.demo),hl
	; ld (hl),page.sequencer
	inc hl
	ld (hl),opcode.out_n_a
	inc hl
	ld (hl),port.hmpr
	inc hl
	ld (hl),opcode.jp_nn
	inc hl
	ld (bp.pointer.addr.demo),hl
	; ld (hl),0
	inc hl
	; ld (hl),0
	inc hl

;---------------------------------------------------------------
;swap channel 3 and 4 addresses if device is SAA

	ld a,(burstplayer.device)
	ld (bp.device),a
	cp device.saa
	ret nz

	ld hl,bp.c3.page
	ld de,bp.c4.page
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

;populate pitch table for Amiga -> SAM sample rate
;---------------------------------------------------------------
	ld ix,pitch.table+4

	; ld b,4
	; pitch.clp:	; ld (ix),0         ;for nocalc on lowest two div
	; inc ix
	; djnz pitch.clp

	ld hl,43544       ;PAL  -> CHL=7093789.2
	ld a,(burstplayer.speed)
	cp pal
	jr z,not.ntsc
	ld hl,45152       ;NTSC -> CHL=7159090.5
not.ntsc:
	ld (lo.amiga+1),hl

	ld de,2
	ld c,0

;divide CDE by CDE'
;put result in (IX+0), (ix+1)

pitch.loop:
	exx

lo.amiga:
	ld de,43544		;CHL=7093789.2*128/10400
	ld c,2			;value in table = HL / offs * 2 for rounding at end
	ld b,24
	exx
;divide:
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
	cp 1024 // 256
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

;---------------------------------------------------------------
insert.xout:

; insert output commands for sound device, but with timing padding and exx 

	ret

	cp 3
	jr c,ins.notjr
	ld (hl),opcode.jr_n		;a = 3 or 4 or 5
	inc hl
	ld (hl),0				;jr $+2
	inc hl
	sub 3
	jr   insert.xout
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
	ld (insert.xout),a	; stop output when 0 encountered
	pop af
	
@nz:		
	inc a
;	jr z,@bad.timing
	dec a
	
	inc ix
	scf
	ret

@bad.timing:

	ld a,r
	out (254),a
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
	ld (hl),&70       ;ld a,3
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

mk.bp11:
	ld de,playtab1 + ( 2 * 208 )
	ld (mk.playtab),de

	ld de,(bp.c1.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c1.page)
	ld (mk.gd.page),de

	ld de,(bp.c1.offs)
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

mk.bp14:
	ld de,(bp.c4.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c4.page)
	ld (mk.gd.page),de

	ld de,(bp.c4.offs)
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

mk.bp12:
	ld de,playtab1 + 1 + ( 2 * 208 )   ;playtab2
	ld (mk.playtab),de

	ld de,(bp.c2.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c2.page)
	ld (mk.gd.page),de

	ld de,(bp.c2.offs)
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

mk.bp13:
	ld de,(bp.c3.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c3.page)
	ld (mk.gd.page),de

	ld de,(bp.c3.offs)
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

mk.bp21:
	ld de,playtab1 + 0
	ld (mk.playtab),de

	ld de,(bp.c1.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c1.page)
	ld (mk.gd.page),de

	ld de,(bp.c1.offs)
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

mk.bp24:
	ld de,(bp.c4.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c4.page)
	ld (mk.gd.page),de

	ld de,(bp.c4.offs)
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

mk.bp22:
	ld de,playtab1 + 1
	ld (mk.playtab),de

	ld de,(bp.c2.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c2.page)
	ld (mk.gd.page),de

	ld de,(bp.c2.offs)
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

mk.bp23:
	ld de,(bp.c3.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c3.page)
	ld (mk.gd.page),de

	ld de,(bp.c3.offs)
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

;	d = volume.table

;	hl = sample pointer
;	a  = sample pointer fraction

;	sp = pitch
;	c  = pitch fraction

;	ld e,(hl)
;	add a,c
;	adc hl,sp
;	ld b,(hl)
;	add a,c
;	adc hl,sp
;	ex af,af'
;	ld a,(de)	
;	ld (nn),a	save for channel mix
;	ld e,b
;	ld a,(de)	
;	ld (nn),a	save for channel mix
;	ld e,(hl)
;	ld a,(de)
;	ld (nn),a
;	ex af,af'
;	add a,c
;	adc hl,sp

;	REPEAT 68 times

;	ld e,hl
;	add a,c
;	adc hl,sp
;	ld (nn),a
;	ld a,(de)
;	ld (nn),a
;	in a,(hmpr)			<- !!!
;	bit 6,h
;	jr z,+3
;	inc a
;	ld (nn),a			<-
;	res 6,h
;	ld (nn),hl

mk.bp.get1st:
	pop af
	ld iy,mk.store

	ld b,208 // 3		; bytes per frame
@loop1.1:
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
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	inc hl
	inc hl

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
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	inc hl
	inc hl

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
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	inc hl
	inc hl

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
	jp nz,@loop1.1

;now get last byte ( 208 / 3 = 69 * 3 = 207 ) and store sample pointer

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
	ld (hl),b
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
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	inc hl
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	push af
	ld a,(burstplayer.external.ram)
	or a
	jr z,@no.megabyte.5
	
	pop af

	ld (hl),opcode.ld_a_nn
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl

	jr @continue.5

@no.megabyte.5:

	pop af

	ld (hl),opcode.in_a_n
	inc hl
	ld (hl),port.hmpr
	inc hl

@continue.5:

	cp 2+3
	call c,insert.xout
	sub 2+3

	ld (hl),opcode.cb
	inc hl
	ld (hl),opcode.bit_6_h
	inc hl
	ld (hl),opcode.jr_z_n
	inc hl
	ld (hl),1				; jr z,$+3
	inc hl
	ld (hl),opcode.inc_a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode.ld_nn_a
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (samplepage+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.cb
	inc hl
	ld (hl),opcode.res_6_h
	inc hl

	cp 5
	call c,insert.xout
	sub 5

	ld (hl),opcode.ld_nn_hl
	inc hl
	ld bc,(mk.gd.offs)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (sample.offs+1),hl
	inc hl

	ret

;---------------------------------------------------------------
;make get second sample byte and add to first routine

;	ld e,(hl)
;	add a,c
;	adc hl,sp
;	ld b,(hl)
;	add a,c
;	adc hl,sp
;	ex af,af'
;	ld a,(de)
;	add a,n		<- from make first sample
;	ld (nn),a	-> buffer
;	ld e,b
;	ld a,(de)
;	add a,n		<- from make first sample
;	ld (nn),a	-> buffer
;	ld e,(hl)
;	ld a,(de)
;	add a,n		<- from make first sample
;	ld (nn),a	-> buffer
;	ex af,af'
;	add a,c
;	adc hl,sp

;	REPEAT 68 times

;	ld e,(hl)
;	add a,c
;	adc hl,sp
;	ld (nn),a	-> store sample pointer fraction
;	ld a,(de)
;	add a,n
;	ld (nn),a	-> buffer
;	in a,(hmpr)	!!!
;	bit 6,h
;	jr z,+1
;	inc a
;	ld (nn),a	-> 
;	res 6,h
;	ld (nn),a	->


mk.bp.get2nd:
	pop af
	ld iy,mk.store

	ld b,208 // 3        ;bytes per frame
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
	ld (hl),d			; ld (2*x+playtab2),a
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
	ld (hl),d			; ld (2*x+playtab2),a
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
	ld (hl),b		; ld (speedfract+1),a
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
	ld (hl),d			; ld (2*x+playtab2),a
	inc hl
	inc de
	inc de
	ld (mk.playtab),de

	cp 4
	call c,insert.xout
	sub 4

	push af
	ld a,(burstplayer.external.ram)
	or a
	jr z,@no.megabyte.6
	
	pop af 

	ld (hl),opcode.ld_a_nn
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b         
	inc hl
	
	jr @continue.6
	
@no.megabyte.6:

	pop af

	ld (hl),opcode.in_a_n
	inc hl
	ld (hl),port.hmpr
	inc hl

@continue.6: 

	cp 2+3
	call c,insert.xout
	sub 2+3

	ld (hl),opcode.cb
	inc hl
	ld (hl),opcode.bit_6_h
	inc hl
	ld (hl),opcode.jr_z_n
	inc hl
	ld (hl),1
	inc hl
	ld (hl),opcode.inc_a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode.ld_nn_a
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b			; ld (samplepage+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.cb
	inc hl
	ld (hl),opcode.res_6_h
	inc hl

	cp 5
	call c,insert.xout
	sub 5

	ld (hl),opcode.ld_nn_hl
	inc hl
	ld bc,(mk.gd.offs)
	ld (hl),c
	inc hl
	ld (hl),b			; ld (sample.offs+1),hl
	inc hl

	ret

;---------------------------------------------------------------
;make get sample byte for QSS - no mixing -> same routine 4*

mk.qss.get:
	pop af

	ld b,208 // 3        ;bytes per frame
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
	ld (hl),d         ;ld (4*x+playtabx),a
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
	ld (hl),b         ;ld (speedfract+1),a
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

	cp 4
	call c,insert.xout
	sub 4

	push af
	ld a,(burstplayer.external.ram)
	or a
	jr z,@no.megabyte.7
	
	pop af

	ld (hl),opcode.ld_a_nn
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl

	jr @continue.7
	
@no.megabyte.7:

	pop af

	ld (hl),opcode.in_a_n
	inc hl
	ld (hl),port.hmpr
	inc hl

@continue.7:

	cp 2+3
	call c,insert.xout
	sub 2+3

	ld (hl),opcode.cb
	inc hl
	ld (hl),opcode.bit_6_h
	inc hl
	ld (hl),opcode.jr_z_n
	inc hl
	ld (hl),1			; jr z,$+3
	inc hl
	ld (hl),opcode.inc_a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode.ld_nn_a
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b			; ld (samplepage+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode.cb
	inc hl
	ld (hl),opcode.res_6_h
	inc hl

	cp 5
	call c,insert.xout
	sub 5

	ld (hl),opcode.ld_nn_hl
	inc hl
	ld bc,(mk.gd.offs)
	ld (hl),c
	inc hl
	ld (hl),b			; ld (sample.offs+1),hl
	inc hl

	ret


;---------------------------------------------------------------
if defined(testing)

timeline:
	ld (hl),opcode.ld_e_a
	inc hl
	ld (hl),opcode.ld_a_n
	inc hl
	ld (hl),&70
	inc hl
	ld (hl),opcode.out_n_a
	inc hl
	ld (hl),port.clut
	inc hl
	ld (hl),opcode.ld_a_e
	inc hl
	ret
	
endif
;---------------------------------------------------------------
;memory needed for mk.bp routines

mk.playtab:	defw 0
mk.gd.spfr:	defw 0
mk.gd.page:	defw 0
mk.gd.offs:	defw 0

mk.store:	defs 208 * 2	; stores adresses

;===============================================================
device.list:

	;clut
	defw 0,0					; init, length             	     
	defw sd.clut,10				; sound driver, length
	defw port.clut	            ; output port
	defb &34                    ; control
	defb &17		            ; control
	defw timing.clut          	; timing table
	defw timing.clut.megabyte	; timing table
	defb 22 * 3 + 2           	; raster interrupts - related to count to 0 in timing.<device>
	defb 6                    	; number of bits
	
	;saa
	defw init.saa, init.saa.length
	defw sd.saa,10
	defw port.sound.address
	defb saa.register.amplitude_5
	defb saa.register.amplitude_2
	defw timing.saa
	defw timing.saa.megabyte
	defb 23 * 3 + 2
	defb 3
	
	;samdac
	defw 0,0
	defw sd.samdac,12
	defw port.printer_1
	defw &0001
	defw timing.samdac
	defw timing.samdac.megabyte
	defb 21 * 3 + 2
	defb 7

	;samdac 2
	defw 0,0
	defw sd.samdac,12
	defw port.printer_2
	defw &0001
	defw timing.samdac
	defw timing.samdac.megabyte
	defb 21 * 3 + 2
	defb 7

	;dac
	defw 0,0
	defw sd.dac,8
	defw port.printer_1
	defw 0
	defw timing.dac
	defw timing.dac.megabyte
	defb 17 * 3 + 2
	defb 6

	;dac 2
	defw 0,0
	defw sd.dac,8
	defw port.printer_2
	defw 0
	defw timing.dac
	defw timing.dac.megabyte
	defb 17 * 3 + 2
	defb 6

	;blue alpha
	defw init.blue_alpha, init.blue_alpha.length
	defw sd.dac,8
	defw 124 * 256 + 127
	defw 0
	defw timing.dac
	defw timing.dac.megabyte
	defb 17 * 3 + 2
	defb 6

	;quazar surround soundcard
	defw init.quazar, init.quazar.length
	defw sd.qss,9
	defw &06D0
	defw &0006					; +1 for OUTI
	defw timing.qss
	defw timing.qss.megabyte
	defb 16 * 3 + 2
	defb 8
	
;---------------------------------------------------------------
;initialise device subroutines

init.saa:
;	ld a,%10001000    ;A=silence value ??? 
	ld bc,port.sound.address
;	ld de,32 * 256 + 31
;	xor a
bp.res.saa:
;	out (c),e
;	out (port.sound.data),a
;	dec e
;	dec d
;	jr nz,bp.res.saa
	
if 1 > 0 

    ; !!! size too large for jr 
 
    ld e,saa.register.sound_enable
    out (c),e
    dec b
    ld e,saa.se.enabled
    out (c),e

    ld e,saa.envelope.enabled | saa.envelope.mode.maximum
    ld d,saa.register.envelope_generator_0
    inc b
    out (c),d
    dec b
    out (c),e
    
    ld d,saa.register.envelope_generator_1
    inc b
    out (c),d
    dec b
    out (c),e
    
else 

	ld hl,bp.saa.init
	ld b,6
	otir
	
endif	
    init.saa.length:    equ $ - init.saa

init.blue_alpha:
	ld bc,127 * 256 + 127
	ld a,255
	out (c),a
	ld b,125
	ld a,253
	out (c),a
    init.blue_alpha.length: equ $ - init.blue_alpha

init.quazar:
	ld bc,&06D0
	in a,(c)			; mode 1
	ld a,128
	dec b
	out (c),a			; rear right
	dec b
	out (c),a			; rear left
	dec b
	out (c),a			; front right
	dec b
	out (c),a			; front left
    init.quazar.length: equ $ - init.quazar

;---------------------------------------------------------------
;sound drivers

sd.clut:
	out (c),e		; 16   2
	inc b			;  4   1
	outi			; 24   2
	out (c),d		; 16   2
	inc b			;  4   1
	outi			; 24   2  = 22

sd.saa:
	out (c),e		; 16   2
	outi			; 24   2
	inc b			;  4   1
	out (c),d		; 16   2
	outi			; 24   2
	inc b			;  4   1  = 26

sd.samdac:
	outi			; 20   2
	inc c			;  4   1
	out (c),e		; 12   2
	dec c			;  4   1
	outi			; 20   2
	inc c			;  4   1
	out (c),d		; 20   2
	dec c			;  4   1

sd.dac:
	ld e,a			;  4   1
	ld a,(hl)		;  8   1
	inc hl			;  8   1
	add (hl)		;  8   1
	inc hl			;  8   1
	out (c),a		; 12   2
	ld a,e			;  4   1

sd.qss:
	ld b,e			;  4   1
	outi			; 20   2
	outi			; 20   2
	outi			; 20   2
	outi			; 20   2  = 21

;---------------------------------------------------------------
; timing tables for sound devices
; last byte in table is delay during line interrupt to get to 1.5
;
; 208 bytes per frame, one byte per 1.5 line -> 312 lines of which 192 screen
; and 120 in border area -> 80 bytes in border area
;
; border area is uncontend, so more instructions can be executed than during 
; screen area:

; if -1 reached -> error, double 0 indicates something went wrong in timing 


timing.clut:
	defb  33,121,121,121,121,121,121,121,121,121  ; 1
	defb 121,121,121,121,121,121,121,121,121,121  ; 2
	defb 121,121,121,121,121,122,121,121,121,121  ; 3
	defb 121,121,121,121,121,121,121,121,121,121  ; 4
	defb 121,121,121,121,121,121,121,121,121,121  ; 5
	defb 121,121,121,122,123,121,121,121,121,121  ; 6
	defb 121,121,121,121,121,121,121,121,121,121  ; 7
	defb 121,121,121,121,121,121,121,121,121,122  ; 8
	defb 110, 76, 79, 76, 78, 76, 79, 77, 82, 77  ; 9
	defb  80, 75, 79, 76, 78, 76, 79, 77, 82, 77  ;10
	defb  80, 75, 79, 76, 78, 76, 79, 77, 82, 77  ;11
	defb  80, 75, 79, 76, 78, 76, 79, 77, 82, 77  ;12
	defb  80, 75, 80, 90, 97,  0, -1, -1, -1, 83  ;13

timing.clut.megabyte:
	defb  34,121,121,121,121,121,121,121,121,121  ; 1
	defb 121,121,121,121,121,121,121,121,121,121  ; 2
	defb 121,121,121,121,121,124,121,121,121,121  ; 3
	defb 121,121,121,121,121,121,121,121,121,121  ; 4
	defb 121,121,121,121,121,121,121,121,121,121  ; 5
	defb 121,121,121,123,123,123,121,121,121,121  ; 6
	defb 121,121,121,121,121,121,121,121,121,121  ; 7
	defb 121,121,121,121,121,121,121,121,121,122  ; 8
	defb 111, 80, 83, 83, 83, 78, 80, 81, 86, 82  ; 9
	defb  84, 78, 80, 81, 86, 82, 83, 78, 80, 81  ;10
	defb  86, 82, 83, 78, 80, 81, 86, 82, 83, 78  ;11
	defb  80, 81, 86, 82, 82, 79, 80, 81, 86, 82  ;12
	defb  84, 76, 89, 93, 80,  0,  0,  0, -1, 83  ;13 I seem to have lost the last out 
	
timing.saa:
	defb  28,119,119,119,119,119,119,119,119,119  ; 1
	defb 119,119,119,119,119,119,119,119,119,119  ; 2
	defb 119,119,119,119,119,119,119,119,119,119  ; 3
	defb 119,119,119,119,119,119,119,119,119,119  ; 4
	defb 119,119,119,119,119,119,119,119,119,119  ; 5
	defb 119,119,119,119,121,119,119,119,119,119  ; 6
	defb 119,119,119,119,119,119,119,119,119,119  ; 7
	defb 119,119,119,119,119,119,119,119,119,119  ; 8
	defb 107, 76, 81, 75, 80, 74, 79, 73, 78, 75  ; 9
	defb  77, 75, 79, 76, 81, 75, 80, 74, 79, 73  ;10
	defb  78, 75, 77, 75, 79, 76, 81, 75, 80, 74  ;11
	defb  79, 73, 78, 75, 77, 75, 79, 76, 81, 75  ;12
	defb  80, 74, 79, 73, 80, 89, 95,  0, -1, 78  ;13

timing.saa.megabyte:
	defb  28,119,119,119,119,119,119,119,119,119  ; 1
	defb 119,119,119,119,119,119,119,119,119,119  ; 2
	defb 119,119,119,119,119,119,119,119,119,119  ; 3
	defb 119,119,119,119,119,119,119,119,119,119  ; 4
	defb 119,119,119,119,119,119,119,119,119,119  ; 5
	defb 119,119,119,119,121,119,119,119,119,119  ; 6
	defb 119,119,119,119,119,119,119,119,119,119  ; 7
	defb 119,119,119,119,119,119,119,119,119,119  ; 8
	defb 107, 76, 81, 75, 80, 74, 79, 73, 78, 75  ; 9
	defb  77, 75, 79, 76, 81, 75, 80, 74, 79, 73  ;10
	defb  78, 75, 77, 75, 79, 76, 81, 75, 80, 74  ;11
	defb  79, 73, 78, 75, 77, 75, 79, 76, 81, 75  ;12
	defb  80, 74, 79, 73, 80, 89, 95,  0, -1, 78  ;13
	
timing.samdac:
	defb  32,122,122,122,122,122,122,122,122,122  ; 1
	defb 122,122,122,122,122,122,122,122,122,122  ; 2
	defb 122,122,122,122,122,122,122,122,122,122  ; 3
	defb 122,122,122,122,122,122,122,122,122,122  ; 4
	defb 122,122,122,122,122,122,122,122,122,122  ; 5
	defb 122,122,122,123,124,122,122,122,122,122  ; 6
	defb 122,122,122,122,122,122,122,122,122,122  ; 7
	defb 122,122,122,122,122,122,122,122,122,123  ; 8
	defb 109, 80, 80, 81, 81, 78, 80, 78, 78, 81  ; 9
	defb  82, 79, 79, 78, 78, 80, 80, 81, 81, 78  ;10
	defb  80, 78, 78, 81, 82, 79, 79, 78, 78, 80  ;11
	defb  80, 81, 81, 78, 80, 78, 78, 81, 82, 79  ;12
	defb  81, 92, 97,  0, -1, -1, -1, -1, -1, 83  ;13
	
timing.samdac.megabyte:
	defb  32,122,122,122,122,122,122,122,122,122  ; 1
	defb 122,122,122,122,122,122,122,122,122,122  ; 2
	defb 122,122,122,122,122,122,122,122,122,122  ; 3
	defb 122,122,122,122,122,122,122,122,122,122  ; 4
	defb 122,122,122,122,122,122,122,122,122,122  ; 5
	defb 122,122,122,123,124,122,122,122,122,122  ; 6
	defb 122,122,122,122,122,122,122,122,122,122  ; 7
	defb 122,122,122,122,122,122,122,122,122,123  ; 8
	defb 109, 80, 80, 81, 81, 78, 80, 78, 78, 81  ; 9
	defb  82, 79, 79, 78, 78, 80, 80, 81, 81, 78  ;10
	defb  80, 78, 78, 81, 82, 79, 79, 78, 78, 80  ;11
	defb  80, 81, 81, 78, 80, 78, 78, 81, 82, 79  ;12
	defb  81, 92, 97,  0, -1, -1, -1, -1, -1, 83  ;13
	
timing.dac:
	defb  40,129,129,129,129,129,129,129,129,129  ; 1
	defb 129,129,129,129,129,129,129,129,129,129  ; 2
	defb 129,129,129,129,129,129,129,129,129,129  ; 3
	defb 129,129,129,129,129,129,129,129,129,129  ; 4
	defb 129,129,129,129,129,129,129,129,129,129  ; 5
	defb 130,129,130,129,129,129,129,129,129,129  ; 6
	defb 129,129,129,129,129,129,129,129,129,129  ; 7
	defb 129,129,129,129,129,129,129,129,129,129  ; 8
	defb 118, 84, 85, 84, 88, 84, 85, 84, 88, 84  ; 9
	defb  85, 84, 88, 84, 85, 84, 88, 84, 85, 84  ;10
	defb  88, 84, 85, 84, 88, 84, 85, 84, 88, 84  ;11
	defb  85, 83, 89,  0,  0, -1, -1, -1, -1, -1  ;12
	defb  -1, -1, -1, -1, -1, -1, -1, -1, -1, 93  ;13
	
timing.dac.megabyte:
	defb  40,129,129,129,129,129,129,129,129,129  ; 1
	defb 129,129,129,129,129,129,129,129,129,129  ; 2
	defb 129,129,129,129,129,129,129,129,129,129  ; 3
	defb 129,129,129,129,129,129,129,129,129,129  ; 4
	defb 129,129,129,129,129,129,129,129,129,129  ; 5
	defb 130,129,130,129,129,129,129,129,129,129  ; 6
	defb 129,129,129,129,129,129,129,129,129,129  ; 7
	defb 129,129,129,129,129,129,129,129,129,129  ; 8
	defb 118, 84, 85, 84, 88, 84, 85, 84, 88, 84  ; 9
	defb  85, 84, 88, 84, 85, 84, 88, 84, 85, 84  ;10
	defb  88, 84, 85, 84, 88, 84, 85, 84, 88, 84  ;11
	defb  85, 83, 89,  0,  0, -1, -1, -1, -1, -1  ;12
	defb  -1, -1, -1, -1, -1, -1, -1, -1, -1, 93  ;13
	
timing.qss:
	defb  32,121,121,121,121,121,121,121,121,121  ; 1
	defb 121,121,121,121,121,121,121,121,121,121  ; 2
	defb 121,121,121,121,121,121,121,121,121,121  ; 3
	defb 121,121,121,121,121,121,121,121,121,121  ; 4
	defb 121,121,121,121,121,121,121,121,121,121  ; 5
	defb 121,123,122,121,121,121,121,121,121,121  ; 6
	defb 121,121,121,121,121,121,121,121,121,121  ; 7
	defb 121,121,121,121,121,121,122,121,121,121  ; 8
	defb 109, 81, 78, 81, 80, 80, 81, 79, 80, 80  ; 9
	defb  78, 81, 80, 80, 81, 79, 80, 80, 78, 81  ;10
	defb  80, 80, 81, 79, 80, 80, 78, 81, 80, 80  ;11
	defb  81, 79, 93,  0,  0, -1, -1, -1, -1, -1  ;12
	defb  -1, -1, -1, -1, -1, -1, -1, -1, -1, 83  ;13

timing.qss.megabyte:
	defb  32,121,121,121,121,121,121,121,121,121  ; 1
	defb 121,121,121,121,121,121,121,121,121,121  ; 2
	defb 121,121,121,121,121,121,121,121,121,121  ; 3
	defb 121,121,121,121,121,121,121,121,121,121  ; 4
	defb 121,121,121,121,121,121,121,121,121,121  ; 5
	defb 121,123,122,121,121,121,121,121,121,121  ; 6
	defb 121,121,121,121,121,121,121,121,121,121  ; 7
	defb 121,121,121,121,121,121,122,121,121,121  ; 8
	defb 109, 81, 78, 81, 80, 80, 81, 79, 80, 80  ; 9
	defb  78, 81, 80, 80, 81, 79, 80, 80, 78, 81  ;10
	defb  80, 80, 81, 79, 80, 80, 78, 81, 80, 80  ;11
	defb  81, 79, 93,  0,  0, -1, -1, -1, -1, -1  ;12
	defb  -1, -1, -1, -1, -1, -1, -1, -1, -1, 83  ;13

;===============================================================
mk.movecode:
	org 0

	di
	in a,(port.vmpr)
;	and %00111111
	push af
	ld (bp.stsp+32769),sp
	in a,(port.lmpr)
	ld (bp.stlmpr+32769),a
	in a,(port.hmpr)
	and high.memory.page.mask
	ld (bp.exitpage+32769),a
	or low.memory.ram.0
	out (port.lmpr),a
	ld sp,32768
run.program:
	jp 0		;test

bp.exit:
	di
	ld a,255
	out (port.line_interrupt),a
bp.exitpage:
	ld a,0
	out (port.hmpr),a
	jp bp.stlmpr+32768

bp.stlmpr:
	ld a,0
	out (port.lmpr),a
bp.stsp:
	ld sp,0
	pop af
	out (port.vmpr),a
; 	ei
	ret

bp.id:

	defm "BUR"			;ID code (at 00053)

;---------------------------------------------------------------
	defs 102 - $		;NMI - corrupts player

	jp bp.exit

; pointers to variables in generated burstplayer code


bp.device:			defb 0

bp.pointers:

bp.point.addr.sequencer:	defw 0
bp.point.page.sequencer:	defw 0
bp.pointer.addr.demo:		defw 0
bp.pointer.page.demo:		defw 0

bp.pointer.addr.enable:		defw 0
bp.pointer.addr.exit:		defw bp.exit
	
bp.pointers.sample:	
				
bp.c1.page:			defw 0	
bp.c1.offs:			defw 0	
bp.c1.vol:			defw 0	
bp.c1.speedlo:		defw 0	
bp.c1.speedhi:		defw 0	
bp.c1.sp.frct:		defw 0	 

bp.pointers.length:	equ $ - bp.pointers.sample

bp.c2.page:			defw 0
bp.c2.offs:			defw 0
bp.c2.vol:			defw 0
bp.c2.speedlo:		defw 0
bp.c2.speedhi:		defw 0
bp.c2.sp.frct:		defw 0

bp.c3.page:			defw 0
bp.c3.offs:			defw 0
bp.c3.vol:			defw 0
bp.c3.speedlo:		defw 0
bp.c3.speedhi:		defw 0
bp.c3.sp.frct:		defw 0

bp.c4.page:			defw 0
bp.c4.offs:			defw 0
bp.c4.vol:			defw 0
bp.c4.speedlo:		defw 0
bp.c4.speedhi:		defw 0
bp.c4.sp.frct:		defw 0


if 0 > 1 
;set saa for samples
bp.saa.init:	defb saa.register.sound_enable        , saa.se.enabled
                defb saa.register.envelope_generator_1, saa.envelope.enabled | saa.envelope.mode.maximum
                defb saa.register.envelope_generator_0, saa.envelope.enabled | saa.envelope.mode.maximum
endif

mk.mv.end:
sound.driver.reset:

length:	equ  mk.movecode - 32768 + sound.driver.reset

if defined( testing )

;===============================================================

;rasterline = 384 T-states / 4 = 96 * 1.5 = 144
;bytes per frame = 10400 Hz / 50 = 208
;192 screen lines
;120 border lines
;312 total lines / 208 = 1.5

;===============================================================

	org 32768
	dump page.sequencer,0

	jp init.sq

demo:
	call 0

	ld bc,keyboard.caps_esc * 256 + port.status
demoloop:
	xor a
	out (port.clut),a
	in a,(c)
	bit 5,a	; escape
	jr nz,demoloop
still:
	in a,(c)
	bit 5,a
	jr z,still

exit:
	jp bp.exit

init.sq:
	di
	in a,(port.lmpr)
	ld (is.stlmpr+1),a
	ld a,page.burstplayer
	or low.memory.ram.0
	out (port.lmpr),a
	ld hl,(bp.sequence)
	ld (hl),sequencer \ 256
	inc hl
	ld (hl),sequencer // 256
	ld hl,demo+1
	ld de,(bp.enable)
	ld (hl),e
	inc hl
	ld (hl),d
	ld hl,(bp.demo)
	ld (hl),demo \ 256
	inc hl
	ld (hl),demo // 256

is.stlmpr:
	ld a,0
	out (port.lmpr),a
	ei
	ret


sequencer:
	ret

endif