;SAM MOD player - MAKE burstplayer 

;(C) 1995-2018 Stefan Drissen


include "ports.i"
include "opcodes.i"

;samdac routine uses most memory!
;quazar needs largest playback buffers

output:		equ 7		;0=clut, 1=saa, 2=samdac, 3=samdac 4=dac, 5=dac 2, 6=blue alpha, 7=qss

pal:		equ 0		;use which Amiga to calculate sample
ntsc:		equ 1		;speeds

count:		equ 255		;number of outs before border change
linetest:	equ 0

mk.page:	equ 5		;only used for assembly address

if defined(testing)

	sq.page:	equ 4		;not important except when testing

endif

	org 32768
	dump mk.page,0

	jp go.burst

device:		defb output
amiga:		defb pal
bp.page:	defb 2

	defm "                          "
	defm "MAKEBURST (C)2018 Stefan Drissen"
	defm "Thanks to Edwin Blink for the   "
	defm "original burst idea and code...."

;---------------------------------------------------------------
go.burst:
;---------------------------------------------------------------
	di
	in a,(low.memory.page.register)
	ld (mk.lmpr+1),a

	ld a,(bp.page)
	or 32
	out (low.memory.page.register),a
	ld (mk.sp+1),sp
	ld sp,49152
	call maker
mk.exit:
mk.lmpr:
	ld a,0
	out (low.memory.page.register),a
mk.sp:
	ld sp,0
	; ei
	ret

;---------------------------------------------------------------
maker:
;---------------------------------------------------------------
	ld a,count
	ld (poke.count+1),a
	xor a
	ld (counter+1),a

	ld hl,0
	ld de,1
	ld bc,32767
	ld (hl),l
	ldir

	ld hl,playtab1 + ( 2 * 208 )
	ld bc,2 * 208
	ld a,(device)
	cp 7					;if QSS -> playtab2 higher
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
	add a,a
	add a,a
	add a,a
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
	
	ld e,(hl)
	inc hl
	ld d,(hl)
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
	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_exx
	inc hl
	call insert.outs
	ld (hl),opcode_cp_n
	inc hl
	ld (hl),191 - 3		
	inc hl
	ld (hl),opcode_jr_z_n
	inc hl
	ld (mk.sto1+1),hl		;jr z,prep.bord.play
	ld (mk.sto1.1+1),hl
	ld (mk.recjradd+1),hl
	inc hl
	ld (hl),opcode_add_a_n
	inc hl
	ld (hl),3
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),line.interrupt.register
	inc hl
	ld (hl),opcode_jp_nn
	inc hl
	ld (mk.sto2+1),hl		;jp no.function
	inc hl
	inc hl

mk.sto1.1:
	ld de,0
	push hl
	scf
	sbc hl,de
	ld a,l
	pop hl
	ld (mk.stojr188+1),a
	ld (hl),opcode_ld_a_n
	inc hl
	ld (mk.recjr191+1),hl
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
mk.recjradd:
	ld de,0
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (jr+1),a
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
	ld (hl),191
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
	dec de
	dec de
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (cp191+1),a
	inc hl

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),line.interrupt.register
	inc hl
	ld (hl),opcode_jp_nn
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
	ld (hl),opcode_ret
	inc hl

;---------------------------------------------------------------
;select border player

mk.sto1:
	ld de,0
	push hl
	scf
	sbc hl,de
	ex de,hl
	ld (hl),e
mk.recjr191:
	ld hl,0
	ld (hl),e
	pop hl             ;prep.bord.play:
	ld (hl),opcode_ld_a_n
	inc hl
mk.stojr188:
	ld (hl),0		;ld a,jr188
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
	ld de,(mk.recjradd+1)
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (jradd+1),a
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
	ld (hl),188
	inc hl
	dec de
	dec de
	ld (hl),opcode_ld_nn_a
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (cp188+1),a
	inc hl

	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
	ld (mk.sto3+1),hl			;ld (prog.p+1),a
	inc hl
	inc hl
	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_exx
	inc hl
	ld (hl),opcode_push_af
	inc hl
	ld (hl),opcode_push_bc
	inc hl
	ld (hl),opcode_push_de
	inc hl
	ld (hl),opcode_push_hl
	inc hl
	ld (hl),opcode_ix
	inc hl
	ld (hl),opcode_push_ix
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),opcode_ld_nn_sp
	inc hl
	ld (mk.sto4+1),hl			;ld (prog.sp+1),sp
	inc hl
	inc hl
	ld (mk.rec2+1),hl ;playerselect:
	ld (hl),opcode_jp_nn
	inc hl
	ld (mk.sto5+1),hl ;jp borderplay1
	inc hl
	inc hl

;---------------------------------------------------------------
;store for current pattern row + other common variables
;there are 256 bytes reserved for this

cur.pat.data:	equ 256							;16
paltab:			equ 256 + 16					;16
frame.scr:		equ 256 + 32					;1  of not 0 -> new screen

buffer:			equ 256 + 128					;128
volume.tab:		equ buffer + 128				;32 * 256

pitch.table:	equ volume.tab + ( 32 * 256 )	;1024 * 2

;---------------------------------------------------------------

output.bits:
	ld b,0
	ld c,32           ;num vol tabs
	ld a,b
	cp 4
	jr nz,$+4         ;saa?
	ld c,16

	call make.vol.tab

	call make.pitch

;---------------------------------------------------------------
;get pattern data routine
get.pat:
	ld hl,pitch.table + ( 1024 * 2 )

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_de_nn
	inc hl
	ld (hl),cur.pat.data \ 256
	inc hl
	ld (hl),cur.pat.data // 256
	inc hl
	ld b,15
mk.gp.blp:
	ld (hl),opcode_ed
	inc hl
	ld (hl),&A0       ;ldi
	inc hl
	djnz mk.gp.blp
	ld (hl),&7E       ;ld a,(hl)
	inc hl
	ld (hl),&12       ;ld (de),a
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl             ;ld a,sq.page
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ret
	inc hl                                     ;42

;---------------------------------------------------------------
;call far routine, C=page, HL=address
call.far:
	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
	ex de,hl
	ld hl,12
	add hl,de
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (nn),a
	inc hl
	ld (hl),&79       ;ld a,c
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_nn_hl
	inc hl
	ex de,hl
	ld hl,3
	add hl,de
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (call+1),hl
	inc hl
	ld (hl),opcode_call_nn
	inc hl
	inc hl             ;call hl
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl             ;ld a,sq.page
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ret
	inc hl                                      ;19

;---------------------------------------------------------------
;far block move 1 - copies C bytes (max 128) to store
;B = source page, HL = source offset
ldir.far.1:
	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
	ld (buf.sto1+1),hl
	inc hl             ;ld (nn),a
	inc hl
	ld (hl),&78       ;ld a,b
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_de_nn
	inc hl
	ld (hl),buffer \ 256
	inc hl
	ld (hl),buffer // 256;ld de,buffer
	inc hl
	ld (hl),&06
	inc hl
	; ld (hl),0			;ld b,0
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&B0			;ldir
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
	ex de,hl
buf.sto1:
	ld hl,0
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl
	inc hl				;ld a,n
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ret
	inc hl                                     ;18

;---------------------------------------------------------------
;far block move 2 - copies C bytes (max 128) from store
;B = target page, DE = target offset
ldir.far.2:
	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ld_nn_a
	inc hl
	ld (buf.sto2+1),hl
	inc hl             ;ld (nn),a
	inc hl
	ld (hl),&78       ;ld a,b
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),&21
	inc hl
	ld (hl),buffer \ 256
	inc hl
	ld (hl),buffer // 256;ld hl,buffer
	inc hl
	ld (hl),&06
	inc hl
	; ld (hl),0			;ld b,0
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&B0			;ldir
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
	ex de,hl
buf.sto2:
	ld hl,0
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl
	inc hl				;ld a,n
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_ret
	inc hl                                     ;18

;---------------------------------------------------------------

;play tables (208*2*2)    if QSS -> 208*4*2

playtab1:	equ  2048+pitch.table+101   ;2*208
							    ; ^^^ previous routines!
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
	ld (hl),&18
	inc hl
	; ld (hl),0		;jr $+2
	inc hl
	sub 3
	jr   ins.nf.nop
ins.nf.notjr:
	or a
	jr z,ins.nf.all
ins.nf.lp:
	; ld (hl),opcode_nop
	inc hl
	dec a
	jr nz,ins.nf.lp
ins.nf.all:

	; ld a,linetest
    ; or a
    ; call nz,timeline

	call insert.outs
	ld (hl),opcode_exx
	inc hl
	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_ei
	inc hl
	ld (hl),opcode_ret
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
	ld (hl),&18
	inc hl
	; ld (hl),0		;jr $+2
	inc hl
	sub 3
	jr   ins.nf.nop2
ins.nf.notjr2:
	or a
	jr z,ins.nf.all2
ins.nf.lp2:
	; ld (hl),opcode_nop
	inc hl
	dec a
	jr nz,ins.nf.lp2
ins.nf.all2:

	; ld a,linetest
	; or a
	; call nz,timeline

	call insert.outs
	ld (hl),opcode_exx
	inc hl
	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_ei
	inc hl
	ld (hl),&76       ;halt
	inc hl
	ld (hl),opcode_ret
	inc hl


;---------------------------------------------------------------
;border player 1  - all get data routines included in here
border.play.1:

mk.timing:
	ld ix,0
	ld a,(ix)
	inc ix

	ex de,hl           ;borderplay1:
mk.sto5:
	ld hl,0
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl
	ld (mk.rec32+1),hl
	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto10+1),hl ;ld ix,bord.pl11
	inc hl
	inc hl
	ld (hl),opcode_jp_nn
	inc hl
	ld (mk.sto24+1),hl
	inc hl             ;jp get.c1.data
	inc hl

;=====----- get channel 1 data

;fetch channel 1 data

	ld (mk.getc1data+1),hl
	ex de,hl
mk.sto24:
	ld hl,0				;get.c1.data:
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c1.page),hl	;c1.page:
	; ld (hl),4			;ld a,4
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&21
	inc hl
	ld (bp.c1.offs),hl	;c1.off:
	; ld (hl),0
	inc hl
	; ld (hl),128		;ld hl,32768
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&16
	inc hl
	ld (bp.c1.vol),hl ;c1.tab:
	; ld (hl),32        ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&0E
	inc hl
	ld (bp.c1.speedlo),hl ;c1.speedlo:
	; ld (hl),128		;ld c,128
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&31
	inc hl
	ld (bp.c1.speedhi),hl ;c1.speedhi:
	; ld (hl),1
	inc hl
	; ld (hl),0         ;ld sp,1
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c1.sp.frct),hl ;c1.sp.frct:
	; ld (hl),0         ;ld a,0
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&DD
	inc hl
	ld (hl),&E9       ;jp (ix)
	inc hl

;=====----- end get channel 1 data

	ld (mk.ix1+2),ix
	ld (mk.a1+1),a
	ld e,a
	ld a,(counter+1)
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

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto11+1),hl ;ld ix,bord.pl14
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c4.page),hl ;c4.pag:
;              ld (hl),4          ;ld a,4
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&21
	inc hl
	ld (bp.c4.offs),hl ;c4.off:
	; ld (hl),0
	inc hl
	; ld (hl),128        ;ld hl,32768
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&16
	inc hl
	ld (bp.c4.vol),hl ;c4.tab:
	; ld (hl),32         ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&0E
	inc hl
	ld (bp.c4.speedlo),hl ;c4.speedlo:
	; ld (hl),128        ;ld c,128
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&31
	inc hl
	ld (bp.c4.speedhi),hl ;c4.speedhi:
	; ld (hl),1
	inc hl
	; ld (hl),0          ;ld sp,1
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c4.sp.frct),hl ;c4.sp.frct:
	; ld (hl),0          ;ld a,0
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&DD
	inc hl
	ld (hl),&E9        ;jp (ix)
	inc hl

;=====----- end get channel 4 data

	ld (mk.ix4+2),ix
	ld (mk.a4+1),a
	ld e,a
	ld a,(counter+1)
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

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto12+1),hl ;ld ix,bordpl1f
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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

	ld (hl),&21
	inc hl
	ld (hl),( paltab + 15 ) \ 256
	inc hl
	ld (hl),( paltab + 15 ) // 256 ;ld hl,paltab1+15
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&01
	inc hl
	ld (hl),color.look.up.table
	inc hl
	ld (hl),16        ;ld bc,16*256+color.look.up.table
	inc hl
	ld b,16
mk.outd.1:
	cp 6
	call c,insert.xout
	sub 6

	ld (hl),opcode_ed
	inc hl
	ld (hl),&AB       ;outd (16*)
	inc hl
	djnz mk.outd.1

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),video.memory.page.register
	inc hl

	cp 1
	call c,insert.xout
	sub 1

	ld (hl),&5F       ;ld e,a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),&3A
	inc hl
	ld (hl),frame.scr \ 256
	inc hl
	ld (hl),frame.scr // 256 ;ld a,(frame.scr)
	inc hl

	cp 1+3
	call c,insert.xout
	sub 1+3

	ld (hl),&A7       ;and a
	inc hl
	ld (hl),&20
	inc hl
	ld (hl),1         ;jr nz,$+3
	inc hl
	ld (hl),&7B       ;ld a,e
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),video.memory.page.register
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&DD
	inc hl
	ld (hl),&E9       ;jp (ix)
	inc hl

;=====----- end paltabselect

	ld (mk.ixp+2),ix
	ld (mk.ap+1),a
	ld e,a
	ld a,(counter+1)
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

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto13+1),hl ;ld ix,bord.pl12
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c2.page),hl ;c2.pag:
	; ld (hl),4          ;ld a,4
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&21
	inc hl
	ld (bp.c2.offs),hl ;c2.off:
	; ld (hl),0
	inc hl
	; ld (hl),128        ;ld hl,32768
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&16
	inc hl
	ld (bp.c2.vol),hl ;c2.tab:
	; ld (hl),32         ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&0E
	inc hl
	ld (bp.c2.speedlo),hl ;c2.speedlo:
	; ld (hl),128        ;ld c,128
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&31
	inc hl
	ld (bp.c2.speedhi),hl ;c2.speedhi:
	; ld (hl),1
	inc hl
	; ld (hl),0          ;ld sp,1
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c2.sp.frct),hl ;c2.sp.frct:
	; ld (hl),0          ;ld a,0
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&DD
	inc hl
	ld (hl),&E9        ;jp (ix)
	inc hl

;=====----- end get channel 2 data

	ld (mk.ix2+2),ix
	ld (mk.a2+1),a
	ld e,a
	ld a,(counter+1)
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

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto14+1),hl ;ld ix,bord.pl13
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c3.page),hl ;c3.pag:
	; ld (hl),4          ;ld a,4
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&21
	inc hl
	ld (bp.c3.offs),hl ;c3.off:
	; ld (hl),0
	inc hl
	; ld (hl),128        ;ld hl,32768
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&16
	inc hl
	ld (bp.c3.vol),hl ;c3.tab:
	; ld (hl),32         ;ld d,32
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&0E
	inc hl
	ld (bp.c3.speedlo),hl ;c3.speedlo:
	; ld (hl),128        ;ld c,128
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&31
	inc hl
	ld (bp.c3.speedhi),hl ;c3.speedhi:
	; ld (hl),1
	inc hl
	; ld (hl),0          ;ld sp,1
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.c3.sp.frct),hl ;c3.sp.frct:
	; ld (hl),0          ;ld a,0
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&DD
	inc hl
	ld (hl),&E9        ;jp (ix)
	inc hl

;=====----- end get channel 3 data

	ld (mk.ix3+2),ix
	ld (mk.a3+1),a
	ld e,a
	ld a,(counter+1)
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

	ld (hl),&21
	inc hl
	ld (mk.sto15+1),hl ;ld hl,borderplay2
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_ld_de_nn
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

	ld (hl),opcode_jp_nn
	inc hl
	ld (mk.sto16+1),hl ;jp player.rejoin
	inc hl
	inc hl

;---------------------------------------------------------------
border.play.2:

	ld a,(maker+1)
	ld (poke.count+1),a
	xor a
	ld (counter+1),a

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
	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto17+1),hl ;ld ix,bord.pl21
	inc hl
	inc hl
	ld (hl),opcode_jp_nn
	inc hl
mk.getc1data:
	ld de,0
	ld (hl),e
	inc hl
	ld (hl),d          ;jp get.c1.data
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
	ld (counter+1),a
mk.ix1:
	ld ix,0
mk.a1:
	ld a,0

	call mk.bp21         ;inc "bp21"

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto18+1),hl ;ld ix,bord.pl24
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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
	ld (counter+1),a
mk.ix4:
	ld ix,0
mk.a4:
	ld a,0

	call mk.bp24         ;inc "bp24"

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto19+1),hl ;ld ix,bordpl2f
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
	inc hl
mk.paltabselr:
	ld de,0
	ld (hl),e
	inc hl
	ld (hl),d          ;jp paltabselect
	inc hl

mk.cp:
	ld a,0
	ld (counter+1),a
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

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto20+1),hl ;ld ix,bord.pl22
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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
	ld (counter+1),a
mk.ix2:
	ld ix,0
mk.a2:
	ld a,0

	call mk.bp22         ;inc "bp22"

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),&DD
	inc hl
	ld (hl),&21
	inc hl
	ld (mk.sto21+1),hl ;ld ix,bord.pl23
	inc hl
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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
	ld (counter+1),a
mk.ix3:
	ld ix,0
mk.a3:
	ld a,0

	call mk.bp23         ;inc "bp23"

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&21
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

	ld (hl),opcode_ld_de_nn
	inc hl
	ld (hl),playtab1 \ 256
	inc hl
	ld (hl),playtab1 // 256 ;ld de,playtab1
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),opcode_jp_nn
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

	ld (hl),opcode_ld_nn_hl
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

	ld (hl),opcode_ed
	inc hl
	ld (hl),&53
	inc hl
	ld (mk.sto23+1),hl ;ld (playtabselect+1),de
	inc hl
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.seq.p),hl
	; ld (hl),sq.page	;ld a,sq.page
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 3
	call c,insert.xout
	sub 3

	ld (hl),&31
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

	ld (hl),opcode_ld_a_n
	inc hl
ras.start.1:
	ld (hl),0          ;ld a,ras.start
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),line.interrupt.register
	inc hl

	call insert.xout
	cp 255
	call nz,insert.xout

	ld (hl),opcode_exx
	inc hl

	ld (hl),opcode_ex_af_af
	inc hl

	ld (hl),&21
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
	ld (hl),opcode_exx
	inc hl

	ld (hl),opcode_ei
	inc hl
	ld (hl),opcode_call_nn
	inc hl
	ld (bp.sequence),hl
	; ld (hl),0
	inc hl
    ; ld (hl),0          ;call sequencer
	inc hl
	ld (hl),&AF
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),color.look.up.table
	inc hl
	ld (hl),opcode_ix
	inc hl
	ld (hl),&E1        ;pop ix
	inc hl
	ld (hl),&E1        ;pop hl
	inc hl
	ld (hl),&D1        ;pop de
	inc hl
	ld (hl),&C1        ;pop bc
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
	ex de,hl
mk.sto3:
	ld hl,0            ;prog.p:
	ld (hl),e
	inc hl
	ld (hl),d
	ex de,hl           ;ld a,0
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),&F1        ;pop af
	inc hl
	ld (hl),opcode_ret
	inc hl

;---------------------------------------------------------------
;enable burstplayer
enable:
	ld (bp.enable),hl  ;enableplayer:

	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),status.register
	inc hl
	ld (hl),&E6
	inc hl
	ld (hl),&08        ;and 8
	inc hl
	ld (hl),&20
	inc hl
	ld (hl),-6         ;jr nz,$-4
	inc hl
	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),status.register
	inc hl
	ld (hl),&E6
	inc hl
	ld (hl),&08        ;and 8
	inc hl
	ld (hl),opcode_jr_z_n
	inc hl
	ld (hl),-6         ;jr z,$-4
	inc hl
	ld (hl),&21
	inc hl
	ld de,(mk.rec32+1)
	ld (hl),e
	inc hl
	ld (hl),d          ;ld hl,borderplay1
	inc hl
	ld (hl),opcode_ld_nn_hl
	inc hl
	ld de,(mk.rec2+1)
	inc de
	ld (hl),e
	inc hl
	ld (hl),d          ;ld (playerselect+1),hl
	inc hl
	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_exx
	inc hl
	ld (hl),&01
	inc hl
sample.port:
	ld de,232
	ld (hl),e
	inc hl
	ld (hl),d          ;ld bc,sample.port
	inc hl
	ld (hl),&21
	inc hl
	ld (hl),playtab1 \ 256
	inc hl
	ld (hl),playtab1 // 256 ;ld hl,playtab1
	inc hl
	ld (hl),opcode_ld_de_nn
	inc hl
sample.ctrl:
	ld de,1
	ld (hl),e
	inc hl
	ld (hl),d          ;ld de,sample.ctrl
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
ras.start.2:
	ld (hl),0          ;ld a,ras.start
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),line.interrupt.register
	inc hl
	ld (hl),opcode_exx
	inc hl
	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_ei
	inc hl
	ld (hl),opcode_ret
	inc hl

;---------------------------------------------------------------
;set silence
set.silence:
	ld (mk.rec37+1),hl ;set.silence
	ld (hl),&21
	inc hl
	ld (hl),playtab1 \ 256
	inc hl
	ld (hl),playtab1 // 256 ;ld hl,playtab1
	inc hl
	ld (hl),&54       ;ld d,h
	inc hl
	ld (hl),&5D       ;ld e,l
	inc hl
	ld (hl),&13       ;inc de
	inc hl
	ld (hl),&36
	inc hl
	ld a,(device)
	dec a
	ld a,%10001000
	jr z,$+4
	ld a,%10000000		;ld (hl),n
	ld (hl),a			;%10000000 for all devices
	inc hl				;except soundchip %10001000
	ld (hl),&01
	inc hl
	ld de,8 * 208 - 1
	ld a,(device)
	cp 7
	jr z,$+5
	ld de,4 * 208 - 1
	ld (hl),e
	inc hl
	ld (hl),d         ;ld bc,4*208-1 (8* = QSS)
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&B0       ;ldir
	inc hl
	ld (hl),opcode_ret
	inc hl

;---------------------------------------------------------------
;test

	ld (run.program+1),hl
	ld (hl),opcode_call_nn
	inc hl
	ld (hl),sound.driver.reset \ 256
	inc hl
	ld (hl),sound.driver.reset // 256  ;call reset sound dev
	inc hl
	ld (hl),opcode_call_nn
	inc hl
mk.rec37:
	ld de,0
	ld (hl),e
	inc hl
	ld (hl),d          ;call set.silence
	inc hl

	ld (hl),opcode_ld_a_n
	inc hl
	ld (bp.demo.p),hl
	; ld (hl),sq.page		;ld a,sq.page
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),high.memory.page.register
	inc hl
	ld (hl),opcode_jp_nn
	inc hl
	ld (bp.demo),hl
	; ld (hl),0
	inc hl
	; ld (hl),0
	inc hl

;---------------------------------------------------------------
;swap channel 3 and 4 addresses if device is SAA

	ld a,(device)
	ld (bp.device),a
	dec a
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
;create volume tables for burstplayer (Amiga samples)
;B = number of bits
;C = number of tables

make.vol.tab:
	ld ix,volume.tab
	push ix

	xor a
	bit 4,c
	jr z,$+3
	inc a
	ld (cv.skip.table+2),a

	ld a,opcode_ret
	bit 4,c
	jr z,$+3
	xor a
	ld (cv.no.double),a

	ld a,c
	ld (cv.max.tables+1),a
	dec a
	ld (cv.div.by+1),a

	dec b
	ld a,1
cv.getbits:
	rla
	djnz cv.getbits
	ld (cv.vol.base.1+2),a
	ld (cv.vol.base.2+2),a

	rla
	ld (cv.vol.bits+1),a
	ld a,0
	adc a,0
	ld (cv.vol.bits+2),a

	xor a
	ld (cv.volume+1),a

cv.loop:
cv.vol.bits:
	ld de,0
	ld hl,0

cv.volume:
	ld a,0
	or a
	jr z,cv.no.mul
	ld b,a
cv.mul.vol:
	add hl,de
	djnz cv.mul.vol
cv.no.mul:
	ld b,h
	ld c,l
cv.div.by:
	ld de,15			;tables-1
	call cv.bc.div.de
	ld (cv.range+1),bc

cv.vol.base.1:
	ld hl,&0800			;H=2^(bits-1)
	ld b,128

;2^bits * v/15

cv.range:
	ld de,15          ;range

cv.blp:
	ld (ix),h
	inc ix

	add hl,de

	djnz cv.blp

	ld c,127
	add ix,bc

cv.vol.base.2:
	ld hl,&0800
	ld b,128
cv.blp2:
	or a
	sbc hl,de

	ld (ix),h
	dec ix
	djnz cv.blp2

cv.skip.table:
	ld bc,129			;B=1 if SAA
	add ix,bc

	ld a,(cv.volume+1)
	inc a
	ld (cv.volume+1),a
cv.max.tables:
	cp 0
	jr nz,cv.loop

	pop hl
cv.no.double:
	ret					;nop if SAA

	ld d,h
	ld e,l
	inc d
	ld bc,16
cv.saa.one:
	ld a,(hl)
	inc hl
	add a,a
	add a,a
	add a,a
	add a,a
	ld (de),a
	inc de
	djnz cv.saa.one
	inc d
	inc h
	dec c
	jr nz,cv.saa.one
	ret

cv.bc.div.de:
	ld a,b            ;divide bc by de
	ld b,16           ;result in bc
	ld hl,0           ;DE is unchanged
cv.clcd1:
	rl c
	rla
	adc hl,hl
	sbc hl,de
	jr nc,cv.clcd2
	add hl,de
cv.clcd2:
	ccf
	djnz cv.clcd1
	rl c
	rla
	ld b,a
	ret

;---------------------------------------------------------------
make.pitch:

;make pitch table for Amiga -> SAM sample rate
;---------------------------------------------------------------
	ld ix,pitch.table+4

	; ld b,4
	; pitch.clp:	; ld (ix),0         ;for nocalc on lowest two div
	; inc ix
	; djnz pitch.clp

	ld hl,43544       ;PAL  -> CHL=7093789.2
	ld a,(amiga)
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
;insert output commands for sound device

insert.outs:
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

insert.xout:
	cp 3
	jr c,ins.notjr
	ld (hl),&18       ;a = 3 or 4 or 5
	inc hl
	ld (hl),0         ;jr $+2
	inc hl
	sub 3
	jr   insert.xout
ins.notjr:
	or a
	jr z,ins.allnop
ins.noplp:
	ld (hl),opcode_nop
	inc hl
	dec a
	jr nz,ins.noplp
ins.allnop:
	ld a,(maker+1)
	and 1
	call nz,time

	ld (hl),opcode_exx
	inc hl
	call insert.outs
	ld (hl),opcode_exx
	inc hl

	ld a,(maker+1)
	and 1
	call z,time

	ld a,(ix)
	inc ix
	scf
	ret

time:
	push af
counter:
	ld a,0
	inc a
poke.count:
	cp 0
	jr nz,notborder

	ld (hl),opcode_ex_af_af
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
border.col:
	ld (hl),&70       ;ld a,3
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),color.look.up.table
	inc hl
	ld (hl),opcode_ex_af_af
	inc hl

notborder:
	ld (counter+1),a
	pop af
	ret

;---------------------------------------------------------------
;make channel 1 for burstplayer 1

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
	ld a,(device)
	cp 7
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
	ld a,(device)
	cp 7
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
	ld a,(device)
	cp 7
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
	ld a,(device)
	cp 7
	jp nz,mk.bp.get2nd

	ld de,playtab1 + 1 + ( 4 * 208 )  ;right
	ld (mk.playtab),de
	jp mk.qss.get

;---------------------------------------------------------------
;make channel 1 for burstplayer 2

mk.bp21:
	ld de,playtab1
	ld (mk.playtab),de

	ld de,(bp.c1.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c1.page)
	ld (mk.gd.page),de

	ld de,(bp.c1.offs)
	ld (mk.gd.offs),de

	push af
	ld a,(device)
	cp 7
	jp nz,mk.bp.get1st

	ld de,playtab1+2  ;left
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
	ld a,(device)
	cp 7
	jp nz,mk.bp.get2nd

	ld de,playtab1+0  ;left
	ld (mk.playtab),de
	jp mk.qss.get

;---------------------------------------------------------------
;make channel 2 for burstplayer 2

mk.bp22:
	ld de,playtab1+1
	ld (mk.playtab),de

	ld de,(bp.c2.sp.frct)
	ld (mk.gd.spfr),de

	ld de,(bp.c2.page)
	ld (mk.gd.page),de

	ld de,(bp.c2.offs)
	ld (mk.gd.offs),de

	push af
	ld a,(device)
	cp 7
	jp nz,mk.bp.get1st

	ld de,playtab1+3  ;right
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
	ld a,(device)
	cp 7
	jp nz,mk.bp.get2nd

	ld de,playtab1+1  ;right
	ld (mk.playtab),de
	jp mk.qss.get

;---------------------------------------------------------------
;make get first sample byte routine

mk.bp.get1st:
	pop af
	ld iy,mk.store

	ld b,208 // 3        ;bytes per frame
blp1.1:
	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&46       ;ld b,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 1
	call c,insert.xout
	sub 1

	ld (hl),opcode_ex_af_af
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	; ld (hl),0
	inc hl
	; ld (hl),0		;ld (nn),a
	inc hl

	cp 1
	call c,insert.xout
	sub 1

	ld (hl),&58       ;ld e,b
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	; ld (hl),0
	inc hl
	; ld (hl),0			;ld (nn),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E			;ld e,(hl)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A			;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
;              ld (hl),0
	inc hl
;              ld (hl),0         ;ld (nn),a
	inc hl

	cp 1
	call c,insert.xout
	sub 1

	ld (hl),opcode_ex_af_af
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	dec b
	jp nz,blp1.1

;now get last byte and store sample pointer

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld bc,(mk.gd.spfr)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (speedfract+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld (iy+0),l
	ld (iy+1),h
	inc iy
	inc iy
	; ld (hl),0
	inc hl
	; ld (hl),0		;ld (nn),a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2+3
	call c,insert.xout
	sub 2+3

	ld (hl),opcode_cb
	inc hl
	ld (hl),opcode_bit_6_h
	inc hl
	ld (hl),opcode_jr_z_n
	inc hl
	ld (hl),1         ;jr z,$+3
	inc hl
	ld (hl),opcode_inc_a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (samplepage+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_cb
	inc hl
	ld (hl),opcode_res_6_h
	inc hl

	cp 5
	call c,insert.xout
	sub 5

	ld (hl),opcode_ld_nn_hl
	inc hl
	ld bc,(mk.gd.offs)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (sample.offs+1),hl
	inc hl

	ret

;---------------------------------------------------------------
;make get second sample byte and add to first routine

mk.bp.get2nd:
	pop af
	ld iy,mk.store

	ld b,208 // 3        ;bytes per frame
blp1.4:
	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&46       ;ld b,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 1
	call c,insert.xout
	sub 1

	ld (hl),opcode_ex_af_af
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_add_a_n
	inc hl
	; ld (hl),0			;add a,n
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

	ld (hl),opcode_ld_nn_a
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

	ld (hl),&58       ;ld e,b
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_add_a_n
	inc hl
	; ld (hl),0			;add a,n
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

	ld (hl),opcode_ld_nn_a
	inc hl
	ld de,(mk.playtab)
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (2*x+playtab2),a
	inc hl
	inc de
	inc de
	ld (mk.playtab),de

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_add_a_n
	inc hl
	; ld (hl),0			;add a,n
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

	ld (hl),opcode_ld_nn_a
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

	ld (hl),opcode_ex_af_af
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	dec b
	jp nz,blp1.4

;now get last byte and store sample pointer

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld bc,(mk.gd.spfr)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (speedfract+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_add_a_n
	inc hl
	; ld (hl),0			;add a,n
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

	ld (hl),opcode_ld_nn_a
	inc hl
	ld de,(mk.playtab)
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (2*x+playtab2),a
	inc hl
	inc de
	inc de
	ld (mk.playtab),de

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2+3
	call c,insert.xout
	sub 2+3

	ld (hl),opcode_cb
	inc hl
	ld (hl),opcode_bit_6_h
	inc hl
	ld (hl),opcode_jr_z_n
	inc hl
	ld (hl),1         ;jr z,$+3
	inc hl
	ld (hl),opcode_inc_a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (samplepage+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_cb
	inc hl
	ld (hl),opcode_res_6_h
	inc hl

	cp 5
	call c,insert.xout
	sub 5

	ld (hl),opcode_ld_nn_hl
	inc hl
	ld bc,(mk.gd.offs)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (sample.offs+1),hl
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

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&46       ;ld b,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 1
	call c,insert.xout
	sub 1

	ld (hl),opcode_ex_af_af
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
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

	ld (hl),&58       ;ld e,b
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
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

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
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

	ld (hl),opcode_ex_af_af
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	dec b
	jp nz,q.blp1.4

;now get last byte and store sample pointer

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&5E       ;ld e,(hl)
	inc hl

	cp 1+4
	call c,insert.xout
	sub 1+4

	ld (hl),&81       ;add a,c
	inc hl
	ld (hl),opcode_ed
	inc hl
	ld (hl),&7A       ;adc hl,sp
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld bc,(mk.gd.spfr)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (speedfract+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),&1A       ;ld a,(de)
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld de,(mk.playtab)
	ld (hl),e
	inc hl
	ld (hl),d         ;ld (4*x+playtabx),a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_in_a_n
	inc hl
	ld (hl),high.memory.page.register
	inc hl

	cp 2+3
	call c,insert.xout
	sub 2+3

	ld (hl),opcode_cb
	inc hl
	ld (hl),opcode_bit_6_h
	inc hl
	ld (hl),opcode_jr_z_n
	inc hl
	ld (hl),1         ;jr z,$+3
	inc hl
	ld (hl),opcode_inc_a
	inc hl

	cp 4
	call c,insert.xout
	sub 4

	ld (hl),opcode_ld_nn_a
	inc hl
	ld bc,(mk.gd.page)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (samplepage+1),a
	inc hl

	cp 2
	call c,insert.xout
	sub 2

	ld (hl),opcode_cb
	inc hl
	ld (hl),opcode_res_6_h
	inc hl

	cp 5
	call c,insert.xout
	sub 5

	ld (hl),opcode_ld_nn_hl
	inc hl
	ld bc,(mk.gd.offs)
	ld (hl),c
	inc hl
	ld (hl),b         ;ld (sample.offs+1),hl
	inc hl

	ret


;---------------------------------------------------------------
if defined(testing)

timeline:
	ld (hl),&5F			;ld e,a
	inc hl
	ld (hl),opcode_ld_a_n
	inc hl
	ld (hl),&70
	inc hl
	ld (hl),opcode_out_n_a
	inc hl
	ld (hl),color.look.up.table
	inc hl
	ld (hl),&7B			;ld a,e
	inc hl
	ret
	
endif
;---------------------------------------------------------------
;memory needed for mk.bp routines

mk.playtab:	defw 0
mk.gd.spfr:	defw 0
mk.gd.page:	defw 0
mk.gd.offs:	defw 0

mk.store:	DEFS 208 * 2        ;stores adresses

;---------------------------------------------------------------
;timing tables for sound devices
;last byte in table is delay during line interrupt

timing.clut:
	defb 033,121,121,121,121,121,121,121,121,121  ; 1
	defb 121,121,121,121,121,121,121,121,121,121  ; 2
	defb 121,121,121,121,121,122,121,121,121,121  ; 3
	defb 121,121,121,121,121,121,121,121,121,121  ; 4
	defb 121,121,121,121,121,121,121,121,121,121  ; 5
	defb 121,121,121,122,123,121,121,121,121,121  ; 6
	defb 121,121,121,121,121,121,121,121,121,121  ; 7
	defb 121,121,121,121,121,121,121,121,121,122  ; 8
	defb 110,076,079,076,078,076,079,077,082,077  ; 9
	defb 080,075,079,076,078,076,079,077,082,077  ;10
	defb 080,075,079,076,078,076,079,077,082,077  ;11
	defb 080,075,079,076,078,076,079,077,082,077  ;12
	defb 080,075,080,090,097,255,255,255,255,083  ;13
timing.saa:
	defb 028,119,119,119,119,119,119,119,119,119  ; 1
	defb 119,119,119,119,119,119,119,119,119,119  ; 2
	defb 119,119,119,119,119,119,119,119,119,119  ; 3
	defb 119,119,119,119,119,119,119,119,119,119  ; 4
	defb 119,119,119,119,119,119,119,119,119,119  ; 5
	defb 119,119,119,119,121,119,119,119,119,119  ; 6
	defb 119,119,119,119,119,119,119,119,119,119  ; 7
	defb 119,119,119,119,119,119,119,119,119,119  ; 8
	defb 107,076,081,075,080,074,079,073,078,075  ; 9
	defb 077,075,079,076,081,075,080,074,079,073  ;10
	defb 078,075,077,075,079,076,081,075,080,074  ;11
	defb 079,073,078,075,077,075,079,076,081,075  ;12
	defb 080,074,079,073,080,089,095,255,255,078  ;13
timing.samdac:
	defb 032,122,122,122,122,122,122,122,122,122  ; 1
	defb 122,122,122,122,122,122,122,122,122,122  ; 2
	defb 122,122,122,122,122,122,122,122,122,122  ; 3
	defb 122,122,122,122,122,122,122,122,122,122  ; 4
	defb 122,122,122,122,122,122,122,122,122,122  ; 5
	defb 122,122,122,123,124,122,122,122,122,122  ; 6
	defb 122,122,122,122,122,122,122,122,122,122  ; 7
	defb 122,122,122,122,122,122,122,122,122,123  ; 8
	defb 109,080,080,081,081,078,080,078,078,081  ; 9
	defb 082,079,079,078,078,080,080,081,081,078  ;10
	defb 080,078,078,081,082,079,079,078,078,080  ;11
	defb 080,081,081,078,080,078,078,081,082,079  ;12
	defb 081,092,097,255,255,255,255,255,255,083  ;13
timing.dac:
	defb 040,129,129,129,129,129,129,129,129,129  ; 1
	defb 129,129,129,129,129,129,129,129,129,129  ; 2
	defb 129,129,129,129,129,129,129,129,129,129  ; 3
	defb 129,129,129,129,129,129,129,129,129,129  ; 4
	defb 129,129,129,129,129,129,129,129,129,129  ; 5
	defb 130,129,130,129,129,129,129,129,129,129  ; 6
	defb 129,129,129,129,129,129,129,129,129,129  ; 7
	defb 129,129,129,129,129,129,129,129,129,129  ; 8
	defb 118,084,085,084,088,084,085,084,088,084  ; 9
	defb 085,084,088,084,085,084,088,084,085,084  ;10
	defb 088,084,085,084,088,084,085,084,088,084  ;11
	defb 085,083,089,255,255,255,255,255,255,255  ;12
	defb 255,255,255,255,255,255,255,255,255,093  ;13
timing.qss:
	defb 032,121,121,121,121,121,121,121,121,121  ; 1
	defb 121,121,121,121,121,121,121,121,121,121  ; 2
	defb 121,121,121,121,121,121,121,121,121,121  ; 3
	defb 121,121,121,121,121,121,121,121,121,121  ; 4
	defb 121,121,121,121,121,121,121,121,121,121  ; 5
	defb 121,123,122,121,121,121,121,121,121,121  ; 6
	defb 121,121,121,121,121,121,121,121,121,121  ; 7
	defb 121,121,121,121,121,121,122,121,121,121  ; 8
	defb 109,081,078,081,080,080,081,079,080,080  ; 9
	defb 078,081,080,080,081,079,080,080,078,081  ;10
	defb 080,080,081,079,080,080,078,081,080,080  ;11
	defb 081,079,093,255,255,255,255,255,255,255  ;12
	defb 255,255,255,255,255,255,255,255,255,083  ;13

;===============================================================
;device list

device.list:
	defw 0,0                  ;clut
	defw sd.clut,10           ;sound device, length
	defw 248                  ;output port
	defw &1734                ;control
	defw timing.clut          ;timing table
	defb 22 * 3 + 2           ;rasi
	defb 6                    ;number of bits

	defw i.saa,e.saa-i.saa    ;saa
	defw sd.saa,10
	defw 511
	defw &0205
	defw timing.saa
	defb 23 * 3 + 2
	defb 4

	defw 0,0                  ;samdac
	defw sd.samdac,12
	defw 232
	defw &0001
	defw timing.samdac
	defb 21 * 3 + 2
	defb 7

	defw 0,0                  ;samdac 2
	defw sd.samdac,12
	defw 234
	defw &0001
	defw timing.samdac
	defb 21 * 3 + 2
	defb 7

	defw 0,0                  ;dac
	defw sd.dac,8
	defw 232
	defw 0
	defw timing.dac
	defb 16 * 3 + 2
	defb 6

	defw 0,0                  ;dac 2
	defw sd.dac,8
	defw 234
	defw 0
	defw timing.dac
	defb 16 * 3 + 2
	defb 6

	defw i.bla,e.bla-i.bla    ;blue alpha
	defw sd.dac,8
	defw 124 * 256 + 127
	defw 0
	defw timing.dac
	defb 16 * 3 + 2
	defb 6

	defw i.qss,e.qss-i.qss    ;quazar surround soundc
	defw sd.qss,9
	defw &06D0
	defw &0006                ;+1 for OUTI
	defw timing.qss
	defb 16 * 3 + 2
	defb 8
;---------------------------------------------------------------
;initialise device subroutines

i.saa:
	ld a,%10001000    ;A=silence value
	ld bc,511
	ld de,32 * 256 + 31
	xor a
bp.res.saa:
	out (c),e
	out (255),a
	dec e
	dec d
	jr nz,bp.res.saa
	ld hl,bp.soundtab
	ld b,6
	otir
e.saa:
i.bla:
	ld bc,127 * 256 + 127
	ld a,255
	out (c),a
	ld b,125
	ld a,253
	out (c),a
e.bla:
i.qss:
	ld bc,&06D0
	in a,(c)          ;mode 1
	ld a,128
	dec b
	out (c),a          ;rear right
	dec b
	out (c),a          ;rear left
	dec b
	out (c),a          ;front right
	dec b
	out (c),a          ;front left
e.qss:
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

;===============================================================
mk.movecode:
	org 0

	di
	in a,(video.memory.page.register)
	push af
	ld (bp.stsp+32769),sp
	in a,(low.memory.page.register)
	ld (bp.stlmpr+32769),a
	in a,(high.memory.page.register)
	and 31
	ld (bp.exitpage+32769),a
	or 32
	out (low.memory.page.register),a
	ld sp,32768
run.program:
	jp 0		;test

bp.exit:
	di
	ld a,255
	out (line.interrupt.register),a
bp.exitpage:
	ld a,0
	out (high.memory.page.register),a
	jp bp.stlmpr+32768

bp.stlmpr:
	ld a,0
	out (low.memory.page.register),a
bp.stsp:
	ld sp,0
	pop af
	out (video.memory.page.register),a
	; ei
	ret

	defm "BUR"			;ID code (at 00053)

;---------------------------------------------------------------
	defs 102 - $		;NMI - corrupts player

	jp bp.exit


bp.device:		defb 0
bp.sequence:	defw 0
bp.seq.p:		defw 0
bp.demo:		defw 0
bp.demo.p:		defw 0
bp.enable:		defw 0

	defw bp.exit

bp.c1.page:		defw 0
bp.c1.offs:		defw 0
bp.c1.vol:		defw 0
bp.c1.speedlo:	defw 0
bp.c1.speedhi:	defw 0
bp.c1.sp.frct:	defw 0

bp.c2.page:		defw 0
bp.c2.offs:		defw 0
bp.c2.vol:		defw 0
bp.c2.speedlo:	defw 0
bp.c2.speedhi:	defw 0
bp.c2.sp.frct:	defw 0

bp.c3.page:		defw 0
bp.c3.offs:		defw 0
bp.c3.vol:		defw 0
bp.c3.speedlo:	defw 0
bp.c3.speedhi:	defw 0
bp.c3.sp.frct:	defw 0

bp.c4.page:		defw 0
bp.c4.offs:		defw 0
bp.c4.vol:		defw 0
bp.c4.speedlo:	defw 0
bp.c4.speedhi:	defw 0
bp.c4.sp.frct:	defw 0


bp.soundtab:
	defb 28,1,25,130,24,130   ;set saa for samples



mk.mv.end:
sound.driver.reset:

length:	equ  mk.movecode-32768+sound.driver.reset

if defined( testing )

;===============================================================

;rasterline = 384 T-states / 4 = 96 * 1.5 = 144
;bytes per frame = 10400 Hz / 50 = 208
;192 screen lines
;120 border lines
;312 total lines / 208 = 1.5

;===============================================================

	org 32768
	dump sq.page,0

	jp init.sq

demo:
	call 0

	ld bc,247 * 256 + status.register
demoloop:
	xor a
	out (color.look.up.table),a
	in a,(c)
	bit 5,A
	jr nz,demoloop
still:
	in a,(c)
	bit 5,a
	jr z,still

exit:
	jp bp.exit

init.sq:
	di
	in a,(low.memory.page.register)
	ld (is.stlmpr+1),a
	ld a,2+32	;burst page
	out (low.memory.page.register),a
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
	out (low.memory.page.register),a
	ei
	ret


sequencer:
	ret

endif	