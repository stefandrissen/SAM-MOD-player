;SAM MOD player - DOS loader 

;(C) 1996-2018 Stefan Drissen

include "ports.i"

load.page:	equ 5
load.offs:	equ 32768

	org 32768
	dump 1,0

	jp go.loader

	org $-16384

loader.device:		defb 2
loader.speed:		defb 0	;0=pal, 1=ntsc
drive.1.steprate:	defb 2	;0=6ms, 1=12ms, 2=2ms, 3=3ms
drive.2.steprate:	defb 1

	org $+16384

;---------------------------------------------------------------
go.loader:
;---------------------------------------------------------------

	di
	ld a,32
	out (low.memory.page.register),a
	jp loader.low

	org $-16384

;---------------------------------------------------------------
loader.low:
;---------------------------------------------------------------
	ld sp,32768

	call set.palette.black
	
	ld a,video.mode.2
	out (video.memory.page.register),a
	
	call cls

	ld a,(drive.1.steprate)
	ld (steprate.1+1),a
	ld a,(drive.2.steprate)
	ld (steprate.2+1),a

	ld de,device.scrn
	ld ix,col.device
	call print.screen

	ld hl,5 * 192
	ld (curs.offs+1),hl

	ld a,(loader.device)
	ld c,a
	add a,a
	add a,c
	add a,a
	ld c,a
	ld (old.cursor+1),a
	xor a
	ld (curs.blink+1),a
	call print.cursor

	ld hl,20 * 192
	ld (curs.offs+1),hl

	ld a,(loader.speed)
	ld c,a
	add a,a
	add a,c
	add a,a
	ld c,a
	ld (old.cursor+1),a
	xor a
	ld (curs.blink+1),a
	call print.cursor

;---------------------------------------------------------------	
select.device:

	ld hl,5 * 192
	ld (curs.offs+1),hl
	ld a,7
	ld (max.select+1),a

	ld a,(loader.device)
	ld c,a
	add a,a
	add a,c
	add a,a
	ld (old.cursor+1),a
@loop:
	call cursor.select

	ld a,c
	ld (loader.device),a

	call scan.return
	jr z,selected

	call scan.space
	jr nc,@loop

	xor a
	ld (curs.blink+1),a
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	call print.cursor

	jr select.speed

;---------------------------------------------------------------
select.speed:

	ld hl,20 * 192
	ld (curs.offs+1),hl
	ld a,1
	ld (max.select+1),a

	ld a,(loader.speed)
	ld c,a
	add a,a
	add a,c
	add a,a
	ld (old.cursor+1),a
@loop:
	call cursor.select

	ld a,c
	ld (loader.speed),a

	call scan.return
	jr z,selected

	call scan.space
	jr nc,@loop

	xor a
	ld (curs.blink+1),a
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	call print.cursor

	jr select.device

selected:

	call cls

	ld a,load.page	;load.page=burst.page
	out (high.memory.page.register),a
	ld hl,0
	ld a,(loader.device)
	ld (32771),a
	ld a,(loader.speed)
	ld (32772),a
	
	call 32768		;make burst

	jp loader

;---------------------------------------------------------------
cursor.select:

;selection routine
;C = current position
;min selection = 0
;max selection = (max.selec+1)

	push bc
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	call print.cursor
	pop bc

	ld a,255
	in a,(keyboard.register)
	ld b,a
	bit 1,b
	call z,curs.up
	bit 2,b
	call z,curs.dn

	ld a,%11101111
	in a,(keyboard.register)
	ld b,a
	bit 1,b
	call z,curs.up
	bit 2,b
	call z,curs.dn
	ret
curs.up:
	ld a,c
	or a
	ret z
	push bc
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	ld b,6
sd.curs.up:
	dec c
	call print.cursor
	djnz sd.curs.up
	pop bc
	dec c
	ret

curs.dn:
max.select:
	ld a,6
	or a
	ret z
	dec a
	cp c
	ret c
	push bc
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	ld b,6
sd.curs.dn:
	inc c
	call print.cursor
	djnz sd.curs.dn
	pop bc
	inc c
	ret

scan.space:
	ld a,127
	in a,(keyboard.register)
	or a
	bit 0,a
	ret nz
sd.stillspc:
	ld a,127
	in a,(keyboard.register)
	bit 0,a
	jr z,sd.stillspc
	scf
	ret
scan.return:
	ld a,%10111111	;RETURN
	in a,(keyboard.register)
	and 1
	ret z
	ld a,%11101111	;0
	in a,(keyboard.register)
	and 1
	ret

;===============================================================

path:			defw patha

patha:
	defb "\",0
	defs 63
	defb 0

pathb:
	defb "\",0
	defs 63
	defb 0

temppath:
	defb "\"
	defs 64

;===============================================================
;loader

loader.palette:	
	defb %0000000 ;    0

	defb %0011101 ;3 1 1;BLUE+green
	defb %1011001 ;3 2 2
	defb %1011101 ;3 3 3

	defb %0101110 ;3 1 4;RED+green
	defb %1101010 ;3 2 5
	defb %1101110 ;3 3 6

	defb %1001101 ;3 1 7;GREEN+blue

	defb %0000000 ;    8;bright background

	defb %1011100 ;3 2 9
	; defb %1011101 ;3 3 same as pen 3

	defb %0101011 ;3 1 A;RED+blue
	defb %0111010 ;3 2 B
	defb %0111011 ;3 3 C

	defb %1001110 ;3 1 D;GREEN+red
	defb %1101100 ;3 2 E
	; defb %1101110 ;3 3  ;same as pen 6

	defb %1110111 ;    F

;---------------------------------------------------------------
set.palette.black:
;---------------------------------------------------------------

	ld bc,15 * 256 + color.look.up.table
	xor a
@loop:
	out (c),a
	djnz @loop
	out (c),a
	ret

;---------------------------------------------------------------
show.screen:
;---------------------------------------------------------------

	call cls

	ld de,load.screen
	ld ix,col.load
	call print.screen
	ret

;---------------------------------------------------------------
loader:
;---------------------------------------------------------------

	ld sp,32768

	call show.screen

	ld hl,mes.nodisc
	ld de,m.vollabel
	ld bc,11
	ldir

	ld a,1
	ld (msdos+1),a

	xor a
	ld (nodisc+1),a

	call dir

	ld a,1
	ld (load.entries),a

	ld hl,option.dir
	ld de,loader.dir
	ld bc,load.len * 2
	ldir

	ld de,loader.dir+load.len
	ld c,225+16
	ld b,0
is.2.pres:
	out (c),b

	ld a,12				;approx 32 micro second delay
@loop:
	dec a
	jr nz,@loop

	in a,(c)
	cp b
	jr nz,no.drive2
	djnz is.2.pres
	ld a,2
	ld (load.entries),a
	ld de,loader.dir + ( 2 * load.len ) 
no.drive2:

nodisc:
	ld a,0	;set by errnodisc
	or a
	jp nz,converted

msdos:
	ld a,0
	or a
	jp z,cnv.sam

;convert pc dir -> loader dir

	ld hl,(data)
pc.to.loader:
	ld a,(hl)
	or a
	jp z,converted
	cp 229
	jp z,pl.skip	;deleted file
	push hl
	pop ix
	ld a,(ix+11)
	and 8
	jp nz,pl.skip	;volume label

	ld a,(ix+8)
	cp "M"
	jp nz,pl.skip
	ld a,(ix+9)
	cp "O"
	jp nz,pl.skip
	ld a,(ix+10)
	cp "D"
	jp nz,pl.skip		;not MOD extension

	push hl
	push de
	ld b,8
pl.copy.name:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz pl.copy.name

	push de

	ld e,(ix+26)
	ld d,(ix+27)		;first cluster

	ld hl,temp.spc-9	;space between scrn & attrib
pc.rd.more:
	call readcluster
	call getfatentry
	ld a,h
	cp ( temp.spc + 1084 - 9 ) // 256 + 1
	jr c,pc.rd.more

	ld hl,temp.spc+1083-9
	ld de,temp.spc+1083
	ld bc,1084
	lddr

	pop de

	ld a,2
	call file.check

	push de

	ld e,(ix+28)
	ld d,(ix+29)
	ld a,(ix+30)
	ex de,hl
pc.resub:
	or a
	sbc hl,de
	sbc c
	jr nc,pc.got.maxmin
	adc c
	add hl,de
	ex de,hl
	ld b,a
	ld a,c
	ld c,b
	jr pc.resub
pc.got.maxmin:			;ahl = difference calc len & file len
	pop de
	or h
	jr z,pc.file.ok

	pop de
	pop hl
	jr pl.skip

pc.file.ok:

;get date
	ld a,(ix+24)
	ld b,a
	and %00011111		;day
	call cnv.a.to.de
	ld a,b
	and %11100000
	rlca
	rlca
	rlca
	ld c,a
	ld a,(ix+25)
	ld b,a
	and %00000001
	rlca
	rlca
	rlca
	or c				;month
	call cnv.a.to.de
	ld a,b
	and %11111110
	rrca
	add 80
	sub 100
	jr nc,$-2
	add 100				;year
	call cnv.a.to.de

	call insert.size
	ld hl,load.entries
	inc (hl)
	pop hl
	ld bc,load.len
	add hl,bc
	ex de,hl
	pop hl
pl.skip:
	ld bc,32
	add hl,bc
	ld a,(load.entries)
	cp 27
	jp z,converted
	jp pc.to.loader

cnv.sam:
	push de
	ld ix,col.black
	ld a,6
	call colour.scrn

;first read in SAM directory
	ld de,1
	ld hl,fat
cs.rd.lp:
	push de
	push hl
	call rdphysec
	pop hl

	ld a,d
	or a
	jr nz,cs.notfirst
	dec e
	jr nz,cs.notfirst

	push hl
	ld de,m.vollabel
	ld hl,fat+210
	ld a,(hl)
	cp "*"
	jr nz,$+4
	ld (hl),0
	ld bc,10
	ldir
	ld a," "
	ld (de),a
	pop hl
cs.notfirst:
	push hl
	push hl
	pop de
	ld bc,245
	add hl,bc
	ld a,e
	add 11
	ld e,a
	jr nc,$+3
	inc d
	ldi
	ldi
	inc de
	inc de
	ldi
	pop hl

	ld bc,16
	ld e,l
	ld d,H
	ex de,hl
	add hl,bc
	ex de,hl
	inc h

	push de
	push hl
	push hl
	pop de
	ld bc,245
	add hl,bc
	ld a,e
	add 11
	ld e,a
	jr nc,$+3
	inc d
	ldi
	ldi
	inc de
	inc de
	ldi
	pop hl
	pop de

	ld bc,16
	ldir
	ex de,hl
	pop de
	inc e
	ld a,e
	cp 11
	jr nz,cs.rd.lp
	ld e,1
	inc d
	ld a,d
	cp 4
	jr nz,cs.rd.lp

;now convert the SAM stuff to loader format

	pop de
	ld hl,fat

	ld a,80				;80 directory entries
sam.to.loader:
	push af
	ld a,(hl)
	and %00111111
	cp 19
	jp nz,sl.skip

	push hl

	inc hl
	inc hl
	ld b,8
ext.find.m:
	ld a,(hl)
	inc hl
	cp "."
	jr nz,ext.not.fnd
	ld a,(hl)
	res 5,a				;->uppercase
	cp "M"
	jr nz,ext.not.fnd
	jr sam.found.m
ext.not.fnd:
	djnz ext.find.m
	pop hl
	jp sl.skip

sam.found.m:
	pop hl
	push hl
	pop ix

	push hl
	push de
	inc hl
	ld b,8
sl.copy.name:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz sl.copy.name

	push de

	ld e,(ix+14)
	ld d,(ix+13)	;first sector

	ld hl,temp.spc-9
	call rdphysec
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	call rdphysec
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	call rdphysec

	pop de

	ld a,2
	call file.check

	call fc.sam
	jr z,sm.file.ok

;not MOD, maybe compressed mod (4 bit)

	pop de
	push de

	ld hl,8
	add hl,de
	ex de,hl

	ld a,1				;only add sample length once
	call file.check

	call fc.sam
	jr z,sm.file.ok

	pop de
	pop hl
	jr sl.skip

sm.file.ok:

;get date
	push de
	ld a,"*"
	ld (de),a

sm.check.date:
	ld a,(ix+11)
	or a
	jr z,sm.done.date		;0->invalid date
	cp 32
	jr nc,sm.done.date		;day>31 = invalid date
	ld a,(ix+12)
	or a
	jr z,sm.done.date		;0->invalid date
	cp 13
	jr nc,sm.done.date		;month>12 = invalid date
	ld a,(ix+15)
	or a
	jr z,sm.done.date ;0->invalid date
	inc a
	jr z,sm.done.date ;255->invalid date

	ld a,(ix+11)
	call cnv.a.to.de
	ld a,(ix+12)
	call cnv.a.to.de
	ld a,(ix+15)
	call cnv.a.to.de
sm.done.date:
	pop de
	ld a,e
	add 6
	ld e,a
	jr nc,$+3
	inc d

	call insert.size

	ld hl,load.entries
	inc (hl)
	pop hl
	ld bc,load.len
	add hl,bc
	ex de,hl
	pop hl
sl.skip:
	ld bc,16
	add hl,bc

	pop af
	ld b,a
	ld a,(load.entries)
	cp 27
	jr z,converted
	ld a,b
	dec a
	jp nz,sam.to.loader

converted:
	call show.screen

	call print.oct

	ld a,(driveselect+1)
	or a
	ld a,"1"
	jr z,$+3
	inc a
	ld (mes.drive+6),a

	ld de,mes.label
	ld hl,m.vollabel
	ld a,(hl)
	or a
	jr nz,$+5
	ld hl,mes.nolabel
	ld bc,11
	ldir

	ld hl,192 * 3
	ld de,mes.drive
	ld b,9+11
	call print.de.b

	ld a,(load.entries)
	cp 24
	jr c,$+4
	ld a,24
	ld c,a
	ld de,loader.dir
	ld hl,192 * 4 + 1
le.loop:
	push de
	push hl
	ld b,8
	call print.de.b
	inc l
	ld b,20
	call print.de.b
	pop hl
	ld de,192
	add hl,de
	pop de
	ld a,e
	add load.len
	ld e,a
	jr nc,$+3
	inc d

	dec c
	jr nz,le.loop

still.esc:
	ld a,247				;to prevent immediate exit
	in a,(status.register)	;when escape used to exit
	bit 5,a					;"DEMO"
	jr z,still.esc

	ld hl,4 * 192
	ld (curs.offs+1),hl
	ld a,(load.entries)
	dec a
	ld (max.select+1),a

	ld c,0
	call print.cursor
cursor.lp:
	call get.entry
	ld a,(ix+28)
	res 6,a
	ld de,mes.noi
	or a
	jr z,got.mes
	ld de,mes.pro
	dec a
	jr z,got.mes
	ld de,mes.sta
	dec a
	jr z,got.mes
	ld de,mes.drv
got.mes:
	ld a,(ix+28)
	bit 7,a
	jp nz,disc.mess		;new disc message
	push ix
	push de
	pop ix
	ld (ix+26),"8"
	bit 6,a
	jr z,gm.is.8
	ld (ix+26),"4"
gm.is.8:
	pop ix
	push bc				;c = select position
	push de
	ld a,e
	add 14
	ld e,a
	jr nc,$+3
	inc d
	ld a,(ix+38)
	call cnv.a.to.de
	pop de

	ld b,32
	ld hl,29 * 192
	call print.de.b

	ld hl,mes.size
	ld a,(ix+29)		;length in patterns
	ld b,"0"
	cp 100
	jr c,gm.len.100
	inc b
	sub 100
gm.len.100:
	ld (hl),b
	inc hl
	ex de,hl
	call cnv.a.to.de

	ld l,(ix+36)
	ld H,(ix+37)

	ld bc,100
	ld a,"0"-1
	or a
gm.get.big:
	sbc hl,bc
	inc a
	jr nc,gm.get.big
	add hl,bc
	ld de,mes.size+18
	ld (de),a
	inc de
	ld a,l
	call cnv.a.to.de

	ld de,mes.size+24
	ld a,(ix+30)
	cp "*"
	jr nz,gm.is.date
	ld hl,mes.no.date
	ld bc,8
	ldir
	jr gm.got.date
gm.is.date:
	ex de,hl
	ld (hl),a
	inc hl
	ld a,(ix+31)
	ld (hl),a
	inc hl
	ld (hl),"-"
	inc hl
	ld a,(ix+32)
	ld (hl),a
	inc hl
	ld a,(ix+33)
	ld (hl),a
	inc hl
	ld (hl),"-"
	inc hl
	ld a,(ix+34)
	ld (hl),a
	inc hl
	ld a,(ix+35)
	ld (hl),a
gm.got.date:
	ld de,mes.size
	pop bc
	jr normal.mess

disc.mess:
	ld hl,29 * 192
	ld b,32
blnk.line:
	ld a," "
	call print.chr
	djnz blnk.line
normal.mess:
	ld b,32
	ld hl,30 * 192
	call print.de.b

	call cursor.select

	ld a,247
	in a,(status.register)
	bit 5,a
	jr z,loader.quit

	call scan.return
	jp z,select.key

	ld a,%11011111
	in a,(keyboard.register)
	bit 1,a
	ld a,0
	jr nz,not.o
still.o:
	scf
	jr c,not.o.nc
	ld a,(loader.octaves+1)
	xor %110
	ld (loader.octaves+1),a
	call print.oct
	ld a,55	;scf
not.o:
	ld (still.o),a
not.o.nc:
	jp cursor.lp

print.oct:
	ld hl,192+26
	ld de,mes.oct
	ld b,5
	call print.de.b

	ld a,(loader.octaves+1)
	add "0"
	call print.chr

	ret

loader.quit:
	in a,(low.memory.page.register)
	and 31
	out (high.memory.page.register),a
	jp quit.hi

	org $+32768
quit.hi:
	xor a
	out (low.memory.page.register),a
	rst 0

	org $-32768

;check to see if the sum of sample lengths + patterns = file len
file.check:
	ld hl,temp.spc
	ld bc,20
	ldir

	ld (bytes.per+1),a		;2=normal, 1=compressed?

	push ix
	ld bc,(temp.spc+1080)
	ld a,1
	or a
	ld hl,&2E4D				;"M."
	sbc hl,bc
	jr z,pl.got.type
	inc a
	or a
	ld hl,&4C46				;"FL"
	sbc hl,bc
	jr z,pl.got.type
	xor a
pl.got.type:				;0=nst, 1=m.k., 2=flt4
	ld (de),a
	ld a,(bytes.per+1)
	dec a
	ld a,(de)
	jr nz,pl.not.comp
	set 6,a					;compressed 4 bit
	ld (de),a
pl.not.comp:
	inc de
	ld hl, temp.spc + ( 30 * 31 ) + 20
	and 63
	jr nz,$+5
	ld hl, temp.spc + ( 30 * 15 ) + 20
	ldi
	push de
	ld bc, 31 * 256
	ld hl, 30 * 31 + 20 + 130 + 4
	and 63
	jr nz,fc.is.nst
	ld b,15
	ld hl, 30 * 15 + 20 + 130
fc.is.nst:
	xor a
	ld (sample.count+1),a
	ld ix,temp.spc+20
add.all.smp:
	ld d,(ix+22)
	ld e,(ix+23)
bytes.per:
	ld a,2
times.sample:
	add hl,de
	jr nc,$+3
	inc c
	dec a
	jr nz,times.sample
	ld a,d
	or a
	jr nz,fc.is.samp
	ld a,e
	cp 2
	jr c,fc.not.samp
fc.is.samp:
	ld a,(sample.count+1)
	inc a
	ld (sample.count+1),a
fc.not.samp:
	ld de,30
	add ix,de
	djnz add.all.smp
	inc ix
	inc ix
	ld b,128
	ld e,0
get.hi.patt:
	ld a,(ix)
	inc ix
	cp e
	jr c,$+3
	ld e,a
	djnz get.hi.patt
	inc e
	ld b,e
	ld de,1024
add.all.pat:
	add hl,de
	jr nc,$+3
	inc c
	djnz add.all.pat
						;so now chl = calc. size
	ld a,(bytes.per+1)
	dec a
	jr z,fl.is.half
	ld a,h
	ld (file.len+1),a
	ld a,c
	ld (file.len+2),a
fl.is.half:
	pop de
	pop ix
	ret

insert.size:
file.len:
	ld hl,0
	srl h
	rr l
	srl h
	rr l
	ex de,hl
	ld (hl),e	;size in k
	inc hl
	ld (hl),d
	inc hl
sample.count:
	ld a,0
	ld (hl),a
	ret


fc.sam:
	push de

	ld de,(temp.spc-9+1);length mod 16384
	ld a,(temp.spc-9+7) ;length in pages (16384)
	and %00000011
	rrca
	rrca
	add D
	ld d,a
	ld a,(temp.spc-9+7)
	jr nc,$+4
	add 4
	srl a
	srl a

	ex de,hl
sm.resub:
	or a
	sbc hl,de
	sbc c
	jr nc,sm.got.maxmin
	adc c
	add hl,de
	ex de,hl
	ld b,a
	ld a,c
	ld c,b
	jr sm.resub
sm.got.maxmin:		;ahl=difference calc len & file len
	pop de
	or h
	ret


mes.load:	defm " Loading: "

select.key:
	call get.entry

	ld de,mes.load
	ld b,10
	ld hl,192 * 31
	call print.de.b
	push ix
	pop de
	ld a,e
	add 8
	ld e,a
	jr nc,$+3
	inc d
	ld b,20
	call print.de.b
	xor a
	call print.chr
	xor a
	call print.chr

	ld a,(ix+28)			;mod type ,+128=drive
	bit 7,a
	jp nz,new.read

	push af

	call checknodisc
	jp z,loader

	push ix
	pop hl
	ld de,parafile
	ld bc,8
	ldir
	ex de,hl
	ld (hl),"M"
	inc hl
	ld (hl),"O"
	inc hl
	ld (hl),"D"

	ld hl,parafile
	call findfile
	ld a,(msdos+1)
	or a
	jr z,sam.load

	push hl
	pop ix
	ld e,(ix+26)
	ld d,(ix+27)

	ld a,load.page
	out (high.memory.page.register),a
	ld hl,load.offs
pc.load.all:
	call readcluster
	in a,(high.memory.page.register)
	bit 6,h
	res 6,h
	jr z,$+3
	inc a
	out (high.memory.page.register),a
	call getfatentry
	ld a,d
	cp 15
	jr nz,pc.load.all
	ld a,e
	cp &F8
	jr c,pc.load.all

	jp file.loaded


sam.load:
	ld de,1
sam.load.dir:
	ld hl,6144
	call rdphysec
	ld hl,6144
	call sam.match
	ld hl,6144+256
	call sam.match
	inc e
	ld a,e
	cp 11
	jr nz,sam.load.dir
	ld e,1
	inc d
	ld a,d
	cp 4
	jr nz,sam.load.dir
file.notfound:
	jp loader

sam.match:
	push ix
	ld a,(hl)
	cp 19
	jr nz,sam.no.match

	ld b,8
sam.match.blp:
	ld a,(ix)
	inc ix
	inc l
	cp (hl)
	jr nz,sam.no.match
	djnz sam.match.blp
	; inc l
	; ld a,(hl)
	; cp "."
	; jr nz,sam.no.match
	; inc l
	; ld a,(hl)
	; cp "m"
	; jr nz,sam.no.match
	pop ix

	pop af			;chuck return address

	ld a,l
	and 128
	or 13
	ld l,a
	ld d,(hl)		;first track
	inc l
	ld e,(hl)		;first sector

	ld hl,6144
	call rdphysec

	ld a,load.page
	out (high.memory.page.register),a
	ld hl,6144+9
	ld de,load.offs
	ld bc,510-9
	ldir
	ld d,(hl)
	inc l
	ld e,(hl)

	ld hl,load.offs+510-9
sam.load.lp:
	call rdphysec
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	ld a,e
	or d
	jr z,file.loaded
	in a,(high.memory.page.register)
	bit 6,h
	res 6,h
	jr z,$+3
	inc a
	out (high.memory.page.register),a
	jr sam.load.lp

sam.no.match:
	pop ix
	ret


file.loaded:
	pop af
	bit 6,a
	jp z,no.decompress
decompress:
	ld c,a
	ld a,load.page
	out (high.memory.page.register),a
	ld a,c
	res 6,a

	ld hl, 31 * 30 + 20 + 2 + 32768
	ld de,4
	or a
	ld a,31
	jr nz,dc.not.noise
	ld hl, 15 * 30 + 20 + 2 + 32768
	ld e,d
	ld a,15
dc.not.noise:
	ld (loader.instruments+1),a
	ld b,128
	ld a,(hl)
@searchtable:
	cp (hl)
	jr nc,@alreadyhi
	ld a,(hl)
@alreadyhi:
	inc hl
	djnz @searchtable	
	inc a

	add hl,de

	ld b,a
	ld e,load.page
convallppats:
	ld a,h
	add 4
	ld h,a
	bit 6,h
	res 6,h
	jr z,$+3
	inc e
	djnz convallppats

;sample starts directly after last pattern

	ld (smp1.offs+1),hl
	ld a,e
	ld (smp1.page+1),a

	ld ix,32768+20

;put starting addresses of samples in sample table

loader.instruments:
	ld b,0

	ld hl,0
	xor a
gsmp.blp:
	ld d,(ix+22)
	ld e,(ix+23)
	add hl,de
	jr nc,$+4
	add 4
	ld de,30
	add ix,de
	djnz gsmp.blp

	bit 7,h
	res 7,h
	jr z,$+4
	add 2
	bit 6,h
	res 6,h
	jr z,$+3
	inc a

	ld (samplen+1),hl
	ld (samppag+1),a

smp1.page:
	add 0
smp1.offs:
	ld de,0
	add hl,de
	jr nc,$+4
	add 2
	set 7,h
	bit 6,h
	res 6,h
	jr z,$+3
	inc a
	ld d,a

	exx
	ld hl,(smp1.offs+1)
	ld a,(smp1.page+1)
	ld d,a

samplen:
	ld bc,0
samppag:
	ld e,0

loop2:
	ld a,b
	or c
	jr z,end2

	ld a,d
	out (high.memory.page.register),a
	ld a,(hl)
	rlca
	rlca
	rlca
	rlca
	exx

	ex af,af'
	ld a,d
	out (high.memory.page.register),a
	ex af,af'
	and %11110000
	ld (hl),a
	inc hl
	exx
	ld a,d
	out (high.memory.page.register),a
	ld a,(hl)
	and %11110000
	ld (hl),a
	inc hl

	dec bc
	jr loop2
end2:
	bit 6,h
	res 6,h
	jr z,$+3
	inc d
	exx
	bit 6,h
	res 6,h
	jr z,$+3
	inc d
	exx
	ld bc,16384
	ld a,e
	dec e
	or a
	jr nz,loop2

no.decompress:
	call cls
	in a,(low.memory.page.register)
	and 31
	out (high.memory.page.register),a
loader.octaves:
	ld a,3				;3 or 5 octave
	call 57344			;demo
	ld a,c
	dec a
	jp nz,converted
	jp loader.quit

get.entry:
	ld ix,loader.dir-load.len
	ld de,load.len
	ld b,c
	inc b
g.load.entry:
	add ix,de
	djnz g.load.entry
	ret


new.read:
	and 127
	or a
	push af
	call z,select1
	pop af
	call nz,select2
	jp loader

print.cursor:
wait4int:
	in a,(status.register)
	and 8
	jr nz,wait4int

old.cursor:
	ld a,0
	call get.pos
	xor a
	ld de,32
	ld (hl),a
	add hl,de
	ld (hl),a
	add hl,de
	ld (hl),a
	add hl,de
	ld (hl),a
	add hl,de
	ld (hl),a

curs.blink:
	ld a,0
	inc a
	bit 3,a
	ld (curs.blink+1),a
	ret nz

	ld a,c
	ld (old.cursor+1),a

	call get.pos
	ld ix, colours + ( 4 * 8 ) 
	ld de,32
	ld (hl),%10000000
	ld a,(ix)
	inc ix
	set 5,h
	ld (hl),a
	res 5,h
	add hl,de
	ld (hl),%11000000
	ld a,(ix)
	inc ix
	set 5,h
	ld (hl),a
	res 5,h
	add hl,de
	ld (hl),%11100000
	ld a,(ix)
	inc ix
	set 5,h
	ld (hl),a
	res 5,h
	add hl,de
	ld (hl),%11000000
	ld a,(ix)
	inc ix
	set 5,h
	ld (hl),a
	res 5,h
	add hl,de
	ld (hl),%10000000
	ld a,(ix)
	inc ix
	set 5,h
	ld (hl),a
	ret

get.pos:
curs.offs:
	ld hl,3 * 192
	ld de,32
	or a
get.pos.lp:
	ret z
	add hl,de
	dec a
	jr get.pos.lp

;---------------------------------------------------------------

mes.oct:		defm "Oct: "
mes.drive:		defm "Drive 1: "
mes.label:		defm "Solar Flare"
mes.nolabel:	defm "No label   "
mes.nodisc:		defm "No disc    "
mes.noi:		defm "Noisetracker, 15 samples, 8 bits"
mes.pro:		defm "Protracker,   31 samples, 8 bits"
mes.sta:		defm "Startrekker,  31 samples, 8 bits"
mes.drv:		defm "Press RETURN for new directory. "
mes.size:		defm "127 song entries, 999k,         "
mes.no.date:	defm "no date "

option.dir:
	defm " < 1: > "
	defm "new disc for drive 1"
	defb 128
	defb 0
	defm "xxxxxx"
	defw 0
	defb 0

	defm " < 2: > "
	defm "new disc for drive 2"
	defb 129
	defb 0
	defm "xxxxxx"
	defw 0
	defb 0

load.entries:	defb 0

loader.dir:
	defm "filename"
	defm "20 char module title"
	defb 0			;module type 0=noise, 1=pro, 2=star +64 = 4 bit compressed
	defb 0			;length in patterns
	defm "010195"	;date stamp
	defw 0			;total size in k
	defb 0			;number of samples (len>1)

load.len:	equ 8+20+1+1+6+2+1

	defs 27 * load.len	;max 27 on screen (25 files)

;put a at address de and de+1 in ascii format

cnv.a.to.de:
	ld c,-1
cad.getten:
	sub 10
	inc c
	jr nc,cad.getten
	add 10 + "0"
	inc de
	ld (de),a
	dec de
	ld a,c
	add "0"
	ld (de),a
	inc de
	inc de
	ret


print.num:
	ld b,a
	and &F0
	rrca
	rrca
	rrca
	rrca
	call pr.num.hex
	ld a,b
	and &0F
pr.num.hex:
	add "0"
	cp ":"
	jr c,$+4
	add 7
	jp print.chr

print.chr:
	push bc
	push de
	push hl
	ld c," "
	cp " "
	jr c,unprintable
	cp 128
	jr nc,unprintable
	ld c,a
unprintable:
	ex de,hl
	ld b,0
	ld l,c
	ld h,b
	add hl,hl
	add hl,hl
	add hl,bc
	ld bc,font - 160	;-" "*5
	add hl,bc
	ld b,5
pr.chr.blp:
	ld a,(hl)
	ld (de),a
	inc hl
	ld a,e
	add 32
	ld e,a
	jr nc,$+3
	inc d
	djnz pr.chr.blp
	pop hl
	pop de
	pop bc
	inc l
	ret

print.screen:
	push de
	ld a,6
	call colour.scrn

	ld hl,0
	pop de
	ld c,32
wel.all:
	ld b,32
	push hl

pr.scr.blp:
	ld a,(de)
	inc de
	or a
	jr z,end.of.line
	call print.chr
	djnz pr.scr.blp
end.of.line:
	pop hl
	ld a,l
	add 192
	ld l,a
	jr nc,$+3
	inc h
	dec c
	jr nz,wel.all
	ld a,255
	ret
cls:
	ld hl,0
	ld de,1
	ld bc,6143
	ld (hl),l
	ldir

	ld hl,8192
	ld de,8193
	ld bc,6143
	ld (hl),l
	ldir

	ld hl,loader.palette + 15
	ld bc,16 * 256 + color.look.up.table
	otdr

	ret

print.de.b:
	ld a,(de)
	inc de
	or a
	jr z,eop
	call print.chr
	djnz print.de.b
	ret

eop:
	ld a," "
	call print.chr
	djnz eop
	ret


colour.scrn:
	ld (line.size+1),a

	ld hl,8192
col.loop:
	ld c,(ix)
	inc ix
col.clp1:
	ld a,(ix)

	ld de,colours
	add a,a
	add a,a
	add a,a
	add a,e
	ld e,a
	jr nc,$+3
	inc d

line.size:
	ld b,8
col.blp2:
	ld a,(de)
	inc de
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l

	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l

	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l

	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc l
	ld (hl),a
	inc hl

	djnz col.blp2

	dec c
	jr nz,col.clp1
	inc ix

	ld a,h
	cp ( 8192 + 6144 ) // 256
	jr nz,col.loop
	ret


;---------------------------------------------------------------

load.screen:
	defm "SAM MOD player             v2.20"
	defm "(C) 2018 Stefan Drissen"
	defb 0,0,0,0,0,0,0
	defb 0,0,0,0,0,0,0,0
	defb 0,0,0,0,0,0,0,0
	defb 0,0,0,0,0,0,0
	defm "Use CURSORS + RETURN or JOYSTICK"


col.load:
	defb 3,2,1,3,25,1,2,3,1,5

device.scrn:
	defm "SAM MOD player             v2.20"
	defm "(C) 2018 Stefan Drissen"
	defb 0,0
	defm "       SELECT SOUND DEVICE"
	defb 0,0
	defm " Colour Look Up Table     (test)"
	defm " Soundchip       (3 bits stereo)"
	defm " SAMdac on port 1(7 bits stereo)"
	defm " SAMdac on port 2"
	defb 0
	defm " DAC on port 1     (6 bits mono)"
	defm " DAC on port 2"
	defb 0
	defm " Blue Alpha Sampler(6 bits mono)"
	defm " Quazar Soundcard (8 bits surr.)"
	defb 0
	defm "The (SAM)DAC can be connected to"
	defm "parallel printer port 1 or 2."
	defb 0,0,0
	defm "       SELECT AMIGA SPEED"
	defb 0,0
	defm " PAL  (7.0937892 MHz)"
	defb 0
	defm " NTSC (7.1590905 MHz)"
	defb 0,0
	defm "This will only make a           "
	defm "noticeable difference if a MOD  "
	defm "consists of very long samples.  "
	defb 0,0
	defm "Use the CURSORS to make a       "
	defm "selection, press SPACE to toggle"
	defm "between DEVICE and SPEED, press "
	defm "RETURN to continue."
	defb 0

col.device:	defb 3,2,2,4,8,1,5,3,2,4,3,1,5,3,4,5

col.black:	defb 2,2,27,0,2,3,1,5

colours:
	defb 0,0,0,0,0,0,0,0						;0
	defb 1,2,3,2,1,0,0,0						;1
	defb 4,5,6,5,4,0,0,0						;2
	defb 7,9+56,3,9+56,7,0,0,0					;3
	defb 10+56,11+56,12+56,11+56,10+56,0,0,0	;4
	defb 13+56,14+56,6,14+56,13+56,0,0,0		;5


font:
	MDAT "../res/font.bin"

;---------------------------------------------------------------
;directory stuff
dir.stuff:

m.vollabel:	defm "01234567890"

	ld a,(driveselect+1)
	rlca
	rlca
	rlca
	rlca
	add "A"
	rst 16
	ld a,":"
	rst 16
	ld hl,(path)

;---------------------------------------------------------------
program:


badcommand:
	ld hl,msbadfile
	ret
	
badfilename:
	ld hl,msbadname
	ret
	
filenotfound:
	ld hl,msfilenot
	ret

invaliddir:
	ld hl,msinvdir
	ret

errnodisc:
	ld sp,(save.sam.sp+1)
	ld a,1
	ld (nodisc+1),a
	ret

commands:

msbadfile:
	defb 13
	defm "Bad command or file name"
	defb 13,0

msbadname:
	defb 13
	defm "Invalid file name"
	defb 13,0

msfilenot:
	defb 13
	defm "File not found"
	defb 13,0

msinvdir:
	defb 13
	defm "Invalid subdirectory"
	defb 13,0


parlast:		defw 0

parameter:		defs 255

parafile:		defs 11

matchfile:		defs 11

getparameter:
	ld hl,(parlast)
	ld de,parameter
getparlp2:
	ld a,(hl)
	ld (de),a
	or a
	ret z
	inc hl
	cp " "
	jr z,getparlp2

getparlp:
	ld (de),a
	inc de
	ld a,(hl)
	inc hl
	or a
	jr z,gpnomore
	cp " "
	jr z,gpnomore
	cp "\"
	jr z,gpnomore
	cp "."
	jr z,gpnomore
	jr getparlp
gpnomore:
	dec hl
	ld (parlast),hl
	xor a
	ld (de),a
	dec a
	ret


chdir:
	call getparameter
	jp z,badcommand
	call getinputpath
	call readroot
	call loadpath
	jp c,invaliddir
	ld hl,temppath
	ld de,(path)
	ld bc,64
	ldir
	ret


findfile:
	ld (save.sam.sp+1),SP

	ld de,matchfile
	ld bc,11
	ldir
	call readroot
	call loadpath
	call c,resetpath
	ld bc,(direntries)
	ld hl,(data)
fmclp:
	push hl
	push bc
	ld b,11
	ld de,matchfile
fmblp:
	ld a,(de)
	cp (hl)
	inc de
	inc hl
	jr nz,nomatch
	djnz fmblp
	pop bc
	pop hl
	ret
nomatch:
	pop bc
	pop hl
	ld de,32
	add hl,de
	dec bc
	ld a,b
	or c
	jr nz,fmclp
	pop af				;chuck return address
	jp file.notfound


resetpath:
	ld hl,(path)
	ld (hl),"\"
	inc hl
	ld (hl),0
	call copypath
	jp readroot
copypath:
	push hl
	push de
	push bc
	ld hl,(path)
	ld de,temppath
	ld bc,64
	ldir
	pop bc
	pop de
	pop hl
	ret

loadpath:
	ld a,(temppath+1)
	or a
	ret z
	ld hl,temppath
	ld (parlast),hl
	call getparameter
lploop:
	ld a,(parameter)
	or a
	ret z
	call getinputfile

	push hl
	ld hl,(data)
	ld bc,(direntries)
lpmatchlp:
	ld de,parafile
	push hl
	push bc
	ld b,11
lpmatchblp:
	ld a,(de)
	cp (hl)
	jr nz,lpnomatch
	inc hl
	inc de
	djnz lpmatchblp
	pop bc
	pop ix
	ld a,(ix+11)
	and %00010000
	jr nz,lpisdir
	push ix
	push bc
	jp lpnomatch
lpisdir:
	ld e,(ix+26)
	ld d,(ix+27)

	ld hl,(data)
	ld bc,0
lpreadmore:
	push bc
	call readcluster
	pop bc
	inc bc
	call getfatentry
	ld a,d
	cp 15
	jr nz,lpreadmore
	ld a,e
	cp &F8
	jr c,lpreadmore
	ld hl,(bytescluster)
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	srl h
	rr l
	ex de,hl
	ld hl,0
lpaddlp:
	add hl,de
	dec bc
	ld a,b
	or c
	jr nz,lpaddlp

	ld (direntries),hl
	pop hl
	jp lploop
lpnomatch:
	pop bc
	pop hl
	ld de,32
	add hl,de
	dec bc
	ld a,b
	or c
	jr nz,lpmatchlp
	pop hl
	scf
	ret


getinputpath:
	call copypath
	ld hl,parameter
	ld de,temppath
	ld a,(hl)
	cp "\"
	jr z,gipnewpath
	ld a,(de)
	or a
	jr z,gipnewpath
gipfindend:
	inc de
	ld a,(de)
	or a
	jr nz,gipfindend
	ld a,e
	cp ( temppath + 1 ) \ 256
	jr nz,gipnewpath
	dec de
gipnewpath:
	ld a,"\"
	ld (de),a
	inc de

	ld a,(parameter)
	cp "\"
	jr nz,gipns
	ld a,(parameter+1)
	or a
	jr nz,gipns
	push de
	call getparameter
	pop de
	ld a,(parameter)
gipns:
	cp "."
	jr nz,gipnotdot
	push de
	call getparameter
	pop de
	ld a,(parameter)
	cp "."
	jr nz,gipfndlstp
	dec de
gipfndlstp:
	dec de
	ld a,(de)
	cp "\"
	jr nz,gipfndlstp
	ld a,e
	cp temppath \ 256
	jr nz,$+3
	inc de
	xor a
	ld (de),a

	push de
	call getparameter
	pop de
	jp gipdoneext
gipnotdot:
	push de
	call getinputfile
	pop de
	jp c,invaliddir

	ld hl,parafile
	ld b,8
gipcopyname:
	ld a,(hl)
	cp " "
	jr z,gipdonename
	ld (de),a
	inc hl
	inc de
	djnz gipcopyname
gipdonename:
	ld hl,parafile+8
	ld a,(hl)
	cp " "
	jr z,gipdoneext
	ld a,"."
	ld (de),a
	inc de
	ld b,3
gipcopyext:
	ld a,(hl)
	cp " "
	jr z,gipdoneext
	ld (de),a
	inc hl
	inc de
	djnz gipcopyext
gipdoneext:
	ld a,(parameter)
	cp "\"
	jr z,gipnewpath
gipend:
	xor a
	ld (de),a
	ret


getinputfile:
	ld hl,parafile
	ld b,11
clearpf:
	ld (hl)," "
	inc hl
	djnz clearpf

	ld hl,parameter
	ld de,parafile
	ld a,(hl)
	cp "."
	jr z,gifextonly
	ld b,9
gifcopynm:
	ld a,(hl)
	inc hl
	or a
	jr z,gifendname
	cp "\"
	jr z,gifcopynm
	ld (de),a
	inc de
	djnz gifcopynm
	scf					;file longer than 8 chars
	ret
gifendname:
	call getparameter
	jr z,gifendext

	ld hl,parameter
	ld a,(hl)
	cp "."
	jr nz,gifendext
gifextonly:
	inc hl
	ld de,parafile+8
	ld b,4
gifcopyext:
	ld a,(hl)
	or a
	jr z,gifendext
	ld (de),a
	inc hl
	inc de
	djnz gifcopyext
	call getparameter
	scf
	ret			;extension longer than 3 chars
gifendext:
	xor a
	ret

select1:
	ld a,0
	call setdrive
	ld hl,patha
	ld (path),hl
	ret

select2:
	ld a,1
	call setdrive
	ld hl,pathb
	ld (path),hl
	ret


filecount:	defw 0

dir:
	ld (save.sam.sp+1),sp

	call readroot

	xor a
	ld (m.vollabel),a

	ld a,(direntries)
	ld hl,(data)
	ld de,11
	add hl,de
	ld de,32
	ld b,a
prfindlabel:
	ld a,(hl)
	and 8
	jr nz,prlabel
	add hl,de
	djnz prfindlabel
prlabel:
	jr z,prnolabel
	ld de,11
	xor a
	sbc hl,de
	ld b,11
	ld de,m.vollabel
prlabblp:
	LD a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz prlabblp
prnolabel:
prdonelabel:
	call copypath
	call loadpath
	call c,resetpath

	ret


;===============================================================

checknodisc:
	push bc
	push hl
	ld c,%11010000		;force interrupt
	call dsendc
	ld c,dstatcom
	call dconvert
	ld hl,0
clearbit:
	dec hl
	ld a,h
	or l
	jr z,cdisdisc
	in a,(c)
	bit 1,a
	jr nz,clearbit
	ld hl,0
cndlp:
	in a,(c)
	nop
	nop
	bit 1,a
	jr nz,cdisdisc
	dec hl
	ld a,h
	or l
	jr nz,cndlp
cdisdisc:
	pop bc
	pop hl
	ret

;---------------------------------------------------------------
;get FAT entry
;de = cluster

getfatentry:
	push hl
	ld h,d
	ld l,e
	add hl,hl
	add hl,de

	ld de,fat
	srl h
	rr l
	jr c,oddfat

	add hl,de
	ld e,(hl)
	inc hl
	ld a,(hl)
	and 15
	ld d,a
	pop hl
	ret

oddfat:
	add hl,de
	ld a,(hl)
	rrca
	rrca
	rrca
	rrca
	and 15
	ld e,a
	inc hl
	ld a,(hl)
	ld d,a
	rlca
	rlca
	rlca
	rlca
	and 240
	or e
	ld e,a
	srl d
	srl d
	srl d
	srl d
	pop hl
	ret


;---------------------------------------------------------------
;read FAT at fixed address, (data) -> first address after FAT

readfat:
	push hl
	ld de,1
	ld hl,fat
	ld a,(bssecsfat)
	ld b,a
rfblp:
	call rdlogsec
	inc de
	djnz rfblp
	ld (data),hl
	pop hl
	ret

;---------------------------------------------------------------
;read root directory
;hl= address

readroot:
	call rdboot
	call readfat
	call startcluster
	push de
	call startroot
	pop hl
	xor a
	sbc hl,de
	ld b,l
	push bc
	call startroot
	pop bc
	ld hl,(data)
rdrtlp:
	call rdlogsec
	inc de
	djnz rdrtlp
	ld a,1
	or a
	ret



;---------------------------------------------------------------
;read cluster from disc
;de= cluster number (2-711)
;hl= address

readcluster:
	push de
	push hl
	dec de
	dec de
	ld hl,0
	ld a,(bsclusize)
	ld b,a
rccalc:
	add hl,de
	djnz rccalc
	call startcluster
	add hl,de
	ex de,hl
	pop hl

	ld a,(bsclusize)
	ld b,a
rcclusrep:
	call rdlogsec
	inc de
	djnz rcclusrep
	pop de
	ret

;---------------------------------------------------------------
;calculate start sector of root directory
;returns de with

startroot:
	ld de,1
	ld a,(bsnumfats)
	ld b,a
rcfats:
	ld a,(bssecsfat)
	add a,e
	ld e,a
	djnz rcfats
	ret

;calculate total number of clusters on disc

calcclusters:
	ld hl,(bstotsecs)
	call startcluster
	xor a
	sbc hl,de
	ld a,(bsclusize)
ccdiv:
	srl a
	jr z,ccdonediv
	srl h
	rr l
	jr ccdiv
ccdonediv:
	ld c,L
	ld b,h
	ret

;calculate start sector of cluster 2 (first data cluster)
;returns de with logical sector of first data cluster

startcluster:
	push hl
	ld hl,(bsrootentries)
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl	; * 32 bytes per entry
	ld de,0
	ld bc,(bssecsize)
	xor a
scdivsecs:
	sbc hl,bc
	inc e
	jr nc,scdivsecs
	dec e
	ex de,hl
	call startroot
	add hl,de
	ex de,hl
	pop hl
	ret

;---------------------------------------------------------------
;read logical sector from disc
;de= sector number (0 - 1439 (for 720k disc))
;hl= address

rdlogsec:
	push bc
	push de
	push hl
	ex de,hl
	ld bc,(bssecstrack) ;number of secs per track
	ld de,0
	xor a
rlsdiv9:
	sbc hl,bc
	inc d
	jr nc,rlsdiv9
	adc hl,bc
	dec d
	ld e,l
	ld a,(bssides)
	cp 2
	jr nz,$+4
	rrc d
	pop hl
	call rdphysec
	pop de
	pop bc
	ret


;---------------------------------------------------------------
;read physical sector from disc
;d = track (+128 for side 2)
;e = sector
;hl= address

dstatcom:	equ 224
dtrack:		equ 225
dsec:		equ 226
ddata:		equ 227

rdphysec:
	; di

	xor a
	ld (derrcount+1),a
	ld (rpshl+1),hl
rpsretry:

rpshl:
	ld hl,0
	push de
	ld a,d
	and 127
	ld d,a
rpsnxtrk:
	call busy
	ld c,dtrack
	call dconvert
	in a,(c)
	cp d
	jr z,rpright
	ld c,%01011000
	call mergestep
	jr c,$+4
	set 5,c
	call dsendc
	jr rpsnxtrk
rpright:
	pop de
	ld c,dsec
	call dconvert
	ld a,e
	out (c),a

	ld c,%10000000
	call dsendc
	ld c,dstatcom
	call dconvert
	ld a,c
	ld (dnodata+1),a
	ld c,ddata
	call dconvert
	jr dnodata
dread:
	ini
dnodata:
	in a,(dstatcom)
	bit 1,a
	jr nz,dread
	rrca
	jr c,dnodata
	and %00001110

	jr z,finread
derrcount:
	ld a,0
	inc a

	ld (derrcount+1),a
	cp 5
	jp c,rpsretry

	push hl
	push de
	push bc
	ld a,"0"
	bit 7,d
	jr z,$+3
	inc a
	ld (mes.sec+12),a
	ld a,d
	and 127
	push de
	ld de,mes.sec+21
	call cnv.a.to.de
	pop de
	ld a,e
	ld de,mes.sec+29
	call cnv.a.to.de

	ld hl,31*6*32
	ld de,mes.sec
	ld b,32
	call print.de.b

	pop bc
	pop de
	pop hl
	inc h
	inc h

finread:
	; ei
	ret

mes.sec:	defm "ERROR: side 0, track 00, sec 00"

dsendc:
	push bc
	call busy
	pop bc
dsendnow:
	ld a,c
	ld c,dstatcom
	call dconvert
	out (c),a
	ld a,16
dwait:
	dec a
	jr nz,dwait
	ret

busy:
	ld c,dstatcom
	call dconvert
busylp:
	in a,(c)
	bit 0,a
	jr nz,busylp
	ret

dconvert:
	push af
	bit 7,d
	jr z,$+4
	set 2,c
driveselect:
	ld a,0		; +16 for drive 2
	or c
	ld c,a
	pop af
	ret

setdrive:
	rlca
	rlca
	rlca
	rlca
	ld (driveselect+1),a
	ret

mergestep:
	push af
	ld a,(driveselect+1)
	or a
steprate.1:
	ld a,%11
	jr z,$+4
steprate.2:
	ld a,%00
	or c
	ld c,a
	pop af
	ret

;---------------------------------------------------------------
;read boot sector from disc at fixed address

rdboot:
	call checknodisc
	jp z,errnodisc
	ld c,%00000000
	call mergestep
	call dsendc
	ld de,1
	ld hl,bootsector
	call rdphysec
	ld hl,(bstotsecs)
	ld de,(bssecstrack)
	ld a,d
	or e
	jr z,notpcdisc
	xor a
	ld bc,0
rblp:
	inc bc
	sbc hl,de
	jr nc,rblp
	dec bc
	add hl,de
	ld a,h
	or l
	jr nz,notpcdisc
	ld h,b
	ld l,c
	ld de,(bssides)
	ld a,d
	or e
	jr z,notpcdisc
	xor a
rblp2:
	sbc hl,de
	jr nc,rblp2
	add hl,de
	ld a,h
	or l
	jr nz,notpcdisc

	ld hl,0
	ld bc,(bssecsize)
	ld a,(bsclusize)
rbcalcsz:
	add hl,bc
	dec a
	jr nz,rbcalcsz
	ld (bytescluster),hl
	ld hl,(bsrootentries)
	ld (direntries),hl

	ld ix,col.black
	ld a,6
	jp colour.scrn

notpcdisc:
	xor a
	ld (msdos+1),a
save.sam.sp:
	ld sp,0
	ret

bootsector:		defs 3
bssysid:		defm "01234567"
bssecsize:		defw 0
bsclusize:		defb 0
bsressec:		defw 0
bsnumfats:		defb 0
bsrootentries:	defw 0
bstotsecs:		defw 0
bsformatid:		defb 0
bssecsfat:		defw 0
bssecstrack:	defw 0
bssides:		defw 0
bshiddensecs:	defw 0,0
bsbigtot:		defw 0,0
bsphysdrv:		defb 0
				defw 0
bsvolserial:	defb 0,0,0,0
bsvolname:		defm "01234567890"

bytescluster:	defw 0

direntries:		defw 0

data:			defw 0		;points to first address after FAT

fat:			equ 2 * 192

temp.spc:		equ 6144+9

;===============================================================

length:			equ $-16384

