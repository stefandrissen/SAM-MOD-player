;SAM MOD player - DOS loader 

;(C) 1996-2019 Stefan Drissen

; to do:
;
; - fix 4-bit "compressed" mods 
; - handle dos errors better (doser) 
; + add sample runways while loading

include "memory.i"
include "ports.i"
include "dos.i"

load.offs:	equ 32768

	org &c000

;---------------------------------------------------------------

	jp loader.start

;---------------------------------------------------------------


loader.device:			defb device.samdac	; [0-5]

	device.saa:			equ 0
	device.samdac:		equ 1
	device.dac:			equ 2
	device.bluealpha:	equ 3
	device.quazar:		equ 4
	device.clut:		equ 5
	
loader.device.port:		defb 0	; [0-1]
loader.speed:			defb 0	; [0-1]

	speed.pal:			equ 0
	speed.ntsc:			equ 1
	
loader.external.ram:	defb 0	; [0-4]

loader.drive:			defb 1	; [1-2]

;---------------------------------------------------------------
loader.font_high:
;---------------------------------------------------------------
	mdat "../res/font.bin"


;---------------------------------------------------------------
relocate.scan:

	org inst.buffer
	
scan.external.memory:	

	in a,(port.hmpr)
	ld c,a
	
	ld hl,&8000
	ld b,0

	ld a,high.memory.external
	out (port.hmpr),a	

@scan.megs:

	ld a,b
	out (port.xmpr.c),a

	xor a
	ld (hl),a
	cp (hl)
	jr nz,@not.ram
	dec a
	ld (hl),a
	cp (hl)
	jr nz,@not.ram

	ld a,b
	add &40
	ld b,a
	jr nc,@scan.megs
	
	ld b,1
	
@not.ram:

	ld a,c
	out (port.hmpr),a
	
	ld a,b
	rlca
	rlca	
	
	ret
	
scan.external.memory.len: equ $ - scan.external.memory

	org relocate.scan + scan.external.memory.len

;---------------------------------------------------------------
loader.start:
;---------------------------------------------------------------

	di
	
	call cls
	
	ld a,page.loader - 1 + video.mode.2
	out (port.vmpr),a
	
	ld (svar.cuscrnp),a
	ld a,1
	ld (svar.mode),a	
	
	ld hl,relocate.scan
	ld de,inst.buffer
	ld bc,scan.external.memory.len
	ldir

	call scan.external.memory

	or a
	jr z,@no.megabyte.1

	ld (loader.external.ram),a

	ld hl,device.screen.memory
	add a,"0"
	ld (hl),a
	inc hl
	ld (hl)," "
	inc hl
	ld (hl),"M"
	inc hl
	ld (hl),"B"
	inc hl
	ld (hl)," "
	inc hl
	ld (hl)," "
	inc hl
	
@no.megabyte.1:

	ld de,device.screen
	ld ix,device.attributes
	call print.screen

	ld hl,video.memory.32.rows * row.device + video.memory.high
	ld (curs.offs+1),hl

	ld a,(loader.device)
	ld c,a
	add a,a
	add a,c
	add a,a
	ld c,a					; c = loader.device * 6
	ld (old.cursor+1),a
	xor a
	ld (curs.blink+1),a
	call print.cursor

	ld hl,video.memory.32.rows * row.speed + video.memory.high
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

	ld hl,video.memory.32.rows * row.device + video.memory.high
	ld (curs.offs+1),hl
	ld a,5
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
	
	cp device.samdac
	jr c,@no.port
	cp device.dac + 1
	jr nc,@no.port
	
	call scan.keyboard.left.right
	ld a,c
	
@no.port:	
	
	call print.device.details

	call scan.keyboard.return
	jr z,@loop

	xor a
	ld (curs.blink+1),a
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	call print.cursor

;---------------------------------------------------------------
select.speed:

	ld hl,video.memory.32.rows * row.speed + video.memory.high
	ld (curs.offs+1),hl
	ld a,1
	ld (max.select+1),a

	ld a,(loader.speed)
	call print.speed.details
	ld c,a
	add a,a
	add a,c
	add a,a
	ld (old.cursor+1),a
@loop:
	call cursor.select

	ld a,c
	call speed.details

	call scan.keyboard.return
	jr nz,selected

	call scan.escape
	jr z,@loop

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

	ld hl,relocate.call.burstplayer.create
	ld de,inst.buffer
	ld bc,call.burstplayer.create.len
	ldir
	
	ld a,(loader.device)
	ld hl,@device.mapping
	add a,l
	ld l,a
	jr nc,@nc
	inc h
@nc:
	ld a,(hl)
	ld hl,loader.device.port
	add a,(hl)
		
	ld (@loader.device+1),a
	ld a,(loader.speed)
	ld (@loader.speed+1),a
	ld a,(loader.external.ram)
	ld (@loader.external.ram+1),a
	
	call call.burstplayer.create

	jp loader
	
; map new loader screen to burstplayer device numbers - fix burstplayer later on
@device.mapping:
	defb 1	; soundchip	
	defb 2	; samdac
	defb 4	; dac
	defb 6	; blue alpha
	defb 7	; quazar
	defb 0	; screen 	

;---------------------------------------------------------------
relocate.call.burstplayer.create:

	org inst.buffer
	
call.burstplayer.create:

	in a,(port.hmpr)
	ld (@store.hmpr+1),a

	ld a,page.create.burstplayer
	out (port.hmpr),a		
	
@loader.device:
	ld a,0
	ld (burstplayer.device),a
@loader.speed:	
	ld a,0
	ld (burstplayer.speed),a
@loader.external.ram:	
	ld a,0
	ld (burstplayer.external.ram),a
	
	call burstplayer.create
	
@store.hmpr:
	ld a,0
	out (port.hmpr),a
	
	ret	
	
call.burstplayer.create.len: equ $ - call.burstplayer.create

	org relocate.call.burstplayer.create + call.burstplayer.create.len			

;---------------------------------------------------------------
	
print.device.details:
	
	push af
	ld hl,device.details
	add a
	ld e,a
	ld d,0
	add hl,de
	ld a,(hl)
	add "0"
	push hl
	ld hl,video.memory.32.rows * 30 + video.memory.high
	call print.chr
	ld de,text.bits
	ld b,7
	call print.de.b
	pop hl
	inc hl
	ld a,(hl)
	add a
	ld e,a
	ld d,0
	ld hl,device.texts
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld b,32 - 8
	ld hl,video.memory.32.rows * 30 + 8 + video.memory.high
	call print.de.b

	pop af
	push af

	ld hl,video.memory.32.rows * 30 + 26 + video.memory.high
	ld b,6
	ld de,text.blank
	
	cp device.samdac
	jr c,@no.port
	cp device.dac+1
	jr nc,@no.port

	ld de,text.port
	
	ld a,(loader.device.port)
	add "1"
	ld (text.port+5),a	
@no.port:				
	call print.de.b
	
	pop af
	
@same:	
		
	ld (loader.device),a
	ret

text.bits:	
	defm " bits, "
	defb 0
	
text.port:
	defm "port x"
text.blank:		
	defb 0
	
	

;---------------------------------------------------------------
speed.details:

	ld hl,loader.speed
	cp (hl)
	ret z

	ld (hl),a
	
print.speed.details:
	
	push af
	
	ld hl,text.speed
	add a,a
	add a,l
	ld l,a
	jr nc,@nc
	inc h
@nc:
	ld e,(hl)
	inc hl
	ld d,(hl)
		
	ld b,32
	ld hl,video.memory.32.rows * 30 + video.memory.high
	call print.de.b
	
	pop af
	
	ret	
	
text.speed:			defw text.speed.pal,text.speed.ntsc	
	
text.speed.pal: 	defm "7.0937892 MHz"
					defb 0
text.speed.ntsc:	defm "7.1590905 MHz"
					defb 0
	
;---------------------------------------------------------------
cursor.select:

; selection routine
; C = current position
; min selection = 0
; max selection = (max.selec+1)

	push bc
	ld a,c
	add a,a
	add a,c
	add a,a
	ld c,a
	call print.cursor
	pop bc

	ld a,keyboard.cursors_ctrl
	in a,(port.keyboard)
	bit 1,a
	jr z,curs.up
	bit 2,a
	jr z,curs.dn

	ld a,keyboard.67890
	in a,(port.keyboard)
	bit 1,a
	jr z,curs.up
	bit 2,a
	jr z,curs.dn
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

scan.escape:
	ld a,keyboard.caps_esc
	in a,(port.status)
	and %00100000 
	jr z,@still.esc
	
	xor a 
	ret
	
@still.esc:
	ld a,keyboard.caps_esc
	in a,(port.status)
	and %00100000
	jr z,@still.esc
	ret

scan.keyboard.return:
	ld a,keyboard.hjkl_return
	in a,(port.keyboard)
	and %00001
	jr z,@still.return
	
	ld a,keyboard.67890
	in a,(port.keyboard)
	and %00001
	jr z,@still.0
	
	xor a	
	ret

@still.return:
	ld a,keyboard.hjkl_return
	in a,(port.keyboard)
	and %00001
	jr z,@still.return
	ret

@still.0:	
	ld a,keyboard.67890
	in a,(port.keyboard)
	and %00001
	jr z,@still.0
	ret

scan.keyboard.left.right:

	ld a,keyboard.cursors_ctrl
	in a,(port.keyboard)
	bit 3,a
	jr z,@still.cursor.left
	bit 4,a
	jr z,@still.cursor.right
	
	ld a,keyboard.67890
	in a,(port.keyboard)
	bit 4,a
	jr z,@still.6
	bit 3,a
	jr z,@still.7
	
	xor a 
	ret	

@still.cursor.left:
	ld a,keyboard.cursors_ctrl
	in a,(port.keyboard)
	bit 3,a
	jr z,@still.cursor.left
	jr @change.port

@still.cursor.right:
	ld a,keyboard.cursors_ctrl
	in a,(port.keyboard)
	bit 4,a
	jr z,@still.cursor.right
	jr @change.port

@still.6:
	ld a,keyboard.67890
	in a,(port.keyboard)
	bit 4,a
	jr z,@still.6
	jr @change.port

@still.7:
	ld a,keyboard.67890
	in a,(port.keyboard)
	bit 3,a
	jr z,@still.7

@change.port:
	ld a,(loader.device.port)
	xor 1
	ld (loader.device.port),a
		
	ret
	

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
show.screen:
;---------------------------------------------------------------

	call cls

	ld de,load.screen
	ld ix,load.attributes
	call print.screen
	
	ret

;---------------------------------------------------------------
loader:
;---------------------------------------------------------------

;	ld sp,&8000

	call show.screen

	ld hl,mes.nodisc
	ld de,m.vollabel
	ld bc,11
	ldir

	ld a,1
	ld (msdos+1),a

	xor a
	ld (nodisc+1),a

	call fat.read.dir

	ld a,1
	ld (loader.entries),a

	ld hl,option.dir
	ld de,loader.dir
	ld bc,load.len * 2
	ldir

	ld de,loader.dir + load.len

	ld a,dvar.records
	call bdos.get.dvar
	or a
	jr nz,@has.drive.2
	
	ld c,port.disk.2.track
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
	
@has.drive.2:

	ld a,2
	ld (loader.entries),a
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

	ld hl,(fat.data)
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
	ld d,(ix+27)		; first cluster

	ld hl,temp.spc		; space between scrn & attrib
pc.rd.more:
	call fat.read_cluster
	call fat.get_entry
	ld a,h
	cp ( temp.spc + 1084 ) // 256 + 1
	jr c,pc.rd.more

	ld hl,temp.spc+1083
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
	ld hl,loader.entries
	inc (hl)
	pop hl
	ld bc,load.len
	add hl,bc
	ex de,hl
	pop hl
pl.skip:
	ld bc,32
	add hl,bc
	ld a,(loader.entries)
	cp 27
	jp z,converted
	jp pc.to.loader

cnv.sam:
	push de
	ld ix,black.attributes
	ld a,6
	call set.attributes 

	call bdos.read.dir

	; now convert the SAM stuff to loader format

	pop de
	ld hl,fat

	ld a,80				;80 directory entries
sam.to.loader:
	push af
	ld a,(hl)
	and %00111111
	cp uifa.filetype.code
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

	in a,(port.hmpr)
	and high.memory.page.mask
	ld c,a

	ld hl,temp.spc
	call bdos.read.sector
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	call bdos.read.sector
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	call bdos.read.sector

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

	ld hl,loader.entries
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
	ld a,(loader.entries)
	cp 27
	jr z,converted
	ld a,b
	dec a
	jp nz,sam.to.loader

converted:
	call show.screen

	call print.oct

	ld a,(loader.drive)
	add a,"0"
	ld (mes.drive+6),a

	ld de,mes.label
	ld hl,m.vollabel
	ld a,(hl)
	or a
	jr nz,$+5
	ld hl,mes.nolabel
	ld bc,11
	ldir

	ld hl,video.memory.32.rows * 3 + video.memory.high
	ld de,mes.drive
	ld b,9+11
	call print.de.b

	ld a,(loader.entries)
	cp 24
	jr c,$+4
	ld a,24
	ld c,a
	ld de,loader.dir
	ld hl,video.memory.32.rows * 4 + 1 + video.memory.high
le.loop:
	push de
	push hl
	ld b,8
	call print.de.b
	inc l
	ld b,20
	call print.de.b
	pop hl
	ld de,video.memory.32.rows
	add hl,de
	pop de
	ld a,e
	add load.len
	ld e,a
	jr nc,$+3
	inc d

	dec c
	jr nz,le.loop

	call @still.esc	; to prevent immediate exit when escape used to exit "DEMO"

	ld hl,video.memory.32.rows * 4 + video.memory.high
	ld (curs.offs+1),hl
	ld a,(loader.entries)
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
	ld de,mes.pro  ; m!k!
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
	ld hl,video.memory.32.rows * 29 + video.memory.high
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
	ld h,(ix+37)

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
	ld hl,video.memory.32.rows * 29 + video.memory.high
	ld b,32
blnk.line:
	ld a," "
	call print.chr
	djnz blnk.line

normal.mess:
	ld b,32
	ld hl,video.memory.32.rows * 30 + video.memory.high
	call print.de.b

	call cursor.select

	ld a,keyboard.caps_esc
	in a,(port.status)
	bit 5,a
	jp z,loader.quit

	call scan.keyboard.return
	jp nz,select.key

	ld a,keyboard.yuiop
	in a,(port.keyboard)
	and %00010
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

    ld a,(ix+28)
    cp &81      ; drive 2
    call z,@scan.keyboard.left.right

	jp cursor.lp

; !!! copy / paste  
	
@scan.keyboard.left.right:

    ld a,keyboard.cursors_ctrl
    in a,(port.keyboard)
    bit 3,a
    jr z,@still.cursor.left
    bit 4,a
    jr z,@still.cursor.right
    
    ld a,keyboard.67890
    in a,(port.keyboard)
    bit 4,a
    jr z,@still.6
    bit 3,a
    jr z,@still.7
    
    ret 

@still.cursor.left:
    ld a,keyboard.cursors_ctrl
    in a,(port.keyboard)
    bit 3,a
    jr z,@still.cursor.left
    jr @record.down

@still.cursor.right:
    ld a,keyboard.cursors_ctrl
    in a,(port.keyboard)
    bit 4,a
    jr z,@still.cursor.right
    jr @record.up

@still.6:
    ld a,keyboard.67890
    in a,(port.keyboard)
    bit 4,a
    jr z,@still.6
    jr @record.down

@still.7:
    ld a,keyboard.67890
    in a,(port.keyboard)
    bit 3,a
    jr z,@still.7
    jr @record.up

@record.down:

    ld a,dvar.record
    call bdos.get.dvar
    ld l,a
    ld a,dvar.record+1
    call bdos.get.dvar
    ld h,a
    dec hl
    ld a,h
    or l
    ret z
    jr @change.record

@record.up:	

    ld a,dvar.record
    call bdos.get.dvar
    ld l,a
    ld a,dvar.record+1
    call bdos.get.dvar
    ld h,a
    inc hl
    ld a,dvar.records
    call bdos.get.dvar
    ld e,a
    ld a,dvar.records+1
    call bdos.get.dvar
    ld d,a
    or a
    sbc hl,de
    ret nc
    add hl,de

@change.record:

    call bdos.select.record.hl
    pop af
    jp select.key 


print.oct:
	ld hl,video.memory.32.rows * 1 + 26 + video.memory.high
	ld de,mes.oct
	ld b,5
	call print.de.b

	ld a,(loader.octaves+1)
	add "0"
	call print.chr

	ret

loader.quit:

	xor a
	out (port.lmpr),a
	rst 0

;check to see if the sum of sample lengths + patterns = file len
file.check:
	ld hl,temp.spc + 9
	ld bc,20
	ldir

	ld (bytes.per+1),a		;2=normal, 1=compressed?

	push ix
	ld bc,(temp.spc + 9 + 1080)
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
    inc a
    or a
    ld hl,&214D             ;"M!"
    sbc hl,bc
    jr z,pl.got.type
	xor a
pl.got.type:				;0=nst, 1=m.k., 2=flt4, 3=m!k!
	ld (de),a
	ld a,(bytes.per+1)
	dec a
	ld a,(de)
	jr nz,pl.not.comp
	set 6,a					;compressed 4 bit
	ld (de),a
pl.not.comp:
	inc de
	ld hl, temp.spc + 9 + ( 30 * 31 ) + 20
	and 63
	jr nz,$+5
	ld hl, temp.spc + 9 + ( 30 * 15 ) + 20
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
	ld ix,temp.spc + 9 + 20
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

	ld de,(temp.spc + 1);length mod 16384
	ld a,(temp.spc + 7) ;length in pages (16384)
	and %00000011
	rrca
	rrca
	add d
	ld d,a
	ld a,(temp.spc + 7)
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
	ld hl,video.memory.32.rows * 31 + video.memory.high
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

	push ix
	pop hl
	ld de,fat.parafile
	ld bc,8
	ldir
	ex de,hl
	ld (hl),"M"
	inc hl
	ld (hl),"O"
	inc hl
	ld (hl),"D"

	ld hl,fat.parafile
	call findfile
	ld a,(msdos+1)
	or a
	jr z,sam.load

;---------------------------------------------------------------

msdos.load:	; !!! does not work yet, needs to be moved to inst.buffer

	push hl
	pop ix
	ld e,(ix+26)
	ld d,(ix+27)

	ld a,(loader.external.ram)
	or a
	jr z,@no.megabyte.2

	ld a,high.memory.external
	out (port.hmpr),a
	ld a,page.mod.megabyte
	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	ld (@external+1),a
	
	jr @continue.2
	
@no.megabyte.2:

	ld a,page.mod
	out (port.hmpr),a
	
@continue.2:	
	
	ld hl,load.offs
pc.load.all:
	call fat.read_cluster
	bit 6,h
	res 6,h
	jr z,@page.ok
	
	ld a,(loader.external.ram)
	or a
	jr z,@no.megabyte.3

@external:
	ld a,0
	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	ld (@external+1),a
	
	jr @page.ok
		
@no.megabyte.3:

	in a,(port.hmpr)
	inc a
	out (port.hmpr),a
	
@page.ok:

	call fat.get_entry
	ld a,d
	cp &0f
	jr nz,pc.load.all
	ld a,e
	cp &f8
	jr c,pc.load.all

	jp file.loaded


;---------------------------------------------------------------

sam.load:

	ld de,&0001

sam.load.dir:
	in a,(port.hmpr)
	and high.memory.page.mask
	ld c,a
	ld hl,temp.spc
	call bdos.read.sector
	ld hl,temp.spc
	call sam.match
	ld hl,temp.spc+256
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
	cp uifa.filetype.code
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

	push hl
	push de
	
	ld hl,relocate.load.mod
	ld bc,load.mod.len
	ld a,(loader.external.ram)	
	or a
	jr z,@no.meg
	ld hl,relocate.meg.load.mod
	ld bc,meg.load.mod.len
@no.meg:	
	ld de,inst.buffer
	ldir
	pop de
	pop hl
	
	ld a,(loader.drive)	

	call load.mod
	
	jp file.loaded
	
sam.no.match:
	pop ix
	ret

relocate.load.mod:

	org inst.buffer
	
load.mod:	
	
	ld (@disk+1),a
	ld hl,uifa	; tape headers + farldir buffer = 512 bytes
	
	rst 8
	defb dos.hrsad
	di

	in a,(port.hmpr)
	ld (@file.loaded+1),a
	
	ld a,page.mod
	out (port.hmpr),a
	ld hl,uifa + 9
	ld de,load.offs
	ld bc,510-9
	ldir
	ld d,(hl)
	inc l
	ld e,(hl)

	ld hl,load.offs+510-9
	ld c,a
	
@loop:

@disk:	
	ld a,0
	ld ix,1
	
	push hl
	push bc
	
	rst 8
	defb dos.hmrsad
	di
	
	pop bc
	
	ld a,c
	out (port.hmpr),a	
	
	pop hl
	inc h
	inc h
	
	dec hl
	ld e,(hl)
	dec hl
	ld d,(hl)
	ld a,e
	or d
	jr z,@file.loaded
	bit 6,h
	jr z,@loop

	res 6,h
	inc c	
	jr @loop
	
@file.loaded:
	ld a,0
	out (port.hmpr),a
	ret	

load.mod.len: equ $ - load.mod

	org relocate.load.mod + load.mod.len

relocate.meg.load.mod:

	org inst.buffer
	
meg.load.mod:	
	
	ld (@meg.disk+1),a
	ld hl,dos.sector
	
	rst 8
	defb dos.hrsad
	di

	in a,(port.hmpr)
	ld (@meg.file.loaded+1),a

	ld a,page.mod.megabyte
	ld (@external+1),a
	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	ld a,high.memory.external
	out (port.hmpr),a
	
	ld hl,dos.sector + 9
	ld de,load.offs
	ld bc,510-9
	ldir
	ld d,(hl)
	inc l
	ld e,(hl)

	ld hl,load.offs+510-9
	
@meg.loop:

	ld a,(@meg.file.loaded+1)
	out (port.hmpr),a

@meg.disk:	
	ld a,0
	
	push hl
	
	ld hl,dos.sector
	
	rst 8
	defb dos.hrsad
	di
	
	ld a,high.memory.external
	out (port.hmpr),a
@external:	
	ld a,0
	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	
	pop de
	
	ld hl,dos.sector
	ld bc,510
	ldir
	
	push de	
	ld d,(hl)
	inc hl
	ld e,(hl)
	pop hl
	
	ld a,e
	or d
	jr z,@meg.file.loaded
	bit 6,h
	jr z,@meg.loop

	res 6,h
	ld a,(@external+1)
	inc a
	ld (@external+1),a
	
	jr @meg.loop
	
@meg.file.loaded:
	ld a,0
	out (port.hmpr),a	
	ret	

meg.load.mod.len: equ $ - meg.load.mod

	org relocate.meg.load.mod + meg.load.mod.len

file.loaded:

	pop af
	bit 6,a
	jp z,no.decompress
	
;---------------------------------------------------------------

decompress:			; !!! does not work yet

	ld c,a

	ld a,(loader.external.ram)
	or a
	jr z,@no.megabyte.6

	ld a,high.memory.external
	out (port.hmpr),a
	ld a,page.mod.megabyte
	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	dec a

	jr @continue.6				

@no.megabyte.6:

	ld a,page.mod
	out (port.hmpr),a
	
@continue.6:
	ld (@page.mod+1),a	

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
@page.mod:	
	ld e,page.mod
	
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
@loop:
	ld d,(ix+22)
	ld e,(ix+23)
	add hl,de
	jr nc,$+4
	add 4
	ld de,30
	add ix,de
	djnz @loop

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

	ld a,(loader.external.ram)
	or a
	
	ld a,d
	
	jr z,@no.megabyte.7

	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	dec a
	
	jr @continue.7
	
@no.megabyte.7:

	out (port.hmpr),a
	
@continue.7:
	
	ld a,(hl)
	rlca
	rlca
	rlca
	rlca
	exx

	ex af,af'
	
	ld a,(loader.external.ram)
	or a
	
	ld a,d
	
	jr z,@no.megabyte.8
	
	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	dec a
	
	jr @continue.8
	
@no.megabyte.8:

	out (port.hmpr),a
	
@continue.8:
	
	ex af,af'
	and %11110000
	ld (hl),a
	inc hl
	exx
	
	ld a,(loader.external.ram)
	or a
	
	ld a,d
	
	jr z,@no.megabyte.9

	out (port.xmpr.c),a
	inc a
	out (port.xmpr.d),a
	dec a
	
	jr @continue.9
	
@no.megabyte.9:

	out (port.hmpr),a
	
@continue.9:

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

;---------------------------------------------------------------

no.decompress:

	call cls
	
	in a,(port.lmpr)
	ld (@store.lmpr+1),a
	ld (@store.sp+1),sp

	ld sp,&8000	
	in a,(port.hmpr)
	and low.memory.page.mask
	or low.memory.ram.0
	out (port.lmpr),a

	ld a,(loader.external.ram)
	ld c,a
	
loader.octaves:
	ld a,3					; 3 or 5 octave
	call demo.setup			; aBcd	 
	
@store.lmpr:
	ld a,0
	out (port.lmpr),a
@store.sp:
	ld sp,0		
	
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
	call z,select.drive.1
	pop af
	call nz,select.drive.2
	jp loader

print.cursor:
wait4int:
	in a,(port.status)
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
	ld ix, colours + ( colour.purple * 8 ) 
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
	ld hl,video.memory.32.rows * 3 + video.memory.high
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

loader.entries:	defb 0

loader.dir:
	defm "filename"
	defm "20 char module title"
	defb 0			;module type 0=noise, 1=pro, 2=star +64 = 4 bit compressed
	defb 0			;length in patterns
	defm "100672"	;date stamp
	defw 0			;total size in k
	defb 0			;number of samples (len>1)

load.len:	equ $ - loader.dir

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
	ld bc,loader.font_high - 160	;-" "*5
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

;---------------------------------------------------------------	
print.screen:
;---------------------------------------------------------------	
	push de
	ld a,6
	call set.attributes

	ld hl,video.memory.high
	pop de
	ld c,32
@wel.all:
	ld b,32
	push hl

@pr.scr.blp:
	ld a,(de)
	inc de
	or a
	jr z,@end.of.line
	call print.chr
	djnz @pr.scr.blp
@end.of.line:
	pop hl
	ld a,l
	add video.memory.32.rows
	ld l,a
	jr nc,$+3
	inc h
	dec c
	jr nz,@wel.all
	ld a,255
	ret
	
;---------------------------------------------------------------	
cls:

; clear mode 2 screen 
;---------------------------------------------------------------	
	ld hl,video.memory.high
	ld de,video.memory.high + 1
	ld bc,6143
	ld (hl),l
	ldir

	ld hl,video.memory.high.attributes
	ld de,video.memory.high.attributes + 1
	ld bc,6143
	ld (hl),l
	ldir

	ld hl,loader.palette + 15
	ld bc,256 * 16 + port.clut
	otdr
	
	ld hl,loader.palette
	ld de,palette.table
	ld bc,&10
	ldir
	
	ld hl,loader.palette
	ld de,palette.table + &14
	ld bc,&10
	ldir

	ret

;---------------------------------------------------------------	
print.de.b:

; print text at DE, with padded out to B characters 
;---------------------------------------------------------------	
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


;---------------------------------------------------------------	
set.attributes:

; apply attributes to mode 2 screen 
;---------------------------------------------------------------	
	ld (line.size+1),a

	ld hl,video.memory.high.attributes
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
	
@fill: equ for 31
	
	ld (hl),a
	inc l
	
next @fill

	ld (hl),a
	inc hl	

	djnz col.blp2

	dec c
	jr nz,col.clp1
	inc ix

	ld a,h
	cp ( video.memory.high.attributes + 6144 ) // 256
	jr nz,col.loop
	ret


;---------------------------------------------------------------

colours:
colour.black:	equ 0
	defb 0,0,0,0,0,0,0,0
	
colour.blue:	equ 1
	defb 1,2,3,2,1,0,0,0
	
colour.orange:	equ 2	
	defb 4,5,6,5,4,0,0,0
	
colour.green:	equ 3	
	defb 7,9+56,3,9+56,7,0,0,0
	
colour.purple:	equ 4	
	defb 10+56,11+56,12+56,11+56,10+56,0,0,0
	
colour.yellow:	equ 5	
	defb 13+56,14+56,6,14+56,13+56,0,0,0

;---------------------------------------------------------------


device.screen:
	defm "SAM MOD player             v"
	defb version.major, ".", version.minor.1, version.minor.2	
	defm "(C) 2019 Stefan Drissen"
	defb 0,0
row.device:	equ 5	
	defm "SOUND DEVICE"
	defb 0,0
	defm " Soundchip"
	defb 0
	defm " SAMdac"
	defb 0
	defm " DAC"
	defb 0
	defm " Blue Alpha Sampler"
	defb 0
	defm " Quazar Soundcard"
	defb 0
	defm " Screen"
	defb 0,0
row.speed:	equ 14
	defm "AMIGA SPEED"
	defb 0,0
	defm " PAL"
	defb 0
	defm " NTSC"
	defb 0,0
	defm "Memory: "
device.screen.memory:	
	defm "512 KB"
	defb 0,0	
	defb 0,0,0,0,0,0,0,0,0,0,0,0
	defm "Use CURSORS + RETURN or JOYSTICK"
	defb 0

device.attributes:	
	defb 3,colour.orange
	defb 2,colour.purple
	defb 6,colour.blue
	defb 2,colour.purple
	defb 3,colour.blue
	defb 2,colour.green
	defb 13,colour.green	
	defb 1,colour.yellow

device.details:
	defb 3, stereo
	defb 7, stereo
	defb 6, mono 
	defb 6, mono 
	defb 8, surround 
	defb 7, visual

	visual:		equ 0
	mono:		equ 1
	stereo:		equ 2
	surround:	equ 3		

device.texts:	defw text.visual,text.mono,text.stereo,text.surround

text.visual:
	defm "visual"
	defb 0
text.mono:
	defm "mono"
	defb 0
text.stereo:
	defm "stereo"
	defb 0
text.surround:
	defm "surround"
	defb 0


load.screen:
	defm "SAM MOD player             v"
	defb version.major, ".", version.minor.1, version.minor.2	
	defm "(C) 2019 Stefan Drissen"
	defb 0,0,0,0,0,0,0
	defb 0,0,0,0,0,0,0,0
	defb 0,0,0,0,0,0,0,0
	defb 0,0,0,0,0,0,0
	defm "Use CURSORS + RETURN or JOYSTICK"


load.attributes:
	defb 3,colour.orange
	defb 1,colour.green
	defb 25,colour.blue
	defb 2,colour.green
	defb 1,colour.yellow

black.attributes:
	defb 2,colour.orange
	defb 27,colour.black
	defb 2,colour.green
	defb 1,colour.yellow

;---------------------------------------------------------------

m.vollabel:	defm "01234567890"

;---------------------------------------------------------------


errnodisc:
	ld sp,(save.sam.sp+1)
	ld a,1
	ld (nodisc+1),a
	ret


findfile:
	ld (save.sam.sp+1),sp

	ld de,fat.matchfile
	ld bc,11
	ldir
	call fat.readroot
	call fat.load_path
	call c,fat.reset_path
	ld bc,(fat.dir_entries)
	ld hl,(fat.data)
fmclp:
	push hl
	push bc
	ld b,11
	ld de,fat.matchfile
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



select.drive.1:
	ld a,1
	ld hl,fat.path_a
@select.drive:	
	ld (loader.drive),a
	ld (fat.path),hl
	ret

select.drive.2:
	ld a,2
	ld hl,fat.path_b
	jr @select.drive

;---------------------------------------------------------------
relocate.low:	

; move code after call to inst.buffer and execute it

	ld (@store.hl+1),hl
	pop hl
	
	push de
	push bc
	
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	
	ld de,inst.buffer
	ldir
	
	pop bc
	pop de
	
	push hl	; return address
	
@store.hl:
	ld hl,0
		
	jp inst.buffer

;---------------------------------------------------------------

include "loader.bdos.i"
include "loader.fat.i"

; in screen area

fat:			equ video.memory.high + 2 * video.memory.32.rows

temp.spc:		equ video.memory.high + video.memory.32.rows * 32 

;===============================================================

length:			equ $-16384

