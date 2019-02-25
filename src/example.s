; SAM MOD player - EXAMPLE routine for BURST+SequENCER

; (C) 1996-2018 Stefan Drissen

	include "memory.i"
	include "ports.i"

; Contents:
;	"BURST" routine (make burstplayer)
;	"BURSTPLAYER"
;	"SEQ" routine (sequencer)

;================================================================
; VARIABLES FOR MAKE BURST PLAYER ROUTINE: "BURST"

; page in which the "BURST" code is located, this routine will
; build up a BURSTPLAYER routine for the selected sound device.
; Since the various sound devices have different sound drivers
; and each sound driver has a different length in bytes and time
; the lengths of the BURSTPLAYER vary.  The SAMdac routine costs
; the most memory, the Soundchip routine costs the most time.
; The make burstplayer routine only needs to be called once, the
; routine may then be wiped from memory.

make.burst:	equ 32768

;set the following address with the desired sound device

device:		equ 32771
	clut:		equ 0
	saa:		equ 1
	samdac1:	equ 2
	samdac2:	equ 3
	dac1:		equ 4
	dac2:		equ 5
	balpha:		equ 6
	quazar:		equ 7

;set the following address with the desired Amiga speed

amiga:		equ 32772
	pal:		equ 0
	ntsc:		equ 1

;set the following address with the page at which to build

burst:		equ 32773

page.example:	equ  1

	dump page.example,0
	org 32768

	di
	in a,(low.memory.page.register)
	ld (st.lmpr+1),a
	in a,(high.memory.page.register)
	and high.memory.page.mask
	or low.memory.ram.0
	out (low.memory.page.register),a
	ld (st.stpr+1),sp
	ld sp,16384
	jp @low

	org $-32768
@low:
	ld a,(seq.setup+1) ;don't build if already done
	or a
	jr nz,already.made

	ld a,page.create.burstplayer
	out (high.memory.page.register),a

	ld a,samdac1
	ld (device),a
	ld a,pal
	ld (amiga),a
	ld a,page.burstplayer
	ld (burst),a

	call make.burst
already.made:

;put the address (and page) of the foreground routine into the
;sequencer.  The foreground routine is called by the BURSTPLAYER
;after the BURSTPLAYER has set up interrupts and initialised the
;sound device.

	ld a,page.sequencer
	out (high.memory.page.register),a
	ld hl,demo.rtn
	ld (sq.pointer.addr.demo),hl
	ld a,demo.rtn.page
	ld (sq.pointer.page.demo),a
	ld a,page.mod
	ld (sq.pointer.page.mod),a
	ld a,3				; 5 for 5 octave mode
	ld (sq.octaves),a

;the INITialise.SEQuencer routine only needs to be called once
;after the BURSTPLAYER has been built so that it can fill in the
;variable addresses of the various BURSTPLAYER variables.  The
;variables are at different addresses depending on the sound
;driver being used.

seq.setup:	 
	ld a,0
	or a
	call z,init.seq
	ld a,1
	ld (seq.setup+1),a

;each sample needs a "runway" after it since the buffer being
;used is 208 bytes.  208 bytes a frame times 50 frames a second
;equals 10400 Hz.  In 3 octave mode the runway is 768 bytes per
;sample, in 5 octave mode (which allows one octave higher) the
;runway is twice the size at 1536 bytes.  Samples with a loop
;smaller than the runway require three times the runway to be
;tagged on.  So now you can calculate the amount of memory
;required by a mod by adding runway times X to the original mod
;length.

	ld a,page.mod
	out (high.memory.page.register),a
	ld a,(32768)
	cp 255
	jr z,already.inst

	ld a,255
	ld ( 32768 ),a

	ld a,page.sequencer
	out (high.memory.page.register),a

	call install.mod	;add gaps between samples
already.inst:
	ld a,page.burstplayer
	out (high.memory.page.register),a

	call burst.player
test:
	in a,(low.memory.page.register)
	and low.memory.page.mask
	out (high.memory.page.register),a
	jp high

	org  $+32768
high:
st.lmpr:
	ld a,0
	out (low.memory.page.register),a
st.stpr:
	ld sp,0
	ei
	ret

;===============================================================
; demo is the program that runs in "foreground" mode
;	the sequencer is called by the burst routine every frame
; the foreground program must be located in the upper memory
; blocks (CD), and can be any page (not used that is!)
; PLEASE NOTE: 
;	your code is NOT allowed to use the alternate registers 
;	since these are used by the BURSTPLAYER.
;   If you page the BURSTPLAYER out of lower memory
;   then the music will also stop, make sure you have
;   an interrupt routine at address 56 of the new page

demo.rtn.page: equ page.example
	
demo.rtn:
	ld hl,(enable.burst)
	ld (mk.enable+1),hl
mk.enable:
	call 0				; enable the burstplayer

	call interrupt.on

	ld hl,my.palette	; the palette is set by the burstplayer at the start of a frame
	ld de,frame.palette
	ld bc,16
	ldir

@demo.loop:
	ld bc,color.look.up.table
	ld a,r
	out (c),a

	ld bc,0
	ld a,247
	in a,(status.register)	; escape key
	and 32
	jr z,@exit

	ld a,(mstatus)			; 1=music stopped
	dec a
	jr z,@exit

	jp @demo.loop

@exit:
	ld hl,(exit.burst)
	jp (hl)


my.palette:
	defb 0,16,32,48,64,80,96,112
	defb 8,17,34,51,68,85,102,119

;if the value of int.rtn.pag is 255 then no user interrupts are
;called.  A value of not 255 is the page of the interrupt
;routine, int.routine contains the address.  The interrupt
;routine must be located in high memory (CD), it does not have
;to be in the same page as the foreground routine.
;Do note that the user interrupt routine is called before the
;sequencer.  If the user interrupt takes too much time then the
;sequencer will be interrupted resulting in a crash.
;You do not need to store any of the registers, but do remember
;no alternate registers allowed! (EXX, EX AF,AF')

interrupt.on:
	ld hl,example.int
	ld (int.routine),hl
	in a,(high.memory.page.register)
	ld (int.rtn.pag),a
	ret

interrupt.off:
	ld a,255
	ld (int.rtn.pag),a
	ret

example.int:
	ld a,r
	and 7
	out (254),a
	ret

length:	equ $-32768
