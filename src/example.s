; SAM MOD player - EXAMPLE routine for BURST+SequENCER

; (C) 1996-2021 Stefan Drissen

    include "memory.i"
    include "ports.i"

; Contents:
;   "BURST" routine (make burstplayer)
;   "BURSTPLAYER"
;   "SEQ" routine (sequencer)

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

make.burst: equ 0x8000

;set the following address with the desired sound device

burstplayer.device:     equ 0x8003
    clut:       equ 0
    saa:        equ 1
    samdac1:    equ 2
    samdac2:    equ 3
    dac1:       equ 4
    dac2:       equ 5
    balpha:     equ 6
    quazar:     equ 7

;set the following address with the desired Amiga speed

burstplayer.speed:      equ 0x8004
    pal:        equ 0
    ntsc:       equ 1

;set the following address with the page at which to build

burstplayer.page:       equ 32774

page.example:           equ 1

    org 0x8000

    di
    in a,(port.lmpr)
    ld (st.lmpr+1),a
    in a,(port.hmpr)
    and high.memory.page.mask
    or low.memory.ram.0
    out (port.lmpr),a
    ld (st.stpr+1),sp
    ld sp,0x4000
    jp @low

    org $-0x8000
@low:
    ld a,(seq.setup+1) ;don't build if already done
    or a
    jr nz,already.made

    ld a,page.create.burstplayer
    out (port.hmpr),a

    ld a,samdac1
    ld (burstplayer.device),a
    ld a,pal
    ld (burstplayer.speed),a
    ld a,page.burstplayer
    ld (burstplayer.page),a

    call make.burst
already.made:

;put the address (and page) of the foreground routine into the
;sequencer.  The foreground routine is called by the BURSTPLAYER
;after the BURSTPLAYER has set up interrupts and initialised the
;sound device.

    ld a,page.sequencer
    out (port.hmpr),a
    ld hl,demo.rtn
    ld (sq.pointer.addr.demo),hl
    ld a,demo.rtn.page
    ld (sq.pointer.page.demo),a
    ld a,page.mod
    ld (sq.pointer.page.mod),a
    ld a,3              ; 5 for 5 octave mode
    ld (sq.octaves),a

;the INITialise.SEQuencer routine only needs to be called once
;after the BURSTPLAYER has been built so that it can fill in the
;variable addresses of the various BURSTPLAYER variables.  The
;variables are at different addresses depending on the sound
;driver being used.

seq.setup:
    ld a,0
    or a
    call z,sequencer.init
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
    out (port.hmpr),a
    ld a,(0x8000)
    cp 0xff
    jr z,already.inst

    ld a,0xff
    ld (0x8000),a

    ld a,page.sequencer
    out (port.hmpr),a

    call sequencer.install.mod  ;add gaps between samples
already.inst:
    ld a,page.burstplayer
    out (port.hmpr),a

    call burstplayer.start
test:
    in a,(port.lmpr)
    and low.memory.page.mask
    out (port.hmpr),a
    jp high

    org  $+0x8000
high:
st.lmpr:
    ld a,0
    out (port.lmpr),a
st.stpr:
    ld sp,0
    ei
    ret

;===============================================================
; demo is the program that runs in "foreground" mode
;   the sequencer is called by the burst routine every frame
; the foreground program must be located in the upper memory
; blocks (CD), and can be any page (not used that is!)
; PLEASE NOTE:
;   your code is NOT allowed to use the alternate registers
;   since these are used by the BURSTPLAYER.
;   If you page the BURSTPLAYER out of lower memory
;   then the music will also stop, make sure you have
;   an interrupt routine at address 56 of the new page

demo.rtn.page: equ page.example

demo.rtn:
    ld hl,(enable.burst)
    ld (mk.enable+1),hl
mk.enable:
    call 0              ; enable the burstplayer

    call interrupt.on

    ld hl,my.palette    ; the palette is set by the burstplayer at the start of a frame
    ld de,frame.palette
    ld bc,0x10
    ldir

@demo.loop:
    ld bc,port.clut
    ld a,r
    out (c),a

    ld bc,0
    ld a,keyboard.caps_tab_esc
    in a,(port.status)  ; escape key
    and %100000
    jr z,@exit

    ld a,(mstatus)          ; 1=music stopped
    dec a
    jr z,@exit

    jp @demo.loop

@exit:
    ld hl,(exit.burst)
    jp (hl)


my.palette:
    defb 0,16,32,48,64,80, 96,112
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
    in a,(port.hmpr)
    ld (int.rtn.pag),a
    ret

interrupt.off:
    ld a,0xff
    ld (int.rtn.pag),a
    ret

example.int:
    ld a,r
    and %111
    out (port.border),a
    ret

length: equ $-0x8000
