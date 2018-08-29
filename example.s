
;SAM MOD player version 2.06
;EXAMPLE routine for BURST+SEQUENCER
;(C) 1996 Stefan Drissen
;last update: 4 May 1996, 23:00

;Contents:
;              "BURST" routine (make burstplayer)
;              "BURSTPLAYER"
;              "SEQ" routine (sequencer)

;the page variables are:
;              mb.page             location of make burst
;              bp.page             where to build burstplayer
;              sq.page             location of SEQuencer
;              mod.page            location of mod file


;===============================================================
;VARIABLES FOR MAKE BURST PLAYER ROUTINE: "BURST"

;page in which the "BURST" code is located, this routine will
;build up a BURSTPLAYER routine for the selected sound device.
;Since the various sound devices have different sound drivers
;and each sound driver has a different length in bytes and time
;the lengths of the BURSTPLAYER vary.  The SAMdac routine costs
;the most memory, the Soundchip routine costs the most time.
;The make burstplayer routine only needs to be called once, the
;routine may then be wiped from memory.

mb.page:       EQU  5              ;set this yourself
make.burst:    EQU  32768

;set the following address with the desired sound device

device:        EQU  32771
clut:          EQU  0
saa:           EQU  1
samdac1:       EQU  2
samdac2:       EQU  3
dac1:          EQU  4
dac2:          EQU  5
balpha:        EQU  6
quasar:        EQU  7

;set the following address with the desired Amiga speed

amiga:         EQU  32772
pal:           EQU  0
ntsc:          EQU  1

;set the following address with the page at which to build

burst:         EQU  32773

;===============================================================
;VARIABLES FOR BURSTPLAYER ROUTINE: as made by "BURST"

bp.page:       EQU  2              ;set this yourself!
burst.player:  EQU  32768

current.row:   EQU  256    ;16 bytes of current row being played
palette.tab:   EQU  256+16 ;16 byte palette set at start frame
framescreen:   EQU  256+32 ;screen page (+mode) set at start of
                           ;frame, 0 = no change

int.routine:   EQU  256+33 ;address (>32k) or interrupt routine
int.rtn.pag:   EQU  256+35 ;255=no interrupt else page

c1.on:         EQU  256+36 ;channel on/off
c2.on:         EQU  256+37 ;   "      "
c3.on:         EQU  256+38 ;   "      "
c4.on:         EQU  256+39 ;   "      "
vol.update:    EQU  256+40 ;when set do xtra burst volume update
                           ;to ensure burstplayer acts on
                           ;changed value of c?.on.

countint:      EQU  256+41 ;user frame counter
counter.fract: EQU  256+42 ;1/256 frame counter for sequencer
counter:       EQU  256+43 ;frame counter for sequencer
speed:         EQU  256+44 ;song speed in frames
tempo:         EQU  256+45 ;bpm speed (relative to 125)
song.pos:      EQU  256+47 ;position in songtable (0-127)
pattern.num:   EQU  256+48 ;pattern being played (0-255)
pattern.pos:   EQU  256+49 ;row being played (0-63)
enable.burst:  EQU  256+50 ;start burstplayer
exit.burst:    EQU  256+52 ;stop burstplayer and exit
disable.pos:   EQU  256+54 ;disable "B" command (jump) + looping
mstatus:       EQU  256+55 ;0=playing, 1=stopped

buffer:        EQU  256+128 ;used by far.ldir, can be used for
                            ;other purposes if far.ldir not used
                            ;max 128 bytes, volume tables follow

far.call:      EQU  10794 ;CALL to upper page C, address HL
;              IN   A,(251)
;              LD   (fc.st.page),A
;              LD   A,C
;              OUT  (251),A
;              LD   (call.hl+1),HL
;call.hl:      CALL 0
;fc.st.page:   LD   A,0
;              OUT  (251),A
;              RET


far.ldir.1:    EQU  10813 ;copy BHL to buffer, C bytes (max128)
;far block move 1 - copies C bytes (max 128) to buffer
;              IN   A,(251)
;              LD   (fm1.st.page+1),A
;              LD   A,B
;              OUT  (251),A
;              LD   DE,buffer
;              LD   B,0
;              LDIR
;fm1.st.page:  LD   A,0
;              OUT  (251),A
;              RET




far.ldir.2:    EQU  10833 ;copy buffer to BDE, C bytes (max128)
;far block move 2 - copies C bytes (max 128) from buffer
;              IN   A,(251)
;              LD   (fm2.st.page+1),A
;              LD   A,B
;              OUT  (251),A
;              LD   HL,buffer
;              LD   B,0
;              LDIR
;fm2.st.page:  LD   A,0
;              OUT  (251),A
;              RET

;===============================================================
;VARIABLES FOR SEQUENCER: "SEQ"

sq.page:       EQU  4     ;set this yourself!
mod.page:      EQU  6     ;set this yourself!

init.seq:      EQU  32768 ;initialse sequencer routine
install.mod:   EQU  32771 ;install mod by adding "runways"

sq.demo:       EQU  32774 ;address of foreground program (>32k)
sq.demo.p:     EQU  32776 ;page of foreground program
sq.modpage:    EQU  32777 ;page mod loaded in at (at 32k)
sq.octaves:    EQU  32778 ;3 or 5 octave mode

;===============================================================

ex.page:       EQU  1

               ORG  32768
               DUMP ex.page,0

               DI
               IN   A,(250)
               LD   (st.lmpr+1),A
               IN   A,(251)
               AND  31
               OR   32
               OUT  (250),A
               LD   (st.stpr+1),SP
               LD   SP,16384
               JP   low

               ORG  $-32768
low:
               LD   A,(seq.setup+1) ;don't build if already done
               OR   A
               JR   NZ,already.made

               LD   A,mb.page
               OUT  (251),A

               LD   A,samdac1
               LD   (device),A
               LD   A,pal
               LD   (amiga),A
               LD   A,bp.page
               LD   (burst),A

               CALL make.burst
already.made:

;put the address (and page) of the foreground routine into the
;sequencer.  The foreground routine is called by the BURSTPLAYER
;after the BURSTPLAYER has set up interrupts and initialised the
;sound device.

               LD   A,sq.page
               OUT  (251),A
               LD   HL,demo.rtn
               LD   (sq.demo),HL
               LD   A,demo.rtn.page
               LD   (sq.demo.p),A
               LD   A,mod.page
               LD   (sq.modpage),A
               LD   A,3            ;5 for 5 octave mode
               LD   (sq.octaves),A

;the INITialise.SEQuencer routine only needs to be called once
;after the BURSTPLAYER has been built so that it can fill in the
;variable addresses of the various BURSTPLAYER variables.  The
;variables are at different addresses depending on the sound
;driver being used.

seq.setup:     LD   A,0
               OR   A
               CALL Z,init.seq
               LD   A,1
               LD   (seq.setup+1),A

;each sample needs a "runway" after it since the buffer being
;used is 208 bytes.  208 bytes a frame times 50 frames a second
;equals 10400HZ.  In 3 octave mode the runway is 768 bytes per
;sample, in 5 octave mode (which allows one octave higher) the
;runway is twice the size at 1536 bytes.  Samples with a loop
;smaller than the runway require three times the runway to be
;tagged on.  So now you can calculate the amount of memory
;required by a mod by adding runway times X to the original mod
;length.

               LD   A,mod.page
               OUT  (251),A
               LD   A,(32768)
               CP   255
               JR   Z,already.inst

               LD   A,255
               LD   (32768),A

               LD   A,sq.page
               OUT  (251),A

               CALL install.mod    ;add gaps between samples
already.inst:
               LD   A,bp.page
               OUT  (251),A

               CALL burst.player
test:
               IN   A,(250)
               AND  31
               OUT  (251),A
               JP   high

               ORG  $+32768
high:
st.lmpr:       LD   A,0
               OUT  (250),A
st.stpr:       LD   SP,0
               EI
               RET

;===============================================================
;demo is the program that runs in "foreground" mode
;     the sequencer is called by the burst routine every frame
;the foreground program must be located in the upper memory
;blocks (CD), and can be any page (not used that is!)
;PLEASE NOTE: your code is NOT allowed to use the alternate
;             registers since these are used by the BURSTPLAYER.
;             If you page the BURSTPLAYER out of lower memory
;             then the music will also stop, make sure you have
;             an interrupt routine at address 56 of the new page

demo.rtn.page: EQU  ex.page
demo.rtn:
               LD   HL,(enable.burst)
               LD   (mk.enable+1),HL
mk.enable:     CALL 0              ;enable the burstplayer

               CALL interrupt.on

               LD   HL,my.palette  ;the palette is set by the
               LD   DE,palette.tab ;burstplayer at the start of
               LD   BC,16          ;a frame
               LDIR

demo.loop:
               LD   BC,248
               LD   A,R
               OUT  (C),A

               LD   BC,0
               LD   A,247
               IN   A,(249)        ;escape key
               AND  32
               JR   Z,exit

               LD   A,(mstatus)    ;1=music stopped
               DEC  A
               JR   Z,exit

               JP   demo.loop

exit:
               LD   HL,(exit.burst)
               JP   (HL)


my.palette:    DEFB 0,16,32,48,64,80,96,112
               DEFB 8,17,34,51,68,85,102,119

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
               LD   HL,example.int
               LD   (int.routine),HL
               IN   A,(251)
               LD   (int.rtn.pag),A
               RET

interrupt.off:
               LD   A,255
               LD   (int.rtn.pag),A
               RET

example.int:
               LD   A,R
               AND  7
               OUT  (254),A
               RET

length:        EQU  $-32768
