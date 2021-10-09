; memory layout

;   0x00 0x40   for basic - can use approx 7k (clear 0x61ff -> 0x1e00 bytes)
;   0x1a        burst player
;   0x1b        burst player
;   0x1c        sequencer
;   0x1d 0x60   dos
;   0x1e 0xc0   screen -> 1 page is enough for mode 2
;   0x1f 0xc0   screen -> replace with loader + demo


;               loader
;               loading screen      -> open new screen
;               loader / demo       -> 0x1f
;               make burstplayer    -> 0x19 -> 0x1a 0x1b
;               sequencer           -> 0x1c

inst.buffer:    equ 0x4f00


; burstplayer

; todo: make pages dynamic based on available?

page.burstplayer:           equ 0x1a
page.screen:                equ 0x1e
page.loader:                equ 0x1f
page.demo:                  equ 0x1f
    demo.setup:                 equ 0x6000

page.sequencer:             equ 0x1c

    sequencer.init:             equ 0x8000  ; initialise sequencer routine
    sequencer.install.mod:      equ 0x8003  ; install mod by adding "runways"
    sq.pointer.addr.demo:       equ 0x8006  ; address of foreground program (>32k)
    sq.pointer.page.demo:       equ 0x8008  ; page of foreground program
    sq.pointer.page.mod:        equ 0x8009  ; page mod loaded in at (at 32k)
    sq.octaves:                 equ 0x800a  ; 3 or 5 octave mode
    sq.external.ram:            equ 0x800b

page.create.burstplayer:    equ 0x19    ; once created -> mode 2 screen

;page.mod:                  equ 6 - when example
page.mod:                   equ 0x01    ; 0 when megabyte
page.mod.megabyte:          equ 0x00
;---------------------------------------------------------------
; page.burstplayer

burstplayer.create:         equ 0x8000
burstplayer.device:         equ 0x8003
burstplayer.port:           equ 0x8004
burstplayer.amiga:          equ 0x8005
burstplayer.external.ram:   equ 0x8006
burstplayer.page:           equ 0x8007

bp.id:                      equ 53      ; "BUR"
bp.device:                  equ 105
bp.pointers:                equ 106     ; offsets to variables located in burst page
bp.pointer.addr.sequencer:  equ 106
bp.pointer.page.sequencer:  equ 108
bp.pointer.addr.demo:       equ 110
bp.pointer.page.demo:       equ 112
bp.pointer.addr.enable:     equ 114
bp.pointer.addr.exit:       equ 116

bp.pointers.sample:         equ 118
bp.pointers.length:         equ 12

; store for current pattern row + other common variables
; there are 256 bytes reserved for this

var:                    equ 256

mod.current.row:        equ var + 0     ; 16 bytes of current row being played
frame.palette:          equ var + 16    ; 16 byte palette set at start frame
frame.screen:           equ var + 32    ; screen page (+mode) set at start of frame, 0 = no change
int.routine:            equ var + 33    ; address (>32k) or interrupt routine
int.rtn.pag:            equ var + 35    ; -1 = no interrupt else page

c1.on:                  equ var + 36    ; channel 1 on/off
c2.on:                  equ var + 37    ; channel 2 on/off
c3.on:                  equ var + 38    ; channel 3 on/off
c4.on:                  equ var + 39    ; channel 4 on/off
vol.update:             equ var + 40    ; when set do extra burst volume update to ensure
                                        ; burstplayer acts on changed value of c?.on.

countint:               equ var + 41    ; user frame counter
counter.fract:          equ var + 42    ; 1/256 frame counter for sequencer
counter:                equ var + 43    ; frame counter for sequencer
speed:                  equ var + 44    ; song speed in frames
tempo:                  equ var + 45    ; bpm speed (relative to 125)
song.pos:               equ var + 47    ; position in songtable (0-127)
pattern.num:            equ var + 48    ; pattern being played (0-255)
pattern.pos:            equ var + 49    ; row being played (0-63)
enable.burst:           equ var + 50    ; start burstplayer
exit.burst:             equ var + 52    ; stop burstplayer and exit
disable.pos:            equ var + 54    ; disable "B" command (jump) + looping
mstatus:                equ var + 55    ; 0=playing, 1=stopped

@var.size:              equ 128

ldir.far.buffer:        equ var + @var.size                     ; used by ldir.from.far / ldir.to.far, can be used for other purposes if not used
far.ldir.buffer:        equ ldir.far.buffer                     ; bp2
@ldir.far.size:         equ 128

volume.table:           equ ldir.far.buffer + @ldir.far.size    ; 32 volume tables * 256 bytes = 8k
@volume.table.size:     equ 32 * 256

pitch.table:            equ volume.table + @volume.table.size   ; 1024 pitches * 2 bytes = 2k
@pitch.table.size:      equ 2 * 1024

get.pattern:            equ pitch.table + @pitch.table.size     ; copies AHL -> mod.current.row, 16 bytes
@get.pattern.size:      equ 49

far.call:               equ get.pattern + @get.pattern.size     ; call CHL, C=set with return page!
@far.call.size:         equ 19

ldir.from.far:          equ far.call + @far.call.size           ; copy BHL to buffer, C bytes (max 128)
@ldir.from.far.size:    equ 27

ldir.to.far:            equ ldir.from.far + @ldir.from.far.size ; copy buffer to BDE, C bytes (max 128)
@ldir.to.far.size:      equ 27

; these do not need to be 256 aligned since all outs should be outi
; play tables ( 208 * 2 * 2) if QSS -> 208 * 4 * 2

playtab1:               equ ldir.to.far + @ldir.to.far.size         ; 2 * 208

; must be aligned at 0x100 boundary for relocate
bp.audio_buffer.1:      equ 0x7800  ; !!! may need to optimize
bp.audio_buffer.2:      equ 0x7c00  ; !!! may need to optimize
bp.audio_buffer.bytes:  equ 208     ; 312 lines per frame / 1.5 lines per sample = 208 bytes per frame

burstplayer.start:  equ 0x8000

video.memory.high:              equ 0x8000
video.memory.high.attributes:   equ video.memory.high + 0x2000

video.memory.24.rows:   equ ( 8 * 0x20 )
video.memory.32.rows:   equ ( 6 * 0x20 )

loader.font_high:   equ 49162

