; SAM MOD player - memory layout

inst.buffer:    equ 0x4f00
                                        ; 256K
page.mod:                   equ 0x01    ; 0x01
page.mod.megabyte:          equ 0x00    ;       in external memory
page.create.burstplayer:    equ 0x19    ; 0x09  can be reclaimed
page.burstplayer:           equ 0x1a    ; 0x0a
page.tracker:               equ 0x1c    ; 0x0c
page.dos:                   equ 0x1d    ; 0x0d
page.screen:                equ 0x1e    ; 0x0e
page.loader:                equ 0x1f    ; 0x0f
page.demo:                  equ 0x1f    ; 0x0f
    demo.setup:                 equ 0x6000

;---------------------------------------------------------------
; page.burstplayer

    burstplayer.create:         equ 0x8000
    burstplayer.device:         equ 0x8003
    burstplayer.port:           equ 0x8004
    burstplayer.amiga:          equ 0x8005
    burstplayer.ram:            equ 0x8006
    burstplayer.page:           equ 0x8007

    bp.id:                      equ 53      ; "BUR"
    bp.device:                  equ 105
    bp.pointers:                equ 106     ; offsets to variables located in burst page
    bp.ptr.addr.tracker:        equ 106
    bp.ptr.page.tracker:        equ 108
    bp.ptr.addr.demo:           equ 110
    bp.ptr.page.demo:           equ 112
    bp.ptr.addr.enable:         equ 114
    bp.ptr.addr.exit:           equ 116

    bp.pointers.sample:         equ 118
    bp.pointers.length:         equ 12

;---------------------------------------------------------------
; page.tracker:

    tracker.init:               equ 0x8000  ; initialise tracker routine
    tracker.install.mod:        equ 0x8003  ; install mod by adding "runways"
    tracker.ptr.addr.demo:      equ 0x8006  ; address of foreground program (>32k)
    tracker.ptr.page.demo:      equ 0x8008  ; page of foreground program
    tracker.ptr.page.mod:       equ 0x8009  ; page mod loaded in at (at 32k)
    tracker.octaves:            equ 0x800a  ; 3 or 5 octave mode
    tracker.samples:            equ 0x800b  ; [15|31]
    tracker.ram:                equ 0x800c  ; XXXRR

;---------------------------------------------------------------
; store for current pattern row + other common variables
; there are 256 bytes reserved for this

var:                    equ 256

mod.current.row:        equ var + 0     ; 16 bytes of current row being played
frame.palette:          equ var + 16    ; 16 byte palette set at start frame
frame.screen:           equ var + 32    ; screen page (+mode) set at start of frame, 0 = no change
int.routine:            equ var + 33    ; address (>32K) or interrupt routine
int.rtn.pag:            equ var + 35    ; -1 = no interrupt else page

c1.on:                  equ var + 36    ; channel 1 on/off
c2.on:                  equ var + 37    ; channel 2 on/off
c3.on:                  equ var + 38    ; channel 3 on/off
c4.on:                  equ var + 39    ; channel 4 on/off
vol.update:             equ var + 40    ; when set do extra burst volume update to ensure
                                        ; burstplayer acts on changed value of c?.on.

countint:               equ var + 41    ; user frame counter
counter.fract:          equ var + 42    ; 1/256 frame counter for tracker
counter:                equ var + 43    ; frame counter for tracker
speed:                  equ var + 44    ; song speed in frames
tempo:                  equ var + 45    ; bpm speed (relative to 125)
song.pos:               equ var + 47    ; position in songtable (0-127)
pattern.num:            equ var + 48    ; pattern being played (0-255)
pattern.pos:            equ var + 49    ; row being played (0-63)
enable.burst:           equ var + 50    ; start burstplayer
exit.burst:             equ var + 52    ; stop burstplayer and exit
disable.pos:            equ var + 54    ; disable "B" command (jump) + looping
mstatus:                equ var + 55    ; 0=playing, 1=stopped

@var.size:              equ 0x80

ldir.far.buffer:        equ var + @var.size                     ; used by ldir.from.far / ldir.to.far, can be used for other purposes if not used
far.ldir.buffer:        equ ldir.far.buffer                     ; bp2
@ldir.far.size:         equ 0x80

volume.table:           equ ldir.far.buffer + @ldir.far.size    ; 32 volume tables * 256 bytes = 8K
assert volume.table == 0x200
@volume.table.size:     equ 0x20 * 0x100

pitch.table:            equ volume.table + @volume.table.size   ; 1024 pitches * 2 bytes = 2K
@pitch.table.size:      equ 2 * 0x400

get.pattern:            equ pitch.table + @pitch.table.size     ; copies AHL -> mod.current.row, 16 bytes
@get.pattern.size:      equ 49

far.call:               equ get.pattern + @get.pattern.size     ; call CHL, C=set with return page!
@far.call.size:         equ 19

ldir.from.far:          equ far.call + @far.call.size           ; copy BHL to buffer, C bytes (max 128)
@ldir.from.far.size:    equ 27

ldir.to.far:            equ ldir.from.far + @ldir.from.far.size ; copy buffer to BDE, C bytes (max 128)
@ldir.to.far.size:      equ 27

; these do not need to be 256 aligned since all outs should be outi
; bp.audio_buffer ( 208 * 2 * 2) if QSS -> 208 * 4 * 2

bp.audio_buffer.bytes:  equ 208     ; 312 lines per frame / 1.5 lines per sample = 208 bytes per frame
bp.audio_buffer.1:      equ ldir.to.far + @ldir.to.far.size         ; 2 * 208
bp.audio_buffer.2:      equ bp.audio_buffer.1 + ( bp.audio_buffer.bytes * 2 )
bp.audio_buffer.2.qss:  equ bp.audio_buffer.1 + ( bp.audio_buffer.bytes * 4 )

burstplayer.start:      equ 0x8000

video.memory.high:              equ 0x8000
video.memory.high.attributes:   equ video.memory.high + 0x2000
video.memory.bytes.per.row:     equ 0x20

video.memory.24.rows:   equ ( 8 * video.memory.bytes.per.row )
video.memory.32.rows:   equ ( 6 * video.memory.bytes.per.row )

;---------------------------------------------------------------
; page.loader:

    loader.device:      equ 0xc011
    loader.font_high:   equ 0xc012

