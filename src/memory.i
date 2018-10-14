; memory layout

version.major:		equ	"2"
version.minor.1:	equ	"2"
version.minor.2:	equ	"1"

; burstplayer

page.loader:				equ 1	
page.burstplayer:			equ 2

	addr.demo:					equ 57344
	
page.sequencer:				equ 4

	init.seq:					equ 32768	; initialise sequencer routine
	install.mod:				equ 32771	; install mod by adding "runways"
	sq.pointer.addr.demo:		equ 32774	; address of foreground program (>32k)
	sq.pointer.page.demo:		equ 32776	; page of foreground program
	sq.pointer.page.mod:		equ 32777 	; page mod loaded in at (at 32k)
	sq.octaves:					equ 32778	; 3 or 5 octave mode
	sq.external.ram:			equ 32779

page.create.burstplayer:	equ 5	; once created, overwritten by mod

page.mod:					equ page.create.burstplayer ; 0 when megabyte
page.mod.megabyte:			equ 0
;---------------------------------------------------------------
; page.burstplayer

burstplayer.device:			equ 32771
burstplayer.speed:			equ 32772
burstplayer.external.ram:	equ 32773

bp.id:						equ 53		; "BUR"
bp.device:					equ 105
bp.pointers:				equ 106		; offsets to variables located in burst page
bp.pointer.addr.sequencer:	equ 106
bp.pointer.page.sequencer:	equ 108
bp.pointer.addr.demo:		equ 110
bp.pointer.page.demo:		equ 112
bp.pointer.addr.enable:		equ 114
bp.pointer.addr.exit:		equ 116

bp.pointers.sample:			equ 118
bp.pointers.length:			equ 12 

; store for current pattern row + other common variables
; there are 256 bytes reserved for this

var:					equ 256

mod.current.row:		equ var	+ 0		; 16 bytes of current row being played
frame.palette:			equ var + 16	; 16 byte palette set at start frame
frame.screen:			equ var + 32	; screen page (+mode) set at start of frame, 0 = no change
int.routine:			equ var + 33	; address (>32k) or interrupt routine
int.rtn.pag:			equ var + 35	; -1 = no interrupt else page

c1.on:					equ var + 36	; channel 1 on/off
c2.on:					equ var + 37	; channel 2 on/off
c3.on:					equ var + 38	; channel 3 on/off
c4.on:					equ var + 39	; channel 4 on/off
vol.update:				equ var + 40	; when set do extra burst volume update to ensure 
										; burstplayer acts on changed value of c?.on.

countint:				equ var + 41	; user frame counter
counter.fract:			equ var + 42	; 1/256 frame counter for sequencer
counter:				equ var + 43	; frame counter for sequencer
speed:					equ var + 44	; song speed in frames
tempo:					equ var + 45	; bpm speed (relative to 125)
song.pos:				equ var + 47	; position in songtable (0-127)
pattern.num:			equ var + 48	; pattern being played (0-255)
pattern.pos:			equ var + 49	; row being played (0-63)
enable.burst:			equ var + 50	; start burstplayer
exit.burst:				equ var + 52	; stop burstplayer and exit
disable.pos:			equ var + 54	; disable "B" command (jump) + looping
mstatus:				equ var + 55	; 0=playing, 1=stopped

@var.size:				equ 128

ldir.far.buffer:		equ var + @var.size						; used by ldir.from.far / ldir.to.far, can be used for other purposes if not used
@ldir.far.size:			equ 128

volume.table:			equ ldir.far.buffer + @ldir.far.size	; 32 volume tables * 256 bytes = 8k
@volume.table.size:		equ 32 * 256

pitch.table:			equ volume.table + @volume.table.size	; 1024 pitches * 2 bytes = 2k
@pitch.table.size:		equ 2 * 1024

get.pattern:			equ pitch.table + @pitch.table.size		; copies AHL -> mod.current.row, 16 bytes  
@get.pattern.size:		equ 49
		
far.call:				equ get.pattern + @get.pattern.size		; call CHL, C=set with return page!
@far.call.size:			equ 19

ldir.from.far:			equ far.call + @far.call.size 			; copy BHL to buffer, C bytes (max 128)
@ldir.from.far.size:		equ 27

ldir.to.far:			equ ldir.from.far + @ldir.from.far.size	; copy buffer to BDE, C bytes (max 128)
@ldir.to.far.size:		equ 27

;! 256 byte align these?
; play tables ( 208 * 2 * 2) if QSS -> 208 * 4 * 2
 
playtab1:				equ ldir.to.far + @ldir.to.far.size 		; 2 * 208  


burst.player:	equ 32768

video.memory.low:				equ 0
video.memory.low.attributes:	equ video.memory.low + 8192 

video.memory.high:				equ 32768
video.memory.high.attributes:	equ video.memory.high + 8192 

video.memory.24.rows:	equ 256
video.memory.32.rows:	equ 192

loader.font:	equ 16401
