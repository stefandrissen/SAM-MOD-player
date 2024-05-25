;SAM MOD player - tracker

;(C) 1995-2024 Stefan Drissen

; first execute "BURST" and install "DEMO"

; https://pastebin.com/pg95YduC - 8bitbubsy
; https://github.com/cmatsuoka/tracker-history/blob/master/reference/amiga/soundtracker/Ultimate_Soundtracker-format.txt
; https://github.com/OpenMPT/openmpt/blob/master/soundlib/Load_mod.cpp
; https://wiki.openmpt.org/Manual:_Effect_Reference

; https://github.com/johnnovak/nim-mod/blob/master/doc/Protracker%20effects%20(FireLight)%20(.mod).txt

; revisit main loop:
; - https://github.com/zeropolis79/PETSCIIRobots-SDL/blob/main/PT2.3A_replay_cia.cpp
; - https://github.com/8bitbubsy/pt2-clone/blob/master/src/pt2_replayer.c
; - https://github.com/8bitbubsy/pt23f/blob/main/PT2.3F.s

; Amiga audio hardware reference
; - https://www.amigarealm.com/computing/knowledge/hardref/ch5.htm

include "../memory.i"
include "../ports/internal.i"
include "../ports/megabyte.i"
include "../constants/mod.i"

    org 0x8000

;-------------------------------------------------------------------------------

tracker.init:           jp @init.tracker
tracker.install.mod:    jp @install.mod

;-------------------------------------------------------------------------------

tracker.ptr.addr.demo:  defw 0              ; offset
tracker.ptr.page.demo:  defb 0              ; & page of demo (foreground)

tracker.ptr.page.mod:   defb page.mod       ; page mod loaded in at
tracker.octaves:        defb 3              ; number of octaves (3 or 5)
tracker.samples:        defb 0              ; [15|31]
tracker.ram:            defb 0              ; %XXXRR (RAM / 256K)
tracker.gap:            defb 0              ; [3|6]

;-------------------------------------------------------------------------------

@init.tracker:  include "init.s"
@install.mod:   include "install.mod.s"

;===============================================================================
; tracker

    defs align 0x100

pattern.table:
    defs 0x100

sample.table:

    org 0   ; offsets

 st.start:      defw 0x0000 ; offset
                defb 0x00   ; page

 st.end:        defw 0x0000 ; offset
                defb 0x00   ; page = start gap

 st.loop:       defb 0x00
    st.loop.none:   equ 0
    st.loop.large:  equ 1
    st.loop.small:  equ 2

 st.loop.end:               ; small -> loop end in gap
 st.loop.start:             ; large -> loop start
                defw 0x0000
                defb 0x00

 st.vol:        defb 0x00   ; volume
 st.finetune:   defb 0x00   ; fine tune value

 st.sample:     defb 0x00   ; empty sample?
                defb 0x00   ; \
                defb 0x00   ;  ) unused
                defb 0x00   ; /

 sample.table.len:    equ $
    assert sample.table.len == 16

    org $ + sample.table

 st.prev.loop:   equ sample.table.len - st.loop
 st.prev.start:  equ sample.table.len - st.start
 st.prev.sample: equ sample.table.len - st.sample

    defs 31 * sample.table.len



; lookup table to get from a note period [56..856] to a note number [0 - 47]
; this table is populated using the values from fine tune 0
table.periods: defs 1024

finelist:       include "tables/finetune.i"

;-------------------------------------------------------------------------------
;first 64 bytes of bpm table are not used, so use this space
;for arpeggio table (32 bytes) and vibrato table (32 bytes) instead.

table.bpm:

table.arpeggio: include "tables/arpeggio.i"
table.vibrato:  include "tables/vibrato.i"
                include "tables/bpm.i"

;-------------------------------------------------------------------------------
table.retrig:   include "tables/retrig.i"

;-------------------------------------------------------------------------------
tracker:

    call c1 + update.bp     ; check for sample boundaries
    call c2 + update.bp     ; and loop the samples if
    call c3 + update.bp     ; necessary
    call c4 + update.bp     ;

    ld hl,vol.update        ; ensures instant response to
    ld a,(hl)               ; a channel being toggled on
    or a                    ; or off
    jr z,@skip.instant.response

    call c1 + bp.volume
    call c2 + bp.volume
    call c3 + bp.volume
    call c4 + bp.volume

    ld (hl),0

 @skip.instant.response:

    ld hl,countint
    inc (hl)

    ld a,(int.rtn.pag)
    inc a
    jr z,@skip.extra.interrupt

    dec a
    ld hl,(int.routine)
    ld c,a
    call far.call

 @skip.extra.interrupt:

    ld a,(mstatus)
    dec a
    ret z

    ld hl,(song.tick.fraction)
    ld a,h
    ld de,(song.bpm)
    add hl,de
    ld (song.tick.fraction),hl
    cp h                    ; tick integer not changed
    ret z

    ld a,(song.speed)
    ld c,a
    ld a,h
    sub c
    jr c,@on.same.row       ; tick <> speed

    ld (song.tick),a

    ld a,(pattern_delay.counter)
    or a
    jr z,@playVoices        ; get note data if no delay

    call @checkEffects      ; else just do fx
    jp dskip

 @on.same.row:              ; tick <> speed
    call @checkEffects      ; do fx
    jp nonewposyet          ; check position change

;-------------------------------------------------------------------------------
@checkEffects:

    ;no new note data for all channels - fx only

    call c1 + check.fx
    call c2 + check.fx
    call c3 + check.fx
    jp c4 + check.fx

;-------------------------------------------------------------------------------
@playVoices:

    ld a,(song.position)
    ld l,a
    ld h,pattern.table / 0x100
    ld a,(hl)               ;get pattern
    ld (pattern.num),a
    ld d,a
    and 0xf0
    rlca
    rlca
    rlca
    rlca
    ld c,a
    ld a,(tracker.ptr.page.mod)
    add c
    ld c,a

    ld a,d
    and 0x0f
    add a,a
    add a,a
 origpat.offsh:
    add a,0                 ;pattern offset hi byte
    ld d,a
 origpat.offsl:
    ld e,0                  ;pattern offset lo byte
    bit 6,d
    res 6,d
    jr z,$+3
    inc c

    ld a,(pattern.row)
    add a
    add a
    ld h,0
    ld l,a
    add hl,hl
    add hl,hl               ;*16
    add hl,de
    ld a,c

    call get.pattern.row    ; in lower memory

    call c1 + play.voice
    call c2 + play.voice
    call c3 + play.voice
    call c4 + play.voice

 dskip:
    ld hl,pattern.row
    inc (hl)

 pattern_delay.flag: equ $+1
    ld a,0
    or a
    jr z,@no.new.delay

    ld (pattern_delay.counter),a
    xor a
    ld (pattern_delay.flag),a

 @no.new.delay:

   pattern_delay.counter: equ $+1
    ld a,0
    or a
    jr z,@no.pattern_delay

    dec a
    ld (pattern_delay.counter),a
    jr z,@no.pattern_delay

    dec (hl)                ; if pattern delay -> undo inc (hl)

 @no.pattern_delay:
   pattern_break.flag: equ $+1
    ld a,0
    or a
    jr z,nnpysk

    xor a
    ld (pattern_break.flag),a
 pattern_break.row: equ $+1
    ld a,0
    ld (hl),a
    xor a
    ld (pattern_break.row),a

 nnpysk:

    ld a,(hl)
    cp 64
    jr c,nonewposyet

 next.position:
    ld a,(pattern_break.row)
    ld (pattern.row),a
    xor a
    ld (pattern_break.row),a
    ld (position_jump.flag),a
    ld hl,song.position
    inc (hl)
    ld a,(hl)
    bit 7,a
    jr nz,loop.time   ;reached song position 128

   song.length: equ $+1
    cp 0
    jr nz,nonewposyet

 loop.time:

    ld (hl),0
    call reset.speed

 play.status:
    ld a,(disable.pos) ;0=keep repeating
    or a
    ret z

 quit:

    ld (mstatus),a
    ret

 nonewposyet:
   position_jump.flag: equ $+1
    ld a,0              ; initial 0, position_jump or pattern_break -> 1
    or a
    jr nz,next.position

    ret

;-------------------------------------------------------------------------------
   defs align 0x20  ; needed to align @list.effects.on_tick_0 in copied routines

routines:

; these routines are copied three times so that there is one for each channel the
; burst player addresses are put in by conv.list

;===============================================================================
    org 0


update.bp:
    ; update burstplayer sample pointers

 bp.page.1:
    ld a,(0)
 sample_end.page: equ $+1
    sub 0
    ret c               ; not past marker yet (page)

 sample_end.offset: equ $+1
    ld de,0
 bp.offset.1:
    ld hl,(0)                                                   ; hl = current sample pointer
    jr z,@in.sample_end.page
    set 6,h
 @in.sample_end.page:
    sbc hl,de           ; cf not set                            ; check length
    ret c               ; not past marker yet (offset)
                        ; -> hl = bytes past end marker

 repeat: equ $+1
    jr @loop.no         ; can be @loop.no, @loop.large or @loop.small

  ;---------------------
  @loop.no:
    ex de,hl            ; -> hl = end marker
   r1.001:
    ld a,(sample_end.page)
   bp.page.2:
    ld (0),a
   bp.offset.2:
    ld (0),hl

    ret

  ;---------------------
  @loop.large:

   @loop.large.page: equ $+1
    ld a,0
   bp.page.3:
    ld (0),a
   @loop.large.offset: equ $+1  ; bpu peek 2e4c < 80
    ld de,0                     ; de = b7d4
    add hl,de                   ; hl was 6696, after 1e6a
   bp.offset.3:
    ld (0),hl

    ret

  ;---------------------
  @loop.small:

   @loop.small.page: equ $+1
    ld a,0
   bp.page.4:
    ld (0),a
   @loop.small.offset: equ $+1
    ld de,0
    add hl,de
   bp.offset.4:
    ld (0),hl

   @loop.small.end.page: equ $+1
    ld a,0
   @loop.small.end.offset: equ $+1
    ld hl,0
   r1.002:
    ld (sample_end.offset),hl
   r1.003:
    ld (sample_end.page),a

    ret

 @loop.small.jr: equ @loop.small - repeat - 1
 @loop.large.jr: equ @loop.large - repeat - 1

;-------------------------------------------------------------------------------
play.voice:

    ; hl = pattern row
    ;
    ; SPppsECC
    ;
    ; S   s     = sample
    ;  Ppp      = note period
    ;      E    = effect
    ;       xy  = parameter

 mk.cur.pat:
    ld hl,0                 ; hl = pattern row

    ld a,(hl)               ; a = SP
    inc l
    and 0x0f                ; a = P
    ld d,a                  ; d = P
    ld e,(hl)               ; e = pp
    dec l
 r2.001:
    ld (@note_period),de    ; -> Ppp

 ;    or e                    ; if no period given then use
 ; r1.004:
 ;    call z,period.nop       ; last given period

    ex de,hl                ; de = pattern row

    ld h,sample.table / 0x100
    ld a,(de)               ; SP
    and 0x10                ; S
    jr z,@sample.lt.16      ; 16 bytes per sample table entry
    inc h
 @sample.lt.16:
    inc e
    inc e
    ld a,(de)               ; a = sE
    and 0xf0                ; a = s
    ld l,a                  ; l = s -> hl = sample.table entry (16 bytes per entry)
                            ; de = pattern row + 2

    ; TODO: should (re)apply effect.sample.offset

    ld c,(hl)               ; \
    inc l                   ;  > sample start offset
    ld b,(hl)               ; /
    inc l
    ld a,(hl)               ; sample start page
    inc a

 r1.005:
    jp z,set.regs           ; page -1 -> no sample

    dec a
    inc l
 r2.002:
    ld (cx.sample.offset),bc
 r1.006:
    ld (cx.sample.page),a

    ld a,l                  ; highest 4 bits are significant
    xor h                   ; lowest bit is significant, -> xor = unique id

 r1.007:
    ld (instrument.new),a

    ld c,(hl)               ; \
    inc l                   ;  > sample end offset
    ld b,(hl)               ; /
    inc l
    ld a,(hl)               ; sample end page
    inc l
 r2.003:
    ld (sample_end.offset),bc
 r1.008:
    ld (sample_end.page),a

    ld a,(hl)               ; sample loop type
    inc l

    dec a
    jr z,@get.loop.large
    dec a
    jr z,@get.loop.small
    inc l
    inc l
    inc l
    xor a       ; @loop.no
    jr @got.loop

 @get.loop.small:
 r1.009:
    ld a,(sample_end.page)
 r1.010:
    ld (@loop.small.page),a
 r2.004:
    ld (@loop.small.offset),bc
    ld c,(hl)
    inc l
    ld b,(hl)
    inc l
    ld a,(hl)
    inc l
 r1.011:
    ld (@loop.small.end.page),a
 r2.005:
    ld (@loop.small.end.offset),bc
    ld a,@loop.small.jr
    jr @got.loop

 @get.loop.large:
    ld c,(hl)
    inc l
    ld b,(hl)
    inc l
    ld a,(hl)
    inc l              ;start of repeat
 r1.012:
    ld (@loop.large.page),a
 r2.006:
    ld (@loop.large.offset),bc
    ld a,@loop.large.jr

 @got.loop:
 r1.013:
    ld (repeat),a

    ld a,(hl)
    inc l
 r1.014:
    ld (volume),a
 r1.015:
    call bp.volume

    ld a,(hl)
 r1.016:
    ld (finetune),a

 set.regs:
    ex de,hl            ; hl = pattern row + 2

    ld a,(hl)           ; a = sE
    inc l               ; hl = pattern row + 3
    and 0x0f            ; a = _E
 r1.017:
    ld (@command),a
    ld c,a              ; c = _E
    ld a,(hl)           ; a = xy
 r1.018:
    ld (parameter),a
    ld b,a              ; b = xy

 r2.007:
    ld de,(@note_period)
    ld a,d
    or e
 r1.019:
    jp z,@no.new.note

    ld a,b              ; a = xy
    and 0xf0            ; a = x_
    or c                ; c = _E hi = param, lo = command
    cp 0x5e             ; E5 = fine tune
    jr nz,@not.FineTune

 r1.020:
    call @eeffect.setFineTune
    jr @setPeriod

 @not.finetune:

    ld a,c              ; a = _E (only command now)
    cp 3                ; 3 = tone portamento
    jr z,@is.TonePorta

    cp 5                ; 5 = tone portamento + volume slide
    jr nz,@not.TonePorta

 @is.TonePorta:

 r1.021:
    call @set.tone
 r1.022:
    jp @effects.on_tick_0

 @not.TonePorta:

    ; cp 9                ; sample offset
    ; call @effects.on_tick_0

 ;-------------------------------------------------------------------------------
 @setPeriod:
 r1.023:
    ld hl,(@note_period)
    ld a,h
    add table.periods / 0x100
    ld h,a

    ld l,(hl)           ; get note number (*2)
    ld b,l
    inc l               ; 255 = note not found
    jr nz,@found.finetune

 r1.024:
    ld hl,(@note_period)
    jr @no.tune

 @found.finetune:
    dec l
 finetune: equ $+1
    ld a,0
    srl a
    jr nc,$+4
    set 7,l
    add finelist / 256
    ld h,a

    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a

 @no.tune:
 r1.025:
    ld (period),hl
    ld a,b
 r1.026:
    ld (note.number),a    ; 0-71, 255=unknown

 r1.027:
    ld a,(parameter)
    and 0xf0
    ld c,a
 r1.028:
    ld a,(@command)     ; a = _E
    or c
    cp 0xde             ; ED = note delay
    jr z,@effects.on_tick_0

 wav.cntrl: equ $+1
    ld c,0
    xor a
    bit 2,c             ;-> retrigger vibrato
    jr z,@vibrato.unchanged

 r1.029:
    ld (vibr.pos),a

 @vibrato.unchanged:
    bit 6,c             ;-> retrigger tremolo
    jr z,@tremolo.unchanged

 r1.030:
    ld (trem.pos),a

 @tremolo.unchanged:

 cx.sample.offset: equ $+1
    ld hl,0
 cx.sample.page: equ $+1
    ld a,0
 bp.page.5:
    ld (0),a
 bp.offset.5:
    ld (0),hl
    xor a
 bp.speed.fraction.1:
    ld (0),a

 period: equ $+1
    ld de,0
 r1.031:
    call period.nop.de

 r1.032:
    ld a,(instrument.new)
 r1.033:
    ld (instrument.current),a

    ; ld a,1
    ; ld (trigger),a

 @effects.on_tick_0:
 r1.034:
    ld hl,@list.effects.on_tick_0
 r1.035:
    ld a,(@command)
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a

    jp (hl)

 @no.new.note:

 instrument.new: equ $+1
    ld a,0
 instrument.current: equ $+1
    cp 0
    jr z,@effects.on_tick_0

 r1.036:
    ld (instrument.current),a
 r1.037:
    ld hl,(cx.sample.offset)
 r1.038:
    ld a,(cx.sample.page)
 bp.page.6:
    ld (0),a
 bp.offset.6:
    ld (0),hl
    xor a
 bp.speed.fraction.2:
    ld (0),a
    jr @effects.on_tick_0

 check.fx:
 @command: equ $+1
    ld c,0
 parameter: equ $+1
    ld a,0
    or c
    jr z,period.nop     ; no command - use old period in case of arpeg

     r1.039:
    ld hl,@list.effects.after_tick_0    ; align 32
    ld a,c
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a

    jp (hl)

;-------------------------------------------------------------------------------
 period.nop:
 r2.008:
    ld de,(period)

 period.nop.de:
    sla e               ;convert pitch
    rl d
    ld a,d

    add pitch.table / 0x100
    ld d,a
    ld a,(de)
 bp.speed.low:
    ld (0),a
    inc e
    ld a,(de)
 bp.speed.high:
    ld (0),a

    ret

;-------------------------------------------------------------------------------
bp.volume:
 channel.on:
    ld a,(0)                    ; fill in variable
    or a
    jr z,@chan.off

 volume: equ $+1
    ld a,0
    rra
 saa.exvol:
    and %01111111               ; %01111110 when SAA
 @chan.off:
    add volume.table \ 0x100    ; 3 when SAA channel 2/3
 bp.volume_table:
    ld (0),a

    ret

;---------------------------------------------------------------

@effect.arpeggio:               include "effect/0.arpeggio.s"
effect.portaUp:                 include "effect/1.portaUp.s"
effect.portaDown:               include "effect/2.portaDown.s"

@set.tone:
 ;---------------------------------------------------------------
 @note_period: equ $+1
    ld hl,0             ; Ppp
    ld a,h
    add table.periods / 0x100
    ld h,a

    ld l,(hl)           ; get note number (*2)
    ld b,l
    inc l               ; 255 = note not found
    jr nz,@standard.period

 r1.053:
    ld hl,(@note_period)
    jr @skip.finetune   ; can only finetune when standard period

 @standard.period:
    dec l
 r1.054:
    ld a,(finetune)
    srl a
    jr nc,$+4
    set 7,l
    add finelist / 0x100
    ld h,a

    ld a,(hl)
    inc l
    ld h,(hl)
    ld l,a

 @skip.finetune:
 r1.055:
    ld (wanted.period),hl
 r2.010:
    ld de,(period)
    xor a
    sbc hl,de
    jr z,@clear.tone

    adc a,0
 r1.056:
    ld (tone.portamento.direction),a      ;0=porta dn, 1=porta up

    ret

 @clear.tone:
 r1.057:
    ld (wanted.period),hl

    ret

@effect.tonePortamento:         include "effect/3.tonePortamento.s"
@effect.vibrato:                include "effect/4.vibrato.s"
@effect.tonePlusVolSlide:       include "effect/5.tonePlusVolSlide.s"
@effect.vibratoPlusVolSlide:    include "effect/6.vibratoPlusVolSlide.s"
@effect.tremolo:                include "effect/7.tremolo.s"

@effect.sampleOffset:           include "effect/9.sampleOffset.s"
@effect.volumeSlide:            include "effect/A.volumeSlide.s"
@effect.positionJump:           include "effect/B.positionJump.s"
@effect.volumeChange:           include "effect/C.volumeChange.s"
@effect.patternBreak:           include "effect/D.patternBreak.s"
@effect.extended:               include "effect/E.extended.s"
@effect.setSpeed:               include "effect/F.setSpeed.s"

@effect.none:           ; simply continue through to filter RET

;---------------------------------------------------------------
; Extended Effects

@eeffect.filterOnOff:           include "effect.extended/0.filterOnOff.s"
@eeffect.finePortaUp:           include "effect.extended/1.finePortaUp.s"
@eeffect.finePortaDown:         include "effect.extended/2.finePortaDown.s"
@eeffect.setGlissControl:       include "effect.extended/3.setGlissControl.s"
@eeffect.setVibratoControl:     include "effect.extended/4.setVibratoControl.s"
@eeffect.setFineTune:           include "effect.extended/5.setFineTune.s"
@eeffect.jumpLoop:              include "effect.extended/6.jumpLoop.s"
@eeffect.setTremoloControl:     include "effect.extended/7.setTremoloControl.s"

@eeffect.retrigNote:            include "effect.extended/9.retrigNote.s"
@eeffect.volumeFineUp:          include "effect.extended/A.volumeFineUp.s"
@eeffect.volumeFineDown:        include "effect.extended/B.volumeFineDown.s"
@eeffect.noteCut:               include "effect.extended/C.noteCut.s"
@eeffect.noteDelay:             include "effect.extended/D.noteDelay.s"
@eeffect.patternDelay:          include "effect.extended/E.patternDelay.s"

;---------------------------------------------------------------

    defs align 0x20

 ;tables for effect parsing

 @list.effects.on_tick_0:
    ; effects on tick 0
    r0.000: defw period.nop             ; 0
    r0.001: defw period.nop             ; 1
    r0.002: defw period.nop             ; 2
    r0.003: defw period.nop             ; 3
    r0.004: defw period.nop             ; 4
    r0.005: defw period.nop             ; 5
    r0.006: defw period.nop             ; 6
    r0.007: defw period.nop             ; 7
    r0.008: defw period.nop             ; 8
    r0.009: defw @effect.sampleOffset   ; 9
    r0.010: defw period.nop             ; A
    r0.011: defw @effect.positionJump   ; B
    r0.012: defw @effect.volumeChange   ; C
    r0.013: defw @effect.patternBreak   ; D
    r0.014: defw @effect.extended       ; E
    r0.015: defw @effect.setSpeed       ; F

 @list.effects.after_tick_0:
    ; effects not on tick 0
    r0.016: defw @effect.arpeggio               ; 0
    r0.017: defw effect.portaUp                 ; 1
    r0.018: defw effect.portaDown               ; 2
    r0.019: defw @effect.tonePortamento         ; 3
    r0.020: defw @effect.vibrato                ; 4
    r0.021: defw @effect.tonePlusVolSlide       ; 5
    r0.022: defw @effect.vibratoPlusVolSlide    ; 6
    r0.023: defw @effect.tremolo                ; 7
    r0.024: defw @effect.none                   ; 8
    r0.025: defw @effect.none                   ; 9
    r0.026: defw @effect.volumeSlide            ; A
    r0.027: defw @effect.none                   ; B
    r0.028: defw @effect.none                   ; C
    r0.029: defw @effect.none                   ; D
    r0.030: defw @effect.extended               ; E
    r0.031: defw @effect.none                   ; F

 list.effects.extended:
    ; extended effects (effect E)
    r0.032: defw @eeffect.filterOnOff           ; 0
    r0.033: defw @eeffect.finePortaUp           ; 1
    r0.034: defw @eeffect.finePortaDown         ; 2
    r0.035: defw @eeffect.setGlissControl       ; 3
    r0.036: defw @eeffect.setVibratoControl     ; 4
    r0.037: defw @eeffect.setFineTune           ; 5
    r0.038: defw @eeffect.jumpLoop              ; 6
    r0.039: defw @eeffect.setTremoloControl     ; 7
    r0.040: defw @effect.none                   ; 8 not a command
    r0.041: defw @eeffect.retrigNote            ; 9
    r0.042: defw @eeffect.volumeFineUp          ; A
    r0.043: defw @eeffect.volumeFineDown        ; B
    r0.044: defw @eeffect.noteCut               ; C
    r0.045: defw @eeffect.noteDelay             ; D
    r0.046: defw @eeffect.patternDelay          ; E
    r0.047: defw @effect.none                   ; F funk it not supported

;-------------------------------------------------------------------------------

    assert ( $ \ 0x20 == 0 )

routine.len:    ;routine start ORGs at 0 -> routine.len = length

;===============================================================
length: equ routine.len + routines - 0x8000

c1:     equ 0 * routine.len + routines
c2:     equ 1 * routine.len + routines
c3:     equ 2 * routine.len + routines
c4:     equ 3 * routine.len + routines

    defs ( 4 - 1 ) * routine.len

;===============================================================
    org ( 4 * routine.len ) + routines - 0x8000
;---------------------------------------------------------------

move.spc:
move.size:  equ 6 * 256     ;move size = gap size

    ; for octave 4 -> 6 (5 oct)
    ; for octave 3 -> 3 (3 oct)

    ; maximum amount of bytes (*256) needed in one sample frame at:
    ;
    ; * Amiga pitch (3 octaves) = 108 -> burst speed = 808
    ;   808 / 256 * 208 = 656.5 -> 3 * 256 bytes

    ; * extended PC pitch (5 octaves) = 54 -> burst speed = 1616
    ;   1616 / 256 * 208 = 1313 -> 6 * 256 bytes

;-------------------------------------------------------------------------------

    org $ + 0x8000

conv.list:  include "lists/conv.i"
build.list: include "lists/build.i"

;-------------------------------------------------------------------------------

    defs move.size - ( $ - conv.list )

    assert $ < 0xc000