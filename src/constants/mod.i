; SAM MOD player - module definition

; new reference: https://github.com/8bitbubsy/pt2-clone/tree/master/src


; ------------------------------------------------------------------------------
mod.title:          equ 0

    mod.title.len:  equ 20

; ------------------------------------------------------------------------------
; sample table
; all words are big-endian (msb/lsb)
; 31 sample entries

mod.samples:    equ mod.title + mod.title.len

    mod.sample.title:               equ 0
        mod.sample.title.len:       equ 22
    mod.sample.len.words:           equ 22  ; length in words (first word ignored)
    mod.sample.finetune:            equ 24  ; [0x00-0x0f] signed nibble
    mod.sample.volume:              equ 25  ; [0x00-0x40]
    mod.sample.repeat.offset.words: equ 26  ;
    mod.sample.repeat.len.words:    equ 28  ; > 1 is repeat

    mod.sample.len:                 equ 30

mod.song.positions:         equ mod.samples + 31 * mod.sample.len               ; [0x01-0x7f]

mod.pattern.table:          equ mod.song.positions + 2
    mod.pattern.table.len:  equ 128

mod.id:                     equ mod.pattern.table + mod.pattern.table.len    ; ['M.K.','FLT4','M!K!']
    mod.id.len:             equ 4   ; introduced in Soundtracker 2.3

mod.pattern:                equ mod.id + mod.id.len
    mod.pattern.rows:       equ 0x40
    mod.pattern.channels:   equ 4
    mod.pattern.note:       equ 4
        mod.note.sample.hi: equ 0   ; H  = upper sample,
        mod.note.note.hi:   equ 0   ; L  = upper note
        mod.note.note:      equ 1   ; HL = note
        mod.note.sample.lo: equ 2   ; H  = lower sample,
        mod.note.effect:    equ 2   ; L  = command
        mod.note.command:   equ 3   ; HL = parameter

    mod.pattern.len:        equ mod.pattern.rows * mod.pattern.channels * mod.pattern.note

; mod.sample.data:          equ mod.pattern + patterns * mod.pattern.len
