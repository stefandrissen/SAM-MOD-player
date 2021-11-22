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

; ------------------------------------------------------------------------------
; protracker

    mod.pt.song.positions:      equ mod.samples + 31 * mod.sample.len               ; [0x01-0x7f]

    mod.pt.pattern.table:       equ mod.pt.song.positions + 2
        mod.pattern.table.len:  equ 128

    mod.pt.id:                  equ mod.pt.pattern.table + mod.pattern.table.len    ; ['M.K.','FLT4','M!K!']
        mod.pt.id.len:          equ 4

    mod.pt.pattern:             equ mod.pt.id + mod.pt.id.len
        mod.pattern.len:        equ 64 * 4 * 4

    ; mod.pt.sample.data:       equ mod.pt.pattern + patterns * mod.pattern.len

; ------------------------------------------------------------------------------
; noisetracker

    mod.nt.song.positions:      equ mod.samples + 31 * mod.sample.len               ; [0x01-0x7f]
    mod.nt.restart.position:    equ mod.nt.song.positions + 1                       ; [0x7f]

    mod.nt.pattern.table:       equ mod.nt.song.positions + 2

    mod.nt.pattern:             equ mod.nt.pattern.table + mod.pattern.table.len    ; 1024 bytes (64 * 4 * 4)


    ; mod.pt.sample.data:        equ mod.ntpattern + patterns * mod.pattern.len
