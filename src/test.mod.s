
; 1         Volume of sample. Legal values are 0..64. Volume is the linear
;           difference between sound intensities. 64 is full volume, and
;           the change in decibels can be calculated with 20*log10(Vol/64)

; unrolled (incorrect) sample at 883c: 01 02 81 80 01 02 81 80 01 02 81 80

; st 0c00

; 64 volume tables
; 0200
; 2100 0x40 -> & 7f -> 0 -> 3f

; 2a7a = sample buffer 7f 80 80 80 40 80 40 80 7f 80 80 80 40 80 40 80

; 5ff4 = write


; loop: 81 80 ff 00 81 80 ff 00 81 80 ff 00

; https://pastebin.com/pg95YduC

; an attempt at a source code mod file

    org 0

song.name:
    defm "song name           "

;---------------------------------------------------------------
sample.info:                                    ; 31 samples

    defm "sample one            "
    defb ( sample.1.length / 2 ) / 256          ; length - in big endian WORDS
    defb ( sample.1.length / 2 ) \ 256          ;
    defb 0                                      ; finetune
    defb 0x40                                   ; volume
    defb ( sample.1.repeat.offset / 2 ) / 256   ; repeat offset - in big endian WORDS
    defb ( sample.1.repeat.offset / 2 ) \ 256
    defb ( sample.1.repeat.length / 2 ) / 256   ; repeat length - in big endian WORDS
    defb ( sample.1.repeat.length / 2 ) \ 256

    defs 30 * ( $ - sample.info )

assert ( $ == 950 )

song.length:    defb 1
                defb 0
;---------------------------------------------------------------
song.positions:

    defb 0
    defs 127

    defm "M.K."

assert ( $ == 1084 )

;---------------------------------------------------------------
patterns:

; standard protracker is 3 octaves [1-3]
; octave 0 and 4 are additional for 5 octave mods

    note.C_0:   equ 1712 ; 0x6b0
    note.Cs0:   equ 1616
    note.D_0:   equ 1525
    note.Ds0:   equ 1440
    note.E_0:   equ 1357
    note.F_0:   equ 1281
    note.Fs0:   equ 1209
    note.G_0:   equ 1141
    note.Gs0:   equ 1077
    note.A_0:   equ 1017
    note.As0:   equ 961
    note.B_0:   equ 907

    note.C_1:   equ 856 ; 0x358
    note.Cs1:   equ 808
    note.D_1:   equ 762
    note.Ds1:   equ 720
    note.E_1:   equ 678
    note.F_1:   equ 640
    note.Fs1:   equ 604
    note.G_1:   equ 570
    note.Gs1:   equ 538
    note.A_1:   equ 508
    note.As1:   equ 480
    note.B_1:   equ 453

    note.C_2:   equ 428 ; 0x1ac
    note.Cs2:   equ 404
    note.D_2:   equ 381
    note.Ds2:   equ 360
    note.E_2:   equ 339
    note.F_2:   equ 320
    note.Fs2:   equ 302
    note.G_2:   equ 285
    note.Gs2:   equ 269
    note.A_2:   equ 254
    note.As2:   equ 240
    note.B_2:   equ 226

    note.C_3:   equ 214 ; 0x0d6
    note.Cs3:   equ 202
    note.D_3:   equ 190
    note.Ds3:   equ 180
    note.E_3:   equ 170
    note.F_3:   equ 160
    note.Fs3:   equ 151
    note.G_3:   equ 143
    note.Gs3:   equ 135 ; 0x087
    note.A_3:   equ 127 ; 0x07f
    note.As3:   equ 120 ; 0x078
    note.B_3:   equ 113 ; 0x071

    note.C_4:   equ 107 ;
    note.Cs4:   equ 101
    note.D_4:   equ 95
    note.Ds4:   equ 90
    note.E_4:   equ 85
    note.F_4:   equ 80
    note.Fs4:   equ 76
    note.G_4:   equ 71
    note.Gs4:   equ 67
    note.A_4:   equ 64
    note.As4:   equ 60
    note.B_4:   equ 57

    note.test:  equ 341 ; pitch has speed 1, fraction 0
    note.test.h:    equ note.test / 256
    note.test.l:    equ note.test \ 256

row.1:

    ; SPPPSECC S = sample, p = period, e = effect, c = command

    @channel.1:
        defb note.test.h        ; H  = upper sample, L upper note
        defb note.test.l        ; HL = note
        defb 0x10               ; H  = lower sample, L = effect
        defb 0x00               ; HL = command
    @channel.2:
        defb 0
        defb 0
        defb 0
        defb 0
    @channel.3:
        defb 0
        defb 0
        defb 0
        defb 0
    @channel.4:
        defb 0
        defb 0
        defb 0
        defb 0

row.2:

    @channel.1:
        defb 0x00       ; H  = upper sample, L upper note
        defb 0x00       ; HL = note
        defb 0x00       ; H  = lower sample, L = effect
        defb 0x00       ; HL = command
    @channel.2:
        defb 0x00
        defb 0x00
        defb 0x00
        defb 0x00
    @channel.3:
        defb 0x00
        defb 0x00
        defb 0x00
        defb 0x00
    @channel.4:
        defb 0x00
        defb 0x00
        defb 0x00
        defb 0x00

row.63:

    defs 62 * 16


assert ( ( $ - patterns ) \ 1024 == 0 )

;---------------------------------------------------------------
samples:

sample.1:

    defb 0x01,0x02  ; first two bytes /should/ be 0x00,0x00

sample.1.repeat:
sample.1.repeat.offset: equ $ - sample.1

;    for 52, defb 127
;    for 156, defb -128
    defb 127,-128
    defb 127,-128

sample.1.repeat.length: equ $ - sample.1.repeat

sample.1.length: equ $ - sample.1
