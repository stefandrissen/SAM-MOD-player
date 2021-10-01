; saa1099 soundchip

; datasheets:
;   - 1986 November https://datasheetspdf.com/datasheet/SAA1099.html
;   - 1984 August   https://www.opl3.com/wp-content/uploads/2020/04/SAA-1099v1.pdf

;---------------------------------------------------------------

port.sound.data:                    equ 0x00ff ; 255
port.sound.address:                 equ 0x01ff ; 511

;---------------------------------------------------------------

; saa1099 soundchip registers

;---------------------------------------------------------------
; amplitude control - 4 bits right and 4 bits left

saa.register.amplitude_0:           equ 0x00 ; rrrrllll
saa.register.amplitude_1:           equ 0x01 ; rrrrllll
saa.register.amplitude_2:           equ 0x02 ; rrrrllll
saa.register.amplitude_3:           equ 0x03 ; rrrrllll
saa.register.amplitude_4:           equ 0x04 ; rrrrllll
saa.register.amplitude_5:           equ 0x05 ; rrrrllll

;---------------------------------------------------------------
; frequency control - 8 bits

saa.register.frequency_tone_0:      equ 0x08 ; ffffffff
saa.register.frequency_tone_1:      equ 0x09 ; ffffffff
saa.register.frequency_tone_2:      equ 0x0a ; ffffffff
saa.register.frequency_tone_3:      equ 0x0b ; ffffffff
saa.register.frequency_tone_4:      equ 0x0c ; ffffffff
saa.register.frequency_tone_5:      equ 0x0d ; ffffffff

;---------------------------------------------------------------
; octave control - 3 bits

saa.register.octave_1_0:            equ 0x10 ; .111.000
saa.register.octave_3_2:            equ 0x11 ; .333.222
saa.register.octave_5_4:            equ 0x12 ; .555.444

    ; 0    31 Hz  -  61 Hz
    ; 1    61 Hz  - 122 Hz
    ; 2   122 Hz  - 244 Hz
    ; 3   245 Hz  - 488 Hz
    ; 4   489 Hz  - 977 Hz
    ; 5   978 Hz  - 1.95 kHz
    ; 6  1.96 kHz - 3.91 kHz
    ; 7  3.91 kHz - 7.81 kHz

;---------------------------------------------------------------

saa.register.frequency_enable:      equ 0x14 ; ..543210
saa.register.noise_enable:          equ 0x15 ; ..543210

;---------------------------------------------------------------

saa.register.noise_generator_1_0:   equ 0x16 ; ..11..00

    saa.noise_0.31.3kHz:                equ   %00000000  ; 31.3 kHz
    saa.noise_0.15.6kHz:                equ   %00000001  ; 15.6 kHz
    saa.noise_0.7.6kHz:                 equ   %00000010  ;  7.6 kHz
    saa.noise_0.variable:               equ   %00000011  ; 61 Hz - 15.6 kHz (frequency generator 0)

    saa.noise_1.31.3kHz:                equ   %00000000  ; 31.3 kHz
    saa.noise_1.15.6kHz:                equ   %00010000  ; 15.6 kHz
    saa.noise_1.7.6kHz:                 equ   %00100000  ;  7.6 kHz
    saa.noise_1.variable:               equ   %00110000  ; 61 Hz - 15.6 kHz (frequency generator 3)

;---------------------------------------------------------------
; envelope control

saa.register.envelope_generator_0:  equ 0x18 ; e.cfttts
saa.register.envelope_generator_1:  equ 0x19 ; e.cfttts

    saa.envelope.left_right.same:       equ   %00000000  ; left and right the same
    saa.envelope.left_right.inverse:    equ   %00000001  ; left and right inverse

    saa.envelope.mode.zero:             equ   %00000000  ; zero amplitude
    saa.envelope.mode.maximum:          equ   %00000010  ; maximum amplitude
    saa.envelope.mode.decay:            equ   %00000100  ; single decay          \
    saa.envelope.mode.repeat_decay:     equ   %00000110  ; repetitve decay       \\\\
    saa.envelope.mode.triangle:         equ   %00001000  ; single triangular     /\
    saa.envelope.mode.repeat_triangle:  equ   %00001010  ; repetitve triangular  /\/\
    saa.envelope.mode.attack:           equ   %00001100  ; single attack         /
    saa.envelope.mode.repeat_attack:    equ   %00001110  ; repetitive attack     ////

    saa.envelope.bits.4:                equ  %00000000  ; 4 bits for envelope control (max 977 Hz)
    saa.envelope.bits.3:                equ  %00010000  ; 3 bits for envelope control (max 1.95 kHz)

    saa.envelope.internal_clock:        equ  %00000000  ; internal envelope clock (frequency generator 1 or 4)
    saa.envelope.external_clock:        equ  %00100000  ; external envelope clock (address write pulse)

    saa.envelope.reset:                 equ  %00000000  ; reset
    saa.envelope.enabled:               equ  %10000000  ; enabled

;---------------------------------------------------------------

saa.register.sound_enable:          equ 0x1c ; ......re

    saa.se.channels.disabled:           equ   %00000000  ; all channels disabled
    saa.se.channels.enabled:            equ   %00000001  ; all channels enabled

    saa.se.generators.enabled:          equ   %00000000  ; all generators enabled
    saa.se.generators.reset:            equ   %00000010  ; all generators reset and synchronized (November 1986 datasheet)
