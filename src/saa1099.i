; saa1099 soundchip registers

;---------------------------------------------------------------
; amplitude control - 4 bits right and 4 bits left

saa.register.amplitude_0:           equ &00 ; rrrrllll
saa.register.amplitude_1:           equ &01 ; rrrrllll
saa.register.amplitude_2:           equ &02 ; rrrrllll
saa.register.amplitude_3:           equ &03 ; rrrrllll
saa.register.amplitude_4:           equ &04 ; rrrrllll
saa.register.amplitude_5:           equ &05 ; rrrrllll

;---------------------------------------------------------------
; frequency control - 8 bits

saa.register.frequency_tone_0:      equ &08 ; ffffffff
saa.register.frequency_tone_1:      equ &09 ; ffffffff
saa.register.frequency_tone_2:      equ &0a ; ffffffff
saa.register.frequency_tone_3:      equ &0b ; ffffffff
saa.register.frequency_tone_4:      equ &0c ; ffffffff
saa.register.frequency_tone_5:      equ &0d ; ffffffff

;---------------------------------------------------------------
; octave control - 3 bits

saa.register.octave_1_0:            equ &10 ; .111.000
saa.register.octave_3_2:            equ &11 ; .333.222
saa.register.octave_5_4:            equ &12 ; .555.444

    ; 0    31 Hz  -  61 Hz
    ; 1    61 Hz  - 122 Hz
    ; 2   122 Hz  - 244 Hz
    ; 3   245 Hz  - 488 Hz
    ; 4   489 Hz  - 977 Hz
    ; 5   978 Hz  - 1.95 kHz
    ; 6  1.96 kHz - 3.91 kHz
    ; 7  3.91 kHz - 7.81 kHz

;---------------------------------------------------------------

saa.register.frequency_enable:      equ &14 ; ..543210
saa.register.noise_enable:          equ &15 ; ..543210

;---------------------------------------------------------------

saa.register.noise_generator_1_0:   equ &16 ; ..11..00

    saa.noise_0.31.3kHz:                equ %00000000   ; 31.3 kHz
    saa.noise_0.15.6kHz:                equ %00000001   ; 15.6 kHz
    saa.noise_0.7.6kHz:                 equ %00000010   ;  7.6 kHz
    saa.noise_0.variable:               equ %00000011   ; 61 Hz - 15.6 kHz (frequency generator 0)

    saa.noise_1.31.3kHz:                equ %00000000   ; 31.3 kHz
    saa.noise_1.15.6kHz:                equ %00010000   ; 15.6 kHz
    saa.noise_1.7.6kHz:                 equ %00100000   ;  7.6 kHz
    saa.noise_1.variable:               equ %00110000   ; 61 Hz - 15.6 kHz (frequency generator 3)

;---------------------------------------------------------------
; envelope control

saa.register.envelope_generator_0:  equ &18 ; e.cfttts
saa.register.envelope_generator_1:  equ &19 ; e.cfttts

    saa.envelope.left_right.same:       equ %00000000   ; left and right the same
    saa.envelope.left_right.inverse:    equ %00000001   ; left and right inverse

    saa.envelope.mode.zero:             equ %00000000   ; zero amplitude
    saa.envelope.mode.maximum:          equ %00000010   ; maximum amplitude
    saa.envelope.mode.decay:            equ %00000100   ; single decay          \
    saa.envelope.mode.repeat_decay:     equ %00000110   ; repetitve decay       \\\\
    saa.envelope.mode.triangle:         equ %00001000   ; single triangular     /\
    saa.envelope.mode.repeat_triangle:  equ %00001010   ; repetitve triangular  /\/\
    saa.envelope.mode.attack:           equ %00001100   ; single attack         /
    saa.envelope.mode.repeat_attack:    equ %00001110   ; repetitive attack     ////

    saa.envelope.bits.4:                equ %00000000   ; 4 bits for envelope control (max 977 Hz)
    saa.envelope.bits.3:                equ %00010000   ; 3 bits for envelope control (max 1.95 kHz)

    saa.envelope.internal_clock:        equ %00000000   ; internal envelope clock (frequency generator 1 or 4)
    saa.envelope.external_clock:        equ %00100000   ; external envelope clock (address write pulse)

    saa.envelope.reset:                 equ %00000000   ; reset
    saa.envelope.enabled:               equ %10000000   ; enabled

;---------------------------------------------------------------

saa.register.sound_enable:          equ &1c ; .......e

    saa.se.disabled:                    equ %00000000
    saa.se.enabled:                     equ %00000001
