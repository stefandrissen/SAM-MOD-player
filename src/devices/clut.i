; colour look up table (clut)

;-------------------------------------------------------------------------------

properties.clut:

    defw 0,0                    ; init routine
    defw @out,@out.len          ; sample out routine
    defw port.clut              ; bc - output port
    ;     GRB!grb
    defb %0110100               ; e - control
    defb %0010111               ; d - control
    defw @timing.ram            ; timing table internal RAM
    defw @timing.megabyte       ; timing table external RAM (megabyte)
    defb 6                      ; bits per channel

;-------------------------------------------------------------------------------

@out:

                ; T-s  bytes
    out (c),e   ; 16   2
    inc b       ;  4   1
    outi        ; 24   2
    out (c),d   ; 16   2
    inc b       ;  4   1
    outi        ; 24   2

    @out.len: equ $ - @out

;-------------------------------------------------------------------------------

@timing.ram:

    defb  33,121,121,121,121,121,121,121    ;   0
    defb 121,121,121,121,121,121,121,121    ;   8
    defb 121,121,121,121,121,121,121,121    ;  16
    defb 121,122,121,121,121,121,121,121    ;  24
    defb 121,121,121,121,121,121,121,121    ;  32
    defb 121,121,121,121,121,121,121,121    ;  40
    defb 121,121,121,121,121,121,121,121    ;  48
    defb 121,121,121,121,121,121,121,121    ;  56
    defb 121,121,121,121,121,121,121,121    ;  64
    defb 121,121,121,121,121,121,121,122    ;  72
    defb 110, 76, 79, 76, 78, 76, 79, 77    ;  80
    defb  82, 77, 80, 75, 79, 76, 78, 76    ;  88
    defb  79, 77, 82, 77, 80, 75, 79, 76    ;  96
    defb  78, 76, 79, 77, 82, 77, 80, 75    ; 104
    defb  79, 76, 78, 76, 79, 77, 82, 77    ; 112
    defb  80, 75, 80, 90, 97                ; 120
    defb 0,-1,-1,-1

    defb 83

    assert $ - @timing.ram == 130

;-------------------------------------------------------------------------------

@timing.megabyte:   ; !!! incorrect

    defb      34
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 124,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 123,123
    defb 123,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 121,121
    defb 122,111
    defb  80, 83
    defb  83, 83
    defb  78, 80
    defb  81, 86
    defb  82, 84
    defb  78, 80
    defb  81, 86
    defb  82, 83
    defb  78, 80
    defb  81, 86
    defb  82, 83
    defb  78, 80
    defb  81, 86
    defb  82, 83
    defb  78, 80
    defb  81, 86
    defb  82, 82
    defb  79, 80
    defb  81, 86
    defb  82, 84
    defb  76, 89
    defb  93, 80
    defb 0

    defb 0,0,-1     ; I seem to have lost the last out

    defb 83

    assert $ - @timing.megabyte == 130

;-------------------------------------------------------------------------------

