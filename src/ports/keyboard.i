; keyboard
;---------------------------------------------------------------

port.status_register:               equ 0xf9 ; 249
port.status:                        equ port.status_register

    ; key pressed = 3 highest bits
    keyboard.inv_period_comma:          equ %01111111
    keyboard.edit_colon_semicolon:      equ %10111111
    keyboard.f0_quotes_equals:          equ %11011111
    keyboard.delete_plus_minus:         equ %11101111
    keyboard.caps_tab_esc:              equ %11110111
    keyboard.f9_f8_f7:                  equ %11111011
    keyboard.f6_f5_f4:                  equ %11111101
    keyboard.f3_f2_f1:                  equ %11111110

;---------------------------------------------------------------

port.keyboard_register:             equ 0xfe ; 254
port.keyboard:                      equ port.keyboard_register

    ; key pressed = 5 lowest bits
    keyboard.cursors_cntrl:             equ %11111111 ; right,left,down,up,cntrl
    keyboard.vcxz_shift:                equ %11111110
    keyboard.gfdsa:                     equ %11111101
    keyboard.trewq:                     equ %11111011
    keyboard.12345:                     equ %11110111
    keyboard.67890:                     equ %11101111
    keyboard.yuiop:                     equ %11011111
    keyboard.hjkl_return:               equ %10111111
    keyboard.bnm_symbol_space:          equ %01111111

;---------------------------------------------------------------
