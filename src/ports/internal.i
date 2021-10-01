; ports

;---------------------------------------------------------------

port.color_look_up_table:           equ 0xf8 ; 248
port.clut:                          equ port.color_look_up_table

;---------------------------------------------------------------

port.status_register:               equ 0xf9 ; in 249
port.status:                        equ port.status_register

    frame.interrupt:                    equ %00001000

    ; see keyboard.i

;---------------------------------------------------------------

port.line_interrupt_register:       equ 0xf9 ; out 249
port.line_interrupt:                    equ port.line_interrupt_register

;---------------------------------------------------------------

port.low_memory_page_register:      equ 0xfa ; in/out 250
port.lmpr:                          equ port.low_memory_page_register

    low.memory.page.mask:               equ %00011111
    low.memory.ram.0:                   equ %00100000

;---------------------------------------------------------------

port.high_memory_page_register:     equ 0xfb ; in/out 251
port.hmpr:                          equ port.high_memory_page_register

    high.memory.page.mask:              equ %00011111
    high.memory.external:               equ %10000000

;---------------------------------------------------------------

port.video_memory_page_register:    equ 0xfc ; in/out 252
port.vmpr:                          equ port.video_memory_page_register

    video.mode.1:                       equ %0000000
    video.mode.2:                       equ %0100000
    video.mode.3:                       equ %1000000
    video.mode.4:                       equ %1100000
    video.memory.page.mask:             equ %0011111

;---------------------------------------------------------------

port.keyboard_register:             equ 0xfe ; in 254
port.keyboard:                      equ port.keyboard_register

    ; see keyboard.i

;---------------------------------------------------------------

port.border:                        equ 0xfe ; out 254
    port.border.screen.off:             equ %10000000


