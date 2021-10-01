; blue alpha sound sampler
;---------------------------------------------------------------

port.blue_alpha.control:    equ 0x7f7f ; control port
port.blue_alpha.c:          equ 0x7e7f ; clock on bit 0
port.blue_alpha.b:          equ 0x7d7f ; sample control, bit 0 low = dac, bit 1 low = adc
port.blue_alpha.a:          equ 0x7c7f ; data

;---------------------------------------------------------------
