; ports

;---------------------------------------------------------------

port.low_memory_page_register:	equ &fa	; 250
port.lmpr:						equ port.low_memory_page_register

	low.memory.page.mask:		equ %00011111
	low.memory.ram.0:			equ %00100000

;---------------------------------------------------------------

port.high_memory_page_register:	equ &fb	; 251
port.hmpr:						equ port.high_memory_page_register

	high.memory.page.mask:		equ %00011111
	high.memory.external:		equ %10000000

port.external.memory.page.c:	equ &80 ; 128 - read-only
port.xmpr.c:					equ port.external.memory.page.c
	
port.external.memory.page.d:	equ &81 ; 129 - read-only
port.xmpr.d:					equ port.external.memory.page.d

;---------------------------------------------------------------

port.video_memory_page_register:	equ &fc	; 252
port.vmpr:							equ port.video_memory_page_register 

	video.mode.1:				equ %0000000
	video.mode.2:				equ %0100000
	video.mode.3:				equ %1000000
	video.mode.4:				equ %1100000
	video.memory.page.mask:		equ %0011111

;---------------------------------------------------------------

port.color_look_up_table:		equ &f8	; 248
port.clut:						equ port.color_look_up_table	

;---------------------------------------------------------------

port.border:					equ &fe	; 254

;---------------------------------------------------------------

port.line_interrupt_register:	equ &f9	; 249
port.line_interrupt:			equ port.line_interrupt_register

;---------------------------------------------------------------

port.status_register:			equ &f9	; 249
port.status:					equ port.status_register

	frame.interrupt:			equ %00001000

	; key pressed = 3 highest bits
	keyboard.edit:				equ %10111111 ; edit ? ?
	
	keyboard.plus_minus:		equ %11101111 ; ? + -
	keyboard.caps_esc:			equ %11110111 ; caps ? esc
	keyboard.f9_f8_f7:			equ %11111011
	keyboard.f6_f5_f4:			equ %11111101
	keyboard.f3_f2_f1:			equ %11111110

;---------------------------------------------------------------

port.keyboard_register:			equ &fe	; 254
port.keyboard:					equ port.keyboard_register

	; key pressed = 5 lowest bits
	keyboard.cursors_ctrl:		equ %11111111 ; right,left,down,up,ctrl 
	keyboard.vcxz_shift:		equ %11111110
	keyboard.gfdsa:				equ %11111101	
	keyboard.12345:				equ %11110111
	keyboard.67890:				equ %11101111
	keyboard.yuiop:				equ %11011111
	keyboard.hjkl_return:		equ %10111111
	keyboard.bnm?_space:		equ %01111111

;---------------------------------------------------------------

port.printer_1:				equ &e8	; 232
port.printer_2:				equ &ea	; 234

;---------------------------------------------------------------

port.disk.1.statcom:		equ &e0	; 224
port.disk.1.track:			equ &e1	; 225
port.disk.1.sector:			equ &e2	; 226
port.disk.1.data:			equ &e3	; 227

port.disk.2.statcom:		equ &f0	; 240
port.disk.2.track:			equ &f1	; 241
port.disk.2.sector:			equ &f2	; 242
port.disk.2.data:			equ &f3	; 243

;---------------------------------------------------------------

port.sound.data:            equ &00ff   ; 255
port.sound.address:         equ &01ff   ; 511

;---------------------------------------------------------------
