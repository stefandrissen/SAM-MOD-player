; ports

low.memory.page.register:	equ 250

	low.memory.page.mask:		equ %00011111
	low.memory.ram.0:			equ %00100000

high.memory.page.register:	equ 251

	high.memory.page.mask:		equ %00011111
	high.memory.external:		equ %10000000

	external.memory.page.c:		equ 128	; read-only
	external.memory.page.d:		equ 129	; read-only

video.memory.page.register:	equ 252

	video.mode.2:				equ 32

color.look.up.table:		equ 248

line.interrupt.register:	equ 249

status.register:			equ 249

	frame.interrupt:			equ %00001000

	; key pressed = 3 highest bits
	keyboard.edit:				equ %10111111 ; edit ? ?
	keyboard.plus_minus:		equ %11101111 ; ? + -
	keyboard.caps_esc:			equ %11110111 ; caps ? esc
	keyboard.f9:				equ %11111011 ; f9 ? ?

keyboard.register:			equ 254

	; key pressed = 5 lowest bits
	keyboard.cursors_ctrl:		equ %11111111 ; right,left,down,up,ctrl 
	keyboard.shift:				equ %11111110 ; ? ? ? ? shift
	keyboard.67890:				equ %11101111
	keyboard.yuiop:				equ %11011111
	keyboard.hjkl_return:		equ %10111111

printer.port.1:				equ 232
printer.port.2:				equ 234