; sambus real time clock

;---------------------------------------------------------------

port.sambus.rtc:        equ 0xef

port.sambus.year.t:     equ 0xb0ef   ; < 80 -> + 100 to allow 1980-2079
port.sambus.year:       equ 0xa0ef

port.sambus.month.t:    equ 0x90ef
port.sambus.month:      equ 0x80ef

port.sambus.day.t:      equ 0x70ef
port.sambus.day:        equ 0x60ef

port.sambus.hour.t:     equ 0x50ef
port.sambus.hour:       equ 0x40ef

port.sambus.minute.t:   equ 0x30ef
port.sambus.minute:     equ 0x20ef

port.sambus.second.t:   equ 0x10ef
port.sambus.second:     equ 0x00ef
