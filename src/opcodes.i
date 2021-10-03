;opcode defines for generating code

opcode.nop:         equ 0x00 ; nop
opcode.ld_bc_nn:    equ 0x01 ; ld bc,nn
opcode.ld_b_n:      equ 0x06 ; ld b,n
opcode.ex_af_af:    equ 0x08 ; ex af,af'
opcode.ld_c_n:      equ 0x0e ; ld c,n

opcode.ld_de_nn:    equ 0x11 ; ld de,nn
opcode.ld_de_a:     equ 0x12 ; ld (de),a
opcode.inc_de:      equ 0x13 ; inc de
opcode.ld_d_n:      equ 0x16 ; ld d,n
opcode.jr_n:        equ 0x18 ; jr n
opcode.ld_a_de:     equ 0x1a ; ld a,(de)

opcode.jr_nz_n:     equ 0x20 ; jr nz,n
opcode.ld_hl_nn:    equ 0x21 ; ld hl,nn
opcode.ld_nn_hl:    equ 0x22 ; ld (nn),hl
opcode.jr_z_n:      equ 0x28 ; jr z,n
opcode.inc_l:       equ 0x2c ; inc l

opcode.ld_sp_nn:    equ 0x31 ; ld sp,nn
opcode.ld_nn_a:     equ 0x32 ; ld (nn),a
opcode.ld_hl_n:     equ 0x36 ; ld (hl),n
opcode.scf:         equ 0x37 ; scf
opcode.ld_a_nn:     equ 0x3a ; ld a,(nn)
opcode.inc_a:       equ 0x3c ; inc a
opcode.ld_a_n:      equ 0x3e ; ld a,n

opcode.ld_b_hl:     equ 0x46 ; ld b,(hl)

opcode.ld_d_h:      equ 0x54 ; ld d,h
opcode.ld_e_b:      equ 0x58 ; ld e,b
opcode.ld_e_l:      equ 0x5d ; ld e,l
opcode.ld_e_hl:     equ 0x5e ; ld e,(hl)
opcode.ld_e_a:      equ 0x5f ; ld e,a

opcode.ld_l_a:      equ 0x6f ; ld l,a

opcode.halt:        equ 0x76 ; halt
opcode.ld_a_b:      equ 0x78 ; ld a,b
opcode.ld_a_c:      equ 0x79 ; ld a,c
opcode.ld_a_e:      equ 0x7b ; ld a,e
opcode.ld_a_l:      equ 0x7d ; ld a,l
opcode.ld_a_hl:     equ 0x7e ; ld a,(hl)

opcode.add_a_b:     equ 0x80 ; add a,b
opcode.add_a_c:     equ 0x81 ; add a,c

opcode.and_a:       equ 0xa7 ; and a
opcode.xor_a:       equ 0xaf ; xor a

opcode.pop_bc:      equ 0xc1 ; pop bc
opcode.jp_nn:       equ 0xc3 ; jp nn
opcode.push_bc:     equ 0xc5 ; push bc
opcode.add_a_n:     equ 0xc6 ; add a,n
opcode.rst_00:      equ 0xc7 ; rst 0x00
opcode.ret:         equ 0xc9 ; ret
opcode.call_nn:     equ 0xcd ; call nn
opcode.adc_a_n:     equ 0xce ; adc a,n
opcode.rst_08:      equ 0xcf ; rst 0x08

opcode.pop_de:      equ 0xd1 ; pop de
opcode.out_n_a:     equ 0xd3 ; out (n),a
opcode.push_de:     equ 0xd5 ; push de
opcode.rst_10:      equ 0xd7 ; rst 0x10
opcode.exx:         equ 0xd9 ; exx
opcode.in_a_n:      equ 0xdb ; in a,(n)
opcode.rst_18:      equ 0xdf ; rst 0x18

opcode.pop_hl:      equ 0xe1 ; pop hl
opcode.push_hl:     equ 0xe5 ; push hl
opcode.and_n:       equ 0xe6 ; and n
opcode.rst_20:      equ 0xe7 ; rst 0x20
opcode.rst_28:      equ 0xef ; rst 0x28

opcode.pop_af:      equ 0xf1 ; pop af
opcode.push_af:     equ 0xf5 ; push af
opcode.rst_30:      equ 0xf7 ; rst 0x30
opcode.ei:          equ 0xfb ; ei
opcode.cp_n:        equ 0xfe ; cp n
opcode.rst_38:      equ 0xff ; rst 0x38

;---------------------------------------------------------------

opcode.cb:          equ 0xcb ; cb modifier

opcode.rlc_h:       equ 0x04 ; rlc h
opcode.bit_6_h:     equ 0x74 ; bit 6,h
opcode.res_6_h:     equ 0xb4 ; res 6,h
opcode.res_7_l:     equ 0xbd ; res 7,l

;---------------------------------------------------------------

opcode.ed:          equ 0xed ; ed modifier

opcode.ld_nn_de:    equ 0x53 ; ld (nn),de
opcode.ld_nn_sp:    equ 0x73 ; ld (nn),sp
opcode.adc_hl_sp:   equ 0x7a ; adc hl,sp
opcode.ldi:         equ 0xa0 ; ldi
opcode.outd:        equ 0xab ; outd
opcode.ldir:        equ 0xb0 ; ldir

;---------------------------------------------------------------

opcode.ix:          equ 0xdd ; ix modifier

opcode.ld_ix_nn:    equ 0x21 ; ld hl,nn
opcode.push_ix:     equ 0xe5 ; push ix
opcode.pop_ix:      equ 0xe1 ; pop ix
opcode.jp_ix:       equ 0xe9 ; jp (ix)

