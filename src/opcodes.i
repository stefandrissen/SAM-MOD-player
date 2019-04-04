;opcode defines for generating code

opcode.nop:			equ &00 ; nop
opcode.ld_bc_nn:	equ &01 ; ld bc,nn
opcode.ld_b_n:		equ &06	; ld b,n
opcode.ex_af_af:	equ &08	; ex af,af'
opcode.ld_c_n:		equ &0E ; ld c,n
opcode.ld_de_nn:	equ &11 ; ld de,nn
opcode.ld_de_a:		equ &12	; ld (de),a
opcode.inc_de:		equ &13	; inc de
opcode.ld_d_n:		equ &16 ; ld d,n
opcode.jr_n:		equ &18	; jr n
opcode.ld_a_de:		equ &1A ; ld a,(de)
opcode.jr_nz_n:		equ &20	; jr nz,n
opcode.ld_hl_nn:	equ &21 ; ld hl,nn
opcode.ld_nn_hl:	equ &22 ; ld (nn),hl
opcode.jr_z_n:		equ &28 ; jr z,n
opcode.inc_l:		equ &2C ; inc l
opcode.ld_sp_nn:	equ &31 ; ld sp,nn
opcode.ld_nn_a:		equ &32 ; ld (nn),a
opcode.ld_hl_n:		equ &36 ; ld (hl),n
opcode.ld_a_nn:		equ &3A ; ld a,(nn)
opcode.inc_a:		equ &3C	; inc a
opcode.ld_a_n:		equ &3E ; ld a,n
opcode.ld_b_hl:		equ &46 ; ld b,(hl)
opcode.ld_d_h:		equ &54 ; ld d,h
opcode.ld_e_b:		equ &58 ; ld e,b
opcode.ld_e_l:		equ &5D ; ld e,l
opcode.ld_e_hl:		equ &5E ; ld e,(hl)
opcode.ld_e_a:		equ &5F	; ld e,a
opcode.ld_l_a:		equ &6F ; ld l,a
opcode.halt:		equ &76	; halt
opcode.ld_a_b:		equ &78	; ld a,b
opcode.ld_a_c:		equ &79	; ld a,c
opcode.adc_hl_sp:	equ &7A ; adc hl,sp
opcode.ld_a_e:		equ &7B ; ld a,e
opcode.ld_a_l:		equ &7D ; ld a,l
opcode.ld_a_hl:		equ &7E	; ld a,(hl)
opcode.add_a_b:		equ &80 ; ld a,b
opcode.add_a_c:		equ &81 ; add a,c
opcode.and_a:		equ &A7 ; and a
opcode.xor_a:		equ &AF ; xor a
opcode.pop_bc:		equ &C1 ; pop bc
opcode.jp_nn:		equ &C3	; jp nn
opcode.push_bc:		equ &C5 ; push bc
opcode.add_a_n:		equ &C6 ; add a,n
opcode.ret:			equ &C9 ; ret
opcode.call_nn:		equ &CD ; call nn
opcode.pop_de:		equ &D1 ; pop de
opcode.out_n_a:		equ &D3 ; out (n),a
opcode.push_de:		equ &D5 ; push de
opcode.exx:			equ &D9 ; exx
opcode.in_a_n:		equ &DB ; in a,(n)
opcode.pop_hl:		equ &E1 ; pop hl
opcode.push_hl:		equ &E5 ; push hl
opcode.and_n:		equ &E6 ; and n
opcode.pop_af:		equ &F1 ; pop af
opcode.push_af:		equ &F5 ; push af
opcode.ei:			equ &FB	; ei
opcode.cp_n:		equ &FE ; cp n

opcode.cb:			equ &CB	; cb modifier

opcode.bit_6_h:		equ &74	; bit 6,h
opcode.res_6_h:		equ &B4	; res 6,h 	
opcode.res_7_l:		equ &BD	; res 7,l

opcode.ed:			equ &ED	; ed modifier

opcode.ld_nn_de:	equ &53 ; ld (nn),de
opcode.ld_nn_sp:	equ &73 ; ld (nn),sp
opcode.ldi:			equ &A0	; ldi
opcode.outd:		equ &AB ; outd
opcode.ldir:		equ &B0	; ldir


opcode.ix:			equ &DD	; ix modifier

opcode.ld_ix_nn:	equ &21 ; ld hl,nn
opcode.push_ix:		equ &E5 ; push ix
opcode.pop_ix:		equ &E1 ; pop ix
opcode.jp_ix:		equ &E9	; jp (ix)

opcode.rst_8:		equ &cf	; rst 8
