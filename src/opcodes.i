;opcode defines for generating code

opcode_nop:			equ &00 ; nop
opcode_ld_bc_nn:	equ &01 ; ld bc,nn
opcode_ld_b_n:		equ &06	; ld b,n
opcode_ex_af_af:	equ &08	; ex af,af'
opcode_ld_c_n:		equ &0E ; ld c,n
opcode_ld_de_nn:	equ &11 ; ld de,nn
opcode_ld_de_a:		equ &12	; ld (de),a
opcode_inc_de:		equ &13	; inc de
opcode_ld_d_n:		equ &16 ; ld d,n
opcode_jr_n:		equ &18	; jr n
opcode_ld_a_de:		equ &1A ; ld a,(de)
opcode_jr_nz_n:		equ &20	; jr nz,n
opcode_ld_hl_nn:	equ &21 ; ld hl,nn
opcode_ld_nn_hl:	equ &22 ; ld (nn),hl
opcode_jr_z_n:		equ &28 ; jr z,n
opcode_inc_l:		equ &2C ; inc l
opcode_ld_sp_nn:	equ &31 ; ld sp,nn
opcode_ld_nn_a:		equ &32 ; ld (nn),a
opcode_ld_hl_n:		equ &36 ; ld (hl),n
opcode_ld_a_nn:		equ &3A ; ld a,(nn)
opcode_inc_a:		equ &3C	; inc a
opcode_ld_a_n:		equ &3E ; ld a,n
opcode_ld_b_hl:		equ &46 ; ld b,(hl)
opcode_ld_d_h:		equ &54 ; ld d,h
opcode_ld_e_b:		equ &58 ; ld e,b
opcode_ld_e_l:		equ &5D ; ld e,l
opcode_ld_e_hl:		equ &5E ; ld e,(hl)
opcode_ld_e_a:		equ &5F	; ld e,a
opcode_ld_l_a:		equ &6F ; ld l,a
opcode_halt:		equ &76	; halt
opcode_ld_a_b:		equ &78	; ld a,b
opcode_ld_a_c:		equ &79	; ld a,c
opcode_adc_hl_sp:	equ &7A ; adc hl,sp
opcode_ld_a_e:		equ &7B ; ld a,e
opcode_ld_a_l:		equ &7D ; ld a,l
opcode_ld_a_hl:		equ &7E	; ld a,(hl)
opcode_add_a_b:		equ &80 ; ld a,b
opcode_add_a_c:		equ &81 ; add a,c
opcode_and_a:		equ &A7 ; and a
opcode_xor_a:		equ &AF ; xor a
opcode_pop_bc:		equ &C1 ; pop bc
opcode_jp_nn:		equ &C3	; jp nn
opcode_push_bc:		equ &C5 ; push bc
opcode_add_a_n:		equ &C6 ; add a,n
opcode_ret:			equ &C9 ; ret
opcode_call_nn:		equ &CD ; call nn
opcode_pop_de:		equ &D1 ; pop de
opcode_out_n_a:		equ &D3 ; out (n),a
opcode_push_de:		equ &D5 ; push de
opcode_exx:			equ &D9 ; exx
opcode_in_a_n:		equ &DB ; in a,(n)
opcode_pop_hl:		equ &E1 ; pop hl
opcode_push_hl:		equ &E5 ; push hl
opcode_and_n:		equ &E6 ; and n
opcode_pop_af:		equ &F1 ; pop af
opcode_push_af:		equ &F5 ; push af
opcode_ei:			equ &FB	; ei
opcode_cp_n:		equ &FE ; cp n

opcode_cb:			equ &CB	; cb modifier

opcode_bit_6_h:		equ &74	; bit 6,h
opcode_res_6_h:		equ &B4	; res 6,h 	
opcode_res_7_l:		equ &BD	; res 7,l

opcode_ed:			equ &ED	; ed modifier

opcode_ld_nn_de:	equ &53 ; ld (nn),de
opcode_ld_nn_sp:	equ &73 ; ld (nn),sp
opcode_ldi:			equ &A0	; ldi
opcode_outd:		equ &AB ; outd
opcode_ldir:		equ &B0	; ldir


opcode_ix:			equ &DD	; ix modifier

opcode_ld_ix_nn:	equ &21 ; ld hl,nn
opcode_push_ix:		equ &E5 ; push ix
opcode_pop_ix:		equ &E1 ; pop ix
opcode_jp_ix:		equ &E9	; jp (ix)