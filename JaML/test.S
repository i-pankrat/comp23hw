extern make_pa
extern add_args_to_pa
extern tuple_make
extern tuple_take
extern print_int
extern print_bool
global main
sum:
	push rbp
	mov rbp, rsp
	mov qword [rbp -16], rsi; move argument a from reg rsi on stack to avoid losing argument
	mov rsi, 0
	mov qword [rbp -8], rdi; move argument b from reg rdi on stack to avoid losing argument
	mov rdi, rdx
	; Start. Caller save register
	mov qword [rbp -80], r11
	mov qword [rbp -72], r10
	mov qword [rbp -64], r9
	mov qword [rbp -56], r8
	mov qword [rbp -48], rdi
	mov qword [rbp -40], rsi
	mov qword [rbp -32], rdx
	mov qword [rbp -24], rcx
	; End. Caller save register
	sub rsp, 80; restore RSP to the valid value and add alignment if necessary
	call tuple_take
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -24]
	mov rdx, qword [rbp -32]
	mov rsi, qword [rbp -40]
	mov rdi, qword [rbp -48]
	mov r8, qword [rbp -56]
	mov r9, qword [rbp -64]
	mov r10, qword [rbp -72]
	mov r11, qword [rbp -80]
	; End. Restore caller save registers
	mov qword [rbp -24], rax; save regturn value
	mov rax, qword [rbp -24]
	mov qword [rbp -32], rax; save regturn value
	mov rsi, 1
	mov rdi, rdx
	; Start. Caller save register
	mov qword [rbp -96], r11
	mov qword [rbp -88], r10
	mov qword [rbp -80], r9
	mov qword [rbp -72], r8
	mov qword [rbp -64], rdi
	mov qword [rbp -56], rsi
	mov qword [rbp -48], rdx
	mov qword [rbp -40], rcx
	; End. Caller save register
	sub rsp, 96; restore RSP to the valid value and add alignment if necessary
	call tuple_take
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -40]
	mov rdx, qword [rbp -48]
	mov rsi, qword [rbp -56]
	mov rdi, qword [rbp -64]
	mov r8, qword [rbp -72]
	mov r9, qword [rbp -80]
	mov r10, qword [rbp -88]
	mov r11, qword [rbp -96]
	; End. Restore caller save registers
	mov qword [rbp -40], rax; save regturn value
	mov rax, qword [rbp -40]
	mov qword [rbp -48], rax; save regturn value
	mov rax, qword [rbp -16]
	mov qword [rbp -56], rdx; move argument tuple_arg2 from reg rdx on stack to avoid losing argument
	mov rdx, qword [rbp -8]
	add rax, rdx
	mov qword [rbp -64], rax; save regturn value
	mov rax, qword [rbp -32]
	mov rdx, qword [rbp -48]
	add rax, rdx
	mov qword [rbp -72], rax; save regturn value
	mov rdx, qword [rbp -72]
	mov rsi, qword [rbp -64]
	mov rdi, 2
	; Start. Caller save register
	mov qword [rbp -136], r11
	mov qword [rbp -128], r10
	mov qword [rbp -120], r9
	mov qword [rbp -112], r8
	mov qword [rbp -104], rdi
	mov qword [rbp -96], rsi
	mov qword [rbp -88], rdx
	mov qword [rbp -80], rcx
	; End. Caller save register
	sub rsp, 144; restore RSP to the valid value and add alignment if necessary
	call tuple_make
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -80]
	mov rdx, qword [rbp -88]
	mov rsi, qword [rbp -96]
	mov rdi, qword [rbp -104]
	mov r8, qword [rbp -112]
	mov r9, qword [rbp -120]
	mov r10, qword [rbp -128]
	mov r11, qword [rbp -136]
	; End. Restore caller save registers
	mov qword [rbp -80], rax; save regturn value
	mov rax, qword [rbp -80]
	pop rbp
	ret
x:
	push rbp
	mov rbp, rsp
	mov qword [rbp -16], rsi; move argument tuple_arg1 from reg rsi on stack to avoid losing argument
	mov rsi, 0
	mov qword [rbp -8], rdi; move argument c from reg rdi on stack to avoid losing argument
	mov rdi, rsi
	; Start. Caller save register
	mov qword [rbp -80], r11
	mov qword [rbp -72], r10
	mov qword [rbp -64], r9
	mov qword [rbp -56], r8
	mov qword [rbp -48], rdi
	mov qword [rbp -40], rsi
	mov qword [rbp -32], rdx
	mov qword [rbp -24], rcx
	; End. Caller save register
	sub rsp, 80; restore RSP to the valid value and add alignment if necessary
	call tuple_take
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -24]
	mov rdx, qword [rbp -32]
	mov rsi, qword [rbp -40]
	mov rdi, qword [rbp -48]
	mov r8, qword [rbp -56]
	mov r9, qword [rbp -64]
	mov r10, qword [rbp -72]
	mov r11, qword [rbp -80]
	; End. Restore caller save registers
	mov qword [rbp -24], rax; save regturn value
	mov rax, qword [rbp -24]
	mov qword [rbp -32], rax; save regturn value
	mov rsi, 1
	mov rdi, qword [rbp -16]
	; Start. Caller save register
	mov qword [rbp -96], r11
	mov qword [rbp -88], r10
	mov qword [rbp -80], r9
	mov qword [rbp -72], r8
	mov qword [rbp -64], rdi
	mov qword [rbp -56], rsi
	mov qword [rbp -48], rdx
	mov qword [rbp -40], rcx
	; End. Caller save register
	sub rsp, 96; restore RSP to the valid value and add alignment if necessary
	call tuple_take
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -40]
	mov rdx, qword [rbp -48]
	mov rsi, qword [rbp -56]
	mov rdi, qword [rbp -64]
	mov r8, qword [rbp -72]
	mov r9, qword [rbp -80]
	mov r10, qword [rbp -88]
	mov r11, qword [rbp -96]
	; End. Restore caller save registers
	mov qword [rbp -40], rax; save regturn value
	mov rax, qword [rbp -40]
	mov qword [rbp -48], rax; save regturn value
	mov rdx, 13
	mov rsi, qword [rbp -8]
	mov rdi, 2
	; Start. Caller save register
	mov qword [rbp -112], r11
	mov qword [rbp -104], r10
	mov qword [rbp -96], r9
	mov qword [rbp -88], r8
	mov qword [rbp -80], rdi
	mov qword [rbp -72], rsi
	mov qword [rbp -64], rdx
	mov qword [rbp -56], rcx
	; End. Caller save register
	sub rsp, 112; restore RSP to the valid value and add alignment if necessary
	call tuple_make
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -56]
	mov rdx, qword [rbp -64]
	mov rsi, qword [rbp -72]
	mov rdi, qword [rbp -80]
	mov r8, qword [rbp -88]
	mov r9, qword [rbp -96]
	mov r10, qword [rbp -104]
	mov r11, qword [rbp -112]
	; End. Restore caller save registers
	mov qword [rbp -56], rax; save regturn value
	mov rdx, 0
	mov rsi, 3
	mov rdi, sum
	; Start. Caller save register
	mov qword [rbp -120], r11
	mov qword [rbp -112], r10
	mov qword [rbp -104], r9
	mov qword [rbp -96], r8
	mov qword [rbp -88], rdi
	mov qword [rbp -80], rsi
	mov qword [rbp -72], rdx
	mov qword [rbp -64], rcx
	; End. Caller save register
	sub rsp, 128; restore RSP to the valid value and add alignment if necessary
	call make_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -64]
	mov rdx, qword [rbp -72]
	mov rsi, qword [rbp -80]
	mov rdi, qword [rbp -88]
	mov r8, qword [rbp -96]
	mov r9, qword [rbp -104]
	mov r10, qword [rbp -112]
	mov r11, qword [rbp -120]
	; End. Restore caller save registers
	mov qword [rbp -64], rax; save regturn value
	mov r8, qword [rbp -56]
	mov rcx, qword [rbp -32]
	mov rdx, qword [rbp -48]
	mov rsi, 3
	mov rdi, qword [rbp -64]
	; Start. Caller save register
	mov qword [rbp -128], r11
	mov qword [rbp -120], r10
	mov qword [rbp -112], r9
	mov qword [rbp -104], r8
	mov qword [rbp -96], rdi
	mov qword [rbp -88], rsi
	mov qword [rbp -80], rdx
	mov qword [rbp -72], rcx
	; End. Caller save register
	sub rsp, 128; restore RSP to the valid value and add alignment if necessary
	call add_args_to_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -72]
	mov rdx, qword [rbp -80]
	mov rsi, qword [rbp -88]
	mov rdi, qword [rbp -96]
	mov r8, qword [rbp -104]
	mov r9, qword [rbp -112]
	mov r10, qword [rbp -120]
	mov r11, qword [rbp -128]
	; End. Restore caller save registers
	mov qword [rbp -72], rax; save regturn value
	mov rax, qword [rbp -72]
	pop rbp
	ret
scd:
	push rbp
	mov rbp, rsp
	mov rsi, 1
	mov qword [rbp -8], rdi; move argument tuple_arg3 from reg rdi on stack to avoid losing argument
	mov rdi, rdi
	; Start. Caller save register
	mov qword [rbp -72], r11
	mov qword [rbp -64], r10
	mov qword [rbp -56], r9
	mov qword [rbp -48], r8
	mov qword [rbp -40], rdi
	mov qword [rbp -32], rsi
	mov qword [rbp -24], rdx
	mov qword [rbp -16], rcx
	; End. Caller save register
	sub rsp, 80; restore RSP to the valid value and add alignment if necessary
	call tuple_take
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -16]
	mov rdx, qword [rbp -24]
	mov rsi, qword [rbp -32]
	mov rdi, qword [rbp -40]
	mov r8, qword [rbp -48]
	mov r9, qword [rbp -56]
	mov r10, qword [rbp -64]
	mov r11, qword [rbp -72]
	; End. Restore caller save registers
	mov qword [rbp -16], rax; save regturn value
	mov rax, qword [rbp -16]
	mov qword [rbp -24], rax; save regturn value
	mov rax, qword [rbp -24]
	pop rbp
	ret
main:
	push rbp
	mov rbp, rsp
	mov rdx, 145
	mov rsi, 10
	mov rdi, 2
	; Start. Caller save register
	mov qword [rbp -64], r11
	mov qword [rbp -56], r10
	mov qword [rbp -48], r9
	mov qword [rbp -40], r8
	mov qword [rbp -32], rdi
	mov qword [rbp -24], rsi
	mov qword [rbp -16], rdx
	mov qword [rbp -8], rcx
	; End. Caller save register
	sub rsp, 64; restore RSP to the valid value and add alignment if necessary
	call tuple_make
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -8]
	mov rdx, qword [rbp -16]
	mov rsi, qword [rbp -24]
	mov rdi, qword [rbp -32]
	mov r8, qword [rbp -40]
	mov r9, qword [rbp -48]
	mov r10, qword [rbp -56]
	mov r11, qword [rbp -64]
	; End. Restore caller save registers
	mov qword [rbp -8], rax; save regturn value ; Тут лежит (10, 145)
	mov rdx, 0
	mov rsi, 2
	mov rdi, x
	; Start. Caller save register
	mov qword [rbp -72], r11
	mov qword [rbp -64], r10
	mov qword [rbp -56], r9
	mov qword [rbp -48], r8
	mov qword [rbp -40], rdi
	mov qword [rbp -32], rsi
	mov qword [rbp -24], rdx
	mov qword [rbp -16], rcx
	; End. Caller save register
	sub rsp, 80; restore RSP to the valid value and add alignment if necessary
	call make_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -16]
	mov rdx, qword [rbp -24]
	mov rsi, qword [rbp -32]
	mov rdi, qword [rbp -40]
	mov r8, qword [rbp -48]
	mov r9, qword [rbp -56]
	mov r10, qword [rbp -64]
	mov r11, qword [rbp -72]
	; End. Restore caller save registers
	mov qword [rbp -16], rax; save regturn value
	mov rcx, qword [rbp -8] ; второй аргумент -- tuple
	mov rdx, 101 ; первый 
	mov rsi, 2 ;  два аргумента
	mov rdi, qword [rbp -16] ; адрес замыкания кладёт в rdi
	; Start. Caller save register
	mov qword [rbp -80], r11
	mov qword [rbp -72], r10
	mov qword [rbp -64], r9
	mov qword [rbp -56], r8
	mov qword [rbp -48], rdi
	mov qword [rbp -40], rsi
	mov qword [rbp -32], rdx
	mov qword [rbp -24], rcx
	; End. Caller save register
	sub rsp, 80; restore RSP to the valid value and add alignment if necessary
	call add_args_to_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -24]
	mov rdx, qword [rbp -32]
	mov rsi, qword [rbp -40]
	mov rdi, qword [rbp -48]
	mov r8, qword [rbp -56]
	mov r9, qword [rbp -64]
	mov r10, qword [rbp -72]
	mov r11, qword [rbp -80]
	; End. Restore caller save registers
	mov qword [rbp -24], rax; save regturn value
	mov rdx, 0
	mov rsi, 1
	mov rdi, scd
	; Start. Caller save register
	mov qword [rbp -88], r11
	mov qword [rbp -80], r10
	mov qword [rbp -72], r9
	mov qword [rbp -64], r8
	mov qword [rbp -56], rdi
	mov qword [rbp -48], rsi
	mov qword [rbp -40], rdx
	mov qword [rbp -32], rcx
	; End. Caller save register
	sub rsp, 96; restore RSP to the valid value and add alignment if necessary
	call make_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -32]
	mov rdx, qword [rbp -40]
	mov rsi, qword [rbp -48]
	mov rdi, qword [rbp -56]
	mov r8, qword [rbp -64]
	mov r9, qword [rbp -72]
	mov r10, qword [rbp -80]
	mov r11, qword [rbp -88]
	; End. Restore caller save registers
	mov qword [rbp -32], rax; save regturn value
	mov rdx, qword [rbp -24]
	mov rsi, 1
	mov rdi, qword [rbp -32]
	; Start. Caller save register
	mov qword [rbp -96], r11
	mov qword [rbp -88], r10
	mov qword [rbp -80], r9
	mov qword [rbp -72], r8
	mov qword [rbp -64], rdi
	mov qword [rbp -56], rsi
	mov qword [rbp -48], rdx
	mov qword [rbp -40], rcx
	; End. Caller save register
	sub rsp, 96; restore RSP to the valid value and add alignment if necessary
	call add_args_to_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -40]
	mov rdx, qword [rbp -48]
	mov rsi, qword [rbp -56]
	mov rdi, qword [rbp -64]
	mov r8, qword [rbp -72]
	mov r9, qword [rbp -80]
	mov r10, qword [rbp -88]
	mov r11, qword [rbp -96]
	; End. Restore caller save registers
	mov qword [rbp -40], rax; save regturn value
	mov rdx, 0
	mov rsi, 1
	mov rdi, print_int
	; Start. Caller save register
	mov qword [rbp -104], r11
	mov qword [rbp -96], r10
	mov qword [rbp -88], r9
	mov qword [rbp -80], r8
	mov qword [rbp -72], rdi
	mov qword [rbp -64], rsi
	mov qword [rbp -56], rdx
	mov qword [rbp -48], rcx
	; End. Caller save register
	sub rsp, 112; restore RSP to the valid value and add alignment if necessary
	call make_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -48]
	mov rdx, qword [rbp -56]
	mov rsi, qword [rbp -64]
	mov rdi, qword [rbp -72]
	mov r8, qword [rbp -80]
	mov r9, qword [rbp -88]
	mov r10, qword [rbp -96]
	mov r11, qword [rbp -104]
	; End. Restore caller save registers
	mov qword [rbp -48], rax; save regturn value
	mov rdx, qword [rbp -40]
	mov rsi, 1
	mov rdi, qword [rbp -48]
	; Start. Caller save register
	mov qword [rbp -112], r11
	mov qword [rbp -104], r10
	mov qword [rbp -96], r9
	mov qword [rbp -88], r8
	mov qword [rbp -80], rdi
	mov qword [rbp -72], rsi
	mov qword [rbp -64], rdx
	mov qword [rbp -56], rcx
	; End. Caller save register
	sub rsp, 112; restore RSP to the valid value and add alignment if necessary
	call add_args_to_pa
	mov rsp, rbp; return RSP to RBP
	; Start. Restore caller save registers
	mov rcx, qword [rbp -56]
	mov rdx, qword [rbp -64]
	mov rsi, qword [rbp -72]
	mov rdi, qword [rbp -80]
	mov r8, qword [rbp -88]
	mov r9, qword [rbp -96]
	mov r10, qword [rbp -104]
	mov r11, qword [rbp -112]
	; End. Restore caller save registers
	mov qword [rbp -56], rax; save regturn value
	mov rax, qword [rbp -56]
	pop rbp
	ret